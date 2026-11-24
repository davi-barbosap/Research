# ==============================================================================
# PROJETO: PERFIL DEMOGRÁFICO E CLÍNICO DOS ATENDIMENTOS AMBULATORIAIS
#          POR ASMA NO SUS — ALAGOAS
#
# Fontes (SIASUS/DATASUS, ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados):
#   PAALaamm.dbc — Produção Ambulatorial (APAC + BPA-I + BPA-C + RAAS)
#   BIALaamm.dbc — BPA Individualizado (CNS do paciente e data de nascimento)
#   AMALaamm.dbc — APAC de Medicamentos (Componente Especializado da
#                  Assistência Farmacêutica: CNS, peso, altura, medicamento)
# Documentação: Informe Técnico SIASUS, DATASUS, jul/2019 ("Informe" abaixo).
#
# Perguntas de pesquisa:
#   1. Qual o perfil demográfico (sexo, idade, raça/cor) e clínico (CID, caráter
#      do atendimento, procedimentos) dos atendimentos por asma em AL?
#   2. O perfil difere entre residentes da capital (Maceió) e do interior?
#   3. Quantos pacientes únicos são atendidos e com que frequência retornam?
#   4. Há associação ecológica entre atendimento fora do município de
#      residência (proxy de acesso) e proporção de atendimentos de urgência?
#
# Definição de caso: CID-10 J45.x (asma) ou J46 (estado de mal asmático),
# residente em Alagoas, competência 2015–2025, produção aprovada.
#
# DECISÕES PARA REDUÇÃO DE VIESES (todas baseadas no Informe):
#  (a) PA_DOCORIG define o instrumento e o que cada linha significa (Informe 2.1):
#        C = BPA-C: agregado, sem CID e sem paciente; PA_MUNPCN traz o município
#            do ESTABELECIMENTO, não o de residência → EXCLUÍDO.
#        A/B = RAAS (AD/Psicossocial): CID não é campo do PA para esses
#            instrumentos → excluído (contagem reportada no fluxograma).
#        I = BPA-I: uma linha por atendimento → 1 linha = 1 atendimento.
#        P/S = APAC: uma linha por PROCEDIMENTO (principal "P" e secundários
#            "S") da mesma APAC → contar linhas infla a contagem e replica o
#            CID. A APAC é colapsada em 1 atendimento por (PA_AUTORIZ, PA_CMP).
#            APAC de MEDICAMENTOS (procedimento principal do grupo 06) são as
#            mesmas do arquivo AM (verificado nos dados: 1:1) → saem do PA e
#            são analisadas só no módulo AM (evita dupla contagem e evita que a
#            dispensação domine o perfil de "atendimento").
#  (b) PA_CIDPRI existe em APAC e BPA-I; PA_CIDSEC existe SÓ em APAC
#      (Informe, campos 30–31). Portanto "asma como diagnóstico secundário" só
#      é observável em APAC; a posição do CID é sempre estratificada por
#      instrumento e não se compara principal × secundário entre instrumentos.
#  (c) Idade: PA_IDADE é em anos. PA_FLIDADE = 3 (inexistente) ou 4 (em branco)
#      → idade ausente; idades > idade_max → ausente (valor implausível);
#      PA_FLIDADE = 2 (fora da faixa SIGTAP do procedimento) é sinalizado e
#      reportado. No BI a idade é recalculada pela data de nascimento.
#      Análise de sensibilidade sem < 5 anos (diagnóstico de asma pouco
#      específico em lactentes e pré-escolares — sibilância transitória).
#  (d) Só produção aprovada: PA_INDICA 5 (total) ou 6 (parcial); 0 = não
#      aprovado é excluído. No BI, QT_APROV > 0.
#  (e) Ano = competência de realização (PA_CMP / DT_ATEND), não o mês de
#      processamento (PA_MVM). São lidos também arquivos dos primeiros
#      `meses_extra` meses após o período para captar apresentação tardia.
#  (f) Residência diferente do estabelecimento: usa a flag oficial PA_MNDIF.
#  (g) O SIA-PA não identifica pacientes. Contagem de pacientes únicos vem do
#      BI (CNS_PAC). Os mesmos atendimentos BPA-I estão no PA (DOCORIG = "I")
#      e no BI — NUNCA somar as duas fontes; o BI é usado só para a análise
#      por paciente e para checar consistência com o PA.
#  (h) Só arquivos de AL são lidos: residentes de AL atendidos em outros
#      estados não são capturados; não residentes atendidos em AL são excluídos.
#  (i) Com N grande, o qui-quadrado é quase sempre significativo; a magnitude
#      da diferença Maceió × interior é reportada pelo V de Cramér.
#
# APAC DE MEDICAMENTOS (AM) — decisões específicas:
#  (j) A APAC de medicamentos é renovada mês a mês: cada registro é um
#      "paciente-mês" de dispensação, não um caso novo. Unidade principal =
#      paciente (AP_CNSPCN); início de tratamento = APAC inicial (AP_TPAPAC = 1).
#      Registros repetidos de (AP_AUTORIZ, AP_CMP, AP_PRIPAL) são removidos.
#      O AM não informa aprovação e inclui APAC não aprovadas: mantêm-se só as
#      APAC aprovadas no PA (PA_INDICA 5/6), mesma regra dos atendimentos.
#  (k) Idade = AP_COIDADE (unidade) + AP_NUIDADE (2 dígitos). Codificação
#      assumida, igual à do SIH: 4 = anos, 5 = centenas de anos (100 + n),
#      2/3 = dias/meses (→ 0 ano). Conferir com o .cnv do TAB_SIA.
#  (l) Peso (AM_PESO, kg) e altura (AM_ALTURA, cm): valores fora de faixas
#      plausíveis → ausentes; IMC só para ≥ 20 anos, categorias da OMS.
#  (m) Persistência = proporção de meses com APAC entre o 1º e o último mês
#      do paciente no período (proxy de continuidade, não de adesão real).
#  (n) AM e BI/PA são fontes distintas: não somar pacientes entre elas.
# ==============================================================================

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# --- PARÂMETROS ---------------------------------------------------------------
output_dir <- "/Users/davibarbosa/Desktop/Henrique"
# Pasta dos .dbc (FTP do DATASUS montado no Finder). Se não estiver acessível,
# os arquivos são baixados do FTP (PA via microdatasus).
dir_dbc    <- "/Volumes/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados"
url_ftp    <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"

anos        <- 2015:2025
meses_extra <- 6                 # meses lidos após o período (apresentação tardia)
cod_uf_al   <- "27"
cod_maceio  <- "270430"
idade_max   <- 110               # acima disso: idade implausível → NA
idade_min_sensibilidade <- 5     # sensibilidade: exclui < 5 anos
min_registros_mun <- 30          # mínimo por município para estabilidade
# Opcional: CSV de população municipal (colunas CO_MUN [6 dígitos], Ano, Pop)
arquivo_pop <- NA
# Nomes dos procedimentos/medicamentos: tenta microdatasus::fetch_sigtab();
# se falhar, usa este CSV (colunas COD [10 dígitos], NOME). NA = sem nomes.
arquivo_sigtap <- NA

# Faixas plausíveis para dados antropométricos da APAC de medicamentos
faixa_peso   <- c(2, 250)        # kg
faixa_altura <- c(40, 230)       # cm
faixa_imc    <- c(10, 80)        # kg/m²

regex_asma <- "^J4[56]"          # J45.x e J46

# --- BIBLIOTECAS --------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ragg)
library(officer)
library(flextable)
library(read.dbc)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir_cache_pa <- file.path(output_dir, "cache_asma_PA")
dir_cache_bi <- file.path(output_dir, "cache_asma_BI")
dir_cache_am <- file.path(output_dir, "cache_asma_AM")
dir.create(dir_cache_pa, showWarnings = FALSE)
dir.create(dir_cache_bi, showWarnings = FALSE)
dir.create(dir_cache_am, showWarnings = FALSE)

# --- FUNÇÕES AUXILIARES -------------------------------------------------------
salvar_grafico <- function(plot, nome_base, w = 10, h = 6) {
  ragg::agg_tiff(file.path(output_dir, paste0(nome_base, ".tiff")),
                 width = w, height = h, units = "in", res = 300, compression = "lzw")
  print(plot); dev.off()
  ragg::agg_png(file.path(output_dir, paste0(nome_base, ".png")),
                width = w, height = h, units = "in", res = 300)
  print(plot); dev.off()
}

ft <- function(df) {
  flextable(df) %>% theme_vanilla() %>% autofit() %>%
    fontsize(size = 10, part = "all") %>% bold(part = "header")
}

wilson_ci_pct <- function(k, n, conf = 0.95) {
  z     <- qnorm(1 - (1 - conf) / 2)
  p     <- k / n
  denom <- 1 + z^2 / n
  ctr   <- (p + z^2 / (2 * n)) / denom
  mgn   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  data.frame(IC_Inf_Pct = pmax(0, ctr - mgn) * 100,
             IC_Sup_Pct = pmin(100, ctr + mgn) * 100)
}

teste_distribuicao <- function(df, var, grupo = "Local") {
  tab <- table(df[[grupo]], df[[var]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (ncol(tab) < 2 || nrow(tab) < 2)
    return(tibble(Variavel = var, Qui2 = NA_real_, gl = NA_integer_,
                  P_Valor = NA_real_, V_Cramer = NA_real_, Magnitude = "Não testável"))
  qt <- suppressWarnings(chisq.test(tab))
  v  <- sqrt(as.numeric(qt$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  tibble(Variavel  = var,
         Qui2      = round(as.numeric(qt$statistic), 1),
         gl        = as.integer(qt$parameter),
         P_Valor   = signif(qt$p.value, 3),
         V_Cramer  = round(v, 3),
         Magnitude = case_when(v < 0.10 ~ "Desprezível",
                               v < 0.30 ~ "Pequena",
                               v < 0.50 ~ "Moderada",
                               TRUE     ~ "Grande"))
}

distribuicao <- function(df, var) {
  bind_rows(df %>% mutate(Local = "Alagoas (total)"), df) %>%
    count(Local, Categoria = as.character(.data[[var]])) %>%
    group_by(Local) %>%
    mutate(Pct = round(n / sum(n) * 100, 2)) %>%
    ungroup() %>%
    mutate(Variavel = var, .before = 1)
}

decod_sexo <- function(x) case_when(
  toupper(trimws(x)) %in% c("M", "1", "MASCULINO")     ~ "Masculino",
  toupper(trimws(x)) %in% c("F", "2", "3", "FEMININO") ~ "Feminino",
  TRUE                                                 ~ "Ignorado")

decod_raca <- function(x) case_when(
  x %in% c("01", "1") ~ "Branca",
  x %in% c("02", "2") ~ "Preta",
  x %in% c("03", "3") ~ "Parda",
  x %in% c("04", "4") ~ "Amarela",
  x %in% c("05", "5") ~ "Indígena",
  TRUE                ~ "Sem informação")         # inclui 99 (Informe)

decod_carater <- function(x) case_when(
  x %in% c("01", "1") ~ "Eletivo",
  x %in% c("02", "2") ~ "Urgência",
  x %in% c("03", "04", "05", "06", "3", "4", "5", "6") ~ "Acidente/lesão",
  TRUE                ~ "Ignorado")

faixa_etaria <- function(idade) {
  f <- cut(idade, breaks = c(-Inf, 4, 9, 19, 39, 59, Inf),
           labels = c("0–4", "5–9", "10–19", "20–39", "40–59", "60+"))
  fct_na_value_to_level(f, "Ignorada")
}

rotulo_cid <- c(J450 = "J45.0 Predominantemente alérgica",
                J451 = "J45.1 Não alérgica",
                J458 = "J45.8 Mista",
                J459 = "J45.9 Não especificada",
                J46  = "J46 Estado de mal asmático")

cid_asma <- function(cod) {
  cod <- if_else(substr(cod, 1, 3) == "J46", "J46", substr(cod, 1, 4))
  coalesce(unname(rotulo_cid[cod]), "J45 sem subcategoria válida")
}

rotulo_grupo_proc <- c("01" = "01 Promoção e prevenção",
                       "02" = "02 Finalidade diagnóstica",
                       "03" = "03 Procedimentos clínicos",
                       "04" = "04 Cirúrgicos",
                       "06" = "06 Medicamentos",
                       "07" = "07 Órteses/próteses/materiais",
                       "08" = "08 Ações complementares")

# ==============================================================================
# 1. EXTRAÇÃO — arquivo a arquivo, guardando só os registros de asma
# ==============================================================================

cols_pa <- c("PA_CMP", "PA_MVM", "PA_UFMUN", "PA_MUNPCN", "PA_PROC_ID",
             "PA_DOCORIG", "PA_AUTORIZ", "PA_CATEND", "PA_CIDPRI", "PA_CIDSEC",
             "PA_IDADE", "PA_FLIDADE", "PA_SEXO", "PA_RACACOR", "PA_ETNIA",
             "PA_QTDPRO", "PA_QTDAPR", "PA_VALAPR", "PA_INDICA", "PA_MNDIF",
             "PA_UFDIF", "PA_CBOCOD", "PA_TPUPS", "PA_CODUNI")

cols_bi <- c("DT_ATEND", "DT_PROCESS", "UFMUN", "MUNPAC", "PROC_ID", "CIDPRI",
             "CATEND", "CNS_PAC", "DTNASC", "TPIDADEPAC", "IDADEPAC", "SEXOPAC",
             "RACACOR", "QT_APROV", "MNDIF", "CBOPROF", "CODUNI")

cols_am <- c("AP_MVM", "AP_CMP", "AP_AUTORIZ", "AP_PRIPAL", "AP_VL_AP", "AP_UFMUN",
             "AP_CODUNI", "AP_CNSPCN", "AP_COIDADE", "AP_NUIDADE", "AP_SEXO",
             "AP_RACACOR", "AP_MUNPCN", "AP_MNDIF", "AP_DTINIC", "AP_DTFIM",
             "AP_TPAPAC", "AP_MOTSAI", "AP_OBITO", "AP_ENCERR", "AP_CATEND",
             "AP_APACANT", "AP_CIDPRI", "AP_CIDSEC", "AP_CIDCAS", "AM_PESO",
             "AM_ALTURA", "AM_GESTANT")

# Os .dbf do DATASUS vêm em latin1, e o CNS do paciente vem criptografado
# (bytes arbitrários). Converte para UTF-8 válido antes de qualquer operação
# de texto. A conversão byte→caractere é determinística, então o CNS continua
# servindo como identificador único do paciente.
utf8_seguro <- function(x) {
  x  <- as.character(x)
  ok <- is.na(x) | validUTF8(x)
  x[!ok] <- iconv(x[!ok], from = "latin1", to = "UTF-8")
  x
}

filtrar_asma <- function(df, cols, cid_cols) {
  dt <- as.data.table(df)
  dt <- dt[, intersect(cols, names(dt)), with = FALSE]
  dt[, (names(dt)) := lapply(.SD, function(x) trimws(utf8_seguro(x)))]
  cid_cols <- intersect(cid_cols, names(dt))
  dt[Reduce(`|`, lapply(cid_cols, function(cc) grepl(regex_asma, dt[[cc]])))]
}

# Arquivos esperados: período + meses_extra; inclui partes a, b, c...
periodos <- format(seq(as.Date(sprintf("%d-01-01", min(anos))), by = "month",
                       length.out = 12 * (max(anos) - min(anos) + 1) + meses_extra), "%y%m")

listar_arquivos <- function(prefixo) {
  if (dir.exists(dir_dbc)) {
    padrao <- sprintf("^%sAL(%s)[a-z]?\\.dbc$", prefixo, paste(periodos, collapse = "|"))
    list.files(dir_dbc, pattern = padrao, full.names = TRUE, ignore.case = TRUE)
  } else {
    paste0(url_ftp, prefixo, "AL", periodos, ".dbc")   # sem partes: ver nota
  }
}

ler_dbc <- function(caminho) {
  if (file.exists(caminho)) return(read.dbc(caminho, as.is = TRUE))
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp))
  download.file(caminho, tmp, mode = "wb", quiet = TRUE)
  read.dbc(tmp, as.is = TRUE)
}

extrair <- function(prefixo, dir_cache, cols, cid_cols) {
  arquivos <- listar_arquivos(prefixo)
  message(prefixo, ": ", length(arquivos), " arquivos")
  for (f in arquivos) {
    cache <- file.path(dir_cache, sub("\\.dbc$", ".rds", basename(f), ignore.case = TRUE))
    if (file.exists(cache)) next                    # retoma de onde parou
    message("  lendo ", basename(f))
    res <- tryCatch(filtrar_asma(ler_dbc(f), cols, cid_cols),
                    error = function(e) { message("  falhou: ", basename(f), " — ",
                                                  conditionMessage(e)); NULL })
    if (!is.null(res)) saveRDS(res, cache)
    gc()
  }
  rbindlist(lapply(list.files(dir_cache, "\\.rds$", full.names = TRUE), readRDS),
            fill = TRUE)
}

pa_raw <- extrair("PA", dir_cache_pa, cols_pa, c("PA_CIDPRI", "PA_CIDSEC"))
bi_raw <- extrair("BI", dir_cache_bi, cols_bi, "CIDPRI")
am_raw <- extrair("AM", dir_cache_am, cols_am, c("AP_CIDPRI", "AP_CIDSEC"))
saveRDS(list(pa = pa_raw, bi = bi_raw, am = am_raw),
        file.path(output_dir, "Base_Asma_SIA_AL_bruta.rds"))

# --- Tabela SIGTAP (código → nome do procedimento/medicamento) ---------------
carregar_sigtap <- function() {
  df <- tryCatch(as.data.frame(microdatasus::fetch_sigtab()), error = function(e) NULL)
  if (is.null(df) && !is.na(arquivo_sigtap) && file.exists(arquivo_sigtap))
    df <- fread(arquivo_sigtap, colClasses = "character")
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- as.data.frame(lapply(df, utf8_seguro), stringsAsFactors = FALSE)
  # Detecta a coluna de código (10 dígitos) e a de nome (texto mais longo)
  eh_cod  <- sapply(df, function(x) mean(grepl("^\\d{10}$", trimws(x)), na.rm = TRUE))
  if (max(eh_cod) < 0.5) return(NULL)
  col_cod <- names(df)[which.max(eh_cod)]
  outras  <- setdiff(names(df), col_cod)
  col_nom <- outras[which.max(sapply(df[outras], function(x) mean(nchar(x), na.rm = TRUE)))]
  # remove caracteres espúrios da tabela de origem (ex.: "Ǩ(POR CAPSULA")
  nome <- gsub("[^ -~À-ÿ]", "", df[[col_nom]])
  tibble(Codigo = trimws(df[[col_cod]]), Nome = trimws(gsub("\\s+", " ", nome))) %>%
    distinct(Codigo, .keep_all = TRUE)
}
sigtap <- carregar_sigtap()
nomear_proc <- function(df, col) {
  if (is.null(sigtap)) return(df %>% mutate(Nome = NA_character_))
  df %>% left_join(sigtap %>% rename(!!col := Codigo), by = col)
}
if (is.null(sigtap)) message("SIGTAP indisponível: procedimentos sairão só com código.")

# ==============================================================================
# 2. FLUXOGRAMA DE SELEÇÃO (PA) — cada exclusão é contada e reportada
# ==============================================================================

pa0 <- pa_raw %>%
  as_tibble() %>%
  mutate(Ano      = as.integer(substr(PA_CMP, 1, 4)),
         Mun_Res  = substr(PA_MUNPCN, 1, 6),
         DOCORIG  = toupper(PA_DOCORIG))

pa1 <- pa0 %>% filter(Ano %in% anos)
pa2 <- pa1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
pa3 <- pa2 %>% filter(DOCORIG %in% c("I", "P", "S"))
pa4 <- pa3 %>% filter(if ("PA_INDICA" %in% names(pa3)) PA_INDICA %in% c("5", "6")
                      else suppressWarnings(as.numeric(PA_QTDAPR)) > 0)

instrumentos_excluidos <- pa2 %>%
  filter(!DOCORIG %in% c("I", "P", "S")) %>%
  count(DOCORIG, name = "Linhas_excluidas")

fluxograma <- tibble(
  Etapa = c("Linhas do PA com CID J45/J46 (principal ou secundário)",
            sprintf("Competência (PA_CMP) em %d–%d", min(anos), max(anos)),
            "Residente em Alagoas (PA_MUNPCN = 27xxxx)",
            "Instrumento BPA-I ou APAC (exclui BPA-C e RAAS)",
            "Produção aprovada (PA_INDICA = 5 ou 6)"),
  Linhas = c(nrow(pa0), nrow(pa1), nrow(pa2), nrow(pa3), nrow(pa4))
) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# ==============================================================================
# 3. PREPARAÇÃO — variáveis em nível de linha
# ==============================================================================

pa_linhas <- pa4 %>%
  mutate(
    Instrumento    = if_else(DOCORIG == "I", "BPA-I", "APAC"),
    Asma_CIDPRI    = grepl(regex_asma, PA_CIDPRI),
    Asma_CIDSEC    = grepl(regex_asma, PA_CIDSEC),
    CID_Asma_Cod   = if_else(Asma_CIDPRI, PA_CIDPRI, PA_CIDSEC),
    Idade_Bruta    = suppressWarnings(as.integer(PA_IDADE)),
    Idade_Invalida = coalesce(PA_FLIDADE %in% c("3", "4"), FALSE) |
                     is.na(Idade_Bruta) | Idade_Bruta < 0 | Idade_Bruta > idade_max,
    Idade          = if_else(Idade_Invalida, NA_integer_, Idade_Bruta),
    Idade_Fora_SIGTAP = coalesce(PA_FLIDADE == "2", FALSE),
    Qtd_Aprov      = suppressWarnings(as.numeric(PA_QTDAPR)),
    Val_Aprov      = suppressWarnings(as.numeric(PA_VALAPR)),
    Fora_Municipio = if ("PA_MNDIF" %in% names(.)) PA_MNDIF == "1"
                     else substr(PA_UFMUN, 1, 6) != Mun_Res,
    Fora_Municipio = coalesce(Fora_Municipio, FALSE)
  )

# --- APAC de MEDICAMENTOS saem do PA: são as mesmas APAC do arquivo AM ------
# Verificado nos dados (AL, jun/2024): as APAC de asma no PA têm procedimento
# principal do grupo 06 (medicamentos) e coincidem 1:1 com os registros do AM.
# Mantê-las aqui misturaria dispensação com atendimento e contaria o mesmo
# paciente duas vezes. Elas são analisadas no módulo AM (seção 9b).
chaves_apac_med <- pa_linhas %>%
  filter(Instrumento == "APAC", DOCORIG == "P", substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP, Ano)
n_antes_med <- nrow(pa_linhas)
pa_linhas   <- pa_linhas %>%
  anti_join(chaves_apac_med %>% select(-Ano), by = c("PA_AUTORIZ", "PA_CMP"))
fluxograma <- bind_rows(
  fluxograma,
  tibble(Etapa = "Sem APAC de medicamentos (grupo 06; analisadas pelo arquivo AM)",
         Linhas = nrow(pa_linhas), Excluidas = n_antes_med - nrow(pa_linhas)))

# Procedimentos: contados em todas as linhas (cada linha = 1 procedimento)
procedimentos <- pa_linhas

# --- Unidade "atendimento": BPA-I = linha; APAC = 1 por (PA_AUTORIZ, PA_CMP) ---
apac_sem_autoriz <- pa_linhas %>%
  filter(Instrumento == "APAC", is.na(PA_AUTORIZ) | PA_AUTORIZ == "" |
           grepl("^0+$", PA_AUTORIZ)) %>% nrow()

atend_apac <- pa_linhas %>%
  filter(Instrumento == "APAC") %>%
  mutate(Chave = if_else(is.na(PA_AUTORIZ) | PA_AUTORIZ == "" | grepl("^0+$", PA_AUTORIZ),
                         paste0("SEM_", row_number()), PA_AUTORIZ)) %>%
  arrange(Chave, PA_CMP, DOCORIG != "P") %>%          # linha "P" primeiro
  group_by(Chave, PA_CMP) %>%
  mutate(Asma_CIDPRI  = any(Asma_CIDPRI),
         Asma_CIDSEC  = any(Asma_CIDSEC),
         CID_Asma_Cod = first(na.omit(CID_Asma_Cod)),
         Qtd_Aprov    = sum(Qtd_Aprov, na.rm = TRUE),
         Val_Aprov    = sum(Val_Aprov, na.rm = TRUE),
         N_Linhas_APAC = n()) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Chave)

atend <- bind_rows(pa_linhas %>% filter(Instrumento == "BPA-I"), atend_apac) %>%
  mutate(
    Local       = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
    Posicao_CID = case_when(
      Asma_CIDPRI                 ~ "Principal",
      Instrumento == "APAC"       ~ "Somente secundário (APAC)",
      TRUE                        ~ "Somente secundário"),
    Instr_Posicao = paste(Instrumento, "—", Posicao_CID),
    CID_Asma     = cid_asma(CID_Asma_Cod),
    Mal_Asmatico = substr(CID_Asma_Cod, 1, 3) == "J46",
    Faixa_Etaria = faixa_etaria(Idade),
    Pediatrico   = !is.na(Idade) & Idade < 20,
    Sexo         = decod_sexo(PA_SEXO),
    Feminino     = Sexo == "Feminino",
    Raca_Cor     = decod_raca(PA_RACACOR),
    Carater      = decod_carater(PA_CATEND),
    Urgencia     = Carater == "Urgência",
    Grupo_Proc   = coalesce(unname(rotulo_grupo_proc[substr(PA_PROC_ID, 1, 2)]), "Outro")
  )

fluxograma <- bind_rows(
  fluxograma,
  tibble(Etapa = "Atendimentos após colapsar APAC (1 por PA_AUTORIZ × PA_CMP)",
         Linhas = nrow(atend), Excluidas = nrow(pa_linhas) - nrow(atend))
)

# ==============================================================================
# 4. QUALIDADE E COMPLETUDE DOS DADOS
# ==============================================================================

completude <- atend %>%
  group_by(Instrumento) %>%
  summarise(
    N                        = n(),
    Pct_Sexo_Ignorado        = round(mean(Sexo == "Ignorado") * 100, 2),
    Pct_Raca_Sem_Info        = round(mean(Raca_Cor == "Sem informação") * 100, 2),
    Pct_Idade_Invalida       = round(mean(is.na(Idade)) * 100, 2),
    Pct_Idade_Fora_SIGTAP    = round(mean(Idade_Fora_SIGTAP) * 100, 2),
    Pct_Carater_Ignorado     = round(mean(Carater == "Ignorado") * 100, 2),
    Pct_CID_Sem_Subcategoria = round(mean(CID_Asma == "J45 sem subcategoria válida") * 100, 2),
    .groups = "drop"
  )

flag_idade <- pa_linhas %>%
  count(PA_FLIDADE, name = "Linhas") %>%
  mutate(Significado = case_when(
    PA_FLIDADE == "0" ~ "Idade não exigida",
    PA_FLIDADE == "1" ~ "Compatível com SIGTAP",
    PA_FLIDADE == "2" ~ "Fora da faixa do SIGTAP (sinalizada)",
    PA_FLIDADE == "3" ~ "Idade inexistente (→ NA)",
    PA_FLIDADE == "4" ~ "Idade em branco (→ NA)",
    TRUE              ~ "Sem flag"),
    Pct = round(Linhas / sum(Linhas) * 100, 2))

idades_extremas <- pa_linhas %>%
  summarise(Idade_maior_que_max = sum(!is.na(Idade_Bruta) & Idade_Bruta > idade_max),
            Idade_negativa      = sum(!is.na(Idade_Bruta) & Idade_Bruta < 0),
            Idade_nao_numerica  = sum(is.na(Idade_Bruta)))

# ==============================================================================
# 5. PERFIL GERAL — Alagoas total, Maceió e Interior (unidade: atendimento)
# ==============================================================================

resumo_perfil <- function(df) {
  df %>%
    summarise(
      N_Atendimentos   = n(),
      N_Procedimentos  = sum(Qtd_Aprov, na.rm = TRUE),
      Valor_Aprovado_R = round(sum(Val_Aprov, na.rm = TRUE), 2),
      Idade_Mediana    = median(Idade, na.rm = TRUE),
      Idade_P25        = quantile(Idade, 0.25, na.rm = TRUE),
      Idade_P75        = quantile(Idade, 0.75, na.rm = TRUE),
      N_Feminino       = sum(Feminino),
      N_Sexo_Valido    = sum(Sexo != "Ignorado"),
      N_Pediatrico     = sum(Pediatrico),
      N_Idade_Valida   = sum(!is.na(Idade)),
      N_Urgencia       = sum(Urgencia),
      N_Carater_Valido = sum(Carater != "Ignorado"),
      N_Mal_Asmatico   = sum(Mal_Asmatico),
      N_Fora_Mun       = sum(Fora_Municipio),
      .groups = "drop")
}

perfil_geral <- bind_rows(atend %>% mutate(Local = "Alagoas (total)"), atend) %>%
  group_by(Local) %>%
  resumo_perfil() %>%
  mutate(Local = factor(Local, levels = c("Alagoas (total)", "Maceió", "Interior"))) %>%
  arrange(Local)

# Denominadores = registros com a informação válida (ignorados fora do cálculo)
indicadores_def <- tribble(
  ~Indicador,                                     ~k,               ~n,
  "Sexo feminino (%)",                            "N_Feminino",     "N_Sexo_Valido",
  "Idade < 20 anos (%)",                          "N_Pediatrico",   "N_Idade_Valida",
  "Caráter de urgência (%)",                      "N_Urgencia",     "N_Carater_Valido",
  "Estado de mal asmático — J46 (%)",             "N_Mal_Asmatico", "N_Atendimentos",
  "Atendido fora do município de residência (%)", "N_Fora_Mun",     "N_Atendimentos"
)

calc_indicadores <- function(perfil, chave) {
  bind_rows(lapply(seq_len(nrow(indicadores_def)), function(i) {
    k <- perfil[[indicadores_def$k[i]]]
    n <- perfil[[indicadores_def$n[i]]]
    tibble(!!chave := perfil[[chave]], Indicador = indicadores_def$Indicador[i],
           k = k, n = n, Pct = round(k / n * 100, 2)) %>%
      bind_cols(wilson_ci_pct(k, n) %>% round(2))
  }))
}

indicadores <- calc_indicadores(perfil_geral, "Local")

# Mesmos indicadores por instrumento (BPA-I e APAC têm populações diferentes)
indicadores_instrumento <- atend %>%
  group_by(Instrumento) %>%
  resumo_perfil() %>%
  calc_indicadores("Instrumento")

# ==============================================================================
# 6. DISTRIBUIÇÕES DEMOGRÁFICAS E CLÍNICAS + TESTES (Maceió × Interior)
#    Categorias "Ignorado"/"Sem informação" são mostradas nas distribuições,
#    mas retiradas dos testes para não distorcer a comparação.
# ==============================================================================

vars_dist <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "CID_Asma", "Instr_Posicao",
               "Carater", "Grupo_Proc")

distribuicoes <- bind_rows(lapply(vars_dist, function(v) distribuicao(atend, v)))

testes <- bind_rows(lapply(vars_dist, function(v) {
  df <- atend %>%
    filter(!as.character(.data[[v]]) %in% c("Ignorado", "Ignorada", "Sem informação"))
  teste_distribuicao(df, v)
}))

cruzamento_idade_sexo <- atend %>%
  filter(Sexo != "Ignorado", Faixa_Etaria != "Ignorada") %>%
  group_by(Faixa_Etaria, Sexo) %>%
  summarise(N            = n(),
            Pct_Urgencia = round(sum(Urgencia) / pmax(sum(Carater != "Ignorado"), 1) * 100, 2),
            Pct_J46      = round(mean(Mal_Asmatico) * 100, 2),
            .groups = "drop")

top_procedimentos <- procedimentos %>%
  count(Instrumento, PA_PROC_ID, wt = Qtd_Aprov, name = "Procedimentos") %>%
  group_by(Instrumento) %>%
  mutate(Pct = round(Procedimentos / sum(Procedimentos) * 100, 2)) %>%
  slice_max(Procedimentos, n = 10) %>%
  ungroup() %>%
  nomear_proc("PA_PROC_ID")

# ==============================================================================
# 7. SENSIBILIDADE — excluindo < 5 anos (diagnóstico de asma pouco específico)
# ==============================================================================

sensibilidade <- bind_rows(
  atend %>% mutate(Cenario = "Principal (todas as idades)"),
  atend %>% filter(!is.na(Idade), Idade >= idade_min_sensibilidade) %>%
    mutate(Cenario = sprintf("Sensibilidade (idade ≥ %d anos)", idade_min_sensibilidade))
) %>%
  group_by(Cenario) %>%
  resumo_perfil() %>%
  calc_indicadores("Cenario")

# ==============================================================================
# 8. TENDÊNCIA TEMPORAL
# ==============================================================================

tendencia <- atend %>%
  group_by(Ano) %>%
  summarise(N_Atendimentos  = n(),
            N_BPA_I         = sum(Instrumento == "BPA-I"),
            N_APAC          = sum(Instrumento == "APAC"),
            Pct_Urgencia    = round(sum(Urgencia) / pmax(sum(Carater != "Ignorado"), 1) * 100, 2),
            Pct_Pediatrico  = round(sum(Pediatrico) / pmax(sum(!is.na(Idade)), 1) * 100, 2),
            Pct_J46         = round(mean(Mal_Asmatico) * 100, 2),
            .groups = "drop")

tendencia_faixa <- atend %>%
  filter(Faixa_Etaria != "Ignorada") %>%
  count(Ano, Faixa_Etaria, name = "N_Atendimentos")

# ==============================================================================
# 9. PACIENTES ÚNICOS — arquivo BI (BPA-I), CNS_PAC como identificador
# ==============================================================================

bi <- bi_raw %>%
  as_tibble() %>%
  mutate(Ano = as.integer(substr(DT_ATEND, 1, 4)))

fluxo_bi <- tibble(
  Etapa  = c("Linhas do BI com CIDPRI J45/J46", "Competência no período",
             "Residente em AL (MUNPAC)", "Quantidade aprovada > 0",
             "CNS do paciente válido"),
  Linhas = NA_integer_)

bi1 <- bi %>% filter(Ano %in% anos)
bi2 <- bi1 %>% filter(substr(MUNPAC, 1, 2) == cod_uf_al)
bi3 <- bi2 %>% filter(suppressWarnings(as.numeric(QT_APROV)) > 0)
# CNS vem criptografado (bytes fora do UTF-8): comparar em bytes
cns_valido <- function(x) !is.na(x) & x != "" & !grepl("^(.)\\1*$", x, useBytes = TRUE)
bi4 <- bi3 %>% filter(cns_valido(CNS_PAC))
fluxo_bi$Linhas    <- c(nrow(bi), nrow(bi1), nrow(bi2), nrow(bi3), nrow(bi4))
fluxo_bi$Excluidas <- lag(fluxo_bi$Linhas) - fluxo_bi$Linhas

# Idade recalculada pela data de nascimento (mais confiável que IDADEPAC)
bi_atend <- bi4 %>%
  mutate(
    Data_Atend = as.Date(paste0(DT_ATEND, "15"), "%Y%m%d"),
    Data_Nasc  = as.Date(DTNASC, "%Y%m%d"),
    Idade      = floor(as.numeric(Data_Atend - Data_Nasc) / 365.25),
    Idade      = if_else(is.na(Idade) | Idade < 0 | Idade > idade_max, NA_real_, Idade),
    Sexo       = decod_sexo(SEXOPAC),
    Raca_Cor   = decod_raca(RACACOR),
    Mun_Res    = substr(MUNPAC, 1, 6),
    Local      = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
    Mal_Asmatico = substr(CIDPRI, 1, 3) == "J46",
    Urgencia     = decod_carater(CATEND) == "Urgência"
  )

# Um registro por paciente: características no PRIMEIRO atendimento do período
pacientes <- bi_atend %>%
  arrange(CNS_PAC, Data_Atend) %>%
  group_by(CNS_PAC) %>%
  summarise(
    Primeiro_Atend   = first(Data_Atend),
    Ano_Primeiro     = first(Ano),
    Idade_Primeiro   = first(Idade),
    Sexo             = first(Sexo),
    Sexo_Inconsist   = n_distinct(Sexo[Sexo != "Ignorado"]) > 1,
    Nasc_Inconsist   = n_distinct(na.omit(Data_Nasc)) > 1,
    Raca_Cor         = first(Raca_Cor),
    Local            = first(Local),
    N_Atendimentos   = n(),
    N_Anos_Com_Atend = n_distinct(Ano),
    Teve_J46         = any(Mal_Asmatico),
    Teve_Urgencia    = any(Urgencia),
    .groups = "drop") %>%
  mutate(Faixa_Etaria = faixa_etaria(Idade_Primeiro),
         Freq_Atend   = cut(N_Atendimentos, c(0, 1, 2, 5, 11, Inf),
                            labels = c("1", "2", "3–5", "6–11", "12+")))

resumo_pacientes <- pacientes %>%
  summarise(
    N_Pacientes            = n(),
    Atend_por_Paciente_Med = median(N_Atendimentos),
    Atend_por_Paciente_P75 = quantile(N_Atendimentos, 0.75),
    Pct_Retorno            = round(mean(N_Atendimentos > 1) * 100, 2),
    Pct_Feminino           = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
    Idade_Mediana_1o_Atend = median(Idade_Primeiro, na.rm = TRUE),
    Pct_Sexo_Inconsistente = round(mean(Sexo_Inconsist) * 100, 2),
    Pct_Nasc_Inconsistente = round(mean(Nasc_Inconsist) * 100, 2))

pacientes_por_ano <- bi_atend %>%
  group_by(Ano) %>%
  summarise(Pacientes_Unicos = n_distinct(CNS_PAC),
            Atendimentos     = n(),
            Atend_por_Pac    = round(Atendimentos / Pacientes_Unicos, 2),
            .groups = "drop") %>%
  left_join(pacientes %>% count(Ano = Ano_Primeiro, name = "Pacientes_Novos_no_Periodo"),
            by = "Ano")

dist_pacientes <- bind_rows(
  pacientes %>% count(Variavel = "Sexo",         Categoria = Sexo),
  pacientes %>% count(Variavel = "Faixa_Etaria", Categoria = as.character(Faixa_Etaria)),
  pacientes %>% count(Variavel = "Raca_Cor",     Categoria = Raca_Cor),
  pacientes %>% count(Variavel = "Freq_Atend",   Categoria = as.character(Freq_Atend))
) %>%
  group_by(Variavel) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# Consistência PA (DOCORIG = I) × BI: devem ser próximos ano a ano
consistencia_pa_bi <- full_join(
  atend %>% filter(Instrumento == "BPA-I") %>% count(Ano, name = "PA_BPA_I"),
  bi3 %>% count(Ano, name = "BI_aprovado"),
  by = "Ano") %>%
  mutate(Diferenca_Pct = round((BI_aprovado - PA_BPA_I) / PA_BPA_I * 100, 2)) %>%
  arrange(Ano)

# ==============================================================================
# 9b. APAC DE MEDICAMENTOS (AM) — tratamento no Componente Especializado
# ==============================================================================

classe_med <- function(nome) {
  n <- toupper(coalesce(nome, ""))
  case_when(
    n == ""                                                   ~ "Sem nome (SIGTAP indisponível)",
    grepl("OMALIZUMABE|MEPOLIZUMABE|BENRALIZUMABE|DUPILUMABE", n) ~ "Imunobiológico",
    grepl("FORMOTEROL|SALMETEROL|VILANTEROL", n) &
      grepl("BUDESONIDA|FLUTICASONA|BECLOMETASONA|MOMETASONA", n) ~ "Corticoide inalatório + LABA",
    grepl("FORMOTEROL|SALMETEROL", n)                         ~ "LABA isolado",
    grepl("BUDESONIDA|BECLOMETASONA|FLUTICASONA|CICLESONIDA", n) ~ "Corticoide inalatório",
    grepl("FENOTEROL|SALBUTAMOL|TERBUTALINA", n)              ~ "Broncodilatador de curta ação",
    TRUE                                                      ~ "Outro")
}

am0 <- am_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(AP_CMP, 1, 4)), Mun_Res = substr(AP_MUNPCN, 1, 6))
am1 <- am0 %>% filter(Ano %in% anos)
am2 <- am1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
am3 <- am2 %>% distinct(AP_AUTORIZ, AP_CMP, AP_PRIPAL, .keep_all = TRUE)
# O AM não tem campo de aprovação; verificado nos dados que ele inclui as APAC
# não aprovadas (~12% em 2024). Para seguir a mesma regra do PA, mantêm-se só
# as APAC aprovadas no PA (PA_INDICA 5/6), pareadas por AUTORIZ × competência.
am3a <- am3 %>%
  semi_join(chaves_apac_med %>% select(-Ano),
            by = c("AP_AUTORIZ" = "PA_AUTORIZ", "AP_CMP" = "PA_CMP"))
am4 <- am3a %>% filter(cns_valido(AP_CNSPCN))

fluxo_am <- tibble(
  Etapa  = c("Registros AM com CID J45/J46 (principal ou secundário)",
             "Competência no período", "Residente em AL (AP_MUNPCN)",
             "Sem duplicatas (AP_AUTORIZ × AP_CMP × AP_PRIPAL)",
             "APAC aprovada (pareada com PA_INDICA 5/6 no PA)",
             "CNS do paciente válido"),
  Linhas = c(nrow(am0), nrow(am1), nrow(am2), nrow(am3), nrow(am3a), nrow(am4))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# Consistência: APAC de medicamentos retiradas do PA × registros do AM
# (o AM não tem campo de aprovação; por isso o PA é contado com e sem o filtro)
consistencia_pa_am <- pa3 %>%
  filter(DOCORIG == "P", substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP, Ano) %>%
  count(Ano, name = "PA_APAC_med_todas") %>%
  full_join(chaves_apac_med %>% count(Ano, name = "PA_APAC_med_aprovadas"), by = "Ano") %>%
  full_join(am3 %>% distinct(AP_AUTORIZ, AP_CMP, Ano) %>% count(Ano, name = "AM_APAC"),
            by = "Ano") %>%
  mutate(Dif_AM_vs_PA_todas_Pct = round((AM_APAC - PA_APAC_med_todas) /
                                          PA_APAC_med_todas * 100, 2)) %>%
  arrange(Ano)

am <- am4 %>%
  mutate(
    Num_Idade = suppressWarnings(as.integer(AP_NUIDADE)),
    Idade = case_when(AP_COIDADE == "4"            ~ Num_Idade,
                      AP_COIDADE == "5"            ~ 100L + Num_Idade,
                      AP_COIDADE %in% c("2", "3")  ~ 0L,
                      TRUE                         ~ NA_integer_),
    Idade  = if_else(!is.na(Idade) & (Idade < 0 | Idade > idade_max), NA_integer_, Idade),
    Sexo   = decod_sexo(AP_SEXO),
    Raca_Cor = decod_raca(AP_RACACOR),
    Local  = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
    Asma_CIDPRI = grepl(regex_asma, AP_CIDPRI),
    CID_Asma = cid_asma(if_else(Asma_CIDPRI, AP_CIDPRI, AP_CIDSEC)),
    Peso   = suppressWarnings(as.numeric(AM_PESO)),
    Peso   = if_else(between(Peso, faixa_peso[1], faixa_peso[2]), Peso, NA_real_),
    Altura = suppressWarnings(as.numeric(AM_ALTURA)),
    Altura = if_else(between(Altura, faixa_altura[1], faixa_altura[2]), Altura, NA_real_),
    IMC    = Peso / (Altura / 100)^2,
    IMC    = if_else(between(IMC, faixa_imc[1], faixa_imc[2]), IMC, NA_real_),
    Inicial = AP_TPAPAC == "1",
    Gestante = toupper(AM_GESTANT) == "S",
    Valor  = suppressWarnings(as.numeric(AP_VL_AP))
  ) %>%
  nomear_proc("AP_PRIPAL") %>%
  rename(Medicamento = Nome) %>%
  mutate(Classe = classe_med(Medicamento))

completude_am <- am %>%
  summarise(N_Registros            = n(),
            Pct_Idade_Invalida     = round(mean(is.na(Idade)) * 100, 2),
            Pct_Sexo_Ignorado      = round(mean(Sexo == "Ignorado") * 100, 2),
            Pct_Raca_Sem_Info      = round(mean(Raca_Cor == "Sem informação") * 100, 2),
            Pct_Peso_Invalido      = round(mean(is.na(Peso)) * 100, 2),
            Pct_Altura_Invalida    = round(mean(is.na(Altura)) * 100, 2),
            Pct_IMC_Invalido       = round(mean(is.na(IMC)) * 100, 2),
            Pct_Asma_CID_Principal = round(mean(Asma_CIDPRI) * 100, 2))

# Paciente: características na 1ª APAC do período; antropometria = mediana
# dos valores válidos do paciente (reduz erro de digitação pontual)
pacientes_am <- am %>%
  arrange(AP_CNSPCN, AP_CMP) %>%
  group_by(AP_CNSPCN) %>%
  summarise(
    Primeiro_CMP  = first(AP_CMP),
    Ultimo_CMP    = last(AP_CMP),
    Ano_Primeiro  = first(Ano),
    Idade         = first(Idade),
    Sexo          = first(Sexo),
    Raca_Cor      = first(Raca_Cor),
    Local         = first(Local),
    Inicio_Obs    = first(Inicial),               # 1ª APAC do período é "inicial"
    Meses_Com_APAC = n_distinct(AP_CMP),
    N_Medicamentos = n_distinct(AP_PRIPAL),
    Usou_Biologico = any(Classe == "Imunobiológico"),
    Classe_Principal = names(which.max(table(Classe))),
    IMC           = median(IMC, na.rm = TRUE),
    Gestante_Algum_Mes = any(Gestante, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    IMC = if_else(is.nan(IMC), NA_real_, IMC),
    Meses_Janela = (as.integer(substr(Ultimo_CMP, 1, 4)) - as.integer(substr(Primeiro_CMP, 1, 4))) * 12 +
                   as.integer(substr(Ultimo_CMP, 5, 6)) - as.integer(substr(Primeiro_CMP, 5, 6)) + 1,
    Persistencia_Pct = round(Meses_Com_APAC / Meses_Janela * 100, 1),
    Faixa_Etaria = faixa_etaria(Idade),
    IMC_Classe = case_when(
      is.na(IMC) | is.na(Idade) | Idade < 20 ~ "Não classificado (< 20 anos ou sem dado)",
      IMC < 18.5 ~ "Baixo peso (< 18,5)",
      IMC < 25   ~ "Eutrofia (18,5–24,9)",
      IMC < 30   ~ "Sobrepeso (25–29,9)",
      TRUE       ~ "Obesidade (≥ 30)"),
    Duracao = cut(Meses_Com_APAC, c(0, 1, 5, 11, 23, Inf),
                  labels = c("1 mês", "2–5", "6–11", "12–23", "24+ meses")))

resumo_am <- pacientes_am %>%
  summarise(
    N_Pacientes             = n(),
    Pct_Feminino            = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
    Idade_Mediana           = median(Idade, na.rm = TRUE),
    Meses_APAC_Mediana      = median(Meses_Com_APAC),
    Pct_6_Meses_ou_Mais     = round(mean(Meses_Com_APAC >= 6) * 100, 2),
    Persistencia_Mediana    = median(Persistencia_Pct),
    Pct_Biologico           = round(mean(Usou_Biologico) * 100, 2),
    Pct_Obesidade_Adultos   = round(sum(IMC_Classe == "Obesidade (≥ 30)") /
                                      pmax(sum(!grepl("^Não", IMC_Classe)), 1) * 100, 2),
    IMC_Mediano_Adultos     = round(median(IMC[!is.na(Idade) & Idade >= 20], na.rm = TRUE), 1))

pacientes_am_ano <- am %>%
  group_by(Ano) %>%
  summarise(Pacientes_em_Tratamento = n_distinct(AP_CNSPCN),
            APAC_Iniciais           = n_distinct(AP_CNSPCN[Inicial %in% TRUE]),
            Valor_Aprovado_R        = round(sum(Valor, na.rm = TRUE), 2),
            .groups = "drop")

dist_am <- bind_rows(
  pacientes_am %>% count(Variavel = "Sexo",         Categoria = Sexo),
  pacientes_am %>% count(Variavel = "Faixa_Etaria", Categoria = as.character(Faixa_Etaria)),
  pacientes_am %>% count(Variavel = "Raca_Cor",     Categoria = Raca_Cor),
  pacientes_am %>% count(Variavel = "IMC_Classe",   Categoria = IMC_Classe),
  pacientes_am %>% count(Variavel = "Duracao",      Categoria = as.character(Duracao)),
  pacientes_am %>% count(Variavel = "Classe_Principal", Categoria = Classe_Principal),
  pacientes_am %>% count(Variavel = "Local",        Categoria = Local)
) %>%
  group_by(Variavel) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup()

medicamentos_am <- am %>%
  group_by(AP_PRIPAL, Medicamento, Classe) %>%
  summarise(Pacientes = n_distinct(AP_CNSPCN), Pacientes_Mes = n(),
            Valor_Aprovado_R = round(sum(Valor, na.rm = TRUE), 2), .groups = "drop") %>%
  arrange(desc(Pacientes))

# Cruzamento clínico × demográfico: IMC (adultos) por sexo e faixa etária
imc_sexo_faixa <- pacientes_am %>%
  filter(!grepl("^Não", IMC_Classe), Sexo != "Ignorado") %>%
  count(Sexo, Faixa_Etaria, IMC_Classe) %>%
  group_by(Sexo, Faixa_Etaria) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 9c. MEDICAMENTO × PERFIL DO PACIENTE
#   Etapa de tratamento = degrau terapêutico MAIS ALTO dispensado ao paciente no
#   período (escalonamento do PCDT de asma): CI ou LABA isolado < CI+LABA com
#   budesonida 200 < CI+LABA com budesonida 400 < imunobiológico.
#   É um proxy de gravidade/controle: não substitui avaliação clínica.
# ------------------------------------------------------------------------------
rotulo_etapa <- c("0 Outro / sem nome", "1 CI ou LABA isolado",
                  "2 CI+LABA (budesonida 200 ou outro)", "3 CI+LABA (budesonida 400)",
                  "4 Imunobiológico")

etapa_num <- function(classe, medicamento) {
  case_when(
    classe == "Imunobiológico"                                         ~ 4L,
    classe == "Corticoide inalatório + LABA" &
      grepl("BUDESONIDA 400", toupper(medicamento))                    ~ 3L,
    classe == "Corticoide inalatório + LABA"                           ~ 2L,
    classe %in% c("Corticoide inalatório", "LABA isolado")             ~ 1L,
    TRUE                                                               ~ 0L)
}

etapa_paciente <- am %>%
  mutate(Etapa_Num = etapa_num(Classe, Medicamento)) %>%
  group_by(AP_CNSPCN) %>%
  summarise(Etapa_Max = factor(rotulo_etapa[max(Etapa_Num) + 1], levels = rotulo_etapa),
            Escalonou = n_distinct(Etapa_Num) > 1,
            .groups = "drop")

pacientes_am <- pacientes_am %>%
  left_join(etapa_paciente, by = "AP_CNSPCN") %>%
  mutate(Etapa_Max = droplevels(Etapa_Max))

ignorados <- c("Ignorado", "Ignorada", "Sem informação",
               "Não classificado (< 20 anos ou sem dado)")
vars_perfil_am <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "Local", "IMC_Classe", "Duracao")

# Distribuição da etapa de tratamento dentro de cada categoria demográfica/clínica
tratamento_perfil <- bind_rows(lapply(vars_perfil_am, function(v) {
  pacientes_am %>%
    filter(!as.character(.data[[v]]) %in% ignorados) %>%
    count(Categoria = as.character(.data[[v]]), Etapa_Max) %>%
    group_by(Categoria) %>%
    mutate(N_Categoria = sum(n), Pct = round(n / N_Categoria * 100, 2)) %>%
    ungroup() %>%
    mutate(Variavel = v, .before = 1)
}))

# Associação etapa × variável (qui-quadrado + V de Cramér, sem ignorados).
# Etapas raras (imunobiológico) geram caselas esparsas: interpretar com cautela.
testes_tratamento <- bind_rows(lapply(vars_perfil_am, function(v) {
  df <- pacientes_am %>% filter(!as.character(.data[[v]]) %in% ignorados)
  teste_distribuicao(df, "Etapa_Max", grupo = v) %>% mutate(Variavel = v)
}))

# Pacientes com imunobiológico: perfil detalhado (n pequeno → só descritivo)
perfil_biologico <- pacientes_am %>%
  group_by(Usou_Biologico) %>%
  summarise(Pacientes          = n(),
            Pct_Feminino       = round(sum(Sexo == "Feminino") / pmax(sum(Sexo != "Ignorado"), 1) * 100, 2),
            Idade_Mediana      = median(Idade, na.rm = TRUE),
            IMC_Mediano_Adulto = round(median(IMC[!is.na(Idade) & Idade >= 20], na.rm = TRUE), 1),
            Meses_APAC_Mediana = median(Meses_Com_APAC),
            Pct_Maceio         = round(mean(Local == "Maceió") * 100, 2),
            .groups = "drop")

# ------------------------------------------------------------------------------
# 9d. VÍNCULO ATENDIMENTO (BI) × TRATAMENTO (AM) PELO CNS
#   O CNS vem criptografado, mas com a mesma chave nos dois arquivos (verificado:
#   centenas de CNS coincidem, o que não ocorreria ao acaso). Permite saber quais
#   pacientes atendidos em BPA-I também recebem medicamento pelo Componente
#   Especializado. Limitações: o BI só cobre BPA-I (APAC de procedimento e
#   atendimentos em atenção básica/e-SUS não entram) e a ordem temporal entre
#   atendimento e início do medicamento não é considerada.
# ------------------------------------------------------------------------------
cns_am <- unique(am$AP_CNSPCN)
pacientes <- pacientes %>%
  mutate(Em_Tratamento_AM = if_else(CNS_PAC %in% cns_am, "Recebe medicamento (AM)",
                                    "Sem APAC de medicamento"))

vinculo_bi_am <- tibble(
  Pacientes_BI     = nrow(pacientes),
  Pacientes_AM     = length(cns_am),
  Em_Ambos         = sum(pacientes$Em_Tratamento_AM == "Recebe medicamento (AM)"),
  Pct_BI_com_AM    = round(Em_Ambos / Pacientes_BI * 100, 2),
  Pct_AM_com_BI    = round(Em_Ambos / Pacientes_AM * 100, 2))

vinculo_perfil <- pacientes %>%
  group_by(Grupo = Em_Tratamento_AM) %>%
  summarise(Pacientes          = n(),
            Idade_Mediana      = median(Idade_Primeiro, na.rm = TRUE),
            Pct_Feminino       = round(sum(Sexo == "Feminino") / pmax(sum(Sexo != "Ignorado"), 1) * 100, 2),
            Atend_Mediana      = median(N_Atendimentos),
            k_Urgencia         = sum(Teve_Urgencia),
            Pct_Teve_Urgencia  = round(k_Urgencia / Pacientes * 100, 2),
            Pct_Teve_J46       = round(mean(Teve_J46) * 100, 2),
            .groups = "drop") %>%
  bind_cols(wilson_ci_pct(.$k_Urgencia, .$Pacientes) %>% round(2) %>%
              rename(Urg_IC_Inf = IC_Inf_Pct, Urg_IC_Sup = IC_Sup_Pct)) %>%
  select(-k_Urgencia)

teste_vinculo_urgencia <- teste_distribuicao(
  pacientes %>% mutate(Teve_Urgencia = if_else(Teve_Urgencia, "Sim", "Não")),
  "Teve_Urgencia", grupo = "Em_Tratamento_AM")

# Cobertura de tratamento entre atendidos, por faixa etária (IC 95% Wilson)
vinculo_faixa <- pacientes %>%
  filter(Faixa_Etaria != "Ignorada") %>%
  group_by(Faixa_Etaria) %>%
  summarise(Pacientes_BI = n(),
            Com_AM       = sum(Em_Tratamento_AM == "Recebe medicamento (AM)"),
            .groups = "drop") %>%
  mutate(Pct = round(Com_AM / Pacientes_BI * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$Com_AM, .$Pacientes_BI) %>% round(2))

# ------------------------------------------------------------------------------
# 9e. DESFECHOS APÓS CADA MEDICAMENTO
#   O SIASUS é ambulatorial: não registra alta hospitalar nem reinternação (isso
#   está no SIH/RD, cujos arquivos públicos não trazem CNS e, portanto, não
#   permitem ligar o paciente). O que o SIASUS permite medir é:
#   (a) Desfecho do TRATAMENTO por episódio (paciente × medicamento), em ordem
#       hierárquica: óbito registrado na APAC > troca para outro medicamento
#       (escalonou / mesma etapa / reduziu) > em uso no fim dos dados > saída
#       registrada (encerramento administrativo / transferência) > interrompeu.
#       "Em uso" = última dispensação nos 3 últimos meses dos dados.
#   (b) Desfecho CLÍNICO: atendimentos de asma em BPA-I (BI) nos 12 meses antes
#       × 12 meses depois do início do 1º medicamento, nos pacientes ligados
#       pelo CNS. Exige 12 meses observáveis antes e depois do início (exclui
#       quem começou no 1º ano dos dados, o que também afasta início anterior
#       ao período). O mês do início fica de fora das duas janelas.
#   Limitações de (b): sem grupo controle; regressão à média (o medicamento
#   costuma começar após uma piora) e tendência secular; o BI cobre só BPA-I.
# ------------------------------------------------------------------------------
mes_idx <- function(cmp) as.integer(substr(cmp, 1, 4)) * 12L + as.integer(substr(cmp, 5, 6)) - 1L
fim_dados_am <- mes_idx(max(am$AP_CMP))

rotulo_motsai <- c("21" = "Permanência (característica da doença)",
                   "31" = "Transferência", "41" = "Óbito (DO do médico assistente)",
                   "42" = "Óbito (DO do IML)", "43" = "Óbito (DO do SVO)",
                   "51" = "Encerramento administrativo")

# Motivo de saída como registrado em cada APAC (mostra que "alta" não é usada)
motivo_saida_am <- am %>%
  count(Motivo = coalesce(rotulo_motsai[AP_MOTSAI], paste("Código", AP_MOTSAI)),
        name = "APAC") %>%
  mutate(Pct = round(APAC / sum(APAC) * 100, 3)) %>%
  arrange(desc(APAC))

med_curto <- function(m) {
  m <- toupper(m)
  case_when(
    grepl("FORMOTEROL 12.*BUDESONIDA 400.*CAPSULA", m) ~ "Formoterol 12 + budesonida 400 (cápsula)",
    grepl("FORMOTEROL 6.*BUDESONIDA 200", m)           ~ "Formoterol 6 + budesonida 200 (cápsula)",
    grepl("MEPOLIZUMABE", m)                           ~ "Mepolizumabe",
    grepl("OMALIZUMABE", m)                            ~ "Omalizumabe",
    TRUE ~ "Outros (budesonida, salmeterol ou formoterol isolados; pó inalante)")
}

niveis_desfecho <- c("Em uso no fim dos dados", "Escalonou (etapa maior)",
                     "Trocou (mesma etapa)", "Reduziu (etapa menor)",
                     "Saída registrada (encerramento/transferência)",
                     "Interrompeu a dispensação", "Óbito registrado")

am_mes <- am %>%
  mutate(Mes = mes_idx(AP_CMP),
         Etapa_Num = etapa_num(Classe, Medicamento),
         Obito  = AP_OBITO == "1" | AP_MOTSAI %in% c("41", "42", "43"),
         Saida  = AP_MOTSAI %in% c("31", "51"))

episodios <- am_mes %>%
  group_by(AP_CNSPCN, AP_PRIPAL, Medicamento, Etapa_Num) %>%
  summarise(Ini = min(Mes), Fim = max(Mes), Meses_Dispensados = n_distinct(Mes),
            Obito = any(Obito), Saida = any(Saida), .groups = "drop")

# Próximo medicamento iniciado DEPOIS do início deste episódio
proximo_med <- episodios %>%
  select(AP_CNSPCN, AP_PRIPAL, Ini) %>%
  inner_join(episodios %>% select(AP_CNSPCN, Prox_PRIPAL = AP_PRIPAL, Prox_Ini = Ini,
                                  Prox_Etapa = Etapa_Num),
             by = "AP_CNSPCN", relationship = "many-to-many") %>%
  filter(Prox_PRIPAL != AP_PRIPAL, Prox_Ini > Ini) %>%
  group_by(AP_CNSPCN, AP_PRIPAL) %>%
  slice_min(Prox_Ini, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(AP_CNSPCN, AP_PRIPAL, Prox_Etapa)

episodios <- episodios %>%
  left_join(proximo_med, by = c("AP_CNSPCN", "AP_PRIPAL")) %>%
  mutate(
    Desfecho = case_when(
      Obito                                        ~ "Óbito registrado",
      !is.na(Prox_Etapa) & Prox_Etapa > Etapa_Num  ~ "Escalonou (etapa maior)",
      !is.na(Prox_Etapa) & Prox_Etapa < Etapa_Num  ~ "Reduziu (etapa menor)",
      !is.na(Prox_Etapa)                           ~ "Trocou (mesma etapa)",
      Fim >= fim_dados_am - 2                      ~ "Em uso no fim dos dados",
      Saida                                        ~ "Saída registrada (encerramento/transferência)",
      TRUE                                         ~ "Interrompeu a dispensação"),
    Desfecho = factor(Desfecho, levels = niveis_desfecho),
    Med_Curto = med_curto(Medicamento),
    Meses_Janela = Fim - Ini + 1)

# Desfecho por medicamento (% dos episódios)
desfecho_medicamento <- episodios %>%
  count(Medicamento = Med_Curto, Desfecho) %>%
  group_by(Medicamento) %>%
  mutate(Episodios = sum(n), Pct = round(n / Episodios * 100, 2)) %>%
  ungroup()

# Duração e interrupção precoce: só episódios com ≥ 9 meses de observação
# possível até o fim dos dados (senão a interrupção precoce é subestimada)
duracao_medicamento <- episodios %>%
  group_by(Medicamento = Med_Curto) %>%
  summarise(
    Episodios           = n(),
    Meses_Disp_Mediana  = median(Meses_Dispensados),
    Meses_Disp_P25      = quantile(Meses_Dispensados, 0.25),
    Meses_Disp_P75      = quantile(Meses_Dispensados, 0.75),
    Pct_Cobertura_Mediana = round(median(Meses_Dispensados / Meses_Janela) * 100, 1),
    Elegiveis_6m        = sum(Ini <= fim_dados_am - 9),
    Interrompeu_ate_6m  = sum(Ini <= fim_dados_am - 9 &
                                Desfecho == "Interrompeu a dispensação" & Meses_Janela <= 6),
    .groups = "drop") %>%
  mutate(Pct_Interrompeu_ate_6m = round(Interrompeu_ate_6m / pmax(Elegiveis_6m, 1) * 100, 2)) %>%
  arrange(desc(Episodios))

# (b) Antes × depois do início do 1º medicamento (pacientes ligados BI × AM)
bi_mes <- bi_atend %>%
  transmute(CNS_PAC, Mes = mes_idx(substr(DT_ATEND, 1, 6)), Urgencia, Mal_Asmatico)
mes_ini_bi <- min(bi_mes$Mes)
mes_fim_bi <- max(bi_mes$Mes)

inicio_tratamento <- am_mes %>%
  group_by(AP_CNSPCN) %>%
  summarise(Ini = min(Mes), Etapa_Ini = max(Etapa_Num[Mes == min(Mes)]), .groups = "drop") %>%
  filter(AP_CNSPCN %in% bi_mes$CNS_PAC,
         Ini >= mes_ini_bi + 12, Ini <= mes_fim_bi - 12)

antes_depois <- inicio_tratamento %>%
  left_join(bi_mes, by = c("AP_CNSPCN" = "CNS_PAC")) %>%
  mutate(d = Mes - Ini, Pre = d >= -12 & d <= -1, Pos = d >= 1 & d <= 12) %>%
  group_by(AP_CNSPCN, Etapa_Ini) %>%
  summarise(Atend_Pre = sum(Pre, na.rm = TRUE), Atend_Pos = sum(Pos, na.rm = TRUE),
            Urg_Pre   = sum(Pre & Urgencia, na.rm = TRUE),
            Urg_Pos   = sum(Pos & Urgencia, na.rm = TRUE),
            J46_Pre   = sum(Pre & Mal_Asmatico, na.rm = TRUE),
            J46_Pos   = sum(Pos & Mal_Asmatico, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Etapa_Inicial = rotulo_etapa[Etapa_Ini + 1])

resumo_antes_depois <- function(df) {
  n <- nrow(df)
  linha <- function(desc, pre, pos) {
    tab <- table(factor(pre > 0, c(FALSE, TRUE)), factor(pos > 0, c(FALSE, TRUE)))
    discord <- tab[1, 2] + tab[2, 1]
    tibble(Desfecho = desc, Pacientes = n,
           Pct_Pre = round(mean(pre > 0) * 100, 2), Pct_Pos = round(mean(pos > 0) * 100, 2),
           Media_Pre = round(mean(pre), 3), Media_Pos = round(mean(pos), 3),
           p_McNemar  = if (n >= 10 && discord > 0) signif(mcnemar.test(tab)$p.value, 3) else NA_real_,
           p_Wilcoxon = if (n >= 10 && any(pre != pos))
                          signif(suppressWarnings(wilcox.test(pre, pos, paired = TRUE))$p.value, 3)
                        else NA_real_)
  }
  bind_rows(
    linha("Qualquer atendimento de asma (BPA-I)", df$Atend_Pre, df$Atend_Pos),
    linha("Atendimento de urgência por asma",     df$Urg_Pre,   df$Urg_Pos),
    linha("Mal asmático (J46)",                    df$J46_Pre,   df$J46_Pos))
}

desfecho_clinico <- bind_rows(
  resumo_antes_depois(antes_depois) %>% mutate(Grupo = "Todos", .before = 1),
  bind_rows(lapply(split(antes_depois, antes_depois$Etapa_Inicial), function(d)
    resumo_antes_depois(d) %>% mutate(Grupo = d$Etapa_Inicial[1], .before = 1))))

# ==============================================================================
# 10. NÍVEL MUNICIPAL E CORRELAÇÃO ECOLÓGICA
# ==============================================================================

asma_mun <- atend %>%
  group_by(Municipio = Mun_Res) %>%
  summarise(
    N_Atendimentos     = n(),
    Pct_Pediatrico     = round(sum(Pediatrico) / pmax(sum(!is.na(Idade)), 1) * 100, 2),
    Pct_Feminino       = round(sum(Feminino) / pmax(sum(Sexo != "Ignorado"), 1) * 100, 2),
    Pct_Urgencia       = round(sum(Urgencia) / pmax(sum(Carater != "Ignorado"), 1) * 100, 2),
    Pct_Fora_Municipio = round(mean(Fora_Municipio) * 100, 2),
    Pct_J46            = round(mean(Mal_Asmatico) * 100, 2),
    .groups = "drop") %>%
  left_join(bi_atend %>% group_by(Municipio = Mun_Res) %>%
              summarise(Pacientes_Unicos = n_distinct(CNS_PAC), .groups = "drop"),
            by = "Municipio") %>%
  left_join(am %>% group_by(Municipio = Mun_Res) %>%
              summarise(Pacientes_AM = n_distinct(AP_CNSPCN), .groups = "drop"),
            by = "Municipio") %>%
  filter(N_Atendimentos >= min_registros_mun)

if (!is.na(arquivo_pop) && file.exists(arquivo_pop)) {
  pop <- fread(arquivo_pop, colClasses = c(CO_MUN = "character")) %>%
    filter(Ano %in% anos) %>%
    group_by(Municipio = substr(CO_MUN, 1, 6)) %>%
    summarise(Pop_Ano = sum(Pop), Pop_Media = mean(Pop), .groups = "drop")
  asma_mun <- asma_mun %>%
    left_join(pop, by = "Municipio") %>%
    mutate(Atend_10mil_hab_ano    = round(N_Atendimentos / Pop_Ano * 1e4, 2),
           Pacientes_10mil_hab    = round(Pacientes_Unicos / Pop_Media * 1e4, 2),
           Pacientes_AM_10mil_hab = round(Pacientes_AM / Pop_Media * 1e4, 2))
}

# Hipótese: menor oferta local (mais deslocamento) → mais urgência
if (nrow(asma_mun) >= 5) {
  correlacao <- cor.test(asma_mun$Pct_Fora_Municipio, asma_mun$Pct_Urgencia,
                         method = "spearman", exact = FALSE)
  rho <- as.numeric(correlacao$estimate); pv <- correlacao$p.value
} else { rho <- NA_real_; pv <- NA_real_ }

resultado_correlacao <- tibble(
  Rho_Spearman = round(rho, 3),
  P_Valor      = round(pv, 4),
  N_Municipios = nrow(asma_mun),
  Forca = case_when(is.na(rho)       ~ "Não calculável",
                    abs(rho) < 0.2   ~ "Desprezível",
                    abs(rho) < 0.4   ~ "Fraca",
                    abs(rho) < 0.6   ~ "Moderada",
                    abs(rho) < 0.8   ~ "Forte",
                    TRUE             ~ "Muito forte"))

# ==============================================================================
# GRÁFICOS
# ==============================================================================

cores_local <- c("Alagoas (total)" = "gray40", "Maceió" = "#D55E00", "Interior" = "#0072B2")
periodo     <- sprintf("%d–%d", min(anos), max(anos))
fonte_txt   <- "Fonte: SIASUS/DATASUS (PA e BI). Atendimento = BPA-I ou APAC colapsada."

p_indicadores <- ggplot(indicadores, aes(x = Local, y = Pct, fill = Local)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct),
                width = 0.2, linewidth = 0.5, color = "gray30") +
  geom_text(aes(y = IC_Sup_Pct, label = round(Pct, 1)),
            vjust = -0.4, size = 3.4, fontface = "bold") +
  facet_wrap(~Indicador, scales = "free_y", nrow = 2, labeller = label_wrap_gen(28)) +
  scale_fill_manual(values = cores_local) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Perfil dos atendimentos ambulatoriais por asma — Alagoas",
       subtitle = paste0("CID J45/J46 | IC 95% Wilson | denominador: registros com informação válida | ",
                         periodo),
       caption = fonte_txt, x = NULL, y = "%") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(size = 8))

piramide_plot <- function(df, idade_col, titulo, subtitulo) {
  d <- df %>%
    mutate(Faixa = faixa_etaria(.data[[idade_col]])) %>%
    filter(Sexo != "Ignorado", Faixa != "Ignorada") %>%
    count(Faixa, Sexo) %>%
    mutate(Pct = n / sum(n) * 100,
           Pct_Plot = if_else(Sexo == "Masculino", -Pct, Pct))
  ggplot(d, aes(x = Faixa, y = Pct_Plot, fill = Sexo)) +
    geom_col(width = 0.8) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(abs(round(x)), "%")) +
    scale_fill_manual(values = c("Masculino" = "#56B4E9", "Feminino" = "#CC79A7")) +
    labs(title = titulo, subtitle = subtitulo, caption = fonte_txt,
         x = "Faixa etária (anos)", y = "%", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

p_piramide_atend <- piramide_plot(atend, "Idade",
  "Atendimentos por asma segundo faixa etária e sexo — Alagoas",
  paste0("Unidade: atendimento | ", periodo))
p_piramide_pac <- piramide_plot(pacientes %>% rename(Idade = Idade_Primeiro), "Idade",
  "Pacientes únicos com asma (BPA-I) segundo faixa etária e sexo — Alagoas",
  paste0("Unidade: paciente (CNS), idade no 1º atendimento do período | ", periodo))

p_cid <- distribuicoes %>%
  filter(Variavel == "CID_Asma") %>%
  mutate(Local = factor(Local, levels = names(cores_local))) %>%
  ggplot(aes(x = Local, y = Pct, fill = Categoria)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = if_else(Pct >= 3, paste0(round(Pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Atendimentos por subcategoria do CID de asma",
       caption = fonte_txt, x = NULL, y = "% dos atendimentos", fill = NULL) +
  theme_minimal(base_size = 13)

p_tendencia <- ggplot(tendencia_faixa, aes(x = Ano, y = N_Atendimentos, color = Faixa_Etaria)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = anos) +
  labs(title    = "Atendimentos ambulatoriais por asma segundo faixa etária — Alagoas",
       subtitle = "Ano de competência de realização",
       caption = fonte_txt, x = "Ano", y = "Nº de atendimentos", color = "Faixa etária") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_pacientes <- pacientes_por_ano %>%
  select(Ano, `Pacientes únicos` = Pacientes_Unicos, `Atendimentos (BPA-I)` = Atendimentos) %>%
  pivot_longer(-Ano, names_to = "Medida", values_to = "N") %>%
  ggplot(aes(x = Ano, y = N, color = Medida)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_x_continuous(breaks = anos) +
  labs(title = "Pacientes únicos e atendimentos BPA-I por asma — Alagoas",
       caption = "Fonte: SIASUS/DATASUS (BI). Paciente identificado pelo CNS.",
       x = "Ano", y = "N", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_tendencia_urg <- tendencia %>%
  select(Ano, `Urgência (%)` = Pct_Urgencia, `J46 — mal asmático (%)` = Pct_J46) %>%
  pivot_longer(-Ano, names_to = "Indicador", values_to = "Pct") %>%
  ggplot(aes(x = Ano, y = Pct, color = Indicador)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_x_continuous(breaks = anos) +
  labs(title = "Proporção de atendimentos de urgência e de estado de mal asmático",
       caption = fonte_txt, x = "Ano", y = "%", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

label_cor <- with(resultado_correlacao, paste0(
  "ρ = ", Rho_Spearman, "\np = ", P_Valor,
  "\nn = ", N_Municipios, " municípios\n(", Forca, ")"))

p_correlacao <- ggplot(asma_mun, aes(x = Pct_Fora_Municipio, y = Pct_Urgencia)) +
  geom_point(aes(size = N_Atendimentos), color = "#D55E00", alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#0072B2",
              linewidth = 0.9, fill = "#0072B2", alpha = 0.15) +
  annotate("text", x = Inf, y = Inf, label = label_cor, hjust = 1.1, vjust = 1.3,
           size = 3.8, family = "mono", color = "gray20") +
  scale_size_continuous(name = "Atendimentos", range = c(1.5, 7)) +
  labs(title    = "Correlação ecológica: atendimento fora do município × urgência",
       subtitle = sprintf("Spearman | LOESS | municípios com ≥ %d atendimentos | %s",
                          min_registros_mun, periodo),
       caption = fonte_txt,
       x = "% atendidos fora do município de residência (PA_MNDIF)",
       y = "% urgência (entre caráter informado)") +
  theme_minimal(base_size = 13)

fonte_am <- "Fonte: SIASUS/DATASUS (APAC de Medicamentos). Paciente identificado pelo CNS."

p_am_ano <- pacientes_am_ano %>%
  select(Ano, `Pacientes em tratamento` = Pacientes_em_Tratamento,
         `Com APAC inicial no ano` = APAC_Iniciais) %>%
  pivot_longer(-Ano, names_to = "Medida", values_to = "N") %>%
  ggplot(aes(x = Ano, y = N, color = Medida)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#009E73", "#0072B2")) +
  scale_x_continuous(breaks = anos) +
  labs(title = "Pacientes com asma em tratamento pelo Componente Especializado — Alagoas",
       caption = fonte_am, x = "Ano (competência)", y = "Pacientes", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_am_imc <- imc_sexo_faixa %>%
  mutate(IMC_Classe = factor(IMC_Classe, levels = c("Baixo peso (< 18,5)", "Eutrofia (18,5–24,9)",
                                                    "Sobrepeso (25–29,9)", "Obesidade (≥ 30)"))) %>%
  ggplot(aes(x = Faixa_Etaria, y = Pct, fill = IMC_Classe)) +
  geom_col(width = 0.7) +
  facet_wrap(~Sexo) +
  scale_fill_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00")) +
  labs(title = "Estado nutricional (IMC) de adultos com asma em tratamento — Alagoas",
       subtitle = "Pacientes ≥ 20 anos, IMC mediano do paciente, classes da OMS",
       caption = fonte_am, x = "Faixa etária (anos)", y = "% dos pacientes", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_am_med <- medicamentos_am %>%
  slice_max(Pacientes, n = 12) %>%
  mutate(Rotulo = coalesce(Medicamento, AP_PRIPAL),
         Rotulo = if_else(nchar(Rotulo) > 55, paste0(substr(Rotulo, 1, 52), "..."), Rotulo)) %>%
  ggplot(aes(x = reorder(Rotulo, Pacientes), y = Pacientes, fill = Classe)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Medicamentos dispensados a pacientes com asma — Alagoas",
       subtitle = paste0("Pacientes únicos por medicamento | ", periodo),
       caption = fonte_am, x = NULL, y = "Pacientes", fill = "Classe") +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

cores_etapa <- setNames(c("gray70", "#56B4E9", "#009E73", "#E69F00", "#D55E00"), rotulo_etapa)

p_am_etapa <- pacientes_am %>%
  filter(Sexo != "Ignorado", Faixa_Etaria != "Ignorada") %>%
  count(Sexo, Faixa_Etaria, Etapa_Max) %>%
  group_by(Sexo, Faixa_Etaria) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = Faixa_Etaria, y = Pct, fill = Etapa_Max)) +
  geom_col(width = 0.7) +
  facet_wrap(~Sexo) +
  scale_fill_manual(values = cores_etapa) +
  labs(title = "Etapa máxima de tratamento segundo faixa etária e sexo — Alagoas",
       subtitle = paste0("Pacientes com APAC de medicamento para asma | ", periodo),
       caption = fonte_am, x = "Faixa etária (anos)", y = "% dos pacientes", fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_vinculo <- ggplot(vinculo_faixa, aes(x = Faixa_Etaria, y = Pct)) +
  geom_col(width = 0.6, fill = "#009E73") +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct), width = 0.2, color = "gray30") +
  geom_text(aes(y = IC_Sup_Pct, label = paste0(round(Pct, 1), "%")), vjust = -0.5, size = 3.6) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Pacientes atendidos (BPA-I) que também recebem medicamento para asma",
       subtitle = paste0("Vínculo BI × AM pelo CNS | IC 95% Wilson | ", periodo),
       caption = "Fonte: SIASUS/DATASUS (BI e AM).",
       x = "Faixa etária no 1º atendimento (anos)", y = "% com APAC de medicamento") +
  theme_minimal(base_size = 13)

cores_desfecho <- setNames(c("#009E73", "#D55E00", "#F0E442", "#56B4E9",
                             "gray55", "gray80", "black"), niveis_desfecho)

p_desfecho <- desfecho_medicamento %>%
  mutate(Medicamento = paste0(Medicamento, "\n(n = ", Episodios, ")"),
         Medicamento = reorder(Medicamento, Episodios)) %>%
  ggplot(aes(x = Medicamento, y = Pct, fill = Desfecho)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = cores_desfecho, drop = TRUE) +
  labs(title = "Desfecho do tratamento após cada medicamento — Alagoas",
       subtitle = paste0("Episódios paciente × medicamento | 'Em uso' = dispensação nos 3 ",
                         "últimos meses dos dados | ", periodo),
       caption = fonte_am, x = NULL, y = "% dos episódios", fill = NULL) +
  guides(fill = guide_legend(nrow = 3)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_antes_depois <- desfecho_clinico %>%
  filter(Grupo == "Todos") %>%
  select(Desfecho, Pacientes, Pct_Pre, Pct_Pos) %>%
  pivot_longer(c(Pct_Pre, Pct_Pos), names_to = "Janela", values_to = "Pct") %>%
  mutate(k = round(Pct / 100 * Pacientes),
         Janela = factor(if_else(Janela == "Pct_Pre", "12 meses antes", "12 meses depois"),
                         levels = c("12 meses antes", "12 meses depois"))) %>%
  bind_cols(wilson_ci_pct(.$k, .$Pacientes)) %>%
  ggplot(aes(x = Desfecho, y = Pct, fill = Janela)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct),
                position = position_dodge(0.75), width = 0.2, color = "gray30") +
  scale_fill_manual(values = c("12 meses antes" = "gray60", "12 meses depois" = "#0072B2")) +
  labs(title = "Atendimentos de asma antes × depois do início do medicamento",
       subtitle = sprintf(paste0("Pacientes ligados BI × AM pelo CNS (n = %d) | ",
                                 "%% com ≥ 1 atendimento na janela | IC 95%% Wilson"),
                          nrow(antes_depois)),
       caption = "Fonte: SIASUS/DATASUS (BI e AM). Sem grupo controle: não indica efeito causal.",
       x = NULL, y = "% dos pacientes", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# EXPORTAÇÃO
# ==============================================================================

salvar_grafico(p_indicadores,    "Asma_AL_01_indicadores", w = 13, h = 7)
salvar_grafico(p_piramide_atend, "Asma_AL_02_piramide_atendimentos")
salvar_grafico(p_piramide_pac,   "Asma_AL_03_piramide_pacientes")
salvar_grafico(p_cid,            "Asma_AL_04_composicao_cid")
salvar_grafico(p_tendencia,      "Asma_AL_05_tendencia_faixa_etaria")
salvar_grafico(p_pacientes,      "Asma_AL_06_pacientes_unicos")
salvar_grafico(p_tendencia_urg,  "Asma_AL_07_tendencia_urgencia_j46")
salvar_grafico(p_correlacao,     "Asma_AL_08_correlacao_acesso_urgencia")
salvar_grafico(p_am_ano,         "Asma_AL_09_AM_pacientes_tratamento")
salvar_grafico(p_am_imc,         "Asma_AL_10_AM_imc_adultos", w = 11, h = 6)
salvar_grafico(p_am_med,         "Asma_AL_11_AM_medicamentos", w = 11, h = 7)
salvar_grafico(p_am_etapa,       "Asma_AL_12_AM_etapa_tratamento", w = 11, h = 6.5)
salvar_grafico(p_vinculo,        "Asma_AL_13_vinculo_atendimento_medicamento")
salvar_grafico(p_desfecho,       "Asma_AL_14_desfecho_por_medicamento", w = 12, h = 7)
salvar_grafico(p_antes_depois,   "Asma_AL_15_atendimentos_antes_depois", w = 11, h = 6.5)

saveRDS(list(fluxograma = fluxograma, instrumentos_excluidos = instrumentos_excluidos,
             completude = completude, flag_idade = flag_idade,
             idades_extremas = idades_extremas, perfil_geral = perfil_geral,
             indicadores = indicadores, indicadores_instrumento = indicadores_instrumento,
             distribuicoes = distribuicoes, testes = testes,
             cruzamento = cruzamento_idade_sexo, top_proc = top_procedimentos,
             sensibilidade = sensibilidade, tendencia = tendencia,
             fluxo_bi = fluxo_bi, resumo_pacientes = resumo_pacientes,
             pacientes_por_ano = pacientes_por_ano, dist_pacientes = dist_pacientes,
             consistencia_pa_bi = consistencia_pa_bi, fluxo_am = fluxo_am,
             consistencia_pa_am = consistencia_pa_am,
             completude_am = completude_am, resumo_am = resumo_am,
             pacientes_am_ano = pacientes_am_ano, dist_am = dist_am,
             medicamentos_am = medicamentos_am, imc_sexo_faixa = imc_sexo_faixa,
             tratamento_perfil = tratamento_perfil, testes_tratamento = testes_tratamento,
             perfil_biologico = perfil_biologico, vinculo_bi_am = vinculo_bi_am,
             vinculo_perfil = vinculo_perfil, teste_vinculo_urgencia = teste_vinculo_urgencia,
             vinculo_faixa = vinculo_faixa,
             motivo_saida_am = motivo_saida_am,
             desfecho_medicamento = desfecho_medicamento,
             duracao_medicamento = duracao_medicamento,
             desfecho_clinico = desfecho_clinico,
             municipal = asma_mun,
             correlacao = resultado_correlacao),
        file.path(output_dir, "Asma_AL_resultados.rds"))
saveRDS(atend, file.path(output_dir, "Base_Asma_AL_atendimentos.rds"))
saveRDS(pacientes_am, file.path(output_dir, "Base_Asma_AL_pacientes_AM.rds"))
saveRDS(episodios, file.path(output_dir, "Base_Asma_AL_episodios_medicamento.rds"))

doc <- read_docx() %>%
  body_add_par("Atendimentos ambulatoriais por asma no SUS — Alagoas", style = "heading 1") %>%
  body_add_par(paste0("Fonte: SIASUS/DATASUS — Produção Ambulatorial (PA) e BPA ",
                      "Individualizado (BI), ", periodo, ". Documentação: Informe Técnico ",
                      "SIASUS (DATASUS, jul/2019)."), style = "Normal") %>%
  body_add_par("Métodos — decisões para redução de vieses", style = "heading 2") %>%
  body_add_par(paste0(
    "(1) BPA consolidado excluído: não informa CID nem paciente, e seu município de ",
    "'residência' é o do estabelecimento. RAAS excluído (CID não disponível no PA). ",
    "(2) A APAC gera uma linha por procedimento (principal e secundários); foi colapsada em ",
    "um atendimento por número de APAC e competência, evitando contagem múltipla. ",
    "(3) O CID secundário só existe na APAC; a posição do diagnóstico é apresentada por instrumento. ",
    "(4) Idade inexistente ou em branco (PA_FLIDADE 3/4) ou > ", idade_max,
    " anos tratada como ausente; idade incompatível com o SIGTAP sinalizada. ",
    "(5) Apenas produção aprovada (PA_INDICA 5/6). ",
    "(6) Ano pela competência de realização, com leitura de ", meses_extra,
    " meses adicionais de processamento para captar apresentação tardia. ",
    "(7) Pacientes únicos estimados pelo CNS no arquivo BI (somente BPA-I); PA e BI não são somados. ",
    "(8) Proporções calculadas sobre registros com informação válida; ignorados reportados à parte. ",
    "(9) Sensibilidade excluindo menores de ", idade_min_sensibilidade, " anos. ",
    "(10) Residentes de AL atendidos em outros estados não foram capturados."
  ), style = "Normal") %>%
  body_add_par("") %>%

  body_add_par("1. Fluxograma de seleção", style = "heading 2") %>%
  body_add_flextable(ft(fluxograma)) %>%
  body_add_par("Linhas excluídas por instrumento (residentes de AL no período):", style = "Normal") %>%
  body_add_flextable(ft(instrumentos_excluidos)) %>%
  body_add_par("") %>%

  body_add_par("2. Qualidade e completude", style = "heading 2") %>%
  body_add_flextable(ft(completude)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(flag_idade)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(idades_extremas)) %>%
  body_add_par("") %>%

  body_add_par("3. Perfil geral (unidade: atendimento)", style = "heading 2") %>%
  body_add_flextable(ft(perfil_geral %>%
    select(Local, N_Atendimentos, N_Procedimentos, Valor_Aprovado_R,
           Idade_Mediana, Idade_P25, Idade_P75))) %>%
  body_add_par("") %>%
  body_add_flextable(ft(indicadores)) %>%
  body_add_par("") %>%
  body_add_par("Indicadores por instrumento de registro", style = "heading 3") %>%
  body_add_flextable(ft(indicadores_instrumento)) %>%
  body_add_par("") %>%

  body_add_par("4. Distribuições demográficas e clínicas", style = "heading 2") %>%
  body_add_flextable(ft(distribuicoes %>% select(-n) %>%
                          pivot_wider(names_from = Local, values_from = Pct))) %>%
  body_add_par("") %>%
  body_add_par("Diferenças Maceió × interior (qui-quadrado e V de Cramér; sem ignorados)",
               style = "heading 3") %>%
  body_add_flextable(ft(testes)) %>%
  body_add_par("") %>%
  body_add_par("Urgência e J46 por faixa etária e sexo", style = "heading 3") %>%
  body_add_flextable(ft(cruzamento_idade_sexo)) %>%
  body_add_par("") %>%
  body_add_par("Procedimentos mais frequentes por instrumento (código SIGTAP)", style = "heading 3") %>%
  body_add_flextable(ft(top_procedimentos)) %>%
  body_add_par("") %>%

  body_add_par("5. Análise de sensibilidade", style = "heading 2") %>%
  body_add_flextable(ft(sensibilidade)) %>%
  body_add_par("") %>%

  body_add_par("6. Tendência temporal", style = "heading 2") %>%
  body_add_flextable(ft(tendencia)) %>%
  body_add_par("") %>%

  body_add_par("7. Pacientes únicos (BPA-I, arquivo BI)", style = "heading 2") %>%
  body_add_flextable(ft(fluxo_bi)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(resumo_pacientes)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(pacientes_por_ano)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(dist_pacientes)) %>%
  body_add_par("") %>%
  body_add_par("Consistência entre PA (DOCORIG = I) e BI", style = "heading 3") %>%
  body_add_flextable(ft(consistencia_pa_bi)) %>%
  body_add_par("") %>%

  body_add_par("8. Tratamento medicamentoso — APAC de Medicamentos (AM)", style = "heading 2") %>%
  body_add_par(paste0(
    "Cada APAC de medicamentos corresponde a um mês de dispensação; a unidade é o paciente (CNS). ",
    "Idade decodificada de AP_COIDADE/AP_NUIDADE; peso fora de ", faixa_peso[1], "–", faixa_peso[2],
    " kg, altura fora de ", faixa_altura[1], "–", faixa_altura[2], " cm e IMC fora de ",
    faixa_imc[1], "–", faixa_imc[2], " kg/m² tratados como ausentes; IMC classificado (OMS) só ",
    "para ≥ 20 anos. Persistência = meses com APAC ÷ meses entre a primeira e a última APAC. ",
    "Pacientes do AM não são somados aos do BI."), style = "Normal") %>%
  body_add_flextable(ft(fluxo_am)) %>%
  body_add_par("") %>%
  body_add_par("Consistência: APAC de medicamentos no PA × arquivo AM", style = "heading 3") %>%
  body_add_flextable(ft(consistencia_pa_am)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(completude_am)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(resumo_am)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(pacientes_am_ano)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(dist_am)) %>%
  body_add_par("") %>%
  body_add_par("Medicamentos (pacientes únicos)", style = "heading 3") %>%
  body_add_flextable(ft(medicamentos_am)) %>%
  body_add_par("") %>%
  body_add_par("IMC de adultos por sexo e faixa etária", style = "heading 3") %>%
  body_add_flextable(ft(imc_sexo_faixa)) %>%
  body_add_par("") %>%
  body_add_par("Medicamento × perfil do paciente (etapa máxima de tratamento)", style = "heading 3") %>%
  body_add_par(paste0(
    "Etapa = degrau terapêutico mais alto dispensado ao paciente no período: CI ou LABA isolado < ",
    "CI+LABA com budesonida 200 < CI+LABA com budesonida 400 < imunobiológico. Proxy de ",
    "gravidade/controle. Teste: qui-quadrado e V de Cramér, sem categorias ignoradas; a etapa ",
    "'imunobiológico' é rara e gera caselas esparsas."), style = "Normal") %>%
  body_add_flextable(ft(testes_tratamento)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(tratamento_perfil %>% select(-n, -N_Categoria) %>%
                          pivot_wider(names_from = Etapa_Max, values_from = Pct, values_fill = 0))) %>%
  body_add_par("") %>%
  body_add_par("Perfil dos pacientes com e sem imunobiológico", style = "heading 3") %>%
  body_add_flextable(ft(perfil_biologico)) %>%
  body_add_par("") %>%
  body_add_par("Vínculo atendimento (BI) × tratamento (AM) pelo CNS", style = "heading 3") %>%
  body_add_par(paste0(
    "O CNS criptografado é o mesmo nos arquivos BI e AM, o que permite ligar os pacientes. ",
    "O BI cobre só BPA-I; a ordem temporal entre atendimento e medicamento não é considerada."),
    style = "Normal") %>%
  body_add_flextable(ft(vinculo_bi_am)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(vinculo_perfil)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(teste_vinculo_urgencia)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(vinculo_faixa)) %>%
  body_add_par("") %>%
  body_add_par("Desfechos após cada medicamento", style = "heading 3") %>%
  body_add_par(paste0(
    "O SIASUS é ambulatorial: não registra alta hospitalar nem reinternação (dados do SIH, ",
    "cujos arquivos públicos não trazem CNS e não permitem ligar o paciente). Na APAC de ",
    "medicamento o motivo de saída é quase sempre 'permanência', como mostra a tabela abaixo. ",
    "Por isso, os desfechos medidos são (a) o destino do tratamento por episódio ",
    "(paciente × medicamento) e (b) atendimentos de asma no BI antes e depois do início."),
    style = "Normal") %>%
  body_add_flextable(ft(motivo_saida_am)) %>%
  body_add_par("") %>%
  body_add_par(paste0(
    "(a) Desfecho do episódio, em ordem hierárquica: óbito registrado > troca para outro ",
    "medicamento (escalonou, mesma etapa ou reduziu) > em uso no fim dos dados (dispensação ",
    "nos 3 últimos meses) > saída registrada > interrompeu a dispensação. Interrupção até 6 ",
    "meses: só episódios iniciados ≥ 9 meses antes do fim dos dados."), style = "Normal") %>%
  body_add_flextable(ft(desfecho_medicamento %>% select(-n) %>%
                          pivot_wider(names_from = Desfecho, values_from = Pct,
                                      values_fill = 0))) %>%
  body_add_par("") %>%
  body_add_flextable(ft(duracao_medicamento)) %>%
  body_add_par("") %>%
  body_add_par(paste0(
    "(b) Pacientes ligados BI × AM com 12 meses observáveis antes e depois do início do 1º ",
    "medicamento (mês do início excluído). Pct = % com ≥ 1 atendimento na janela; Média = ",
    "atendimentos por paciente. Testes pareados: McNemar (proporção) e Wilcoxon (contagem). ",
    "Sem grupo controle; regressão à média e tendência secular podem explicar diferenças."),
    style = "Normal") %>%
  body_add_flextable(ft(desfecho_clinico)) %>%
  body_add_par("") %>%

  body_add_par("9. Nível municipal", style = "heading 2") %>%
  body_add_par(sprintf("Municípios de residência com ≥ %d atendimentos no período.",
                       min_registros_mun), style = "Normal") %>%
  body_add_flextable(ft(asma_mun %>% arrange(desc(N_Atendimentos)))) %>%
  body_add_par("") %>%

  body_add_par("10. Correlação ecológica: atendimento fora do município × urgência",
               style = "heading 2") %>%
  body_add_par(paste0(
    "Spearman com município como unidade (exact = FALSE por empates); tendência por LOESS. ",
    "Associação ecológica: não permite inferência individual (falácia ecológica)."),
    style = "Normal") %>%
  body_add_flextable(ft(resultado_correlacao))

print(doc, target = file.path(output_dir, "Asma_AL_Resultados.docx"))
message("Concluído. Saídas em: ", output_dir)
