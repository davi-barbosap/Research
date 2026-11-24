# ==============================================================================
# PROJETO: ASMA EM CRIANÇAS E ADOLESCENTES (0–19 ANOS) NO SUS — ALAGOAS
#          Atendimentos ambulatoriais (SIASUS) e tratamento no Componente
#          Especializado da Assistência Farmacêutica (CEAF), 2015–2025
#
# Fontes (DATASUS, ftp.datasus.gov.br/dissemin/publicos/{SIASUS,SIHSUS}/200801_/Dados):
#   PAALaamm.dbc — Produção Ambulatorial (fluxograma, APAC, quantidades)
#   BIALaamm.dbc — BPA Individualizado (CNS, data de nascimento) → atendimentos
#   AMALaamm.dbc — APAC de Medicamentos (CEAF: CNS, medicamento, peso, altura)
#   RDALaamm.dbc — SIH, AIH reduzida (internações; sem CNS nos arquivos públicos)
# População: estimativas DATASUS 2024 por município, sexo e faixa etária
#   (pacote brpop; 2025 = 2024 repetido).
# Documentação: Informe Técnico SIASUS (jul/2019) e PCDT de Asma.
# Auditoria que motivou esta versão: Auditoria_Asma_SIASUS.md (itens 1–20).
#
# POPULAÇÃO DO ESTUDO: 0–19 anos (pediatria até 19 anos — SBP/OMS).
#   Sensibilidade: 0–17 anos (ECA) e 5–19 anos (asma pouco específica < 5 anos).
#   Idade sempre medida NA DATA do registro: um atendimento/dispensação aos
#   20+ anos não entra, mesmo que o paciente tenha entrado criança.
#
# PRINCIPAIS DECISÕES (número = item da auditoria)
#  [2] APAC de medicamento saem do PA por QUALQUER linha (P ou S) do grupo 06,
#      antes do filtro de aprovação.
#  [4] Atendimentos analisados no BI (= BPA-I do PA, conferido 1:1), que tem
#      CNS: IC robustos a cluster por paciente (survey) e testes no nível do
#      PACIENTE (Holm para múltiplas comparações).
#  [3][7] Cada atendimento recebe tipo de SERVIÇO (CNES/TPUPS) e tipo de
#      ATENDIMENTO (procedimento + CBO). "Urgência" = caráter do atendimento,
#      fortemente dependente do serviço → sempre estratificada; tendência com
#      painel estável de estabelecimentos e ajuste por estabelecimento.
#  [8][9] Taxas por 10 mil habitantes de 0–19 anos (padronizadas por idade);
#      tendência 2018+ (cobertura estável), COVID (2020–21) marcada e testada
#      em sensibilidade; variação anual (APC) por Prais-Winsten.
#  [1] Dispensação (AM) analisada com TODAS as APAC: aprovação é evento de
#      faturamento, não de dispensação. Aprovação só entra em sensibilidade.
#      Fim dos dados = último mês com volume completo; meses com queda abrupta
#      de volume (provável falha de registro/desabastecimento) não contam como
#      "sem dispensação".
#  [6][12] Paciente NOVO no CEAF = 1ª APAC após ≥ 12 meses sem APAC (washout);
#      APAC "inicial" (AP_TPAPAC = 1) é reemitida a cada 3 meses e não serve.
#  [5] Intensidade do tratamento = dose diária estimada de budesonida
#      (quantidade dispensada × µg por unidade ÷ 30), classificada pela GINA
#      por idade. A apresentação (200 × 400 µg) é descrita à parte.
#  [13][14] Episódio = paciente × medicamento, quebrado por lacuna ≥ 3 meses
#      (CEAF: entrada aos 0–19 anos, seguimento até 20 — margem de 1 ano)
#      observáveis; persistência por Kaplan-Meier.
#  [10] Vínculo BI × AM com regressão logística ajustada; antes × depois em
#      janelas múltiplas + grupo controle pareado (diferença-em-diferenças).
#  [11] IMC: escore-z IMC-para-idade OMS (anthro 0–4; anthroplus 5–19);
#      gestantes excluídas.
#  [15] V de Cramér com limiares dependentes dos graus de liberdade; Holm.
#  [16] Ecológico: regressão quase-binomial ponderada + Spearman.
#  [19] Valores (R$) não analisados: AP_VL_AP = 0 em 2015–2022.
#  [20] Cache validado por hash dos parâmetros; sessionInfo no relatório.
# ==============================================================================

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
set.seed(2026)

# --- PARÂMETROS ---------------------------------------------------------------
output_dir <- "/Users/davibarbosa/Desktop/Henrique"
dir_dbc    <- "/Volumes/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados"
url_ftp    <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"

anos        <- 2015:2025
anos_tend   <- 2018:2025          # tendência: cobertura do BPA-I estabilizada [9]
anos_covid  <- 2020:2021
meses_extra <- 6                  # meses lidos após o período (apresentação tardia)
cod_uf_al   <- "27"
cod_maceio  <- "270430"
idade_max   <- 110                # acima disso: idade implausível → NA
idade_ped   <- 19                 # recorte: 0–19 anos
margem_idade <- 1                 # CEAF: quem entra até 19 anos é seguido até 20
idade_eca   <- 17                 # sensibilidade: 0–17 anos
idade_min_sensibilidade <- 5      # sensibilidade: 5–19 anos
min_registros_mun <- 30           # mínimo por município (estabilidade)
washout_meses <- 12               # paciente novo: ≥ 12 meses sem registro
gap_meses     <- 3                # interrupção: ≥ 3 meses observáveis sem APAC
limiar_volume_fim      <- 0.80    # fim dos dados: volume ≥ 80% da mediana anterior
limiar_volume_suspeito <- 0.50    # mês suspeito: volume < 50% da mediana local
controles_por_caso <- 4

arquivo_pop    <- NA              # CSV alternativo: CO_MUN, Ano, Faixa, Pop
arquivo_sigtap <- NA

faixa_peso   <- c(2, 200)         # kg
faixa_altura <- c(40, 220)        # cm

regex_asma <- "^J4[56]"

# µg de budesonida por unidade dispensada (cápsula) — SIGTAP
ug_budesonida <- c("0604040040" = 400,   # formoterol 12 + budesonida 400 cápsula
                   "0604040075" = 200,   # formoterol 6 + budesonida 200 cápsula
                   "0604280068" = 200)   # budesonida 200 cápsula

# --- BIBLIOTECAS --------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(forcats)
  library(ggplot2); library(ragg); library(officer); library(flextable)
  library(read.dbc); library(survey); library(survival); library(sandwich)
  library(lmtest)
})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- FUNÇÕES AUXILIARES -------------------------------------------------------
salvar_grafico <- function(plot, nome_base, w = 10, h = 6) {
  ragg::agg_tiff(file.path(output_dir, paste0(nome_base, ".tiff")),
                 width = w, height = h, units = "in", res = 300, compression = "lzw")
  print(plot); dev.off()
  ragg::agg_png(file.path(output_dir, paste0(nome_base, ".png")),
                width = w, height = h, units = "in", res = 300)
  print(plot); dev.off()
  invisible(file.path(output_dir, paste0(nome_base, ".png")))
}

ft <- function(df) {
  flextable(as.data.frame(df)) %>% theme_vanilla() %>% autofit() %>%
    fontsize(size = 9, part = "all") %>% bold(part = "header")
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

# Proporção com IC 95% robusto a cluster (atendimentos do mesmo paciente) [4]
prop_cluster <- function(x, cl) {
  d <- data.frame(x = as.numeric(x), cl = cl)
  d <- d[!is.na(d$x), , drop = FALSE]
  n <- nrow(d); k <- sum(d$x)
  if (n == 0) return(tibble(k = 0, n = 0, Pacientes = 0, Pct = NA_real_,
                            IC_Inf_Pct = NA_real_, IC_Sup_Pct = NA_real_))
  if (k == 0 || k == n || n_distinct(d$cl) < 2) {
    ic <- wilson_ci_pct(k, n)                      # sem variância: Wilson
    return(tibble(k = k, n = n, Pacientes = n_distinct(d$cl), Pct = 100 * k / n,
                  IC_Inf_Pct = ic$IC_Inf_Pct, IC_Sup_Pct = ic$IC_Sup_Pct))
  }
  des <- suppressWarnings(svydesign(ids = ~cl, data = d))
  r   <- svyciprop(~x, des, method = "logit")
  ci  <- attr(r, "ci")
  tibble(k = k, n = n, Pacientes = n_distinct(d$cl), Pct = 100 * as.numeric(r),
         IC_Inf_Pct = 100 * ci[[1]], IC_Sup_Pct = 100 * ci[[2]])
}

# Qui-quadrado + V de Cramér; magnitude pelos limiares de Cohen ajustados
# pelos graus de liberdade (w / sqrt(df*)) [15]
teste_distribuicao <- function(df, var, grupo) {
  tab <- table(df[[grupo]], df[[var]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (ncol(tab) < 2 || nrow(tab) < 2)
    return(tibble(Variavel = var, Grupo = grupo, N = sum(tab), Qui2 = NA_real_,
                  gl = NA_integer_, P_Valor = NA_real_, V_Cramer = NA_real_,
                  Magnitude = "Não testável", Esperado_menor_5_Pct = NA_real_))
  qt   <- suppressWarnings(chisq.test(tab))
  # > 20% das caselas com esperado < 5: p-valor por Monte Carlo (10 mil réplicas)
  if (mean(qt$expected < 5) > 0.20)
    qt$p.value <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 10000))$p.value
  dfs  <- min(dim(tab)) - 1
  v    <- sqrt(as.numeric(qt$statistic) / (sum(tab) * dfs))
  lim  <- c(0.10, 0.30, 0.50) / sqrt(dfs)
  tibble(Variavel  = var, Grupo = grupo, N = sum(tab),
         Qui2      = round(as.numeric(qt$statistic), 1),
         gl        = as.integer(qt$parameter),
         P_Valor   = qt$p.value,
         V_Cramer  = round(v, 3),
         Magnitude = case_when(v < lim[1] ~ "Desprezível", v < lim[2] ~ "Pequena",
                               v < lim[3] ~ "Moderada", TRUE ~ "Grande"),
         Esperado_menor_5_Pct = round(mean(qt$expected < 5) * 100, 1))
}

ajustar_holm <- function(tab) {
  tab %>% mutate(P_Holm  = signif(p.adjust(P_Valor, "holm"), 3),
                 P_Valor = signif(P_Valor, 3))
}

decod_sexo <- function(x) case_when(
  toupper(trimws(x)) %in% c("M", "1", "MASCULINO") ~ "Masculino",
  toupper(trimws(x)) %in% c("F", "2", "FEMININO")  ~ "Feminino",
  TRUE                                             ~ "Ignorado")

decod_raca <- function(x) case_when(
  x %in% c("01", "1") ~ "Branca",
  x %in% c("02", "2") ~ "Preta",
  x %in% c("03", "3") ~ "Parda",
  x %in% c("04", "4") ~ "Amarela",
  x %in% c("05", "5") ~ "Indígena",
  TRUE                ~ "Sem informação")

decod_carater <- function(x) case_when(
  x %in% c("01", "1") ~ "Eletivo",
  x %in% c("02", "2") ~ "Urgência",
  x %in% c("03", "04", "05", "06", "3", "4", "5", "6") ~ "Acidente/lesão",
  TRUE                ~ "Ignorado")

niveis_faixa <- c("0–4", "5–9", "10–14", "15–19")
faixa_ped <- function(idade) {
  f <- cut(idade, breaks = c(-Inf, 4, 9, 14, 19), labels = niveis_faixa)
  fct_na_value_to_level(f, "Ignorada")
}

rotulo_cid <- c(J450 = "J45.0 Predominantemente alérgica", J451 = "J45.1 Não alérgica",
                J458 = "J45.8 Mista", J459 = "J45.9 Não especificada",
                J46  = "J46 Estado de mal asmático")
cid_asma <- function(cod) {
  cod <- if_else(substr(cod, 1, 3) == "J46", "J46", substr(cod, 1, 4))
  coalesce(unname(rotulo_cid[cod]), "J45 sem subcategoria válida")
}

# Tipo de SERVIÇO pelo tipo de estabelecimento do CNES (PA_TPUPS) [3]
tipo_servico <- function(tp) case_when(
  tp %in% c("20", "21", "73")       ~ "Urgência/pronto-atendimento",
  tp %in% c("05", "07", "15", "62") ~ "Hospital/unidade mista",
  tp %in% c("04", "36", "39")       ~ "Ambulatório especializado",
  tp %in% c("01", "02", "32", "71") ~ "Atenção primária",
  is.na(tp)                         ~ "Sem informação",
  TRUE                              ~ "Outro")

# Tipo de ATENDIMENTO pelo procedimento SIGTAP e CBO do profissional [7]
tipo_atendimento <- function(proc, cbo) case_when(
  substr(proc, 1, 7) == "0301060"                         ~ "Urgência/observação",
  substr(proc, 1, 4) == "0302" | substr(cbo, 1, 4) == "2236" ~ "Fisioterapia",
  substr(proc, 1, 6) == "030101"                          ~ "Consulta",
  substr(proc, 1, 2) == "02"                              ~ "Diagnóstico",
  TRUE                                                    ~ "Outros procedimentos")

mes_idx   <- function(cmp) as.integer(substr(cmp, 1, 4)) * 12L + as.integer(substr(cmp, 5, 6)) - 1L
idx_data  <- function(m) as.Date(sprintf("%d-%02d-15", m %/% 12L, m %% 12L + 1L))
cns_valido <- function(x) !is.na(x) & x != "" & !grepl("^(.)\\1*$", x, useBytes = TRUE)

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

# Cache validado pelos parâmetros de extração [20]: se o filtro de CID ou as
# colunas mudarem, o cache antigo deixa de valer (evita resultado silencioso).
verificar_cache <- function(dir_cache, cols, cid_cols) {
  dir.create(dir_cache, showWarnings = FALSE)
  hash <- digest::digest(list(regex_asma, cols, cid_cols))
  f    <- file.path(dir_cache, "PARAMETROS_EXTRACAO.txt")
  if (file.exists(f) && readLines(f, warn = FALSE)[1] != hash)
    stop("Parâmetros de extração mudaram desde o cache em ", dir_cache,
         ". Apague a pasta para extrair de novo.")
  writeLines(hash, f)
}

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

periodos <- format(seq(as.Date(sprintf("%d-01-01", min(anos))), by = "month",
                       length.out = 12 * (max(anos) - min(anos) + 1) + meses_extra), "%y%m")

# Lista os arquivos (inclui partes a, b, c...). Sem a pasta montada, lista o
# diretório do FTP; se a listagem falhar, monta os nomes sem partes. [20]
listar_arquivos <- function(prefixo, url = url_ftp, pasta = dir_dbc) {
  padrao <- sprintf("^%sAL(%s)[a-z]?\\.dbc$", prefixo, paste(periodos, collapse = "|"))
  if (!is.null(pasta) && dir.exists(pasta))
    return(list.files(pasta, pattern = padrao, full.names = TRUE, ignore.case = TRUE))
  nomes <- tryCatch(strsplit(RCurl::getURL(url, dirlistonly = TRUE,
                                           ftp.use.epsv = FALSE), "\r?\n")[[1]],
                    error = function(e) NULL)
  if (!is.null(nomes)) return(paste0(url, grep(padrao, nomes, value = TRUE, ignore.case = TRUE)))
  paste0(url, prefixo, "AL", periodos, ".dbc")
}

# O FTP do DATASUS recusa muitas conexões de dados: até 6 tentativas
ler_dbc <- function(caminho, tentativas = 6) {
  if (file.exists(caminho)) return(read.dbc(caminho, as.is = TRUE))
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp))
  for (i in seq_len(tentativas)) {
    ok <- tryCatch({ download.file(caminho, tmp, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) e, warning = function(w) w)
    if (isTRUE(ok) && file.exists(tmp) && file.size(tmp) > 0) return(read.dbc(tmp, as.is = TRUE))
    if (inherits(ok, "condition") && grepl("550|unavailable", conditionMessage(ok))) break
    Sys.sleep(3)
  }
  stop("download falhou após ", tentativas, " tentativas")
}

arquivos_falha <- tibble(Arquivo = character(), Motivo = character())

extrair <- function(prefixo, dir_cache, cols, cid_cols, url = url_ftp, pasta = dir_dbc) {
  verificar_cache(dir_cache, cols, cid_cols)
  arquivos <- listar_arquivos(prefixo, url, pasta)
  message(prefixo, ": ", length(arquivos), " arquivos")
  for (f in arquivos) {
    cache <- file.path(dir_cache, sub("\\.dbc$", ".rds", basename(f), ignore.case = TRUE))
    if (file.exists(cache)) next
    message("  lendo ", basename(f))
    res <- tryCatch(filtrar_asma(ler_dbc(f), cols, cid_cols),
                    error = function(e) {
                      arquivos_falha <<- bind_rows(arquivos_falha,
                        tibble(Arquivo = basename(f), Motivo = conditionMessage(e)))
                      NULL })
    if (!is.null(res)) saveRDS(res, cache)
    gc()
  }
  rbindlist(lapply(list.files(dir_cache, "\\.rds$", full.names = TRUE), readRDS), fill = TRUE)
}

pa_raw <- extrair("PA", file.path(output_dir, "cache_asma_PA"), cols_pa, c("PA_CIDPRI", "PA_CIDSEC"))
bi_raw <- extrair("BI", file.path(output_dir, "cache_asma_BI"), cols_bi, "CIDPRI")
am_raw <- extrair("AM", file.path(output_dir, "cache_asma_AM"), cols_am, c("AP_CIDPRI", "AP_CIDSEC"))

# SIH/RD — internações (AIH reduzida). Guarda asma em qualquer posição do CID;
# a definição de caso (CID principal) é aplicada depois.
url_ftp_sih <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
dir_dbc_sih <- "/Volumes/ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados"
cols_sih <- c("ANO_CMPT", "MES_CMPT", "N_AIH", "IDENT", "CEP", "MUNIC_RES", "MUNIC_MOV",
              "NASC", "SEXO", "RACA_COR", "IDADE", "COD_IDADE", "DT_INTER", "DT_SAIDA",
              "DIAS_PERM", "UTI_MES_TO", "MARCA_UTI", "MORTE", "CAR_INT", "COBRANCA",
              "PROC_REA", "CNES", "DIAG_PRINC", "DIAG_SECUN", paste0("DIAGSEC", 1:9),
              "CID_MORTE", "VAL_TOT")
sih_raw <- extrair("RD", file.path(output_dir, "cache_asma_SIH"), cols_sih,
                   c("DIAG_PRINC", "DIAG_SECUN", paste0("DIAGSEC", 1:9)),
                   url = url_ftp_sih, pasta = dir_dbc_sih)

# --- Tabela SIGTAP -------------------------------------------------------------
carregar_sigtap <- function() {
  df <- tryCatch(as.data.frame(microdatasus::fetch_sigtab()), error = function(e) NULL)
  if (is.null(df) && !is.na(arquivo_sigtap) && file.exists(arquivo_sigtap))
    df <- fread(arquivo_sigtap, colClasses = "character")
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- as.data.frame(lapply(df, utf8_seguro), stringsAsFactors = FALSE)
  eh_cod  <- sapply(df, function(x) mean(grepl("^\\d{10}$", trimws(x)), na.rm = TRUE))
  if (max(eh_cod) < 0.5) return(NULL)
  col_cod <- names(df)[which.max(eh_cod)]
  outras  <- setdiff(names(df), col_cod)
  col_nom <- outras[which.max(sapply(df[outras], function(x) mean(nchar(x), na.rm = TRUE)))]
  nome <- gsub("[^ -~À-ÿ]", "", df[[col_nom]])
  tibble(Codigo = trimws(df[[col_cod]]), Nome = trimws(gsub("\\s+", " ", nome))) %>%
    distinct(Codigo, .keep_all = TRUE)
}
sigtap <- suppressMessages(carregar_sigtap())
nomear_proc <- function(df, col) {
  if (is.null(sigtap)) return(df %>% mutate(Nome = NA_character_))
  df %>% left_join(sigtap %>% rename(!!col := Codigo), by = col)
}

# --- População 0–19 anos por município, ano, sexo e faixa [8] ------------------
carregar_pop <- function() {
  if (!is.na(arquivo_pop) && file.exists(arquivo_pop))
    return(fread(arquivo_pop, colClasses = c(CO_MUN = "character")) %>% as_tibble())
  mapa_faixa <- c("From 0 to 4 years" = "0–4", "From 5 to 9 years" = "5–9",
                  "From 10 to 14 years" = "10–14", "From 15 to 19 years" = "15–19")
  ler <- function(f, sexo) f() %>%
    filter(substr(as.character(code_muni), 1, 2) == cod_uf_al,
           age_group %in% names(mapa_faixa), year %in% anos) %>%
    transmute(CO_MUN = substr(as.character(code_muni), 1, 6), Ano = as.integer(year),
              Sexo = sexo, Faixa = unname(mapa_faixa[age_group]), Pop = pop)
  pop <- tryCatch(suppressMessages(bind_rows(
    ler(brpop::datasus2024_mun_female_pop, "Feminino"),
    ler(brpop::datasus2024_mun_male_pop,   "Masculino"))), error = function(e) NULL)
  if (is.null(pop)) return(NULL)
  falta <- setdiff(anos, unique(pop$Ano))              # 2025: repete o último ano
  out <- bind_rows(pop, bind_rows(lapply(falta, function(a)
    pop %>% filter(Ano == max(pop$Ano)) %>% mutate(Ano = a))))
  attr(out, "ano_max") <- max(pop$Ano)
  out
}
pop_ped <- carregar_pop()
pop_ano_disponivel <- if (is.null(pop_ped)) NA else attr(pop_ped, "ano_max")
if (is.null(pop_ped)) message("População indisponível: taxas não serão calculadas.")

# ==============================================================================
# 2. PA — FLUXOGRAMA, EXCLUSÃO DAS APAC DE MEDICAMENTO, MAPA DE ESTABELECIMENTOS
# ==============================================================================

pa0 <- pa_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(PA_CMP, 1, 4)), Mun_Res = substr(PA_MUNPCN, 1, 6),
         DOCORIG = toupper(PA_DOCORIG), Idade = suppressWarnings(as.integer(PA_IDADE)),
         Idade = if_else(coalesce(PA_FLIDADE %in% c("3", "4"), FALSE) |
                           is.na(Idade) | Idade < 0 | Idade > idade_max, NA_integer_, Idade))
pa1 <- pa0 %>% filter(Ano %in% anos)
pa2 <- pa1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
pa3 <- pa2 %>% filter(DOCORIG %in% c("I", "P", "S"))

# [2] APAC de medicamento identificadas por QUALQUER linha do grupo 06, antes
# do filtro de aprovação (antes: só linha P aprovada → 1.889 vazavam).
chaves_apac_med <- pa3 %>%
  filter(DOCORIG %in% c("P", "S"), substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP)

pa4 <- pa3 %>% filter(PA_INDICA %in% c("5", "6"))
pa5 <- pa4 %>% anti_join(chaves_apac_med, by = c("PA_AUTORIZ", "PA_CMP"))
pa6 <- pa5 %>% filter(!is.na(Idade))
pa7 <- pa6 %>% filter(Idade <= idade_ped)

fluxograma_pa <- tibble(
  Etapa = c("Linhas do PA com CID J45/J46 (principal ou secundário)",
            sprintf("Competência em %d–%d", min(anos), max(anos)),
            "Residente em Alagoas", "Instrumento BPA-I ou APAC (exclui BPA-C e RAAS)",
            "Produção aprovada (PA_INDICA 5/6)",
            "Sem APAC de medicamento (qualquer linha do grupo 06 → módulo AM)",
            "Idade válida", sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(pa0), nrow(pa1), nrow(pa2), nrow(pa3), nrow(pa4), nrow(pa5),
             nrow(pa6), nrow(pa7))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# O que sobra no PA pediátrico: BPA-I (analisado pelo BI) e APAC não medicamento
pa_ped_instrumento <- pa7 %>%
  mutate(Instrumento = if_else(DOCORIG == "I", "BPA-I", "APAC (não medicamento)")) %>%
  group_by(Instrumento) %>%
  summarise(Linhas = n(),
            Atendimentos = if (first(Instrumento) == "BPA-I") n()
                           else n_distinct(paste(PA_AUTORIZ, PA_CMP)),
            .groups = "drop")

# Tipo de estabelecimento (CNES) por código de unidade — usado no BI
mapa_tpups <- pa0 %>%
  filter(!is.na(PA_TPUPS), PA_TPUPS != "") %>%
  count(PA_CODUNI, PA_TPUPS) %>%
  group_by(PA_CODUNI) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(CODUNI = PA_CODUNI, TPUPS = PA_TPUPS)

# ==============================================================================
# 3. ATENDIMENTOS PEDIÁTRICOS — arquivo BI (= BPA-I do PA, com CNS) [4]
# ==============================================================================

bi <- bi_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(DT_ATEND, 1, 4)),
         Mes = mes_idx(substr(DT_ATEND, 1, 6)),
         Data_Atend = as.Date(paste0(substr(DT_ATEND, 1, 6), "15"), "%Y%m%d"),
         Data_Nasc  = as.Date(DTNASC, "%Y%m%d"),
         Idade      = floor(as.numeric(Data_Atend - Data_Nasc) / 365.25),
         Idade      = if_else(is.na(Idade) | Idade < 0 | Idade > idade_max, NA_real_, Idade))

bi1 <- bi  %>% filter(Ano %in% anos)
bi2 <- bi1 %>% filter(substr(MUNPAC, 1, 2) == cod_uf_al)
bi3 <- bi2 %>% filter(suppressWarnings(as.numeric(QT_APROV)) > 0)
bi4 <- bi3 %>% filter(!is.na(Idade))
bi5 <- bi4 %>% filter(Idade <= idade_ped)

fluxo_bi <- tibble(
  Etapa  = c("Linhas do BI com CIDPRI J45/J46", "Competência no período",
             "Residente em AL", "Quantidade aprovada > 0",
             "Idade válida (data de nascimento)", sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(bi), nrow(bi1), nrow(bi2), nrow(bi3), nrow(bi4), nrow(bi5))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# Consistência PA × BI (todas as idades): o BI deve reproduzir o BPA-I do PA
consistencia_pa_bi <- full_join(
  pa4 %>% filter(DOCORIG == "I") %>% count(Ano, name = "PA_BPA_I"),
  bi3 %>% count(Ano, name = "BI_aprovado"), by = "Ano") %>%
  mutate(Diferenca_Pct = round((BI_aprovado - PA_BPA_I) / PA_BPA_I * 100, 2)) %>%
  arrange(Ano)

atend <- bi5 %>%
  left_join(mapa_tpups, by = "CODUNI") %>%
  mutate(
    CNS_Valido   = cns_valido(CNS_PAC),
    Cluster      = if_else(CNS_Valido, CNS_PAC, paste0("linha_", row_number())),
    Faixa_Etaria = faixa_ped(Idade),
    Sexo         = decod_sexo(SEXOPAC),
    Raca_Cor     = decod_raca(RACACOR),
    Mun_Res      = substr(MUNPAC, 1, 6),
    Local        = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
    Fora_Municipio = coalesce(MNDIF == "1", FALSE),
    CID_Asma     = cid_asma(CIDPRI),
    Mal_Asmatico = substr(CIDPRI, 1, 3) == "J46",
    Carater      = decod_carater(CATEND),
    Urgencia     = Carater == "Urgência",
    Tipo_Servico = tipo_servico(TPUPS),
    Tipo_Atend   = tipo_atendimento(PROC_ID, CBOPROF),
    Covid        = Ano %in% anos_covid)

completude <- atend %>%
  summarise(N_Atendimentos           = n(),
            Pct_CNS_Invalido         = round(mean(!CNS_Valido) * 100, 2),
            Pct_Sexo_Ignorado        = round(mean(Sexo == "Ignorado") * 100, 2),
            Pct_Raca_Sem_Info        = round(mean(Raca_Cor == "Sem informação") * 100, 2),
            Pct_Carater_Ignorado     = round(mean(Carater == "Ignorado") * 100, 2),
            Pct_CID_Sem_Subcategoria = round(mean(CID_Asma == "J45 sem subcategoria válida") * 100, 2),
            Pct_Servico_Sem_Info     = round(mean(Tipo_Servico == "Sem informação") * 100, 2))

# ==============================================================================
# 4. PERFIL DOS ATENDIMENTOS — descritivo, IC robusto a cluster por paciente
# ==============================================================================

atend_ind <- atend %>%
  mutate(I_Feminino   = if_else(Sexo == "Ignorado", NA, Sexo == "Feminino"),
         I_Urgencia   = if_else(Carater == "Ignorado", NA, Urgencia),
         I_J46        = Mal_Asmatico,
         I_Fora_Mun   = Fora_Municipio,
         I_Serv_Urg   = Tipo_Servico == "Urgência/pronto-atendimento",
         I_Fisio      = Tipo_Atend == "Fisioterapia")

rotulo_ind <- c(I_Feminino = "Sexo feminino (%)",
                I_Urgencia = "Caráter de urgência (%)",
                I_Serv_Urg = "Em serviço de urgência/pronto-atendimento (%)",
                I_Fisio    = "Sessão de fisioterapia (%)",
                I_J46      = "Estado de mal asmático — J46 (%)",
                I_Fora_Mun = "Atendido fora do município de residência (%)")

indicadores_por <- function(df, grupo) {
  bind_rows(lapply(names(rotulo_ind), function(v) {
    df %>% group_by(Grupo = as.character(.data[[grupo]])) %>%
      group_modify(~ prop_cluster(.x[[v]], .x$Cluster)) %>%
      ungroup() %>%
      mutate(Indicador = rotulo_ind[[v]], Estratificacao = grupo, .before = 1)
  }))
}

indicadores <- bind_rows(
  indicadores_por(atend_ind %>% mutate(Total = "Alagoas (0–19 anos)"), "Total"),
  indicadores_por(atend_ind, "Local"),
  indicadores_por(atend_ind, "Faixa_Etaria")) %>%
  mutate(across(c(Pct, IC_Inf_Pct, IC_Sup_Pct), ~ round(.x, 2)))

# [3] Urgência depende do serviço: estratificação obrigatória
urgencia_servico <- atend_ind %>%
  filter(!is.na(I_Urgencia)) %>%
  group_by(Local, Tipo_Servico) %>%
  group_modify(~ prop_cluster(.x$I_Urgencia, .x$Cluster)) %>%
  ungroup() %>%
  group_by(Local) %>% mutate(Pct_dos_Atend_do_Local = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
  mutate(across(c(Pct, IC_Inf_Pct, IC_Sup_Pct), ~ round(.x, 2))) %>%
  rename(Pct_Urgencia = Pct)

# Concentração por estabelecimento
concentracao_estab <- atend %>%
  count(Ano, CODUNI) %>%
  group_by(Ano) %>%
  summarise(Atendimentos = sum(n), Estabelecimentos = n(),
            Pct_Maior_Estab = round(max(n) / sum(n) * 100, 1),
            Pct_4_Maiores = round(sum(sort(n, decreasing = TRUE)[1:min(4, n())]) / sum(n) * 100, 1),
            .groups = "drop")

distribuicao <- function(df, var) {
  bind_rows(df %>% mutate(Local = "Alagoas (total)"), df) %>%
    count(Local, Categoria = as.character(.data[[var]])) %>%
    group_by(Local) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
    mutate(Variavel = var, .before = 1)
}
vars_dist <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "CID_Asma", "Carater",
               "Tipo_Servico", "Tipo_Atend")
distribuicoes <- bind_rows(lapply(vars_dist, function(v) distribuicao(atend, v)))

tipo_atend_faixa <- atend %>%
  count(Faixa_Etaria, Tipo_Atend) %>%
  group_by(Faixa_Etaria) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup()

# ==============================================================================
# 5. PACIENTES PEDIÁTRICOS (BI, CNS válido) — unidade de inferência [4]
# ==============================================================================

atend_cns <- atend %>% filter(CNS_Valido)
mes_ini_bi <- mes_idx(sprintf("%d01", min(anos)))

pacientes <- atend_cns %>%
  arrange(CNS_PAC, Mes) %>%
  group_by(CNS_PAC) %>%
  summarise(
    Mes_Primeiro     = first(Mes),
    Ano_Primeiro     = first(Ano),
    Idade_Primeiro   = first(Idade),
    Sexo             = first(Sexo),
    Raca_Cor         = first(Raca_Cor),
    Local            = first(Local),
    Mun_Res          = first(Mun_Res),
    Servico_1o       = first(Tipo_Servico),
    Tipo_Atend_1o    = first(Tipo_Atend),
    N_Atendimentos   = n(),
    N_Contatos       = n_distinct(paste(CODUNI, Mes)),   # paciente × estab. × mês [7]
    Teve_J46         = any(Mal_Asmatico),
    Teve_Urgencia    = any(Urgencia),
    Teve_Serv_Urg    = any(Tipo_Servico == "Urgência/pronto-atendimento"),
    Teve_Fisio       = any(Tipo_Atend == "Fisioterapia"),
    .groups = "drop") %>%
  mutate(Faixa_Etaria = faixa_ped(Idade_Primeiro),
         Novo = Mes_Primeiro >= mes_ini_bi + washout_meses,       # [12]
         Freq_Contatos = cut(N_Contatos, c(0, 1, 2, 5, Inf), labels = c("1", "2", "3–5", "6+")))

resumo_pacientes <- pacientes %>%
  summarise(N_Pacientes          = n(),
            Atend_Mediana        = median(N_Atendimentos),
            Contatos_Mediana     = median(N_Contatos),
            Pct_Retorno          = round(mean(N_Contatos > 1) * 100, 2),
            Pct_Feminino         = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
            Idade_Mediana        = median(Idade_Primeiro),
            Pct_Teve_Urgencia    = round(mean(Teve_Urgencia) * 100, 2),
            Pct_Teve_Fisio       = round(mean(Teve_Fisio) * 100, 2),
            Pct_Teve_J46         = round(mean(Teve_J46) * 100, 2))

dist_pacientes <- bind_rows(lapply(
  c("Sexo", "Faixa_Etaria", "Raca_Cor", "Servico_1o", "Freq_Contatos"), function(v)
    pacientes %>% count(Local, Categoria = as.character(.data[[v]])) %>%
      bind_rows(pacientes %>% count(Categoria = as.character(.data[[v]])) %>%
                  mutate(Local = "Alagoas (total)")) %>%
      group_by(Local) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
      mutate(Variavel = v, .before = 1)))

# Maceió × interior no nível do paciente, com correção de Holm [4][15]
ign <- c("Ignorado", "Ignorada", "Sem informação")
pac_teste <- pacientes %>%
  mutate(across(c(Teve_Urgencia, Teve_J46, Teve_Fisio, Teve_Serv_Urg),
                ~ if_else(.x, "Sim", "Não")))
vars_teste_pac <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "Servico_1o", "Freq_Contatos",
                    "Teve_Urgencia", "Teve_Serv_Urg", "Teve_Fisio", "Teve_J46")
testes_local <- bind_rows(lapply(vars_teste_pac, function(v)
  teste_distribuicao(pac_teste %>% filter(!as.character(.data[[v]]) %in% ign), v, "Local"))) %>%
  ajustar_holm()

# [18] Raça/cor ausente: quem fica sem informação é diferente?
perfil_raca_missing <- pacientes %>%
  group_by(Raca = if_else(Raca_Cor == "Sem informação", "Sem informação", "Com informação")) %>%
  summarise(Pacientes = n(),
            Idade_Mediana = median(Idade_Primeiro),
            Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Pct_Maceio = round(mean(Local == "Maceió") * 100, 2),
            Pct_Serv_Urg_1o = round(mean(Servico_1o == "Urgência/pronto-atendimento") * 100, 2),
            Pct_Teve_Urgencia = round(mean(Teve_Urgencia) * 100, 2), .groups = "drop")

# Sensibilidade de idade e de período [9]
resumo_cenario <- function(df_atend, nome) {
  p <- df_atend %>% filter(CNS_Valido) %>% group_by(CNS_PAC) %>%
    summarise(Sexo = first(Sexo), Urg = any(Urgencia), J46 = any(Mal_Asmatico),
              N = n_distinct(paste(CODUNI, Mes)), .groups = "drop")
  tibble(Cenario = nome, Atendimentos = nrow(df_atend), Pacientes = nrow(p),
         Pct_Feminino = round(mean(p$Sexo == "Feminino") * 100, 2),
         Pct_Pac_Urgencia = round(mean(p$Urg) * 100, 2),
         Pct_Pac_J46 = round(mean(p$J46) * 100, 2),
         Contatos_por_Paciente = round(mean(p$N), 2),
         Pct_Atend_Serv_Urg = round(mean(df_atend$Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2))
}
sensibilidade <- bind_rows(
  resumo_cenario(atend, sprintf("Principal: 0–%d anos", idade_ped)),
  resumo_cenario(atend %>% filter(Idade <= idade_eca), sprintf("0–%d anos (ECA)", idade_eca)),
  resumo_cenario(atend %>% filter(Idade >= idade_min_sensibilidade),
                 sprintf("%d–%d anos (sem pré-escolares)", idade_min_sensibilidade, idade_ped)),
  resumo_cenario(atend %>% filter(!Covid), "Sem 2020–2021 (COVID)"))

# ==============================================================================
# 6. TENDÊNCIA E TAXAS POPULACIONAIS [3][8][9]
# ==============================================================================

pop_ano <- if (!is.null(pop_ped)) pop_ped %>% group_by(Ano) %>% summarise(Pop = sum(Pop)) else NULL
pop_faixa_ano <- if (!is.null(pop_ped)) pop_ped %>% group_by(Ano, Faixa) %>% summarise(Pop = sum(Pop), .groups = "drop") else NULL
# Padrão para taxa padronizada: população 0–19 de AL em 2022 (Censo)
pop_padrao <- if (!is.null(pop_ped)) pop_faixa_ano %>% filter(Ano == 2022) %>%
  mutate(w = Pop / sum(Pop)) %>% select(Faixa, w) else NULL

# Painel estável: estabelecimentos com atendimento pediátrico em todos os anos
# de anos_tend (relaxa para n-1, n-2 anos se ninguém cumprir)
anos_por_estab <- atend %>% filter(Ano %in% anos_tend) %>% distinct(CODUNI, Ano) %>% count(CODUNI)
for (folga in 0:3) {
  painel_estab <- anos_por_estab %>% filter(n >= length(anos_tend) - folga) %>% pull(CODUNI)
  if (length(painel_estab) >= 3) break
}
criterio_painel <- sprintf("Estabelecimentos com atendimento em ≥ %d dos %d anos de %d–%d (n = %d)",
                           length(anos_tend) - folga, length(anos_tend), min(anos_tend),
                           max(anos_tend), length(painel_estab))

# Pacientes no CEAF por ano são calculados na seção 7 e acrescentados depois
tendencia <- atend %>%
  group_by(Ano) %>%
  summarise(Atendimentos     = n(),
            Contatos         = n_distinct(paste(Cluster, CODUNI, Mes)),
            Pacientes        = n_distinct(CNS_PAC[CNS_Valido]),
            Atend_Painel     = sum(CODUNI %in% painel_estab),
            Pct_Urgencia     = round(mean(Urgencia) * 100, 2),
            Pct_Serv_Urg     = round(mean(Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2),
            Pct_Fisio        = round(mean(Tipo_Atend == "Fisioterapia") * 100, 2),
            .groups = "drop") %>%
  left_join(pacientes %>% filter(Novo) %>% count(Ano = Ano_Primeiro, name = "Pacientes_Novos"), by = "Ano") %>%
  left_join(concentracao_estab %>% select(Ano, Estabelecimentos, Pct_Maior_Estab), by = "Ano")

if (!is.null(pop_ped)) {
  tendencia <- tendencia %>% left_join(pop_ano, by = "Ano") %>%
    mutate(Taxa_Atend_10mil     = round(Atendimentos / Pop * 1e4, 2),
           Taxa_Pacientes_10mil = round(Pacientes / Pop * 1e4, 2))
  # Taxa de pacientes padronizada por idade (método direto)
  taxa_padronizada <- atend_cns %>%
    filter(Faixa_Etaria %in% niveis_faixa) %>%
    group_by(Ano, Faixa = as.character(Faixa_Etaria)) %>%
    summarise(Pacientes = n_distinct(CNS_PAC), .groups = "drop") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    left_join(pop_padrao, by = "Faixa") %>%
    group_by(Ano) %>%
    summarise(Taxa_Pac_Padronizada_10mil = round(sum(Pacientes / Pop * w) * 1e4, 2), .groups = "drop")
  tendencia <- tendencia %>% left_join(taxa_padronizada, by = "Ano")
  taxa_faixa <- atend_cns %>%
    group_by(Ano, Faixa = as.character(Faixa_Etaria)) %>%
    summarise(Pacientes = n_distinct(CNS_PAC), .groups = "drop") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    mutate(Taxa_10mil = round(Pacientes / Pop * 1e4, 2))
}

# Variação percentual anual (APC) por Prais-Winsten sobre log(taxa) [9]
apc_prais <- function(df, var, nome, excluir_covid = FALSE, periodo_anos = anos_tend) {
  d <- df %>% filter(Ano %in% periodo_anos, !is.na(.data[[var]]), .data[[var]] > 0)
  if (excluir_covid) d <- d %>% filter(!Ano %in% anos_covid)
  if (nrow(d) < 5) return(NULL)
  d$y <- log10(d[[var]]); d$id <- 1L
  m <- tryCatch(suppressMessages(prais::prais_winsten(y ~ Ano, data = d, index = c("id", "Ano"))),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  cf <- summary(m)$coefficients["Ano", ]
  b <- cf[[1]]; se <- cf[[2]]
  tibble(Serie = nome, Periodo = paste0(min(d$Ano), "–", max(d$Ano),
                                        if (excluir_covid) " (sem 2020–21)" else ""),
         APC_Pct = round((10^b - 1) * 100, 2),
         IC_Inf = round((10^(b - 1.96 * se) - 1) * 100, 2),
         IC_Sup = round((10^(b + 1.96 * se) - 1) * 100, 2),
         P_Valor = signif(cf[[4]], 3),
         Tendencia = case_when(cf[[4]] >= 0.05 ~ "Estacionária",
                               b > 0 ~ "Crescente", TRUE ~ "Decrescente"))
}

# Urgência: bruta × ajustada por estabelecimento (padronização marginal) [3]
# Só anos da série estável e estabelecimentos presentes em ≥ 2 anos: um
# estabelecimento de um ano só é inteiramente confundido com o ano.
estab_2anos <- atend %>% filter(Ano %in% anos_tend) %>% distinct(CODUNI, Ano) %>%
  count(CODUNI) %>% filter(n >= 2) %>% pull(CODUNI)
d_urg <- atend %>% filter(Carater != "Ignorado", Ano %in% anos_tend, CODUNI %in% estab_2anos) %>%
  mutate(Ano_f = factor(Ano), Estab = factor(CODUNI))
m_urg <- suppressWarnings(glm(Urgencia ~ Ano_f + Estab, family = binomial, data = d_urg))
urgencia_ajustada <- bind_rows(lapply(levels(d_urg$Ano_f), function(a) {
  nd <- d_urg %>% mutate(Ano_f = factor(a, levels = levels(d_urg$Ano_f)))
  tibble(Ano = as.integer(a),
         Pct_Ajustada = round(mean(predict(m_urg, nd, type = "response")) * 100, 2))
})) %>%
  right_join(atend %>% filter(Carater != "Ignorado") %>% group_by(Ano) %>%
               summarise(Pct_Bruta = round(mean(Urgencia) * 100, 2),
                         Pct_Painel = if (any(CODUNI %in% painel_estab))
                           round(mean(Urgencia[CODUNI %in% painel_estab]) * 100, 2) else NA_real_),
             by = "Ano") %>%
  arrange(Ano) %>% relocate(Ano, Pct_Bruta)

# Sensibilidade: sem o maior estabelecimento de cada ano
maior_estab <- atend %>% count(Ano, CODUNI) %>% group_by(Ano) %>% slice_max(n, n = 1, with_ties = FALSE)
urgencia_sem_maior <- atend %>% anti_join(maior_estab, by = c("Ano", "CODUNI")) %>%
  group_by(Ano) %>% summarise(Pct_Urg_Sem_Maior_Estab = round(mean(Urgencia) * 100, 2))
urgencia_ajustada <- urgencia_ajustada %>% left_join(urgencia_sem_maior, by = "Ano")

# ==============================================================================
# 7. TRATAMENTO NO CEAF (APAC DE MEDICAMENTOS) — 0–19 ANOS
# ==============================================================================

classe_med <- function(nome) {
  n <- toupper(coalesce(nome, ""))
  case_when(
    n == ""                                                       ~ "Sem nome (SIGTAP indisponível)",
    grepl("OMALIZUMABE|MEPOLIZUMABE|BENRALIZUMABE|DUPILUMABE", n) ~ "Imunobiológico",
    grepl("FORMOTEROL|SALMETEROL|VILANTEROL", n) &
      grepl("BUDESONIDA|FLUTICASONA|BECLOMETASONA|MOMETASONA", n) ~ "Corticoide inalatório + LABA",
    grepl("FORMOTEROL|SALMETEROL", n)                             ~ "LABA isolado",
    grepl("BUDESONIDA|BECLOMETASONA|FLUTICASONA|CICLESONIDA", n)  ~ "Corticoide inalatório",
    TRUE                                                          ~ "Outro")
}
med_curto <- function(m) {
  m <- toupper(coalesce(m, ""))
  case_when(
    grepl("FORMOTEROL 12.*BUDESONIDA 400.*CAPSULA", m) ~ "Formoterol 12 + budesonida 400 (cápsula)",
    grepl("FORMOTEROL 6.*BUDESONIDA 200", m)           ~ "Formoterol 6 + budesonida 200 (cápsula)",
    grepl("OMALIZUMABE|MEPOLIZUMABE", m)               ~ "Imunobiológico",
    TRUE ~ "Outros (budesonida, salmeterol, formoterol isolados; pó inalante)")
}

# Classe e apresentação pelo CÓDIGO SIGTAP (não depende do download dos nomes)
classe_cod <- function(cod) case_when(
  substr(cod, 1, 8) == "06048400"                      ~ "Imunobiológico",
  cod %in% c("0604040040", "0604040059", "0604040075") ~ "Corticoide inalatório + LABA",
  cod %in% c("0604040024", "0604040091")               ~ "LABA isolado",
  substr(cod, 1, 8) == "06042800"                      ~ "Corticoide inalatório",
  TRUE                                                 ~ "Outro")
apresentacao_cod <- function(cod) case_when(
  cod == "0604040040"                ~ "Formoterol 12 + budesonida 400 (cápsula)",
  cod == "0604040075"                ~ "Formoterol 6 + budesonida 200 (cápsula)",
  substr(cod, 1, 8) == "06048400"    ~ "Imunobiológico",
  TRUE ~ "Outros (budesonida, salmeterol, formoterol isolados; pó inalante)")

am0 <- am_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(AP_CMP, 1, 4)), Mes = mes_idx(AP_CMP),
         Mun_Res = substr(AP_MUNPCN, 1, 6))
am1 <- am0 %>% filter(Ano %in% anos)
am2 <- am1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
am3 <- am2 %>% distinct(AP_AUTORIZ, AP_CMP, AP_PRIPAL, .keep_all = TRUE) %>%
  mutate(Num_Idade = suppressWarnings(as.integer(AP_NUIDADE)),
         Idade = case_when(AP_COIDADE == "4"           ~ Num_Idade,
                           AP_COIDADE == "5"           ~ 100L + Num_Idade,
                           AP_COIDADE %in% c("2", "3") ~ 0L,      # dias/meses (a confirmar no .cnv)
                           TRUE                        ~ NA_integer_),
         Idade = if_else(!is.na(Idade) & (Idade < 0 | Idade > idade_max), NA_integer_, Idade))

# --- [1] Volume mensal: fim dos dados válidos e meses suspeitos ---------------
volume_mensal <- am3 %>% count(Mes, name = "APAC") %>%
  complete(Mes = seq(min(Mes), max(Mes)), fill = list(APAC = 0)) %>% arrange(Mes) %>%
  mutate(Mediana_12_Ant = sapply(seq_along(Mes), function(i)
           if (i <= 12) NA_real_ else median(APAC[(i - 12):(i - 1)])),
         Mediana_Local  = sapply(seq_along(Mes), function(i) {
           j <- setdiff(max(1, i - 6):min(length(Mes), i + 6), i); median(APAC[j]) }))
mes_fim_valido <- volume_mensal %>%
  filter(!is.na(Mediana_12_Ant), APAC >= limiar_volume_fim * Mediana_12_Ant) %>%
  pull(Mes) %>% max()
volume_mensal <- volume_mensal %>%
  mutate(Competencia = format(idx_data(Mes), "%Y-%m"),
         Apos_Fim    = Mes > mes_fim_valido,
         Suspeito    = !Apos_Fim & APAC < limiar_volume_suspeito * Mediana_Local)
meses_suspeitos <- volume_mensal$Mes[volume_mensal$Suspeito]

# Aprovação vem do PA (o AM não tem campo de aprovação): só para sensibilidade
chaves_aprovadas <- pa4 %>%
  filter(DOCORIG %in% c("P", "S"), substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP)
# [5] Quantidade apresentada por APAC × competência × medicamento
qtd_med <- pa3 %>%
  filter(substr(PA_PROC_ID, 1, 2) == "06") %>%
  group_by(AP_AUTORIZ = PA_AUTORIZ, AP_CMP = PA_CMP, AP_PRIPAL = PA_PROC_ID) %>%
  summarise(Qtd = sum(suppressWarnings(as.numeric(PA_QTDPRO)), na.rm = TRUE), .groups = "drop")

am_todas <- am3 %>%
  mutate(CNS_OK = cns_valido(AP_CNSPCN)) %>%
  left_join(chaves_aprovadas %>% mutate(Aprovada = TRUE),
            by = c("AP_AUTORIZ" = "PA_AUTORIZ", "AP_CMP" = "PA_CMP")) %>%
  mutate(Aprovada = coalesce(Aprovada, FALSE)) %>%
  left_join(qtd_med, by = c("AP_AUTORIZ", "AP_CMP", "AP_PRIPAL"))

am4 <- am_todas %>% filter(CNS_OK)
# Entrada: paciente com 1ª APAC do período aos 0–19 anos. Seguimento: até
# idade_ped + margem_idade (quem entra aos 19 e faz 20 durante o tratamento
# continua sendo acompanhado; o corte por idade só ocorre aos 21).
cns_entrada_ped <- am4 %>% filter(!is.na(Idade)) %>% group_by(AP_CNSPCN) %>%
  summarise(Idade_Entrada = Idade[which.min(Mes)], .groups = "drop") %>%
  filter(Idade_Entrada <= idade_ped) %>% pull(AP_CNSPCN)
am5 <- am4 %>% filter(AP_CNSPCN %in% cns_entrada_ped, !is.na(Idade),
                      Idade <= idade_ped + margem_idade)
am6 <- am5 %>% filter(Mes <= mes_fim_valido)

fluxo_am <- tibble(
  Etapa = c("Registros AM com CID J45/J46", "Competência no período", "Residente em AL",
            "Sem duplicatas (AUTORIZ × CMP × medicamento)", "CNS válido",
            sprintf("Entrada aos 0–%d anos; seguimento até %d anos (margem de %d ano)",
                    idade_ped, idade_ped + margem_idade, margem_idade),
            sprintf("Até o fim dos dados completos (%s)", format(idx_data(mes_fim_valido), "%m/%Y"))),
  Linhas = c(nrow(am0), nrow(am1), nrow(am2), nrow(am3), nrow(am4), nrow(am5), nrow(am6))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

classe_dose_gina <- function(idade, ug_dia) case_when(
  is.na(ug_dia)          ~ "Dose não informada",
  idade < 6              ~ "< 6 anos (fora da faixa do PCDT)",
  idade <= 11 & ug_dia <= 200 ~ "Baixa",
  idade <= 11 & ug_dia <= 400 ~ "Média",
  idade <= 11            ~ "Alta",
  ug_dia <= 400          ~ "Baixa",
  ug_dia <= 800          ~ "Média",
  TRUE                   ~ "Alta")
niveis_intensidade <- c("Baixa", "Média", "Alta", "Imunobiológico",
                        "< 6 anos (fora da faixa do PCDT)", "Dose não informada", "Outro")
ordem_intensidade <- c("Baixa" = 1, "Média" = 2, "Alta" = 3, "Imunobiológico" = 4)

am <- am6 %>%
  mutate(Sexo     = decod_sexo(AP_SEXO),
         Raca_Cor = decod_raca(AP_RACACOR),
         Local    = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
         Margem   = Idade > idade_ped,
         Faixa_Etaria = if_else(Margem, sprintf("%d (margem de seguimento)", idade_ped + 1),
                                as.character(faixa_ped(Idade))),
         Faixa_Etaria = factor(Faixa_Etaria, levels = c(niveis_faixa,
                               sprintf("%d (margem de seguimento)", idade_ped + 1), "Ignorada")),
         Peso     = suppressWarnings(as.numeric(AM_PESO)),
         Peso     = if_else(between(Peso, faixa_peso[1], faixa_peso[2]), Peso, NA_real_),
         Altura   = suppressWarnings(as.numeric(AM_ALTURA)),
         Altura   = if_else(between(Altura, faixa_altura[1], faixa_altura[2]), Altura, NA_real_),
         Gestante = coalesce(toupper(AM_GESTANT) == "S", FALSE),
         Obito    = AP_OBITO == "1" | AP_MOTSAI %in% c("41", "42", "43"),
         Saida    = AP_MOTSAI %in% c("31", "51"),
         Ug_Bud   = unname(ug_budesonida[AP_PRIPAL]),
         Ug_Dia   = if_else(!is.na(Ug_Bud) & coalesce(Qtd, 0) > 0, Qtd * Ug_Bud / 30, NA_real_)) %>%
  nomear_proc("AP_PRIPAL") %>%
  rename(Medicamento = Nome) %>%
  mutate(Classe = classe_cod(AP_PRIPAL),
         Apresentacao = apresentacao_cod(AP_PRIPAL),
         Intensidade = case_when(
           Classe == "Imunobiológico" ~ "Imunobiológico",
           !is.na(Ug_Bud)             ~ classe_dose_gina(Idade, Ug_Dia),
           TRUE                       ~ "Outro"))

# Escore-z de IMC-para-idade (OMS): idade em meses = ponto médio do ano [11]
z_imc <- function(sexo, idade, peso, altura) {
  s  <- if_else(sexo == "Masculino", 1L, if_else(sexo == "Feminino", 2L, NA_integer_))
  ok <- !is.na(s) & !is.na(peso) & !is.na(altura) & !is.na(idade)
  meses <- idade * 12 + 6
  z <- rep(NA_real_, length(idade))
  i5 <- which(ok & idade < 5)
  if (length(i5)) {
    r <- suppressWarnings(anthro::anthro_zscores(sex = s[i5], age = meses[i5], is_age_in_month = TRUE,
                                                 weight = peso[i5], lenhei = altura[i5]))
    z[i5] <- if_else(r$fbmi == 1, NA_real_, r$zbmi)
  }
  i19 <- which(ok & idade >= 5)
  if (length(i19)) {
    r <- suppressWarnings(anthroplus::anthroplus_zscores(sex = s[i19], age_in_months = pmin(meses[i19], 228),
                                                         height_in_cm = altura[i19], weight_in_kg = peso[i19]))
    z[i19] <- if_else(r$fbfa == 1, NA_real_, r$zbfa)
  }
  z
}
am <- am %>% mutate(Z_IMC = if_else(Gestante, NA_real_, z_imc(Sexo, Idade, Peso, Altura)))

completude_am <- am %>%
  summarise(N_Registros = n(),
            Pct_Raca_Sem_Info = round(mean(Raca_Cor == "Sem informação") * 100, 2),
            Pct_Peso_Invalido = round(mean(is.na(Peso)) * 100, 2),
            Pct_Altura_Invalida = round(mean(is.na(Altura)) * 100, 2),
            Pct_Z_IMC_Ausente = round(mean(is.na(Z_IMC)) * 100, 2),
            Pct_Dose_Nao_Informada = round(mean(Intensidade == "Dose não informada") * 100, 2),
            Pct_APAC_Aprovada = round(mean(Aprovada) * 100, 2))

# Paciente novo (washout): sem APAC (qualquer idade) nos 12 meses anteriores [6]
primeira_apac <- am_todas %>% filter(CNS_OK) %>% group_by(AP_CNSPCN) %>% summarise(Mes_1a_APAC = min(Mes))

# Características do paciente: só meses aos 0–19 anos (a margem vale só para
# o seguimento dos episódios de tratamento)
pacientes_am <- am %>%
  filter(!Margem) %>%
  arrange(AP_CNSPCN, Mes) %>%
  group_by(AP_CNSPCN) %>%
  summarise(
    Mes_Primeiro  = first(Mes),
    Ano_Primeiro  = first(Ano),
    Idade         = first(Idade),
    Sexo          = first(Sexo),
    Raca_Cor      = first(Raca_Cor),
    Local         = first(Local),
    Mun_Res       = first(Mun_Res),
    Meses_Com_APAC = n_distinct(Mes),
    Apresentacao_Principal = names(which.max(table(Apresentacao))),
    Usou_Biologico = any(Classe == "Imunobiológico"),
    Intensidade_Max = {
      v <- Intensidade[Intensidade %in% names(ordem_intensidade)]
      if (length(v)) names(ordem_intensidade)[max(ordem_intensidade[v])]
      else names(which.max(table(Intensidade)))
    },
    Fora_PCDT_6anos = any(Intensidade == "< 6 anos (fora da faixa do PCDT)"),
    Z_IMC = median(Z_IMC, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(primeira_apac, by = "AP_CNSPCN") %>%
  mutate(
    Z_IMC = if_else(is.nan(Z_IMC), NA_real_, Z_IMC),
    Novo  = Mes_1a_APAC == Mes_Primeiro & Mes_Primeiro >= mes_idx(sprintf("%d01", min(anos))) + washout_meses,
    Faixa_Etaria = faixa_ped(Idade),
    Intensidade_Max = factor(Intensidade_Max, levels = niveis_intensidade),
    Estado_Nutricional = case_when(
      is.na(Z_IMC)              ~ "Sem dado válido",
      Idade < 5 & Z_IMC > 3     ~ "Obesidade",
      Idade < 5 & Z_IMC > 2     ~ "Sobrepeso",
      Idade >= 5 & Z_IMC > 2    ~ "Obesidade",
      Idade >= 5 & Z_IMC > 1    ~ "Sobrepeso",
      Z_IMC < -2                ~ "Magreza",
      TRUE                      ~ "Eutrofia"),
    Duracao = cut(Meses_Com_APAC, c(0, 1, 5, 11, 23, Inf),
                  labels = c("1 mês", "2–5", "6–11", "12–23", "24+ meses")))

resumo_am <- pacientes_am %>%
  summarise(N_Pacientes = n(),
            Pct_Novos = round(mean(Novo) * 100, 2),
            Pct_Feminino = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
            Idade_Mediana = median(Idade),
            Meses_APAC_Mediana = median(Meses_Com_APAC),
            Pct_Biologico = round(mean(Usou_Biologico) * 100, 2),
            N_Fora_PCDT_6anos = sum(Fora_PCDT_6anos),
            N_Com_IMC_Valido = sum(Estado_Nutricional != "Sem dado válido"),
            Pct_Excesso_Peso = round(sum(Estado_Nutricional %in% c("Sobrepeso", "Obesidade")) /
                                       pmax(N_Com_IMC_Valido, 1) * 100, 2))

# Produção por ano: todas as APAC × só aprovadas (o filtro antigo) [1]
# Só meses aos 0–19 anos: é o numerador das taxas por habitante de 0–19
pacientes_am_ano <- am %>%
  filter(!Margem) %>%
  group_by(Ano) %>%
  summarise(Pacientes_Todas_APAC = n_distinct(AP_CNSPCN),
            Pacientes_So_Aprovadas = n_distinct(AP_CNSPCN[Aprovada]),
            Pct_APAC_Aprovadas = round(mean(Aprovada) * 100, 1), .groups = "drop") %>%
  left_join(pacientes_am %>% filter(Novo) %>% count(Ano = Ano_Primeiro, name = "Pacientes_Novos"), by = "Ano")

dist_am <- bind_rows(lapply(c("Sexo", "Faixa_Etaria", "Raca_Cor", "Local", "Apresentacao_Principal",
                              "Intensidade_Max", "Estado_Nutricional", "Duracao"), function(v)
  pacientes_am %>% count(Categoria = as.character(.data[[v]])) %>%
    mutate(Variavel = v, Pct = round(n / sum(n) * 100, 2), .before = 1)))

# Apresentação e dose por faixa etária (pacientes-mês) [5]
dose_faixa <- am %>%
  count(Faixa_Etaria, Apresentacao, Intensidade) %>%
  group_by(Faixa_Etaria) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup()
dose_resumo <- am %>% filter(!is.na(Ug_Bud)) %>%
  group_by(Apresentacao, Faixa_Etaria) %>%
  summarise(Pacientes_Mes = n(), Pct_Dose_Informada = round(mean(!is.na(Ug_Dia)) * 100, 1),
            Ug_Dia_Mediana = median(Ug_Dia, na.rm = TRUE),
            Ug_Dia_P25 = quantile(Ug_Dia, 0.25, na.rm = TRUE),
            Ug_Dia_P75 = quantile(Ug_Dia, 0.75, na.rm = TRUE), .groups = "drop")

# Intensidade × perfil (paciente), Holm [5][15]
intensidade_validas <- c("Baixa", "Média", "Alta", "Imunobiológico")
pac_int <- pacientes_am %>% filter(Intensidade_Max %in% intensidade_validas) %>%
  mutate(Intensidade_Max = droplevels(Intensidade_Max))
vars_perfil_am <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "Local", "Estado_Nutricional")
tratamento_perfil <- bind_rows(lapply(vars_perfil_am, function(v)
  pac_int %>% filter(!as.character(.data[[v]]) %in% c(ign, "Sem dado válido")) %>%
    count(Categoria = as.character(.data[[v]]), Intensidade_Max) %>%
    group_by(Categoria) %>% mutate(N_Categoria = sum(n), Pct = round(n / N_Categoria * 100, 2)) %>%
    ungroup() %>% mutate(Variavel = v, .before = 1)))
testes_tratamento <- bind_rows(lapply(vars_perfil_am, function(v)
  teste_distribuicao(pac_int %>% filter(!as.character(.data[[v]]) %in% c(ign, "Sem dado válido")),
                     "Intensidade_Max", v))) %>% ajustar_holm()

# ------------------------------------------------------------------------------
# 7b. EPISÓDIOS, PERSISTÊNCIA (KAPLAN-MEIER) E DESFECHO DO TRATAMENTO [1][13][14]
#   Episódio = paciente × medicamento; lacuna ≥ gap_meses meses OBSERVÁVEIS sem
#   APAC abre novo episódio. Meses suspeitos (queda abrupta de volume) não
#   contam como lacuna. Desfecho (hierárquico): óbito > atingiu o limite de
#   idade do seguimento (21 anos, com a margem) e seguiu no CEAF > troca de
#   medicamento até gap_meses após o fim >
#   em uso no fim dos dados > saída registrada > interrompeu.
# ------------------------------------------------------------------------------
meses_todos <- seq(min(am3$Mes), mes_fim_valido)
obs_cum <- setNames(cumsum(!meses_todos %in% meses_suspeitos), meses_todos)
meses_obs_entre <- function(m1, m2) {           # meses observáveis estritamente entre m1 e m2
  as.integer(obs_cum[as.character(m2 - 1)] - obs_cum[as.character(m1)])
}

# idade_limite = última idade acompanhada; registros acima dela (mesmos CNS)
# indicam que o tratamento continuou, só que fora do seguimento.
construir_episodios <- function(df, idade_limite = idade_ped + margem_idade) {
  adulto <- am_todas %>% filter(CNS_OK, !is.na(Idade), Idade > idade_limite, Mes <= mes_fim_valido) %>%
    distinct(AP_CNSPCN, Mes)
  base <- df %>%
    distinct(AP_CNSPCN, AP_PRIPAL, Apresentacao, Ug_Bud, Mes, .keep_all = TRUE) %>%
    arrange(AP_CNSPCN, AP_PRIPAL, Mes) %>%
    group_by(AP_CNSPCN, AP_PRIPAL) %>%
    mutate(Lacuna = if_else(row_number() == 1, NA_integer_, meses_obs_entre(lag(Mes), Mes)),
           Episodio = cumsum(is.na(Lacuna) | Lacuna >= gap_meses)) %>%
    ungroup()
  ep <- base %>%
    group_by(AP_CNSPCN, AP_PRIPAL, Apresentacao, Ug_Bud, Episodio) %>%
    summarise(Ini = min(Mes), Fim = max(Mes), Meses_Dispensados = n(),
              Idade_Ini = first(Idade), Obito = any(Obito), Saida = any(Saida),
              Classe = first(Classe), .groups = "drop")
  # Troca: outro medicamento do paciente iniciado entre Ini e Fim + gap
  troca <- ep %>% select(AP_CNSPCN, AP_PRIPAL, Episodio, Ini, Fim) %>%
    inner_join(ep %>% select(AP_CNSPCN, P2 = AP_PRIPAL, Ini2 = Ini, Ug2 = Ug_Bud, Classe2 = Classe),
               by = "AP_CNSPCN", relationship = "many-to-many") %>%
    filter(P2 != AP_PRIPAL, Ini2 > Ini, Ini2 <= Fim + gap_meses) %>%
    group_by(AP_CNSPCN, AP_PRIPAL, Episodio) %>%
    slice_min(Ini2, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(AP_CNSPCN, AP_PRIPAL, Episodio, Ug2, Classe2)
  virou_adulto <- ep %>% select(AP_CNSPCN, AP_PRIPAL, Episodio, Fim) %>%
    inner_join(adulto, by = "AP_CNSPCN", relationship = "many-to-many") %>%
    filter(Mes > Fim, Mes <= Fim + gap_meses + 1) %>%
    distinct(AP_CNSPCN, AP_PRIPAL, Episodio) %>% mutate(Virou_Adulto = TRUE)
  ep %>%
    left_join(troca, by = c("AP_CNSPCN", "AP_PRIPAL", "Episodio")) %>%
    left_join(virou_adulto, by = c("AP_CNSPCN", "AP_PRIPAL", "Episodio")) %>%
    mutate(
      Virou_Adulto = coalesce(Virou_Adulto, FALSE),
      Desfecho = case_when(
        Obito                                         ~ "Óbito registrado",
        Virou_Adulto                                  ~ "Atingiu o limite de idade do seguimento (segue no CEAF)",
        !is.na(Classe2) & Classe2 == "Imunobiológico" ~ "Trocou para imunobiológico",
        !is.na(Classe2) & coalesce(Ug2 > Ug_Bud, FALSE) ~ "Trocou para cápsula de maior dose",
        !is.na(Classe2) & coalesce(Ug2 < Ug_Bud, FALSE) ~ "Trocou para cápsula de menor dose",
        !is.na(Classe2)                               ~ "Trocou (outro medicamento)",
        Fim >= mes_fim_valido - (gap_meses - 1)       ~ "Em uso no fim dos dados",
        Saida                                         ~ "Saída registrada (encerramento/transferência)",
        TRUE                                          ~ "Interrompeu (≥ 3 meses sem dispensação)"),
      Meses_Seguimento = Fim - Ini + 1,
      Evento_Interrupcao = Desfecho == "Interrompeu (≥ 3 meses sem dispensação)") %>%
    # Margem só prolonga episódios iniciados até 19 anos; episódio que começa
    # na margem (20 anos) é início de tratamento adulto e sai da análise
    filter(Idade_Ini <= idade_ped)
}

niveis_desfecho <- c("Em uso no fim dos dados", "Trocou para cápsula de maior dose",
                     "Trocou para imunobiológico", "Trocou para cápsula de menor dose",
                     "Trocou (outro medicamento)", "Atingiu o limite de idade do seguimento (segue no CEAF)",
                     "Saída registrada (encerramento/transferência)",
                     "Interrompeu (≥ 3 meses sem dispensação)", "Óbito registrado")

episodios <- construir_episodios(am) %>% mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))
episodios_aprov <- construir_episodios(am %>% filter(Aprovada)) %>%
  mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))
# Sensibilidade: sem a margem (seguimento cortado aos 20 anos, versão anterior)
episodios_sem_margem <- construir_episodios(am %>% filter(!Margem), idade_limite = idade_ped) %>%
  mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))

resumir_desfecho <- function(ep, rotulo) ep %>%
  count(Apresentacao, Desfecho, .drop = FALSE) %>%
  group_by(Apresentacao) %>% filter(sum(n) > 0) %>%
  mutate(Episodios = sum(n), Pct = round(n / Episodios * 100, 2)) %>% ungroup() %>%
  mutate(Base = rotulo, .before = 1)

desfecho_medicamento <- resumir_desfecho(episodios, "Todas as APAC (principal)")
desfecho_sensibilidade <- bind_rows(
  desfecho_medicamento,
  resumir_desfecho(episodios_aprov, "Só APAC aprovadas (sensibilidade)"),
  resumir_desfecho(episodios_sem_margem, sprintf("Sem margem: seguimento só até %d anos", idade_ped))) %>%
  filter(Desfecho %in% c("Em uso no fim dos dados", "Interrompeu (≥ 3 meses sem dispensação)",
                         "Atingiu o limite de idade do seguimento (segue no CEAF)")) %>%
  select(Base, Apresentacao, Desfecho, Episodios, Pct)

motivo_saida_am <- am %>%
  mutate(Motivo = case_when(AP_MOTSAI == "21" ~ "Permanência (característica da doença)",
                            AP_MOTSAI == "31" ~ "Transferência",
                            AP_MOTSAI %in% c("41", "42", "43") ~ "Óbito",
                            AP_MOTSAI == "51" ~ "Encerramento administrativo",
                            TRUE ~ paste("Código", AP_MOTSAI))) %>%
  count(Motivo, name = "APAC") %>% mutate(Pct = round(APAC / sum(APAC) * 100, 3))

# Kaplan-Meier: tempo até interrupção (censura: em uso, troca, 20 anos, óbito)
km_fit <- survfit(Surv(Meses_Seguimento, Evento_Interrupcao) ~ Apresentacao,
                  data = episodios)
km_tab <- summary(km_fit, times = c(3, 6, 12, 24), extend = TRUE)
nomes_estratos <- if (is.null(km_fit$strata)) unique(episodios$Apresentacao) else
  sub("Apresentacao=", "", names(km_fit$strata))
tab_km <- summary(km_fit)$table
medianas_km <- if (is.matrix(tab_km)) tab_km[, "median"] else tab_km[["median"]]
persistencia_km <- tibble(
  Apresentacao = if (is.null(km_tab$strata)) nomes_estratos else sub("Apresentacao=", "", as.character(km_tab$strata)),
  Meses = km_tab$time, Em_Risco = km_tab$n.risk,
  Persistentes_Pct = round(km_tab$surv * 100, 1),
  IC_Inf = round(km_tab$lower * 100, 1), IC_Sup = round(km_tab$upper * 100, 1)) %>%
  left_join(tibble(Apresentacao = nomes_estratos, Mediana_Meses = unname(medianas_km)), by = "Apresentacao")
km_curvas <- tibble(
  Apresentacao = if (is.null(km_fit$strata)) nomes_estratos else rep(nomes_estratos, km_fit$strata),
  Meses = km_fit$time, Persistentes = km_fit$surv * 100)

# ==============================================================================
# 8. VÍNCULO ATENDIMENTO (BI) × TRATAMENTO (AM) PELO CNS [10]
# ==============================================================================
cns_am_ped <- unique(am$AP_CNSPCN)
cns_am_qualquer <- unique(am_todas$AP_CNSPCN[am_todas$CNS_OK])

pacientes <- pacientes %>%
  mutate(Em_AM = CNS_PAC %in% cns_am_ped,
         Em_Tratamento_AM = if_else(Em_AM, "Recebe medicamento (CEAF)", "Sem APAC de medicamento"))

vinculo_bi_am <- tibble(
  Pacientes_BI = nrow(pacientes), Pacientes_AM = length(cns_am_ped),
  Em_Ambos = sum(pacientes$Em_AM),
  Pct_BI_com_AM = round(Em_Ambos / Pacientes_BI * 100, 2),
  Pct_AM_com_BI = round(Em_Ambos / Pacientes_AM * 100, 2))

vinculo_perfil <- pacientes %>%
  group_by(Grupo = Em_Tratamento_AM) %>%
  summarise(Pacientes = n(), Idade_Mediana = median(Idade_Primeiro),
            Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Pct_Serv_Urg_1o = round(mean(Servico_1o == "Urgência/pronto-atendimento") * 100, 2),
            Contatos_Mediana = median(N_Contatos),
            k = sum(Teve_Urgencia), .groups = "drop") %>%
  mutate(Pct_Teve_Urgencia = round(k / Pacientes * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$k, .$Pacientes) %>% round(2)) %>% select(-k)

d_vinc <- pacientes %>%
  filter(Sexo != "Ignorado") %>%
  mutate(Faixa_Etaria = droplevels(Faixa_Etaria), Ano_1o = factor(Ano_Primeiro),
         Servico_1o = relevel(factor(Servico_1o), ref = "Ambulatório especializado"))
or_tab <- function(m, rotulo) {
  cf <- coeftest(m, vcov = vcovHC(m, type = "HC0"))["Em_AMTRUE", ]
  tibble(Modelo = rotulo, OR = round(exp(cf[[1]]), 2),
         IC_Inf = round(exp(cf[[1]] - 1.96 * cf[[2]]), 2),
         IC_Sup = round(exp(cf[[1]] + 1.96 * cf[[2]]), 2), P_Valor = signif(cf[[4]], 3))
}
vinculo_modelo <- bind_rows(
  or_tab(glm(Teve_Urgencia ~ Em_AM, binomial, d_vinc), "Bruto"),
  or_tab(glm(Teve_Urgencia ~ Em_AM + Faixa_Etaria + Sexo + Local + Ano_1o, binomial, d_vinc),
         "Ajustado: faixa, sexo, local, ano do 1º atendimento"),
  or_tab(suppressWarnings(glm(Teve_Urgencia ~ Em_AM + Faixa_Etaria + Sexo + Local + Ano_1o + Servico_1o,
                              binomial, d_vinc)),
         "Ajustado + tipo de serviço do 1º atendimento")) %>%
  mutate(Desfecho = "Teve ≥ 1 atendimento de urgência por asma", .before = 1)

vinculo_faixa <- pacientes %>%
  group_by(Faixa_Etaria) %>%
  summarise(Pacientes_BI = n(), Com_AM = sum(Em_AM), .groups = "drop") %>%
  mutate(Pct = round(Com_AM / Pacientes_BI * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$Com_AM, .$Pacientes_BI) %>% round(2))

# ==============================================================================
# 9. ANTES × DEPOIS DO INÍCIO DO TRATAMENTO — janelas múltiplas + controles [10]
#   Casos: pacientes NOVOS no CEAF (washout) presentes no BI (qualquer idade),
#   com 24 meses observáveis antes e depois do início. Desfecho: atendimentos
#   de urgência por asma no BI por janela de 12 meses.
#   Controles: crianças do BI SEM nenhuma APAC de medicamento, mesmo sexo e
#   faixa etária no mês índice (= mês de início do caso), com ≥ 1 atendimento
#   no BI nos 12 meses anteriores; mesma exigência aplicada aos casos no DiD.
# ------------------------------------------------------------------------------
bi_todos <- bi3 %>% filter(cns_valido(CNS_PAC)) %>%
  transmute(CNS_PAC, Mes, Urgencia = decod_carater(CATEND) == "Urgência")
mes_fim_bi <- max(bi_todos$Mes)
janelas <- tibble(Janela = c("−24 a −13", "−12 a −1", "+1 a +12", "+13 a +24"),
                  De = c(-24, -12, 1, 13), Ate = c(-13, -1, 12, 24))

contar_janelas <- function(ids_index) {
  ids_index %>%
    left_join(bi_todos, by = "CNS_PAC", relationship = "many-to-many") %>%
    mutate(d = Mes - Mes_Index) %>%
    tidyr::crossing(janelas) %>%
    group_by(ID, CNS_PAC, Grupo, Janela, De) %>%
    summarise(Urg = sum(Urgencia & d >= De & d <= Ate, na.rm = TRUE),
              Atend = sum(d >= De & d <= Ate, na.rm = TRUE), .groups = "drop") %>%
    mutate(Janela = factor(Janela, levels = janelas$Janela))
}

casos <- pacientes_am %>%
  filter(Novo, AP_CNSPCN %in% bi_todos$CNS_PAC,
         Mes_Primeiro >= mes_ini_bi + 24, Mes_Primeiro <= mes_fim_bi - 24) %>%
  transmute(ID = paste0("caso_", row_number()), CNS_PAC = AP_CNSPCN, Grupo = "Caso (inicia CEAF)",
            Mes_Index = Mes_Primeiro, Sexo, Faixa_Etaria)

janelas_casos <- contar_janelas(casos)

resumo_janelas <- janelas_casos %>%
  group_by(Grupo, Janela) %>%
  summarise(Pacientes = n_distinct(ID), Pct_com_Urgencia = round(mean(Urg > 0) * 100, 2),
            Urg_por_Paciente = round(mean(Urg), 3), Atend_por_Paciente = round(mean(Atend), 3),
            .groups = "drop")

rr_janelas <- tryCatch({
  d <- janelas_casos %>% group_by(ID) %>% filter(sum(Urg) > 0) %>% ungroup() %>%
    mutate(Janela = relevel(Janela, ref = "−12 a −1"))
  m <- glm(Urg ~ Janela + factor(ID), family = poisson, data = d)
  cf <- coef(summary(m)); cf <- cf[grepl("^Janela", rownames(cf)), , drop = FALSE]
  tibble(Janela = sub("^Janela", "", rownames(cf)), RR = round(exp(cf[, 1]), 2),
         IC_Inf = round(exp(cf[, 1] - 1.96 * cf[, 2]), 2),
         IC_Sup = round(exp(cf[, 1] + 1.96 * cf[, 2]), 2), P_Valor = signif(cf[, 4], 3),
         Pacientes_com_Evento = n_distinct(d$ID), Referencia = "−12 a −1")
}, error = function(e) tibble(Janela = NA, Observacao = conditionMessage(e)))

# Controles pareados e diferença-em-diferenças (janelas −12..−1 × +1..+12)
idade_no_mes <- bi %>% filter(cns_valido(CNS_PAC), !is.na(Data_Nasc)) %>%
  distinct(CNS_PAC, Data_Nasc) %>% group_by(CNS_PAC) %>% slice(1) %>% ungroup()
sexo_bi <- atend_cns %>% distinct(CNS_PAC, Sexo) %>% group_by(CNS_PAC) %>% slice(1) %>% ungroup()
pool_controles <- pacientes %>% filter(!CNS_PAC %in% cns_am_qualquer) %>%
  select(CNS_PAC) %>% left_join(idade_no_mes, by = "CNS_PAC") %>% left_join(sexo_bi, by = "CNS_PAC")
teve_atend_pre <- function(cns, m) {
  bi_todos %>% filter(CNS_PAC %in% cns, Mes >= m - 12, Mes <= m - 1) %>% distinct(CNS_PAC) %>% pull()
}
casos_did <- casos %>% filter(CNS_PAC %in% unlist(lapply(seq_len(nrow(casos)), function(i)
  teve_atend_pre(casos$CNS_PAC[i], casos$Mes_Index[i]))))
controles <- bind_rows(lapply(seq_len(nrow(casos_did)), function(i) {
  cs <- casos_did[i, ]
  cand <- pool_controles %>%
    mutate(Idade_Index = floor(as.numeric(idx_data(cs$Mes_Index) - Data_Nasc) / 365.25),
           Faixa_Index = faixa_ped(Idade_Index)) %>%
    filter(Sexo == cs$Sexo, Faixa_Index == cs$Faixa_Etaria)
  cand <- cand %>% filter(CNS_PAC %in% teve_atend_pre(cand$CNS_PAC, cs$Mes_Index))
  if (!nrow(cand)) return(NULL)
  cand %>% slice_sample(n = min(controles_por_caso, nrow(cand))) %>%
    transmute(ID = paste0(cs$ID, "_ctrl", row_number()), CNS_PAC, Grupo = "Controle pareado",
              Mes_Index = cs$Mes_Index, Sexo, Faixa_Etaria = Faixa_Index)
}))
janelas_did <- contar_janelas(bind_rows(casos_did %>% mutate(Grupo = "Caso (inicia CEAF)"), controles)) %>%
  filter(Janela %in% c("−12 a −1", "+1 a +12")) %>%
  mutate(Pos = Janela == "+1 a +12", Caso = Grupo == "Caso (inicia CEAF)")
resumo_did <- janelas_did %>% group_by(Grupo, Janela) %>%
  summarise(Pacientes = n_distinct(ID), Pct_com_Urgencia = round(mean(Urg > 0) * 100, 2),
            Urg_por_Paciente = round(mean(Urg), 3), .groups = "drop")
did_modelo <- tryCatch({
  # Poisson condicional: só pacientes com ≥ 1 evento contribuem
  d <- janelas_did %>% group_by(ID) %>% filter(sum(Urg) > 0) %>% ungroup()
  m <- glm(Urg ~ Pos * Caso + factor(ID), family = poisson, data = d)
  v <- vcovCL(m, cluster = ~ID)
  b <- coef(m)[["PosTRUE:CasoTRUE"]]; se <- sqrt(v["PosTRUE:CasoTRUE", "PosTRUE:CasoTRUE"])
  if (!is.finite(se)) se <- coef(summary(m))["PosTRUE:CasoTRUE", 2]
  tibble(Termo = "Razão de taxas (DiD): mudança pós/pré nos casos ÷ nos controles",
         RR = round(exp(b), 2), IC_Inf = round(exp(b - 1.96 * se), 2),
         IC_Sup = round(exp(b + 1.96 * se), 2), P_Valor = signif(2 * pnorm(-abs(b / se)), 3),
         Casos = n_distinct(casos_did$ID), Controles = nrow(controles))
}, error = function(e) tibble(Termo = "DiD não estimável", Observacao = conditionMessage(e),
                              Casos = nrow(casos_did), Controles = nrow(controles)))

# ==============================================================================
# 9b. INTERNAÇÕES POR ASMA (SIH/RD) — 0–19 ANOS
#   Caso: AIH com CID PRINCIPAL J45/J46 (definição usual de internação por asma
#   e da lista de condições sensíveis à atenção primária, Portaria SAS 221/2008).
#   Ano = data de internação. AIH de continuação (IDENT = 5) e reapresentações
#   (mesmo N_AIH) excluídas. Só arquivos de AL: residentes internados em outros
#   estados não entram.
#   Reinternação: os arquivos públicos não têm CNS. Usa-se uma chave
#   determinística (data de nascimento + sexo + CEP + município de residência).
#   Duas AIH da mesma chave com nova internação até 1 dia após a alta anterior
#   são o mesmo episódio (transferência/continuidade). A versão sem CEP serve
#   de limite superior (mais falsos vínculos).
# ==============================================================================
decod_sexo_sih <- function(x) case_when(x == "1" ~ "Masculino", x %in% c("2", "3") ~ "Feminino",
                                        TRUE ~ "Ignorado")
desfecho_aih <- function(cob, morte) case_when(
  morte == "1" | substr(cob, 1, 1) == "4" ~ "Óbito",
  substr(cob, 1, 1) == "1"                ~ "Alta",
  substr(cob, 1, 1) == "3"                ~ "Transferência",
  substr(cob, 1, 1) == "2"                ~ "Permanência (AIH seguinte)",
  TRUE                                    ~ "Outro/encerramento")

sih0 <- sih_raw %>% as_tibble() %>%
  mutate(Data_Inter = as.Date(DT_INTER, "%Y%m%d"), Data_Saida = as.Date(DT_SAIDA, "%Y%m%d"),
         Data_Nasc = as.Date(NASC, "%Y%m%d"), Ano = as.integer(format(Data_Inter, "%Y")),
         Cmpt = paste0(ANO_CMPT, MES_CMPT))
sih1 <- sih0 %>% filter(Ano %in% anos)
sih2 <- sih1 %>% filter(substr(MUNIC_RES, 1, 2) == cod_uf_al)
sih3 <- sih2 %>% filter(IDENT != "5")
sih4 <- sih3 %>% arrange(N_AIH, desc(Cmpt)) %>% distinct(N_AIH, .keep_all = TRUE)
sih5 <- sih4 %>% filter(grepl(regex_asma, DIAG_PRINC))
sih5 <- sih5 %>%
  mutate(Idade = floor(as.numeric(Data_Inter - Data_Nasc) / 365.25),
         Idade_Campo = case_when(COD_IDADE == "4" ~ as.numeric(IDADE),
                                 COD_IDADE == "5" ~ 100 + as.numeric(IDADE),
                                 COD_IDADE %in% c("2", "3") ~ 0, TRUE ~ NA_real_),
         Idade = if_else(is.na(Idade) | Idade < 0 | Idade > idade_max, Idade_Campo, Idade))
sih6 <- sih5 %>% filter(!is.na(Idade), Idade <= idade_ped)

fluxo_sih <- tibble(
  Etapa = c("AIH com J45/J46 em qualquer diagnóstico", sprintf("Internação em %d–%d", min(anos), max(anos)),
            "Residente em AL", "Exclui AIH de continuação (IDENT = 5)",
            "Exclui reapresentação (N_AIH repetido)", "CID principal J45/J46",
            sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(sih0), nrow(sih1), nrow(sih2), nrow(sih3), nrow(sih4), nrow(sih5), nrow(sih6))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# Asma só como diagnóstico secundário (descritivo, não entra na definição)
sih_asma_secundaria <- sih4 %>%
  filter(!grepl(regex_asma, DIAG_PRINC)) %>%
  mutate(Idade = floor(as.numeric(Data_Inter - Data_Nasc) / 365.25)) %>%
  filter(!is.na(Idade), Idade <= idade_ped) %>%
  count(Capitulo_CID_Principal = substr(DIAG_PRINC, 1, 1), name = "AIH") %>%
  arrange(desc(AIH))

intern <- sih6 %>%
  mutate(Sexo = decod_sexo_sih(SEXO), Raca_Cor = decod_raca(RACA_COR),
         Faixa_Etaria = faixa_ped(Idade),
         Mun_Res = substr(MUNIC_RES, 1, 6),
         Local = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
         Fora_Municipio = substr(MUNIC_MOV, 1, 6) != Mun_Res,
         CID_Asma = cid_asma(DIAG_PRINC),
         Carater = decod_carater(CAR_INT),
         Dias = suppressWarnings(as.numeric(DIAS_PERM)),
         UTI = coalesce(suppressWarnings(as.numeric(UTI_MES_TO)), 0) > 0 |
               !MARCA_UTI %in% c("", "00", NA),
         Desfecho = desfecho_aih(COBRANCA, MORTE),
         Obito = Desfecho == "Óbito",
         Mes_Inter = mes_idx(format(Data_Inter, "%Y%m")),
         Covid = Ano %in% anos_covid,
         Chave     = paste(NASC, Sexo, CEP, Mun_Res),
         Chave_sem_CEP = paste(NASC, Sexo, Mun_Res))

resumo_intern <- intern %>%
  summarise(Internacoes = n(), Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Idade_Mediana = median(Idade), Pct_Menor_5 = round(mean(Idade < 5) * 100, 2),
            Pct_J46 = round(mean(CID_Asma == "J46 Estado de mal asmático") * 100, 2),
            Pct_Urgencia = round(mean(Carater == "Urgência") * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE),
            Permanencia_P75 = quantile(Dias, 0.75, na.rm = TRUE),
            Pct_UTI = round(mean(UTI) * 100, 2),
            Obitos = sum(Obito), Letalidade_Pct = round(mean(Obito) * 100, 3),
            Pct_Fora_Municipio = round(mean(Fora_Municipio) * 100, 2))

perfil_intern_faixa <- intern %>%
  group_by(Faixa_Etaria) %>%
  summarise(Internacoes = n(), Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE),
            k_UTI = sum(UTI), Obitos = sum(Obito), .groups = "drop") %>%
  mutate(Pct_UTI = round(k_UTI / Internacoes * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$k_UTI, .$Internacoes) %>% round(2) %>%
              rename(UTI_IC_Inf = IC_Inf_Pct, UTI_IC_Sup = IC_Sup_Pct)) %>%
  select(-k_UTI)

desfecho_hospitalar <- intern %>% count(Desfecho, name = "Internacoes") %>%
  mutate(Pct = round(Internacoes / sum(Internacoes) * 100, 2))

dist_intern <- bind_rows(lapply(c("Sexo", "Faixa_Etaria", "Raca_Cor", "CID_Asma", "Carater", "Local"),
  function(v) intern %>% count(Categoria = as.character(.data[[v]])) %>%
    mutate(Variavel = v, Pct = round(n / sum(n) * 100, 2), .before = 1)))

# Série anual e taxas por 10 mil hab. de 0–19 anos (bruta e padronizada)
intern_ano <- intern %>% group_by(Ano) %>%
  summarise(Internacoes = n(), Pct_UTI = round(mean(UTI) * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE), Obitos = sum(Obito), .groups = "drop")
if (!is.null(pop_ped)) {
  intern_ano <- intern_ano %>% left_join(pop_ano, by = "Ano") %>%
    mutate(Taxa_Intern_10mil = round(Internacoes / Pop * 1e4, 2)) %>%
    left_join(intern %>% filter(Faixa_Etaria %in% niveis_faixa) %>%
                count(Ano, Faixa = as.character(Faixa_Etaria), name = "n") %>%
                left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
                left_join(pop_padrao, by = "Faixa") %>%
                group_by(Ano) %>%
                summarise(Taxa_Intern_Padronizada_10mil = round(sum(n / Pop * w) * 1e4, 2), .groups = "drop"),
              by = "Ano")
  taxa_intern_faixa <- intern %>% count(Ano, Faixa = as.character(Faixa_Etaria), name = "Internacoes") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    mutate(Taxa_10mil = round(Internacoes / Pop * 1e4, 2))
  apc_intern <- bind_rows(
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", TRUE, periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_Padronizada_10mil", "Internações, padronizada por idade", periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", periodo_anos = anos_tend))
}

# Concentração por hospital: mudanças na rede explicam parte da série
concentracao_hosp <- intern %>% count(Ano, CNES) %>%
  group_by(Ano) %>%
  summarise(Internacoes = sum(n), Hospitais = n(),
            Maior_Hospital = CNES[which.max(n)], Pct_Maior_Hospital = round(max(n) / sum(n) * 100, 1),
            .groups = "drop") %>%
  left_join(intern %>% group_by(Ano) %>% summarise(Pct_UTI = round(mean(UTI) * 100, 2)), by = "Ano")

# Sazonalidade: média de internações por mês do calendário (exclui anos COVID)
sazonalidade <- intern %>% filter(!Covid) %>%
  mutate(Mes_Cal = as.integer(format(Data_Inter, "%m"))) %>%
  count(Ano, Mes_Cal) %>%
  complete(Ano, Mes_Cal = 1:12, fill = list(n = 0)) %>%
  group_by(Mes_Cal) %>%
  summarise(Media = round(mean(n), 2), DP = round(sd(n), 2), .groups = "drop") %>%
  mutate(Indice_Sazonal = round(Media / mean(Media) * 100, 1))

# Reinternação pela chave determinística
reinternacao <- function(df, chave, rotulo) {
  ep <- df %>%
    transmute(Chave = .data[[chave]], Data_Inter, Data_Saida, Obito) %>%
    filter(!is.na(Data_Inter), !is.na(Data_Saida)) %>%
    arrange(Chave, Data_Inter) %>%
    group_by(Chave) %>%
    mutate(Saida_Max_Ant = lag(cummax(as.numeric(Data_Saida))),
           Novo_Ep = is.na(Saida_Max_Ant) | as.numeric(Data_Inter) > Saida_Max_Ant + 1,
           Ep = cumsum(Novo_Ep)) %>%
    group_by(Chave, Ep) %>%
    summarise(Ini = min(Data_Inter), Fim = max(Data_Saida), Obito = any(Obito), .groups = "drop") %>%
    arrange(Chave, Ini) %>%
    group_by(Chave) %>%
    mutate(Prox_Ini = lead(Ini), Dias_Ate_Prox = as.numeric(Prox_Ini - Fim)) %>%
    ungroup()
  fim_obs <- max(df$Data_Saida, na.rm = TRUE)
  idx <- ep %>% filter(!Obito, Fim <= fim_obs - 30)
  idx12 <- ep %>% filter(!Obito, Fim <= fim_obs - 365)
  pac <- ep %>% count(Chave, name = "Episodios")
  r30 <- sum(!is.na(idx$Dias_Ate_Prox) & idx$Dias_Ate_Prox <= 30)
  r365 <- sum(!is.na(idx12$Dias_Ate_Prox) & idx12$Dias_Ate_Prox <= 365)
  tibble(Chave = rotulo, AIH = nrow(df), Episodios = nrow(ep), Criancas = nrow(pac),
         Pct_Criancas_2mais_Episodios = round(mean(pac$Episodios >= 2) * 100, 2),
         Reintern_30d_Pct = round(r30 / nrow(idx) * 100, 2),
         Reintern_30d_IC_Inf = round(wilson_ci_pct(r30, nrow(idx))$IC_Inf_Pct, 2),
         Reintern_30d_IC_Sup = round(wilson_ci_pct(r30, nrow(idx))$IC_Sup_Pct, 2),
         Reintern_12m_Pct = round(r365 / nrow(idx12) * 100, 2),
         Reintern_12m_IC_Inf = round(wilson_ci_pct(r365, nrow(idx12))$IC_Inf_Pct, 2),
         Reintern_12m_IC_Sup = round(wilson_ci_pct(r365, nrow(idx12))$IC_Sup_Pct, 2))
}
reinternacoes <- bind_rows(
  reinternacao(intern, "Chave", "Nascimento + sexo + CEP + município (principal)"),
  reinternacao(intern, "Chave_sem_CEP", "Nascimento + sexo + município (limite superior)"))
reintern_faixa <- bind_rows(lapply(niveis_faixa, function(f)
  reinternacao(intern %>% filter(Faixa_Etaria == f), "Chave", f) %>% rename(Faixa_Etaria = Chave)))

# ==============================================================================
# 10. NÍVEL MUNICIPAL, REGIÕES DE SAÚDE E ANÁLISE ECOLÓGICA [8][16]
# ==============================================================================
pop_mun <- if (!is.null(pop_ped)) pop_ped %>% group_by(Municipio = CO_MUN) %>%
  summarise(Pop_Anos = sum(Pop), Pop_Media = sum(Pop) / length(anos)) else NULL

asma_mun <- atend %>%
  group_by(Municipio = Mun_Res) %>%
  summarise(N_Atendimentos = n(),
            Pct_Urgencia = round(mean(Urgencia) * 100, 2),
            Urg = sum(Urgencia),
            Pct_Fora_Municipio = round(mean(Fora_Municipio) * 100, 2),
            Pct_Serv_Urg = round(mean(Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2),
            .groups = "drop") %>%
  full_join(pacientes %>% count(Municipio = Mun_Res, name = "Pacientes_BI"), by = "Municipio") %>%
  full_join(pacientes_am %>% count(Municipio = Mun_Res, name = "Pacientes_AM"), by = "Municipio") %>%
  full_join(intern %>% count(Municipio = Mun_Res, name = "Internacoes"), by = "Municipio") %>%
  mutate(across(c(N_Atendimentos, Pacientes_BI, Pacientes_AM, Internacoes), ~ coalesce(.x, 0L)))

regioes <- tryCatch(brpop::mun_reg_saude_449 %>%
                      transmute(Municipio = substr(as.character(code_muni), 1, 6),
                                Regiao_Saude = as.character(codi_reg_saude)) %>%
                      filter(substr(Municipio, 1, 2) == cod_uf_al),
                    error = function(e) NULL)
if (!is.null(pop_mun)) {
  asma_mun <- asma_mun %>% left_join(pop_mun, by = "Municipio") %>%
    mutate(Taxa_Pac_BI_10mil_ano = round(Pacientes_BI / Pop_Anos * 1e4, 2),
           Taxa_Pac_AM_10mil_ano = round(Pacientes_AM / Pop_Anos * 1e4, 2),
           Taxa_Intern_10mil_ano = round(Internacoes / Pop_Anos * 1e4, 2))
  # Maceió × interior: razão de taxas (quase-Poisson com offset) [8]
  rr_local <- function(var) {
    d <- asma_mun %>% filter(!is.na(Pop_Anos)) %>%
      mutate(Local = factor(if_else(Municipio == cod_maceio, "Maceió", "Interior"),
                            levels = c("Interior", "Maceió")))
    m <- glm(d[[var]] ~ Local + offset(log(Pop_Anos)), family = quasipoisson, data = d)
    cf <- coef(summary(m))["LocalMaceió", ]
    tibble(Desfecho = var, RR_Maceio_vs_Interior = round(exp(cf[[1]]), 2),
           IC_Inf = round(exp(cf[[1]] - 1.96 * cf[[2]]), 2),
           IC_Sup = round(exp(cf[[1]] + 1.96 * cf[[2]]), 2), P_Valor = signif(cf[[4]], 3))
  }
  razao_taxas_local <- bind_rows(rr_local("Pacientes_BI"), rr_local("Pacientes_AM"),
                                 rr_local("Internacoes")) %>%
    mutate(Desfecho = recode(Desfecho, Pacientes_BI = "Pacientes atendidos (BI) por habitante 0–19",
                             Pacientes_AM = "Pacientes no CEAF por habitante 0–19",
                             Internacoes = "Internações por asma por habitante 0–19"))
  # Ecológico: municípios com mais crianças no CEAF / atendidas internam menos?
  # Quase-Poisson com offset da população; RR por +1 paciente/10 mil hab.-ano.
  d_eco_int <- asma_mun %>% filter(!is.na(Pop_Anos), Pop_Anos > 0)
  m_int <- glm(Internacoes ~ Taxa_Pac_AM_10mil_ano + Taxa_Pac_BI_10mil_ano + offset(log(Pop_Anos)),
               family = quasipoisson, data = d_eco_int)
  cf_int <- coef(summary(m_int))[-1, , drop = FALSE]
  sp_int <- cor.test(d_eco_int$Taxa_Pac_AM_10mil_ano, d_eco_int$Taxa_Intern_10mil_ano,
                     method = "spearman", exact = FALSE)
  eco_internacao <- bind_rows(
    tibble(Analise = c("RR de internação por +1 paciente/10 mil no CEAF (ajustado por BI)",
                       "RR de internação por +1 paciente/10 mil atendido no BI (ajustado por CEAF)"),
           Estimativa = round(exp(cf_int[, 1]), 3),
           IC_Inf = round(exp(cf_int[, 1] - 1.96 * cf_int[, 2]), 3),
           IC_Sup = round(exp(cf_int[, 1] + 1.96 * cf_int[, 2]), 3),
           P_Valor = signif(cf_int[, 4], 3)),
    tibble(Analise = "Spearman: taxa CEAF × taxa de internação", Estimativa = round(sp_int$estimate, 3),
           P_Valor = signif(sp_int$p.value, 3))) %>%
    mutate(N_Municipios = nrow(d_eco_int))
  if (!is.null(regioes)) {
    taxas_regiao <- asma_mun %>% left_join(regioes, by = "Municipio") %>%
      filter(!is.na(Regiao_Saude)) %>%
      group_by(Regiao_Saude) %>%
      summarise(Municipios = n(), Pop_Media = round(sum(Pop_Media, na.rm = TRUE)),
                Pacientes_BI = sum(Pacientes_BI), Pacientes_AM = sum(Pacientes_AM),
                Taxa_Pac_BI_10mil_ano = round(sum(Pacientes_BI) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                Taxa_Pac_AM_10mil_ano = round(sum(Pacientes_AM) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                Internacoes = sum(Internacoes),
                Taxa_Intern_10mil_ano = round(sum(Internacoes) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                .groups = "drop")
  }
}

mun_eco <- asma_mun %>% filter(N_Atendimentos >= min_registros_mun)
eco <- if (nrow(mun_eco) >= 5) {
  sp <- cor.test(mun_eco$Pct_Fora_Municipio, mun_eco$Pct_Urgencia, method = "spearman", exact = FALSE)
  m  <- glm(cbind(Urg, N_Atendimentos - Urg) ~ I(Pct_Fora_Municipio / 10) + I(Pct_Serv_Urg / 10),
            family = quasibinomial, data = mun_eco)
  cf <- coef(summary(m))
  tibble(Analise = c("Spearman (não ponderado)",
                     "OR por +10 p.p. fora do município (quase-binomial ponderada)",
                     "OR por +10 p.p. em serviço de urgência (mesmo modelo)"),
         Estimativa = round(c(sp$estimate, exp(cf[2, 1]), exp(cf[3, 1])), 3),
         IC_Inf = c(NA, round(exp(cf[2, 1] - 1.96 * cf[2, 2]), 3), round(exp(cf[3, 1] - 1.96 * cf[3, 2]), 3)),
         IC_Sup = c(NA, round(exp(cf[2, 1] + 1.96 * cf[2, 2]), 3), round(exp(cf[3, 1] + 1.96 * cf[3, 2]), 3)),
         P_Valor = signif(c(sp$p.value, cf[2, 4], cf[3, 4]), 3),
         N_Municipios = nrow(mun_eco))
} else tibble(Analise = "Municípios insuficientes", N_Municipios = nrow(mun_eco))

# Pacientes no CEAF por ano na tabela de tendência (+ taxa) e APC
tendencia <- tendencia %>%
  left_join(pacientes_am_ano %>% select(Ano, Pacientes_CEAF = Pacientes_Todas_APAC), by = "Ano") %>%
  left_join(intern_ano %>% select(Ano, Internacoes, any_of(c("Taxa_Intern_10mil", "Taxa_Intern_Padronizada_10mil"))),
            by = "Ano")
if (!is.null(pop_ped)) tendencia <- tendencia %>%
  mutate(Taxa_CEAF_10mil = round(Pacientes_CEAF / Pop * 1e4, 2))
apc <- if (!is.null(pop_ped)) bind_rows(
  apc_prais(tendencia, "Taxa_Pacientes_10mil", "Pacientes atendidos (BI) / 10 mil"),
  apc_prais(tendencia, "Taxa_Pacientes_10mil", "Pacientes atendidos (BI) / 10 mil", TRUE),
  apc_prais(tendencia, "Taxa_Pac_Padronizada_10mil", "Pacientes (BI), padronizada por idade"),
  apc_prais(tendencia, "Taxa_CEAF_10mil", "Pacientes no CEAF / 10 mil"),
  apc_prais(tendencia, "Taxa_CEAF_10mil", "Pacientes no CEAF / 10 mil", TRUE)) else tibble()

# ==============================================================================
# GRÁFICOS
# ==============================================================================
periodo   <- sprintf("%d–%d", min(anos), max(anos))
fonte_bi  <- "Fonte: SIASUS/DATASUS (BI = BPA-I com CNS). 0–19 anos, CID J45/J46."
fonte_am  <- "Fonte: SIASUS/DATASUS (APAC de Medicamentos, CEAF). 0–19 anos."
tema <- theme_minimal(base_size = 12) + theme(legend.position = "bottom")
cores_local <- c("Alagoas (0–19 anos)" = "gray40", "Maceió" = "#D55E00", "Interior" = "#0072B2")

p_indicadores <- indicadores %>%
  filter(Estratificacao %in% c("Total", "Local")) %>%
  mutate(Grupo = factor(Grupo, levels = names(cores_local))) %>%
  ggplot(aes(Grupo, Pct, fill = Grupo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct), width = 0.2, color = "gray30") +
  geom_text(aes(y = IC_Sup_Pct, label = round(Pct, 1)), vjust = -0.4, size = 3.3) +
  facet_wrap(~Indicador, scales = "free_y", nrow = 2, labeller = label_wrap_gen(30)) +
  scale_fill_manual(values = cores_local) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Atendimentos ambulatoriais por asma em crianças e adolescentes — Alagoas",
       subtitle = paste0("IC 95% robusto a cluster por paciente | ", periodo),
       caption = fonte_bi, x = NULL, y = "%") +
  tema + theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(size = 8))

p_piramide <- pacientes %>%
  filter(Sexo != "Ignorado") %>%
  count(Faixa_Etaria, Sexo) %>%
  mutate(Pct = n / sum(n) * 100, Pct_Plot = if_else(Sexo == "Masculino", -Pct, Pct)) %>%
  ggplot(aes(Faixa_Etaria, Pct_Plot, fill = Sexo)) +
  geom_col(width = 0.8) + coord_flip() +
  scale_y_continuous(labels = function(x) paste0(abs(round(x)), "%")) +
  scale_fill_manual(values = c("Masculino" = "#56B4E9", "Feminino" = "#CC79A7")) +
  labs(title = "Pacientes de 0–19 anos atendidos por asma segundo sexo e idade",
       subtitle = paste0("Unidade: paciente (CNS), idade no 1º atendimento | ", periodo),
       caption = fonte_bi, x = "Faixa etária (anos)", y = "% dos pacientes", fill = NULL) + tema

p_urg_servico <- urgencia_servico %>%
  filter(n >= 30) %>%
  ggplot(aes(reorder(Tipo_Servico, Pct_Urgencia), Pct_Urgencia, fill = Local)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct), position = position_dodge(0.8),
                width = 0.2, color = "gray30") +
  coord_flip() +
  scale_fill_manual(values = cores_local[c("Maceió", "Interior")]) +
  labs(title = "Caráter de urgência segundo o tipo de serviço",
       subtitle = "A urgência acompanha o tipo de estabelecimento (CNES), não só a gravidade | estratos com ≥ 30 atendimentos",
       caption = fonte_bi, x = NULL, y = "% dos atendimentos com caráter de urgência", fill = NULL) + tema

d_tend <- tendencia %>%
  select(Ano, any_of(c("Taxa_Pacientes_10mil", "Taxa_Pac_Padronizada_10mil", "Taxa_CEAF_10mil",
                       "Taxa_Intern_10mil"))) %>%
  pivot_longer(-Ano, names_to = "Serie", values_to = "Taxa") %>%
  mutate(Serie = recode(Serie, Taxa_Pacientes_10mil = "Pacientes atendidos (BI), bruta",
                        Taxa_Pac_Padronizada_10mil = "Pacientes atendidos (BI), padronizada por idade",
                        Taxa_CEAF_10mil = "Pacientes em tratamento no CEAF",
                        Taxa_Intern_10mil = "Internações por asma (SIH)"))
p_tendencia <- ggplot(d_tend, aes(Ano, Taxa, color = Serie)) +
  annotate("rect", xmin = min(anos_covid) - 0.5, xmax = max(anos_covid) + 0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "gray50") +
  annotate("text", x = mean(anos_covid), y = Inf, label = "COVID-19", vjust = 1.5, size = 3.3) +
  geom_vline(xintercept = min(anos_tend) - 0.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_x_continuous(breaks = anos) +
  scale_color_manual(values = c("Pacientes atendidos (BI), bruta" = "#0072B2",
                                "Pacientes atendidos (BI), padronizada por idade" = "#56B4E9",
                                "Pacientes em tratamento no CEAF" = "#009E73",
                                "Internações por asma (SIH)" = "#D55E00")) +
  labs(title = "Asma em crianças e adolescentes (0–19 anos) por 10 mil habitantes — Alagoas",
       subtitle = sprintf("Linha tracejada: início da série estável (%d) | população %s repetida em 2025",
                          min(anos_tend), pop_ano_disponivel),
       caption = "Fonte: SIASUS e SIH/DATASUS; população: estimativas DATASUS (brpop).",
       x = NULL, y = "Por 10 mil hab. 0–19 anos", color = NULL) +
  guides(color = guide_legend(nrow = 2)) + tema

p_urg_tend <- urgencia_ajustada %>%
  pivot_longer(c(Pct_Bruta, Pct_Ajustada, Pct_Urg_Sem_Maior_Estab), names_to = "Serie", values_to = "Pct") %>%
  mutate(Serie = recode(Serie, Pct_Bruta = "Bruta",
                        Pct_Ajustada = "Ajustada por estabelecimento",
                        Pct_Urg_Sem_Maior_Estab = "Sem o maior estabelecimento do ano")) %>%
  ggplot(aes(Ano, Pct, color = Serie)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_x_continuous(breaks = anos) +
  scale_color_manual(values = c("gray50", "#D55E00", "#0072B2")) +
  labs(title = "Atendimentos de urgência por asma (0–19 anos): bruta × ajustada",
       subtitle = "Ajuste por estabelecimento (padronização marginal) remove a troca de unidades notificantes",
       caption = fonte_bi, x = NULL, y = "% dos atendimentos", color = NULL) + tema

p_tipo_atend <- ggplot(tipo_atend_faixa, aes(Faixa_Etaria, Pct, fill = Tipo_Atend)) +
  geom_col(width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Tipo de atendimento por faixa etária",
       subtitle = "Procedimento SIGTAP + CBO | fisioterapia gera sessões repetidas",
       caption = fonte_bi, x = "Faixa etária (anos)", y = "% dos atendimentos", fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) + tema

p_volume <- volume_mensal %>%
  mutate(Data = idx_data(Mes),
         Status = case_when(Apos_Fim ~ "Após o fim dos dados completos",
                            Suspeito ~ "Mês suspeito (queda abrupta)", TRUE ~ "Normal")) %>%
  ggplot(aes(Data, APAC)) +
  geom_line(color = "gray50") +
  geom_point(aes(color = Status), size = 1.6) +
  scale_color_manual(values = c("Normal" = "gray30", "Mês suspeito (queda abrupta)" = "#D55E00",
                                "Após o fim dos dados completos" = "#56B4E9")) +
  labs(title = "Volume mensal de APAC de medicamentos para asma (todas as idades) — Alagoas",
       subtitle = "Base para definir o fim dos dados e os meses que não contam como interrupção",
       caption = "Fonte: SIASUS/DATASUS (AM).", x = NULL, y = "APAC no mês", color = NULL) + tema

p_dose <- dose_faixa %>%
  filter(Faixa_Etaria %in% niveis_faixa) %>%
  mutate(Intensidade = factor(Intensidade, levels = niveis_intensidade)) %>%
  group_by(Faixa_Etaria, Intensidade) %>% summarise(Pct = sum(Pct), .groups = "drop") %>%
  ggplot(aes(Faixa_Etaria, Pct, fill = Intensidade)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Baixa" = "#56B4E9", "Média" = "#009E73", "Alta" = "#E69F00",
                               "Imunobiológico" = "#D55E00", "< 6 anos (fora da faixa do PCDT)" = "#CC79A7",
                               "Dose não informada" = "gray75", "Outro" = "gray45")) +
  labs(title = "Intensidade do tratamento (dose diária estimada de budesonida, GINA) por faixa etária",
       subtitle = "Pacientes-mês no CEAF | dose = quantidade dispensada × µg/cápsula ÷ 30",
       caption = fonte_am, x = "Faixa etária (anos)", y = "% dos pacientes-mês", fill = NULL) +
  guides(fill = guide_legend(nrow = 3)) + tema

p_nutricional <- pacientes_am %>%
  filter(Estado_Nutricional != "Sem dado válido", Sexo != "Ignorado") %>%
  count(Sexo, Faixa_Etaria, Estado_Nutricional) %>%
  group_by(Sexo, Faixa_Etaria) %>% mutate(Pct = n / sum(n) * 100) %>% ungroup() %>%
  mutate(Estado_Nutricional = factor(Estado_Nutricional, c("Magreza", "Eutrofia", "Sobrepeso", "Obesidade"))) %>%
  ggplot(aes(Faixa_Etaria, Pct, fill = Estado_Nutricional)) +
  geom_col(width = 0.7) + facet_wrap(~Sexo) +
  scale_fill_manual(values = c("Magreza" = "#56B4E9", "Eutrofia" = "#009E73",
                               "Sobrepeso" = "#E69F00", "Obesidade" = "#D55E00")) +
  labs(title = "Estado nutricional (escore-z de IMC-para-idade, OMS) dos pacientes no CEAF",
       subtitle = "Mediana do escore-z do paciente | gestantes excluídas",
       caption = fonte_am, x = "Faixa etária (anos)", y = "% dos pacientes", fill = NULL) + tema

estratos_km <- episodios %>% count(Apresentacao) %>% filter(n >= 20) %>% pull(Apresentacao)
p_km <- km_curvas %>%
  bind_rows(tibble(Apresentacao = unique(km_curvas$Apresentacao), Meses = 0, Persistentes = 100)) %>%
  filter(Apresentacao %in% estratos_km) %>%
  ggplot(aes(Meses, Persistentes, color = Apresentacao)) +
  geom_step(linewidth = 1) +
  coord_cartesian(xlim = c(0, 36)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Persistência no tratamento (Kaplan-Meier) — 0–19 anos",
       subtitle = "Evento: ≥ 3 meses observáveis sem dispensação | censura: em uso, troca, limite de idade, óbito | apresentações com ≥ 20 episódios",
       caption = fonte_am, x = "Meses desde o início do episódio", y = "% ainda em tratamento", color = NULL) +
  guides(color = guide_legend(nrow = 2)) + tema

cores_desfecho <- setNames(c("#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#CC79A7",
                             "gray55", "gray80", "black"), niveis_desfecho)
p_desfecho <- desfecho_medicamento %>%
  filter(n > 0) %>%
  mutate(Apresentacao = paste0(Apresentacao, "\n(n = ", Episodios, ")")) %>%
  ggplot(aes(reorder(Apresentacao, Episodios), Pct, fill = Desfecho)) +
  geom_col(width = 0.7) + coord_flip() +
  scale_fill_manual(values = cores_desfecho) +
  labs(title = "Desfecho do episódio de tratamento por medicamento — 0–19 anos",
       subtitle = sprintf("Todas as APAC; fim dos dados completos: %s", format(idx_data(mes_fim_valido), "%m/%Y")),
       caption = fonte_am, x = NULL, y = "% dos episódios", fill = NULL) +
  guides(fill = guide_legend(nrow = 4)) + tema

p_vinculo <- vinculo_modelo %>%
  mutate(Modelo = factor(Modelo, levels = rev(Modelo))) %>%
  ggplot(aes(OR, Modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = IC_Inf, xmax = IC_Sup), height = 0.2) +
  geom_point(size = 3, color = "#0072B2") +
  scale_x_log10() +
  labs(title = "Tratamento no CEAF e atendimento de urgência (0–19 anos)",
       subtitle = "Razão de chances de ≥ 1 atendimento de urgência: recebe × não recebe medicamento",
       caption = "Fonte: SIASUS/DATASUS (BI × AM pelo CNS). Regressão logística, EP robusto.",
       x = "OR (escala log)", y = NULL) + tema

p_antes_depois <- bind_rows(resumo_janelas, resumo_did %>% filter(Grupo == "Controle pareado")) %>%
  ggplot(aes(Janela, Urg_por_Paciente, color = Grupo, group = Grupo)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
  scale_color_manual(values = c("Caso (inicia CEAF)" = "#0072B2", "Controle pareado" = "gray50")) +
  labs(title = "Atendimentos de urgência por asma antes e depois do início do tratamento",
       subtitle = sprintf("Casos novos no CEAF ligados ao BI (n = %d); controles pareados por sexo, faixa e mês (n = %d)",
                          nrow(casos), nrow(controles)),
       caption = "Fonte: SIASUS/DATASUS (BI × AM). Exploratório: n pequeno, sem efeito causal.",
       x = "Janela (meses em relação ao início)", y = "Urgências por paciente", color = NULL) + tema

p_regiao <- if (exists("taxas_regiao")) taxas_regiao %>%
  pivot_longer(c(Taxa_Pac_BI_10mil_ano, Taxa_Pac_AM_10mil_ano, Taxa_Intern_10mil_ano),
               names_to = "Serie", values_to = "Taxa") %>%
  mutate(Serie = recode(Serie, Taxa_Pac_BI_10mil_ano = "Atendidos (BI)", Taxa_Pac_AM_10mil_ano = "Em tratamento (CEAF)",
                        Taxa_Intern_10mil_ano = "Internações (SIH)")) %>%
  ggplot(aes(reorder(Regiao_Saude, Taxa), Taxa, fill = Serie)) +
  geom_col(position = position_dodge(0.8), width = 0.7) + coord_flip() +
  scale_fill_manual(values = c("Atendidos (BI)" = "#0072B2", "Em tratamento (CEAF)" = "#009E73",
                               "Internações (SIH)" = "#D55E00")) +
  labs(title = "Pacientes de 0–19 anos com asma por região de saúde de residência",
       subtitle = paste0("Pacientes por 10 mil hab.-ano | ", periodo),
       caption = "Fonte: SIASUS/DATASUS; regiões e população: brpop.",
       x = "Região de saúde (código)", y = "Por 10 mil habitantes-ano", fill = NULL) + tema else NULL

p_sazonal <- ggplot(sazonalidade, aes(Mes_Cal, Media)) +
  geom_col(fill = "#D55E00", width = 0.7) +
  geom_errorbar(aes(ymin = pmax(Media - DP, 0), ymax = Media + DP), width = 0.2, color = "gray30") +
  scale_x_continuous(breaks = 1:12, labels = c("jan", "fev", "mar", "abr", "mai", "jun",
                                               "jul", "ago", "set", "out", "nov", "dez")) +
  labs(title = "Sazonalidade das internações por asma (0–19 anos) — Alagoas",
       subtitle = "Média mensal de internações (± DP), excluindo 2020–2021",
       caption = "Fonte: SIH/DATASUS (RD). CID principal J45/J46.",
       x = NULL, y = "Internações por mês (média)") + tema

p_eco_int <- if (exists("d_eco_int")) d_eco_int %>%
  mutate(Local = if_else(Municipio == cod_maceio, "Maceió", "Interior")) %>%
  ggplot(aes(Taxa_Pac_AM_10mil_ano, Taxa_Intern_10mil_ano)) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "gray40", linewidth = 0.8) +
  geom_point(aes(size = Pop_Media, color = Local), alpha = 0.7) +
  scale_color_manual(values = c("Maceió" = "#D55E00", "Interior" = "#0072B2")) +
  scale_size_continuous(range = c(1.5, 9), guide = "none") +
  labs(title = "Tratamento no CEAF × internação por asma, por município (0–19 anos)",
       subtitle = "Cada ponto é um município (tamanho = população 0–19) | taxas por 10 mil hab.-ano",
       caption = "Fonte: SIASUS (AM) e SIH/DATASUS. Associação ecológica: não vale para indivíduos.",
       x = "Pacientes no CEAF por 10 mil hab.-ano", y = "Internações por 10 mil hab.-ano", color = NULL) +
  tema else NULL

# ==============================================================================
# EXPORTAÇÃO
# ==============================================================================
figuras <- list(
  "Asma_Ped_01_indicadores"            = list(p_indicadores, 13, 7),
  "Asma_Ped_02_piramide_pacientes"     = list(p_piramide, 10, 6),
  "Asma_Ped_03_urgencia_por_servico"   = list(p_urg_servico, 11, 6),
  "Asma_Ped_04_taxas_tendencia"        = list(p_tendencia, 11, 6.5),
  "Asma_Ped_05_urgencia_bruta_ajustada"= list(p_urg_tend, 11, 6),
  "Asma_Ped_06_tipo_atendimento"       = list(p_tipo_atend, 10, 6.5),
  "Asma_Ped_07_volume_mensal_AM"       = list(p_volume, 11, 5.5),
  "Asma_Ped_08_intensidade_tratamento" = list(p_dose, 11, 7),
  "Asma_Ped_09_estado_nutricional"     = list(p_nutricional, 11, 6),
  "Asma_Ped_10_persistencia_KM"        = list(p_km, 11, 6.5),
  "Asma_Ped_11_desfecho_medicamento"   = list(p_desfecho, 12, 7.5),
  "Asma_Ped_12_vinculo_urgencia_OR"    = list(p_vinculo, 11, 5),
  "Asma_Ped_13_antes_depois"           = list(p_antes_depois, 11, 6.5),
  "Asma_Ped_14_regioes_saude"          = list(p_regiao, 11, 6.5),
  "Asma_Ped_15_internacoes_sazonalidade" = list(p_sazonal, 10, 5.5),
  "Asma_Ped_16_CEAF_x_internacao_municipios" = list(p_eco_int, 10, 6.5))
figuras <- Filter(function(f) !is.null(f[[1]]), figuras)
arquivos_fig <- mapply(function(nome, f) salvar_grafico(f[[1]], nome, w = f[[2]], h = f[[3]]),
                       names(figuras), figuras)

resultados <- list(
  fluxograma_pa = fluxograma_pa, pa_ped_instrumento = pa_ped_instrumento,
  fluxo_bi = fluxo_bi, consistencia_pa_bi = consistencia_pa_bi, completude = completude,
  indicadores = indicadores, urgencia_servico = urgencia_servico,
  concentracao_estab = concentracao_estab, distribuicoes = distribuicoes,
  tipo_atend_faixa = tipo_atend_faixa, resumo_pacientes = resumo_pacientes,
  dist_pacientes = dist_pacientes, testes_local = testes_local,
  perfil_raca_missing = perfil_raca_missing, sensibilidade = sensibilidade,
  tendencia = tendencia, apc = apc, urgencia_ajustada = urgencia_ajustada,
  criterio_painel = criterio_painel,
  taxa_faixa = if (exists("taxa_faixa")) taxa_faixa else NULL,
  volume_mensal = volume_mensal, fluxo_am = fluxo_am, completude_am = completude_am,
  resumo_am = resumo_am, pacientes_am_ano = pacientes_am_ano, dist_am = dist_am,
  dose_faixa = dose_faixa, dose_resumo = dose_resumo,
  tratamento_perfil = tratamento_perfil, testes_tratamento = testes_tratamento,
  motivo_saida_am = motivo_saida_am, desfecho_medicamento = desfecho_medicamento,
  desfecho_sensibilidade = desfecho_sensibilidade, persistencia_km = persistencia_km,
  vinculo_bi_am = vinculo_bi_am, vinculo_perfil = vinculo_perfil,
  vinculo_modelo = vinculo_modelo, vinculo_faixa = vinculo_faixa,
  resumo_janelas = resumo_janelas, rr_janelas = rr_janelas,
  resumo_did = resumo_did, did_modelo = did_modelo,
  municipal = asma_mun,
  razao_taxas_local = if (exists("razao_taxas_local")) razao_taxas_local else NULL,
  taxas_regiao = if (exists("taxas_regiao")) taxas_regiao else NULL,
  ecologico = eco, arquivos_falha = arquivos_falha,
  fluxo_sih = fluxo_sih, resumo_intern = resumo_intern, perfil_intern_faixa = perfil_intern_faixa,
  desfecho_hospitalar = desfecho_hospitalar, dist_intern = dist_intern, intern_ano = intern_ano,
  apc_intern = if (exists("apc_intern")) apc_intern else NULL, sazonalidade = sazonalidade,
  reinternacoes = reinternacoes, reintern_faixa = reintern_faixa,
  sih_asma_secundaria = sih_asma_secundaria, concentracao_hosp = concentracao_hosp,
  eco_internacao = if (exists("eco_internacao")) eco_internacao else NULL)
saveRDS(resultados, file.path(output_dir, "Asma_Ped_resultados.rds"))
saveRDS(atend, file.path(output_dir, "Base_Asma_Ped_atendimentos.rds"))
saveRDS(pacientes, file.path(output_dir, "Base_Asma_Ped_pacientes_BI.rds"))
saveRDS(pacientes_am, file.path(output_dir, "Base_Asma_Ped_pacientes_AM.rds"))
saveRDS(episodios, file.path(output_dir, "Base_Asma_Ped_episodios.rds"))
saveRDS(intern, file.path(output_dir, "Base_Asma_Ped_internacoes.rds"))

# ==============================================================================
# RELATÓRIO DOCX
# ==============================================================================
mes_fim_txt <- format(idx_data(mes_fim_valido), "%m/%Y")
susp_txt <- if (length(meses_suspeitos)) paste(format(idx_data(meses_suspeitos), "%m/%Y"), collapse = ", ") else "nenhum"
add_tab <- function(doc, df, titulo = NULL, nota = NULL) {
  if (!is.null(titulo)) doc <- body_add_par(doc, titulo, style = "heading 3")
  if (!is.null(nota)) doc <- body_add_par(doc, nota, style = "Normal")
  if (is.null(df) || !nrow(df)) return(body_add_par(doc, "(sem dados)", style = "Normal"))
  doc %>% body_add_flextable(ft(df)) %>% body_add_par("")
}
add_fig <- function(doc, nome) {
  f <- file.path(output_dir, paste0(nome, ".png"))
  if (!file.exists(f)) return(doc)
  d <- figuras[[nome]]
  doc %>% body_add_img(f, width = 6.5, height = 6.5 * d[[3]] / d[[2]]) %>% body_add_par("")
}

doc <- read_docx() %>%
  body_add_par("Asma em crianças e adolescentes (0–19 anos) no SUS — Alagoas", style = "heading 1") %>%
  body_add_par(paste0(
    "Atendimentos ambulatoriais (SIASUS, BPA-I), tratamento no CEAF (APAC de medicamentos) e ",
    "internações (SIH), ", periodo, ". Versão revisada após auditoria metodológica (Auditoria_Asma_SIASUS.md). ",
    "Idade medida na data de cada registro. A atenção primária (SISAB) não está incluída."),
    style = "Normal") %>%

  body_add_par("1. Seleção e qualidade", style = "heading 2") %>%
  add_tab(fluxograma_pa, "Fluxograma — PA") %>%
  add_tab(pa_ped_instrumento, "O que resta no PA pediátrico",
          "As APAC de medicamento saem por qualquer linha do grupo 06. Os atendimentos são analisados pelo BI.") %>%
  add_tab(fluxo_bi, "Fluxograma — BI (atendimentos)") %>%
  add_tab(consistencia_pa_bi, "Consistência PA (BPA-I) × BI, todas as idades") %>%
  add_tab(completude, "Completude (atendimentos 0–19 anos)") %>%
  add_tab(concentracao_estab, "Concentração por estabelecimento") %>%

  body_add_par("2. Perfil dos atendimentos", style = "heading 2") %>%
  add_tab(indicadores %>% select(-Estratificacao), "Indicadores com IC 95% robusto a cluster (paciente)",
          "Descritivo. O IC considera atendimentos repetidos do mesmo paciente (survey, cluster = CNS).") %>%
  add_fig("Asma_Ped_01_indicadores") %>%
  add_tab(urgencia_servico, "Caráter de urgência segundo tipo de serviço",
          "A urgência depende do tipo de estabelecimento: comparações de urgência só dentro do mesmo tipo de serviço.") %>%
  add_fig("Asma_Ped_03_urgencia_por_servico") %>%
  add_tab(distribuicoes, "Distribuições (atendimentos)") %>%
  add_fig("Asma_Ped_06_tipo_atendimento") %>%

  body_add_par("3. Pacientes (unidade de inferência)", style = "heading 2") %>%
  add_tab(resumo_pacientes, "Resumo") %>%
  add_fig("Asma_Ped_02_piramide_pacientes") %>%
  add_tab(dist_pacientes, "Distribuições por local de residência") %>%
  add_tab(testes_local, "Maceió × interior (qui-quadrado no nível do paciente, Holm)",
          "Magnitude do V de Cramér com limiares de Cohen ajustados pelos graus de liberdade.") %>%
  add_tab(perfil_raca_missing, "Pacientes com e sem raça/cor informada") %>%
  add_tab(sensibilidade, "Análises de sensibilidade") %>%

  body_add_par("4. Tendência e taxas", style = "heading 2") %>%
  add_tab(tendencia, "Série anual",
          sprintf("Taxas por 10 mil hab. de 0–19 anos; padronização direta (pop. AL 2022). População de %s repetida nos anos seguintes. %s.",
                  pop_ano_disponivel, criterio_painel)) %>%
  add_fig("Asma_Ped_04_taxas_tendencia") %>%
  add_tab(apc, "Variação percentual anual (Prais-Winsten, log10 da taxa)",
          sprintf("Período %d–%d (cobertura estável); sensibilidade sem 2020–2021.", min(anos_tend), max(anos_tend))) %>%
  add_tab(urgencia_ajustada, "Urgência: bruta, ajustada por estabelecimento, painel estável e sem o maior estabelecimento") %>%
  add_fig("Asma_Ped_05_urgencia_bruta_ajustada") %>%

  body_add_par("5. Tratamento no CEAF", style = "heading 2") %>%
  add_tab(fluxo_am, "Fluxograma — AM",
          sprintf(paste0("Todas as APAC entram (aprovação é evento de faturamento). Fim dos dados completos: %s. ",
                         "Meses suspeitos (queda abrupta de volume, não contam como interrupção): %s."),
                  mes_fim_txt, susp_txt)) %>%
  add_fig("Asma_Ped_07_volume_mensal_AM") %>%
  add_tab(completude_am, "Completude (APAC 0–19 anos)") %>%
  add_tab(resumo_am, "Resumo dos pacientes",
          "Paciente novo = 1ª APAC após ≥ 12 meses sem APAC (a APAC 'inicial' é reemitida a cada 3 meses).") %>%
  add_tab(pacientes_am_ano, "Pacientes por ano: todas as APAC × só aprovadas") %>%
  add_tab(dist_am, "Distribuições") %>%
  add_tab(dose_resumo, "Dose diária estimada de budesonida (µg/dia)",
          paste0("Dose = quantidade apresentada no mês × µg por cápsula ÷ 30. Quantidade zero = dose não informada. ",
                 "A quantidade registrada é quase sempre 60 cápsulas/mês em todas as idades, o que sugere ",
                 "registro padronizado: a dose estimada reflete mais a apresentação dispensada do que a ",
                 "prescrição individual. Interpretar a intensidade com cautela.")) %>%
  add_fig("Asma_Ped_08_intensidade_tratamento") %>%
  add_tab(testes_tratamento, "Intensidade máxima × perfil (Holm)") %>%
  add_tab(tratamento_perfil %>% select(-n, -N_Categoria) %>%
            pivot_wider(names_from = Intensidade_Max, values_from = Pct, values_fill = 0),
          "Intensidade máxima por categoria (%)") %>%
  add_fig("Asma_Ped_09_estado_nutricional") %>%

  body_add_par("6. Desfechos do tratamento", style = "heading 2") %>%
  body_add_par(paste0(
    "O SIASUS não registra alta nem reinternação (estão no SIH, sem CNS nos arquivos públicos). ",
    "Episódio = paciente × medicamento; lacuna ≥ 3 meses observáveis abre novo episódio. ",
    sprintf("Entrada aos 0–%d anos; seguimento até %d anos (margem de %d ano para quem faz %d durante o tratamento). ",
            idade_ped, idade_ped + margem_idade, margem_idade, idade_ped + 1),
    "Desfecho hierárquico: óbito > atingiu o limite de idade e seguiu no CEAF > troca > em uso no fim ",
    "> saída registrada > interrompeu. 'Troca de cápsula' é troca de apresentação, não necessariamente de etapa."),
    style = "Normal") %>%
  add_tab(motivo_saida_am, "Motivo de saída registrado nas APAC") %>%
  add_tab(desfecho_medicamento %>% select(-Base, -n) %>%
            pivot_wider(names_from = Desfecho, values_from = Pct, values_fill = 0),
          "Desfecho por medicamento (% dos episódios)") %>%
  add_fig("Asma_Ped_11_desfecho_medicamento") %>%
  add_tab(desfecho_sensibilidade, "Sensibilidade: todas as APAC × só aprovadas × sem margem de idade") %>%
  add_tab(persistencia_km, "Persistência (Kaplan-Meier)") %>%
  add_fig("Asma_Ped_10_persistencia_KM") %>%

  body_add_par("7. Tratamento × atendimentos (ligação pelo CNS)", style = "heading 2") %>%
  add_tab(vinculo_bi_am, "Pacientes nas duas bases") %>%
  add_tab(vinculo_perfil, "Perfil por grupo") %>%
  add_tab(vinculo_modelo, "Regressão logística (EP robusto)",
          "Associação transversal: não indica efeito do tratamento.") %>%
  add_fig("Asma_Ped_12_vinculo_urgencia_OR") %>%
  add_tab(vinculo_faixa, "Cobertura do CEAF entre atendidos, por faixa") %>%
  add_tab(resumo_janelas, "Casos: urgência por janela de 12 meses",
          "Casos novos no CEAF com 24 meses observáveis antes e depois do início.") %>%
  add_tab(rr_janelas, "Razão de taxas por janela (Poisson condicional ao paciente; ref. −12 a −1)") %>%
  add_tab(resumo_did, "Casos × controles pareados (pré e pós)") %>%
  add_tab(did_modelo, "Diferença-em-diferenças (Poisson com efeito fixo do paciente, EP por cluster)",
          paste0("Exploratório. Os controles foram escolhidos por terem atendimento nos 12 meses antes do ",
                 "índice, que costuma ser urgência: a queda deles é regressão à média. RR > 1 indica que a ",
                 "urgência caiu MENOS nos casos que nos controles; não indica efeito do tratamento.")) %>%
  add_fig("Asma_Ped_13_antes_depois") %>%

  body_add_par("8. Internações por asma (SIH)", style = "heading 2") %>%
  body_add_par(paste0(
    "AIH com CID principal J45/J46 (asma é condição sensível à atenção primária, Portaria SAS 221/2008), ",
    "ano da internação, residentes em AL internados em AL. AIH de continuação e reapresentações excluídas. ",
    "Os arquivos públicos não têm CNS: reinternação estimada por chave determinística (data de nascimento, ",
    "sexo, CEP e município); a chave sem CEP dá o limite superior. Não há ligação individual com BI/CEAF."),
    style = "Normal") %>%
  add_tab(fluxo_sih, "Fluxograma — SIH") %>%
  add_tab(resumo_intern, "Resumo das internações") %>%
  add_tab(perfil_intern_faixa, "Permanência, UTI e óbitos por faixa etária") %>%
  add_tab(desfecho_hospitalar, "Desfecho da internação") %>%
  add_tab(dist_intern, "Distribuições") %>%
  add_tab(intern_ano, "Série anual") %>%
  add_tab(concentracao_hosp, "Concentração por hospital",
          paste0("Em 2015–2017 um único hospital concentrou 38–55% das internações, com UTI quase nula; ",
                 "a saída dele da série em 2018 explica boa parte da queda das internações e da alta da ",
                 "proporção de UTI. Interpretar a tendência com cautela.")) %>%
  add_tab(if (exists("apc_intern")) apc_intern else NULL, "Variação percentual anual (Prais-Winsten)") %>%
  add_tab(sazonalidade, "Sazonalidade (média por mês, sem 2020–2021)") %>%
  add_fig("Asma_Ped_15_internacoes_sazonalidade") %>%
  add_tab(reinternacoes, "Reinternação (chave determinística)",
          "Episódio = AIH unidas quando a nova internação ocorre até 1 dia após a alta (transferência). Índice: alta viva com 30 dias (ou 12 meses) observáveis.") %>%
  add_tab(reintern_faixa, "Reinternação por faixa etária (chave principal)") %>%
  add_tab(sih_asma_secundaria, "AIH com asma só como diagnóstico secundário (capítulo do CID principal)") %>%
  add_tab(if (exists("eco_internacao")) eco_internacao else NULL,
          "Ecológico: tratamento no CEAF e atendimento ambulatorial × internação (municípios)",
          "Quase-Poisson com offset da população 0–19. Associação entre municípios, não entre pacientes.") %>%
  add_fig("Asma_Ped_16_CEAF_x_internacao_municipios") %>%

  body_add_par("9. Municípios e regiões de saúde", style = "heading 2") %>%
  add_tab(if (exists("razao_taxas_local")) razao_taxas_local else NULL, "Maceió × interior (razão de taxas, quase-Poisson)") %>%
  add_tab(if (exists("taxas_regiao")) taxas_regiao else NULL, "Taxas por região de saúde") %>%
  add_fig("Asma_Ped_14_regioes_saude") %>%
  add_tab(eco, "Análise ecológica: fora do município × urgência",
          sprintf("Municípios com ≥ %d atendimentos. Falácia ecológica: associação entre municípios, não entre pacientes.",
                  min_registros_mun)) %>%
  add_tab(asma_mun %>% arrange(desc(N_Atendimentos)), "Municípios") %>%

  body_add_par("10. Reprodutibilidade", style = "heading 2") %>%
  add_tab(arquivos_falha, "Arquivos que falharam na leitura") %>%
  body_add_par(paste(capture.output(print(sessionInfo())), collapse = "\n"), style = "Normal")

print(doc, target = file.path(output_dir, "Asma_Ped_Resultados.docx"))
message("Concluído: ", output_dir)
