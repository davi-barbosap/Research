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
semente_principal  <- 2026
n_sementes_did     <- 50          # sensibilidade ao sorteio dos controles
n_bootstrap        <- 200         # IC da urgência ajustada por estabelecimento

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
  library(lmtest); library(scales); library(ggrepel)
})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir_figuras    <- file.path(output_dir, "figuras")
dir_referencia <- file.path(output_dir, "referencias")
dir.create(dir_figuras, showWarnings = FALSE)
dir.create(dir_referencia, showWarnings = FALSE)

# --- PADRÃO VISUAL ------------------------------------------------------------
# Números no padrão brasileiro (vírgula decimal, ponto de milhar)
num_br <- function(x, dig = 1) formatC(x, format = "f", digits = dig, big.mark = ".", decimal.mark = ",")
eixo_br <- label_number(big.mark = ".", decimal.mark = ",")
eixo_pct <- label_number(suffix = "%", big.mark = ".", decimal.mark = ",")
pal <- list(azul = "#0072B2", laranja = "#D55E00", verde = "#009E73", amarelo = "#E69F00",
            celeste = "#56B4E9", rosa = "#CC79A7", cinza = "gray45", cinza_claro = "gray75")
cores_local <- c("Alagoas (0–19 anos)" = "gray40", "Maceió" = pal$laranja, "Interior" = pal$azul)

tema_asma <- theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray25", margin = margin(b = 8)),
        plot.caption = element_text(hjust = 0, size = 9, color = "gray35", margin = margin(t = 8)),
        legend.position = "bottom", legend.title = element_text(size = 10),
        panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
        plot.margin = margin(12, 16, 10, 12))
theme_set(tema_asma)

salvar_grafico <- function(plot, nome_base, w = 10, h = 6) {
  ragg::agg_tiff(file.path(dir_figuras, paste0(nome_base, ".tiff")),
                 width = w, height = h, units = "in", res = 300, compression = "lzw")
  print(plot); dev.off()
  ragg::agg_png(file.path(dir_figuras, paste0(nome_base, ".png")),
                width = w, height = h, units = "in", res = 300)
  print(plot); dev.off()
  invisible(file.path(dir_figuras, paste0(nome_base, ".png")))
}

rotulo_var <- c(Sexo = "Sexo", Faixa_Etaria = "Faixa etária", Raca_Cor = "Raça/cor",
                Servico_1o = "Tipo de serviço do 1º atendimento", Freq_Contatos = "Nº de contatos",
                Teve_Urgencia = "Teve atendimento de urgência", Teve_Serv_Urg = "Atendido em pronto-atendimento",
                Teve_Fisio = "Fez fisioterapia", Teve_J46 = "Teve mal asmático (J46)", Local = "Maceió × interior",
                Estado_Nutricional = "Estado nutricional")

# --- TABELAS: rótulos legíveis e formato brasileiro ----------------------------
set_flextable_defaults(decimal.mark = ",", big.mark = ".", digits = 2, na_str = "—",
                       font.family = "Arial", font.size = 9, padding = 3)
rotulo_coluna <- function(x) {
  x <- gsub("_", " ", x)
  x <- sub("^Pct (.*)$", "% \\1", x)
  x <- gsub("\\bIC Inf Pct\\b|\\bIC Inf\\b", "IC 95% inf.", x)
  x <- gsub("\\bIC Sup Pct\\b|\\bIC Sup\\b", "IC 95% sup.", x)
  x <- gsub("\\bP Valor\\b", "p", x)
  x <- gsub("\\bP Holm Global\\b", "p (Holm global)", x)
  x <- gsub("\\bN\\b", "Nº", x)
  x <- gsub("10mil", "10 mil", x)
  acentos <- c(Excluidas = "Excluídas", Invalido = "inválido", Invalida = "inválida", Raca = "Raça",
               Carater = "Caráter", Servico = "Serviço", Estab = "estabelecimento", Media = "Média",
               Periodo = "Período", Regiao = "Região", Municipio = "Município", Municipios = "Municípios",
               Internacoes = "Internações", Intern = "internação", Obitos = "Óbitos", Obito = "Óbito",
               Permanencia = "Permanência", Apresentacao = "Apresentação", Variavel = "Variável",
               Analise = "Análise", Indice = "Índice", Mes = "Mês", Info = "informação", Urg = "urgência",
               Atend = "atendimentos", Pac = "pacientes", Pop = "População", Diferenca = "Diferença",
               Estatistica = "Estatística", Parametro = "Parâmetro", Versao = "Versão", Episodios = "Episódios",
               Criancas = "Crianças", Reintern = "Reinternação", Letalidade = "Letalidade", Ajustada = "Ajustada",
               Faixa = "Faixa", Etaria = "etária", Nutricional = "nutricional", Concentracao = "Concentração",
               Notificacao = "Notificação", Magnitude = "Magnitude", Esperado = "Esperado", menor = "<",
               Significativo = "Significativo", Familia = "Família", Disp = "dispensados", Seguimento = "seguimento")
  acentos <- c(acentos, Urgencia = "urgência", Ambulatorio = "ambulatório", Pediatrico = "pediátrico",
               Proporcao = "proporção", Padronizada = "padronizada", Unicos = "únicos", Medio = "médio")
  for (k in names(acentos)) x <- gsub(paste0("\\b", k, "\\b"), acentos[[k]], x)
  x <- sub(" Pct$", " (%)", x)
  x <- gsub("\\bV Cramer\\b", "V de Cramér", x)
  x <- gsub("\\bQui2\\b", "Qui²", x)
  x <- gsub("\\bUg\\b", "µg", x)
  x
}
formatar_p <- function(p) ifelse(is.na(p), "—", ifelse(p < 0.001, "< 0,001", num_br(p, 3)))
ft <- function(df, larg_max = 9) {
  df <- as.data.frame(df)
  for (col in intersect(c("P_Valor", "P_Holm_Global", "p"), names(df))) df[[col]] <- formatar_p(as.numeric(df[[col]]))
  for (col in intersect(c("Ano", "Ano_Primeiro", "Meses", "Regiao_Saude"), names(df))) df[[col]] <- as.character(df[[col]])
  t <- flextable(df) %>%
    set_header_labels(values = setNames(as.list(rotulo_coluna(names(df))), names(df))) %>%
    colformat_double(big.mark = ".", decimal.mark = ",", digits = 2) %>%
    colformat_int(big.mark = ".") %>%
    theme_booktabs() %>% bold(part = "header") %>%
    fontsize(size = 8.5, part = "all") %>% align(align = "center", part = "header") %>%
    autofit()
  # Não deixa a tabela passar da largura útil da página
  if (sum(dim_pretty(t)$widths) > larg_max) t <- fit_to_width(t, max_width = larg_max)
  t
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
