# =============================================================================
# Análise: Perfil Lipídico em Pacientes com IAM — Alagoas
# Arquivo: tabela.csv (Google Forms → Excel BR → CSV com separador ";")
# VERSÃO CORRIGIDA — corrige o bug de parsing da etapa 1 do script original
# =============================================================================

library(tidyverse)
library(rstatix)
library(gtsummary)
library(janitor)

# -----------------------------------------------------------------------------
# 1. LEITURA E PARSING DO CSV (CORRIGIDO)
# -----------------------------------------------------------------------------
# PROBLEMA DA VERSÃO ANTERIOR:
#   gsub('""', '\x01', linha) seguido de read.csv(text = linha) quebrava o
#   alinhamento das colunas sempre que um campo de texto livre (ex.:
#   "tempo de internamento") continha vírgulas literais — o que ocorre em
#   ~92% das linhas. Isso deslocava idade, tipo de IAM, fatores de risco e
#   óbito para colunas erradas (ou cortava esses campos do final do vetor
#   quando length(x) <- 21 era aplicado).
#
# CORREÇÃO:
#   Em vez de depender do read.csv para resolver a quebra de campos, a linha
#   é dividida primeiro por ";" (delimitador externo do Excel BR) e, dentro
#   de cada segmento, os subcampos entre aspas duplicadas (""campo"") são
#   separados manualmente pelo padrão literal ',",' — que é o que o Google
#   Forms gera de fato como delimitador de campo nesse export. Os dois
#   campos de seleção múltipla (Critério Diagnóstico e Fatores de Risco) têm
#   tamanho variável e são identificados por âncoras (palavra "SUPRA" para
#   localizar Tipo de IAM, e contagem reversa de 4 campos finais fixos para
#   separar Fatores de Risco dos campos de desfecho/observações).

arquivo <- "/Users/davibarbosa/Downloads/tabela.csv"

linhas_raw <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
linhas_dados <- linhas_raw[grepl('^"20', linhas_raw)]

# Quebra uma linha em tokens individuais, tratando corretamente vírgulas
# literais dentro de campos de texto livre.
flatten_linha <- function(linha) {
  linha <- sub(';+$', '', linha)
  segs  <- strsplit(linha, ';', fixed = TRUE)[[1]]

  tokens <- character(0)
  for (seg in segs) {
    seg2 <- gsub('""', '"', seg, fixed = TRUE)
    if (startsWith(seg2, '"') && endsWith(seg2, '"') && nchar(seg2) >= 2) {
      seg2 <- substr(seg2, 2, nchar(seg2) - 1)
    } else if (startsWith(seg2, '"')) {
      seg2 <- substr(seg2, 2, nchar(seg2))
    } else if (endsWith(seg2, '"')) {
      seg2 <- substr(seg2, 1, nchar(seg2) - 1)
    }
    parts <- if (nchar(seg2) == 0) "" else strsplit(seg2, '","', fixed = TRUE)[[1]]
    tokens <- c(tokens, parts)
  }
  tokens
}

# Converte os tokens de uma linha nas 21 colunas nomeadas, usando âncoras
# para localizar Tipo de IAM (contém "SUPRA") e os 4 campos finais fixos.
parse_record <- function(toks) {

  t0 <- toks[1]
  pos <- regexpr(',', t0, fixed = TRUE)
  if (pos > 0) {
    timestamp <- substr(t0, 1, pos - 1)
    nome      <- substr(t0, pos + 1, nchar(t0))
  } else {
    timestamp <- t0
    nome      <- ""
  }

  data_nasc          <- toks[2]
  data_coleta        <- toks[3]
  data_admissao      <- toks[4]
  setor              <- toks[5]
  internamento_texto <- toks[6]
  idade_texto        <- toks[7]
  iam_confirmado     <- toks[8]

  tipo_idx <- NA_integer_
  if (length(toks) >= 9) {
    for (j in 9:length(toks)) {
      if (grepl('SUPRA', toupper(toks[j]))) { tipo_idx <- j; break }
    }
  }

  if (!is.na(tipo_idx)) {
    criterio <- if (tipo_idx > 9) paste(toks[9:(tipo_idx - 1)], collapse = "; ") else ""
    tipo_iam_texto <- toks[tipo_idx]

    lip <- toks[(tipo_idx + 1):min(tipo_idx + 4, length(toks))]
    length(lip) <- 4

    classificacao <- if (length(toks) >= tipo_idx + 5) toks[tipo_idx + 5] else NA_character_
    remaining <- if (length(toks) >= tipo_idx + 6) toks[(tipo_idx + 6):length(toks)] else character(0)
  } else {
    criterio       <- ""
    tipo_iam_texto <- ""
    lip            <- rep(NA_character_, 4)
    classificacao  <- NA_character_
    remaining      <- character(0)
  }

  n_fatores <- max(0, length(remaining) - 4)
  fatores_risco <- if (n_fatores > 0) paste(remaining[1:n_fatores], collapse = "; ") else ""

  if (length(remaining) > n_fatores) {
    trail <- remaining[(n_fatores + 1):length(remaining)]
  } else {
    trail <- character(0)
  }
  length(trail) <- 4

  tibble(
    timestamp              = timestamp,
    nome                   = trimws(nome),
    data_nasc              = trimws(data_nasc),
    data_coleta            = trimws(data_coleta),
    data_admissao          = trimws(data_admissao),
    setor                  = trimws(setor),
    internamento_texto     = trimws(internamento_texto),
    idade_texto            = trimws(idade_texto),
    iam_confirmado         = trimws(iam_confirmado),
    criterio_diagnostico   = criterio,
    tipo_iam_texto         = trimws(tipo_iam_texto),
    ct_texto               = trimws(lip[1]),
    ldl_texto              = trimws(lip[2]),
    hdl_texto              = trimws(lip[3]),
    tgl_texto              = trimws(lip[4]),
    classificacao_lipidico = trimws(classificacao),
    fatores_risco          = fatores_risco,
    uso_hipolipemiante     = trimws(trail[1]),
    medicacoes_uso         = trimws(trail[2]),
    gravidade_complicacoes = trimws(trail[3]),
    observacoes            = trimws(trail[4])
  )
}

dados_raw <- map_dfr(linhas_dados, ~ parse_record(flatten_linha(.x)))

cat(sprintf("\nLinhas de dados lidas: %d | Linhas parseadas: %d\n",
            length(linhas_dados), nrow(dados_raw)))

# -----------------------------------------------------------------------------
# 2. LIMPEZA E VARIÁVEIS DERIVADAS
# -----------------------------------------------------------------------------

remove_acentos <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

valores_ausentes <- c(
  "PENDENTE", "Pendente", "pendente", "PEDENTE", "PENDENTE ",
  "NÃO SE APLICA", "NAO SE APLICA",
  "NAO INFORMADO", "NÃO INFORMADO", "Não informado",
  "PENDENTE TODOS OS EXAMES LIPÍDICOS", ""
)

limpar_numerico <- function(x) {
  x[x %in% valores_ausentes] <- NA
  x <- gsub("\\s*m[gh]/dL.*", "", x, ignore.case = TRUE)
  x <- gsub(",", ".", x)
  x <- trimws(x)
  suppressWarnings(as.numeric(x))
}

padronizar_simnao <- function(x) {
  x <- trimws(toupper(x))
  case_when(
    x %in% c("SIM", "S")        ~ "Sim",
    x %in% c("NÃO", "NAO", "N") ~ "Não",
    TRUE                         ~ NA_character_
  )
}

dados <- dados_raw |>
  mutate(

    # Idade
    idade = suppressWarnings(as.numeric(gsub("\\s*ANOS?.*", "", trimws(toupper(idade_texto))))),

    # Datas (corrige anos 0025 → 2025, 0026 → 2026)
    data_admissao = as.Date(gsub("^00([0-9]{2})", "20\\1", data_admissao)),
    data_nasc     = as.Date(gsub("^00([0-9]{2})", "20\\1", data_nasc)),

    # Setor
    setor = case_when(
      grepl("UTI",        toupper(setor)) ~ "UTI",
      grepl("ENFERMARIA", toupper(setor)) ~ "Enfermaria",
      TRUE ~ NA_character_
    ) |> factor(levels = c("UTI", "Enfermaria")),

    # Tipo de IAM
    tipo_iam = case_when(
      grepl("Com Supra", tipo_iam_texto, ignore.case = TRUE) ~ "IAMCSST",
      grepl("Sem Supra", tipo_iam_texto, ignore.case = TRUE) ~ "IAMSSST",
      TRUE ~ NA_character_
    ) |> factor(levels = c("IAMCSST", "IAMSSST")),

    # Perfil lipídico
    ct  = limpar_numerico(ct_texto),
    ldl = limpar_numerico(ldl_texto),
    hdl = limpar_numerico(hdl_texto),
    tgl = limpar_numerico(tgl_texto),

    # ---- Classificações operacionais (item 2.4 do TCC, calculadas a partir
    #      dos valores numéricos — não a partir do campo de texto livre
    #      "classificacao_lipidico", cuja estrutura interna é ambígua) ----

    ct_alterado = case_when(
      !is.na(ct) & ct >= 190 ~ "Sim",
      !is.na(ct) & ct <  190 ~ "Não",
      TRUE ~ NA_character_
    ) |> factor(),

    flag_hiperc  = (!is.na(ct)  & ct  >= 190) | (!is.na(ldl) & ldl >= 100),
    flag_hipertg = (!is.na(tgl) & tgl >= 150),
    flag_hdl_low = (!is.na(hdl) & hdl <  40),

    n_perfil_ok = (!is.na(ct)) + (!is.na(ldl)) + (!is.na(hdl)) + (!is.na(tgl)),
    n_alteracoes = rowSums(cbind(flag_hiperc, flag_hipertg, flag_hdl_low), na.rm = TRUE),

    tipo_dislipidemia = case_when(
      n_perfil_ok < 4 ~ NA_character_,
      n_alteracoes >= 2 ~ "Dislipidemia Mista",
      n_alteracoes == 1 & flag_hiperc  ~ "Hipercolesterolemia Isolada",
      n_alteracoes == 1 & flag_hipertg ~ "Hipertrigliceridemia Isolada",
      n_alteracoes == 1 & flag_hdl_low ~ "HDL Baixo Isolado",
      n_alteracoes == 0 ~ "Sem Alteração Lipídica",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Dislipidemia Mista", "Hipercolesterolemia Isolada",
                            "HDL Baixo Isolado", "Hipertrigliceridemia Isolada",
                            "Sem Alteração Lipídica")),

    # ---- Fatores de risco (variáveis binárias individuais) ----
    # Lidas a partir de fatores_risco (itens de seleção múltipla unidos por "; ")
    fatores_risco_norm = toupper(remove_acentos(fatores_risco)),

    has             = if_else(grepl("\\bHAS\\b",             fatores_risco_norm), "Sim", "Não") |> factor(),
    dm              = if_else(grepl("\\bDM\\b",              fatores_risco_norm), "Sim", "Não") |> factor(),
    tabagismo       = if_else(grepl("TABAGISMO",             fatores_risco_norm), "Sim", "Não") |> factor(),
    obesidade       = if_else(grepl("OBESIDADE",             fatores_risco_norm), "Sim", "Não") |> factor(),
    dlp             = if_else(grepl("\\bDLP\\b",             fatores_risco_norm), "Sim", "Não") |> factor(),
    sedentarismo    = if_else(grepl("SEDENTARISMO",          fatores_risco_norm), "Sim", "Não") |> factor(),
    hist_familiar   = if_else(grepl("FAMILIAR",              fatores_risco_norm), "Sim", "Não") |> factor(),
    doenca_arterial = if_else(grepl("DOENCA ARTERIAL",       fatores_risco_norm), "Sim", "Não") |> factor(),
    etilismo        = if_else(grepl("ETILISTA",              fatores_risco_norm), "Sim", "Não") |> factor(),

    # Uso de hipolipemiante
    hipolipemiante = padronizar_simnao(uso_hipolipemiante) |> factor(),

    # Insuficiência cardíaca (complicação)
    texto_evolucao_norm = toupper(remove_acentos(paste(gravidade_complicacoes, observacoes))),
    ic = if_else(grepl("INSUFICIENCIA CARDIACA", texto_evolucao_norm), "Sim", "Não") |> factor(),

    # Óbito hospitalar — buscado no texto livre de internamento/observações,
    # não apenas em internamento_texto isoladamente (campo é cortado por
    # vírgulas no export e a informação pode aparecer em observações)
    texto_obito_norm = toupper(remove_acentos(
      paste(internamento_texto, gravidade_complicacoes, observacoes)
    )),
    obito = if_else(grepl("OBITO", texto_obito_norm), "Sim", "Não") |> factor()
  )

# -----------------------------------------------------------------------------
# 3. DEDUPLICAÇÃO — UM REGISTRO POR PACIENTE (INTERNAÇÃO-ÍNDICE)
# -----------------------------------------------------------------------------
# - Pacientes identificados pela combinação nome + data de nascimento.
# - Registros com a mesma combinação e admissões a poucos dias de distância
#   são tratados como o mesmo episódio (duplicidade de registro/transferência).
# - Mantém-se o registro com mais componentes do perfil lipídico preenchidos;
#   em empate, mantém-se o de admissão mais antiga.
# - Casos com admissões distantes (>5 dias) são tratados como episódios de
#   internação distintos (reinternação) e mantidos separadamente.

dados <- dados |>
  mutate(
    nome_norm = toupper(trimws(remove_acentos(nome))),
    n_lipidico_ok = n_perfil_ok
  )

cat("\n===== Registros com nome + data de nascimento repetidos =====\n")
dados |>
  filter(!is.na(setor)) |>
  group_by(nome_norm, data_nasc) |>
  filter(n() > 1) |>
  arrange(nome_norm, data_admissao) |>
  select(nome, data_nasc, data_admissao, setor, ct, ldl, hdl, tgl) |>
  print(n = Inf)

dados_unicos <- dados |>
  filter(!is.na(setor)) |>
  group_by(nome_norm, data_nasc) |>
  group_modify(function(df, key) {
    if (nrow(df) == 1) return(df)
    # admissões muito distantes (>5 dias) = reinternação distinta -> mantém todas
    if (max(df$data_admissao, na.rm = TRUE) - min(df$data_admissao, na.rm = TRUE) > 5) {
      return(df)
    }
    df |>
      arrange(desc(n_lipidico_ok), data_admissao) |>
      slice(1)
  }) |>
  ungroup()

cat(sprintf(
  "\nRegistros brutos (com setor identificado): %d | Após deduplicação (internação-índice): %d\n",
  sum(!is.na(dados$setor)), nrow(dados_unicos)
))

cat("\nDistribuição final por setor:\n")
print(table(dados_unicos$setor, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 4. SHAPIRO-WILK EM VARIÁVEIS CONTÍNUAS
# -----------------------------------------------------------------------------

vars_continuas <- c("idade", "ct", "ldl", "hdl", "tgl")

sw <- dados_unicos |>
  select(all_of(vars_continuas)) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") |>
  drop_na(valor) |>
  group_by(variavel) |>
  filter(n() >= 3) |>
  summarise(
    n       = n(),
    W       = shapiro.test(valor)$statistic,
    p_valor = shapiro.test(valor)$p.value,
    .groups = "drop"
  ) |>
  mutate(distribuicao = if_else(p_valor >= 0.05, "Simétrica (p≥0,05)", "Assimétrica (p<0,05)"))

cat("\n===== Shapiro-Wilk =====\n")
sw |> mutate(across(c(W, p_valor), \(x) round(x, 4))) |> print(n = Inf)

vars_simetricas   <- sw |> filter(p_valor >= 0.05) |> pull(variavel)
vars_assimetricas <- sw |> filter(p_valor <  0.05) |> pull(variavel)

# -----------------------------------------------------------------------------
# 5. DESCRITIVAS CONTÍNUAS (para conferência)
# -----------------------------------------------------------------------------

descrever_continua <- function(df, var, simetrica) {
  x <- df[[var]][!is.na(df[[var]])]
  if (simetrica)
    tibble(variavel = var, n = length(x),
           resumo = sprintf("%.1f ± %.1f", mean(x), sd(x)),
           tipo   = "Simétrica (média ± DP)")
  else
    tibble(variavel = var, n = length(x),
           resumo = sprintf("%.1f [%.1f–%.1f]", median(x), quantile(x, .25), quantile(x, .75)),
           tipo   = "Assimétrica (mediana [IQR])")
}

desc <- bind_rows(
  lapply(vars_simetricas,   descrever_continua, df = dados_unicos, simetrica = TRUE),
  lapply(vars_assimetricas, descrever_continua, df = dados_unicos, simetrica = FALSE)
) |>
  mutate(variavel = factor(variavel, levels = vars_continuas)) |>
  arrange(variavel)

cat("\n===== Descritivas — Variáveis Contínuas =====\n")
print(desc, n = Inf)

# -----------------------------------------------------------------------------
# 6. TABELA 1 — ESTRATIFICADA POR SETOR (UTI | ENFERMARIA)
# -----------------------------------------------------------------------------

label_list <- list(
  idade             ~ "Idade (anos)",
  ct                ~ "Colesterol Total (mg/dL)",
  ldl               ~ "LDL-C (mg/dL)",
  hdl               ~ "HDL-C (mg/dL)",
  tgl               ~ "Triglicerídeos (mg/dL)",
  tipo_iam          ~ "Tipo de IAM",
  ct_alterado       ~ "Colesterol Total alterado (≥190 mg/dL)",
  tipo_dislipidemia ~ "Tipo de dislipidemia",
  hipolipemiante    ~ "Uso de hipolipemiante",
  has               ~ "HAS",
  dm                ~ "DM",
  tabagismo         ~ "Tabagismo",
  obesidade         ~ "Obesidade",
  dlp               ~ "DLP",
  sedentarismo      ~ "Sedentarismo",
  hist_familiar     ~ "Histórico familiar de DCV",
  doenca_arterial   ~ "Doença arterial prévia",
  etilismo          ~ "Etilismo",
  ic                ~ "Insuficiência cardíaca (complicação)",
  obito             ~ "Óbito hospitalar"
)

stat_continuas <- c(
  setNames(rep(list("{mean} ± {sd}"),          length(vars_simetricas)),   vars_simetricas),
  setNames(rep(list("{median} [{p25}–{p75}]"), length(vars_assimetricas)), vars_assimetricas)
)

vars_tabela <- c(vars_continuas,
                 "tipo_iam", "ct_alterado", "tipo_dislipidemia",
                 "has", "dm", "tabagismo", "obesidade", "dlp", "sedentarismo",
                 "hist_familiar", "doenca_arterial", "etilismo",
                 "hipolipemiante", "ic", "obito")
vars_categoricas <- setdiff(vars_tabela, vars_continuas)

# Detecta quais categóricas precisam de Fisher (célula esperada < 5)
usar_fisher <- vars_categoricas[
  sapply(vars_categoricas, function(v) {
    tab <- table(dados_unicos[[v]], dados_unicos$setor)
    any(suppressWarnings(chisq.test(tab)$expected) < 5)
  })
]
usar_chisq <- setdiff(vars_categoricas, usar_fisher)

# add_p(test = ...) em versões recentes do gtsummary só aceita uma lista de
# fórmulas (ou fórmula única / lista nomeada simples), não mais um vetor
# nomeado construído com c(setNames(...)). Monta-se a lista de fórmulas
# dinamicamente, descartando grupos vazios para evitar erro com all_of(character(0)).

grupos_teste <- list(
  list(vars = vars_simetricas,   teste = "t.test"),
  list(vars = vars_assimetricas, teste = "wilcox.test"),
  list(vars = usar_chisq,        teste = "chisq.test"),
  list(vars = usar_fisher,       teste = "fisher.test")
)

formulas_teste <- grupos_teste |>
  keep(~ length(.x$vars) > 0) |>
  map(~ as.formula(paste0(
    "c(", paste(.x$vars, collapse = ", "), ") ~ \"", .x$teste, "\""
  )))

tbl1 <- dados_unicos |>
  select(setor, all_of(vars_tabela)) |>
  tbl_summary(
    by        = setor,
    statistic = stat_continuas,
    label     = label_list,
    missing   = "ifany",
    missing_text = "Dados ausentes"
  ) |>
  add_overall(last = FALSE) |>
  add_p(test = formulas_teste) |>
  bold_labels() |>
  bold_p(t = 0.05) |>
  modify_spanning_header(c(stat_1, stat_2) ~ "**Setor de admissão**")

cat("\n===== Tabela 1 — Estratificada por Setor =====\n")
print(tbl1)

# Exportar para Word (descomente):
# tbl1 |> as_gt() |> gt::gtsave("tabela1_iam_perfil_lipidico.docx")

# -----------------------------------------------------------------------------
# 7. COMPLETUDE DO PERFIL LIPÍDICO E PADRÕES DE DISLIPIDEMIA (TABELA 3)
# -----------------------------------------------------------------------------

cat("\n===== Completude do Perfil Lipídico =====\n")
dados_unicos |>
  summarise(
    n_total  = n(),
    ct_ok    = sum(!is.na(ct)),
    ldl_ok   = sum(!is.na(ldl)),
    hdl_ok   = sum(!is.na(hdl)),
    tgl_ok   = sum(!is.na(tgl)),
    todos_ok = sum(!is.na(ct) & !is.na(ldl) & !is.na(hdl) & !is.na(tgl))
  ) |>
  pivot_longer(-n_total, names_to = "variavel", values_to = "n_validos") |>
  mutate(pct = sprintf("%.1f%%", 100 * n_validos / n_total)) |>
  print()

cat("\n===== Frequência de alterações lipídicas (entre disponíveis) =====\n")
freq_alteracoes <- dados_unicos |>
  summarise(
    `CT >= 190`   = sprintf("%d/%d = %.1f%%", sum(ct  >= 190, na.rm=TRUE), sum(!is.na(ct)),  100*sum(ct  >= 190, na.rm=TRUE)/sum(!is.na(ct))),
    `LDL >= 100`  = sprintf("%d/%d = %.1f%%", sum(ldl >= 100, na.rm=TRUE), sum(!is.na(ldl)), 100*sum(ldl >= 100, na.rm=TRUE)/sum(!is.na(ldl))),
    `LDL >= 55`   = sprintf("%d/%d = %.1f%%", sum(ldl >= 55,  na.rm=TRUE), sum(!is.na(ldl)), 100*sum(ldl >= 55,  na.rm=TRUE)/sum(!is.na(ldl))),
    `HDL < 40`    = sprintf("%d/%d = %.1f%%", sum(hdl < 40,   na.rm=TRUE), sum(!is.na(hdl)), 100*sum(hdl < 40,   na.rm=TRUE)/sum(!is.na(hdl))),
    `TG >= 150`   = sprintf("%d/%d = %.1f%%", sum(tgl >= 150, na.rm=TRUE), sum(!is.na(tgl)), 100*sum(tgl >= 150, na.rm=TRUE)/sum(!is.na(tgl)))
  )
print(t(freq_alteracoes))

cat("\n===== Padrão de dislipidemia (perfil completo) =====\n")
dados_unicos |>
  filter(!is.na(tipo_dislipidemia)) |>
  count(tipo_dislipidemia) |>
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
  print()
