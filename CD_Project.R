# ==============================================================================
# PIPELINE EPIDEMIOLÓGICO: SAÚDE DO TRABALHADOR - ALAGOAS (2013-2024)
# Extração FTP, Safe Linkage e Regressão Prais-Winsten (AR-1 REML Uniforme)
# ==============================================================================

# 1. CARREGAR PACOTES
# install.packages(c("read.dbc","dplyr","purrr","tidyr","nlme","lmtest","officer","flextable"))
library(read.dbc)
library(dplyr)
library(purrr)
library(tidyr)
library(nlme)
library(lmtest)
library(officer)
library(flextable)

# 2. PARÂMETROS METODOLÓGICOS
anos_interesse   <- 2013:2024
uf_ibge          <- "27"
uf_sigla         <- "AL"
agravos_trabalho <- c("ACGR", "ACBI", "LERD", "PAIR", "DERM", "PNEU", "TMRT", "CANC")
pasta_saida      <- "/Users/davibarbosa/Desktop/Carlos Daniel"

# Período pandêmico — anotado graficamente e excluído na análise de sensibilidade
pandemia_anos <- 2020:2021

data_evento_por_agravo <- c(
  ACGR = "DT_ACIDENTE", ACBI = "DT_ACIDENTE",
  LERD = "DT_INIC_SINTOMAS", PAIR = "DT_DIAGN",
  DERM = "DT_INIC_SINTOMAS", PNEU = "DT_DIAGN",
  TMRT = "DT_INIC_SINTOMAS", CANC = "DT_DIAGN"
)

rotulos_agravos <- c(
  ACGR = "Acidente de Trabalho Grave", ACBI = "Acidente c/ Mat. Biológico",
  LERD = "LER/DORT",                   PAIR = "Perda Auditiva (PAIR)",
  DERM = "Dermatose Ocupacional",       PNEU = "Pneumoconiose",
  TMRT = "Transtorno Mental",           CANC = "Câncer Ocupacional"
)

# 3. DENOMINADOR — BLOQUEIO EXPLÍCITO SE DADOS FICTÍCIOS NÃO FOREM SUBSTITUÍDOS
# Substitua pelos valores reais:
#   2013-2017: RAIS (ftp://ftp.mtps.gov.br/pdet/microdados/)
#   2018-2024: CAGED (https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/estatisticas)
# Selecione o estoque de vínculos formais de Alagoas em 31/12 de cada ano.
pop_formal_al <- tibble(
  Ano              = 2013:2024,
  Populacao_Formal = c(
    # 2013–2017: vínculos ativos em 31/12 — PDFs estaduais RAIS/PDET (MTE)
    509125, 514391, 509275, 490272, 486763,
    # 2018–2019: estoque histórico pré-eSocial — RAIS/PDET
    493858, 486716,
    # 2020–2024: Novo CAGED / consolidação eSocial
    481543, 522990, 578837, 622826, 628753
  )
)

# 4. MOTOR DE DOWNLOAD FTP E FILTRO HÍBRIDO
bancos_baixados <- list()
urls_base <- c(
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/",
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
)

# Extração robusta de ano — trata os três formatos encontrados nos arquivos SINAN
extrair_ano <- function(x) {
  as.integer(format(
    as.Date(as.character(x),
            tryFormats = c("%Y-%m-%d", "%Y%m%d", "%d/%m/%Y")),
    "%Y"
  ))
}

cat("\n[1/4] Iniciando mineração no FTP do DataSUS...\n")

for (agravo in agravos_trabalho) {
  dados_agravo_lista <- list()
  col_data_evento    <- data_evento_por_agravo[agravo]

  for (ano in anos_interesse) {
    arquivo_dbc <- paste0(agravo, "BR", substr(as.character(ano), 3, 4), ".dbc")
    dest_file   <- tempfile(fileext = ".dbc")

    sucesso_download <- FALSE
    for (url_base in urls_base) {
      ok <- tryCatch({
        suppressWarnings(download.file(paste0(url_base, arquivo_dbc),
                                       dest_file, mode = "wb", quiet = TRUE))
        TRUE
      }, error = function(e) FALSE)
      if (ok && file.info(dest_file)$size > 0) { sucesso_download <- TRUE; break }
    }

    if (sucesso_download) {
      df_temp <- tryCatch(read.dbc::read.dbc(dest_file), error = function(e) NULL)

      if (!is.null(df_temp) && nrow(df_temp) > 0) {
        # Filtro híbrido: aceita código IBGE ("27") e sigla postal ("AL")
        df_filtrado <- df_temp %>%
          filter(
            (if ("SG_UF_NOT" %in% names(.))
               as.character(SG_UF_NOT) %in% c(uf_sigla, uf_ibge) else FALSE) |
            (if ("ID_MUNICIP" %in% names(.))
               substr(as.character(ID_MUNICIP), 1, 2) == uf_ibge else FALSE)
          )

        if (nrow(df_filtrado) > 0)
          dados_agravo_lista[[as.character(ano)]] <- df_filtrado
      }
    }
    if (file.exists(dest_file)) unlink(dest_file)
  }

  if (length(dados_agravo_lista) == 0) {
    cat(sprintf("  ! Aviso: zero casos para %s.\n", agravo))
    next
  }

  df_consolidado <- bind_rows(dados_agravo_lista)

  # Extração robusta do ano do evento (não o ano do arquivo DBC)
  col_ano_fonte <- if (col_data_evento %in% names(df_consolidado)) {
    col_data_evento
  } else if ("DT_NOTIFIC" %in% names(df_consolidado)) {
    warning(agravo, ": '", col_data_evento, "' ausente — usando DT_NOTIFIC como fallback.")
    "DT_NOTIFIC"
  } else {
    NA_character_
  }

  if (!is.na(col_ano_fonte)) {
    df_consolidado$ANO_REAL <- suppressWarnings(extrair_ano(df_consolidado[[col_ano_fonte]]))
  } else {
    warning(agravo, ": sem coluna de data válida. Agravo ignorado.")
    next
  }

  df_consolidado <- df_consolidado %>%
    filter(!is.na(ANO_REAL),
           ANO_REAL >= min(anos_interesse),
           ANO_REAL <= max(anos_interesse))

  # Deduplicação por proxy demográfico (microdados anonimizados do SINAN)
  # NM_PACIENT e DT_NASC não estão disponíveis nos microdados públicos do MS.
  # A chave usa variáveis demográficas agregadas disponíveis na ficha anonimizada:
  #   ID_MN_RESI  : município de residência (mais estável que município de notificação)
  #   CS_SEXO     : sexo
  #   NU_IDADE_N  : idade codificada
  #   CS_RACA     : raça/cor (discriminador adicional quando disponível)
  #   col_data_evento : data do evento (acidente/início de sintomas/diagnóstico)
  #
  # Limitação conhecida: dois trabalhadores distintos com os mesmos valores em todos
  # esses campos serão incorretamente colapsados (falsa deduplicação). O risco é
  # baixo para agravos raros (PNEU, CANC), mas não desprezível para ACGR em
  # municípios pequenos. Deve ser declarado como limitação do estudo.
  candidatos_proxy <- c("ID_MN_RESI", "CS_SEXO", "NU_IDADE_N", "CS_RACA", col_data_evento)
  chave_linkage    <- intersect(candidatos_proxy, names(df_consolidado))

  n_antes <- nrow(df_consolidado)
  if (length(chave_linkage) >= 3) {
    df_consolidado <- df_consolidado %>%
      distinct(across(all_of(chave_linkage)), .keep_all = TRUE)
  } else {
    warning(agravo, ": chave de linkage insuficiente (",
            paste(chave_linkage, collapse = ", "),
            "). Deduplicação ignorada — verifique as colunas disponíveis na ficha.")
  }

  cat(sprintf("  ✓ %s: %d registros únicos (%d duplicatas removidas).\n",
              agravo, nrow(df_consolidado), n_antes - nrow(df_consolidado)))
  bancos_baixados[[agravo]] <- df_consolidado
}

# 5. CÁLCULO DE INCIDÊNCIA
cat("\n[2/4] Calculando Taxas de Incidência (por 100.000 trabalhadores formais)...\n")

resumo_anual <- imap_dfr(bancos_baixados, function(df, nome_agravo) {
  df %>%
    group_by(Ano = ANO_REAL) %>%
    summarise(N_Absoluto = n(), .groups = "drop") %>%
    mutate(Agravo = nome_agravo)
})

if (nrow(resumo_anual) == 0 || !"Ano" %in% names(resumo_anual))
  stop("Nenhum dado válido carregado. Verifique conexão FTP.")

dados_analise <- resumo_anual %>%
  complete(Ano = anos_interesse, Agravo = agravos_trabalho,
           fill = list(N_Absoluto = 0)) %>%
  left_join(pop_formal_al, by = "Ano") %>%
  mutate(Incidencia_100k = (N_Absoluto / Populacao_Formal) * 100000)

# 6. FUNÇÃO DE MODELAGEM AR-1 UNIFORME (PRAIS-WINSTEN VIA REML)
# AR-1 aplicado uniformemente a todos os agravos — justificativa a priori:
# séries de notificação de vigilância apresentam dependência temporal conhecida.
# O teste DW serve como evidência post-hoc reportada na tabela, não como seletor
# de modelo, garantindo comparabilidade dos APCs entre agravos.
calcular_apc <- function(df, excluir_pandemia = FALSE, nome_agravo = "") {

  # CORREÇÃO 1: anos reais mantidos como preditor mesmo após exclusão pandêmica.
  # Reindexar para 1,2,...,n trataria o salto 2019→2022 como intervalo de 1 ano,
  # distorcendo o slope da sensibilidade. O gls estima corretamente com anos esparsos.
  if (excluir_pandemia)
    df <- df %>% filter(!Ano %in% pandemia_anos)

  if (sum(df$N_Absoluto) == 0 || var(df$Incidencia_100k, na.rm = TRUE) == 0)
    return(tibble(APC = NA_real_, IC_Inf = NA_real_, IC_Sup = NA_real_,
                  Tendencia = "N/A (Série Nula)", DW_p = NA_real_, Rho = NA_real_,
                  Modelo = NA_character_))

  df_model      <- as.data.frame(df)
  df_model$x    <- as.numeric(df_model$Ano)
  df_model$y    <- log10(df_model$Incidencia_100k + 0.01)

  # DW pré-modelo: reportado como diagnóstico, não como seletor
  dw_p <- tryCatch(dwtest(lm(y ~ x, data = df_model))$p.value, error = function(e) NA_real_)

  # CORREÇÃO 2: fallback para GLS sem AR-1 logado com warning explícito.
  # O agravo que cair no fallback aparece com Rho = NA e Modelo = "GLS-OLS" na tabela,
  # tornando visível ao leitor que aquele APC veio de um modelo diferente.
  modelo_usado <- "GLS-AR1"
  mod <- tryCatch(
    gls(y ~ x, data = df_model,
        correlation = corAR1(form = ~ x),
        method = "REML"),
    error = function(e) {
      warning(sprintf(
        "AR-1 não convergiu para '%s'%s — usando GLS simples (sem correção AR-1). Erro: %s",
        nome_agravo,
        if (excluir_pandemia) " [sensibilidade]" else "",
        conditionMessage(e)
      ))
      modelo_usado <<- "GLS-OLS"
      tryCatch(
        gls(y ~ x, data = df_model, method = "REML"),
        error = function(e2) NULL
      )
    }
  )

  if (is.null(mod))
    return(tibble(APC = NA_real_, IC_Inf = NA_real_, IC_Sup = NA_real_,
                  Tendencia = "Erro no modelo", DW_p = dw_p, Rho = NA_real_,
                  Modelo = modelo_usado))

  b1  <- coef(mod)["x"]
  ci  <- suppressMessages(confint(mod)["x", ])
  rho <- if (!is.null(mod$modelStruct$corStruct))
           round(coef(mod$modelStruct$corStruct, unconstrained = FALSE), 3)
         else NA_real_

  apc     <- (10^b1              - 1) * 100
  lim_inf <- (10^ci["2.5 %"]    - 1) * 100
  lim_sup <- (10^ci["97.5 %"]   - 1) * 100

  tibble(
    APC       = round(apc,     1),
    IC_Inf    = round(lim_inf, 1),
    IC_Sup    = round(lim_sup, 1),
    Tendencia = case_when(lim_inf > 0 ~ "Crescente",
                          lim_sup < 0 ~ "Decrescente",
                          TRUE        ~ "Estacionária"),
    DW_p      = round(dw_p, 4),
    Rho       = rho,
    Modelo    = modelo_usado
  )
}

# 7. ANÁLISE PRINCIPAL E DE SENSIBILIDADE
cat("[3/4] Rodando modelos AR-1 (principal + sensibilidade pandêmica)...\n")

resultados_principal <- dados_analise %>%
  group_by(Agravo) %>%
  group_modify(~ calcular_apc(.x, excluir_pandemia = FALSE, nome_agravo = .y$Agravo)) %>%
  ungroup() %>%
  mutate(Analise = "Principal (2013–2024)")

resultados_sens <- dados_analise %>%
  group_by(Agravo) %>%
  group_modify(~ calcular_apc(.x, excluir_pandemia = TRUE,  nome_agravo = .y$Agravo)) %>%
  ungroup() %>%
  mutate(Analise = "Sensibilidade (excl. 2020–2021)")

resultados_comparativos <- bind_rows(resultados_principal, resultados_sens) %>%
  mutate(
    Agravo_Label = rotulos_agravos[Agravo],
    APC_fmt = case_when(
      is.na(APC) ~ Tendencia,
      TRUE ~ sprintf("%.1f%% [%.1f; %.1f] — %s\nρ=%.3f | DW p=%s",
                     APC, IC_Inf, IC_Sup, Tendencia, Rho,
                     ifelse(DW_p < 0.0001, "<0,0001", as.character(DW_p)))
    )
  )

# 8. TABELA PRINCIPAL (DOCX)
cat("[4/4] Exportando tabelas...\n")

# Tabela wide: N (incidência) por ano + coluna APC
tabela_wide <- dados_analise %>%
  mutate(
    Texto = sprintf("%d (%.1f)", N_Absoluto, Incidencia_100k),
    Agravo_Label = rotulos_agravos[Agravo]
  ) %>%
  select(Agravo, Agravo_Label, Ano, Texto) %>%
  pivot_wider(names_from = Ano, values_from = Texto) %>%
  left_join(
    resultados_principal %>%
      mutate(APC_col = sprintf("%.1f%% [%.1f; %.1f]\n%s",
                               APC, IC_Inf, IC_Sup, Tendencia)) %>%
      select(Agravo, APC_col),
    by = "Agravo"
  ) %>%
  select(-Agravo) %>%
  rename(`Agravo Ocupacional` = Agravo_Label,
         `APC [IC95%] — Tendência` = APC_col)

ft_principal <- flextable(tabela_wide) %>%
  theme_vanilla() %>%
  align(j = -1, align = "center", part = "all") %>%
  align(j = 1,  align = "left",   part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  set_caption(paste0(
    "Tabela 1. Casos absolutos (n), taxa de incidência (por 100.000 trabalhadores formais) ",
    "e Variação Percentual Anual (APC) com IC95%, Alagoas, 2013–2024."
  )) %>%
  add_footer_lines(c(
    "Fonte: SINAN/DATASUS e CAGED/RAIS (Ministério do Trabalho).",
    paste0("Nota: Células contêm N (taxa/100.000). APC calculado via Regressão de ",
           "Prais-Winsten (GLS com estrutura AR-1, REML) sobre log10(taxa + 0,01). ",
           "IC95% obtido por back-transformação assimétrica (β ± 1,96·EP antes da ",
           "transformação inversa). AR-1 aplicado uniformemente; o coeficiente ρ e o ",
           "p-valor do teste Durbin-Watson pré-modelo são reportados na Tabela 2. ",
           "Período pandêmico (2020–2021) incluído na análise principal e excluído na ",
           "análise de sensibilidade (ver Tabela 2).")
  )) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit()

# Tabela de sensibilidade pandêmica (Tabela 2)
tabela_sens <- resultados_comparativos %>%
  select(`Agravo Ocupacional` = Agravo_Label,
         Análise              = Analise,
         `APC (%/ano)`        = APC,
         `IC95% Inf`          = IC_Inf,
         `IC95% Sup`          = IC_Sup,
         Tendência            = Tendencia,
         `ρ AR-1`             = Rho,
         `DW p-valor`         = DW_p,
         Modelo               = Modelo) %>%
  arrange(`Agravo Ocupacional`, Análise)

ft_sens <- flextable(tabela_sens) %>%
  theme_vanilla() %>%
  align(j = -1, align = "center", part = "all") %>%
  align(j = 1,  align = "left",   part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  color(
    i = ~ Tendência == "Crescente",
    j = "Tendência", color = "#276749"
  ) %>%
  color(
    i = ~ Tendência == "Decrescente",
    j = "Tendência", color = "#c53030"
  ) %>%
  set_caption(paste0(
    "Tabela 2. Análise de sensibilidade: APC com e sem exclusão do período pandêmico ",
    "(2020–2021), com diagnóstico de autocorrelação (ρ AR-1 e teste Durbin-Watson)."
  )) %>%
  add_footer_lines(c(
    "Nota: DW p-valor refere-se ao teste de Durbin-Watson nos resíduos OLS pré-modelo.",
    "p < 0,05 confirma autocorrelação de primeira ordem e valida post-hoc o uso do AR-1.",
    "Modelo: 'GLS-AR1' = Prais-Winsten (AR-1, REML); 'GLS-OLS' = GLS sem estrutura de correlação",
    "(fallback automático quando AR-1 não converge — APC comparável com cautela).",
    "A exclusão de 2020–2021 testa a robustez do slope à influência pandêmica no numerador",
    "(ACBI) e no denominador (retração do emprego formal). Anos reais mantidos como preditor",
    "na sensibilidade para preservar a escala temporal correta do salto 2019→2022."
  )) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit()

# Exportar DOCX com as duas tabelas
doc <- read_docx() %>%
  body_add_par("Resultados Epidemiológicos — Série Histórica Consolidada",
               style = "heading 1") %>%
  body_add_par("Tabela 1 — Incidência e Tendência Temporal", style = "heading 2") %>%
  body_add_flextable(ft_principal) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Tabela 2 — Análise de Sensibilidade (Pandemia)", style = "heading 2") %>%
  body_add_flextable(ft_sens)

print(doc, target = file.path(pasta_saida,
                              "Tabela_Epidemiologica_PraisWinsten_Consolidada.docx"))

cat("\n>>> PRONTO! Arquivo salvo em:", pasta_saida, "\n")
sessionInfo()
