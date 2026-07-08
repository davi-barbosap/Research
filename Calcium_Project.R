# ══════════════════════════════════════════════════════════════════════════════
# IMPACTO DA SUPLEMENTAÇÃO DE CÁLCIO NA DHEG — MATERNIDADE SANTA MÔNICA (AL)
# Série Temporal Interrompida | M1 Quasipoisson + M2 Poisson Bayesiano
# Padrão de Formatação: The Lancet Regional Health - Americas
# ══════════════════════════════════════════════════════════════════════════════

# Força o uso do idioma inglês nas datas do sistema
Sys.setlocale("LC_TIME", "C")
# Garante que o R use ponto (.) como decimal internamente antes das conversões
options(OutDec = ".")

# ── 0. Pacotes ─────────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  microdatasus, dplyr, tidyr, lubridate, ggplot2,
  brms, cmdstanr, posterior, bayesplot,
  officer, flextable, lmtest
)

if (is.null(cmdstan_version(error_on_NA = FALSE))) {
  install_cmdstan(cores = parallel::detectCores())
}


# ── 1. Diretórios ──────────────────────────────────────────────────────────────
dir_resultados <- "/Users/davibarbosa/Desktop/Beatriz/Resultados"
dir_sih        <- "/Users/davibarbosa/Desktop/Beatriz/SIHSUS"
dir.create(dir_resultados, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_sih,        recursive = TRUE, showWarnings = FALSE)


# ── 2. Funções Auxiliares (Padrão The Lancet) ──────────────────────────────────

# Troca o ponto final tradicional pelo ponto mediano (midline decimal)
lancet_dec <- function(x) {
  gsub("\\.", "·", as.character(x))
}

# Formata números com casas decimais fixas e aplica o ponto mediano
fmt_num <- function(x, digits = 2) {
  lancet_dec(format(round(x, digits), nsmall = digits, trim = TRUE))
}

# Formata p-valor com 2 algarismos significativos e ponto mediano (Regra Lancet)
fmt_pval_lancet <- function(p) {
  if (is.na(p)) return("—")
  if (p < 0.0001) return("<0·0001")
  res <- format(signif(p, 2), scientific = FALSE, drop0trailing = TRUE)
  return(lancet_dec(res))
}

# Salva ggplot em PNG + TIFF 300 dpi
salvar_grafico <- function(grafico, nome, width_mm = 190, height_mm = 110) {
  ggsave(file.path(dir_resultados, paste0(nome, ".png")),
         plot = grafico, width = width_mm, height = height_mm, units = "mm", 
         dpi = 300, bg = "white")
  ggsave(file.path(dir_resultados, paste0(nome, ".tiff")),
         plot = grafico, width = width_mm, height = height_mm, units = "mm",
         dpi = 300, bg = "white", device = "tiff")
}

# Salva plot base em PNG + TIFF 300 dpi
salvar_plot_base <- function(plot_fn, nome, width_mm = 190, height_mm = 65) {
  png(file.path(dir_resultados, paste0(nome, ".png")),
      width = width_mm, height = height_mm, units = "mm", res = 300)
  plot_fn()
  dev.off()
  tiff(file.path(dir_resultados, paste0(nome, ".tiff")),
       width = width_mm, height = height_mm, units = "mm", res = 300)
  plot_fn()
  dev.off()
}

# Adiciona output bruto no DOCX (Material Suplementar)
doc_add_console <- function(doc, expr, titulo, nivel = "heading 1") {
  doc <- body_add_par(doc, titulo, style = nivel)
  linhas    <- capture.output(expr)
  prop_mono <- fp_text(font.family = "Courier New", font.size = 9)
  for (l in linhas) {
    doc <- body_add_fpar(doc, fpar(ftext(l, prop = prop_mono)))
  }
  doc
}


# ── 3. Download / checkpoint SIH ──────────────────────────────────────────────
arquivo_checkpoint <- file.path(dir_sih, "sih_limpo_AL_2023_2026.rds")

if (file.exists(arquivo_checkpoint)) {
  sih_processado <- readRDS(arquivo_checkpoint)
} else {
  anos  <- 2023:2026
  meses <- list(
    `2023` = list(ini = 2,  fim = 12),
    `2024` = list(ini = 1,  fim = 12),
    `2025` = list(ini = 1,  fim = 12),
    `2026` = list(ini = 1,  fim = 3)
  )
  lista_sih <- vector("list", length(anos))
  for (i in seq_along(anos)) {
    ano <- anos[i]
    m   <- meses[[as.character(ano)]]
    raw <- fetch_datasus(
      year_start  = ano, month_start = m$ini,
      year_end    = ano, month_end   = m$fim,
      uf = "AL",  information_system = "SIH-RD"
    )
    lista_sih[[i]] <- process_sih(raw)
  }
  sih_processado <- bind_rows(lista_sih)
  saveRDS(sih_processado, arquivo_checkpoint)
}


# ── 4. Filtros SIH — Santa Mônica, internações obstétricas ────────────────────
cids_dheg <- c("O14", "O140", "O141", "O149",
               "O15", "O150", "O151", "O152", "O159")

sih_santa_monica <- sih_processado %>%
  filter(CNES == "2009250", ESPEC == "02") %>%
  select(ANO_CMPT, MES_CMPT, DIAG_PRINC, MORTE, DIAS_PERM, MARCA_UTI,
         IDADE, GESTRISCO) %>%
  mutate(
    caso_dheg = if_else(DIAG_PRINC %in% cids_dheg, 1L, 0L),
    obito     = if_else(MORTE == "Sim" & caso_dheg == 1L, 1L, 0L),
    alto_risco = if_else(GESTRISCO == "2", 1L, 0L)
  )


# ── 5. Base mensal e variáveis ITS ────────────────────────────────────────────
mes_inicio_pol <- 25L   # Fev/2025
lag_base       <- 6L
mes_corte_base <- mes_inicio_pol + lag_base  # = 31 (Ago/2025)

base_mensal <- sih_santa_monica %>%
  group_by(ANO_CMPT, MES_CMPT) %>%
  summarise(
    total_internacoes = n(),
    casos_dheg        = sum(caso_dheg),
    obitos_maternos   = sum(obito),
    .groups = "drop"
  )

base_its <- base_mensal %>%
  mutate(
    ANO_CMPT = as.integer(ANO_CMPT),
    MES_CMPT = as.integer(MES_CMPT)
  ) %>%
  arrange(ANO_CMPT, MES_CMPT) %>%
  mutate(
    tempo       = row_number(),
    intervencao = if_else(tempo >= mes_corte_base, 1L, 0L),
    tempo_pos   = if_else(intervencao == 1L, tempo - (mes_corte_base - 1L), 0L),
    mes_num = MES_CMPT,
    sin1    = sin(2 * pi * 1 * mes_num / 12),
    cos1    = cos(2 * pi * 1 * mes_num / 12),
    sin2    = sin(2 * pi * 2 * mes_num / 12),
    cos2    = cos(2 * pi * 2 * mes_num / 12)
  )


# ── 6. Modelo M1 — Quasipoisson ITS (frequentista) ────────────────────────────
modelo_step <- glm(
  casos_dheg ~ tempo + intervencao + sin1 + cos1 + sin2 + cos2,
  offset = log(total_internacoes),
  family = quasipoisson(link = "log"),
  data   = base_its
)

modelo_freq <- glm(
  casos_dheg ~ tempo + intervencao + tempo_pos + sin1 + cos1 + sin2 + cos2,
  offset = log(total_internacoes),
  family = quasipoisson(link = "log"),
  data   = base_its
)

ftest_slope <- anova(modelo_step, modelo_freq, test = "F")

irr_m1 <- exp(coef(modelo_freq))
ci_m1  <- exp(confint.default(modelo_freq))
phi_m1 <- summary(modelo_freq)$dispersion

res_m1 <- residuals(modelo_freq, type = "deviance")

ljung_box_6  <- Box.test(res_m1, lag = 6,  type = "Ljung-Box")
ljung_box_12 <- Box.test(res_m1, lag = 12, type = "Ljung-Box")


# ── 7. Modelo M2 — Poisson Bayesiano ITS (brms) ───────────────────────────────
prioris_its <- c(
  prior(normal(0, 1.0), class = Intercept),
  prior(normal(0, 0.1), class = b, coef = tempo),
  prior(normal(0, 0.5), class = b, coef = intervencao),
  prior(normal(0, 0.5), class = b, coef = tempo_pos),
  prior(normal(0, 0.5), class = b, coef = sin1),
  prior(normal(0, 0.5), class = b, coef = cos1),
  prior(normal(0, 0.5), class = b, coef = sin2),
  prior(normal(0, 0.5), class = b, coef = cos2)
)

modelo_bayes <- brm(
  formula = casos_dheg ~ tempo + intervencao + tempo_pos +
    sin1 + cos1 + sin2 + cos2 +
    offset(log(total_internacoes)),
  family  = poisson(link = "log"),
  data    = base_its,
  prior   = prioris_its,
  chains  = 4,
  iter    = 10000,
  warmup  = 2000,
  cores   = 4,
  seed    = 42,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

sumario_bayes <- summary(modelo_bayes)
divergencias  <- nuts_params(modelo_bayes) %>% filter(Parameter == "divergent__") %>% pull(Value) %>% sum()

fixef_bayes <- sumario_bayes$fixed
irr_m2      <- exp(fixef_bayes[, "Estimate"])
ci_m2_lo    <- exp(fixef_bayes[, "l-95% CI"])
ci_m2_hi    <- exp(fixef_bayes[, "u-95% CI"])


# ── 8. Análise de sensibilidade — lags 4 a 8 meses (M1) ──────────────────────
lags_testar <- 4:8
lista_sens  <- vector("list", length(lags_testar))

for (i in seq_along(lags_testar)) {
  lag_n     <- lags_testar[i]
  mes_corte <- mes_inicio_pol + lag_n
  
  dados_temp <- base_its %>%
    mutate(
      intervencao = if_else(tempo >= mes_corte, 1L, 0L),
      tempo_pos   = if_else(intervencao == 1L, tempo - (mes_corte - 1L), 0L)
    )
  
  m_temp  <- glm(
    casos_dheg ~ tempo + intervencao + tempo_pos + sin1 + cos1 + sin2 + cos2,
    offset = log(total_internacoes),
    family = quasipoisson(link = "log"),
    data   = dados_temp
  )
  ci_temp <- suppressMessages(confint.default(m_temp))
  
  lista_sens[[i]] <- tibble(
    lag     = lag_n,
    irr     = exp(coef(m_temp)["intervencao"]),
    ci_lo   = exp(ci_temp["intervencao", 1]),
    ci_hi   = exp(ci_temp["intervencao", 2]),
    p_valor = summary(m_temp)$coefficients["intervencao", "Pr(>|t|)"]
  )
}
sens_df <- bind_rows(lista_sens)


# ── 9. ITS de Proporção — Eclâmpsia vs Pré-eclâmpsia ─────────────────────────
antes_inicio <- bind_rows(
  tibble(ANO_CMPT = 2024L, MES_CMPT = 7:12),
  tibble(ANO_CMPT = 2025L, MES_CMPT = 1L)
)
depois_inicio <- bind_rows(
  tibble(ANO_CMPT = 2025L, MES_CMPT = 8:12),
  tibble(ANO_CMPT = 2026L, MES_CMPT = c(1L, 2L))
)

classificar_dheg <- function(cid) {
  case_when(
    grepl("^O14", cid) ~ "Pre-eclampsia (O14)",
    grepl("^O15", cid) ~ "Eclampsia (O15)",
    TRUE               ~ "Other"
  )
}

tabela_cid <- sih_santa_monica %>%
  filter(caso_dheg == 1L) %>%
  mutate(
    ano_int = as.integer(ANO_CMPT),
    mes_int = as.integer(MES_CMPT),
    periodo = case_when(
      paste(ano_int, mes_int) %in% paste(antes_inicio$ANO_CMPT, antes_inicio$MES_CMPT) ~ "Before",
      paste(ano_int, mes_int) %in% paste(depois_inicio$ANO_CMPT, depois_inicio$MES_CMPT) ~ "After",
      TRUE ~ NA_character_
    ),
    grupo_cid = classificar_dheg(DIAG_PRINC)
  ) %>%
  filter(!is.na(periodo)) %>%
  count(grupo_cid, periodo) %>%
  pivot_wider(names_from = periodo, values_from = n, values_fill = 0)

split_total <- sih_santa_monica %>%
  filter(caso_dheg == 1L) %>%
  mutate(tipo = if_else(grepl("^O14", DIAG_PRINC), "PE", "EC")) %>%
  count(tipo)

base_subtipos <- base_its %>%
  left_join(
    sih_santa_monica %>%
      filter(caso_dheg == 1L) %>%
      mutate(ANO_CMPT = as.integer(ANO_CMPT), MES_CMPT = as.integer(MES_CMPT)) %>%
      group_by(ANO_CMPT, MES_CMPT) %>%
      summarise(eclampsia = sum(if_else(grepl("^O15", DIAG_PRINC), 1L, 0L)), .groups = "drop"),
    by = c("ANO_CMPT", "MES_CMPT")
  ) %>%
  mutate(
    eclampsia     = replace_na(eclampsia, 0L),
    pre_eclampsia = casos_dheg - eclampsia
  )

modelo_subtipo_its <- glm(
  cbind(eclampsia, pre_eclampsia) ~ tempo + intervencao + tempo_pos + sin1 + cos1 + sin2 + cos2,
  family = binomial(link = "logit"),
  data   = base_subtipos
)

or_subtipo <- exp(coef(modelo_subtipo_its))
ci_subtipo <- exp(suppressMessages(confint.default(modelo_subtipo_its)))


# ── 10. N Amostral (Console Print) ─────────────────────────────────────────────
n_total_internacoes <- nrow(sih_santa_monica)
n_casos_dheg        <- sum(sih_santa_monica$caso_dheg)
n_obitos            <- sum(sih_santa_monica$obito)
n_meses             <- nrow(base_its)
n_meses_pre         <- sum(base_its$intervencao == 0)
n_meses_pos         <- sum(base_its$intervencao == 1)

mes_ini_str <- with(base_its[1, ], paste0(month.abb[MES_CMPT], "/", ANO_CMPT))
mes_fim_str <- with(base_its[nrow(base_its), ], paste0(month.abb[MES_CMPT], "/", ANO_CMPT))

chave_pre <- paste(base_its$ANO_CMPT[base_its$intervencao == 0], base_its$MES_CMPT[base_its$intervencao == 0])
chave_pos <- paste(base_its$ANO_CMPT[base_its$intervencao == 1], base_its$MES_CMPT[base_its$intervencao == 1])

internacoes_pre <- sih_santa_monica %>% filter(paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pre) %>% nrow()
internacoes_pos <- sih_santa_monica %>% filter(paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pos) %>% nrow()
dheg_pre <- sih_santa_monica %>% filter(caso_dheg == 1L, paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pre) %>% nrow()
dheg_pos <- sih_santa_monica %>% filter(caso_dheg == 1L, paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pos) %>% nrow()

taxa_geral <- round(n_casos_dheg / n_total_internacoes * 1000, 1)
taxa_pre   <- round(dheg_pre / internacoes_pre * 1000, 1)
taxa_pos   <- round(dheg_pos / internacoes_pos * 1000, 1)


# ── 10.5 Table 1 — Demographic and Clinical Characteristics ───────────────────
tab1_dados <- sih_santa_monica %>%
  filter(caso_dheg == 1L) %>%
  mutate(
    periodo = case_when(
      paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pre ~ "Pre-intervention",
      paste(as.integer(ANO_CMPT), as.integer(MES_CMPT)) %in% chave_pos ~ "Post-intervention",
      TRUE ~ NA_character_
    ),
    eclampsia_bin = if_else(grepl("^O15", DIAG_PRINC), 1L, 0L),
    uti_bin       = if_else(!is.na(MARCA_UTI) & MARCA_UTI != "" & MARCA_UTI != "0" & MARCA_UTI != "Sem UTI", 1L, 0L),
    idade_num     = suppressWarnings(as.numeric(IDADE)),
    dias_num      = suppressWarnings(as.numeric(DIAS_PERM))
  )

resumo_tab1 <- function(df, per) {
  d <- if (per == "Total") df else filter(df, periodo == per)
  n <- nrow(d)
  tibble(
    n_casos       = n,
    idade_med     = round(median(d$idade_num, na.rm = TRUE), 1),
    idade_iqr_lo  = round(quantile(d$idade_num, 0.25, na.rm = TRUE), 1),
    idade_iqr_hi  = round(quantile(d$idade_num, 0.75, na.rm = TRUE), 1),
    dias_med      = round(median(d$dias_num,  na.rm = TRUE), 1),
    dias_iqr_lo   = round(quantile(d$dias_num, 0.25, na.rm = TRUE), 1),
    dias_iqr_hi   = round(quantile(d$dias_num, 0.75, na.rm = TRUE), 1),
    n_uti         = sum(d$uti_bin, na.rm = TRUE),
    pct_uti       = round(sum(d$uti_bin, na.rm = TRUE) / n * 100, 1),
    n_obito       = sum(d$obito, na.rm = TRUE),
    pct_obito     = round(sum(d$obito, na.rm = TRUE) / n * 100, 1),
    n_eclampsias  = sum(d$eclampsia_bin, na.rm = TRUE),
    pct_eclampsia = round(sum(d$eclampsia_bin, na.rm = TRUE) / n * 100, 1),
    n_alto_risco  = sum(d$alto_risco, na.rm = TRUE),
    pct_alto_risco = round(sum(d$alto_risco, na.rm = TRUE) / n * 100, 1)
  )
}

tab1_pre   <- resumo_tab1(tab1_dados, "Pre-intervention")
tab1_pos   <- resumo_tab1(tab1_dados, "Post-intervention")
tab1_total <- resumo_tab1(tab1_dados, "Total")

tab1_pval_idade <- wilcox.test(idade_num ~ periodo, data = filter(tab1_dados, periodo %in% c("Pre-intervention", "Post-intervention")), exact = FALSE)$p.value
tab1_pval_dias  <- wilcox.test(dias_num ~ periodo, data = filter(tab1_dados, periodo %in% c("Pre-intervention", "Post-intervention")), exact = FALSE)$p.value

safe_fisher <- function(var) {
  d <- filter(tab1_dados, periodo %in% c("Pre-intervention", "Post-intervention"))
  tab <- table(d$periodo, d[[var]])
  if (nrow(tab) >= 2 && ncol(tab) >= 2) return(fisher.test(tab)$p.value) else return(NA_real_)
}

tab1_pval_uti      <- safe_fisher("uti_bin")
tab1_pval_obito    <- safe_fisher("obito")
tab1_pval_eclamp   <- safe_fisher("eclampsia_bin")
tab1_pval_alto     <- safe_fisher("alto_risco")

tabela_descritiva <- tibble(
  Characteristic = c(
    "HDP cases, n", "Maternal age, median [IQR], years", "Length of hospital stay, median [IQR], days",
    "ICU admission, n (%)", "Maternal death, n (%)", "Eclampsia (ICD-10 O15), n (%)", "High-risk pregnancy, n (%)"
  ),
  Total = lancet_dec(c(
    as.character(tab1_total$n_casos), sprintf("%.1f [%.1f–%.1f]", tab1_total$idade_med, tab1_total$idade_iqr_lo, tab1_total$idade_iqr_hi),
    sprintf("%.1f [%.1f–%.1f]", tab1_total$dias_med, tab1_total$dias_iqr_lo, tab1_total$dias_iqr_hi), sprintf("%d (%.1f%%)", tab1_total$n_uti, tab1_total$pct_uti),
    sprintf("%d (%.1f%%)", tab1_total$n_obito, tab1_total$pct_obito), sprintf("%d (%.1f%%)", tab1_total$n_eclampsias, tab1_total$pct_eclampsia),
    sprintf("%d (%.1f%%)", tab1_total$n_alto_risco, tab1_total$pct_alto_risco)
  )),
  `Pre-intervention` = lancet_dec(c(
    as.character(tab1_pre$n_casos), sprintf("%.1f [%.1f–%.1f]", tab1_pre$idade_med, tab1_pre$idade_iqr_lo, tab1_pre$idade_iqr_hi),
    sprintf("%.1f [%.1f–%.1f]", tab1_pre$dias_med, tab1_pre$dias_iqr_lo, tab1_pre$dias_iqr_hi), sprintf("%d (%.1f%%)", tab1_pre$n_uti, tab1_pre$pct_uti),
    sprintf("%d (%.1f%%)", tab1_pre$n_obito, tab1_pre$pct_obito), sprintf("%d (%.1f%%)", tab1_pre$n_eclampsias, tab1_pre$pct_eclampsia),
    sprintf("%d (%.1f%%)", tab1_pre$n_alto_risco, tab1_pre$pct_alto_risco)
  )),
  `Post-intervention` = lancet_dec(c(
    as.character(tab1_pos$n_casos), sprintf("%.1f [%.1f–%.1f]", tab1_pos$idade_med, tab1_pos$idade_iqr_lo, tab1_pos$idade_iqr_hi),
    sprintf("%.1f [%.1f–%.1f]", tab1_pos$dias_med, tab1_pos$dias_iqr_lo, tab1_pos$dias_iqr_hi), sprintf("%d (%.1f%%)", tab1_pos$n_uti, tab1_pos$pct_uti),
    sprintf("%d (%.1f%%)", tab1_pos$n_obito, tab1_pos$pct_obito), sprintf("%d (%.1f%%)", tab1_pos$n_eclampsias, tab1_pos$pct_eclampsia),
    sprintf("%d (%.1f%%)", tab1_pos$n_alto_risco, tab1_pos$pct_alto_risco)
  )),
  `p-value` = c(
    "—", fmt_pval_lancet(tab1_pval_idade), fmt_pval_lancet(tab1_pval_dias), fmt_pval_lancet(tab1_pval_uti),
    fmt_pval_lancet(tab1_pval_obito), fmt_pval_lancet(tab1_pval_eclamp), fmt_pval_lancet(tab1_pval_alto)
  )
)


# ── 11. Figures (Lancet Standard with Midline Decimals & Panel Letters) ────────
Sys.setlocale("LC_TIME", "C")

tema_revista <- theme_minimal(base_size = 11, base_family = "Times New Roman") +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.2, "cm"),
    legend.box.margin = margin(t = 5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray93", linewidth = 0.4),
    axis.title.y = element_text(size = 11, margin = margin(r = 15)), 
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    plot.margin = margin(t = 15, r = 20, b = 5, l = 5)
  )

# 11.1 Interrupted Time Series Plot
junho_harm <- list(
  mes_num = 6L,
  sin1 = sin(2 * pi * 1 * 6 / 12), cos1 = cos(2 * pi * 1 * 6 / 12),
  sin2 = sin(2 * pi * 2 * 6 / 12), cos2 = cos(2 * pi * 2 * 6 / 12)
)

pred_ajustado <- predict(modelo_freq, newdata = base_its %>% mutate(!!!junho_harm), type = "response") / base_its$total_internacoes * 1000
pred_contrafactual <- predict(modelo_freq, newdata = base_its %>% mutate(intervencao = 0L, tempo_pos = 0L, !!!junho_harm), type = "response") / base_its$total_internacoes * 1000

pred_df <- base_its %>%
  mutate(taxa = casos_dheg / total_internacoes * 1000, pred_ajustado = pred_ajustado, pred_contrafactual = pred_contrafactual)

breaks_custom <- c(1, mes_inicio_pol, mes_corte_base, max(base_its$tempo))
labels_custom <- format(ymd("2023-02-01") %m+% months(breaks_custom - 1), "%b/%y")

g_its <- pred_df %>%
  ggplot(aes(x = tempo)) +
  annotate("rect", xmin = mes_corte_base - 0.5, xmax = max(base_its$tempo) + 0.5, ymin = -Inf, ymax = Inf, fill = "#FFF3CD", alpha = 0.5) +
  annotate("text", x = 12, y = 370, label = "Pre-intervention", family = "Times New Roman", size = 3.8, fontface = "italic", color = "gray30") +
  annotate("text", x = 28, y = 370, label = "Transition", family = "Times New Roman", size = 3.3, fontface = "italic", color = "gray40") +
  annotate("text", x = 34, y = 370, label = "Post-intervention", family = "Times New Roman", size = 3.8, fontface = "italic", color = "gray30") +
  geom_point(aes(y = taxa), color = "#0B2073", size = 2) +
  geom_line(aes(y = taxa), color = "#0B2073", alpha = 0.4, linewidth = 0.6) +
  geom_line(aes(y = pred_ajustado, linetype = "Adjusted (deseasonalized)"), color = "firebrick", linewidth = 1) +
  geom_line(aes(y = pred_contrafactual, linetype = "Counterfactual"), color = "firebrick", linewidth = 1) +
  geom_vline(xintercept = mes_inicio_pol - 0.5, linetype = "dotted", color = "gray30", linewidth = 0.7) +
  geom_vline(xintercept = mes_corte_base - 0.5, linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  scale_linetype_manual(values = c("Adjusted (deseasonalized)" = "solid", "Counterfactual" = "dashed")) +
  scale_x_continuous(breaks = breaks_custom, labels = labels_custom) +
  labs(x = "Month", y = "Incidence rate\n(per 1,000 obstetric admissions)") +
  guides(linetype = guide_legend(nrow = 1, byrow = TRUE)) +
  coord_cartesian(ylim = c(100, 380), clip = "off") + 
  tema_revista

salvar_grafico(g_its, "grafico_its_santa_monica", width_mm = 190, height_mm = 110)

# 11.2 Residual Diagnostics Plot (Aplicando midline decimal E os painéis a, b, c)
salvar_plot_base(
  function() {
    op <- options(OutDec = "·") 
    # Aumentei a margem superior (t = 2.5) para as letras a, b, c não cortarem
    par(mfrow = c(1, 3), family = "serif", ps = 10, bty = "l", 
        mar = c(4, 4.5, 2.5, 1), mgp = c(2.5, 0.7, 0))
    
    # Painel a
    plot(base_its$tempo, res_m1, ylim = c(-4, 4), pch = 21, bg = "gray60", col = "white", 
         cex = 1.4, lwd = 1, ylab = "Deviance residuals", xlab = "Month")
    abline(h = 0, lty = 2, col = "gray40", lwd = 1.5)
    mtext("a", side = 3, line = 0.5, adj = 0, cex = 1.5, font = 2)
    
    # Painel b
    acf (res_m1, main = "", ylab = "ACF", xlab = "Lag")
    mtext("b", side = 3, line = 0.5, adj = 0, cex = 1.5, font = 2)
    
    # Painel c
    pacf(res_m1, main = "", ylab = "Partial ACF", xlab = "Lag")
    mtext("c", side = 3, line = 0.5, adj = 0, cex = 1.5, font = 2)
    
    par(mfrow = c(1, 1))
    options(op) 
  },
  "diagnostico_residuos_m1", width_mm = 190, height_mm = 65
)

# 11.3 Posterior Predictive Check Plot (Correção para HDP Cases)
pp <- pp_check(modelo_bayes, ndraws = 100) + 
  labs(title = NULL, subtitle = NULL, x = "HDP Cases", y = "Posterior density") + 
  scale_y_continuous(labels = function(x) lancet_dec(format(x, nsmall = 2, trim = TRUE))) +
  tema_revista
salvar_grafico(pp, "pp_check_m2", width_mm = 190, height_mm = 90)

# 11.4 Sensitivity Forest Plot
g_sens <- sens_df %>%
  mutate(
    lag_label = paste0("Lag ", lag, "m\n(month ", mes_inicio_pol + lag, ")"),
    sig       = if_else(p_valor < 0.05, "p < 0·05", "p \u2265 0·05") 
  ) %>%
  ggplot(aes(x = irr, y = reorder(lag_label, -lag), color = sig, xmin = ci_lo, xmax = ci_hi)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray75", linewidth = 0.8) + 
  geom_errorbarh(height = 0.12, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("IRR=%s (p=%s)", fmt_num(irr, 2), sapply(p_valor, fmt_pval_lancet))), 
            nudge_y = 0.32, size = 3.5, color = "black", family = "Times New Roman") +
  scale_color_manual(values = c("p < 0·05" = "firebrick", "p \u2265 0·05" = "steelblue")) +
  scale_x_continuous(limits = c(0.3, 2.0), expand = expansion(mult = c(0.05, 0.05)),
                     labels = function(x) lancet_dec(format(x, nsmall = 1, trim = TRUE))) + 
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.15))) +
  labs(x = "IRR with 95% CI", y = NULL) +
  tema_revista +
  theme(legend.position = "none")

salvar_grafico(g_sens, "sensibilidade_lag_biologico", width_mm = 190, height_mm = 95)
Sys.setlocale("LC_TIME", "")


# ── 12. Documento DOCX — Material Suplementar ─────────────────────────────────
doc <- read_docx() %>%
  body_add_par("Supplementary Material", style = "heading 1") %>%
  body_add_par("HDP — Maternidade Santa Mônica (CNES 2009250) — AL", style = "heading 2") %>%
  body_add_par(paste("Generated on:", format(Sys.Date(), "%d/%m/%Y")), style = "Normal") %>%
  body_add_par("", style = "Normal")

doc <- doc_add_console(doc, cat(sprintf("Time series             : %d months\n", n_meses), sprintf("  Pre-intervention      : %d months\n", n_meses_pre), sprintf("  Post-intervention     : %d months\n", n_meses_pos), sprintf("Obstetric admissions    : %d\n", n_total_internacoes), sprintf("  Pre-intervention      : %d\n", internacoes_pre), sprintf("  Post-intervention     : %d\n", internacoes_pos), sprintf("HDP Cases (O14-O15)     : %d\n", n_casos_dheg), sprintf("  Pre-intervention      : %d  (rate: %s per 1,000)\n", dheg_pre, lancet_dec(taxa_pre)), sprintf("  Post-intervention     : %d  (rate: %s per 1,000)\n", dheg_pos, lancet_dec(taxa_pos)), sprintf("Overall incidence       : %s per 1,000 admissions\n", lancet_dec(taxa_geral)), sprintf("Maternal deaths (HDP)   : %d\n", n_obitos), sep = ""), titulo = "S1. Sample Size")
doc <- doc_add_console(doc, print(as.data.frame(base_its[, c("ANO_CMPT", "MES_CMPT", "tempo", "total_internacoes", "casos_dheg", "obitos_maternos", "intervencao", "tempo_pos")])), titulo = "S2. Monthly Dataset (ITS)")
doc <- doc_add_console(doc, print(ftest_slope), titulo = "S3. F-Test — Need for slope change (tempo_pos)")
doc <- doc_add_console(doc, summary(modelo_freq), titulo = "S4. M1 — Quasipoisson ITS: Full Summary")
doc <- doc_add_console(doc, print(data.frame(Parameter = names(irr_m1), IRR = fmt_num(irr_m1, 4), CI_low = fmt_num(ci_m1[, 1], 4), CI_high = fmt_num(ci_m1[, 2], 4))), titulo = "S5. M1 — IRR and 95% CI (Wald)")
doc <- doc_add_console(doc, { print(ljung_box_6); print(ljung_box_12) }, titulo = "S6. Ljung-Box Test — Residual Independence (M1, lags 6 and 12)")
doc <- doc_add_console(doc, print(sumario_bayes), titulo = "S7. M2 — Bayesian Poisson: Posterior Summary")
doc <- doc_add_console(doc, prior_summary(modelo_bayes), titulo = "S7.5. M2 — Summary of Priors Used (brms)")
doc <- doc_add_console(doc, cat(sprintf("Total divergent transitions: %d\n", divergencias)), titulo = "S7.6. M2 — MCMC Divergence Diagnostics")
doc <- doc_add_console(doc, print(data.frame(Parameter = rownames(fixef_bayes), IRR = fmt_num(irr_m2, 4), CrI_low = fmt_num(ci_m2_lo, 4), CrI_high = fmt_num(ci_m2_hi, 4), Rhat = fmt_num(fixef_bayes[, "Rhat"], 4))), titulo = "S8. M2 — Bayesian Poisson: IRR and 95% Credible Intervals")
doc <- doc_add_console(doc, { cat("── Total distribution PE vs Eclampsia ──\n"); print(split_total); cat("\n── Descriptive table by window (7 months) ──\n"); print(as.data.frame(tabela_cid)); cat("\n── Binomial ITS — Proportion of Eclampsia ──\n"); print(summary(modelo_subtipo_its)); cat("\n── OR (exp(coef)) ──\n"); print(data.frame(Parameter = names(or_subtipo), OR = fmt_num(or_subtipo, 4), CI_low = fmt_num(ci_subtipo[, 1], 4), CI_high = fmt_num(ci_subtipo[, 2], 4))) }, titulo = "S9. HDP Subtypes: Binomial ITS — Proportion of Eclampsia")

for (g in list(
  list(nm = "grafico_its_santa_monica",    w = 19.0, h = 11.0),
  list(nm = "diagnostico_residuos_m1",     w = 19.0, h =  6.5),
  list(nm = "pp_check_m2",                 w = 19.0, h =  9.0),
  list(nm = "sensibilidade_lag_biologico", w = 19.0, h =  9.5)
)) {
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_img(doc, src = file.path(dir_resultados, paste0(g$nm, ".png")), width = g$w / 2.54, height = g$h / 2.54)
}

print(doc, target = file.path(dir_resultados, "resultados.docx"))


# ── 13. Tables — The Lancet Standard ──────────────────────────────────────────
lancet_ft <- function(df) {
  nc <- ncol(df)
  flextable(df) %>%
    border_remove() %>%
    hline_top   (border = fp_border(color = "black", width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 1.0), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 1.5), part = "body")   %>%
    font    (fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 8, part = "all") %>% 
    bold    (bold = TRUE, part = "header") %>% 
    align   (align = "center", part = "header") %>%
    align   (align = "left",   part = "body", j = 1) %>%
    align   (align = "center", part = "body", j = 2:nc) %>%
    line_spacing(space = 1, part = "all") %>% 
    padding (padding = 2, part = "all") %>%
    autofit() %>%
    set_table_properties(layout = "autofit")
}

lancet_nota <- function(ft, nota) {
  ft %>%
    add_footer_lines(nota) %>%
    font    (fontname = "Times New Roman", part = "footer") %>%
    fontsize(size = 10, part = "footer") %>% 
    align   (align = "left", part = "footer") %>%
    line_spacing(space = 1, part = "footer")
}

inserir_tabela_lancet <- function(doc, ft, title_text) {
  prop_txt <- fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)
  prop_par <- fp_par(padding.bottom = 4)
  doc %>%
    body_add_fpar(fpar(ftext(title_text, prop = prop_txt), fp_p = prop_par)) %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")
}

termos_param <- c(
  "Intercept", "Time trend", "Intervention (level)", "Post-intervention \u00D7 Time (slope)", 
  "Harmonic sin(1)", "Harmonic cos(1)", "Harmonic sin(2)", "Harmonic cos(2)"
)

# ── Table 1 ────────────────────────────────────────────────────────
ft_tabela_desc <- lancet_ft(tabela_descritiva) %>%
  lancet_nota(paste0(
    "Continuous variables are presented as median [IQR]. ",
    "ICU = Intensive Care Unit. High-risk pregnancy defined by SIH-RD codes. ",
    "p-values calculated via Mann-Whitney U test (continuous) or Fisher's Exact test (categorical). ",
    "Pre-intervention: Jul/2024–Jan/2025; Post-intervention: Aug/2025–Feb/2026."
  ))

# ── Table 2 ────────────────────────────────────────────────────────
coef_m1 <- summary(modelo_freq)$coefficients
tab2_its <- data.frame(
  `Parameter`      = termos_param,
  `IRR`            = fmt_num(irr_m1, 2), 
  `Lower 95% CI`   = fmt_num(ci_m1[, 1], 2), 
  `Upper 95% CI`   = fmt_num(ci_m1[, 2], 2),
  `p-value`        = sapply(coef_m1[, "Pr(>|t|)"], fmt_pval_lancet), check.names = FALSE
)

ft_tab2 <- lancet_ft(tab2_its) %>%
  lancet_nota(paste0(
    "IRR = Incidence Rate Ratio; CI = Confidence Interval. ",
    "Quasipoisson ITS model with harmonic seasonality adjustment. Dispersion parameter (\u03C6) = ", 
    fmt_num(phi_m1, 2), ". Ljung-Box test (lag 6): Q = ", fmt_num(ljung_box_6$statistic, 2), 
    ", p = ", fmt_pval_lancet(ljung_box_6$p.value), "."
  ))

# ── Table 3 ────────────────────────────────────────────────────────
tab3_bayes <- data.frame(
  `Parameter`      = termos_param, 
  `IRR`            = fmt_num(irr_m2, 2), 
  `Lower 95% CrI`  = fmt_num(ci_m2_lo, 2), 
  `Upper 95% CrI`  = fmt_num(ci_m2_hi, 2),
  `Rhat`           = fmt_num(fixef_bayes[, "Rhat"], 2), check.names = FALSE
)

ft_tab3 <- lancet_ft(tab3_bayes) %>%
  lancet_nota(paste0(
    "IRR = Incidence Rate Ratio; CrI = Credible Interval. ",
    "Bayesian Poisson ITS model (CmdStanR). Rhat < 1·01 indicates convergence. ",
    "Divergent transitions: ", divergencias, "."
  ))

# ── Table 4 ────────────────────────────────────────────────────────
coef_sub <- summary(modelo_subtipo_its)$coefficients
tab4_sub <- data.frame(
  `Parameter`      = termos_param,
  `OR`             = fmt_num(or_subtipo, 2), 
  `Lower 95% CI`   = fmt_num(ci_subtipo[, 1], 2), 
  `Upper 95% CI`   = fmt_num(ci_subtipo[, 2], 2),
  `p-value`        = sapply(coef_sub[, "Pr(>|z|)"], fmt_pval_lancet), check.names = FALSE
)

ft_tab4 <- lancet_ft(tab4_sub) %>%
  lancet_nota(paste0(
    "OR = Odds Ratio; CI = Confidence Interval. ",
    "Binomial ITS model assessing the proportion of Eclampsia (ICD-10 O15) relative to total HDP cases. ",
    "OR < 1·0 indicates reduced odds of progression to eclampsia."
  ))

# ── Table 5 ────────────────────────────────────────────────────────
tab5_sens <- data.frame(
  `Biological lag` = paste0(sens_df$lag, " months"), 
  `Cut-off month`  = mes_inicio_pol + sens_df$lag,
  `IRR`            = fmt_num(sens_df$irr, 2), 
  `Lower 95% CI`   = fmt_num(sens_df$ci_lo, 2), 
  `Upper 95% CI`   = fmt_num(sens_df$ci_hi, 2),
  `p-value`        = sapply(sens_df$p_valor, fmt_pval_lancet), check.names = FALSE
)

ft_tab5 <- lancet_ft(tab5_sens) %>%
  lancet_nota("Sensitivity analysis of the biological lag using Quasipoisson ITS models.")

# ── Compilação Final DOCX ──────────────────────────────────────────
doc_lancet <- read_docx() %>%
  body_add_par("Tables — The Lancet Standard", style = "heading 1") %>%
  body_add_par("10pt Bold Titles | 8pt Times New Roman Tables | Single Spaced", style = "Normal") %>%
  body_add_par("", style = "Normal")

doc_lancet <- inserir_tabela_lancet(doc_lancet, ft_tabela_desc, "Table 1: Demographic and clinical characteristics of patients with HDP according to intervention period")
doc_lancet <- inserir_tabela_lancet(doc_lancet, ft_tab2, "Table 2: Quasipoisson ITS model coefficients for the impact of calcium supplementation on HDP incidence")
doc_lancet <- inserir_tabela_lancet(doc_lancet, ft_tab3, "Table 3: Bayesian Poisson ITS model posterior estimates for the impact of calcium supplementation on HDP incidence")
doc_lancet <- inserir_tabela_lancet(doc_lancet, ft_tab4, "Table 4: Binomial ITS model for the proportion of Eclampsia (ICD-10 O15) among total HDP cases")
doc_lancet <- inserir_tabela_lancet(doc_lancet, ft_tab5, "Table 5: Sensitivity analysis assuming different biological lags for the intervention effect")

print(doc_lancet, target = file.path(dir_resultados, "tables_lancet.docx"))