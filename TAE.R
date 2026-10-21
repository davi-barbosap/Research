if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(meta, googlesheets4, ragg, officer)

# ── Setup ──────────────────────────────────────────────────────────────────────

url        <- "https://docs.google.com/spreadsheets/d/1lnhxPp8FhuKckQ81w6SSwUhMevjVsEm5fIwHL54iVWc/edit?gid=905187297#gid=905187297"   # <── Link do Google Sheets
aba        <- "Pre_Post_Patients"
output_dir <- "/Users/davibarbosa/Desktop/TAE"   # <── Pasta raiz

followups  <- c("1m", "3m", "6m", "12m")

# Exclusões da análise principal, por tempo de seguimento
# Gandhi 12m: apenas 2 pés atingiram 12 meses; demais valores obtidos por telefone
# antes de completar 12 meses (Tabela 2, coluna "Follow-up duration")
exclusoes <- list(
  "12m" = c("Gandhi, 2024")
)

# Sensibilidade por exclusão de estudo (r = 0.5), por tempo de seguimento
# Gandhi 6m: perda de seguimento (6/10 pacientes) + SD pós = 0
sens_exclusao <- list(
  "6m"  = c("Gandhi, 2024")
)

# Abaixo deste k, o tempo é apresentado só de forma descritiva (sem estimativa combinada)
k_min <- 3

# ── Cochrane SD change score ───────────────────────────────────────────────────
# SD_change = sqrt(SD_pre² + SD_pos² - 2 × r × SD_pre × SD_pos)

sd_change <- function(sd_pre, sd_pos, corr) {
  sqrt(sd_pre^2 + sd_pos^2 - 2 * corr * sd_pre * sd_pos)
}

correlacoes <- c("05" = 0.5, "03" = 0.3, "07" = 0.7)

# ── Tema visual ────────────────────────────────────────────────────────────────

.theme <- list(
  col.square        = "blue",
  col.square.lines  = "black",
  col.diamond       = "black",
  col.diamond.lines = "black",
  col.predict       = "red",
  col.predict.lines = "red",
  fontsize          = 10,
  ff.study          = "plain",
  ff.heading        = "bold"
)

# ── Helpers ────────────────────────────────────────────────────────────────────

save_plot <- function(filename_base, plot_fn, out_dir, width = 10, height = 3.5, res = 300) {
  agg_tiff(
    file.path(out_dir, paste0(filename_base, ".tiff")),
    width = width, height = height, units = "in", res = res,
    compression = "lzw"
  )
  plot_fn()
  dev.off()
  message("Salvo: ", filename_base, ".tiff")
  
  agg_png(
    file.path(out_dir, paste0(filename_base, ".png")),
    width = width, height = height, units = "in", res = res
  )
  plot_fn()
  dev.off()
  message("Salvo: ", filename_base, ".png")
}

save_output_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 9)
  doc  <- officer::read_docx()
  for (line in lines) {
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(line, prop)))
  }
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
  message("Salvo: ", filename_base, ".docx")
}

run_metamean <- function(data, sd_col) {
  pooled <- nrow(data) >= k_min
  metamean(
    n        = data$n.e,
    mean     = data$mean_change,
    sd       = data[[sd_col]],
    data     = data,
    sm       = "MRAW",
    null.effect = 0,   # sem isso o metamean não testa a mudança contra zero (p = NA)
    studlab  = data$studlab,
    random   = pooled,
    common   = FALSE,
    method.tau       = "REML",
    method.random.ci = "HK",
    prediction       = pooled
  )
}

plot_forest <- function(mc, filename_base, out_dir) {
  pooled <- mc$random
  cols <- c("studlab", "mean", "sd", "n", "w.random", "effect", "ci")
  labs <- c("Study", "Mean change", "SD change", "Total", "Weight", "MD", "95% CI")
  if (!pooled) {
    keep <- cols != "w.random"
    cols <- cols[keep]
    labs <- labs[keep]
  }
  save_plot(filename_base, function() {
    do.call(meta::forest, c(
      list(
        x         = mc,
        layout    = "Revman5",
        smlab     = if (pooled) "MD\nIV, Random, 95% CI" else "MD\n95% CI",
        leftcols  = cols,
        leftlabs  = labs,
        colgap    = "4mm",
        colgap.forest.left  = "5mm",
        pooled.totals       = TRUE,
        digits    = 2,
        digits.sd = 2,
        just      = "center",
        sortvar   = mc$TE,
        overall   = pooled,
        overall.hetstat     = pooled,
        test.overall.random = pooled
      ),
      .theme
    ))
  }, out_dir)
}

prep_data <- function(raw) {
  df <- as.data.frame(raw)
  df <- df[!is.na(df$study) & df$study != "", ]
  num_cols <- c("n.e", "mean_pre", "sd_pre", "mean_post", "sd_post")
  df[num_cols]  <- lapply(df[num_cols], function(x) as.numeric(as.character(x)))
  df$follow_up  <- as.character(df$follow_up)
  df$studlab    <- paste0(df$study, ", ", df$year)
  df$mean_change <- df$mean_post - df$mean_pre
  for (k in names(correlacoes)) {
    df[[paste0("sd_change_", k)]] <- sd_change(df$sd_pre, df$sd_post, corr = correlacoes[[k]])
  }
  df
}

# Linha-resumo do modelo para a tabela consolidada
resumo_modelo <- function(mc, fu, analise) {
  if (!mc$random) {
    return(data.frame(follow_up = fu, analise = paste0(analise, " (descritivo, k < ", k_min, ")"),
                      k = mc$k, n_total = sum(mc$n),
                      MD = NA, IC95_inf = NA, IC95_sup = NA, p = NA,
                      PI_inf = NA, PI_sup = NA, I2 = NA, tau2 = NA))
  }
  data.frame(
    follow_up = fu,
    analise   = analise,
    k         = mc$k,
    n_total   = sum(mc$n),
    MD        = round(mc$TE.random, 2),
    IC95_inf  = round(mc$lower.random, 2),
    IC95_sup  = round(mc$upper.random, 2),
    p         = signif(mc$pval.random, 3),
    PI_inf    = round(mc$lower.predict, 2),
    PI_sup    = round(mc$upper.predict, 2),
    I2        = round(100 * mc$I2, 1),
    tau2      = round(mc$tau2, 3)
  )
}

# ── Main ───────────────────────────────────────────────────────────────────────

run_followup <- function(dados, fu, base_dir = output_dir) {
  out_dir <- file.path(base_dir, fu)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("\n══ Iniciando: ", fu, " ══")
  
  df  <- dados[dados$follow_up == fu, ]
  if (!is.null(exclusoes[[fu]])) {
    df <- subset(df, !(studlab %in% exclusoes[[fu]]))
    message("Excluído(s) da análise principal: ", paste(exclusoes[[fu]], collapse = "; "))
  }
  res <- list()
  
  # 01 — Principal (r = 0.5) | 02/03 — Sensibilidade (r = 0.3 / r = 0.7)
  rotulos <- c("05" = "01_forest_principal_r05",
               "03" = "02_forest_sensibilidade_r03",
               "07" = "03_forest_sensibilidade_r07")
  for (k in names(rotulos)) {
    mc <- run_metamean(df, paste0("sd_change_", k))
    plot_forest(mc, rotulos[[k]], out_dir)
    save_output_docx(paste0("output_", rotulos[[k]]), capture.output(summary(mc)), out_dir)
    res[[length(res) + 1]] <- resumo_modelo(mc, fu, paste0("r = ", correlacoes[[k]]))
  }
  
  # 04 — Sensibilidade por exclusão (r = 0.5)
  excl <- sens_exclusao[[fu]]
  if (!is.null(excl)) {
    df_sub <- subset(df, !(studlab %in% excl))
    mc_sub <- run_metamean(df_sub, "sd_change_05")
    plot_forest(mc_sub, "04_forest_sensibilidade_exclusao", out_dir)
    save_output_docx("output_04_forest_sensibilidade_exclusao",
                     c(paste0("Excluído(s): ", paste(excl, collapse = "; ")), "",
                       capture.output(summary(mc_sub))),
                     out_dir)
    res[[length(res) + 1]] <- resumo_modelo(mc_sub, fu,
                                            paste0("r = 0.5, sem ", paste(excl, collapse = "; ")))
  }
  
  message("══ Concluído: ", fu, " ══\n")
  do.call(rbind, res)
}

# ══════════════════════════════════════════════════════════════════════════════
# Execução
# ══════════════════════════════════════════════════════════════════════════════

dados <- prep_data(read_sheet(url, sheet = aba))

resumo <- do.call(rbind, lapply(followups, function(fu) run_followup(dados, fu)))

write.csv(resumo, file.path(output_dir, "resumo_modelos.csv"), row.names = FALSE)
save_output_docx("resumo_modelos", capture.output(print(resumo, row.names = FALSE)), output_dir)
print(resumo, row.names = FALSE)
