if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(meta, dmetar, googlesheets4, ragg, officer, ggplot2, ggrepel)

# ── Setup ──────────────────────────────────────────────────────────────────────
# Projeto NEURO — PSP (paralisia supranuclear progressiva): terapia anti-tau vs placebo
url        <- "https://docs.google.com/spreadsheets/d/1yHZv3lmak90JrDbVYJ9L4lC1rUeo0eRDOEMSq-3mMc0/edit?gid=968337662#gid=968337662"
output_dir <- "/Users/davibarbosa/Desktop/PSP"   # <- ajuste se quiser outra pasta de saída

# Desativa a autenticação para ler links públicos sem pedir login
gs4_deauth()

# Lê os nomes das abas DIRETO da planilha (evita erro de transcrição:
# marcadores (+)/(-), en-dash, espaços à esquerda, etc.)
all_sheets <- googlesheets4::sheet_names(url)

cat("Abas encontradas na planilha:\n")
print(all_sheets)

# ── Regra de IC do efeito aleatório (fluxograma "Meta-Analysis Academy") ─────────
#   tau² = 0            -> Wald (classic)
#   tau² > 0 & k > 2    -> HKSJ (Hartung-Knapp-Sidik-Jonkman)
#   tau² > 0 & k <= 2   -> decisão (default HKSJ, mais conservador). Troque em CI_TIE_BREAK.
# Mantém REML para tau² (menos viesado, recomendado com < 10 estudos).
CI_TIE_BREAK <- "HK"   # "HK" (conservador) ou "classic" (Wald) quando tau²>0 & k<=2

apply_ci_rule <- function(ma0, tie = CI_TIE_BREAK) {
  k    <- ma0$k
  tau2 <- ma0$tau2
  method <- if (isTRUE(tau2 > 0)) { if (k > 2) "HK" else tie } else "classic"

  ci <- if (method == "HK") {
    list(method.random.ci = "HK",      adhoc.hakn.ci = "se",
         method.predict   = "HK",      adhoc.hakn.pi = "se")
  } else {
    list(method.random.ci = "classic", method.predict = "V")   # Wald + PI clássico (HTS)
  }
  message(sprintf("  Regra IC -> tau2=%.4g, k=%d  =>  %s",
                  ifelse(is.na(tau2), NA_real_, tau2), k,
                  ifelse(method == "HK", "HKSJ", "Wald")))
  do.call(update, c(list(object = ma0), ci))
}

# Intervalo de predição só com >= 3 estudos (instável/enganoso com 2)
use_prediction <- function(df) nrow(df) >= 3

# Argumentos comuns a todos os modelos (tau² por REML; IC definido depois pela regra)
.re_args <- list(
  method.tau = "REML",
  method.I2  = "tau2",
  random     = TRUE,
  common     = FALSE
)

# ── Visual theme ───────────────────────────────────────────────────────────────
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
save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10, res = 300) {
  agg_tiff(file.path(out_dir, paste0(filename_base, ".tiff")),
           width = width, height = height, units = "in", res = res, compression = "lzw")
  plot_fn(); dev.off()
  message("Saved: ", filename_base, ".tiff")

  agg_png(file.path(out_dir, paste0(filename_base, ".png")),
          width = width, height = height, units = "in", res = res)
  plot_fn(); dev.off()
  message("Saved: ", filename_base, ".png")
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 9)
  doc  <- officer::read_docx()
  for (line in lines)
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(line, prop)))
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
  message("Saved: ", filename_base, ".docx")
}

# ── Column definitions ─────────────────────────────────────────────────────────
.cont_cols <- list(
  leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
  leftlabs = c("Studies", "Mean", "SD", "Total", "Mean", "SD", "Total", "Weight", "MD", "95% CI")
)

.bin_cols <- list(
  leftcols        = c("studlab", "event.e", "n.e", "event.c", "n.c", "w.random", "effect", "ci"),
  leftlabs        = c("Studies", "Events", "Total", "Events", "Total", "Weight", "RR", "95% CI"),
  ff.lr           = "bold",
  col.label.left  = "black",
  col.label.right = "black"
)

# Rótulos direcionais do forest. Fixos, EXCETO onde maior = melhor (SEADL e MRI),
# onde a interpretação inverte (efeito benéfico -> MD positiva -> lado direito).
.invert_label_sheets <- "SEADL|MRI"   # regex (case-insensitive) das abas a inverter
forest_labels <- function(sheet) {
  if (grepl(.invert_label_sheets, sheet, ignore.case = TRUE)) {
    list(label.left = "Favours placebo",           label.right = "Favours tau-target therapy")
  } else {
    list(label.left = "Favours tau-target therapy", label.right = "Favours placebo")
  }
}

# Base do forest — grupos: terapia anti-tau vs placebo.
# Rótulos direcionais (label.left/right) vêm de forest_labels(sheet).
.forest_base <- list(
  layout              = "RevMan5",
  rightcols           = FALSE,
  just.addcols        = "right",
  pooled.events       = TRUE,
  pooled.totals       = TRUE,
  digits              = 2,
  digits.pval         = 3,
  digits.sd           = 2,
  label.e             = "Tau-target therapy",
  label.c             = "Placebo",
  colgap              = "6mm",
  colgap.forest       = "6mm",
  print.pval.Q        = TRUE,
  print.tau.ci        = TRUE,
  print.subgroup.name = FALSE
)

# ── Data prep (Lendo direto do Google Sheets) ──────────────────────────────────
prep_data <- function(sheet) {
  df <- as.data.frame(read_sheet(url, sheet = sheet))

  # 1. Padroniza o nome das colunas para minúsculas
  names(df) <- tolower(names(df))

  # 2. Filtra as linhas vazias
  if ("study" %in% names(df)) {
    df <- df[!is.na(df$study) & df$study != "", ]
  } else if ("studlab" %in% names(df)) {
    df <- df[!is.na(df$studlab) & df$studlab != "", ]
  }

  # 3. Colunas que DEVEM ser estritamente numéricas
  num_cols <- c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c", "event.e", "event.c")

  # 4. Força a conversão para número (e corrige vírgula decimal)
  for (col in num_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
      df[[col]] <- gsub(",", ".", df[[col]])
      df[[col]] <- as.numeric(df[[col]])
    }
  }

  return(df)
}

detect_subgroup <- function(df) {
  cols <- names(df)[grepl("^subgroup", names(df), ignore.case = TRUE)]
  if (length(cols) > 0) cols[1] else NULL
}

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 1 — Forest plot + summary output
# ══════════════════════════════════════════════════════════════════════════════

run_continuous <- function(sheet, subgroup_col = NULL, base_dir = output_dir) {
  out_dir <- if (!is.null(subgroup_col))
    file.path(base_dir, trimws(sheet), "subgroup")
  else
    file.path(base_dir, trimws(sheet))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n══ Continuous", if (!is.null(subgroup_col)) " [subgroup]" else "", ": ", sheet, " ══")

  df  <- prep_data(sheet)
  if (nrow(df) < 2) {
    message("  -> Pulando aba: não há estudos suficientes para meta-análise.")
    return(NULL)
  }

  has <- !is.null(subgroup_col)
  if (has) {
    # Remove do plot de subgrupos os estudos sem classificação
    keep <- !is.na(df[[subgroup_col]]) & df[[subgroup_col]] != ""
    if (any(!keep))
      message("  -> Removendo ", sum(!keep), " estudo(s) sem classificação de subgrupo.")
    df  <- df[keep, , drop = FALSE]
    sg  <- list(subgroup = as.character(df[[subgroup_col]]))
  } else {
    sg <- list()
  }

  ma0 <- do.call(metacont, c(
    list(
      n.e = df$n.e, mean.e = df$mean.e, sd.e = df$sd.e,
      n.c = df$n.c, mean.c = df$mean.c, sd.c = df$sd.c,
      studlab    = df$study,
      sm         = "MD",
      prediction = use_prediction(df)
    ), .re_args, sg
  ))
  ma <- apply_ci_rule(ma0)

  save_plot("01_forest", function() {
    do.call(meta::forest, c(
      .forest_base, forest_labels(sheet),
      .cont_cols,
      .theme,
      list(
        x                           = ma,
        sortvar                     = ma$studlab,
        overall                     = !has,
        overall.hetstat             = !has,
        test.overall.random         = !has,
        test.effect.subgroup.random = if (has) FALSE else NULL
      )
    ))
  }, out_dir)
  save_docx("output_01_meta", capture.output(summary(ma)), out_dir)

  message("══ Done: ", sheet, " ══\n")
  invisible(ma)
}

run_binary <- function(sheet, subgroup_col = NULL, base_dir = output_dir) {
  out_dir <- if (!is.null(subgroup_col))
    file.path(base_dir, trimws(sheet), "subgroup")
  else
    file.path(base_dir, trimws(sheet))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n══ Binary", if (!is.null(subgroup_col)) " [subgroup]" else "", ": ", sheet, " ══")

  df  <- prep_data(sheet)
  if (nrow(df) < 2) {
    message("  -> Pulando aba: não há estudos suficientes para meta-análise.")
    return(NULL)
  }

  has <- !is.null(subgroup_col)
  if (has) {
    # Remove do plot de subgrupos os estudos sem classificação
    keep <- !is.na(df[[subgroup_col]]) & df[[subgroup_col]] != ""
    if (any(!keep))
      message("  -> Removendo ", sum(!keep), " estudo(s) sem classificação de subgrupo.")
    df  <- df[keep, , drop = FALSE]
    sg  <- list(subgroup = as.character(df[[subgroup_col]]))
  } else {
    sg <- list()
  }

  ma0 <- do.call(metabin, c(
    list(
      event.e = df$event.e, n.e = df$n.e,
      event.c = df$event.c, n.c = df$n.c,
      studlab    = df$study,
      sm         = "RR",
      method     = "MH",       # Mantel-Haenszel: padrão Cochrane p/ RR, robusto a células-zero
      prediction = use_prediction(df)
    ), .re_args, sg
  ))
  ma <- apply_ci_rule(ma0)

  save_plot("01_forest", function() {
    do.call(meta::forest, c(
      .forest_base, forest_labels(sheet),
      .bin_cols,
      .theme,
      list(
        x                   = ma,
        sortvar             = ma$studlab,
        print.Q             = TRUE,
        overall             = !has,
        overall.hetstat     = !has,
        test.overall.random = !has
      )
    ))
  }, out_dir)
  save_docx("output_01_meta", capture.output(summary(ma)), out_dir)

  message("══ Done: ", sheet, " ══\n")
  invisible(ma)
}

# ══════════════════════════════════════════════════════════════════════════════
# Sheet classification
# ══════════════════════════════════════════════════════════════════════════════

cont_sheets <- c()
bin_sheets  <- c()

for (s in all_sheets) {
  df   <- prep_data(s)
  cols <- names(df)
  if (all(c("mean.e", "sd.e", "mean.c", "sd.c") %in% cols))
    cont_sheets <- c(cont_sheets, s)
  else if (all(c("event.e", "event.c") %in% cols))
    bin_sheets  <- c(bin_sheets, s)
  else
    message("Sheet '", s, "' skipped — columns not recognized as continuous or binary.")
}

cat("\nContinuous sheets:", paste(cont_sheets, collapse = ", "), "\n")
cat("Binary sheets:    ", paste(bin_sheets,  collapse = ", "), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 1 — Forest plots + summary outputs
# ══════════════════════════════════════════════════════════════════════════════

ma.cont <- list()
for (aba in cont_sheets) {
  df     <- prep_data(aba)
  sg_col <- detect_subgroup(df)

  ma.cont[[aba]] <- run_continuous(aba)

  if (!is.null(sg_col))
    ma.cont[[paste0(aba, "_sg")]] <- run_continuous(aba, subgroup_col = sg_col)
}

ma.bin <- list()
for (aba in bin_sheets) {
  df     <- prep_data(aba)
  sg_col <- detect_subgroup(df)

  ma.bin[[aba]] <- run_binary(aba)

  if (!is.null(sg_col))
    ma.bin[[paste0(aba, "_sg")]] <- run_binary(aba, subgroup_col = sg_col)
}
