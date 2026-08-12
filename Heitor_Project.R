if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(meta, dmetar, googlesheets4, ragg, officer, ggplot2, ggrepel)

# ── Setup ──────────────────────────────────────────────────────────────────────
url        <- "https://docs.google.com/spreadsheets/d/1PEYMRaguTvPBNtX5vBW9Zh0wIP0abIpalwikFCqoOtw/edit?gid=1445029508#gid=1445029508"
output_dir <- "/Users/davibarbosa/Desktop/Heitor"

# Desativa a autenticação para ler links públicos sem pedir login
gs4_deauth()

# Define apenas as abas específicas que você quer rodar
all_sheets <- c(
  "Δbaseline Post-treatment LH - PP", 
  "Δbaseline Post-treatment LH - ITT", 
  "Δbaseline Post-treatment prolactin- PP", 
  "Δbaseline Post-treatment prolactin- ITT"
)

cat("Abas selecionadas para análise:\n")
print(all_sheets)

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

.forest_base <- list(
  layout              = "RevMan5",
  rightcols           = FALSE,
  just.addcols        = "right",
  pooled.events       = TRUE,
  pooled.totals       = TRUE,
  digits              = 2,
  digits.pval         = 3,
  digits.sd           = 2,
  label.e             = "Cyproterone acetate",
  label.c             = "Spironolactone",
  label.left          = "Favors cyproterone acetate",
  label.right         = "Favors spironolactone",
  colgap              = "6mm",
  colgap.forest       = "6mm",
  print.pval.Q        = TRUE,
  print.tau.ci        = TRUE,
  print.subgroup.name = FALSE
)

.loo_base <- list(
  col.bg    = "#AEC6CF",
  ff.xlab   = "bold",
  leftcols  = "studlab",
  leftlabs  = "Omitted Study",
  rightcols = c("effect", "ci", "I2"),
  rightlabs = c("Estimate", "95% CI", "I²")
)

# ── Data prep (Lendo direto do Google Sheets) ──────────────────────────────────
prep_data <- function(sheet) {
  # Lê os dados
  df <- as.data.frame(read_sheet(url, sheet = sheet))
  
  # 1. Padroniza o nome das colunas para minúsculas
  names(df) <- tolower(names(df))
  
  # 2. Filtra as linhas vazias
  if ("study" %in% names(df)) {
    df <- df[!is.na(df$study) & df$study != "", ]
  } else if ("studlab" %in% names(df)) {
    df <- df[!is.na(df$studlab) & df$studlab != "", ]
  }
  
  # 3. Lista das colunas que DEVEM ser estritamente numéricas
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
# PHASE 1 FUNCTIONS — Forest plot + summary output
# ══════════════════════════════════════════════════════════════════════════════

run_continuous <- function(sheet, subgroup_col = NULL, base_dir = output_dir) {
  out_dir <- if (!is.null(subgroup_col))
    file.path(base_dir, sheet, "subgroup")
  else
    file.path(base_dir, sheet)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n══ Continuous", if (!is.null(subgroup_col)) " [subgroup]" else "", ": ", sheet, " ══")
  
  df  <- prep_data(sheet)
  
  # Trava de segurança
  if (nrow(df) < 2) {
    message("  -> Pulando aba: não há estudos suficientes para meta-análise.")
    return(NULL)
  }
  
  has <- !is.null(subgroup_col)
  sg  <- if (has) list(subgroup = df[[subgroup_col]]) else list()
  
  ma <- do.call(metacont, c(
    list(
      n.e = df$n.e, mean.e = df$mean.e, sd.e = df$sd.e,
      n.c = df$n.c, mean.c = df$mean.c, sd.c = df$sd.c,
      studlab          = df$study,
      sm               = "MD",
      method.tau       = "REML",
      method.I2        = "tau2",
      method.random.ci = "HK",
      method.predict   = "HK",
      random           = TRUE,
      common           = FALSE,
      prediction       = TRUE
    ), sg
  ))
  
  save_plot("01_forest", function() {
    do.call(meta::forest, c(
      .forest_base,
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
    file.path(base_dir, sheet, "subgroup")
  else
    file.path(base_dir, sheet)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n══ Binary", if (!is.null(subgroup_col)) " [subgroup]" else "", ": ", sheet, " ══")
  
  df  <- prep_data(sheet)
  
  # Trava de segurança
  if (nrow(df) < 2) {
    message("  -> Pulando aba: não há estudos suficientes para meta-análise.")
    return(NULL)
  }
  
  has <- !is.null(subgroup_col)
  sg  <- if (has) list(subgroup = df[[subgroup_col]]) else list()
  
  ma <- do.call(metabin, c(
    list(
      event.e = df$event.e, n.e = df$n.e,
      event.c = df$event.c, n.c = df$n.c,
      studlab          = df$study,
      sm               = "RR",
      method           = "Inverse",
      method.tau       = "REML",
      method.I2        = "tau2",
      method.random.ci = "HK",
      method.predict   = "HK",
      random           = TRUE,
      common           = FALSE,
      prediction       = TRUE
    ), sg
  ))
  
  save_plot("01_forest", function() {
    do.call(meta::forest, c(
      .forest_base,
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
# PHASE 2 FUNCTIONS — LOO + publication bias
# ══════════════════════════════════════════════════════════════════════════════

run_loo_continuous <- function(sheet, base_dir = output_dir) {
  out_dir <- file.path(base_dir, sheet)
  message("\n══ LOO Continuous: ", sheet, " ══")
  
  df <- prep_data(sheet)
  
  if (nrow(df) < 2) return(NULL)
  
  ma <- do.call(metacont, c(
    list(
      n.e = df$n.e, mean.e = df$mean.e, sd.e = df$sd.e,
      n.c = df$n.c, mean.c = df$mean.c, sd.c = df$sd.c,
      studlab          = df$study,
      sm               = "MD",
      method.tau       = "REML",
      method.I2        = "tau2",
      method.random.ci = "HK",
      method.predict   = "HK",
      random           = TRUE,
      common           = FALSE,
      prediction       = TRUE
    )
  ))
  
  LOO <- metainf(ma, pooled = "random", prediction = FALSE)
  
  save_plot("02_LOO", function() {
    do.call(meta::forest, c(
      .loo_base,
      .theme[c("col.diamond", "col.diamond.lines", "fontsize", "ff.heading", "ff.study")],
      list(x = LOO, xlab = "Pooled MD (Omitting Study)")
    ))
  }, out_dir)
  save_docx("output_02_LOO", capture.output(print(LOO)), out_dir)
  
  message("══ Done LOO: ", sheet, " ══\n")
  invisible(LOO)
}

run_loo_binary <- function(sheet, base_dir = output_dir) {
  out_dir <- file.path(base_dir, sheet)
  message("\n══ LOO Binary: ", sheet, " ══")
  
  df <- prep_data(sheet)
  
  if (nrow(df) < 2) return(NULL)
  
  ma <- do.call(metabin, c(
    list(
      event.e = df$event.e, n.e = df$n.e,
      event.c = df$event.c, n.c = df$n.c,
      studlab          = df$study,
      sm               = "RR",
      method           = "Inverse",
      method.tau       = "REML",
      method.I2        = "tau2",
      method.random.ci = "HK",
      method.predict   = "HK",
      random           = TRUE,
      common           = FALSE,
      prediction       = TRUE
    )
  ))
  
  LOO <- metainf(ma, pooled = "random", prediction = FALSE)
  
  save_plot("02_LOO", function() {
    do.call(meta::forest, c(
      .loo_base,
      .theme[c("col.diamond", "col.diamond.lines", "fontsize", "ff.heading", "ff.study")],
      list(x = LOO, xlab = "Pooled RR (Omitting Study)")
    ))
  }, out_dir)
  save_docx("output_02_LOO", capture.output(print(LOO)), out_dir)
  
  if (nrow(df) >= 10) {
    tf <- trimfill(ma)
    save_plot("03_funnel_trimfill", function() {
      funnel(tf,
             studlab     = TRUE,
             bg          = "lightblue",
             cex         = 1,
             cex.studlab = 0.8,
             random      = TRUE,
             common      = FALSE,
             backtransf  = FALSE,
             contour     = c(0.95, 0.99))
    }, out_dir)
    save_docx("output_03_trimfill", capture.output(summary(tf)), out_dir)
    
    egger <- eggers.test(ma)
    print(egger)
    save_docx("output_04_egger", capture.output(print(egger)), out_dir)
  } else {
    message("  Skipping trim-and-fill & Egger's: fewer than 10 studies (n = ", nrow(df), ")")
  }
  
  message("══ Done LOO: ", sheet, " ══\n")
  invisible(LOO)
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

# ══════════════════════════════════════════════════════════════════════════════
# PHASE 2 — LOO exploration + publication bias
# ══════════════════════════════════════════════════════════════════════════════

loo.cont <- list()
for (aba in cont_sheets)
  loo.cont[[aba]] <- run_loo_continuous(aba)

loo.bin <- list()
for (aba in bin_sheets)
  loo.bin[[aba]] <- run_loo_binary(aba)
