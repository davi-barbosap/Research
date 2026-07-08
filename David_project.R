# ==============================================================================
# PIPELINE AUTOMATIZADO DE METANÁLISE (EXCLUSIVO PARA DADOS BINÁRIOS)
# Parâmetros: Risco Relativo (RR), Mantel-Haenszel (MH), REML, Hartung-Knapp (HK) x 
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(meta, dmetar, googlesheets4, ragg, officer, ggplot2, ggrepel)

# ── Setup ──────────────────────────────────────────────────────────────────────
url        <- "https://docs.google.com/spreadsheets/d/1Nj69ANLPz9Aw9SqbcaR3j_1vEH94e8J8Wl-DUmIdleQ/edit?gid=0#gid=0"
output_dir <- "/Users/davibarbosa/Desktop/David"

gs4_auth()

cat("Available sheets:\n")
print(sheet_names(url))

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

# ── Helpers para Exportação ────────────────────────────────────────────────────
save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10, res = 300) {
  for (ext in c("tiff", "png")) {
    path <- file.path(out_dir, paste0(filename_base, ".", ext))
    tryCatch({
      if (ext == "tiff")
        agg_tiff(path, width = width, height = height, units = "in", res = res, compression = "lzw")
      else
        agg_png(path, width = width, height = height, units = "in", res = res)
      on.exit(dev.off(), add = TRUE)
      plot_fn()
    }, error = function(e) {
      dev.off()
      message("Erro ao salvar ", path, ": ", e$message)
    })
    on.exit(NULL)
    dev.off()
    message("Saved: ", basename(path))
  }
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 9)
  doc  <- officer::read_docx()
  for (line in lines)
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(line, prop)))
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
  message("Saved: ", filename_base, ".docx")
}

# ── Configurações Visuais dos Gráficos (Forest e LOO) ──────────────────────────
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
  label.e             = "Psychosocial care",
  label.c             = "Usual care",
  label.left          = "Favors psychosocial care",
  label.right         = "Favors usual care",
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

# ── Preparação de Dados ────────────────────────────────────────────────────────
prep_data <- function(sheet) {
  df <- as.data.frame(read_sheet(url, sheet = sheet))
  df <- df[!is.na(df$study), ]
  
  num_cols <- c("n.e", "n.c", "event.e", "event.c")
  for (col in num_cols[num_cols %in% names(df)]) {
    x <- df[[col]]
    if (is.character(x)) x <- gsub(",", ".", x, fixed = TRUE)
    df[[col]] <- as.numeric(x)
  }
  df
}

detect_subgroup <- function(df) {
  cols <- names(df)[grepl("^subgroup", names(df), ignore.case = TRUE)]
  if (length(cols) > 1) warning("Múltiplas colunas de subgrupo encontradas. Usando a primeira: ", cols[1])
  if (length(cols) > 0) cols[1] else NULL
}

# ── Motor da Metanálise (Configurado conforme as diretrizes) ───────────────────
fit_binary <- function(df) {
  metabin(
    event.e = df$event.e, n.e = df$n.e,
    event.c = df$event.c, n.c = df$n.c,
    studlab          = df$study,
    sm               = "RR",      # Risco Relativo
    method           = "MH",      # Mantel-Haenszel
    method.tau       = "REML",    # Restricted Maximum Likelihood
    method.I2        = "tau2",
    method.random.ci = "HK",      # Hartung-Knapp-Sidik-Jonkman
    random           = TRUE,      # Modelo de efeitos aleatórios
    common           = FALSE,     
    prediction       = TRUE       # Intervalo de predição ativo
  )
}

forest_args_binary <- function(ma, subgroup = FALSE) {
  args <- c(.forest_base, .bin_cols, .theme,
            list(x = ma, sortvar = ma$studlab, print.Q = TRUE,
                 overall = !subgroup, overall.hetstat = !subgroup, test.overall.random = !subgroup))
  if (subgroup) args$test.effect.subgroup.random <- FALSE
  args
}

# ══════════════════════════════════════════════════════════════════════════════
# FASE 1 — Gráfico de Floresta (Análise Principal e Subgrupos)
# ══════════════════════════════════════════════════════════════════════════════
run_binary <- function(sheet, df, sg_col = NULL, base_dir = output_dir) {
  main_dir <- file.path(base_dir, sheet)
  dir.create(main_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n══ Processando Aba: ", sheet, " ══")
  
  ma_main <- fit_binary(df)
  
  save_plot("01_forest", function() {
    do.call(meta::forest, forest_args_binary(ma_main, subgroup = FALSE))
  }, main_dir)
  save_docx("output_01_meta", capture.output(summary(ma_main)), main_dir)
  
  if (!is.null(sg_col)) {
    sub_dir <- file.path(main_dir, "subgroup")
    dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
    message("  ↳ Subgrupo detectado: ", sg_col)
    
    ma_sg <- update(ma_main, subgroup = df[[sg_col]])
    
    save_plot("01_forest_subgroup", function() {
      do.call(meta::forest, forest_args_binary(ma_sg, subgroup = TRUE))
    }, sub_dir)
    save_docx("output_01_meta_subgroup", capture.output(summary(ma_sg)), sub_dir)
  }
  invisible(ma_main)
}

# ══════════════════════════════════════════════════════════════════════════════
# FASE 2 — Leave-One-Out (LOO) + Funnel Plot de Contorno + Viés de Publicação
# ══════════════════════════════════════════════════════════════════════════════
run_loo_binary <- function(sheet, df, base_dir = output_dir) {
  out_dir <- file.path(base_dir, sheet)
  message("  ↳ Gerando Leave-One-Out (LOO)")
  
  ma  <- fit_binary(df)
  LOO <- metainf(ma, pooled = "random")
  
  save_plot("02_LOO", function() {
    do.call(meta::forest, c(.loo_base, .theme[c("col.diamond", "col.diamond.lines", "fontsize", "ff.heading", "ff.study")],
                            list(x = LOO, xlab = "Pooled RR (Omitting Study)")))
  }, out_dir)
  save_docx("output_02_LOO", capture.output(print(LOO)), out_dir)
  
  # Viés de Publicação: Apenas rodar se houver 10 ou mais estudos
  if (nrow(df) >= 10) {
    message("  ↳ Gerando Trim and Fill e Teste de Egger (k >= 10)")
    tf <- trimfill(ma)
    
    # Salvar o plot do Trim and Fill com design aprimorado (Contour Funnel Plot)
    save_plot("03_funnel_trimfill_contour", function() {
      
      contour_levels <- c(0.90, 0.95, 0.99)
      contour_colors <- c("gray90", "gray80", "gray70")
      
      funnel(tf,
             studlab = TRUE, 
             bg = "lightblue", 
             cex = 1.5,
             cex.studlab = 0.8,
             random = TRUE,
             common = FALSE,
             backtransf = FALSE,
             contour = contour_levels,
             col.contour = contour_colors,
             xlab = "Log Risk Ratio",
             ylab = "Standard Error",
             lwd = 1.2)
      
      # Adiciona a legenda de significância ao gráfico
      legend("topright",
             legend = c("p > 0.10", "0.05 < p < 0.10", "0.01 < p < 0.05", "p < 0.01"),
             fill = c("white", contour_colors),
             title = "Significance Levels",
             bty = "n",
             cex = 0.8,
             bg = "transparent")
      
    }, out_dir)
    
    save_docx("output_03_trimfill", capture.output(summary(tf)), out_dir)
    
    # Teste de Egger apropriado para Risco Relativo
    vies_pub <- meta::metabias(ma, method = "egger")
    save_docx("output_04_bias_test", capture.output(print(vies_pub)), out_dir)
  }
  invisible(LOO)
}

# ══════════════════════════════════════════════════════════════════════════════
# LOOP MESTRE DE EXECUÇÃO
# ══════════════════════════════════════════════════════════════════════════════
all_sheets <- sheet_names(url)

for (aba in all_sheets) {
  tryCatch({
    df <- prep_data(aba)
    
    # Condição: Só processa a aba se ela tiver as quatro colunas binárias essenciais
    if (all(c("event.e", "event.c", "n.e", "n.c") %in% names(df))) {
      sg_col <- detect_subgroup(df)
      
      # Executa a metanálise principal e salva
      run_binary(aba, df, sg_col = sg_col)
      
      # Executa a análise de sensibilidade e viés se k >= 3
      if (nrow(df) >= 3) {
        run_loo_binary(aba, df)
      } else {
        message("  ↳ LOO ignorado para '", aba, "': possui menos de 3 estudos.")
      }
      
    } else {
      message("\nIgnorando Aba '", aba, "': colunas binárias necessárias não foram encontradas.")
    }
  }, error = function(e) message("Erro fatal ao processar aba '", aba, "': ", e$message))
}

message("\n==== TODAS AS ANÁLISES FORAM CONCLUÍDAS ====")