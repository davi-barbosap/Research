if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(meta, dmetar, googlesheets4, ragg, officer, ggplot2)

# ── Setup ──────────────────────────────────────────────────────────────────────

url        <- "https://docs.google.com/spreadsheets/d/1WfJCvlhFiU4MeTxq_aeqMmBGoF_OKlHWFdbVZbaAKjg/edit?gid=1851035878#gid=1851035878"   # <── Link do Google Sheets
output_dir <- "/Users/davibarbosa/Desktop/Atul"   # <── Pasta raiz

# ── Cochrane SD change score ───────────────────────────────────────────────────
# e = baseline (pré), c = follow-up (pós)
# SD_change = sqrt(SD_pre² + SD_pos² - 2 × r × SD_pre × SD_pos)

sd_change <- function(sd_pre, sd_pos, corr) {
  sqrt(sd_pre^2 + sd_pos^2 - 2 * corr * sd_pre * sd_pos)
}

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

save_plot <- function(filename_base, plot_fn, out_dir, width = 14, height = 8, res = 300) {
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
  metamean(
    n        = data$n.e,
    mean     = data$mean_change,
    sd       = data[[sd_col]],
    data     = data,
    sm       = "MRAW",
    studlab  = data$Authors,
    random   = TRUE,
    common   = FALSE,
    method.tau       = "REML",
    method.random.ci = TRUE,
    prediction       = TRUE
  )
}

plot_forest <- function(mc, label, filename_base, out_dir) {
  save_plot(filename_base, function() {
    do.call(meta::forest, c(
      list(
        x         = mc,
        layout    = "Revman5",
        smlab     = "MRAW\nIV, Random, 95% CI", 
        leftcols  = c("studlab", "mean", "sd", "n", "w.random", "effect", "ci"), 
        leftlabs  = c("Study", NA, NA, NA, "Weight", NA, NA), 
        colgap    = "4mm",            
        colgap.forest.left  = "5mm",  
        pooled.totals       = TRUE,
        digits    = 2,
        digits.sd = 2,
        just      = "center",
        sortvar   = mc$TE,
        overall   = TRUE,
        overall.hetstat     = TRUE,
        test.overall.random = TRUE
      ),
      .theme
    ))
  }, out_dir)
}

prep_data <- function(sheet) {
  df <- as.data.frame(read_sheet(url, sheet = sheet))
  df <- df[!is.na(df$Authors) & df$Authors != "", ] 
  df$mean_change  <- df$mean.c - df$mean.e
  df$sd_change_05 <- sd_change(df$sd.e, df$sd.c, corr = 0.5)
  df$sd_change_03 <- sd_change(df$sd.e, df$sd.c, corr = 0.3)
  df$sd_change_07 <- sd_change(df$sd.e, df$sd.c, corr = 0.7)
  df
}

# ── Main Functions ─────────────────────────────────────────────────────────────

run_diagnostics <- function(sheet, label = sheet, base_dir = output_dir) {
  out_dir <- file.path(base_dir, sheet)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("\n══ Iniciando diagnósticos: ", label, " ══")
  
  df <- prep_data(sheet)
  mc <- run_metamean(df, "sd_change_05")
  
  # 01 — Forest principal
  plot_forest(mc, paste0(label, " (r = 0.5)"), "01_forest_principal", out_dir)
  save_output_docx("output_01_meta", capture.output(summary(mc)), out_dir)
  
  # 02 — Influence
  ID <- InfluenceAnalysis(mc, random = TRUE)
  
  save_plot("02_influence_principal", function() plot(ID, "influence"), out_dir)
  save_output_docx("output_02_influence", capture.output(print(ID)), out_dir)
  
  # 03 — Baujat (com ajuste de tamanho dos rótulos internos)
  save_plot("03_baujat_principal", function() {
    p <- plot(ID, "baujat")
    if (inherits(p, "ggplot")) {
      p$layers <- lapply(p$layers, function(layer) {
        if (inherits(layer$geom, c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel"))) {
          layer$aes_params$size <- 5  
        }
        layer
      })
      print(p + theme(text = element_text(size = 18)))
    } else {
      print(p)
    }
  }, out_dir)
  
  grDevices::pdf(nullfile())
  baujat_lines <- capture.output(plot(ID, "baujat"))
  grDevices::dev.off()
  if (length(baujat_lines) == 0) baujat_lines <- "(Sem output textual)"
  save_output_docx("output_03_baujat", baujat_lines, out_dir)
  
  # 04 — LOO
  LOO <- metainf(mc, pooled = "random", prediction = TRUE)
  
  save_plot("04_LOO_principal", function() {
    do.call(meta::forest, c(
      list(
        x          = LOO,
        col.bg     = "#AEC6CF",
        xlab       = "Pooled Mean Change (Omitting Study)",
        ff.xlab    = "bold",
        leftcols   = "studlab",
        leftlabs   = "Omitted Study",
        rightcols  = c("effect", "ci", "I2"),
        rightlabs  = c("MRAW", "95% CI", "I²"),
        prediction = TRUE
      ),
      .theme[c("col.diamond", "col.diamond.lines",
               "col.predict", "col.predict.lines",
               "fontsize", "ff.heading", "ff.study")]
    ))
  }, out_dir)
  save_output_docx("output_04_LOO", capture.output(print(LOO)), out_dir)
  
  # 05/06 — Sensibilidade
  mc_r03 <- run_metamean(df, "sd_change_03")
  plot_forest(mc_r03, paste0(label, " \u2014 Sensibility (r = 0.3)"),
              "05_forest_sensibilidade_r03", out_dir)
  
  mc_r07 <- run_metamean(df, "sd_change_07")
  plot_forest(mc_r07, paste0(label, " \u2014 Sensibility (r = 0.7)"),
              "06_forest_sensibilidade_r07", out_dir)
  
  message("══ Concluído: ", label, " ══\n")
}

run_subset <- function(sheet, label = sheet, outliers, base_dir = output_dir) {
  out_dir <- file.path(base_dir, sheet)
  
  message("\n══ Subset: ", label, " ══")
  
  df     <- prep_data(sheet)
  df_sub <- subset(df, !(Authors %in% outliers))
  mc_sub <- run_metamean(df_sub, "sd_change_05")
  
  # 07 — Forest do subset
  plot_forest(mc_sub, paste0(label, " \u2014 Subset (r = 0.5)"),
              "07_forest_subset", out_dir)
  
  # NOVO: Salva os resultados estatísticos do subset em formato Word (.docx)
  save_output_docx("output_07_subset_meta", capture.output(summary(mc_sub)), out_dir) # <── Ajuste
  
  message("══ Concluído subset: ", label, " ══\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# FASE 1 — Diagnósticos
# ══════════════════════════════════════════════════════════════════════════════
run_diagnostics("WOMAC total 6mo")

abas <- googlesheets4::sheet_names(url) 

for (aba in abas) run_diagnostics(aba)

# ══════════════════════════════════════════════════════════════════════════════
# FASE 2 — Subset (Análises sem Outliers)
# ══════════════════════════════════════════════════════════════════════════════

# ── WOMAC Total ──
run_subset("WOMAC total 1mo",  outliers = c("Little et al., 2025", "Guzelbey et al., 2024"))
run_subset("WOMAC total 3mo",  outliers = c("Little et al., 2025", "Bagla et al., 2022"))
run_subset("WOMAC total 6mo",  outliers = c("Little et al., 2025"))
run_subset("WOMAC total 12mo", outliers = c("Bagla et al., 2022", "Okuno et al., 2017"))

# ── WOMAC Pain ──
run_subset("WOMAC pain 1mo",  outliers = c("Guzelbey et al., 2024", "Hindsø et al., 2024"))
run_subset("WOMAC pain 3mo",  outliers = c("Little et al., 2025", "Talaskian et al., 2025"))
run_subset("WOMAC pain 6mo",  outliers = c("Hindsø et al., 2024", "Okuno et al., 2017", "Little et al., 2025"))
run_subset("WOMAC pain 12mo", outliers = c("Okuno et al., 2017"))

# ── WOMAC Stiffness ──
run_subset("WOMAC stiff 1mo",  outliers = c("Sun et al., 2024"))
run_subset("WOMAC stiff 3mo",  outliers = c("Sun et al., 2024"))
run_subset("WOMAC stiff 6mo",  outliers = c("Sun et al., 2024"))
# WOMAC stiff 12mo — k=3, subset inviável; modelo principal mantido

# ── WOMAC Function ──
run_subset("WOMAC func 1mo",  outliers = c("Guzelbey et al., 2024"))
run_subset("WOMAC func 3mo",  outliers = c("Guzelbey et al., 2024"))
# WOMAC func 6mo — sem outlier estatístico; modelo principal mantido

# ── VAS ──
run_subset("VAS-1",  outliers = c("Guzelbey et al., 2024", "Little et al., 2025"))
run_subset("VAS-3",  outliers = c("Little et al., 2025", "Bagla et al., 2022"))
run_subset("VAS-6",  outliers = c("Hindsø et al., 2024", "Bagla et al., 2022"))
run_subset("VAS-12", outliers = c("Okuno et al., 2017", "Sun et al., 2024"))

# ── KOOS Pain ──
run_subset("KOOS pain 3mo", outliers = c("Little et al., 2021", "Talaskian et al., 2025"))
run_subset("KOOS pain 6mo", outliers = c("Hindsø et al., 2024"))  # ⚠ resulta em k=2