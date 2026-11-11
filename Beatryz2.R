if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(meta, googlesheets4, ragg, officer)

# ── Configuração ──────────────────────────────────────────────────────────────
url        <- "https://docs.google.com/spreadsheets/d/1yHZv3lmak90JrDbVYJ9L4lC1rUeo0eRDOEMSq-3mMc0/edit?gid=968337662#gid=968337662"
output_dir <- "/Users/davibarbosa/Desktop/PSP"

# Método: unidade = ensaio (doses combinadas, Handbook 6.5.a); REML; IC de Wald em
# todas as figuras; Hartung-Knapp como sensibilidade; IP de HTS (t, k-2 gl).
MIN_K_PI      <- 4
SUBGROUP_FILL <- c("^Boxer et al\\., 2014" = "Microtubule stabilizers")
INVERT_LABELS <- "SEADL|MRI"   # desfechos em que maior = melhor

gs4_deauth()
all_sheets <- googlesheets4::sheet_names(url)

# ── Dados ─────────────────────────────────────────────────────────────────────
.cache <- new.env()

prep_data <- function(sheet) {
  if (!exists(sheet, envir = .cache, inherits = FALSE))
    assign(sheet, as.data.frame(googlesheets4::read_sheet(url, sheet = sheet)), envir = .cache)
  df <- get(sheet, envir = .cache)
  names(df) <- tolower(names(df))
  df <- df[!is.na(df$study) & df$study != "", ]
  
  for (col in intersect(c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c", "event.e", "event.c"), names(df)))
    df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  
  if (!"subgroup" %in% names(df)) df$subgroup <- ""
  df$subgroup <- as.character(df$subgroup)
  df$subgroup[is.na(df$subgroup) | df$subgroup == "NULL"] <- ""
  for (pat in names(SUBGROUP_FILL))
    df$subgroup[grepl(pat, df$study) & df$subgroup == ""] <- SUBGROUP_FILL[[pat]]
  
  # Ensaio = nome sem o sufixo de dose, ex. "(600 mg)"
  df$trial <- trimws(sub("\\s*\\([^()]*\\bmg\\b[^()]*\\)\\s*$", "", df$study, ignore.case = TRUE, perl = TRUE))
  df
}

pool_groups <- function(n, m, s) {
  N <- n[1]; M <- m[1]; S <- s[1]
  for (i in seq_along(n)[-1]) {
    Nt <- N + n[i]
    S  <- sqrt(((N - 1) * S^2 + (n[i] - 1) * s[i]^2 + N * n[i] / Nt * (M^2 + m[i]^2 - 2 * M * m[i])) / (Nt - 1))
    M  <- (N * M + n[i] * m[i]) / Nt
    N  <- Nt
  }
  c(n = N, mean = M, sd = S)
}

# Combina braços de dose de um mesmo ensaio. Placebo dividido (mesma média/DP) é somado.
combine_arms <- function(df, type) {
  out <- lapply(split(df, factor(df$trial, levels = unique(df$trial))), function(d) {
    row <- d[1, , drop = FALSE]
    row$study <- d$trial[1]
    if (nrow(d) == 1) return(row)
    row$study <- paste0(d$trial[1], " (doses combined)")
    if (type == "cont") {
      e  <- pool_groups(d$n.e, d$mean.e, d$sd.e)
      pc <- if (length(unique(d$mean.c)) == 1 && length(unique(d$sd.c)) == 1)
        c(n = sum(d$n.c), mean = d$mean.c[1], sd = d$sd.c[1])
      else pool_groups(d$n.c, d$mean.c, d$sd.c)
      row[c("n.e", "mean.e", "sd.e")] <- unname(e)
      row[c("n.c", "mean.c", "sd.c")] <- unname(pc)
    } else {
      row[c("event.e", "n.e", "event.c", "n.c")] <- colSums(d[c("event.e", "n.e", "event.c", "n.c")])
    }
    row
  })
  res <- do.call(rbind, out); rownames(res) <- NULL
  res
}

# ── Modelo ────────────────────────────────────────────────────────────────────
fit_meta <- function(df, type, subgroup = FALSE, ci = "classic") {
  args <- list(studlab = df$study, sm = if (type == "cont") "MD" else "RR",
               common = FALSE, random = TRUE, method.tau = "REML", method.I2 = "tau2",
               method.random.ci = ci,
               prediction = !subgroup && length(unique(df$trial)) >= MIN_K_PI,
               method.predict = "HTS")
  if (ci == "HK") args$adhoc.hakn.ci <- "se"
  if (subgroup) args <- c(args, list(subgroup = df$subgroup, prediction.subgroup = FALSE))
  
  if (type == "cont")
    do.call(metacont, c(df[c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c")], args))
  else
    do.call(metabin, c(df[c("event.e", "n.e", "event.c", "n.c")], list(method = "Inverse"), args))
}

# ── Saídas ────────────────────────────────────────────────────────────────────
forest_args <- function(sheet, sm, subgroup) {
  better_up <- grepl(INVERT_LABELS, sheet, ignore.case = TRUE)
  cols <- if (sm == "MD")
    list(leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
         leftlabs = c("Studies", "Mean", "SD", "Total", "Mean", "SD", "Total", "Weight", "MD", "95% CI"))
  else
    list(leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "w.random", "effect", "ci"),
         leftlabs = c("Studies", "Events", "Total", "Events", "Total", "Weight", "RR", "95% CI"))
  c(cols, list(
    layout = "RevMan5", rightcols = FALSE, just.addcols = "right",
    smlab = paste0(if (sm == "MD") "Mean Difference" else "Risk Ratio", "\nIV, Random (REML), Wald 95% CI"),
    label.e = "Tau-target therapy", label.c = "Placebo",
    label.left  = if (better_up) "Favours placebo" else "Favours tau-target therapy",
    label.right = if (better_up) "Favours tau-target therapy" else "Favours placebo",
    pooled.events = TRUE, pooled.totals = TRUE,
    digits = 2, digits.pval = 3, digits.sd = 2,
    colgap = "6mm", colgap.forest = "6mm",
    print.pval.Q = TRUE, print.tau.ci = TRUE, print.subgroup.name = FALSE,
    text.predict = "Prediction interval (HTS, t[k-2])",
    col.square = "blue", col.square.lines = "black",
    col.diamond = "black", col.diamond.lines = "black",
    col.predict = "red", col.predict.lines = "red",
    fontsize = 10, ff.study = "plain", ff.heading = "bold",
    overall = !subgroup, overall.hetstat = !subgroup, test.overall.random = !subgroup),
    if (subgroup) list(test.subgroup.random = TRUE, test.effect.subgroup.random = TRUE, subgroup.hetstat = TRUE))
}

save_plot <- function(out_dir, plot_fn, width = 16, height = 10, res = 300) {
  agg_tiff(file.path(out_dir, "01_forest.tiff"), width = width, height = height, units = "in", res = res, compression = "lzw")
  plot_fn(); dev.off()
  agg_png(file.path(out_dir, "01_forest.png"), width = width, height = height, units = "in", res = res)
  plot_fn(); dev.off()
}

save_docx <- function(out_dir, lines) {
  prop <- fp_text(font.family = "Courier New", font.size = 9)
  doc  <- read_docx()
  for (line in lines) doc <- body_add_fpar(doc, fpar(ftext(line, prop)))
  print(doc, target = file.path(out_dir, "output_01_meta.docx"))
}

summary_row <- function(sheet, analysis, df, ma, ma_hk) {
  bt <- function(x) if (is.null(x)) NA else if (ma$sm == "RR") exp(x) else x
  k_trials <- length(unique(df$trial[!is.na(ma$TE)]))
  show_pi  <- length(unique(df$trial)) >= MIN_K_PI
  data.frame(
    outcome = trimws(sheet), analysis = analysis,
    k_trials = k_trials, k_entries = ma$k, sm = ma$sm,
    estimate = bt(ma$TE.random),
    wald_lower = bt(ma$lower.random), wald_upper = bt(ma$upper.random), p_wald = ma$pval.random,
    hk_lower = bt(ma_hk$lower.random), hk_upper = bt(ma_hk$upper.random), hk_df = ma$k - 1, p_hk = ma_hk$pval.random,
    tau2 = ma$tau2, I2 = 100 * ma$I2,
    I2_Q = if (ma$Q > 0) 100 * max(0, (ma$Q - (ma$k - 1)) / ma$Q) else 0,
    Q = ma$Q, pval_Q = ma$pval.Q,
    pi_lower = if (show_pi) bt(ma$lower.predict) else NA,
    pi_upper = if (show_pi) bt(ma$upper.predict) else NA
  )
}

# ── Execução ──────────────────────────────────────────────────────────────────
results <- list()
splits  <- list()

run_outcome <- function(sheet, type, mode = c("combined", "split"), subgroup = FALSE) {
  mode <- match.arg(mode)
  raw  <- prep_data(sheet)
  df   <- if (mode == "combined") combine_arms(raw, type) else raw
  if (subgroup) {
    df <- df[df$subgroup != "", , drop = FALSE]
    if (length(unique(df$subgroup)) < 2) return(NULL)
  }
  if (length(unique(df$trial)) < 2) return(NULL)
  
  parts   <- c(output_dir, trimws(sheet), if (mode == "split") "sensitivity_dose_split", if (subgroup) "subgroup")
  out_dir <- do.call(file.path, as.list(parts))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  ma    <- fit_meta(df, type, subgroup, ci = "classic")
  ma_hk <- fit_meta(df, type, subgroup, ci = "HK")
  
  save_plot(out_dir, function() do.call(forest, c(list(x = ma, sortvar = ma$studlab), forest_args(sheet, ma$sm, subgroup))))
  save_docx(out_dir, c(
    sprintf("R %s | meta %s | %s", getRversion(), packageVersion("meta"),
            if (mode == "combined") "unidade = ensaio (doses combinadas)" else "sensibilidade: doses separadas, placebo dividido"),
    "", capture.output(summary(ma)),
    "", "── Sensibilidade: IC por Hartung-Knapp ──", capture.output(summary(ma_hk))))
  
  if (!subgroup) {
    results[[paste(sheet, mode)]] <<- summary_row(sheet, if (mode == "combined") "main (trials)" else "sensitivity (dose split)", df, ma, ma_hk)
    if (mode == "combined" && any(duplicated(raw$trial))) {
      multi <- raw[raw$trial %in% raw$trial[duplicated(raw$trial)], ]
      if (!"event.c" %in% names(multi)) multi$event.c <- NA
      splits[[sheet]] <<- cbind(outcome = trimws(sheet), multi[c("study", "trial", "n.e", "n.c", "event.c")])
    }
  }
  invisible(ma)
}

sheet_type <- function(s) {
  cols <- names(prep_data(s))
  if (all(c("mean.e", "sd.e", "mean.c", "sd.c") %in% cols)) "cont"
  else if (all(c("event.e", "event.c") %in% cols)) "bin"
  else NA_character_
}
types <- vapply(all_sheets, sheet_type, character(1))

ma.all <- list()
for (s in all_sheets[!is.na(types)]) {
  ty <- types[[s]]
  ma.all[[s]] <- run_outcome(s, ty)
  ma.all[[paste0(s, "_sg")]] <- run_outcome(s, ty, subgroup = TRUE)
  if (any(duplicated(prep_data(s)$trial)))
    ma.all[[paste0(s, "_split")]] <- run_outcome(s, ty, "split")
}

# ── Tabela-resumo ─────────────────────────────────────────────────────────────
summ <- do.call(rbind, results); rownames(summ) <- NULL
write.csv(summ, file.path(output_dir, "summary_all_outcomes.csv"), row.names = FALSE)

split_tab <- do.call(rbind, splits); rownames(split_tab) <- NULL
write.csv(split_tab, file.path(output_dir, "placebo_split_by_outcome.csv"), row.names = FALSE)

fmt <- function(x, d = 2) ifelse(is.na(x), "—", formatC(x, format = "f", digits = d))
tab <- with(summ, data.frame(
  Outcome = outcome, Analysis = analysis, `k (trials/entries)` = paste0(k_trials, "/", k_entries),
  Effect = paste(sm, fmt(estimate)),
  `Wald 95% CI` = paste(fmt(wald_lower), "to", fmt(wald_upper)),
  `HK 95% CI (df)` = paste0(fmt(hk_lower), " to ", fmt(hk_upper), " (", hk_df, ")"),
  `I² tau² / Q (%)` = paste(fmt(I2, 1), "/", fmt(I2_Q, 1)), tau2 = fmt(tau2, 3),
  `95% PI (HTS)` = ifelse(is.na(pi_lower), "—", paste(fmt(pi_lower), "to", fmt(pi_upper))),
  check.names = FALSE))

doc <- read_docx()
doc <- body_add_par(doc, "Summary of all outcomes", style = "heading 1")
doc <- body_add_par(doc, sprintf(
  "Random effects, REML tau². Main CI: Wald. Sensitivity CI: Hartung-Knapp (ad hoc 'se'). PI: Higgins-Thompson-Spiegelhalter (t, k-2 df), k >= %d trials. I² from tau² / from Q. R %s, meta %s.",
  MIN_K_PI, getRversion(), packageVersion("meta")))
doc <- body_add_table(doc, tab, style = "table_template")
doc <- body_add_par(doc, "Placebo split across dose arms (sensitivity analysis only)", style = "heading 2")
doc <- body_add_table(doc, split_tab, style = "table_template")
print(doc, target = file.path(output_dir, "summary_all_outcomes.docx"))

print(tab, row.names = FALSE)
