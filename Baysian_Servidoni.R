# ==============================================================================
# MetaBayes — Bayesian Meta-Analysis + LOO | Google Sheets Automation
# Binary Outcome (Risk Ratio)
# Phase 1: Primary Analysis + Sensitivity Analyses
# Phase 2: Leave-One-Out (LOO) — executado automaticamente quando tau > 0.3
# ==============================================================================


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0 — CONFIGURAÇÃO
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# Cole o link nativo da planilha Google Sheets
url        <- "https://docs.google.com/spreadsheets/d/1aHE6ndgJdPnjffEx9f8SxSUxQulg_Rsad9XOSliwVNE/edit?gid=1023766173#gid=1023766173"

# Pasta raiz de saída: subpastas por desfecho serão criadas automaticamente
output_dir <- "/Users/davibarbosa/Desktop/Servidoni"


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §1 — PACOTES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

if (!require("devtools")) install.packages("devtools")
if (!require("pacman"))   install.packages("pacman")

# BayesianPriorsTool — disponível apenas no GitHub
if (!requireNamespace("BayesianPriorsTool", quietly = TRUE))
  devtools::install_github("arthur-albuquerque/BayesianPriorsTool")

pacman::p_load(
  googlesheets4,
  ragg,
  officer,
  bayesmeta,
  BayesianPriorsTool,
  dplyr, tidyr,
  metafor,
  ggdist,
  ggplot2,
  patchwork,
  meta,
  janitor,
  future,
  future.apply
)


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §2 — AUTENTICAÇÃO E TEMA
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

gs4_auth()

theme_set(
  theme(
    plot.title.position = "plot",
    axis.ticks.x        = element_blank(),
    axis.ticks.y        = element_blank(),
    axis.text.x         = element_text(size = 12),
    axis.text.y         = element_text(size = 12),
    axis.title.x        = element_text(size = 14),
    panel.background    = element_blank(),
    panel.grid.major.x  = element_line(color = "gray80", linewidth = 0.3)
  )
)


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §3 — FUNÇÕES AUXILIARES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# Registra erros em arquivo de log (sem poluir o console)
log_error <- function(context, error_msg) {
  log_path <- file.path(output_dir, "pipeline_errors.log")
  write(
    paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", context, " — ", error_msg),
    file   = log_path,
    append = TRUE
  )
}

save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10) {
  agg_tiff(
    file.path(out_dir, paste0(filename_base, ".tiff")),
    width = width, height = height, units = "in", res = 300, compression = "lzw"
  )
  plot_fn()
  dev.off()
  
  agg_png(
    file.path(out_dir, paste0(filename_base, ".png")),
    width = width, height = height, units = "in", res = 300
  )
  plot_fn()
  dev.off()
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 10)
  doc  <- officer::read_docx()
  for (line in lines) {
    doc <- officer::body_add_fpar(
      doc, officer::fpar(officer::ftext(as.character(line), prop))
    )
  }
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
}

# Usa "mean" para mu e theta; "median" para tau (distribuição assimétrica à direita)
get_summary <- function(model) {
  list(
    mu_mean     = model$summary["mean",      "mu"],
    mu_upper    = model$summary["95% upper", "mu"],
    mu_lower    = model$summary["95% lower", "mu"],
    tau_median  = model$summary["median",    "tau"],
    tau_upper   = model$summary["95% upper", "tau"],
    tau_lower   = model$summary["95% lower", "tau"],
    theta_mean  = model$summary["mean",      "theta"],
    theta_upper = model$summary["95% upper", "theta"],
    theta_lower = model$summary["95% lower", "theta"]
  )
}

# tau.sample = FALSE: tau fixado na mediana posterior; afeta apenas a visualização
# da densidade — todos os resultados reportados são calculados analiticamente pelo bayesmeta
generate_posterior_plots <- function(model, fill_color, is_predict = FALSE) {
  set.seed(123)
  samples <- model$rposterior(n = 10000, tau.sample = FALSE, predict = is_predict)
  df_s    <- data.frame(samples = samples)
  
  if (is_predict) {
    x_breaks <- c(0.2, 0.5, 1, 2, 5)
    x_coord  <- c(0.2, 5)
    seq_by   <- 1
  } else {
    x_breaks <- c(0.5, 0.75, 1, 1.5, 2)
    x_coord  <- c(0.4, 2.5)
    seq_by   <- 0.4
  }
  
  p_dens <- ggplot(df_s, aes(x = samples)) +
    ggdist::stat_halfeye(.width = 0.95, point_interval = "mean_hdi",
                         fill = fill_color) +
    scale_x_continuous(breaks = log(x_breaks),
                       labels = function(x) round(exp(x), 2)) +
    labs(x = "\nRisk Ratio", y = "Density") +
    theme(axis.text.y = element_blank()) +
    coord_cartesian(x = log(x_coord))
  
  if (is_predict) {
    cutoffs <- log(seq(0.1, 5,  by = 0.1))
    probs   <- model$pposterior(theta = cutoffs, predict = TRUE)
  } else {
    cutoffs <- log(seq(0.2, 3,  by = 0.02))
    probs   <- model$pposterior(mu = cutoffs)
  }
  
  p_prob <- ggplot(data.frame(cutoffs, probs), aes(x = exp(cutoffs), y = probs)) +
    geom_line() +
    scale_x_continuous(breaks = seq(x_coord[1], x_coord[2], by = seq_by)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    coord_cartesian(x = x_coord) +
    labs(x = "\nRisk Ratio", y = expression("Probability RR" <= "X")) +
    theme(panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3))
  
  list(density = p_dens, probability = p_prob)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §4 — FASE 2: Leave-One-Out (LOO) Sensitivity Analysis
# Acionada automaticamente quando tau_median > 0.3 na análise primária
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_loo_analysis <- function(dat_RR, full_model,
                             mu_mean, mu_sd, tau_prior_func,
                             out_dir, sheet_name) {
  K <- nrow(dat_RR)
  
  plan(multisession)
  on.exit(plan(sequential), add = TRUE)
  
  loo_list <- future_lapply(seq_len(K), function(i) {
    dat_loo <- dat_RR[-i, ]
    fit_loo <- bayesmeta(
      y             = dat_loo$yi,
      sigma         = dat_loo$sei,
      labels        = dat_loo$study,
      mu.prior.mean = mu_mean,
      mu.prior.sd   = mu_sd,
      tau.prior     = tau_prior_func
    )
    list(
      study_excluded = dat_RR$study[i],
      estimate       = fit_loo$summary["median", "mu"],
      ci_lower       = fit_loo$summary["95% lower", "mu"],
      ci_upper       = fit_loo$summary["95% upper", "mu"],
      tau_median     = fit_loo$summary["median", "tau"]
    )
  }, future.seed = TRUE)
  
  loo_results <- data.frame(
    study_excluded = sapply(loo_list, `[[`, "study_excluded"),
    estimate       = sapply(loo_list, `[[`, "estimate"),
    ci_lower       = sapply(loo_list, `[[`, "ci_lower"),
    ci_upper       = sapply(loo_list, `[[`, "ci_upper"),
    tau_median     = sapply(loo_list, `[[`, "tau_median"),
    stringsAsFactors = FALSE
  )
  
  full_mu    <- full_model$summary["median",    "mu"]
  full_mu_lo <- full_model$summary["95% lower", "mu"]
  full_mu_hi <- full_model$summary["95% upper", "mu"]
  
  m_loo <- metagen(
    sm      = "RR",
    TE      = loo_results$estimate,
    lower   = loo_results$ci_lower,
    upper   = loo_results$ci_upper,
    studlab = paste0("Excluindo: ", loo_results$study_excluded),
    random  = FALSE,
    fixed   = FALSE
  )
  
  m_loo_ref <- metaadd(
    m_loo, type = "random",
    TE    = full_mu,
    lower = full_mu_lo,
    upper = full_mu_hi,
    name  = "Full Analysis (Reference)"
  )
  
  loo_forest_height <- max(10, K * 0.45 + 5)
  
  save_plot("Supp_Fig_5_LOO_Forest", function() {
    meta::forest(
      m_loo_ref,
      func.transf      = exp,
      leftcols         = "studlab",
      leftlabs         = "Omitted Study",
      rightcols        = c("effect", "ci"),
      rightlabs        = c("RR", "95% CrI"),
      xlab             = "Risk Ratio",
      smlab            = "",
      weight.study     = "same",
      col.square       = "steelblue",
      col.square.lines = "steelblue",
      overall          = FALSE,
      print.tau2       = FALSE,
      print.I2         = FALSE,
      print.pval.Q     = FALSE,
      hetstat          = FALSE,
      overall.hetstat  = FALSE
    )
  }, out_dir, height = loo_forest_height)
  
  loo_header <- sprintf("%-32s | %6s [%6s, %6s] | %s",
                        "Study Excluded", "RR", "Lower", "Upper", "tau (median)")
  loo_rows   <- vapply(seq_len(nrow(loo_results)), function(i) {
    r <- loo_results[i, ]
    sprintf("%-32s | %6.2f [%6.2f, %6.2f] | %.3f",
            r$study_excluded,
            exp(r$estimate), exp(r$ci_lower), exp(r$ci_upper),
            r$tau_median)
  }, character(1))
  
  save_docx("Supp_Table_2_LOO_Results", c(
    paste0("Supplementary Table 2 — LOO Sensitivity Analysis: ", sheet_name),
    paste(rep("-", 72), collapse = ""),
    loo_header,
    paste(rep("-", 72), collapse = ""),
    loo_rows,
    paste(rep("-", 72), collapse = ""),
    sprintf("%-32s | %6.2f [%6.2f, %6.2f] | %s",
            "Full Analysis (Reference)",
            exp(full_mu), exp(full_mu_lo), exp(full_mu_hi), "—")
  ), out_dir)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §5 — ANÁLISE COMPLETA POR DESFECHO (Fase 1 + Fase 2 automática)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_bayesian_meta <- function(sheet, base_dir = output_dir) {
  
  out_dir_main <- file.path(base_dir, sheet)
  out_dir_meth <- file.path(out_dir_main, "Supplementary_Methods")
  out_dir_resu <- file.path(out_dir_main, "Supplementary_Results")
  
  dir.create(out_dir_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir_resu, showWarnings = FALSE, recursive = TRUE)
  
  # ── Dados ───────────────────────────────────────────────────────────────────
  dat <- as.data.frame(read_sheet(url, sheet = sheet)) |> janitor::clean_names()
  dat <- dat[!is.na(dat$study), ]
  
  required_cols <- c("study", "ai", "n1i", "ci", "n2i")
  missing_cols  <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0)
    stop("Colunas ausentes na aba '", sheet, "': ",
         paste(missing_cols, collapse = ", "))
  
  dat_RR     <- escalc(measure = "RR",
                       ai = ai, n1i = n1i, ci = ci, n2i = n2i,
                       data = dat) |> data.frame()
  dat_RR$sei <- sqrt(dat_RR$vi)
  
  # Remove estudos com zero eventos em ambos os grupos ou com yi/sei inválidos.
  # Esses estudos não contribuem para o RR e distorcem o forest plot.
  dat_RR <- dat_RR[is.finite(dat_RR$yi) & is.finite(dat_RR$sei) & dat_RR$sei > 0, ]
  
  if (nrow(dat_RR) < 2)
    stop("Menos de 2 estudos validos apos remocao de zeros em '", sheet, "'.")
  
  # ── Priors ──────────────────────────────────────────────────────────────────
  
  # >>> AJUSTE (1/3): Efeito global (Mu)
  # Média 0 = sem efeito a priori (hipótese nula). SD 2.7 = vago na escala log-RR.
  prior_mu_vague_mean_RR    <- 0
  prior_mu_vague_sd_RR      <- 2.7
  
  # >>> AJUSTE (2/3): Heterogeneidade (Tau) — análise primária
  # Scale 0.5 = Weakly-Informative. Aumente para populações muito distintas;
  # diminua para réplicas de um protocolo único.
  prior_tau_weakly_scale_RR <- 0.5
  prior_tau_primary_func    <- function(x) dhalfnormal(x, scale = prior_tau_weakly_scale_RR)
  
  # >>> AJUSTE (3/3): Cenários de sensibilidade
  # Sensibilidade 1: Tau Vago (1.5) — testa robustez a maior heterogeneidade
  # Sensibilidade 2: Mu Weakly-Informative (SD 0.82) — prior mais cético sobre efeito
  vague_prior_RR_scale    <- 1.5
  vague_tau_RR_func       <- function(x) dhalfnormal(x, scale = vague_prior_RR_scale)
  prior_mu_weakly_mean_RR <- 0
  prior_mu_weakly_sd_RR   <- 0.82
  
  # Gráficos de priors
  plot_overall_vague_prior  <- mu_dist_viz(
    mean = prior_mu_vague_mean_RR, sd = prior_mu_vague_sd_RR,
    scale = "relative", xlabel = "Risk Ratio"
  )
  plot_tau_weakly_prior     <- tau_dist_viz(dist = "half-normal",
                                            scale = prior_tau_weakly_scale_RR)
  plot_tau_vague_prior      <- tau_dist_viz(dist = "half-normal",
                                            scale = vague_prior_RR_scale)
  plot_overall_weakly_prior <- mu_dist_viz(
    mean = prior_mu_weakly_mean_RR, sd = prior_mu_weakly_sd_RR,
    scale = "relative", xlabel = "Risk Ratio"
  )
  
  # ── Modelos Bayesianos ───────────────────────────────────────────────────────
  meta_RR_primary <- bayesmeta(
    y             = dat_RR$yi,
    sigma         = dat_RR$sei,
    labels        = dat_RR$study,
    mu.prior.mean = prior_mu_vague_mean_RR,
    mu.prior.sd   = prior_mu_vague_sd_RR,
    tau.prior     = prior_tau_primary_func
  )
  
  meta_sens1 <- bayesmeta(
    y             = dat_RR$yi,
    sigma         = dat_RR$sei,
    labels        = dat_RR$study,
    mu.prior.mean = prior_mu_vague_mean_RR,
    mu.prior.sd   = prior_mu_vague_sd_RR,
    tau.prior     = vague_tau_RR_func
  )
  
  meta_sens2 <- bayesmeta(
    y             = dat_RR$yi,
    sigma         = dat_RR$sei,
    labels        = dat_RR$study,
    mu.prior.mean = prior_mu_weakly_mean_RR,
    mu.prior.sd   = prior_mu_weakly_sd_RR,
    tau.prior     = prior_tau_primary_func
  )
  
  post_prim  <- get_summary(meta_RR_primary)
  post_sens1 <- get_summary(meta_sens1)
  post_sens2 <- get_summary(meta_sens2)
  
  # ── Forest base ──────────────────────────────────────────────────────────────
  forest_data_RR <- metabin(
    data    = dat_RR,
    studlab = study,
    event.e = ai,  n.e = n1i,
    event.c = ci,  n.c = n2i,
    sm      = "RR",
    common  = FALSE,
    random  = FALSE
  )
  
  mf_prim      <- metaadd(forest_data_RR, type = "random",
                          TE = post_prim$mu_mean,
                          upper = post_prim$mu_upper,
                          lower = post_prim$mu_lower)
  mf_prim_pred <- metaadd(mf_prim, type = "prediction",
                          TE = post_prim$theta_mean,
                          upper = post_prim$theta_upper,
                          lower = post_prim$theta_lower)
  
  mf_s1      <- metaadd(forest_data_RR, type = "random",
                        TE = post_sens1$mu_mean,
                        upper = post_sens1$mu_upper,
                        lower = post_sens1$mu_lower)
  mf_s1_pred <- metaadd(forest_data_RR, type = "prediction",
                        TE = post_sens1$theta_mean,
                        upper = post_sens1$theta_upper,
                        lower = post_sens1$theta_lower)
  
  mf_s2      <- metaadd(forest_data_RR, type = "random",
                        TE = post_sens2$mu_mean,
                        upper = post_sens2$mu_upper,
                        lower = post_sens2$mu_lower)
  mf_s2_pred <- metaadd(forest_data_RR, type = "prediction",
                        TE = post_sens2$theta_mean,
                        upper = post_sens2$theta_upper,
                        lower = post_sens2$theta_lower)
  
  analyses_names <- c(
    "Primary Analysis: Weakly-Informative Tau",
    "Sensitivity 1: Vague Tau",
    "Sensitivity 2: Weakly-Informative Mu"
  )
  all_overall <- metabind(mf_prim, mf_s1, mf_s2,                name = analyses_names)
  all_predict <- metabind(mf_prim_pred, mf_s1_pred, mf_s2_pred, name = analyses_names)
  
  tau_prim_text  <- paste0("τ = ", round(post_prim$tau_median,  2),
                           " [95% CrI ", round(post_prim$tau_lower,  2),
                           ", ", round(post_prim$tau_upper,  2), "]")
  tau_sens1_text <- paste0("τ = ", round(post_sens1$tau_median, 2),
                           " [95% CrI ", round(post_sens1$tau_lower, 2),
                           ", ", round(post_sens1$tau_upper, 2), "]")
  tau_sens2_text <- paste0("τ = ", round(post_sens2$tau_median, 2),
                           " [95% CrI ", round(post_sens2$tau_lower, 2),
                           ", ", round(post_sens2$tau_upper, 2), "]")
  
  # ── Gráficos posteriores ─────────────────────────────────────────────────────
  plots_prim <- generate_posterior_plots(meta_RR_primary, "skyblue")
  plots_pred <- generate_posterior_plots(meta_RR_primary, "firebrick",  is_predict = TRUE)
  plots_s1   <- generate_posterior_plots(meta_sens1,      "forestgreen")
  plots_s2   <- generate_posterior_plots(meta_sens2,      "blue")
  
  # ── Probabilidades posteriores ───────────────────────────────────────────────
  p_benefit_avg  <- round(meta_RR_primary$pposterior(mu    = 0)                       * 100, 2)
  p_harm_avg     <- round((1 - meta_RR_primary$pposterior(mu    = 0))                 * 100, 2)
  p_benefit_pred <- round(meta_RR_primary$pposterior(theta = 0, predict = TRUE)       * 100, 2)
  p_harm_pred    <- round((1 - meta_RR_primary$pposterior(theta = 0, predict = TRUE)) * 100, 2)
  
  n_studies     <- nrow(dat_RR)
  forest_height <- max(10, n_studies * 0.4 + 5)
  sens_height   <- max(8,  3 * 0.6 + 4)
  
  # ════════════════════════════════════════════════════════════════════════════
  # EXPORTAÇÕES — FASE 1
  # ════════════════════════════════════════════════════════════════════════════
  
  # Figure 1 — Forest plot (análise primária + intervalo de predição Bayesiano)
  save_plot("Figure_1_Forest_Primary", function() {
    meta::forest(
      mf_prim_pred,
      layout        = "RevMan5",
      text.random   = "Bayesian Summary",
      text.predict  = "Bayesian 95% Prediction Interval",
      text.addline1 = tau_prim_text,
      smlab         = "Risk Ratio",
      leftcols      = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"),
      leftlabs      = c("Study", NA, NA, NA, NA, "RR"),
      pooled.events = TRUE,
      label.e       = "Intervention",
      label.c       = "Control",
      label.left    = "Favors Intervention",
      label.right   = "Favors Control",
      colgap        = "6mm"
    )
  }, out_dir_main, height = forest_height)
  
  # Figure 2 — Distribuição posterior (análise primária)
  save_plot("Figure_2_Posterior_Dist", function() {
    print(plots_prim$density + plots_prim$probability)
  }, out_dir_main)
  
  save_docx("Posterior_Probabilities", c(
    paste0("Outcome: ", sheet),
    paste(rep("=", 60), collapse = ""),
    "",
    "Primary Analysis — Posterior Probabilities",
    paste(rep("-", 50), collapse = ""),
    paste0("P(RR < 1) — Prob. of Average Benefit:     ", p_benefit_avg,  "%"),
    paste0("P(RR > 1) — Prob. of Average Harm:        ", p_harm_avg,     "%"),
    paste0("P(RR < 1) — Prob. of Predictive Benefit:  ", p_benefit_pred, "%"),
    paste0("P(RR > 1) — Prob. of Predictive Harm:     ", p_harm_pred,    "%")
  ), out_dir_main)
  
  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet),
    paste(rep("=", 60), collapse = ""),
    "",
    "=== Primary Model ===",
    capture.output(print(meta_RR_primary$summary)),
    "",
    "=== Sensitivity 1: Vague Tau ===",
    capture.output(print(meta_sens1$summary)),
    "",
    "=== Sensitivity 2: Weakly-Informative Mu ===",
    capture.output(print(meta_sens2$summary))
  ), out_dir_main)
  
  # ── Supplementary Methods ────────────────────────────────────────────────────
  save_plot("01_Primary_Priors", function() {
    print(plot_overall_vague_prior / plot_tau_weakly_prior)
  }, out_dir_meth, height = 12)
  
  save_plot("02_Sensitivity_1_Priors", function() {
    print(plot_overall_vague_prior / plot_tau_vague_prior)
  }, out_dir_meth, height = 12)
  
  save_plot("03_Sensitivity_2_Priors", function() {
    print(plot_overall_weakly_prior / plot_tau_weakly_prior)
  }, out_dir_meth, height = 12)
  
  # ── Supplementary Results ────────────────────────────────────────────────────
  save_plot("Supp_Fig_1_Posterior_Predictive", function() {
    print(plots_pred$density + plots_pred$probability)
  }, out_dir_resu)
  
  save_plot("Supp_Fig_2_Sens_Overall_Effect", function() {
    meta::forest(all_overall, smlab = "Overall Effect (RR)", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)
  
  save_plot("Supp_Fig_3_Sens_Distributions", function() {
    print(
      (plots_s1$density + plots_s1$probability) /
        (plots_s2$density + plots_s2$probability)
    )
  }, out_dir_resu, height = 12)
  
  save_plot("Supp_Fig_4_Sens_Predictive", function() {
    meta::forest(all_predict, smlab = "95% Prediction Interval", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)
  
  save_docx("Supp_Table_1_Heterogeneity", c(
    paste0("Outcome: ", sheet),
    "Supplementary Table 1 — Posterior Distribution: Between-Study Heterogeneity",
    paste(rep("-", 80), collapse = ""),
    paste0("Primary (Weakly-Informative Tau):  ", tau_prim_text),
    paste0("Sensitivity 1 (Vague Tau):         ", tau_sens1_text),
    paste0("Sensitivity 2 (Weakly-Inform. Mu): ", tau_sens2_text)
  ), out_dir_resu)
  
  # ════════════════════════════════════════════════════════════════════════════
  # FASE 2 — LOO automático: acionado quando tau_median > 0.3
  # tryCatch isolado: erro no LOO não interrompe nem desfaz os outputs da Fase 1
  # ════════════════════════════════════════════════════════════════════════════
  if (post_prim$tau_median > 0.3) {
    tryCatch(
      run_loo_analysis(
        dat_RR         = dat_RR,
        full_model     = meta_RR_primary,
        mu_mean        = prior_mu_vague_mean_RR,
        mu_sd          = prior_mu_vague_sd_RR,
        tau_prior_func = prior_tau_primary_func,
        out_dir        = out_dir_resu,
        sheet_name     = sheet
      ),
      error = function(e) log_error(paste0("LOO — ", sheet), e$message)
    )
  }
  
  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# ► EXECUTAR
# ══════════════════════════════════════════════════════════════════════════════

all_sheets <- sheet_names(url)

for (aba in all_sheets) {
  tryCatch(
    run_bayesian_meta(aba),
    error = function(e) log_error(aba, e$message)
  )
}