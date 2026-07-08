# ==============================================================================
# MetaBayes — Bayesian Meta-Analysis | Google Sheets Automation
# Desfecho Binário (Risk Ratio) + Desfecho Contínuo (SMD)
# Detecção automática do tipo de desfecho por nomes de coluna
# Primary Analysis (Rhodes et al. Prior) + Sensitivity Analysis (Half-Normal)
# ==============================================================================

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0 — CONFIGURAÇÃO
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
url        <- "https://docs.google.com/spreadsheets/d/1OelEZPOfXezFtsF4qHp6X-JvSIgrR23-o7Lgpz0rRV0/edit?gid=1304369423#gid=1304369423"
output_dir <- "/Users/davibarbosa/Desktop/TCC - ELA/Estatística"

# ── Inversão de rótulos do forest (TRUE = inverter label.left ↔ label.right) ─
invert_labels_config <- c(
   "SVC + FVC" = TRUE,
   "SVC + FVC 2º geração" = TRUE
)

# ── Labels Globais da Intervenção ───────────────────────────────────────────
label_e <- "FSTAs"
label_c <- "Placebo"
label_left  <- "Favors FSTAs"
label_right <- "Favors Placebo"

# ── Priors: Desfecho Binário (RR) ───────────────────────────────────────────
prior_mu_vague_mean_RR    <- 0      
prior_mu_vague_sd_RR      <- 2.7
prior_tau_weakly_scale_RR <- 0.5    
vague_prior_RR_scale      <- 1.5    
prior_mu_weakly_mean_RR   <- 0      
prior_mu_weakly_sd_RR     <- 0.82

# ── Priors: Desfecho Contínuo (SMD) ─────────────────────────────────────────
prior_mu_vague_mean_SMD    <- 0
prior_mu_vague_sd_SMD      <- 510   

# Tau Primário: INFORMATIVO (Rhodes et al., 2015)
prior_tau_info_loc   <- -3.16
prior_tau_info_scale <- 2.50
prior_tau_info_df    <- 5

# Tau Sensibilidade: WEAKLY INFORMATIVE
prior_tau_weakly_scale_SMD <- 0.5   

smd_x_range <- c(-5, 5)
smd_x_by    <- 1
smd_x_label <- "Standardized Mean Difference"

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §1 — PACOTES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
if (!require("devtools")) install.packages("devtools")
if (!require("pacman"))   install.packages("pacman")
if (!requireNamespace("BayesianPriorsTool", quietly = TRUE))
  devtools::install_github("arthur-albuquerque/BayesianPriorsTool")

pacman::p_load(
  googlesheets4, ragg, officer, bayesmeta, BayesianPriorsTool, 
  dplyr, tidyr, metafor, ggdist, ggplot2, patchwork, meta, janitor
)

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §2 — AUTENTICAÇÃO E TEMA
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
gs4_auth()
theme_set(
  theme(
    plot.title.position = "plot",
    axis.ticks.x        = element_blank(), axis.ticks.y = element_blank(),
    axis.text.x         = element_text(size = 12), axis.text.y = element_text(size = 12),
    axis.title.x        = element_text(size = 14),
    panel.background    = element_blank(),
    panel.grid.major.x  = element_line(color = "gray80", linewidth = 0.3)
  )
)

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §3 — FUNÇÕES AUXILIARES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
log_error <- function(context, error_msg) {
  log_path <- file.path(output_dir, "pipeline_errors.log")
  write(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", context, " — ", error_msg), file = log_path, append = TRUE)
}

save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10) {
  agg_tiff(file.path(out_dir, paste0(filename_base, ".tiff")), width = width, height = height, units = "in", res = 300, compression = "lzw")
  plot_fn(); dev.off()
  agg_png(file.path(out_dir, paste0(filename_base, ".png")), width = width, height = height, units = "in", res = 300)
  plot_fn(); dev.off()
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 10)
  doc  <- officer::read_docx()
  for (line in lines) doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(as.character(line), prop)))
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
}

get_summary <- function(model) {
  list(
    mu_mean = model$summary["mean", "mu"], mu_upper = model$summary["95% upper", "mu"], mu_lower = model$summary["95% lower", "mu"],
    tau_median = model$summary["median", "tau"], tau_upper = model$summary["95% upper", "tau"], tau_lower = model$summary["95% lower", "tau"],
    theta_mean = model$summary["mean", "theta"], theta_upper = model$summary["95% upper", "theta"], theta_lower = model$summary["95% lower", "theta"]
  )
}

detect_outcome_type <- function(dat) {
  cols <- names(dat)
  binary_cols <- c("study", "ai", "n1i", "ci", "n2i")
  continuous_cols <- c("source", "n1i", "m1i", "sd1i", "n2i", "m2i", "sd2i")
  if (all(binary_cols %in% cols)) return("binary")
  if (all(continuous_cols %in% cols)) return("continuous")
  stop("Tipo de desfecho nao identificado.")
}

get_forest_labels <- function(sheet, left, right) {
  if (isTRUE(invert_labels_config[sheet])) list(left = right, right = left)
  else list(left = left, right = right)
}

# Transformação de densidade COM trava de underflow (integração segura)
prior_tau_informative_func <- function(tau) {
  res <- rep(0, length(tau))
  # O limite > 1e-3 previne a singularidade do Jacobiano (2/tau) no zero
  # que causa o erro "extremely bad integrand behaviour" no integrate() do R.
  valid <- tau > 1e-3 
  if (any(valid)) {
    x <- 2 * log(tau[valid])
    dens_x <- dt((x - prior_tau_info_loc) / prior_tau_info_scale, df = prior_tau_info_df) / prior_tau_info_scale
    res[valid] <- dens_x * (2 / tau[valid])
  }
  return(res)
}

generate_posterior_plots <- function(model, fill_color, is_predict = FALSE, x_range = NULL, x_by = NULL, x_lab = NULL, is_rr = FALSE) {
  set.seed(123)
  samples <- model$rposterior(n = 10000, tau.sample = FALSE, predict = is_predict)
  df_s <- data.frame(samples = samples)
  
  if (is_rr) {
    x_breaks <- if (is_predict) c(0.2, 0.5, 1, 2, 5) else c(0.5, 0.75, 1, 1.5, 2)
    x_coord  <- if (is_predict) c(0.2, 5) else c(0.4, 2.5)
    seq_by   <- if (is_predict) 1 else 0.4
    
    p_dens <- ggplot(df_s, aes(x = samples)) +
      ggdist::stat_halfeye(.width = 0.95, point_interval = "mean_hdi", fill = fill_color) +
      scale_x_continuous(breaks = log(x_breaks), labels = function(x) round(exp(x), 2)) +
      coord_cartesian(x = log(x_coord))
    
    cutoffs <- if (is_predict) log(seq(0.1, 5, by = 0.1)) else log(seq(0.2, 3, by = 0.02))
    probs   <- if (is_predict) model$pposterior(theta = cutoffs, predict = TRUE) else model$pposterior(mu = cutoffs)
    
    p_prob <- ggplot(data.frame(cutoffs, probs), aes(x = exp(cutoffs), y = probs)) + geom_line() +
      scale_x_continuous(breaks = seq(x_coord[1], x_coord[2], by = seq_by)) + coord_cartesian(x = x_coord)
    
    x_label_str <- "Risk Ratio"
    
  } else {
    cutoff_seq <- seq(x_range[1], x_range[2], by = 0.5)
    p_dens <- ggplot(df_s, aes(x = samples)) + ggdist::stat_halfeye(.width = 0.95, point_interval = "mean_hdi", fill = fill_color)
    probs  <- if (is_predict) model$pposterior(theta = cutoff_seq, predict = TRUE) else model$pposterior(mu = cutoff_seq)
    p_prob <- ggplot(data.frame(cutoffs = cutoff_seq, probs = probs), aes(x = cutoffs, y = probs)) + geom_line() +
      scale_x_continuous(breaks = seq(x_range[1], x_range[2], by = x_by))
    
    x_label_str <- x_lab
  }
  
  p_dens <- p_dens + labs(x = paste0("\n", x_label_str), y = "Density") + theme(axis.text.y = element_blank())
  p_prob <- p_prob + scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + labs(x = paste0("\n", x_label_str), y = expression("Probability" <= "X")) + theme(panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3))
  
  list(density = p_dens, probability = p_prob)
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §4 — ANÁLISE BINÁRIA (Risk Ratio)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
run_bayesian_meta_binary <- function(sheet, dat, base_dir = output_dir) {
  out_dir_main <- file.path(base_dir, sheet)
  out_dir_meth <- file.path(out_dir_main, "Supplementary_Methods")
  out_dir_resu <- file.path(out_dir_main, "Supplementary_Results")
  dir.create(out_dir_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir_resu, showWarnings = FALSE, recursive = TRUE)
  
  dat <- dat[!is.na(dat$study), ] |> 
    mutate(across(any_of(c("ai", "n1i", "ci", "n2i")), ~ as.numeric(as.character(.)))) |> 
    filter(!is.na(ai) & !is.na(ci)) |> 
    filter(ai > 0 | ci > 0)
  
  dat_RR <- escalc(measure = "RR", ai = ai, n1i = n1i, ci = ci, n2i = n2i, data = dat) |> data.frame()
  dat_RR$sei <- sqrt(dat_RR$vi)
  dat_RR <- dat_RR[is.finite(dat_RR$yi) & is.finite(dat_RR$sei) & dat_RR$sei > 0, ]
  
  if (nrow(dat_RR) < 2) stop("Menos de 2 estudos com eventos validos em '", sheet, "'.")
  
  prior_tau_primary_func <- function(x) dhalfnormal(x, scale = prior_tau_weakly_scale_RR)
  vague_tau_RR_func      <- function(x) dhalfnormal(x, scale = vague_prior_RR_scale)
  
  plot_mu_vague_prior   <- mu_dist_viz(mean = prior_mu_vague_mean_RR, sd = prior_mu_vague_sd_RR, scale = "relative", xlabel = "Risk Ratio")
  plot_tau_weakly_prior <- tau_dist_viz(dist = "half-normal", scale = prior_tau_weakly_scale_RR)
  plot_tau_vague_prior  <- tau_dist_viz(dist = "half-normal", scale = vague_prior_RR_scale)
  plot_mu_weakly_prior  <- mu_dist_viz(mean = prior_mu_weakly_mean_RR, sd = prior_mu_weakly_sd_RR, scale = "relative", xlabel = "Risk Ratio")
  
  meta_RR_primary <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei, labels = dat_RR$study,
                               mu.prior.mean = prior_mu_vague_mean_RR, mu.prior.sd = prior_mu_vague_sd_RR, tau.prior = prior_tau_primary_func)
  meta_sens1      <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei, labels = dat_RR$study,
                               mu.prior.mean = prior_mu_vague_mean_RR, mu.prior.sd = prior_mu_vague_sd_RR, tau.prior = vague_tau_RR_func)
  meta_sens2      <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei, labels = dat_RR$study,
                               mu.prior.mean = prior_mu_weakly_mean_RR, mu.prior.sd = prior_mu_weakly_sd_RR, tau.prior = prior_tau_primary_func)
  
  post_prim  <- get_summary(meta_RR_primary)
  post_sens1 <- get_summary(meta_sens1)
  post_sens2 <- get_summary(meta_sens2)
  
  forest_data_RR <- metabin(data = dat_RR, studlab = study, event.e = ai, n.e = n1i, event.c = ci, n.c = n2i, sm = "RR", common = FALSE, random = FALSE)
  
  mf_prim      <- metaadd(forest_data_RR, type = "random", TE = post_prim$mu_mean, upper = post_prim$mu_upper, lower = post_prim$mu_lower)
  mf_prim_pred <- metaadd(mf_prim, type = "prediction", TE = post_prim$theta_mean, upper = post_prim$theta_upper, lower = post_prim$theta_lower)
  mf_s1        <- metaadd(forest_data_RR, type = "random", TE = post_sens1$mu_mean, upper = post_sens1$mu_upper, lower = post_sens1$mu_lower)
  mf_s1_pred   <- metaadd(forest_data_RR, type = "prediction", TE = post_sens1$theta_mean, upper = post_sens1$theta_upper, lower = post_sens1$theta_lower)
  mf_s2        <- metaadd(forest_data_RR, type = "random", TE = post_sens2$mu_mean, upper = post_sens2$mu_upper, lower = post_sens2$mu_lower)
  mf_s2_pred   <- metaadd(forest_data_RR, type = "prediction", TE = post_sens2$theta_mean, upper = post_sens2$theta_upper, lower = post_sens2$theta_lower)
  
  analyses_names <- c("Primary Analysis: Weakly-Informative Tau", "Sensitivity 1: Vague Tau", "Sensitivity 2: Weakly-Informative Mu")
  all_overall <- metabind(mf_prim, mf_s1, mf_s2,                name = analyses_names)
  all_predict <- metabind(mf_prim_pred, mf_s1_pred, mf_s2_pred, name = analyses_names)
  
  tau_fmt <- function(p) paste0("τ = ", round(p$tau_median, 2), " [95% CrI ", round(p$tau_lower, 2), ", ", round(p$tau_upper, 2), "]")
  
  plots_prim <- generate_posterior_plots(meta_RR_primary, "skyblue", is_rr = TRUE)
  plots_pred <- generate_posterior_plots(meta_RR_primary, "firebrick", is_predict = TRUE, is_rr = TRUE)
  plots_s1   <- generate_posterior_plots(meta_sens1, "forestgreen", is_rr = TRUE)
  plots_s2   <- generate_posterior_plots(meta_sens2, "blue", is_rr = TRUE)
  
  p_benefit_avg  <- round(meta_RR_primary$pposterior(mu    = 0)                       * 100, 2)
  p_harm_avg     <- round((1 - meta_RR_primary$pposterior(mu    = 0))                 * 100, 2)
  p_benefit_pred <- round(meta_RR_primary$pposterior(theta = 0, predict = TRUE)       * 100, 2)
  p_harm_pred    <- round((1 - meta_RR_primary$pposterior(theta = 0, predict = TRUE)) * 100, 2)
  
  lbl <- get_forest_labels(sheet, label_left, label_right)
  forest_height <- max(10, nrow(dat_RR) * 0.4 + 5)
  
  save_plot("Figure_1_Forest_Primary", function() {
    meta::forest(mf_prim_pred, layout = "RevMan5", text.random = "Bayesian Summary", text.predict = "Bayesian 95% Prediction Interval",
                 text.addline1 = tau_fmt(post_prim), smlab = "Risk Ratio",
                 leftcols = c("studlab","event.e","n.e","event.c","n.c","effect.ci"), leftlabs = c("Study", NA, NA, NA, NA, "RR"),
                 pooled.events = TRUE, label.e = label_e, label.c = label_c, label.left = lbl$left, label.right = lbl$right, colgap = "6mm")
  }, out_dir_main, height = forest_height)
  
  save_plot("Figure_2_Posterior_Dist", function() print(plots_prim$density + plots_prim$probability), out_dir_main)
  
  save_docx("Posterior_Probabilities", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "", "Primary Analysis — Posterior Probabilities", paste(rep("-", 50), collapse = ""),
    paste0("P(RR < 1) — Prob. of Average Benefit:     ", p_benefit_avg,  "%"), paste0("P(RR > 1) — Prob. of Average Harm:        ", p_harm_avg,     "%"),
    paste0("P(RR < 1) — Prob. of Predictive Benefit:  ", p_benefit_pred, "%"), paste0("P(RR > 1) — Prob. of Predictive Harm:     ", p_harm_pred,    "%")
  ), out_dir_main)
  
  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    "=== Primary Model ===", capture.output(print(meta_RR_primary$summary)), "",
    "=== Sensitivity 1: Vague Tau ===", capture.output(print(meta_sens1$summary)), "",
    "=== Sensitivity 2: Weakly-Informative Mu ===", capture.output(print(meta_sens2$summary))
  ), out_dir_main)
  
  save_plot("01_Primary_Priors", function() print(plot_mu_vague_prior / plot_tau_weakly_prior), out_dir_meth, height = 12)
  save_plot("02_Sensitivity_1_Priors", function() print(plot_mu_vague_prior / plot_tau_vague_prior), out_dir_meth, height = 12)
  save_plot("03_Sensitivity_2_Priors", function() print(plot_mu_weakly_prior / plot_tau_weakly_prior), out_dir_meth, height = 12)
  save_plot("Supp_Fig_1_Posterior_Predictive", function() print(plots_pred$density + plots_pred$probability), out_dir_resu)
  save_plot("Supp_Fig_2_Sens_Overall_Effect", function() meta::forest(all_overall, smlab = "Overall Effect (RR)", leftcols = "studlab"), out_dir_resu, height = max(8, 3 * 0.6 + 4))
  save_plot("Supp_Fig_3_Sens_Distributions", function() print((plots_s1$density + plots_s1$probability) / (plots_s2$density + plots_s2$probability)), out_dir_resu, height = 12)
  save_plot("Supp_Fig_4_Sens_Predictive", function() meta::forest(all_predict, smlab = "95% Prediction Interval", leftcols = "studlab"), out_dir_resu, height = max(8, 3 * 0.6 + 4))
  save_docx("Supp_Table_1_Heterogeneity", c(
    paste0("Outcome: ", sheet), "Supplementary Table 1 — Between-Study Heterogeneity", paste(rep("-", 80), collapse = ""),
    paste0("Primary (Weakly-Informative Tau):  ", tau_fmt(post_prim)), paste0("Sensitivity 1 (Vague Tau):         ", tau_fmt(post_sens1)),
    paste0("Sensitivity 2 (Weakly-Inform. Mu): ", tau_fmt(post_sens2))
  ), out_dir_resu)
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §5 — ANÁLISE CONTÍNUA (Standardized Mean Difference)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
run_bayesian_meta_smd <- function(sheet, dat, base_dir = output_dir) {
  out_dir_main <- file.path(base_dir, sheet)
  out_dir_meth <- file.path(out_dir_main, "Supplementary_Methods")
  out_dir_resu <- file.path(out_dir_main, "Supplementary_Results")
  dir.create(out_dir_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir_resu, showWarnings = FALSE, recursive = TRUE)
  
  dat <- dat[!is.na(dat$source), ] |> 
    mutate(across(any_of(c("n1i", "m1i", "sd1i", "n2i", "m2i", "sd2i")), ~ as.numeric(as.character(.)))) |> 
    filter(!is.na(m1i) & !is.na(m2i))
  
  dat_SMD <- escalc(measure = "SMD", m1i = m1i, sd1i = sd1i, n1i = n1i, m2i = m2i, sd2i = sd2i, n2i = n2i, data = dat) |> data.frame()
  dat_SMD$sei <- sqrt(dat_SMD$vi)
  dat_SMD <- dat_SMD[is.finite(dat_SMD$yi) & is.finite(dat_SMD$sei) & dat_SMD$sei > 0, ]
  
  if (nrow(dat_SMD) < 2) stop("Menos de 2 estudos validos em '", sheet, "'.")
  
  prior_tau_weakly_func <- function(x) dhalfnormal(x, scale = prior_tau_weakly_scale_SMD)
  
  plot_mu_vague_prior <- mu_dist_viz(mean = prior_mu_vague_mean_SMD, sd = prior_mu_vague_sd_SMD, scale = "absolute", xlabel = smd_x_label)
  plot_tau_weakly_prior <- tau_dist_viz(dist = "half-normal", scale = prior_tau_weakly_scale_SMD)
  
  plot_tau_informative_prior <- ggplot(data.frame(x = c(0, 3)), aes(x)) +
    stat_function(fun = prior_tau_informative_func, color = "steelblue", linewidth = 1) +
    labs(x = expression(tau), y = "Density", title = "Informative Prior (Rhodes et al. 2015)", subtitle = "log-t(-3.16, 2.50^2, 5)") +
    theme_minimal()
  
  meta_SMD_primary <- bayesmeta(y = dat_SMD$yi, sigma = dat_SMD$sei, labels = dat_SMD$source,
                                mu.prior.mean = prior_mu_vague_mean_SMD, mu.prior.sd = prior_mu_vague_sd_SMD, 
                                tau.prior = prior_tau_informative_func)
  
  meta_sens <- bayesmeta(y = dat_SMD$yi, sigma = dat_SMD$sei, labels = dat_SMD$source,
                         mu.prior.mean = prior_mu_vague_mean_SMD, mu.prior.sd = prior_mu_vague_sd_SMD, 
                         tau.prior = prior_tau_weakly_func)
  
  post_prim <- get_summary(meta_SMD_primary)
  post_sens <- get_summary(meta_sens)
  
  forest_data_SMD <- metacont(data = dat_SMD, studlab = source, mean.e = m1i, sd.e = sd1i, n.e = n1i, mean.c = m2i, sd.c = sd2i, n.c = n2i,
                              sm = "SMD", common = FALSE, random = FALSE)
  
  mf_prim      <- metaadd(forest_data_SMD, type = "random", TE = post_prim$mu_mean, upper = post_prim$mu_upper, lower = post_prim$mu_lower)
  mf_prim_pred <- metaadd(mf_prim, type = "prediction", TE = post_prim$theta_mean, upper = post_prim$theta_upper, lower = post_prim$theta_lower)
  mf_s1        <- metaadd(forest_data_SMD, type = "random", TE = post_sens$mu_mean, upper = post_sens$mu_upper, lower = post_sens$mu_lower)
  mf_s1_pred   <- metaadd(forest_data_SMD, type = "prediction", TE = post_sens$theta_mean, upper = post_sens$theta_upper, lower = post_sens$theta_lower)
  
  analyses_names <- c("Primary Analysis: Informative Tau (Rhodes)", "Sensitivity: Weakly-Informative Tau (HN 0.5)")
  all_overall <- metabind(mf_prim, mf_s1,           name = analyses_names)
  all_predict <- metabind(mf_prim_pred, mf_s1_pred, name = analyses_names)
  
  tau_fmt <- function(p) paste0("τ = ", round(p$tau_median, 2), " [95% CrI ", round(p$tau_lower, 2), ", ", round(p$tau_upper, 2), "]")
  
  plots_prim <- generate_posterior_plots(meta_SMD_primary, "skyblue", x_range = smd_x_range, x_by = smd_x_by, x_lab = smd_x_label, is_rr = FALSE)
  plots_pred <- generate_posterior_plots(meta_SMD_primary, "firebrick", is_predict = TRUE, x_range = smd_x_range, x_by = smd_x_by, x_lab = smd_x_label, is_rr = FALSE)
  plots_s1   <- generate_posterior_plots(meta_sens, "forestgreen", x_range = smd_x_range, x_by = smd_x_by, x_lab = smd_x_label, is_rr = FALSE)
  
  p_mu_le0    <- round(meta_SMD_primary$pposterior(mu    = 0)                       * 100, 2)
  p_mu_gt0    <- round((1 - meta_SMD_primary$pposterior(mu    = 0))                 * 100, 2)
  p_theta_le0 <- round(meta_SMD_primary$pposterior(theta = 0, predict = TRUE)       * 100, 2)
  p_theta_gt0 <- round((1 - meta_SMD_primary$pposterior(theta = 0, predict = TRUE)) * 100, 2)
  
  lbl <- get_forest_labels(sheet, label_left, label_right)
  forest_height <- max(10, nrow(dat_SMD) * 0.4 + 5)
  sens_height   <- max(8,  2 * 0.6 + 4)
  
  save_plot("Figure_1_Forest_Primary", function() {
    meta::forest(mf_prim_pred, layout = "RevMan5", sortvar = dat_SMD$source,
                 text.random = "Bayesian Summary", text.predict = "Bayesian 95% Prediction Interval",
                 text.addline1 = tau_fmt(post_prim), smlab = "Standardized Mean Difference", digits.mean = 2, digits.sd = 2,
                 leftcols = c("studlab","mean.e","sd.e","n.e","mean.c","sd.c","n.c","effect.ci"), leftlabs = c("Study", NA, NA, NA, NA, NA, NA, "SMD"),
                 pooled.events = TRUE, label.e = label_e, label.c = label_c, label.left = lbl$left, label.right = lbl$right, colgap = "6mm")
  }, out_dir_main, height = forest_height)
  
  save_plot("Figure_2_Posterior_Dist", function() print(plots_prim$density + plots_prim$probability), out_dir_main)
  
  save_docx("Posterior_Probabilities", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "", "Primary Analysis — Posterior Probabilities", paste(rep("-", 50), collapse = ""),
    paste0("P(SMD <= 0): ", p_mu_le0,    "%"), paste0("P(SMD >  0): ", p_mu_gt0,    "%"),
    paste0("P(theta <= 0) [predictive]: ", p_theta_le0, "%"), paste0("P(theta >  0) [predictive]: ", p_theta_gt0, "%")
  ), out_dir_main)
  
  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    "=== Primary Model (Informative Tau) ===", capture.output(print(meta_SMD_primary$summary)), "",
    "=== Sensitivity (Weakly Tau HN 0.5) ===", capture.output(print(meta_sens$summary))
  ), out_dir_main)
  
  save_plot("01_Primary_Priors", function() print(plot_mu_vague_prior / plot_tau_informative_prior), out_dir_meth, height = 12)
  save_plot("02_Sensitivity_Priors", function() print(plot_mu_vague_prior / plot_tau_weakly_prior), out_dir_meth, height = 12)
  save_plot("Supp_Fig_1_Posterior_Predictive", function() print(plots_pred$density + plots_pred$probability), out_dir_resu)
  save_plot("Supp_Fig_2_Sens_Overall_Effect", function() meta::forest(all_overall, smlab = "Overall Effect (SMD)", leftcols = "studlab"), out_dir_resu, height = sens_height)
  save_plot("Supp_Fig_3_Sens_Distributions", function() print((plots_s1$density + plots_s1$probability)), out_dir_resu, height = 12)
  save_plot("Supp_Fig_4_Sens_Predictive", function() meta::forest(all_predict, smlab = "95% Prediction Interval", leftcols = "studlab"), out_dir_resu, height = sens_height)
  save_docx("Supp_Table_1_Heterogeneity", c(
    paste0("Outcome: ", sheet), "Supplementary Table 1 — Between-Study Heterogeneity", paste(rep("-", 80), collapse = ""),
    paste0("Primary (Informative Tau Rhodes):  ", tau_fmt(post_prim)), paste0("Sensitivity (Weakly Tau HN 0.5):   ", tau_fmt(post_sens))
  ), out_dir_resu)
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §6 — DISPATCHER
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
run_bayesian_meta <- function(sheet, base_dir = output_dir) {
  dat <- tryCatch({
    as.data.frame(read_sheet(url, sheet = sheet)) |> janitor::clean_names()
  }, error = function(e) { log_error(paste0("read_sheet — ", sheet), e$message); NULL })
  
  if (is.null(dat)) return(invisible(NULL))
  type <- tryCatch(detect_outcome_type(dat), error = function(e) { log_error(paste0("detect — ", sheet), e$message); NA_character_ })
  if (is.na(type)) return(invisible(NULL))
  
  if (type == "binary") run_bayesian_meta_binary(sheet, dat, base_dir)
  else                  run_bayesian_meta_smd(sheet, dat, base_dir)
}

# ══════════════════════════════════════════════════════════════════════════════
# ► EXECUTAR
# ══════════════════════════════════════════════════════════════════════════════
all_sheets <- sheet_names(url)
for (aba in all_sheets) {
  tryCatch({
    run_bayesian_meta(aba)
  }, error = function(e) {
    log_error(aba, e$message)
    graphics.off() 
  })
}