# ==============================================================================
# MetaBayes — Bayesian Meta-Analysis + LOO | Google Sheets Automation
# Desfecho Binário (Risk Ratio) + Desfecho Contínuo (Mean Difference)
# Detecção automática do tipo de desfecho por nomes de coluna
# Phase 1: Primary Analysis + Sensitivity Analyses
# Phase 2: Leave-One-Out (LOO) — controle manual via loo_config
# ==============================================================================


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0 — CONFIGURAÇÃO
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

url        <- "https://docs.google.com/spreadsheets/d/1aHE6ndgJdPnjffEx9f8SxSUxQulg_Rsad9XOSliwVNE/edit?gid=159106587#gid=159106587"
output_dir <- "/Users/davibarbosa/Desktop/Joseli"

# ── Phase 2 — LOO (TRUE = executar / FALSE = não executar) ──────────────────
# Aplica-se a desfechos binários E contínuos
loo_config <- c(
  # "Nome_da_Aba" = TRUE,
  # "Outra_Aba"   = FALSE
)

# ── Inversão de rótulos do forest (TRUE = inverter label.left ↔ label.right) ─
invert_labels_config <- c(
  # "Nome_da_Aba" = TRUE
)

# ── Priors: Desfecho Binário (RR) ───────────────────────────────────────────
prior_mu_vague_mean_RR    <- 0      # Mu primário: Normal(0, 2.7²)
prior_mu_vague_sd_RR      <- 2.7
prior_tau_weakly_scale_RR <- 0.5   # Tau primário: Half-Normal(0.5)
vague_prior_RR_scale      <- 1.5   # Tau sensibilidade 1: Half-Normal(1.5)
prior_mu_weakly_mean_RR   <- 0     # Mu sensibilidade 2: Normal(0, 0.82²)
prior_mu_weakly_sd_RR     <- 0.82
label_e_RR     <- "Paclitaxel"
label_c_RR     <- "Sirolimus"
label_left_RR  <- "Favors Paclitaxel"
label_right_RR <- "Favors Sirolimus"

# ── Priors: Desfecho Contínuo (MD) ──────────────────────────────────────────
# Mu primário: sempre Normal(0, 510²) — fixo para todos os desfechos
prior_mu_vague_mean_MD <- 0
prior_mu_vague_sd_MD   <- 510
# Tau sensibilidade: sempre Half-Normal(4) — fixo para todos os desfechos
vague_tau_MD_scale     <- 4
# Tau primário e Mu sensibilidade → definidos interativamente por aba
# Eixo X dos gráficos de probabilidade — >>> AJUSTE conforme a escala <<<
md_x_range <- c(-20, 20)
md_x_by    <- 5
md_x_label <- "Mean Difference (SVC %)"  # >>> AJUSTE <<<
label_e_MD     <- "FSTAs"
label_c_MD     <- "Placebo"
label_left_MD  <- "Favors Placebo"
label_right_MD <- "Favors FSTAs"


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §1 — PACOTES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

if (!require("devtools")) install.packages("devtools")
if (!require("pacman"))   install.packages("pacman")

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
# §3 — FUNÇÕES AUXILIARES (compartilhadas)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

log_error <- function(context, error_msg) {
  log_path <- file.path(output_dir, "pipeline_errors.log")
  write(
    paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
           context, " — ", error_msg),
    file = log_path, append = TRUE
  )
}

save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10) {
  agg_tiff(file.path(out_dir, paste0(filename_base, ".tiff")),
           width = width, height = height, units = "in",
           res = 300, compression = "lzw")
  plot_fn(); dev.off()

  agg_png(file.path(out_dir, paste0(filename_base, ".png")),
          width = width, height = height, units = "in", res = 300)
  plot_fn(); dev.off()
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 10)
  doc  <- officer::read_docx()
  for (line in lines)
    doc <- officer::body_add_fpar(
      doc, officer::fpar(officer::ftext(as.character(line), prop))
    )
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
}

# "mean" para mu e theta; "median" para tau (assimétrico à direita)
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

# Detecta o tipo de desfecho pelas colunas da aba
detect_outcome_type <- function(dat) {
  cols            <- names(dat)
  binary_cols     <- c("study", "ai", "n1i", "ci", "n2i")
  continuous_cols <- c("source", "n1i", "m1i", "sd1i", "n2i", "m2i", "sd2i")
  if (all(binary_cols     %in% cols)) return("binary")
  if (all(continuous_cols %in% cols)) return("continuous")
  stop("Tipo de desfecho nao identificado. Esperado: ",
       paste(binary_cols, collapse = ", "), " (binario) ou ",
       paste(continuous_cols, collapse = ", "), " (continuo). ",
       "Encontrado: ", paste(cols, collapse = ", "))
}

# ── Controles manuais ────────────────────────────────────────────────────────

should_run_loo <- function(sheet) isTRUE(loo_config[sheet])

get_forest_labels <- function(sheet, left, right) {
  if (isTRUE(invert_labels_config[sheet]))
    list(left = right, right = left)
  else
    list(left = left,  right = right)
}

# ── Prompt interativo: priors do desfecho contínuo ───────────────────────────

read_positive_number <- function(prompt_text) {
  repeat {
    val <- suppressWarnings(as.numeric(trimws(readline(prompt_text))))
    if (!is.na(val) && val > 0) return(val)
    message("  Valor invalido. Digite um numero positivo.")
  }
}

prompt_continuous_priors <- function(sheet) {
  sep <- paste(rep("━", 62), collapse = "")
  message("\n", sep)
  message("Desfecho continuo detectado: \"", sheet, "\"")
  message(sep)

  message("\n[TAU PRIMARIO] Weakly-Informative — Half-Normal")
  message("  Qual a diferenca extrema plausivel entre estudos?")
  extreme_tau  <- read_positive_number("  extreme_diff para tau primario: ")
  scale_tau    <- tau_prior_scale(extreme_tau)
  message("  => Half-Normal(scale = ", round(scale_tau, 4), ")")

  message("\n[MU SENSIBILIDADE] Weakly-Informative — Normal(0, SD^2)")
  message("  Qual a diferenca extrema plausivel para o efeito global?")
  extreme_mu   <- read_positive_number("  extreme_diff para mu sensibilidade: ")
  sd_mu_sens   <- mu_prior_sd(extreme_mu)
  message("  => Normal(0, ", round(sd_mu_sens, 4), "^2)")

  message("\n[TAU SENSIBILIDADE] Half-Normal(", vague_tau_MD_scale,
          ") — fixo para todos os desfechos")
  message(sep, "\n")

  list(extreme_tau = extreme_tau,
       scale_tau   = scale_tau,
       extreme_mu  = extreme_mu,
       sd_mu_sens  = sd_mu_sens)
}

# ── Gráficos posteriores: escala relativa (RR) ──────────────────────────────
generate_posterior_plots_rr <- function(model, fill_color, is_predict = FALSE) {
  set.seed(123)
  samples <- model$rposterior(n = 10000, tau.sample = FALSE, predict = is_predict)
  df_s    <- data.frame(samples = samples)

  if (is_predict) {
    x_breaks <- c(0.2, 0.5, 1, 2, 5); x_coord <- c(0.2, 5); seq_by <- 1
  } else {
    x_breaks <- c(0.5, 0.75, 1, 1.5, 2); x_coord <- c(0.4, 2.5); seq_by <- 0.4
  }

  p_dens <- ggplot(df_s, aes(x = samples)) +
    ggdist::stat_halfeye(.width = 0.95, point_interval = "mean_hdi",
                         fill = fill_color) +
    scale_x_continuous(breaks = log(x_breaks),
                       labels = function(x) round(exp(x), 2)) +
    labs(x = "\nRisk Ratio", y = "Density") +
    theme(axis.text.y = element_blank()) +
    coord_cartesian(x = log(x_coord))

  cutoffs <- if (is_predict) log(seq(0.1, 5,  by = 0.1)) else log(seq(0.2, 3, by = 0.02))
  probs   <- if (is_predict) model$pposterior(theta = cutoffs, predict = TRUE) else
    model$pposterior(mu = cutoffs)

  p_prob <- ggplot(data.frame(cutoffs, probs), aes(x = exp(cutoffs), y = probs)) +
    geom_line() +
    scale_x_continuous(breaks = seq(x_coord[1], x_coord[2], by = seq_by)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    coord_cartesian(x = x_coord) +
    labs(x = "\nRisk Ratio", y = expression("Probability RR" <= "X")) +
    theme(panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3))

  list(density = p_dens, probability = p_prob)
}

# ── Gráficos posteriores: escala absoluta (MD) ──────────────────────────────
generate_posterior_plots_md <- function(model, fill_color, is_predict = FALSE,
                                        x_range = md_x_range,
                                        x_by    = md_x_by,
                                        x_lab   = md_x_label) {
  set.seed(123)
  samples    <- model$rposterior(n = 10000, tau.sample = FALSE, predict = is_predict)
  df_s       <- data.frame(samples = samples)
  cutoff_seq <- seq(x_range[1], x_range[2], by = 1)

  p_dens <- ggplot(df_s, aes(x = samples)) +
    ggdist::stat_halfeye(.width = 0.95, point_interval = "mean_hdi",
                         fill = fill_color) +
    labs(x = paste0("\n", x_lab), y = "Density") +
    theme(axis.text.y = element_blank())

  probs <- if (is_predict) model$pposterior(theta = cutoff_seq, predict = TRUE) else
    model$pposterior(mu = cutoff_seq)

  p_prob <- ggplot(data.frame(cutoffs = cutoff_seq, probs = probs),
                   aes(x = cutoffs, y = probs)) +
    geom_line() +
    scale_x_continuous(breaks = seq(x_range[1], x_range[2], by = x_by)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    labs(x = paste0("\n", x_lab), y = expression("Probability MD" <= "X")) +
    theme(panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3))

  list(density = p_dens, probability = p_prob)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §4 — FASE 2: Leave-One-Out (LOO) — binário e contínuo
# Acionada manualmente via loo_config em §0
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_loo_analysis <- function(dat, full_model,
                             mu_mean, mu_sd, tau_prior_func,
                             label_col,   # "study" (binário) | "source" (contínuo)
                             scale,       # "RR" | "MD"
                             out_dir, sheet_name) {
  K      <- nrow(dat)
  is_rr  <- (scale == "RR")
  eff_lb <- if (is_rr) "RR" else "MD"

  plan(multisession)
  on.exit(plan(sequential), add = TRUE)

  loo_list <- future_lapply(seq_len(K), function(i) {
    dat_loo <- dat[-i, ]
    fit     <- bayesmeta(y             = dat_loo$yi,
                         sigma         = dat_loo$sei,
                         labels        = dat_loo[[label_col]],
                         mu.prior.mean = mu_mean,
                         mu.prior.sd   = mu_sd,
                         tau.prior     = tau_prior_func)
    list(study_excluded = dat[[label_col]][i],
         estimate       = fit$summary["median",    "mu"],
         ci_lower       = fit$summary["95% lower", "mu"],
         ci_upper       = fit$summary["95% upper", "mu"],
         tau_median     = fit$summary["median",    "tau"])
  }, future.seed = TRUE)

  loo_res <- data.frame(
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

  m_ref <- metagen(
    sm      = scale,
    TE      = c(loo_res$estimate, full_mu),
    lower   = c(loo_res$ci_lower, full_mu_lo),
    upper   = c(loo_res$ci_upper, full_mu_hi),
    studlab = c(paste0("Omitting: ", loo_res$study_excluded),
                "Full Analysis (Reference)"),
    random  = FALSE, fixed = FALSE
  )

  sq_col <- c(rep("steelblue", K), "firebrick")
  loo_h  <- max(10, (K + 1) * 0.45 + 5)

  save_plot("Supp_Fig_5_LOO_Forest", function() {
    forest_args <- list(
      m_ref,
      leftcols         = "studlab", leftlabs  = "Study",
      rightcols        = c("effect", "ci"),
      rightlabs        = c(eff_lb, "95% CrI"),
      xlab             = if (is_rr) "Risk Ratio" else "Mean Difference",
      smlab            = "", weight.study = "same",
      col.square       = sq_col, col.square.lines = sq_col,
      overall          = FALSE, print.tau2 = FALSE, print.I2 = FALSE,
      print.pval.Q     = FALSE, hetstat    = FALSE, overall.hetstat = FALSE
    )
    if (is_rr) forest_args$func.transf <- exp
    do.call(meta::forest, forest_args)
  }, out_dir, height = loo_h)

  # Formata valores para tabela textual
  fmt <- if (is_rr) {
    function(e, lo, hi) sprintf("%6.2f [%6.2f, %6.2f]", exp(e), exp(lo), exp(hi))
  } else {
    function(e, lo, hi) sprintf("%6.2f [%6.2f, %6.2f]", e, lo, hi)
  }

  hdr  <- sprintf("%-32s | %-22s | %s", "Study Excluded",
                  paste0(eff_lb, " [95% CrI]"), "tau (median)")
  rows <- vapply(seq_len(nrow(loo_res)), function(i) {
    r <- loo_res[i, ]
    sprintf("%-32s | %-22s | %.3f",
            r$study_excluded, fmt(r$estimate, r$ci_lower, r$ci_upper), r$tau_median)
  }, character(1))
  ref_row <- sprintf("%-32s | %-22s | %s",
                     "Full Analysis (Reference)", fmt(full_mu, full_mu_lo, full_mu_hi), "—")

  save_docx("Supp_Table_2_LOO_Results", c(
    paste0("LOO Sensitivity Analysis: ", sheet_name),
    paste(rep("-", 72), collapse = ""),
    hdr,
    paste(rep("-", 72), collapse = ""),
    rows,
    paste(rep("-", 72), collapse = ""),
    ref_row
  ), out_dir)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §5 — ANÁLISE BINÁRIA (Risk Ratio)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_bayesian_meta_binary <- function(sheet, dat, base_dir = output_dir) {

  out_dir_main <- file.path(base_dir, sheet)
  out_dir_meth <- file.path(out_dir_main, "Supplementary_Methods")
  out_dir_resu <- file.path(out_dir_main, "Supplementary_Results")
  dir.create(out_dir_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir_resu, showWarnings = FALSE, recursive = TRUE)

  # ── Dados ───────────────────────────────────────────────────────────────────
  dat    <- dat[!is.na(dat$study), ]
  dat_RR <- escalc(measure = "RR",
                   ai = ai, n1i = n1i, ci = ci, n2i = n2i,
                   data = dat) |> data.frame()
  dat_RR$sei <- sqrt(dat_RR$vi)
  dat_RR     <- dat_RR[is.finite(dat_RR$yi) & is.finite(dat_RR$sei) & dat_RR$sei > 0, ]

  if (nrow(dat_RR) < 2)
    stop("Menos de 2 estudos validos em '", sheet, "'.")

  # ── Priors ──────────────────────────────────────────────────────────────────
  prior_tau_primary_func <- function(x) dhalfnormal(x, scale = prior_tau_weakly_scale_RR)
  vague_tau_RR_func      <- function(x) dhalfnormal(x, scale = vague_prior_RR_scale)

  plot_mu_vague_prior   <- mu_dist_viz(mean = prior_mu_vague_mean_RR,
                                       sd   = prior_mu_vague_sd_RR,
                                       scale = "relative", xlabel = "Risk Ratio")
  plot_tau_weakly_prior <- tau_dist_viz(dist = "half-normal",
                                        scale = prior_tau_weakly_scale_RR)
  plot_tau_vague_prior  <- tau_dist_viz(dist = "half-normal",
                                        scale = vague_prior_RR_scale)
  plot_mu_weakly_prior  <- mu_dist_viz(mean = prior_mu_weakly_mean_RR,
                                       sd   = prior_mu_weakly_sd_RR,
                                       scale = "relative", xlabel = "Risk Ratio")

  # ── Modelos ──────────────────────────────────────────────────────────────────
  meta_RR_primary <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei,
                               labels        = dat_RR$study,
                               mu.prior.mean = prior_mu_vague_mean_RR,
                               mu.prior.sd   = prior_mu_vague_sd_RR,
                               tau.prior     = prior_tau_primary_func)
  meta_sens1      <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei,
                               labels        = dat_RR$study,
                               mu.prior.mean = prior_mu_vague_mean_RR,
                               mu.prior.sd   = prior_mu_vague_sd_RR,
                               tau.prior     = vague_tau_RR_func)
  meta_sens2      <- bayesmeta(y = dat_RR$yi, sigma = dat_RR$sei,
                               labels        = dat_RR$study,
                               mu.prior.mean = prior_mu_weakly_mean_RR,
                               mu.prior.sd   = prior_mu_weakly_sd_RR,
                               tau.prior     = prior_tau_primary_func)

  post_prim  <- get_summary(meta_RR_primary)
  post_sens1 <- get_summary(meta_sens1)
  post_sens2 <- get_summary(meta_sens2)

  # ── Forest base ──────────────────────────────────────────────────────────────
  forest_data_RR <- metabin(data = dat_RR, studlab = study,
                            event.e = ai, n.e = n1i,
                            event.c = ci, n.c = n2i,
                            sm = "RR", common = FALSE, random = FALSE)

  mf_prim      <- metaadd(forest_data_RR, type = "random",
                          TE = post_prim$mu_mean,
                          upper = post_prim$mu_upper, lower = post_prim$mu_lower)
  mf_prim_pred <- metaadd(mf_prim, type = "prediction",
                          TE = post_prim$theta_mean,
                          upper = post_prim$theta_upper, lower = post_prim$theta_lower)
  mf_s1        <- metaadd(forest_data_RR, type = "random",
                          TE = post_sens1$mu_mean,
                          upper = post_sens1$mu_upper, lower = post_sens1$mu_lower)
  mf_s1_pred   <- metaadd(forest_data_RR, type = "prediction",
                          TE = post_sens1$theta_mean,
                          upper = post_sens1$theta_upper, lower = post_sens1$theta_lower)
  mf_s2        <- metaadd(forest_data_RR, type = "random",
                          TE = post_sens2$mu_mean,
                          upper = post_sens2$mu_upper, lower = post_sens2$mu_lower)
  mf_s2_pred   <- metaadd(forest_data_RR, type = "prediction",
                          TE = post_sens2$theta_mean,
                          upper = post_sens2$theta_upper, lower = post_sens2$theta_lower)

  analyses_names <- c("Primary Analysis: Weakly-Informative Tau",
                      "Sensitivity 1: Vague Tau",
                      "Sensitivity 2: Weakly-Informative Mu")
  all_overall <- metabind(mf_prim, mf_s1, mf_s2,                name = analyses_names)
  all_predict <- metabind(mf_prim_pred, mf_s1_pred, mf_s2_pred, name = analyses_names)

  tau_fmt <- function(p) paste0("τ = ", round(p$tau_median, 2),
                                " [95% CrI ", round(p$tau_lower, 2),
                                ", ", round(p$tau_upper, 2), "]")
  tau_prim_text  <- tau_fmt(post_prim)
  tau_sens1_text <- tau_fmt(post_sens1)
  tau_sens2_text <- tau_fmt(post_sens2)

  plots_prim <- generate_posterior_plots_rr(meta_RR_primary, "skyblue")
  plots_pred <- generate_posterior_plots_rr(meta_RR_primary, "firebrick", is_predict = TRUE)
  plots_s1   <- generate_posterior_plots_rr(meta_sens1,      "forestgreen")
  plots_s2   <- generate_posterior_plots_rr(meta_sens2,      "blue")

  p_benefit_avg  <- round(meta_RR_primary$pposterior(mu    = 0)                       * 100, 2)
  p_harm_avg     <- round((1 - meta_RR_primary$pposterior(mu    = 0))                 * 100, 2)
  p_benefit_pred <- round(meta_RR_primary$pposterior(theta = 0, predict = TRUE)       * 100, 2)
  p_harm_pred    <- round((1 - meta_RR_primary$pposterior(theta = 0, predict = TRUE)) * 100, 2)

  n_studies     <- nrow(dat_RR)
  forest_height <- max(10, n_studies * 0.4 + 5)
  sens_height   <- max(8,  3 * 0.6 + 4)

  lbl <- get_forest_labels(sheet, label_left_RR, label_right_RR)

  # ── Exportações — Fase 1 ──────────────────────────────────────────────────

  save_plot("Figure_1_Forest_Primary", function() {
    meta::forest(mf_prim_pred, layout = "RevMan5",
                 text.random   = "Bayesian Summary",
                 text.predict  = "Bayesian 95% Prediction Interval",
                 text.addline1 = tau_prim_text,
                 smlab         = "Risk Ratio",
                 leftcols      = c("studlab","event.e","n.e","event.c","n.c","effect.ci"),
                 leftlabs      = c("Study", NA, NA, NA, NA, "RR"),
                 pooled.events = TRUE,
                 label.e       = label_e_RR, label.c   = label_c_RR,
                 label.left    = lbl$left,   label.right = lbl$right,
                 colgap        = "6mm")
  }, out_dir_main, height = forest_height)

  save_plot("Figure_2_Posterior_Dist", function() {
    print(plots_prim$density + plots_prim$probability)
  }, out_dir_main)

  save_docx("Posterior_Probabilities", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    "Primary Analysis — Posterior Probabilities",
    paste(rep("-", 50), collapse = ""),
    paste0("P(RR < 1) — Prob. of Average Benefit:     ", p_benefit_avg,  "%"),
    paste0("P(RR > 1) — Prob. of Average Harm:        ", p_harm_avg,     "%"),
    paste0("P(RR < 1) — Prob. of Predictive Benefit:  ", p_benefit_pred, "%"),
    paste0("P(RR > 1) — Prob. of Predictive Harm:     ", p_harm_pred,    "%")
  ), out_dir_main)

  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    "=== Primary Model ===",        capture.output(print(meta_RR_primary$summary)), "",
    "=== Sensitivity 1: Vague Tau ===", capture.output(print(meta_sens1$summary)), "",
    "=== Sensitivity 2: Weakly-Informative Mu ===",
    capture.output(print(meta_sens2$summary))
  ), out_dir_main)

  save_plot("01_Primary_Priors", function() {
    print(plot_mu_vague_prior / plot_tau_weakly_prior)
  }, out_dir_meth, height = 12)
  save_plot("02_Sensitivity_1_Priors", function() {
    print(plot_mu_vague_prior / plot_tau_vague_prior)
  }, out_dir_meth, height = 12)
  save_plot("03_Sensitivity_2_Priors", function() {
    print(plot_mu_weakly_prior / plot_tau_weakly_prior)
  }, out_dir_meth, height = 12)

  save_plot("Supp_Fig_1_Posterior_Predictive", function() {
    print(plots_pred$density + plots_pred$probability)
  }, out_dir_resu)
  save_plot("Supp_Fig_2_Sens_Overall_Effect", function() {
    meta::forest(all_overall, smlab = "Overall Effect (RR)", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)
  save_plot("Supp_Fig_3_Sens_Distributions", function() {
    print((plots_s1$density + plots_s1$probability) /
            (plots_s2$density + plots_s2$probability))
  }, out_dir_resu, height = 12)
  save_plot("Supp_Fig_4_Sens_Predictive", function() {
    meta::forest(all_predict, smlab = "95% Prediction Interval", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)

  save_docx("Supp_Table_1_Heterogeneity", c(
    paste0("Outcome: ", sheet),
    "Supplementary Table 1 — Between-Study Heterogeneity",
    paste(rep("-", 80), collapse = ""),
    paste0("Primary (Weakly-Informative Tau):  ", tau_prim_text),
    paste0("Sensitivity 1 (Vague Tau):         ", tau_sens1_text),
    paste0("Sensitivity 2 (Weakly-Inform. Mu): ", tau_sens2_text)
  ), out_dir_resu)

  # ── Fase 2 — LOO manual ──────────────────────────────────────────────────────
  if (should_run_loo(sheet)) {
    tryCatch(
      run_loo_analysis(dat = dat_RR, full_model = meta_RR_primary,
                       mu_mean = prior_mu_vague_mean_RR, mu_sd = prior_mu_vague_sd_RR,
                       tau_prior_func = prior_tau_primary_func,
                       label_col = "study", scale = "RR",
                       out_dir = out_dir_resu, sheet_name = sheet),
      error = function(e) log_error(paste0("LOO — ", sheet), e$message)
    )
  }

  invisible(NULL)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §6 — ANÁLISE CONTÍNUA (Mean Difference)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_bayesian_meta_continuous <- function(sheet, dat, base_dir = output_dir) {

  out_dir_main <- file.path(base_dir, sheet)
  out_dir_meth <- file.path(out_dir_main, "Supplementary_Methods")
  out_dir_resu <- file.path(out_dir_main, "Supplementary_Results")
  dir.create(out_dir_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir_resu, showWarnings = FALSE, recursive = TRUE)

  # ── Prompt interativo ────────────────────────────────────────────────────────
  pi <- prompt_continuous_priors(sheet)

  # ── Dados ───────────────────────────────────────────────────────────────────
  dat    <- dat[!is.na(dat$source), ]
  dat_MD <- escalc(measure = "MD",
                   m1i = m1i, sd1i = sd1i, n1i = n1i,
                   m2i = m2i, sd2i = sd2i, n2i = n2i,
                   data = dat) |> data.frame()
  dat_MD$sei <- sqrt(dat_MD$vi)

  if (nrow(dat_MD) < 2)
    stop("Menos de 2 estudos validos em '", sheet, "'.")

  # ── Priors ──────────────────────────────────────────────────────────────────
  prior_tau_weakly_func <- function(x) dhalfnormal(x, scale = pi$scale_tau)
  prior_tau_vague_func  <- function(x) dhalfnormal(x, scale = vague_tau_MD_scale)

  # Visualizações
  plot_mu_primary_prior  <- mu_dist_viz(mean = prior_mu_vague_mean_MD,
                                        sd = prior_mu_vague_sd_MD,
                                        scale = "absolute", xlabel = md_x_label)
  plot_tau_primary_prior <- tau_prior_plot(pi$extreme_tau)

  plot_mu_sens_prior    <- mu_dist_viz(mean = 0, sd = pi$sd_mu_sens,
                                       scale = "absolute", xlabel = md_x_label)
  plot_tau_vague_prior  <- tau_dist_viz(dist = "half-normal",
                                        scale = vague_tau_MD_scale)

  # ── Modelos ──────────────────────────────────────────────────────────────────
  # Primário: Mu vago Normal(0,510²)  /  Tau weakly Half-Normal(scale_tau)
  meta_MD_primary <- bayesmeta(y = dat_MD$yi, sigma = dat_MD$sei,
                               labels        = dat_MD$source,
                               mu.prior.mean = prior_mu_vague_mean_MD,
                               mu.prior.sd   = prior_mu_vague_sd_MD,
                               tau.prior     = prior_tau_weakly_func)

  # Sensibilidade: Mu weakly Normal(0,sd_mu_sens²)  /  Tau vago Half-Normal(4)
  meta_MD_sens    <- bayesmeta(y = dat_MD$yi, sigma = dat_MD$sei,
                               labels        = dat_MD$source,
                               mu.prior.mean = 0,
                               mu.prior.sd   = pi$sd_mu_sens,
                               tau.prior     = prior_tau_vague_func)

  post_prim <- get_summary(meta_MD_primary)
  post_sens <- get_summary(meta_MD_sens)

  # ── Forest base ──────────────────────────────────────────────────────────────
  forest_data_MD <- metacont(data = dat_MD, studlab = source,
                             mean.e = m1i, sd.e = sd1i, n.e = n1i,
                             mean.c = m2i, sd.c = sd2i, n.c = n2i,
                             sm = "MD", common = FALSE, random = FALSE)

  mf_prim      <- metaadd(forest_data_MD, type = "random",
                          TE = post_prim$mu_mean,
                          upper = post_prim$mu_upper, lower = post_prim$mu_lower)
  mf_prim_pred <- metaadd(mf_prim, type = "prediction",
                          TE = post_prim$theta_mean,
                          upper = post_prim$theta_upper, lower = post_prim$theta_lower)
  mf_sens      <- metaadd(forest_data_MD, type = "random",
                          TE = post_sens$mu_mean,
                          upper = post_sens$mu_upper, lower = post_sens$mu_lower)
  mf_sens_pred <- metaadd(forest_data_MD, type = "prediction",
                          TE = post_sens$theta_mean,
                          upper = post_sens$theta_upper, lower = post_sens$theta_lower)

  analyses_names <- c("Primary: Vague Mu / Weakly-Informative Tau",
                      "Sensitivity: Weakly-Informative Mu / Vague Tau")
  all_overall <- metabind(mf_prim,      mf_sens,      name = analyses_names)
  all_predict <- metabind(mf_prim_pred, mf_sens_pred, name = analyses_names)

  tau_fmt <- function(p) paste0("τ = ", round(p$tau_median, 2),
                                " [95% CrI ", round(p$tau_lower, 2),
                                ", ", round(p$tau_upper, 2), "]")
  tau_prim_text <- tau_fmt(post_prim)
  tau_sens_text <- tau_fmt(post_sens)

  plots_prim <- generate_posterior_plots_md(meta_MD_primary, "skyblue")
  plots_pred <- generate_posterior_plots_md(meta_MD_primary, "firebrick", is_predict = TRUE)

  p_mu_le0    <- round(meta_MD_primary$pposterior(mu    = 0)                       * 100, 2)
  p_mu_gt0    <- round((1 - meta_MD_primary$pposterior(mu    = 0))                 * 100, 2)
  p_theta_le0 <- round(meta_MD_primary$pposterior(theta = 0, predict = TRUE)       * 100, 2)
  p_theta_gt0 <- round((1 - meta_MD_primary$pposterior(theta = 0, predict = TRUE)) * 100, 2)

  n_studies     <- nrow(dat_MD)
  forest_height <- max(10, n_studies * 0.4 + 5)
  sens_height   <- max(8,  2 * 0.6 + 4)

  lbl <- get_forest_labels(sheet, label_left_MD, label_right_MD)

  # ── Exportações — Fase 1 ──────────────────────────────────────────────────

  save_plot("Figure_1_Forest_Primary", function() {
    meta::forest(mf_prim_pred, layout = "RevMan5",
                 sortvar       = dat_MD$source,
                 text.random   = "Bayesian Summary",
                 text.predict  = "Bayesian 95% Prediction Interval",
                 text.addline1 = tau_prim_text,
                 smlab         = "Mean Difference",
                 digits.mean   = 2, digits.sd = 2,
                 leftcols  = c("studlab","mean.e","sd.e","n.e","mean.c","sd.c","n.c","effect.ci"),
                 leftlabs  = c("Study", NA, NA, NA, NA, NA, NA, "MD"),
                 pooled.events = TRUE,
                 label.e       = label_e_MD,   label.c     = label_c_MD,
                 label.left    = lbl$left,     label.right = lbl$right,
                 colgap        = "6mm")
  }, out_dir_main, height = forest_height)

  save_plot("Figure_2_Posterior_Dist", function() {
    print(plots_prim$density + plots_prim$probability)
  }, out_dir_main)

  save_docx("Posterior_Probabilities", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    "Primary Analysis — Posterior Probabilities",
    paste(rep("-", 50), collapse = ""),
    paste0("P(MD <= 0): ", p_mu_le0,    "%"),
    paste0("P(MD >  0): ", p_mu_gt0,    "%"),
    paste0("P(theta <= 0) [predictive]: ", p_theta_le0, "%"),
    paste0("P(theta >  0) [predictive]: ", p_theta_gt0, "%")
  ), out_dir_main)

  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet), paste(rep("=", 60), collapse = ""), "",
    paste0("=== Primary: Vague Mu / Weakly Tau",
           " [extreme_diff=", pi$extreme_tau, "] ==="),
    capture.output(print(meta_MD_primary$summary)), "",
    paste0("=== Sensitivity: Weakly Mu [extreme_diff=", pi$extreme_mu,
           "] / Vague Tau [Half-Normal(", vague_tau_MD_scale, ")] ==="),
    capture.output(print(meta_MD_sens$summary))
  ), out_dir_main)

  save_plot("01_Primary_Priors", function() {
    print(plot_mu_primary_prior / plot_tau_primary_prior)
  }, out_dir_meth, height = 12)
  save_plot("02_Sensitivity_Priors", function() {
    print(plot_mu_sens_prior / plot_tau_vague_prior)
  }, out_dir_meth, height = 12)

  save_plot("Supp_Fig_1_Posterior_Predictive", function() {
    print(plots_pred$density + plots_pred$probability)
  }, out_dir_resu)
  save_plot("Supp_Fig_2_Sens_Overall_Effect", function() {
    meta::forest(all_overall, smlab = "Overall Effect (MD)", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)
  save_plot("Supp_Fig_3_Sens_Predictive", function() {
    meta::forest(all_predict, smlab = "95% Prediction Interval", leftcols = "studlab")
  }, out_dir_resu, height = sens_height)

  save_docx("Supp_Table_1_Heterogeneity", c(
    paste0("Outcome: ", sheet),
    "Supplementary Table 1 — Between-Study Heterogeneity",
    paste(rep("-", 80), collapse = ""),
    paste0("Primary (Vague Mu / Weakly Tau):     ", tau_prim_text),
    paste0("Sensitivity (Weakly Mu / Vague Tau): ", tau_sens_text)
  ), out_dir_resu)

  # ── Fase 2 — LOO manual ──────────────────────────────────────────────────────
  if (should_run_loo(sheet)) {
    tryCatch(
      run_loo_analysis(dat = dat_MD, full_model = meta_MD_primary,
                       mu_mean = prior_mu_vague_mean_MD, mu_sd = prior_mu_vague_sd_MD,
                       tau_prior_func = prior_tau_weakly_func,
                       label_col = "source", scale = "MD",
                       out_dir = out_dir_resu, sheet_name = sheet),
      error = function(e) log_error(paste0("LOO — ", sheet), e$message)
    )
  }

  invisible(NULL)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §7 — DISPATCHER
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_bayesian_meta <- function(sheet, base_dir = output_dir) {

  dat <- tryCatch({
    as.data.frame(read_sheet(url, sheet = sheet)) |> janitor::clean_names()
  }, error = function(e) { log_error(paste0("read_sheet — ", sheet), e$message); NULL })

  if (is.null(dat)) return(invisible(NULL))

  type <- tryCatch(detect_outcome_type(dat),
                   error = function(e) {
                     log_error(paste0("detect — ", sheet), e$message)
                     NA_character_
                   })

  if (is.na(type)) return(invisible(NULL))

  if (type == "binary") run_bayesian_meta_binary(sheet, dat, base_dir)
  else                  run_bayesian_meta_continuous(sheet, dat, base_dir)
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
