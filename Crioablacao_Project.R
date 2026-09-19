# ==============================================================================
# SINGLE-PROPORTION META-ANALYSIS — Crioablacao de Mama  (VERSAO FINAL, v3)
# Modelo: GLMM (logistico de intercepto aleatorio) + PLOGIT
# ------------------------------------------------------------------------------
# Historico:
#   v1  script original (auto-deteccao de subgrupos/covariaveis).
#   v2  allowlists, validacao event<=n, rename meta->fit, LOO GLMM manual.
#   v3  (ESTA)  ajustes 1-9 discutidos, verificados empiricamente nos 10 estudos:
#     1. Allowlist de subgrupos corrigida p/ bater com a planilha (subgroup1) e
#        reduzida a UM contraste confirmatorio; covariaveis separadas em
#        CONFIRMATORIA (luminal_prop) vs EXPLORATORIAS (as demais).
#     2. Meta-regressao PRIMARIA = rma.glmm (metareg, SEM correcao de
#        continuidade). Permutacao roda SO num ramo de sensibilidade em
#        rma.uni-logit (escalc PLO), pois permutest nao tem metodo p/ rma.glmm;
#        a correcao de continuidade nos 3 zeros incide APENAS nesse ramo.
#     3. Multiplicidade: Holm sobre os p de permutacao de todos os moderadores.
#     4. R^2 removido do log/resumo (instavel em rma.glmm com k pequeno).
#     5. Trim-and-fill removido (reajusta por inverse-variance, nao pelo GLMM);
#        mantidos funnel + Peters + log de estudos efetivamente usados.
#     6. tau.common = TRUE nos ajustes com subgrupo (niveis de k=4-6).
#     7. Sensibilidade IV+logit+HKSJ do efeito combinado.
#     8. tau^2 e intervalo de predicao (PI) adicionados ao resumo. I^2
#        reportado como grandeza descritiva (nos dados atuais ~38%, moderado).
#     9. Reprodutibilidade: leitura opcional de CSV congelado, set.seed global,
#        sessionInfo() gravado ao final.
#   v3.1 HOTFIX: (a) Peters usava peters$k (inexistente) -> NULL -> if() estourava
#        fora de tryCatch e abortava o desfecho; trocado por peters$df + 2L e
#        guarda isTRUE(). (b) Holm passa a corrigir SO as exploratorias; a
#        confirmatoria (luminal_prop) fica sem ajuste. (c) ramo de permutacao
#        documentado como REML/IV-logit (a primaria e ML/rma.glmm).
#   NOTA DE MANUSCRITO: subgroup1, luminal_prop e follow_up_mo sao o mesmo eixo
#   latente (r 0,59-0,85; so 'Ye' fora do bloco). O efeito NAO e atribuivel ao
#   subtipo luminal isoladamente; descreva o moderador como "definicao de
#   recorrencia / intencao de tratamento" e registre UM no PROSPERO.
# Saidas graficas em PNG e TIFF a >= 300 dpi (pacote ragg).
# Referencias: Schwarzer 2019; Stijnen 2010; Munn/Barker (JBI) 2021.
# ==============================================================================

set.seed(2025)                                    # [v3-9] reprodutibilidade

CONFIG <- list(
  gs_url  = "https://docs.google.com/spreadsheets/d/1hmYzg0fDSPuRdiZ76aiPiPXKy5SQ72cLUr8yRSgURHU/edit",
  public  = TRUE,
  # [v3-9] Se um CSV congelado for informado e existir, ELE e a fonte (a planilha
  # publica pode mudar silenciosamente entre execucoes). Deixe NULL p/ ler o GS.
  csv_frozen = NULL,                              # ex.: "extraction_frozen.csv"
  out_dir = file.path(getwd(), "resultados_meta"),

  col_event = "event",
  col_n     = "n",
  col_study = "study",

  # ---- ALLOWLISTS EXPLICITAS -------------------------------------------------
  # [v3-1] Os nomes DEVEM bater com os cabecalhos reais da planilha.
  # Um unico subgrupo confirmatorio (o contraste latente recorrencia/intencao).
  subgroups  = c("subgroup1"),
  # Moderador continuo CONFIRMATORIO (prespecificado no PROSPERO).
  covariates_confirmatory = c("luminal_prop"),
  # Moderadores EXPLORATORIOS (reportar como geradores de hipotese).
  covariates_exploratory  = c("tumor_size_mm", "follow_up_mo",
                              "rt_prop", "endo_prop"),
  auto_detect = FALSE,   # fallback de auto-deteccao mantido desligado

  loo_manual = TRUE,

  min_k_bias    = 10,
  min_k_per_cov = 10,
  max_sg_levels = 6,
  perm_iter     = 5000,  # [v3-2] iteracoes do teste de permutacao

  width = 3000, height = 2500, res = 300, formats = c("png", "tiff"),

  labels = c(
    tumor_size_mm = "Tumor size (mm)",
    luminal_prop  = "Proportion of luminal subtype",
    follow_up_mo  = "Follow-up (months)",
    rt_prop       = "Proportion receiving radiotherapy",
    endo_prop     = "Proportion receiving endocrine therapy"
  )
)

if (!require("pacman")) install.packages("pacman")
# [v3] metafor entra explicitamente (escalc / rma.uni / permutest).
pacman::p_load(googlesheets4, ragg, meta, metafor, officer)

if (isTRUE(CONFIG$public)) googlesheets4::gs4_deauth()

dir.create(CONFIG$out_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(CONFIG$out_dir, "log_meta.txt")
logmsg <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", sprintf(...))
  cat(msg, "\n"); cat(msg, "\n", file = log_file, append = TRUE)
}
cat("", file = log_file)

save_plot <- function(path_noext, draw) {
  for (fmt in CONFIG$formats) {
    fn <- paste0(path_noext, ".", fmt)
    if (fmt == "png")
      ragg::agg_png(fn, width = CONFIG$width, height = CONFIG$height, res = CONFIG$res)
    else
      ragg::agg_tiff(fn, width = CONFIG$width, height = CONFIG$height,
                     res = CONFIG$res, compression = "lzw")
    draw(); invisible(dev.off())
  }
}

nice_label <- function(col) {
  if (!is.null(CONFIG$labels[[col]])) return(CONFIG$labels[[col]])
  tools::toTitleCase(gsub("_", " ", col))
}

pad_xlim <- function(x, left = 0.42, right = 0.15) {
  x <- x[is.finite(x)]
  r <- range(x, na.rm = TRUE); d <- diff(r); if (d == 0) d <- abs(r[1]) + 1
  c(r[1] - left * d, r[2] + right * d)
}

# [v3-6] fit_metaprop aceita tau.common (TRUE quando ha subgrupo)
fit_metaprop <- function(df, subgroup = NULL, tau.common = FALSE) {
  args <- list(event = df$event, n = df$n, studlab = df$study, data = df,
               sm = "PLOGIT", method = "GLMM", method.tau = "ML",
               random = TRUE, common = FALSE, prediction = TRUE)
  if (!is.null(subgroup)) {
    args$subgroup <- subgroup; args$subgroup.name <- ""
    args$tau.common <- tau.common
  }
  do.call(metaprop, args)
}

# [v3-7] Efeito combinado por Inverse-Variance + logit + HKSJ (sensibilidade)
fit_metaprop_hksj <- function(df) {
  metaprop(event = df$event, n = df$n, studlab = df$study, data = df,
           sm = "PLOGIT", method = "Inverse", method.random.ci = "HK",
           random = TRUE, common = FALSE, prediction = TRUE)
}

# ---- Seletores por allowlist -------------------------------------------------
select_subgroups <- function(df, allow) {
  cols <- intersect(allow, names(df))
  cols[vapply(cols, function(c) {
    v <- df[[c]]
    if (is.numeric(v)) return(FALSE)
    k <- length(unique(v[!is.na(v) & nzchar(as.character(v))]))
    k >= 2 && k <= CONFIG$max_sg_levels
  }, logical(1))]
}
select_covariates <- function(df, allow) {
  cols <- intersect(allow, names(df))
  cols[vapply(cols, function(c) is.numeric(df[[c]]) &&
                sum(!is.na(df[[c]])) >= 3, logical(1))]
}
detect_subgroups_auto <- function(df, core) {
  cand <- setdiff(names(df), core)
  cand <- cand[!is.na(cand) & nzchar(cand) & !grepl("^\\.\\.\\.", cand)]
  cand[vapply(cand, function(c) {
    v <- df[[c]]; if (is.numeric(v)) return(FALSE)
    k <- length(unique(v[!is.na(v) & nzchar(as.character(v))]))
    k >= 2 && k <= CONFIG$max_sg_levels
  }, logical(1))]
}
detect_covariates_auto <- function(df, core) {
  cand <- setdiff(names(df), core)
  cand <- cand[!is.na(cand) & nzchar(cand) & !grepl("^\\.\\.\\.", cand)]
  cand[vapply(cand, function(c) is.numeric(df[[c]]) &&
                sum(!is.na(df[[c]])) >= 3, logical(1))]
}
resolve_subgroups <- function(df, core) {
  sg <- select_subgroups(df, CONFIG$subgroups)
  if (length(sg) == 0 && isTRUE(CONFIG$auto_detect)) {
    sg <- detect_subgroups_auto(df, core)
    if (length(sg))
      logmsg("  [AVISO] allowlist de subgrupos vazia; auto-deteccao ATIVA: %s",
             paste(sg, collapse = ", "))
  }
  sg
}

# [v3-1] Covariaveis = confirmatoria + exploratorias, com rotulo de papel.
resolve_covariates <- function(df) {
  conf <- select_covariates(df, CONFIG$covariates_confirmatory)
  expl <- select_covariates(df, CONFIG$covariates_exploratory)
  if (length(conf) == 0 && length(expl) == 0 && isTRUE(CONFIG$auto_detect)) {
    auto <- detect_covariates_auto(df, c("event", "n", "study"))
    if (length(auto)) {
      logmsg("  [AVISO] allowlists de covariaveis vazias; auto-deteccao ATIVA: %s",
             paste(auto, collapse = ", "))
      expl <- auto
    }
  }
  data.frame(cov = c(conf, expl),
             role = c(rep("confirmatory", length(conf)),
                      rep("exploratory",  length(expl))),
             stringsAsFactors = FALSE)
}

# [v3-2] Permutacao no ramo IV-logit (rma.uni, REML). Retorna p de permutacao
# do coeficiente do moderador. Modelo DELIBERADAMENTE distinto da primaria
# (metareg/rma.glmm usa ML): a permutacao e IV-logit/REML e a correcao de
# continuidade (0.5) de escalc PLO incide nos estudos de celula-zero — declarar
# ambos (REML/IV-logit e a cc nos zeros) como caracteristicas DESTE ramo.
perm_pval_ivlogit <- function(df, cov) {
  esc <- metafor::escalc(measure = "PLO", xi = df$event, ni = df$n)
  ok  <- is.finite(esc$yi) & is.finite(esc$vi) & !is.na(df[[cov]])
  if (sum(ok) < CONFIG$min_k_per_cov) return(NA_real_)
  mr_uni <- tryCatch(
    metafor::rma.uni(yi = esc$yi[ok], vi = esc$vi[ok],
                     mods = ~ df[[cov]][ok], method = "REML"),
    error = function(e) NULL)
  if (is.null(mr_uni)) return(NA_real_)
  pt <- tryCatch(metafor::permutest(mr_uni, iter = CONFIG$perm_iter),
                 error = function(e) NULL)
  if (is.null(pt)) return(NA_real_)
  as.numeric(pt$pval[2])
}

# ---- Leave-one-out GLMM manual (refit por estudo) ----------------------------
glmm_loo <- function(df) {
  do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    f <- tryCatch(fit_metaprop(df[-i, , drop = FALSE]), error = function(e) NULL)
    if (is.null(f))
      return(data.frame(omitted = df$study[i], prev = NA, lo = NA, hi = NA, I2 = NA))
    data.frame(omitted = df$study[i],
               prev = plogis(f$TE.random) * 100,
               lo   = plogis(f$lower.random) * 100,
               hi   = plogis(f$upper.random) * 100,
               I2   = f$I2 * 100)
  }))
}

FCOLS <- c("studlab", "event", "n", "effect", "ci")
FLABS <- c("Studies", NA, NA, "Prevalence", NA)

docx_lines <- function(doc, obj) {
  for (ln in capture.output(print(obj))) {
    if (!nzchar(ln)) ln <- " "
    doc <- officer::body_add_fpar(
      doc, officer::fpar(officer::ftext(
        ln, officer::fp_text(font.family = "Courier New", font.size = 8))))
  }
  doc
}
docx_append <- function(doc, obj) officer::body_add_par(docx_lines(doc, obj), " ")
docx_single <- function(path, obj) {
  doc <- docx_lines(officer::read_docx(), obj)
  print(doc, target = path)
}

# ---- Leitura da fonte de dados -----------------------------------------------
# [v3-9] CSV congelado tem prioridade sobre a planilha publica.
if (!is.null(CONFIG$csv_frozen) && file.exists(CONFIG$csv_frozen)) {
  logmsg("Fonte: CSV congelado '%s'", CONFIG$csv_frozen)
  nm <- tools::file_path_sans_ext(basename(CONFIG$csv_frozen))
  ma <- setNames(list(as.data.frame(utils::read.csv(CONFIG$csv_frozen,
                                                    stringsAsFactors = FALSE))), nm)
  sheetsnames <- nm
} else {
  sheetsnames <- googlesheets4::sheet_names(CONFIG$gs_url)
  logmsg("Fonte: Google Sheet. Abas encontradas: %s", paste(sheetsnames, collapse = ", "))
  ma <- lapply(sheetsnames, function(x)
    as.data.frame(googlesheets4::read_sheet(CONFIG$gs_url, sheet = x)))
  names(ma) <- sheetsnames
}

core_cols <- c(CONFIG$col_event, CONFIG$col_n, CONFIG$col_study)
resumo <- list()

for (outcome in sheetsnames) {

  logmsg("==== DESFECHO: %s ====", outcome)
  df <- ma[[outcome]]
  prefix <- file.path(CONFIG$out_dir, gsub("[^A-Za-z0-9_-]", "_", outcome))
  doc <- officer::read_docx()

  faltando <- setdiff(core_cols, names(df))
  if (length(faltando) > 0) {
    logmsg("  PULADA — faltam colunas: %s", paste(faltando, collapse = ", ")); next
  }
  names(df)[match(core_cols, names(df))] <- c("event", "n", "study")

  # coercao explicita de tipos
  ev_raw <- df$event; n_raw <- df$n
  df$event <- suppressWarnings(as.numeric(as.character(df$event)))
  df$n     <- suppressWarnings(as.numeric(as.character(df$n)))
  coerc_bad <- (is.na(df$event) & !is.na(ev_raw) & nzchar(as.character(ev_raw))) |
               (is.na(df$n)     & !is.na(n_raw)  & nzchar(as.character(n_raw)))
  if (any(coerc_bad))
    logmsg("  [AVISO] %d linha(s) com event/n nao numerico descartada(s): %s",
           sum(coerc_bad), paste(df$study[coerc_bad], collapse = ", "))
  df <- df[!is.na(df$event) & !is.na(df$n), , drop = FALSE]

  # validacao de sanidade: inteiros nao-negativos e event <= n
  viol <- with(df, event < 0 | n <= 0 | event > n)
  if (any(viol, na.rm = TRUE)) {
    logmsg("  [AVISO] %d linha(s) com event>n ou n<=0 descartada(s): %s",
           sum(viol, na.rm = TRUE), paste(df$study[which(viol)], collapse = ", "))
    df <- df[!viol, , drop = FALSE]
  }

  k <- nrow(df)
  if (k < 2) { logmsg("  PULADA — apenas %d estudo(s) valido(s).", k); next }

  # ---- Modelo principal (GLMM/PLOGIT) ----------------------------------------
  fit <- tryCatch(fit_metaprop(df), error = function(e) {
    logmsg("  ERRO no modelo principal: %s", conditionMessage(e)); NULL })
  if (is.null(fit)) next

  prev  <- plogis(fit$TE.random) * 100
  pi_lo <- plogis(fit$lower.predict) * 100          # [v3-8]
  pi_hi <- plogis(fit$upper.predict) * 100
  logmsg("  k=%d | prev=%.1f%% [%.1f-%.1f] | I2=%.1f%% | tau2=%.3f | PI [%.1f-%.1f]",
         k, prev, plogis(fit$lower.random) * 100, plogis(fit$upper.random) * 100,
         fit$I2 * 100, fit$tau2, pi_lo, pi_hi)
  doc <- docx_append(doc, summary(fit))

  # [v3-7] Sensibilidade IV + logit + HKSJ
  prev_hk <- ic_hk_lo <- ic_hk_hi <- NA_real_
  fit_hk <- tryCatch(fit_metaprop_hksj(df), error = function(e) NULL)
  if (!is.null(fit_hk)) {
    prev_hk  <- plogis(fit_hk$TE.random) * 100
    ic_hk_lo <- plogis(fit_hk$lower.random) * 100
    ic_hk_hi <- plogis(fit_hk$upper.random) * 100
    logmsg("  [SENSIBILIDADE HKSJ] prev=%.1f%% [%.1f-%.1f] (vs GLMM Wald)",
           prev_hk, ic_hk_lo, ic_hk_hi)
    doc <- docx_append(doc, summary(fit_hk))
  }

  save_plot(paste0(prefix, "_Forestplot"), function()
    forest(fit, layout = "Revman5", leftcols = FCOLS, leftlabs = FLABS,
           colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
           colgap.forest.left = "6mm", just = "center", col.square = "darkblue",
           col.square.lines = "black", sortvar = TE,
           ref = round(prev, 2)))

  # ---- Subgrupo(s) confirmatorio(s), tau comum -------------------------------
  for (sg in resolve_subgroups(df, c("event", "n", "study"))) {
    dfg <- df[!is.na(df[[sg]]) & nzchar(as.character(df[[sg]])), , drop = FALSE]
    obj <- tryCatch(fit_metaprop(dfg, subgroup = dfg[[sg]], tau.common = TRUE),
                    error = function(e) { logmsg("  subgrupo '%s' falhou: %s",
                                                 sg, conditionMessage(e)); NULL })
    if (is.null(obj)) next
    logmsg("  Subgrupo '%s' (k=%d, tau.common): p(entre subgrupos, EXPLORATORIO) = %s",
           sg, nrow(dfg), formatC(obj$pval.Q.b.random, digits = 4, format = "f"))
    doc <- docx_append(doc, summary(obj))
    local({
      obj_l <- obj
      save_plot(paste0(prefix, "_Subgroup_", sg), function()
        forest(obj_l, layout = "Revman5", leftcols = FCOLS, leftlabs = FLABS,
               colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
               colgap.forest.left = "6mm", just = "center", col.square = "darkblue",
               col.square.lines = "black", sortvar = TE,
               print.subgroup.name = FALSE, test.subgroup = TRUE))
    })
  }

  # ---- Leave-one-out ---------------------------------------------------------
  loo <- tryCatch(metainf(fit, pooled = "random"), error = function(e) NULL)
  if (!is.null(loo)) {
    docx_single(paste0(prefix, "_Leave-one-out.docx"), loo)
    save_plot(paste0(prefix, "_Leave-one-out"), function()
      forest(loo, col.bg = "lightblue", col.diamond = "gray", pscale = 100,
             ff.xlab = "bold", rightcols = c("effect", "ci", "I2"),
             colgap.right = "6mm", just = "center"))
  }
  if (isTRUE(CONFIG$loo_manual)) {
    loo_m <- tryCatch(glmm_loo(df), error = function(e) NULL)
    if (!is.null(loo_m)) {
      write.csv(loo_m, paste0(prefix, "_Leave-one-out_GLMM_manual.csv"),
                row.names = FALSE)
      rng <- range(loo_m$prev, na.rm = TRUE)
      logmsg("  LOO GLMM manual: prevalencia entre %.1f%% e %.1f%% (omitindo %s / %s)",
             rng[1], rng[2],
             loo_m$omitted[which.min(loo_m$prev)],
             loo_m$omitted[which.max(loo_m$prev)])
      if (!is.null(loo)) {
        mi <- plogis(loo$TE) * 100
        dif <- max(abs(range(mi, na.rm = TRUE) - rng), na.rm = TRUE)
        if (is.finite(dif) && dif > 0.5)
          logmsg("  [AVISO] LOO metainf difere do GLMM manual em ate %.2f p.p. — usar o manual.", dif)
      }
    }
  }

  # ---- Vies de publicacao: funnel + Peters (SEM trim-and-fill) ---------------
  peters_p <- NA_real_
  if (k >= CONFIG$min_k_bias) {
    fxlim <- pad_xlim(fit$TE, left = 0.35, right = 0.18)
    save_plot(paste0(prefix, "_FunnelPlot"), function()
      funnel(fit, studlab = TRUE, bg = "red", cex = 2, cex.studlab = 0.6,
             random = TRUE, common = FALSE, backtransf = FALSE, xlim = fxlim))

    peters <- tryCatch(metabias(fit, method.bias = "peters"), error = function(e) NULL)
    if (!is.null(peters)) {
      pv <- suppressWarnings(as.numeric(peters$pval)[1])
      if (length(pv) && !is.na(pv)) peters_p <- pv
      # [v3.1 HOTFIX] metabias() NAO expoe $k (peters$k = NULL -> if() estoura
      # fora de tryCatch e aborta o desfecho). k usado = df + 2 (Peters: intercepto
      # + inclinacao); com os 3 zeros na escala logit isso da 7 de 10.
      k_used <- tryCatch(peters$df + 2L, error = function(e) NA_integer_)
      if (length(k_used) != 1 || is.na(k_used)) k_used <- NA_integer_
      logmsg("  Teste de Peters: p = %s | estudos usados = %s de %d",
             formatC(peters_p, digits = 3, format = "f"),
             ifelse(is.na(k_used), "?", k_used), k)
      if (isTRUE(k_used < k))
        logmsg("  [AVISO] Peters usou %d/%d estudos (celulas-zero excluidas) — reportar no texto.",
               k_used, k)
      docx_single(paste0(prefix, "_PetersTest.docx"), peters)
    }
    # [v3-5] trim-and-fill REMOVIDO: reajusta por inverse-variance (nao pelo GLMM)
    # e nao e recomendado p/ proporcoes. Assimetria de funnel em proporcao unica
    # reflete relacao proporcao-variancia, nao necessariamente vies de publicacao.
  } else {
    logmsg("  Vies/funnel PULADOS (k=%d < %d).", k, CONFIG$min_k_bias)
  }

  # ---- Meta-regressao: PRIMARIA rma.glmm + permutacao IV-logit (sensib.) -----
  covtab <- resolve_covariates(df)
  metareg_out <- character(0)
  perm_tab <- data.frame(cov = character(0), role = character(0),
                         k_cov = integer(0), p_glmm = numeric(0),
                         p_perm = numeric(0), stringsAsFactors = FALSE)

  if (nrow(covtab) > 0) for (r in seq_len(nrow(covtab))) {
    cov  <- covtab$cov[r]; role <- covtab$role[r]
    k_cov <- sum(!is.na(df[[cov]]))
    if (k_cov < CONFIG$min_k_per_cov) {
      logmsg("  metareg '%s' (%s) PULADA (k_cov=%d < %d).",
             cov, role, k_cov, CONFIG$min_k_per_cov); next
    }
    # PRIMARIA: rma.glmm via meta::metareg (SEM correcao de continuidade)
    mr <- tryCatch(metareg(fit, as.formula(paste0("~", cov))),
                   error = function(e) { logmsg("  metareg '%s' falhou: %s",
                                                cov, conditionMessage(e)); NULL })
    if (is.null(mr)) next
    p_glmm <- mr$pval[2]                                   # [v3-4] R^2 removido
    # SENSIBILIDADE: permutacao no ramo IV-logit (cc nos zeros; declarar)
    p_perm <- perm_pval_ivlogit(df, cov)
    logmsg("  metareg ~%s (%s, k_cov=%d): p_glmm=%.4f | p_perm(IVlogit)=%s",
           cov, role, k_cov, p_glmm,
           ifelse(is.na(p_perm), "NA", formatC(p_perm, digits = 4, format = "f")))
    docx_single(paste0(prefix, "_MetaReg_", cov, ".docx"), summary(mr))
    perm_tab <- rbind(perm_tab, data.frame(cov = cov, role = role, k_cov = k_cov,
                                           p_glmm = p_glmm, p_perm = p_perm,
                                           stringsAsFactors = FALSE))

    lab <- nice_label(cov); bxlim <- pad_xlim(df[[cov]], left = 0.42, right = 0.15)
    local({ mr_l <- mr; lab_l <- lab; xl <- bxlim
      save_plot(paste0(prefix, "_Bubble_", cov), function()
        bubble(mr_l, xlab = lab_l, ylab = "Prevalence",
               studlab = TRUE, cex.studlab = 0.65, xlim = xl,
               col = "darkblue", bg = "lightblue")) })
  }

  # [v3.1-3] Multiplicidade: Holm SO no conjunto EXPLORATORIO. A hipotese
  # confirmatoria unica (prespecificada no PROSPERO) e reportada SEM ajuste —
  # nao se penaliza a hipotese unica com a familia exploratoria.
  if (nrow(perm_tab) > 0) {
    perm_tab$p_perm_holm <- perm_tab$p_perm              # confirmatoria = sem ajuste
    is_expl <- perm_tab$role == "exploratory"
    if (any(is_expl))
      perm_tab$p_perm_holm[is_expl] <-
        p.adjust(perm_tab$p_perm[is_expl], method = "holm")
    write.csv(perm_tab, paste0(prefix, "_MetaReg_pvalues.csv"), row.names = FALSE)
    for (i in seq_len(nrow(perm_tab)))
      logmsg("  [Holm] %s (%s): p_perm=%.4f -> Holm=%.4f",
             perm_tab$cov[i], perm_tab$role[i],
             perm_tab$p_perm[i], perm_tab$p_perm_holm[i])
    metareg_out <- sprintf("%s[%s](glmm=%.3f;permHolm=%.3f)",
                           perm_tab$cov, substr(perm_tab$role, 1, 4),
                           perm_tab$p_glmm, perm_tab$p_perm_holm)
    doc <- docx_append(doc, perm_tab)
  }

  print(doc, target = paste0(prefix, "_Resultados.docx"))
  resumo[[outcome]] <- data.frame(
    desfecho = outcome, k = k, prevalencia = round(prev, 1),
    ic_inf = round(plogis(fit$lower.random) * 100, 1),
    ic_sup = round(plogis(fit$upper.random) * 100, 1),
    I2 = round(fit$I2 * 100, 1), tau2 = round(fit$tau2, 3),          # [v3-8]
    PI_inf = round(pi_lo, 1), PI_sup = round(pi_hi, 1),             # [v3-8]
    prev_HKSJ = round(prev_hk, 1),                                  # [v3-7]
    ic_HKSJ_inf = round(ic_hk_lo, 1), ic_HKSJ_sup = round(ic_hk_hi, 1),
    peters_p = round(peters_p, 3),
    metareg = paste(metareg_out, collapse = "; "), stringsAsFactors = FALSE)
}

if (length(resumo) > 0) {
  tab <- do.call(rbind, resumo)
  write.csv(tab, file.path(CONFIG$out_dir, "resumo_desfechos.csv"), row.names = FALSE)
  logmsg("Concluido. Resumo salvo em resumo_desfechos.csv (%d desfechos).", nrow(tab))
  print(tab)
} else logmsg("Nenhum desfecho valido foi analisado.")

# [v3-9] Ambiente de execucao para reprodutibilidade
writeLines(capture.output(sessionInfo()),
           file.path(CONFIG$out_dir, "sessionInfo.txt"))
logmsg("sessionInfo() gravado em sessionInfo.txt")
