# ==============================================================================
# SINGLE-PROPORTION META-ANALYSIS — Crioablação de Mama
# Modelo: GLMM (logístico de intercepto aleatório) + PLOGIT
# ------------------------------------------------------------------------------
# Percorre TODAS as abas de uma planilha Google e, para cada aba (= desfecho):
#   - Análise principal (1 forest plot)
#   - Subgrupo(s) detectado(s) automaticamente (1 forest plot cada, com teste
#     de diferença entre subgrupos + Tau²/I²/p)
#   - Leave-one-out
#   - Funnel plot + teste de Peters + trim-and-fill  (só com k >= 10; Cochrane/JBI)
#   - Meta-regressão UNIVARIÁVEL: 1 modelo + 1 bubble plot por covariável
#   - Um arquivo .docx por desfecho com TODOS os outputs estatísticos
# Saídas gráficas em PNG e TIFF a >= 300 dpi (pacote ragg).
# Referências: Schwarzer 2019; Stijnen 2010; Munn/Barker (JBI) 2021.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURAÇÃO  (único bloco que você precisa editar)
# ------------------------------------------------------------------------------
CONFIG <- list(
  gs_url  = "https://docs.google.com/spreadsheets/d/1hmYzg0fDSPuRdiZ76aiPiPXKy5SQ72cLUr8yRSgURHU/edit",
  public  = TRUE,                       # TRUE = planilha pública (sem login); FALSE = login interativo
  out_dir = file.path(getwd(), "resultados_meta"),

  col_event = "event",                  # colunas obrigatórias (renomeadas p/ padrão interno)
  col_n     = "n",
  col_study = "study",

  min_k_bias    = 10,                   # k mínimo p/ funnel/Peters/trim-fill
  min_k_per_cov = 10,                   # estudos por covariável na meta-regressão
  max_sg_levels = 6,                    # máx. de níveis p/ tratar coluna como subgrupo

  width = 3000, height = 2500, res = 300, formats = c("png", "tiff"),

  # Rótulos em INGLÊS para o eixo X dos bubble plots (coluna -> rótulo)
  labels = c(
    tumor_size_mm = "Tumor size (mm)",
    luminal_prop  = "Proportion of luminal subtype",
    follow_up_mo  = "Follow-up (months)",
    rt_prop       = "Proportion receiving radiotherapy",
    endo_prop     = "Proportion receiving endocrine therapy"
  )
)

# ------------------------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(googlesheets4, ragg, meta, officer)

# ------------------------------------------------------------------------------
# 2. AUTENTICAÇÃO + LOG
# ------------------------------------------------------------------------------
if (isTRUE(CONFIG$public)) googlesheets4::gs4_deauth()

dir.create(CONFIG$out_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(CONFIG$out_dir, "log_meta.txt")
logmsg <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", sprintf(...))
  cat(msg, "\n"); cat(msg, "\n", file = log_file, append = TRUE)
}
cat("", file = log_file)

# ------------------------------------------------------------------------------
# 3. FUNÇÕES AUXILIARES
# ------------------------------------------------------------------------------

# Salva QUALQUER figura em todos os formatos configurados (PNG + TIFF, 300 dpi).
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

# Rótulo em inglês para uma coluna (CONFIG$labels; senão embeleza o nome)
nice_label <- function(col) {
  if (!is.null(CONFIG$labels[[col]])) return(CONFIG$labels[[col]])
  tools::toTitleCase(gsub("_", " ", col))
}

# Limites de eixo com folga (evita truncar rótulos): mais folga à esquerda
pad_xlim <- function(x, left = 0.42, right = 0.15) {
  x <- x[is.finite(x)]
  r <- range(x, na.rm = TRUE); d <- diff(r); if (d == 0) d <- abs(r[1]) + 1
  c(r[1] - left * d, r[2] + right * d)
}

# Modelo principal (proporção única, GLMM/PLOGIT, com predição)
# IMPORTANTE: event/n/studlab/subgroup são passados como VETORES já avaliados.
# Se passássemos os nomes nus com data=df, a avaliação não-padrão (NSE) do metaprop
# capturaria por engano uma coluna chamada "subgroup" no modelo principal.
# data=df é mantido apenas para a meta-regressão localizar as covariáveis.
fit_metaprop <- function(df, subgroup = NULL) {
  args <- list(event = df$event, n = df$n, studlab = df$study, data = df,
               sm = "PLOGIT", method = "GLMM", method.tau = "ML",
               random = TRUE, common = FALSE, prediction = TRUE)
  if (!is.null(subgroup)) { args$subgroup <- subgroup; args$subgroup.name <- "" }
  do.call(metaprop, args)
}

# Detecta colunas categóricas candidatas a SUBGRUPO (2..max níveis)
detect_subgroups <- function(df, core) {
  cand <- setdiff(names(df), core)
  cand <- cand[!is.na(cand) & nzchar(cand) & !grepl("^\\.\\.\\.", cand)]  # descarta nomes NA/vazios/"...N"
  cand[vapply(cand, function(c) {
    v <- df[[c]]
    if (is.numeric(v)) return(FALSE)
    k <- length(unique(v[!is.na(v) & nzchar(as.character(v))]))
    k >= 2 && k <= CONFIG$max_sg_levels
  }, logical(1))]
}

# Detecta colunas numéricas candidatas a COVARIÁVEL de meta-regressão
detect_covariates <- function(df, core) {
  cand <- setdiff(names(df), core)
  cand <- cand[!is.na(cand) & nzchar(cand) & !grepl("^\\.\\.\\.", cand)]
  cand[vapply(cand, function(c) is.numeric(df[[c]]) &&
                sum(!is.na(df[[c]])) >= 3, logical(1))]
}

# Colunas do forest SEM a coluna Weight (vazia no GLMM -> omitida)
FCOLS <- c("studlab", "event", "n", "effect", "ci")
FLABS <- c("Studies", NA, NA, "Prevalence", NA)

# Adiciona linhas de output do R em fonte monoespaçada a um documento
docx_lines <- function(doc, obj) {
  for (ln in capture.output(print(obj))) {
    if (!nzchar(ln)) ln <- " "
    doc <- officer::body_add_fpar(
      doc, officer::fpar(officer::ftext(
        ln, officer::fp_text(font.family = "Courier New", font.size = 8))))
  }
  doc
}

# DOCX consolidado: acrescenta um título (heading 2) + o output do R
docx_block <- function(doc, heading, obj) {
  doc <- officer::body_add_par(doc, heading, style = "heading 2")
  doc <- docx_lines(doc, obj)
  officer::body_add_par(doc, " ")
}

# DOCX isolado: grava UM output do R puro (sem título) em seu próprio arquivo
docx_single <- function(path, obj) {
  doc <- docx_lines(officer::read_docx(), obj)
  print(doc, target = path)
}

# ------------------------------------------------------------------------------
# 4. LEITURA DE TODAS AS ABAS
# ------------------------------------------------------------------------------
sheetsnames <- googlesheets4::sheet_names(CONFIG$gs_url)
logmsg("Abas encontradas: %s", paste(sheetsnames, collapse = ", "))

ma <- lapply(sheetsnames, function(x) as.data.frame(googlesheets4::read_sheet(CONFIG$gs_url, sheet = x)))
names(ma) <- sheetsnames

core_cols <- c(CONFIG$col_event, CONFIG$col_n, CONFIG$col_study)
resumo <- list()

# ------------------------------------------------------------------------------
# 5. LOOP PRINCIPAL — uma passada completa por aba/desfecho
# ------------------------------------------------------------------------------
for (outcome in sheetsnames) {

  logmsg("==== DESFECHO: %s ====", outcome)
  df <- ma[[outcome]]
  prefix <- file.path(CONFIG$out_dir, gsub("[^A-Za-z0-9_-]", "_", outcome))
  doc <- officer::read_docx()   # documento de outputs deste desfecho
  doc <- officer::body_add_par(doc, paste0("Meta-analysis outputs — ", outcome),
                               style = "heading 1")

  # 5.1 Validação --------------------------------------------------------------
  faltando <- setdiff(core_cols, names(df))
  if (length(faltando) > 0) {
    logmsg("  PULADA — faltam colunas: %s", paste(faltando, collapse = ", ")); next
  }
  names(df)[match(core_cols, names(df))] <- c("event", "n", "study")
  df <- df[!is.na(df$event) & !is.na(df$n), , drop = FALSE]
  k <- nrow(df)
  if (k < 2) { logmsg("  PULADA — apenas %d estudo(s) válido(s).", k); next }

  # 5.2 Modelo principal + forest ----------------------------------------------
  meta <- tryCatch(fit_metaprop(df), error = function(e) {
    logmsg("  ERRO no modelo principal: %s", conditionMessage(e)); NULL })
  if (is.null(meta)) next

  prev <- plogis(meta$TE.random) * 100
  logmsg("  k = %d | prevalência = %.1f%% [%.1f–%.1f] | I² = %.1f%%",
         k, prev, plogis(meta$lower.random) * 100, plogis(meta$upper.random) * 100,
         meta$I2 * 100)
  doc <- docx_block(doc, "Main analysis (GLMM, logit-transformed proportion)", summary(meta))

  save_plot(paste0(prefix, "_Forestplot"), function()
    forest(meta, layout = "Revman5", leftcols = FCOLS, leftlabs = FLABS,
           colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
           colgap.forest.left = "6mm", just = "center", col.square = "darkblue",
           col.square.lines = "black", sortvar = TE,
           ref = round(prev, 2)))   # linha central explícita = estimativa combinada

  # 5.3 Subgrupos (todas as colunas categóricas) -------------------------------
  for (sg in detect_subgroups(df, c("event", "n", "study"))) {
    dfg <- df[!is.na(df[[sg]]) & nzchar(as.character(df[[sg]])), , drop = FALSE]  # remove grupo "NA"
    obj <- tryCatch(fit_metaprop(dfg, subgroup = dfg[[sg]]),
                    error = function(e) { logmsg("  subgrupo '%s' falhou: %s",
                                                 sg, conditionMessage(e)); NULL })
    if (is.null(obj)) next
    logmsg("  Subgrupo '%s': p(entre subgrupos) = %s", sg,
           formatC(obj$pval.Q.b.random, digits = 4, format = "f"))
    doc <- docx_block(doc, paste0("Subgroup analysis: ", sg), summary(obj))
    local({
      obj_l <- obj
      save_plot(paste0(prefix, "_Subgroup_", sg), function()
        forest(obj_l, layout = "Revman5", leftcols = FCOLS, leftlabs = FLABS,
               colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
               colgap.forest.left = "6mm", just = "center", col.square = "darkblue",
               col.square.lines = "black", sortvar = TE,
               print.subgroup.name = FALSE, test.subgroup = TRUE))  # mostra Tau²/I²/p
    })
  }

  # 5.4 Leave-one-out ----------------------------------------------------------
  loo <- tryCatch(metainf(meta, pooled = "random"), error = function(e) NULL)
  if (!is.null(loo))
    save_plot(paste0(prefix, "_Leave-one-out"), function()
      forest(loo, col.bg = "lightblue", col.diamond = "gray", pscale = 100,
             ff.xlab = "bold", rightcols = c("effect", "ci", "I2"),
             colgap.right = "6mm", just = "center"))

  # 5.5 Viés de publicação — só com k >= 10 (Cochrane/JBI) ---------------------
  peters_p <- NA_real_
  if (k >= CONFIG$min_k_bias) {
    fxlim <- pad_xlim(meta$TE, left = 0.35, right = 0.18)  # evita truncar rótulos
    save_plot(paste0(prefix, "_FunnelPlot"), function()
      funnel(meta, studlab = TRUE, bg = "red", cex = 2, cex.studlab = 0.6,  # fonte menor -> menos sobreposição
             random = TRUE, common = FALSE, backtransf = FALSE, xlim = fxlim))

    peters <- tryCatch(metabias(meta, method.bias = "peters"), error = function(e) NULL)
    if (!is.null(peters)) {
      pv <- suppressWarnings(as.numeric(peters$pval)[1])
      if (length(pv) && !is.na(pv)) peters_p <- pv
      logmsg("  Teste de Peters: p = %s", formatC(peters_p, digits = 3, format = "f"))
      # Output do teste de Peters em .docx próprio (pareado com o funnel plot)
      docx_single(paste0(prefix, "_PetersTest.docx"), peters)
    }
    tf <- tryCatch(trimfill(meta), error = function(e) NULL)  # EXPLORATÓRIO em proporções
    if (!is.null(tf)) {
      # Output do trim-and-fill em .docx próprio (pareado com o funnel trim-fill)
      docx_single(paste0(prefix, "_TrimFill.docx"), summary(tf))
      txlim <- pad_xlim(tf$TE, left = 0.35, right = 0.18)
      local({ tf_l <- tf
        save_plot(paste0(prefix, "_FunnelPlot_TrimFill"), function()
          funnel(tf_l, studlab = TRUE, bg = "red", cex = 2, cex.studlab = 0.6,
                 random = TRUE, common = FALSE, backtransf = FALSE, xlim = txlim)) })
    }
  } else {
    logmsg("  Viés/funnel/trim-fill PULADOS (k=%d < %d).", k, CONFIG$min_k_bias)
  }

  # 5.6 Meta-regressão UNIVARIÁVEL + bubble — 1 por covariável -----------------
  covs <- detect_covariates(df, c("event", "n", "study"))
  metareg_out <- character(0)
  for (cov in covs) {
    if (k < CONFIG$min_k_per_cov) {
      logmsg("  metareg '%s' PULADA (k=%d < %d).", cov, k, CONFIG$min_k_per_cov); next }
    mr <- tryCatch(metareg(meta, as.formula(paste0("~", cov))),
                   error = function(e) { logmsg("  metareg '%s' falhou: %s",
                                                cov, conditionMessage(e)); NULL })
    if (is.null(mr)) next
    p <- mr$pval[2]; r2 <- if (is.null(mr$R2)) NA else mr$R2
    logmsg("  Meta-regressão ~%s: p = %.3f | R² = %.1f%%", cov, p, r2)
    metareg_out <- c(metareg_out, sprintf("%s(p=%.3f)", cov, p))
    # Output de CADA meta-regressão em seu próprio .docx (pareado com o bubble plot)
    docx_single(paste0(prefix, "_MetaReg_", cov, ".docx"), summary(mr))

    lab <- nice_label(cov); bxlim <- pad_xlim(df[[cov]], left = 0.42, right = 0.15)
    local({ mr_l <- mr; lab_l <- lab; xl <- bxlim
      save_plot(paste0(prefix, "_Bubble_", cov), function()
        bubble(mr_l, xlab = lab_l, ylab = "Prevalence",   # eixo Y em proporção (retro-transformado) -> sem "logit"
               studlab = TRUE, cex.studlab = 0.65, xlim = xl,
               col = "darkblue", bg = "lightblue")) })
  }

  # 5.7 Grava o DOCX e o sumário do desfecho -----------------------------------
  print(doc, target = paste0(prefix, "_Resultados.docx"))
  resumo[[outcome]] <- data.frame(
    desfecho = outcome, k = k, prevalencia = round(prev, 1),
    ic_inf = round(plogis(meta$lower.random) * 100, 1),
    ic_sup = round(plogis(meta$upper.random) * 100, 1),
    I2 = round(meta$I2 * 100, 1), peters_p = round(peters_p, 3),
    metareg = paste(metareg_out, collapse = "; "), stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------------
# 6. TABELA-RESUMO CONSOLIDADA
# ------------------------------------------------------------------------------
if (length(resumo) > 0) {
  tab <- do.call(rbind, resumo)
  write.csv(tab, file.path(CONFIG$out_dir, "resumo_desfechos.csv"), row.names = FALSE)
  logmsg("Concluído. Resumo salvo em resumo_desfechos.csv (%d desfechos).", nrow(tab))
  print(tab)
} else logmsg("Nenhum desfecho válido foi analisado.")
