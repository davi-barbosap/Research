# ==============================================================
#  Network Meta-Analysis — Trombectomia M2 (ACM)
#  Técnicas: Direct Aspiration (DA), Stent Retriever (SR),
#            Terapia combinada (DA_SR)
#  Todos os desfechos DICOTÔMICOS | Sem placebo
#  Referência DINÂMICA: tratamento mais frequente em cada desfecho
#  Fonte: Google Sheets | Saída: pasta por desfecho + relatório final
# ==============================================================
#
#  Estrutura esperada de cada aba:
#    study | year | responders | sampleSize | treatment
#
#  Nome da aba DEVE terminar com (+) ou (-) para indicar a direção
#  clínica do desfecho (higher-is-better vs lower-is-better).
#    (+)  desfecho bom  -> ex.: "Recanalization (+)", "mRS 0-2 (+)"
#    (-)  desfecho ruim -> ex.: "Mortality (-)", "sICH (-)"
#
#  Efeito: risco relativo (RR) via binomial + link log.
#  Incerteza: intervalo de CREDIBILIDADE de 95% (ICr) — inferência bayesiana.
# ==============================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gemtc, rjags, coda, dmetar, netmeta,
  googlesheets4, openxlsx, dplyr, ggplot2,
  ragg, magick, officer, flextable
)

# ==============================================================
#  CONFIGURAÇÃO — edite apenas este bloco
# ==============================================================

SHEETS_URL <- "https://docs.google.com/spreadsheets/d/11jydixqgUUFA2Ev6uCQwdu1GfVjpUjWn5oWj1mECCHk/edit"
BASE_DIR   <- "/Users/davibarbosa/Desktop/Stats"
GS_EMAIL   <- "davibarbosaps@gmail.com"   # e-mail Google (evita trava em execução não-interativa)

# ── Abas a processar ────────────────────────────────────────
#   character(0) => detecta automaticamente TODAS as abas da planilha.
#   Ou informe um vetor explícito, ex.: c("Recanalization (+)", "Mortality (-)")
SHEET_LIST <- character(0)

# ── Medida de efeito ────────────────────────────────────────
#   "RR" -> binomial + link log (risco relativo). Fixo para este projeto.
EFFECT_MEASURE <- "RR"

# ── Referência dinâmica ─────────────────────────────────────
#   A referência de cada desfecho = tratamento que aparece em MAIS estudos.
#   Critério de desempate (nesta ordem):
#     1) maior nº de estudos  2) maior N somado  3) ordem alfabética
#   O braço escolhido é apenas a ÂNCORA de parametrização/apresentação
#   (base level do JAGS, orientação de forest/league). NÃO altera a posteriori
#   dos efeitos relativos nem o SUCRA — estes são invariantes à referência.

# ── MCMC (alto por opção — precisão máxima; thin>1 só por memória) ──
MCMC <- list(n.adapt = 200000, n.iter = 1000000, thin = 50)
SEED <- 42
#   PSRF é apenas REPORTADO (não há reprocessamento automático).

# ── Rótulos de tratamento (apresentação) ────────────────────
TRT_LABELS <- c(DA = "DA", SR = "SR", DA_SR = "DA + SR")

# ── Network plot ────────────────────────────────────────────
CANVAS_IN      <- 20
LABEL_SCALE    <- 1.0
REF_COLOR      <- "#0f203e"   # nó de referência (destacado)
OTHER_COLOR    <- "#5a6b85"   # demais nós
EDGE_COLOR     <- "#A0A0A0"
LABEL_COLOR    <- "#2A2A2A"
KNOWN_ACRONYMS <- c("DA", "SR")

# ==============================================================
#  Autenticação Google
# ==============================================================
if (nzchar(GS_EMAIL)) gs4_auth(email = GS_EMAIL) else gs4_auth()
ss <- as_sheets_id(SHEETS_URL)

# ── Funções auxiliares ──────────────────────────────────────

detect_direction <- function(sheet_name) {
  if (grepl("(+)", sheet_name, fixed = TRUE)) return(1L)
  if (grepl("(-)", sheet_name, fixed = TRUE)) return(-1L)
  warning("'", sheet_name, "': aba sem sinal (+/-). Assumindo (-) [lower-is-better].")
  return(-1L)
}

safe_name <- function(x) trimws(gsub("[^[:alnum:] _\\-]", "", x))

wrap_label <- function(x, width = 14) {
  words <- strsplit(x, " ")[[1]]; lines <- character(0); cur <- ""
  for (w in words) {
    cand <- if (nchar(cur) == 0) w else paste(cur, w)
    if (nchar(cand) <= width || nchar(cur) == 0) cur <- cand
    else { lines <- c(lines, cur); cur <- w }
  }
  if (nchar(cur) > 0) lines <- c(lines, cur)
  paste(lines, collapse = "\n")
}

# Rótulo de tratamento: usa TRT_LABELS se houver; senão heurística de siglas
format_trt_label <- function(name, known_acronyms = character(0)) {
  if (name %in% names(TRT_LABELS)) return(unname(TRT_LABELS[[name]]))
  parts <- strsplit(name, "_")[[1]]
  fmt <- vapply(parts, function(p) {
    if (p %in% known_acronyms) return(toupper(p))
    if (grepl("^[A-Z0-9]+$", p) && nchar(p) <= 6) return(toupper(p))
    paste0(toupper(substr(p, 1, 1)), tolower(substr(p, 2, nchar(p))))
  }, character(1))
  paste(fmt, collapse = " ")
}

# ── Validação e limpeza dos dados (medida de segurança) ──────
#   - coage colunas (googlesheets4 devolve list-column em tipos mistos)
#   - remove duplicatas exatas study×treatment (erro de extração)
#   - checa integridade: responders<=sampleSize, valores >0, sem NA
clean_validate <- function(df, sheet) {
  df <- as.data.frame(df)
  need <- c("study", "responders", "sampleSize", "treatment")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0)
    stop("Colunas ausentes: ", paste(miss, collapse = ", "))

  df$study      <- as.character(unlist(df$study))
  df$treatment  <- as.character(unlist(df$treatment))
  df$responders <- suppressWarnings(as.numeric(unlist(df$responders)))
  df$sampleSize <- suppressWarnings(as.numeric(unlist(df$sampleSize)))

  notes <- character(0)

  dup_exact <- duplicated(df[, c("study", "treatment", "responders", "sampleSize")])
  if (any(dup_exact)) {
    notes <- c(notes, sprintf("Removidas %d linha(s) duplicada(s) exata(s).", sum(dup_exact)))
    df <- df[!dup_exact, ]
  }
  key <- paste(df$study, df$treatment, sep = "||")
  if (any(duplicated(key)))
    stop("Conflito de dados: study×treatment repetido com valores divergentes -> ",
         paste(unique(key[duplicated(key)]), collapse = "; "))

  bad <- is.na(df$responders) | is.na(df$sampleSize) |
    df$sampleSize <= 0 | df$responders < 0 | df$responders > df$sampleSize
  if (any(bad))
    stop("Linhas inválidas (NA / n<=0 / responders>sampleSize): ",
         paste(which(bad), collapse = ", "))

  n_zero <- sum(df$responders == 0)
  if (n_zero > 0)
    notes <- c(notes, sprintf("%d braço(s) com zero eventos (sem correção de continuidade no modelo bayesiano).", n_zero))

  list(data = df, notes = notes)
}

# ── Escolha da referência (tratamento mais frequente) ────────
choose_reference <- function(df) {
  n_studies <- tapply(df$study, df$treatment, function(s) length(unique(s)))
  n_sum     <- tapply(df$sampleSize, df$treatment, sum)
  trts      <- names(n_studies)
  tbl <- data.frame(
    treatment = trts,
    n_studies = as.integer(n_studies[trts]),
    n_total   = as.numeric(n_sum[trts]),
    stringsAsFactors = FALSE
  )
  ord <- order(-tbl$n_studies, -tbl$n_total, tbl$treatment)
  tbl <- tbl[ord, ]
  ref <- tbl$treatment[1]
  tie <- sum(tbl$n_studies == tbl$n_studies[1]) > 1
  rationale <- sprintf(
    "%s aparece em %d estudo(s) (N=%s)%s",
    ref, tbl$n_studies[1], format(tbl$n_total[1], big.mark = ","),
    if (tie) " — empate em nº de estudos resolvido por maior N somado" else ""
  )
  list(ref = ref, table = tbl, rationale = rationale, tie = tie)
}

# ── Network plot (referência destacada; sem placebo) ─────────
REF_CANVAS <- 14
plot_nma_network_custom <- function(
    nma_df, ref_treatment,
    ref_color = REF_COLOR, other_color = OTHER_COLOR,
    edge_color = EDGE_COLOR, label_color = LABEL_COLOR,
    known_acronyms = KNOWN_ACRONYMS,
    canvas_in = CANVAS_IN, label_scale = LABEL_SCALE) {

  s <- canvas_in / REF_CANVAS
  max_node_size <- 40.0 * s; min_node_size <- 1.5 * s
  label_size <- 8.5 * s * label_scale
  edge_max <- 11.0 * s; edge_min <- 2.2 * s
  label_gap <- 0.08; margin_pt <- round(55 * s)
  coord_range <- 2.52; mm_per_coord <- canvas_in * 25.4 / coord_range

  trt_n <- aggregate(nma_df$sampleSize, by = list(treatment = nma_df$treatment), FUN = sum)
  names(trt_n)[2] <- "total_n"; trt_n$treatment <- as.character(trt_n$treatment)

  pairs_list <- lapply(unique(nma_df$study), function(s_id) {
    trts <- sort(unique(as.character(nma_df$treatment[nma_df$study == s_id])))
    if (length(trts) < 2) return(NULL)
    idx <- t(combn(seq_along(trts), 2))
    data.frame(t1 = trts[idx[, 1]], t2 = trts[idx[, 2]], stringsAsFactors = FALSE)
  })
  all_pairs <- do.call(rbind, Filter(Negate(is.null), pairs_list))
  edge_counts <- aggregate(rep(1L, nrow(all_pairs)),
                           by = list(t1 = all_pairs$t1, t2 = all_pairs$t2), FUN = sum)
  names(edge_counts)[3] <- "n_studies"
  edge_counts$t1 <- as.character(edge_counts$t1); edge_counts$t2 <- as.character(edge_counts$t2)

  trts <- sort(unique(as.character(nma_df$treatment))); n_t <- length(trts)
  angles <- seq(0, 2 * pi * (1 - 1 / n_t), length.out = n_t)
  ref_idx <- which(trts == ref_treatment)
  if (length(ref_idx) > 0) { shift <- (3 * pi / 2) - angles[ref_idx]; angles <- (angles + shift) %% (2 * pi) }

  nodes <- data.frame(treatment = trts, angle = angles,
                      x = cos(angles), y = sin(angles), stringsAsFactors = FALSE)
  nodes <- merge(nodes, trt_n, by = "treatment")
  nodes$label <- vapply(nodes$treatment, format_trt_label, character(1), known_acronyms = known_acronyms)
  nodes$label_full <- paste0(vapply(nodes$label, wrap_label, character(1), width = 14),
                             "\n(n = ", formatC(nodes$total_n, format = "d", big.mark = ","), ")")
  nodes$fill <- ifelse(nodes$treatment == ref_treatment, ref_color, other_color)

  n_max <- max(nodes$total_n)
  nodes$node_r_coord <- (max_node_size / 2) * sqrt(nodes$total_n / n_max) / mm_per_coord
  nodes$label_r <- 1 + nodes$node_r_coord + label_gap
  nodes$lx <- cos(nodes$angle) * nodes$label_r; nodes$ly <- sin(nodes$angle) * nodes$label_r
  nodes$hjust <- ifelse(cos(nodes$angle) < -0.1, 1, ifelse(cos(nodes$angle) > 0.1, 0, 0.5))
  nodes$vjust <- ifelse(sin(nodes$angle) >  0.1, 0, ifelse(sin(nodes$angle) < -0.1, 1, 0.5))

  edges <- merge(edge_counts, nodes[, c("treatment", "x", "y")], by.x = "t1", by.y = "treatment")
  names(edges)[4:5] <- c("x1", "y1")
  edges <- merge(edges, nodes[, c("treatment", "x", "y")], by.x = "t2", by.y = "treatment")
  names(edges)[6:7] <- c("x2", "y2")

  ggplot() +
    geom_segment(data = edges, aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n_studies),
                 color = edge_color, alpha = 0.70, lineend = "round") +
    geom_point(data = nodes, aes(x = x, y = y, size = total_n), color = nodes$fill) +
    geom_text(data = nodes, aes(x = lx, y = ly, label = label_full),
              hjust = nodes$hjust, vjust = nodes$vjust, size = label_size,
              fontface = "plain", color = label_color, lineheight = 1.1) +
    scale_size_area(max_size = max_node_size, guide = "none") +
    scale_linewidth_continuous(range = c(edge_min, edge_max), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.25)) +
    scale_y_continuous(expand = expansion(mult = 0.20)) +
    coord_fixed(clip = "off") + theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(margin_pt, margin_pt, margin_pt, margin_pt))
}

save_fig_gg <- function(gg_plot, base_path, canvas_in = CANVAS_IN) {
  ggsave(paste0(base_path, ".tiff"), plot = gg_plot,
         width = canvas_in, height = canvas_in, units = "in", dpi = 300,
         device = agg_tiff, compression = "lzw")
  ggsave(paste0(base_path, ".png"), plot = gg_plot,
         width = canvas_in, height = canvas_in, units = "in", dpi = 300, device = agg_png)
}

save_fig <- function(plot_fn, base_path, width = 10, height = 7) {
  agg_tiff(paste0(base_path, ".tiff"), width = width, height = height,
           units = "in", res = 300, compression = "lzw")
  tryCatch(plot_fn(), finally = dev.off())
  tryCatch({
    img <- magick::image_read(paste0(base_path, ".tiff"))
    magick::image_write(img[1], path = paste0(base_path, ".png"), format = "png")
  }, error = function(e) warning("PNG nao gerado para '", basename(base_path), "': ", e$message))
}

save_docx <- function(sections, path) {
  doc <- read_docx()
  for (s in sections) {
    if (!is.null(s$heading)) doc <- body_add_par(doc, s$heading, style = "heading 2")
    txt <- tryCatch(paste(capture.output(print(s$content)), collapse = "\n"),
                    error = function(e) paste("Erro ao capturar output:", e$message))
    doc <- body_add_par(doc, txt, style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
  }
  print(doc, target = path)
}

# League table (RR) a partir do array 3-D de relative.effect.table.
# Células: mediana (ICr 2.5%, ICr 97.5%). Exponencia (log-RR -> RR; monótono).
# Orientação gemtc: célula [linha i, coluna j] = efeito da COLUNA j vs LINHA i.
build_league <- function(mcmc_nma, exp_scale) {
  arr <- unclass(relative.effect.table(mcmc_nma))   # [t, t, 3] = 2.5%, 50%, 97.5%
  if (exp_scale) arr <- exp(arr)
  lg <- apply(arr, c(1, 2), function(v)
    if (any(is.na(v))) "—" else sprintf("%.2f (%.2f, %.2f)", v[2], v[1], v[3]))
  diag(lg) <- rownames(lg)
  as.data.frame(lg, stringsAsFactors = FALSE)
}

# Efeitos relativos (RR) de TODOS os tratamentos vs a referência escolhida.
# Colunas: RR + ICr 95% (intervalo de credibilidade).
ref_effects_table <- function(mcmc_nma, ref, exp_scale) {
  trts <- as.character(mcmc_nma$model$network$treatments$id)
  others <- setdiff(trts, ref)
  rows <- lapply(others, function(t) {
    smp <- as.matrix(relative.effect(mcmc_nma, ref, t, preserve.extra = FALSE)$samples)
    q <- quantile(smp, c(0.025, 0.5, 0.975))
    if (exp_scale) q <- exp(q)
    data.frame(comparacao = paste0(t, " vs ", ref),
               RR = round(q[2], 2), ICr_inf = round(q[1], 2), ICr_sup = round(q[3], 2),
               row.names = NULL)
  })
  out <- do.call(rbind, rows)
  names(out) <- c("comparacao", "RR", "ICr_2.5%", "ICr_97.5%")
  out
}

# Poda para o maior componente conectado (raro com 3 nós, mas defensivo)
fix_disconnected_network <- function(df) {
  edges <- do.call(rbind, lapply(unique(df$study), function(s) {
    trts <- unique(df$treatment[df$study == s])
    if (length(trts) < 2) return(NULL); t(combn(sort(trts), 2))
  }))
  if (is.null(edges)) return(list(data = df, disconnected = FALSE,
                                  trts_dropped = character(0), studs_dropped = character(0)))
  edges <- as.data.frame(edges, stringsAsFactors = FALSE); names(edges) <- c("from", "to")
  all_trts <- unique(c(edges$from, edges$to))
  adj <- matrix(0L, length(all_trts), length(all_trts), dimnames = list(all_trts, all_trts))
  for (i in seq_len(nrow(edges))) { adj[edges$from[i], edges$to[i]] <- 1L; adj[edges$to[i], edges$from[i]] <- 1L }
  visited <- setNames(rep(FALSE, length(all_trts)), all_trts); components <- list()
  for (start in all_trts) {
    if (visited[start]) next
    queue <- start; comp <- character(0)
    while (length(queue) > 0) {
      node <- queue[1]; queue <- queue[-1]; if (visited[node]) next
      visited[node] <- TRUE; comp <- c(comp, node)
      nbrs <- all_trts[adj[node, ] == 1L]; queue <- c(queue, nbrs[!visited[nbrs]])
    }
    components <- c(components, list(comp))
  }
  if (length(components) == 1L) return(list(data = df, disconnected = FALSE,
                                            trts_dropped = character(0), studs_dropped = character(0)))
  sizes <- vapply(components, length, integer(1))
  keep_trts <- components[[which.max(sizes)]]; drop_trts <- setdiff(all_trts, keep_trts)
  df_clean <- df[df$treatment %in% keep_trts, ]
  arms <- tapply(df_clean$treatment, df_clean$study, function(x) length(unique(x)))
  drop_studs <- names(arms[arms < 2]); df_clean <- df_clean[!df_clean$study %in% drop_studs, ]
  list(data = df_clean, disconnected = TRUE, trts_dropped = drop_trts, studs_dropped = drop_studs)
}

# Constrói e roda o modelo bayesiano RR (binomial + link log).
run_bayes <- function(nma_net, link, mcmc) {
  model <- mtc.model(nma_net, type = "consistency", n.chain = 4,
                     linearModel = "random", likelihood = "binom", link = link)
  set.seed(SEED)
  mtc.run(model, n.adapt = mcmc$n.adapt, n.iter = mcmc$n.iter, thin = mcmc$thin)
}

# ==============================================================
#  Loop principal
# ==============================================================
erros <- list(); avisos <- list(); report_rows <- list()
log_dir <- file.path(BASE_DIR, "_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

link_of <- function(m) if (m == "OR") "logit" else "log"

sheet_list <- if (length(SHEET_LIST) > 0) SHEET_LIST else googlesheets4::sheet_names(ss)

for (sheet in sheet_list) {

  cat("\n======================================================\n")
  cat(">>> Desfecho:", sheet, "\n")
  cat("======================================================\n")

  sucesso <- tryCatch({

    out_dir <- file.path(BASE_DIR, safe_name(sheet))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    raw <- read_sheet(ss, sheet = sheet)
    cv  <- clean_validate(raw, sheet); nma_df <- cv$data
    sheet_notes <- cv$notes

    net_check <- fix_disconnected_network(nma_df); nma_df <- net_check$data
    if (net_check$disconnected) {
      msg <- paste0("Rede podada p/ maior componente. Removidos: ",
                    paste(net_check$trts_dropped, collapse = ", "))
      avisos[[sheet]] <- msg; sheet_notes <- c(sheet_notes, msg); message("[AVISO] ", sheet, ": ", msg)
    }

    direction <- detect_direction(sheet)

    # --- Referência dinâmica -------------------------------------------
    refsel <- choose_reference(nma_df); ref <- refsel$ref
    message("[INFO] Referência (mais frequente): ", ref, " | ", refsel$rationale)
    trt_order <- c(ref, setdiff(sort(unique(nma_df$treatment)), ref))

    # --- Rede + plot ---------------------------------------------------
    nma_net <- mtc.network(data.ab = nma_df, treatments = trt_order,
                           description = paste("NMA -", sheet))
    save_fig_gg(plot_nma_network_custom(nma_df, ref_treatment = ref),
                file.path(out_dir, "network_plot"))

    # --- Modelo bayesiano RR (MCMC alto) -------------------------------
    measure  <- EFFECT_MEASURE
    mcmc_nma <- run_bayes(nma_net, link_of(measure), MCMC)

    # Diagnóstico (reportado, sem gate)
    gelman_d <- gelman.diag(mcmc_nma, multivariate = FALSE)
    psrf_max <- max(gelman_d$psrf[, "Point est."], na.rm = TRUE)
    if (psrf_max > 1.05)
      sheet_notes <- c(sheet_notes, sprintf("PSRF max = %.3f (>1.05) — inspecionar trace/gelman.", psrf_max))

    mcmc_sum <- summary(mcmc_nma)
    exp_scale <- TRUE   # binário -> exponencia (RR)

    save_fig(function() plot(mcmc_nma), file.path(out_dir, "trace_plot"), width = 14, height = 10)
    save_fig(function() gelman.plot(mcmc_nma, autoburnin = FALSE), file.path(out_dir, "gelman_plot"))

    # τ (heterogeneidade) e DIC
    tau_row <- tryCatch(mcmc_sum$summaries$statistics["sd.d", , drop = FALSE], error = function(e) NULL)
    dic_val <- tryCatch(mcmc_sum$DIC, error = function(e) NULL)

    save_docx(list(
      list(heading = paste("MCMC Summary -", sheet, "(RR; ICr 95%)"), content = mcmc_sum),
      list(heading = "Heterogeneidade (sd.d = tau)", content = if (is.null(tau_row)) "n/d" else tau_row),
      list(heading = "DIC / model fit", content = if (is.null(dic_val)) "n/d" else dic_val),
      list(heading = "Gelman-Rubin (PSRF)", content = gelman_d)
    ), file.path(out_dir, "mcmc_summary.docx"))

    # --- League table + efeitos vs referência (ICr 95%) ----------------
    write.xlsx(build_league(mcmc_nma, exp_scale),
               file.path(out_dir, "league_table.xlsx"), rowNames = TRUE)
    write.xlsx(ref_effects_table(mcmc_nma, ref, exp_scale),
               file.path(out_dir, paste0("effects_vs_", safe_name(ref), ".xlsx")), rowNames = FALSE)

    # --- Ranking + SUCRA (direção aplicada UMA vez) --------------------
    rank_prob <- rank.probability(mcmc_nma, preferredDirection = direction)
    sucra_res <- sucra(rank_prob, lower.is.better = FALSE)
    save_docx(list(
      list(heading = "Rank Probability", content = rank_prob),
      list(heading = "SUCRA", content = sucra_res)
    ), file.path(out_dir, "rank_sucra.docx"))

    # --- Inconsistência: node splitting só se houver loop fechado ------
    splits <- tryCatch(mtc.nodesplit.comparisons(nma_net), error = function(e) NULL)
    has_loop <- !is.null(splits) && nrow(as.data.frame(splits)) > 0

    if (has_loop) {
      ns <- mtc.nodesplit(nma_net, linearModel = "random", likelihood = "binom",
                          link = link_of(measure),
                          n.adapt = MCMC$n.adapt, n.iter = MCMC$n.iter, thin = MCMC$thin)
      ns_sum <- summary(ns)
      save_fig(function() plot(ns_sum), file.path(out_dir, "nodesplit_plot"))
      save_docx(list(list(heading = "Node Splitting Summary", content = ns_sum)),
                file.path(out_dir, "nodesplit.docx"))
      sheet_notes <- c(sheet_notes, "Node splitting executado (loop fechado presente).")
    } else {
      pw <- pairwise(event = responders, n = sampleSize, treat = treatment,
                     studlab = study, data = nma_df, sm = measure)
      net_freq <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw, sm = measure,
                          random = TRUE, common = FALSE, reference.group = ref, method.tau = "REML")
      save_docx(list(list(heading = "Inconsistência - decomp.design (sem loop p/ node splitting)",
                          content = decomp.design(net_freq))),
                file.path(out_dir, "nodesplit_fallback_inconsistency.docx"))
      sheet_notes <- c(sheet_notes, "Sem loop fechado; inconsistência via decomp.design (frequentista).")
    }

    # --- Linha do relatório final --------------------------------------
    cnt <- refsel$table
    counts_str <- paste(sprintf("%s=%d(N%s)", cnt$treatment, cnt$n_studies,
                                format(cnt$n_total, big.mark = ",")), collapse = "; ")
    report_rows[[sheet]] <- data.frame(
      Desfecho     = sheet,
      Direcao      = ifelse(direction == 1L, "(+) higher better", "(-) lower better"),
      Referencia   = ref,
      Ref_estudos  = cnt$n_studies[cnt$treatment == ref],
      Ref_N        = format(cnt$n_total[cnt$treatment == ref], big.mark = ","),
      Empate       = ifelse(refsel$tie, "sim", "nao"),
      Frequencias  = counts_str,
      Medida       = measure,
      N_estudos    = length(unique(nma_df$study)),
      N_tratamentos= length(unique(nma_df$treatment)),
      PSRF_max     = round(psrf_max, 3),
      Node_split   = ifelse(has_loop, "sim", "nao (decomp.design)"),
      Observacoes  = ifelse(length(sheet_notes) > 0, paste(sheet_notes, collapse = " | "), "—"),
      stringsAsFactors = FALSE
    )
    TRUE
  }, error = function(e) {
    call_info <- if (!is.null(e$call)) deparse(e$call)[1] else "desconhecida"
    detalhe <- paste0("Desfecho : ", sheet, "\nMensagem : ", e$message,
                      "\nFuncao   : ", call_info,
                      "\nData/hora: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    erros[[sheet]] <<- detalhe
    writeLines(detalhe, file.path(log_dir, paste0("ERRO_", safe_name(sheet), ".txt")))
    message("\n[!] ERRO em '", sheet, "': ", e$message, "\n")
    FALSE
  })
  if (!sucesso) next
}

# ==============================================================
#  Relatório final: referência adotada por desfecho
# ==============================================================
if (length(report_rows) > 0) {
  report_df <- do.call(rbind, report_rows); rownames(report_df) <- NULL

  write.xlsx(report_df, file.path(BASE_DIR, "_RELATORIO_referencias.xlsx"))
  write.csv(report_df, file.path(BASE_DIR, "_RELATORIO_referencias.csv"), row.names = FALSE)

  ft <- autofit(flextable(report_df))
  doc <- read_docx()
  doc <- body_add_par(doc, "Relatório de referências adotadas por desfecho", style = "heading 1")
  doc <- body_add_par(doc, paste0("Regra: referência = tratamento com maior nº de estudos ",
                                  "(desempate: maior N somado, depois alfabético). ",
                                  "Efeito: RR (binomial/log); incerteza: intervalo de credibilidade 95%. ",
                                  "Gerado em ", format(Sys.time(), "%Y-%m-%d %H:%M"), "."), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft)
  print(doc, target = file.path(BASE_DIR, "_RELATORIO_referencias.docx"))

  cat("\n>>> Relatório final salvo em: ", file.path(BASE_DIR, "_RELATORIO_referencias.{xlsx,csv,docx}"), "\n")
}

# ── Resumo do pipeline ──────────────────────────────────────
n_total <- length(sheet_list); n_erros <- length(erros)
n_avisos <- length(avisos);    n_ok <- n_total - n_erros
cat("\n")
cat("========== RESUMO DO PIPELINE ==========\n")
cat(sprintf("Total de desfechos   : %d\n", n_total))
cat(sprintf("Concluidos com exito : %d\n", n_ok))
cat(sprintf("Com aviso            : %d\n", n_avisos))
cat(sprintf("Com ERRO (pulados)   : %d\n", n_erros))
cat("========================================\n")
if (n_erros > 0) {
  cat("\n--- ERROS ---\n")
  for (nm in names(erros)) cat("\n[", nm, "]\n", erros[[nm]], "\n")
  cat("\nLogs em:", log_dir, "\n")
} else cat("\nTodos os desfechos processados sem erros.\n")
