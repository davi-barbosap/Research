# ==============================================================
#  TOOLKIT UNIFICADO DE META-ANALISE  (auto-detect)
#  Fonte: Google Sheets (publico) | Saida: subpasta por aba
# ==============================================================
#
#  Um unico script processa TODAS as abas da planilha. Para cada
#  aba, o formato e detectado automaticamente:
#
#    +---------------------------+-------------------+--------------------+
#    | Aba contem a coluna...     | Formato           | Motor              |
#    +---------------------------+-------------------+--------------------+
#    | treatment (formato longo) | NMA em rede       | gemtc (Bayesiano)  |
#    | .e / .c   (formato largo) | Pairwise          | meta (frequentista)|
#    +---------------------------+-------------------+--------------------+
#
#  Dentro de cada formato, o tipo do desfecho tambem e detectado:
#    NMA:      binario  -> colunas study, responders, sampleSize, treatment
#              continuo -> colunas study, mean, std.dev, sampleSize, treatment
#    Pairwise: binario  -> colunas study, event.e, n.e, event.c, n.c
#              continuo -> colunas study, n.e, mean.e, sd.e, n.c, mean.c, sd.c
#
#  IMPORTANTE (NMA): o nome da aba DEVE terminar com (+) ou (-) para
#  indicar a direcao do SUCRA. Ex.: "Mortality (-)", "Quality (+)".
#  (Abas pairwise ignoram esse sufixo.)
# ==============================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  # --- NMA ---
  gemtc, rjags, coda, dmetar, netmeta,
  # --- Pairwise ---
  meta, ggrepel,
  # --- Comuns / IO / graficos ---
  googlesheets4, openxlsx, dplyr, ggplot2, ragg, magick, officer
)

# ==============================================================
#  CONFIGURACAO -- edite apenas este bloco
# ==============================================================

# >>> COLE AQUI a URL da planilha unificada (deixe entre aspas) <<<
SHEETS_URL <- "https://docs.google.com/spreadsheets/d/1NK6VjG4nLlao-JVoMQwNBpJqiUKJisS-_iSnboky2Ok/edit?gid=1461524718#gid=1461524718"

OUTPUT_DIR <- "/Users/davibarbosa/Desktop/Maju"

# ── Network plot estetico (apenas abas NMA) ─────────────────
#   CANVAS_IN      : tamanho do canvas (polegadas, quadrado) -- ancora de escala.
#   LABEL_SCALE    : ajuste de fonte dos rotulos (>1 aumenta, <1 diminui).
#   REF_TREATMENT  : tratamento posicionado na base do circulo (layout).
#   RANK_REFERENCE : baseline do modelo Bayesiano. Aceita:
#                      - um nome EXATO de tratamento (ex.: "Placebo")
#                      - ""     -> nenhum relevel
#                      - "auto" -> tratamento presente em MAIS estudos (mais conectado)
#                    NAO afeta o ranking/SUCRA (invariante ao baseline num modelo
#                    de consistencia); muda so a parametrizacao e a estabilidade.
#   KNOWN_ACRONYMS : siglas que devem ficar em MAIUSCULAS (ex.: c("IV","TXA")).
CANVAS_IN      <- 20
LABEL_SCALE    <- 1.0
REF_TREATMENT  <- "Placebo"
RANK_REFERENCE <- "auto"
REF_COLOR      <- "#0f203e"
PLACEBO_COLOR  <- "#0f203e"
OTHER_COLOR    <- "#0f203e"
EDGE_COLOR     <- "#A0A0A0"
LABEL_COLOR    <- "#2A2A2A"
KNOWN_ACRONYMS <- character(0)

# ── Parametros MCMC (apenas abas NMA) ───────────────────────
#   O node splitting reroda um modelo por comparacao; se estiver lento,
#   reduza NMA_MCMC_NS (ex.: n.iter = 100000) sem mexer no modelo principal.
NMA_MCMC    <- list(n.adapt = 200000, n.iter = 1000000, thin = 50)
NMA_MCMC_NS <- list(n.adapt = 200000, n.iter = 1000000, thin = 50)

# ── Rotulos dos forest plots (apenas abas pairwise) ─────────
#   Por padrao os rotulos sao LIDOS do nome da aba, no formato:
#       "Nome do desfecho (Braco A vs Braco B)"
#   -> Braco A = intervencao (.e); Braco B = controle (.c).
#   Os valores abaixo sao apenas FALLBACK, usados quando o nome da aba
#   nao contiver o padrao "(A vs B)".
PW_LABEL_E     <- "Intervention"
PW_LABEL_C     <- "Control"
PW_LABEL_LEFT  <- "Favors intervention"
PW_LABEL_RIGHT <- "Favors control"

# ==============================================================
#  Autenticacao Google -- planilhas publicas (sem login)
# ==============================================================
gs4_deauth()

if (!nzchar(trimws(SHEETS_URL)))
  stop("Defina SHEETS_URL no bloco de configuracao antes de rodar.")

ss <- as_sheets_id(SHEETS_URL)

# ==============================================================
#  HELPERS COMPARTILHADOS
# ==============================================================

safe_name <- function(x) trimws(gsub("[^[:alnum:] _\\-]", "", x))

# Detecta o formato da aba a partir dos nomes de coluna (case-insensitive)
detect_format <- function(df) {
  cols <- tolower(names(df))
  if ("treatment" %in% cols) return("nma")
  if (any(c("mean.e", "sd.e", "event.e", "event.c", "n.e") %in% cols))
    return("pairwise")
  return("unknown")
}

# --- .docx a partir de uma lista de secoes {heading, content} (uso NMA) -------
save_docx_sections <- function(sections, path) {
  doc <- read_docx()
  for (s in sections) {
    if (!is.null(s$heading))
      doc <- body_add_par(doc, s$heading, style = "heading 2")
    txt <- tryCatch(
      paste(capture.output(print(s$content)), collapse = "\n"),
      error = function(e) paste("Erro ao capturar output:", e$message)
    )
    doc <- body_add_par(doc, txt, style = "Normal")
    doc <- body_add_par(doc, "",  style = "Normal")
  }
  print(doc, target = path)
}

# --- .docx a partir de linhas de texto monoespacado (uso pairwise) -----------
save_docx_lines <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 9)
  doc  <- officer::read_docx()
  for (line in lines)
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(line, prop)))
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
}

# --- Figura base via ragg (TIFF multipagina) + PNG da 1a pagina (uso NMA) -----
save_fig <- function(plot_fn, base_path, width = 10, height = 7) {
  tiff_path <- paste0(base_path, ".tiff")
  agg_tiff(tiff_path, width = width, height = height, units = "in",
           res = 300, compression = "lzw")
  tryCatch(plot_fn(), finally = dev.off())
  tryCatch({
    img <- magick::image_read(tiff_path)
    magick::image_write(img[1], path = paste0(base_path, ".png"), format = "png")
  }, error = function(e) {
    warning("PNG nao gerado para '", basename(base_path), "': ", e$message)
  })
}

# --- ggplot -> TIFF + PNG sem limites forcados (uso NMA network plot) --------
save_fig_gg <- function(gg_plot, base_path, canvas_in = CANVAS_IN) {
  tiff_path <- paste0(base_path, ".tiff")
  ggsave(tiff_path, plot = gg_plot,
         width = canvas_in, height = canvas_in, units = "in", dpi = 300,
         device = agg_tiff, compression = "lzw")
  tryCatch({
    img <- magick::image_read(tiff_path)
    magick::image_write(img[1], path = paste0(base_path, ".png"), format = "png")
  }, error = function(e) {
    warning("PNG nao gerado para '", basename(base_path), "': ", e$message)
  })
}

# --- Plot base via ragg: renderiza TIFF e PNG separadamente (uso pairwise) ----
save_plot <- function(filename_base, plot_fn, out_dir,
                      width = 16, height = 10, res = 300) {
  agg_tiff(file.path(out_dir, paste0(filename_base, ".tiff")),
           width = width, height = height, units = "in",
           res = res, compression = "lzw")
  plot_fn(); dev.off()
  
  agg_png(file.path(out_dir, paste0(filename_base, ".png")),
          width = width, height = height, units = "in", res = res)
  plot_fn(); dev.off()
}

# ==============================================================
#  BLOCO NMA  (gemtc)  ------------------------------------------------
# ==============================================================

detect_type_nma <- function(df) {
  cols <- tolower(names(df))
  if ("responders" %in% cols)              return("binary")
  if (all(c("mean", "std.dev") %in% cols)) return("continuous")
  stop(
    "Colunas NMA nao reconhecidas: ", paste(names(df), collapse = ", "), "\n",
    "Esperado (binario):  study, responders, sampleSize, treatment\n",
    "Esperado (continuo): study, mean, std.dev, sampleSize, treatment"
  )
}

detect_direction <- function(sheet_name) {
  if (grepl("(+)", sheet_name, fixed = TRUE)) return(1L)
  if (grepl("(-)", sheet_name, fixed = TRUE)) return(-1L)
  warning("'", sheet_name, "': sem sinal (+/-). Usando -1 como padrao.")
  return(-1L)
}

# Limpeza NMA: mantem os nomes originais (gemtc exige sampleSize/std.dev exatos)
prep_nma <- function(df) {
  df <- as.data.frame(df)
  names(df) <- trimws(names(df))
  # Coluna de estudo pode vir como "Study, year", "Study", "studlab" etc.
  if (!"study" %in% names(df)) {
    cand <- which(grepl("^study", names(df), ignore.case = TRUE) |
                    tolower(names(df)) == "studlab")
    if (length(cand) > 0) names(df)[cand[1]] <- "study"
  }
  if ("study" %in% names(df))
    df <- df[!is.na(df$study) & trimws(as.character(df$study)) != "", ]
  for (col in c("sampleSize", "responders", "mean", "std.dev")) {
    if (col %in% names(df))
      df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  rownames(df) <- NULL
  df
}

wrap_label <- function(x, width = 14) {
  words <- strsplit(x, " ")[[1]]
  lines <- character(0); cur <- ""
  for (w in words) {
    candidate <- if (nchar(cur) == 0) w else paste(cur, w)
    if (nchar(candidate) <= width || nchar(cur) == 0) cur <- candidate
    else { lines <- c(lines, cur); cur <- w }
  }
  if (nchar(cur) > 0) lines <- c(lines, cur)
  paste(lines, collapse = "\n")
}

format_trt_label <- function(name, known_acronyms = character(0)) {
  parts <- strsplit(name, "_")[[1]]
  formatted <- vapply(parts, function(p) {
    if (p %in% known_acronyms) return(toupper(p))
    if (grepl("^[A-Z0-9]+$", p) && nchar(p) <= 6) return(toupper(p))
    paste0(toupper(substr(p, 1, 1)), tolower(substr(p, 2, nchar(p))))
  }, character(1))
  paste(formatted, collapse = " ")
}

REF_CANVAS <- 14   # canvas de calibracao (nao altere)

plot_nma_network_custom <- function(
    nma_df, ref_treatment,
    ref_color = "#1B2E6E", placebo_color = "#4A7C59", other_color = "#7B3A70",
    edge_color = "#A0A0A0", label_color = "#2A2A2A",
    known_acronyms = character(0),
    canvas_in = CANVAS_IN, label_scale = LABEL_SCALE) {
  
  s             <- canvas_in / REF_CANVAS
  max_node_size <- 40.0 * s
  min_node_size <-  1.5 * s
  label_size    <-  8.5 * s * label_scale
  edge_max      <- 11.0 * s
  edge_min      <-  2.2 * s
  label_gap     <-  0.08
  margin_pt     <- round(55 * s)
  coord_range   <- 2.52
  mm_per_coord  <- canvas_in * 25.4 / coord_range
  
  trt_n <- aggregate(nma_df$sampleSize,
                     by = list(treatment = nma_df$treatment), FUN = sum)
  names(trt_n)[2] <- "total_n"
  trt_n$treatment <- as.character(trt_n$treatment)
  
  pairs_list <- lapply(unique(nma_df$study), function(s_id) {
    trts <- sort(unique(as.character(nma_df$treatment[nma_df$study == s_id])))
    if (length(trts) < 2) return(NULL)
    idx  <- t(combn(seq_along(trts), 2))
    data.frame(t1 = trts[idx[, 1]], t2 = trts[idx[, 2]],
               stringsAsFactors = FALSE)
  })
  all_pairs   <- do.call(rbind, Filter(Negate(is.null), pairs_list))
  edge_counts <- aggregate(rep(1L, nrow(all_pairs)),
                           by = list(t1 = all_pairs$t1, t2 = all_pairs$t2),
                           FUN = sum)
  names(edge_counts)[3] <- "n_studies"
  edge_counts$t1 <- as.character(edge_counts$t1)
  edge_counts$t2 <- as.character(edge_counts$t2)
  
  trts   <- sort(unique(as.character(nma_df$treatment)))
  n_t    <- length(trts)
  angles <- seq(0, 2 * pi * (1 - 1 / n_t), length.out = n_t)
  ref_idx <- which(trts == ref_treatment)
  if (length(ref_idx) > 0) {
    shift  <- (3 * pi / 2) - angles[ref_idx]
    angles <- (angles + shift) %% (2 * pi)
  }
  
  nodes <- data.frame(treatment = trts, angle = angles,
                      x = cos(angles), y = sin(angles),
                      stringsAsFactors = FALSE)
  nodes <- merge(nodes, trt_n, by = "treatment")
  
  nodes$label <- vapply(nodes$treatment, format_trt_label,
                        character(1), known_acronyms = known_acronyms)
  nodes$label_full <- paste0(
    vapply(nodes$label, wrap_label, character(1), width = 14),
    "\n(n = ", formatC(nodes$total_n, format = "d", big.mark = ","), ")")
  
  has_placebo <- any(grepl("^placebo$", trts, ignore.case = TRUE))
  nodes$fill <- dplyr::case_when(
    nodes$treatment == ref_treatment ~ ref_color,
    has_placebo & grepl("^placebo$", nodes$treatment, ignore.case = TRUE) ~ placebo_color,
    TRUE ~ other_color
  )
  
  n_max <- max(nodes$total_n)
  nodes$node_r_coord <- (max_node_size / 2) *
    sqrt(nodes$total_n / n_max) / mm_per_coord
  nodes$label_r <- 1 + nodes$node_r_coord + label_gap
  nodes$lx <- cos(nodes$angle) * nodes$label_r
  nodes$ly <- sin(nodes$angle) * nodes$label_r
  nodes$hjust <- ifelse(cos(nodes$angle) < -0.1, 1,
                        ifelse(cos(nodes$angle) > 0.1, 0, 0.5))
  nodes$vjust <- ifelse(sin(nodes$angle) > 0.1, 0,
                        ifelse(sin(nodes$angle) < -0.1, 1, 0.5))
  
  edges <- merge(edge_counts, nodes[, c("treatment", "x", "y")],
                 by.x = "t1", by.y = "treatment")
  names(edges)[4:5] <- c("x1", "y1")
  edges <- merge(edges, nodes[, c("treatment", "x", "y")],
                 by.x = "t2", by.y = "treatment")
  names(edges)[6:7] <- c("x2", "y2")
  
  ggplot() +
    geom_segment(data = edges,
                 aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n_studies),
                 color = edge_color, alpha = 0.70, lineend = "round") +
    geom_point(data = nodes, aes(x = x, y = y, size = total_n),
               color = nodes$fill) +
    geom_text(data = nodes, aes(x = lx, y = ly, label = label_full),
              hjust = nodes$hjust, vjust = nodes$vjust,
              size = label_size, fontface = "plain",
              color = label_color, lineheight = 1.1) +
    scale_size_area(max_size = max_node_size, guide = "none") +
    scale_linewidth_continuous(range = c(edge_min, edge_max), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.25)) +
    scale_y_continuous(expand = expansion(mult = 0.20)) +
    coord_fixed(clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(margin_pt, margin_pt, margin_pt, margin_pt))
}

exp_league_table <- function(league_matrix) {
  apply(as.matrix(league_matrix), c(1, 2), function(cell) {
    if (is.na(cell) || cell == "") return(cell)
    if (grepl("\\(", cell)) {
      nums <- suppressWarnings(
        as.numeric(unlist(regmatches(cell, gregexpr("-?\\d+\\.\\d+|-?\\d+", cell)))))
      nums <- nums[!is.na(nums)]
      if (length(nums) == 3) {
        nums_exp <- round(exp(nums), 2)
        return(sprintf("%.2f (%.2f, %.2f)", nums_exp[1], nums_exp[2], nums_exp[3]))
      }
    }
    return(cell)
  })
}

# Poda a rede para manter apenas o maior componente conectado
fix_disconnected_network <- function(df) {
  edges <- do.call(rbind, lapply(unique(df$study), function(s) {
    trts <- unique(df$treatment[df$study == s])
    if (length(trts) < 2) return(NULL)
    t(combn(sort(trts), 2))
  }))
  if (is.null(edges))
    return(list(data = df, disconnected = FALSE,
                trts_dropped = character(0), studs_dropped = character(0)))
  
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  names(edges) <- c("from", "to")
  all_trts <- unique(c(edges$from, edges$to))
  adj <- matrix(0L, length(all_trts), length(all_trts),
                dimnames = list(all_trts, all_trts))
  for (i in seq_len(nrow(edges))) {
    adj[edges$from[i], edges$to[i]] <- 1L
    adj[edges$to[i], edges$from[i]] <- 1L
  }
  
  visited <- setNames(rep(FALSE, length(all_trts)), all_trts)
  components <- list()
  for (start in all_trts) {
    if (visited[start]) next
    queue <- start; comp <- character(0)
    while (length(queue) > 0) {
      node <- queue[1]; queue <- queue[-1]
      if (visited[node]) next
      visited[node] <- TRUE
      comp <- c(comp, node)
      nbrs <- all_trts[adj[node, ] == 1L]
      queue <- c(queue, nbrs[!visited[nbrs]])
    }
    components <- c(components, list(comp))
  }
  
  if (length(components) == 1L)
    return(list(data = df, disconnected = FALSE,
                trts_dropped = character(0), studs_dropped = character(0)))
  
  sizes     <- vapply(components, length, integer(1))
  keep_trts <- components[[which.max(sizes)]]
  drop_trts <- setdiff(all_trts, keep_trts)
  df_clean  <- df[df$treatment %in% keep_trts, ]
  
  arms_per_study <- tapply(df_clean$treatment, df_clean$study,
                           function(x) length(unique(x)))
  drop_studs <- names(arms_per_study[arms_per_study < 2])
  df_clean   <- df_clean[!df_clean$study %in% drop_studs, ]
  
  list(data = df_clean, disconnected = TRUE,
       trts_dropped = drop_trts, studs_dropped = drop_studs)
}

run_nma <- function(sheet, df_raw, base_dir = OUTPUT_DIR) {
  out_dir <- file.path(base_dir, safe_name(sheet))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  nma_df <- prep_nma(df_raw)
  
  # --- 0. Rede desconectada -------------------------------------------------
  net_check <- fix_disconnected_network(nma_df)
  nma_df    <- net_check$data
  
  data_type <- detect_type_nma(nma_df)
  direction <- detect_direction(sheet)
  
  # --- 0b. Referencia do modelo Bayesiano -----------------------------------
  # NAO altera o ranking/SUCRA (invariante ao baseline num modelo de consistencia);
  # muda so a parametrizacao (d_ref = 0), a coluna-base da league table e a
  # estabilidade numerica. "auto" = tratamento presente em mais estudos.
  studies_per_trt <- sort(
    tapply(nma_df$study, as.character(nma_df$treatment),
           function(x) length(unique(x))),
    decreasing = TRUE)
  # String "o que foi considerado": Trat:nEstudos, Trat:nEstudos, ...
  ranking_str <- paste0(names(studies_per_trt), ":", studies_per_trt,
                        collapse = ", ")
  
  ref_choice <- trimws(RANK_REFERENCE)
  ref_modo   <- if (identical(tolower(ref_choice), "auto")) "auto"
  else if (nchar(ref_choice) > 0) "fixo" else "nenhum"
  if (ref_modo == "auto") {
    ref_choice <- names(studies_per_trt)[1]
  }
  ref_aplicada <- NA_character_
  if (nchar(ref_choice) > 0 &&
      ref_choice %in% unique(as.character(nma_df$treatment))) {
    nma_df$treatment <- relevel(factor(as.character(nma_df$treatment)),
                                ref = ref_choice)
    ref_aplicada <- ref_choice
  }
  ref_n_estudos <- if (!is.na(ref_aplicada)) as.integer(studies_per_trt[ref_aplicada]) else NA_integer_
  
  # --- 1. Rede + network plot ----------------------------------------------
  nma_net <- mtc.network(data.ab = nma_df, description = paste("NMA -", sheet))
  net_gg  <- plot_nma_network_custom(
    nma_df = nma_df, ref_treatment = REF_TREATMENT,
    ref_color = REF_COLOR, placebo_color = PLACEBO_COLOR,
    other_color = OTHER_COLOR, edge_color = EDGE_COLOR,
    label_color = LABEL_COLOR, known_acronyms = KNOWN_ACRONYMS)
  save_fig_gg(net_gg, file.path(out_dir, "network_plot"))
  
  # --- 2. Modelo de consistencia -------------------------------------------
  if (data_type == "continuous") {
    model_nma <- mtc.model(nma_net, type = "consistency", n.chain = 4,
                           linearModel = "random",
                           likelihood = "normal", link = "identity")
  } else {
    model_nma <- mtc.model(nma_net, type = "consistency", n.chain = 4,
                           linearModel = "random",
                           likelihood = "binom", link = "log")
  }
  
  # --- 3. MCMC --------------------------------------------------------------
  set.seed(42)
  mcmc_nma <- mtc.run(model_nma, n.adapt = NMA_MCMC$n.adapt,
                      n.iter = NMA_MCMC$n.iter, thin = NMA_MCMC$thin)
  mcmc_sum <- summary(mcmc_nma)
  gelman_d <- tryCatch(gelman.diag(mcmc_nma),
                       error = function(e) paste("gelman.diag falhou:", e$message))
  save_fig(function() plot(mcmc_nma),
           file.path(out_dir, "trace_plot"), width = 14, height = 10)
  save_docx_sections(list(
    list(heading = paste("MCMC Summary -", sheet), content = mcmc_sum),
    list(heading = "Gelman-Rubin Diagnostic",      content = gelman_d)
  ), file.path(out_dir, "mcmc_summary.docx"))
  
  # --- 4. Gelman plot -------------------------------------------------------
  save_fig(function() gelman.plot(mcmc_nma), file.path(out_dir, "gelman_plot"))
  
  # --- 5. League table ------------------------------------------------------
  raw_league <- relative.effect.table(mcmc_nma)
  league_tbl <- if (data_type == "binary")
    exp_league_table(as.data.frame(raw_league)) else as.data.frame(raw_league)
  write.xlsx(as.data.frame(league_tbl),
             file.path(out_dir, "league_table.xlsx"), rowNames = TRUE)
  
  # --- 6. Rank probability & SUCRA -----------------------------------------
  # A direcao e aplicada UMA vez, via preferredDirection. NAO reaplicar em
  # sucra() (lower.is.better = FALSE sempre) -- reaplicar inverteria o SUCRA.
  rank_prob <- rank.probability(mcmc_nma, preferredDirection = direction)
  sucra_res <- sucra(rank_prob, lower.is.better = FALSE)
  save_docx_sections(list(
    list(heading = "Rank Probability", content = rank_prob),
    list(heading = "SUCRA",            content = sucra_res)
  ), file.path(out_dir, "rank_sucra.docx"))
  
  # --- 7. Node splitting (com fallback) ------------------------------------
  tryCatch({
    set.seed(42)
    if (data_type == "continuous") {
      ns <- mtc.nodesplit(nma_net, linearModel = "random",
                          likelihood = "normal", link = "identity",
                          n.adapt = NMA_MCMC_NS$n.adapt,
                          n.iter = NMA_MCMC_NS$n.iter, thin = NMA_MCMC_NS$thin)
    } else {
      ns <- mtc.nodesplit(nma_net, linearModel = "random",
                          likelihood = "binom", link = "log",
                          n.adapt = NMA_MCMC_NS$n.adapt,
                          n.iter = NMA_MCMC_NS$n.iter, thin = NMA_MCMC_NS$thin)
    }
    ns_sum <- summary(ns)
    save_fig(function() plot(ns_sum), file.path(out_dir, "nodesplit_plot"))
    save_docx_sections(list(list(heading = "Node Splitting Summary",
                                 content = ns_sum)),
                       file.path(out_dir, "nodesplit.docx"))
  }, error = function(e) {
    has_placebo <- "Placebo" %in% unique(as.character(nma_df$treatment))
    ref <- if (has_placebo) "Placebo" else sort(unique(nma_df$treatment))[1]
    if (data_type == "binary") {
      pw <- netmeta::pairwise(event = responders, n = sampleSize,
                              treat = treatment, studlab = study,
                              data = nma_df, sm = "RR")
      net_freq <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw,
                          sm = "RR", random = TRUE, common = FALSE,
                          reference.group = ref, method.tau = "REML")
    } else {
      pw <- netmeta::pairwise(treat = treatment, mean = mean, sd = `std.dev`,
                              n = sampleSize, studlab = study,
                              data = nma_df, sm = "MD")
      net_freq <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw,
                          sm = "MD", random = TRUE, common = FALSE,
                          reference.group = ref, method.tau = "REML")
    }
    qd <- decomp.design(net_freq)
    save_docx_sections(list(list(
      heading = "Inconsistencia - decomp.design (fallback node splitting)",
      content = qd)), file.path(out_dir, "nodesplit_fallback_inconsistency.docx"))
  })
  
  list(
    aba           = sheet,
    formato       = "NMA",
    tipo          = data_type,
    direcao       = if (direction == 1L) "(+) maior = melhor" else "(-) menor = melhor",
    n_estudos     = length(unique(nma_df$study)),
    n_tratamentos = length(unique(as.character(nma_df$treatment))),
    ref_modo      = ref_modo,
    referencia    = if (!is.na(ref_aplicada)) ref_aplicada else "(nenhuma)",
    ref_n_estudos = ref_n_estudos,
    rede_podada   = if (net_check$disconnected)
      paste0("SIM (removidos: ",
             paste(net_check$trts_dropped, collapse = "; "), ")")
    else "nao",
    considerados  = ranking_str
  )
}

# ==============================================================
#  BLOCO PAIRWISE  (meta)  --------------------------------------------
# ==============================================================

# Limpeza pairwise: minusculas, filtra linhas vazias, coage numericos
prep_pairwise <- function(df) {
  df <- as.data.frame(df)
  names(df) <- trimws(tolower(names(df)))
  
  # Coluna de estudo pode vir como "study, year", "study", "studlab" etc.
  if (!"study" %in% names(df)) {
    cand <- which(grepl("^study", names(df)) | names(df) == "studlab")
    if (length(cand) > 0) names(df)[cand[1]] <- "study"
  }
  
  num_cols <- c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c",
                "event.e", "event.c")
  for (col in num_cols) {
    if (col %in% names(df))
      df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  
  # Remove linhas sem rotulo de estudo
  if ("study" %in% names(df))
    df <- df[!is.na(df$study) & trimws(as.character(df$study)) != "", ]
  
  # Remove linhas em que TODAS as colunas de desfecho presentes sao NA
  present <- intersect(num_cols, names(df))
  if (length(present) > 0)
    df <- df[rowSums(!is.na(df[, present, drop = FALSE])) > 0, ]
  
  rownames(df) <- NULL
  df
}

detect_subgroup <- function(df) {
  cols <- names(df)[grepl("^subgroup", names(df), ignore.case = TRUE)]
  if (length(cols) > 0) cols[1] else NULL
}

# Le "Nome do desfecho (Braco A vs Braco B)" e devolve os rotulos do forest:
#   label.e / label.c   -> cabecalhos das colunas intervencao (.e) / controle (.c)
#   label.left / .right -> "Favors ..." sob o eixo (esq = A/.e, dir = B/.c)
# Aceita separadores: "vs", "vs.", "versus" (case-insensitive). Se a aba nao
# contiver o padrao "(A vs B)", cai nos fallbacks PW_LABEL_*.
parse_pw_labels <- function(sheet) {
  fallback <- list(label.e = PW_LABEL_E,     label.c = PW_LABEL_C,
                   label.left = PW_LABEL_LEFT, label.right = PW_LABEL_RIGHT)
  
  # Captura todos os grupos entre parenteses (o desfecho pode ter os seus)
  groups <- regmatches(sheet, gregexpr("\\(([^()]+)\\)", sheet))[[1]]
  groups <- gsub("^\\(|\\)$", "", groups)
  
  sep  <- "(?i)\\s+(?:vs\\.?|versus)\\s+"   # vs | vs. | versus
  cand <- groups[grepl(sep, groups, perl = TRUE)]
  if (length(cand) == 0) return(fallback)
  
  # Usa o ultimo parenteses valido (caso o desfecho tenha parenteses antes)
  arms <- trimws(strsplit(cand[length(cand)], sep, perl = TRUE)[[1]])
  arms <- arms[nzchar(arms)]
  if (length(arms) != 2) return(fallback)
  
  list(label.e     = arms[1], label.c = arms[2],
       label.left  = paste("Favors", arms[1]),
       label.right = paste("Favors", arms[2]))
}

.theme <- list(
  col.square = "blue", col.square.lines = "black",
  col.diamond = "black", col.diamond.lines = "black",
  col.predict = "red", col.predict.lines = "red",
  fontsize = 10, ff.study = "plain", ff.heading = "bold"
)

.cont_cols <- list(
  leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c",
               "w.random", "effect", "ci"),
  leftlabs = c("Studies", "Mean", "SD", "Total", "Mean", "SD", "Total",
               "Weight", "MD", "95% CI")
)

.bin_cols <- list(
  leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c",
               "w.random", "effect", "ci"),
  leftlabs = c("Studies", "Events", "Total", "Events", "Total",
               "Weight", "RR", "95% CI"),
  ff.lr = "bold", col.label.left = "black", col.label.right = "black"
)

# label.e/.c/.left/.right sao injetados por aba via parse_pw_labels() (nao aqui)
.forest_base <- list(
  layout = "RevMan5", rightcols = FALSE, just.addcols = "right",
  pooled.events = TRUE, pooled.totals = TRUE,
  digits = 2, digits.pval = 3, digits.sd = 2,
  colgap = "6mm", colgap.forest = "6mm",
  print.pval.Q = TRUE, print.tau.ci = TRUE, print.subgroup.name = FALSE
)

.loo_base <- list(
  col.bg = "#AEC6CF", ff.xlab = "bold",
  leftcols = "studlab", leftlabs = "Omitted Study",
  rightcols = c("effect", "ci", "I2"),
  rightlabs = c("Estimate", "95% CI", "I2")
)

# --- Fase 1: forest + summary -------------------------------------------------
run_continuous <- function(sheet, df, subgroup_col = NULL, base_dir = OUTPUT_DIR,
                           pw_lab = parse_pw_labels(sheet)) {
  out_dir <- if (!is.null(subgroup_col))
    file.path(base_dir, safe_name(sheet), "subgroup")
  else file.path(base_dir, safe_name(sheet))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (nrow(df) < 2) return(NULL)
  
  has <- !is.null(subgroup_col)
  sg  <- if (has) list(subgroup = df[[subgroup_col]]) else list()
  
  ma <- do.call(metacont, c(list(
    n.e = df$n.e, mean.e = df$mean.e, sd.e = df$sd.e,
    n.c = df$n.c, mean.c = df$mean.c, sd.c = df$sd.c,
    studlab = df$study, sm = "MD", method.tau = "REML",
    method.I2 = "tau2", method.random.ci = "HK", method.predict = "HK",
    random = TRUE, common = FALSE, prediction = TRUE), sg))
  
  save_plot("01_forest", function() {
    do.call(meta::forest, c(modifyList(.forest_base, pw_lab), .cont_cols, .theme, list(
      x = ma, sortvar = ma$studlab,
      overall = !has, overall.hetstat = !has, test.overall.random = !has,
      test.effect.subgroup.random = if (has) FALSE else NULL)))
  }, out_dir)
  save_docx_lines("output_01_meta", capture.output(summary(ma)), out_dir)
  
  invisible(ma)
}

run_binary <- function(sheet, df, subgroup_col = NULL, base_dir = OUTPUT_DIR,
                       pw_lab = parse_pw_labels(sheet)) {
  out_dir <- if (!is.null(subgroup_col))
    file.path(base_dir, safe_name(sheet), "subgroup")
  else file.path(base_dir, safe_name(sheet))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (nrow(df) < 2) return(NULL)
  
  has <- !is.null(subgroup_col)
  sg  <- if (has) list(subgroup = df[[subgroup_col]]) else list()
  
  ma <- do.call(metabin, c(list(
    event.e = df$event.e, n.e = df$n.e,
    event.c = df$event.c, n.c = df$n.c,
    studlab = df$study, sm = "RR", method = "Inverse", method.tau = "REML",
    method.I2 = "tau2", method.random.ci = "HK", method.predict = "HK",
    random = TRUE, common = FALSE, prediction = TRUE), sg))
  
  save_plot("01_forest", function() {
    do.call(meta::forest, c(modifyList(.forest_base, pw_lab), .bin_cols, .theme, list(
      x = ma, sortvar = ma$studlab, print.Q = TRUE,
      overall = !has, overall.hetstat = !has, test.overall.random = !has)))
  }, out_dir)
  save_docx_lines("output_01_meta", capture.output(summary(ma)), out_dir)
  
  invisible(ma)
}

# --- Fase 2: LOO + vies de publicacao ----------------------------------------
run_loo_continuous <- function(sheet, df, base_dir = OUTPUT_DIR) {
  out_dir <- file.path(base_dir, safe_name(sheet))
  if (nrow(df) < 3) return(NULL)   # LOO exige >= 3 estudos
  
  ma <- do.call(metacont, c(list(
    n.e = df$n.e, mean.e = df$mean.e, sd.e = df$sd.e,
    n.c = df$n.c, mean.c = df$mean.c, sd.c = df$sd.c,
    studlab = df$study, sm = "MD", method.tau = "REML",
    method.I2 = "tau2", method.random.ci = "HK", method.predict = "HK",
    random = TRUE, common = FALSE, prediction = TRUE)))
  
  LOO <- metainf(ma, pooled = "random", prediction = FALSE)
  save_plot("02_LOO", function() {
    do.call(meta::forest, c(.loo_base,
                            .theme[c("col.diamond", "col.diamond.lines", "fontsize",
                                     "ff.heading", "ff.study")],
                            list(x = LOO, xlab = "Pooled MD (Omitting Study)")))
  }, out_dir)
  save_docx_lines("output_02_LOO", capture.output(print(LOO)), out_dir)
  
  invisible(LOO)
}

run_loo_binary <- function(sheet, df, base_dir = OUTPUT_DIR) {
  out_dir <- file.path(base_dir, safe_name(sheet))
  if (nrow(df) < 3) return(NULL)   # LOO exige >= 3 estudos
  
  ma <- do.call(metabin, c(list(
    event.e = df$event.e, n.e = df$n.e,
    event.c = df$event.c, n.c = df$n.c,
    studlab = df$study, sm = "RR", method = "Inverse", method.tau = "REML",
    method.I2 = "tau2", method.random.ci = "HK", method.predict = "HK",
    random = TRUE, common = FALSE, prediction = TRUE)))
  
  LOO <- metainf(ma, pooled = "random", prediction = FALSE)
  save_plot("02_LOO", function() {
    do.call(meta::forest, c(.loo_base,
                            .theme[c("col.diamond", "col.diamond.lines", "fontsize",
                                     "ff.heading", "ff.study")],
                            list(x = LOO, xlab = "Pooled RR (Omitting Study)")))
  }, out_dir)
  save_docx_lines("output_02_LOO", capture.output(print(LOO)), out_dir)
  
  if (nrow(df) >= 10) {
    tf <- meta::trimfill(ma)
    save_plot("03_funnel_trimfill", function() {
      meta::funnel(tf, studlab = TRUE, bg = "lightblue", cex = 1,
                   cex.studlab = 0.8, random = TRUE, common = FALSE,
                   backtransf = FALSE, contour = c(0.95, 0.99))
    }, out_dir)
    save_docx_lines("output_03_trimfill", capture.output(summary(tf)), out_dir)
    
    egger <- eggers.test(ma)
    save_docx_lines("output_04_egger", capture.output(print(egger)), out_dir)
  }
  
  invisible(LOO)
}

run_pairwise <- function(sheet, df_raw, base_dir = OUTPUT_DIR) {
  df <- prep_pairwise(df_raw)
  ptype <- if (all(c("event.e", "event.c") %in% names(df))) "binary" else "continuous"
  sg_col <- detect_subgroup(df)
  pw_lab <- parse_pw_labels(sheet)   # rotulos lidos do nome da aba (1x)
  
  if (ptype == "continuous") {
    run_continuous(sheet, df, base_dir = base_dir, pw_lab = pw_lab)
    if (!is.null(sg_col))
      run_continuous(sheet, df, subgroup_col = sg_col, base_dir = base_dir, pw_lab = pw_lab)
    run_loo_continuous(sheet, df, base_dir = base_dir)
  } else {
    run_binary(sheet, df, base_dir = base_dir, pw_lab = pw_lab)
    if (!is.null(sg_col))
      run_binary(sheet, df, subgroup_col = sg_col, base_dir = base_dir, pw_lab = pw_lab)
    run_loo_binary(sheet, df, base_dir = base_dir)
  }
  list(
    aba          = sheet,
    formato      = "Pairwise",
    tipo         = ptype,
    n_estudos    = nrow(df),
    grupo_e      = pw_lab$label.e,
    grupo_c      = pw_lab$label.c,
    subgrupo     = if (!is.null(sg_col)) sg_col else "-"
  )
}

# ==============================================================
#  LOOP PRINCIPAL  --------------------------------------------
# ==============================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
log_dir <- file.path(OUTPUT_DIR, "_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

all_sheets <- googlesheets4::sheet_names(ss)

erros     <- list()
pulados   <- list()
relatorio <- list()   # metadados por aba, para o relatorio final

for (sheet in all_sheets) {
  
  info <- tryCatch({
    df_raw <- as.data.frame(read_sheet(ss, sheet = sheet))
    fmt    <- detect_format(df_raw)
    
    if (fmt == "nma") {
      run_nma(sheet, df_raw)
    } else if (fmt == "pairwise") {
      run_pairwise(sheet, df_raw)
    } else {
      msg <- paste0("Formato nao reconhecido (nem 'treatment' nem '.e/.c'). Colunas: ",
                    paste(names(df_raw), collapse = ", "))
      pulados[[sheet]] <- msg
      list(aba = sheet, formato = "PULADA", detalhe = msg)
    }
  }, error = function(e) {
    detalhe <- paste0(
      "Aba      : ", sheet, "\n",
      "Mensagem : ", e$message, "\n",
      "Data/hora: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    erros[[sheet]] <<- detalhe
    writeLines(detalhe, file.path(log_dir, paste0("ERRO_", safe_name(sheet), ".txt")))
    list(aba = sheet, formato = "ERRO", detalhe = e$message)
  })
  
  relatorio[[sheet]] <- info
}

# ── Relatorio final por desfecho -> _relatorio_final.txt ────────────────────
# Mostra, para cada aba, o que foi considerado (com foco na referencia AUTO).
{
  linhas <- c(
    "==========================================================",
    "  RELATORIO FINAL POR DESFECHO",
    paste0("  Gerado em : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("  Planilha  : ", SHEETS_URL),
    paste0("  Saida     : ", OUTPUT_DIR),
    "==========================================================",
    ""
  )
  
  for (nm in names(relatorio)) {
    r <- relatorio[[nm]]
    linhas <- c(linhas, paste0("[", r$formato, "]  ", r$aba))
    
    if (identical(r$formato, "NMA")) {
      linhas <- c(linhas,
                  paste0("   Tipo          : ", r$tipo),
                  paste0("   Direcao rank  : ", r$direcao, "   (definida pelo sufixo +/-)"),
                  paste0("   Estudos       : ", r$n_estudos,
                         "  |  Tratamentos: ", r$n_tratamentos),
                  paste0("   Referencia    : ", r$referencia,
                         "   (modo: ", r$ref_modo,
                         if (!is.na(r$ref_n_estudos))
                           paste0("; presente em ", r$ref_n_estudos, " estudos)") else ")"),
                  paste0("   Rede podada   : ", r$rede_podada),
                  paste0("   Considerados  : ", r$considerados,
                         "   (tratamento:n_estudos)"))
    } else if (identical(r$formato, "Pairwise")) {
      linhas <- c(linhas,
                  paste0("   Tipo          : ", r$tipo),
                  paste0("   Estudos       : ", r$n_estudos),
                  paste0("   Grupos        : .e = ", r$grupo_e, "  |  .c = ", r$grupo_c),
                  paste0("   Subgrupo      : ", r$subgrupo))
    } else {  # PULADA ou ERRO
      linhas <- c(linhas, paste0("   Detalhe       : ", r$detalhe))
    }
    linhas <- c(linhas, "")
  }
  
  n_total <- length(all_sheets)
  n_erros <- length(erros)
  n_pul   <- length(pulados)
  n_ok    <- n_total - n_erros - n_pul
  linhas <- c(linhas,
              "==========================================================",
              "  RESUMO",
              paste0("  Total de abas : ", n_total),
              paste0("  Concluidas    : ", n_ok),
              paste0("  Puladas       : ", n_pul),
              paste0("  Com ERRO      : ", n_erros,
                     if (n_erros > 0) "  (ver _logs/ERRO_*.txt)" else ""),
              "==========================================================")
  
  relatorio_path <- file.path(OUTPUT_DIR, "_relatorio_final.txt")
  writeLines(linhas, relatorio_path)
}
