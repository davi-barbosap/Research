# ==============================================================
#  Network Meta-Analysis — Pipeline Automatizado
#  Fonte: Google Sheets | Saída: pasta por desfecho
# ==============================================================
#
#  Estrutura esperada das abas:
#    Desfecho binário   → colunas: study, responders, sampleSize, treatment
#    Desfecho contínuo  → colunas: study, mean, std.dev, sampleSize, treatment
#
#  Nome da aba DEVE terminar com (+) ou (-) para indicar direção do SUCRA.
#  Exemplo: "Mortality (-)", "Quality of Life (+)"
# ==============================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gemtc,         # Bayesian NMA (mtc.*)
  rjags,         # Backend JAGS para gemtc
  coda,          # Diagnósticos MCMC
  dmetar,        # sucra()
  netmeta,       # NMA frequentista + pairwise + decomp.design
  googlesheets4, # Leitura do Google Sheets
  openxlsx,      # Escrita de .xlsx
  dplyr,         # case_when para lógica de cores dos nós
  ggplot2,       # Network plot estético
  ragg,          # Dispositivos gráficos de alta qualidade (TIFF)
  magick,        # Conversão TIFF → PNG (primeira página)
  officer        # Escrita de .docx
)

# ==============================================================
#  CONFIGURAÇÃO — edite apenas este bloco
# ==============================================================

SHEETS_URL <- "https://docs.google.com/spreadsheets/d/1C5n-j3yOdxn0sCTk7L2GzF5ldikI_hpq1SrdXMPkNvM/edit?gid=862497479#gid=862497479"
BASE_DIR   <- "/Users/davibarbosa/Desktop/Servidoni"

# ── Network plot estético ──────────────────────────────────
#   CANVAS_IN      : tamanho do canvas (polegadas, quadrado). É a ÂNCORA de escala:
#                    todos os elementos visuais (nós, texto, arestas, margens)
#                    derivam proporcionalmente deste valor. Altere só aqui.
#   REF_TREATMENT  : nome EXATO do tratamento referência (como na planilha)
#   REF_COLOR      : cor hex do nó referência
#   PLACEBO_COLOR  : cor hex do nó placebo (ignorado se não houver placebo)
#   OTHER_COLOR    : cor hex dos demais nós (exceto referência e placebo)
#   EDGE_COLOR     : cor hex das linhas de conexão
#   LABEL_COLOR    : cor hex do texto dos rótulos
#   KNOWN_ACRONYMS : vetor de siglas que devem ficar em MAIÚSCULAS
#                    (e.g. c("IV", "IM", "TXA"))
CANVAS_IN      <- 20
LABEL_SCALE    <- 1.0    # ajuste de fonte: > 1 aumenta, < 1 diminui (ex: 0.8, 1.2)
REF_TREATMENT  <- "Placebo"
#   RANK_REFERENCE : braço de referência do modelo Bayesiano (ex: "placebo", "oxytocin").
#                    Deixe "" para nenhuma referência explícita.
#                    Quando preenchido: relevel() posiciona esse braço como nível base
#                    antes de mtc.network() — todos os efeitos relativos são expressos
#                    em função dele (d_ref = 0 na parametrização JAGS).
#                    IMPORTANTE: o braço de referência NÃO é excluído nem recebe SUCRA=0;
#                    ele participa normalmente do ranking com suas próprias probabilidades.
RANK_REFERENCE <- "Placebo"
REF_COLOR      <- "#0f203e"
PLACEBO_COLOR  <- "#0f203e"
OTHER_COLOR    <- "#0f203e"
EDGE_COLOR     <- "#A0A0A0"
LABEL_COLOR    <- "#2A2A2A"
KNOWN_ACRONYMS <- character(0)

# ==============================================================
#  Autenticação Google (abre browser na primeira execução)
# ==============================================================

gs4_auth()
ss         <- as_sheets_id(SHEETS_URL)
# Lê automaticamente todas as abas existentes na planilha
sheet_list <- c("Leiomyoma volume change 3mo (-)", "Leiomyoma volume change 6mo (-)", "Uterine volume change 3mo (-)", "Uterine volume change 6mo (-)")

# ── Funções auxiliares ──────────────────────────────────────

detect_type <- function(df) {
  cols <- tolower(names(df))
  if ("responders" %in% cols)              return("binary")
  if (all(c("mean", "std.dev") %in% cols)) return("continuous")
  stop(
    "Colunas nao reconhecidas: ", paste(names(df), collapse = ", "), "\n",
    "Esperado (binario):   study, responders, sampleSize, treatment\n",
    "Esperado (continuo):  study, mean, std.dev, sampleSize, treatment"
  )
}

detect_direction <- function(sheet_name) {
  if (grepl("(+)", sheet_name, fixed = TRUE)) return(1L)
  if (grepl("(-)", sheet_name, fixed = TRUE)) return(-1L)
  warning("'", sheet_name, "': sem sinal (+/-). Usando -1 como padrao.")
  return(-1L)
}

safe_name <- function(x) trimws(gsub("[^[:alnum:] _\\-]", "", x))


# Quebra texto longo em múltiplas linhas sem cortar palavras
wrap_label <- function(x, width = 14) {
  words <- strsplit(x, " ")[[1]]
  lines <- character(0)
  cur   <- ""
  for (w in words) {
    candidate <- if (nchar(cur) == 0) w else paste(cur, w)
    if (nchar(candidate) <= width || nchar(cur) == 0) {
      cur <- candidate
    } else {
      lines <- c(lines, cur)
      cur   <- w
    }
  }
  if (nchar(cur) > 0) lines <- c(lines, cur)
  paste(lines, collapse = "\n")
}

# Formata nome de tratamento: siglas → CAPS, compostos sem "_", resto Title Case
format_trt_label <- function(name, known_acronyms = character(0)) {
  parts <- strsplit(name, "_")[[1]]
  formatted <- vapply(parts, function(p) {
    if (p %in% known_acronyms) return(toupper(p))
    # Heurística: palavra toda em maiúsculas na planilha → sigla
    if (grepl("^[A-Z0-9]+$", p) && nchar(p) <= 6) return(toupper(p))
    paste0(toupper(substr(p, 1, 1)), tolower(substr(p, 2, nchar(p))))
  }, character(1))
  paste(formatted, collapse = " ")
}

# ── Sistema de proporcionalidade ────────────────────────────────────────────
#
#  canvas_in é a ÚNICA âncora física (polegadas, quadrado).
#  Todos os tamanhos físicos (mm) derivam de s = canvas_in / REF_CANVAS.
#
#  Por que label_gap NÃO escala com s:
#    O raio do maior nó em unidades de coordenada =
#      (max_node_mm / 2) / (canvas_mm / coord_range)
#    Como max_node_mm ∝ s e canvas_mm ∝ s, a razão é constante.
#    Portanto label_gap em coord também é fixo — o gap físico (mm)
#    cresce junto com o canvas automaticamente.
#
#  REF_CANVAS = 14 pol → constantes calibradas contra o plot padrão do gemtc
#  (490 × 490 px medidos diretamente):
#
#    Elemento          % canvas (gemtc)   valor em mm a 6 pol   ref (14 pol)
#    ─────────────────────────────────────────────────────────────────────────
#    max_node_size      11 %               16.8 mm               40 mm
#    label_size         2.5 %              → 8.5 ggplot units    8.5
#    edge_max           3.0 %               4.6 mm               11 mm
#    edge_min           0.6 %               0.9 mm                2.2 mm
#    label_gap          —                   0.08 coord (fixo)     —
#    margin             —                  55 pt × s             —
#
#  Razões fixas (derivadas da medição; não altere):
#    label_size / max_node_size = 0.213
#    edge_max   / max_node_size = 0.275
#    edge_min   / max_node_size = 0.055
# ────────────────────────────────────────────────────────────────────────────
REF_CANVAS <- 14   # canvas de calibração (não altere)

plot_nma_network_custom <- function(
    nma_df,
    ref_treatment,
    ref_color      = "#1B2E6E",
    placebo_color  = "#4A7C59",
    other_color    = "#7B3A70",
    edge_color     = "#A0A0A0",
    label_color    = "#2A2A2A",
    known_acronyms = character(0),
    canvas_in      = CANVAS_IN,    # âncora de escala — ajuste em CANVAS_IN acima
    label_scale    = LABEL_SCALE   # multiplicador de fonte — ajuste em LABEL_SCALE acima
) {
  
  # ── Tamanhos derivados — fonte única de verdade ──────────────────────────
  s             <- canvas_in / REF_CANVAS
  max_node_size <- 40.0 * s    # mm — 11% do canvas (medido no gemtc 490 px)
  min_node_size <-  1.5 * s    # mm — diâmetro mínimo
  label_size    <-  8.5 * s * label_scale  # ggplot units — ajuste via LABEL_SCALE
  edge_max      <- 11.0 * s    # mm — 3.0% canvas (medido no gemtc)
  edge_min      <-  2.2 * s    # mm — 0.6% canvas (medido no gemtc; era 0.7 → 3× maior)
  label_gap     <-  0.08       # coord units — gap entre borda do nó e âncora do label
  # (scale-invariante: node_r_coord e mm_per_coord escalam juntos)
  margin_pt     <-  round(55 * s)    # pt — margem do plot
  
  # mm por unidade de coordenada — necessário para converter node_r (mm) → coord
  coord_range  <- 2.52         # range estimado do painel (nodes ±1 + expansão 25%)
  mm_per_coord <- canvas_in * 25.4 / coord_range
  
  # 1. N total por tratamento
  trt_n <- aggregate(nma_df$sampleSize,
                     by  = list(treatment = nma_df$treatment),
                     FUN = sum)
  names(trt_n)[2] <- "total_n"
  trt_n$treatment <- as.character(trt_n$treatment)
  
  # 2. Número de estudos por par de tratamentos
  pairs_list <- lapply(unique(nma_df$study), function(s_id) {
    trts <- sort(unique(as.character(nma_df$treatment[nma_df$study == s_id])))
    if (length(trts) < 2) return(NULL)
    idx  <- t(combn(seq_along(trts), 2))
    data.frame(t1 = trts[idx[, 1]], t2 = trts[idx[, 2]],
               stringsAsFactors = FALSE)
  })
  all_pairs   <- do.call(rbind, Filter(Negate(is.null), pairs_list))
  edge_counts <- aggregate(rep(1L, nrow(all_pairs)),
                           by  = list(t1 = all_pairs$t1, t2 = all_pairs$t2),
                           FUN = sum)
  names(edge_counts)[3] <- "n_studies"
  edge_counts$t1 <- as.character(edge_counts$t1)
  edge_counts$t2 <- as.character(edge_counts$t2)
  
  # 3. Layout circular — referência posicionada na base (270°)
  trts   <- sort(unique(as.character(nma_df$treatment)))
  n_t    <- length(trts)
  angles <- seq(0, 2 * pi * (1 - 1 / n_t), length.out = n_t)
  
  ref_idx <- which(trts == ref_treatment)
  if (length(ref_idx) > 0) {
    shift  <- (3 * pi / 2) - angles[ref_idx]
    angles <- (angles + shift) %% (2 * pi)
  }
  
  nodes <- data.frame(
    treatment = trts,
    angle     = angles,
    x         = cos(angles),
    y         = sin(angles),
    stringsAsFactors = FALSE
  )
  nodes <- merge(nodes, trt_n, by = "treatment")
  
  # Rótulo: nome com quebra automática de linha + (n = X) na linha seguinte
  nodes$label <- vapply(nodes$treatment, format_trt_label,
                        character(1), known_acronyms = known_acronyms)
  nodes$label_full <- paste0(
    vapply(nodes$label, wrap_label, character(1), width = 14),
    "\n(n = ",
    formatC(nodes$total_n, format = "d", big.mark = ","), ")"
  )
  
  # Cor dos nós
  has_placebo <- any(grepl("^placebo$", trts, ignore.case = TRUE))
  nodes$fill <- dplyr::case_when(
    nodes$treatment == ref_treatment ~
      ref_color,
    has_placebo & grepl("^placebo$", nodes$treatment, ignore.case = TRUE) ~
      placebo_color,
    TRUE ~
      other_color
  )
  
  # ── Posicionamento per-nó: âncora do label sempre além da borda do nó ──────
  # Raio visual de cada nó em coord (scale_size_area: raio ∝ sqrt(n / n_max))
  n_max              <- max(nodes$total_n)
  nodes$node_r_coord <- (max_node_size / 2) *
    sqrt(nodes$total_n / n_max) / mm_per_coord
  
  # Âncora = centro do nó (raio 1) + raio visual + gap em coord
  nodes$label_r <- 1 + nodes$node_r_coord + label_gap
  nodes$lx      <- cos(nodes$angle) * nodes$label_r
  nodes$ly      <- sin(nodes$angle) * nodes$label_r
  nodes$hjust   <- ifelse(cos(nodes$angle) < -0.1, 1,
                          ifelse(cos(nodes$angle) >  0.1, 0, 0.5))
  nodes$vjust   <- ifelse(sin(nodes$angle) >  0.1, 0,
                          ifelse(sin(nodes$angle) < -0.1, 1, 0.5))
  
  # 4. Dados das arestas
  edges <- merge(edge_counts,
                 nodes[, c("treatment", "x", "y")], by.x = "t1", by.y = "treatment")
  names(edges)[4:5] <- c("x1", "y1")
  edges <- merge(edges,
                 nodes[, c("treatment", "x", "y")], by.x = "t2", by.y = "treatment")
  names(edges)[6:7] <- c("x2", "y2")
  
  # 5. Plot
  ggplot() +
    geom_segment(
      data      = edges,
      aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n_studies),
      color     = edge_color,
      alpha     = 0.70,
      lineend   = "round"
    ) +
    geom_point(
      data  = nodes,
      aes(x = x, y = y, size = total_n),
      color = nodes$fill
    ) +
    geom_text(
      data       = nodes,
      aes(x = lx, y = ly, label = label_full),
      hjust      = nodes$hjust,
      vjust      = nodes$vjust,
      size       = label_size,
      fontface   = "plain",      # regular weight — igual à referência NMAStudio
      color      = label_color,
      lineheight = 1.1
    ) +
    scale_size_area(max_size = max_node_size, guide = "none") +
    scale_linewidth_continuous(range = c(edge_min, edge_max), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.25)) +
    scale_y_continuous(expand = expansion(mult = 0.20)) +
    coord_fixed(clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(margin_pt, margin_pt, margin_pt, margin_pt)
    )
}

# Salva ggplot em TIFF + PNG sem limites de dimensão forçados.
# O ggplot expande o canvas para conter todos os elementos; margens brancas
# extras podem ser cortadas depois na edição.
save_fig_gg <- function(gg_plot, base_path, canvas_in = CANVAS_IN) {
  tiff_path <- paste0(base_path, ".tiff")
  
  ggsave(tiff_path, plot = gg_plot,
         width = canvas_in, height = canvas_in, units = "in", dpi = 300,
         device = agg_tiff, compression = "lzw")
  
  tryCatch({
    img <- magick::image_read(tiff_path)
    magick::image_write(img[1],
                        path   = paste0(base_path, ".png"),
                        format = "png")
  }, error = function(e) {
    warning("PNG nao gerado para '", basename(base_path), "': ", e$message)
  })
}

# Função para exponenciar a matriz de texto do gemtc mantendo os ICs (evita erro)
exp_league_table <- function(league_matrix) {
  apply(as.matrix(league_matrix), c(1, 2), function(cell) {
    if (is.na(cell) || cell == "") return(cell)
    # Procura padrões de números com ou sem sinal de menos e decimais
    if (grepl("\\(", cell)) {
      nums <- suppressWarnings(
        as.numeric(unlist(regmatches(cell, gregexpr("-?\\d+\\.\\d+|-?\\d+", cell))))
      )
      nums <- nums[!is.na(nums)]
      if (length(nums) == 3) {
        nums_exp <- round(exp(nums), 2)
        return(sprintf("%.2f (%.2f, %.2f)", nums_exp[1], nums_exp[2], nums_exp[3]))
      }
    }
    return(cell)
  })
}

# Renderiza uma vez em TIFF multi-página (via ragg) e extrai a primeira
# página como PNG (via magick) — evita renderizar o plot duas vezes.
save_fig <- function(plot_fn, base_path, width = 10, height = 7) {
  tiff_path <- paste0(base_path, ".tiff")
  
  agg_tiff(tiff_path,
           width = width, height = height, units = "in", res = 300,
           compression = "lzw")
  tryCatch(plot_fn(), finally = dev.off())
  
  tryCatch({
    img <- magick::image_read(tiff_path)
    magick::image_write(img[1],
                        path   = paste0(base_path, ".png"),
                        format = "png")
  }, error = function(e) {
    warning("PNG nao gerado para '", basename(base_path), "': ", e$message)
  })
}

save_docx <- function(sections, path) {
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

# Detecta componentes desconectados e poda a rede para manter apenas
# o maior componente conectado. Retorna lista com dados limpos e metadados.
fix_disconnected_network <- function(df) {
  
  # Monta grafo de tratamentos via estudos compartilhados
  edges <- do.call(rbind, lapply(unique(df$study), function(s) {
    trts <- unique(df$treatment[df$study == s])
    if (length(trts) < 2) return(NULL)
    t(combn(sort(trts), 2))
  }))
  
  if (is.null(edges)) {
    return(list(data = df, disconnected = FALSE,
                trts_dropped = character(0), studs_dropped = character(0)))
  }
  
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  names(edges) <- c("from", "to")
  
  all_trts <- unique(c(edges$from, edges$to))
  adj      <- matrix(0L, length(all_trts), length(all_trts),
                     dimnames = list(all_trts, all_trts))
  for (i in seq_len(nrow(edges))) {
    adj[edges$from[i], edges$to[i]] <- 1L
    adj[edges$to[i], edges$from[i]] <- 1L
  }
  
  # BFS para encontrar todos os componentes
  visited    <- setNames(rep(FALSE, length(all_trts)), all_trts)
  components <- list()
  
  for (start in all_trts) {
    if (visited[start]) next
    queue <- start
    comp  <- character(0)
    while (length(queue) > 0) {
      node     <- queue[1]
      queue    <- queue[-1]
      if (visited[node]) next
      visited[node] <- TRUE
      comp  <- c(comp, node)
      nbrs  <- all_trts[adj[node, ] == 1L]
      queue <- c(queue, nbrs[!visited[nbrs]])
    }
    components <- c(components, list(comp))
  }
  
  if (length(components) == 1L) {
    return(list(data = df, disconnected = FALSE,
                trts_dropped = character(0), studs_dropped = character(0)))
  }
  
  # Mantém o maior componente
  sizes      <- vapply(components, length, integer(1))
  keep_trts  <- components[[which.max(sizes)]]
  drop_trts  <- setdiff(all_trts, keep_trts)
  
  df_clean   <- df[df$treatment %in% keep_trts, ]
  
  # Remove estudos que ficaram com menos de 2 braços
  arms_per_study <- tapply(df_clean$treatment, df_clean$study,
                           function(x) length(unique(x)))
  drop_studs     <- names(arms_per_study[arms_per_study < 2])
  df_clean       <- df_clean[!df_clean$study %in% drop_studs, ]
  
  list(
    data          = df_clean,
    disconnected  = TRUE,
    trts_dropped  = drop_trts,
    studs_dropped = drop_studs
  )
}

# ── Loop principal ──────────────────────────────────────────

erros  <- list()
avisos <- list()

log_dir <- file.path(BASE_DIR, "_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

for (sheet in sheet_list) {
  
  cat("\n======================================================\n")
  cat(">>> Processando desfecho:", sheet, "...\n")
  cat("======================================================\n")
  
  sucesso <- tryCatch({
    
    out_dir <- file.path(BASE_DIR, safe_name(sheet))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    nma_df <- as.data.frame(read_sheet(ss, sheet = sheet))
    
    # --- 0. Ajuste de Rede Desconectada -----------------------------------
    net_check <- fix_disconnected_network(nma_df)
    nma_df    <- net_check$data
    
    if (net_check$disconnected) {
      msg <- paste0(
        "Rede podada para manter o maior componente.\n",
        "  -> Tratamentos removidos: ",
        paste(net_check$trts_dropped, collapse = ", "), "\n",
        "  -> Estudos removidos (<2 bracos restantes): ",
        ifelse(
          length(net_check$studs_dropped) > 0,
          paste(net_check$studs_dropped, collapse = ", "),
          "Nenhum"
        )
      )
      avisos[[sheet]] <- msg
      message("[AVISO] ", sheet, ": ", msg)
    }
    
    data_type <- detect_type(nma_df)
    direction <- detect_direction(sheet)
    
    # --- 0b. Referência do modelo Bayesiano ----------------------------------------
    # relevel() posiciona RANK_REFERENCE como nível base do fator treatment.
    # mtc.network() herda essa ordem e o JAGS parametriza d_ref = 0 — todos os
    # efeitos relativos são calculados em função desse braço.
    # O braço de referência continua na análise e recebe rank/SUCRA normalmente;
    # apenas serve de âncora para os demais (não é excluído nem zerado).
    if (nchar(trimws(RANK_REFERENCE)) > 0 &&
        RANK_REFERENCE %in% unique(as.character(nma_df$treatment))) {
      nma_df$treatment <- relevel(
        factor(as.character(nma_df$treatment)),
        ref = RANK_REFERENCE
      )
      message("[INFO] Referencia de ranking definida: '", RANK_REFERENCE, "'")
    }
    
    # --- 1. Rede ----------------------------------------------------------
    nma_net <- mtc.network(data.ab = nma_df,
                           description = paste("NMA -", sheet))
    
    net_gg <- plot_nma_network_custom(
      nma_df         = nma_df,
      ref_treatment  = REF_TREATMENT,
      ref_color      = REF_COLOR,
      placebo_color  = PLACEBO_COLOR,
      other_color    = OTHER_COLOR,
      edge_color     = EDGE_COLOR,
      label_color    = LABEL_COLOR,
      known_acronyms = KNOWN_ACRONYMS
    )
    save_fig_gg(net_gg, file.path(out_dir, "network_plot"))
    
    # --- 2. Modelo de consistência ----------------------------------------
    if (data_type == "continuous") {
      model_nma <- mtc.model(
        nma_net,
        type        = "consistency",
        n.chain     = 4,
        linearModel = "random",
        likelihood  = "normal",
        link        = "identity"
      )
    } else {
      model_nma <- mtc.model(
        nma_net,
        type        = "consistency",
        n.chain     = 4,
        linearModel = "random",
        likelihood  = "binom",
        link        = "log"
      )
    }
    
    # --- 3. MCMC ----------------------------------------------------------
    set.seed(42)
    mcmc_nma <- mtc.run(model_nma,
                        n.adapt = 200000, n.iter = 1000000, thin = 50)
    
    mcmc_sum <- summary(mcmc_nma)
    gelman_d <- gelman.diag(mcmc_nma)
    
    save_fig(function() plot(mcmc_nma),
             file.path(out_dir, "trace_plot"),
             width = 14, height = 10)
    
    save_docx(
      list(
        list(heading = paste("MCMC Summary -", sheet), content = mcmc_sum),
        list(heading = "Gelman-Rubin Diagnostic",      content = gelman_d)
      ),
      file.path(out_dir, "mcmc_summary.docx")
    )
    
    # --- 4. Gelman Plot ---------------------------------------------------
    save_fig(function() gelman.plot(mcmc_nma),
             file.path(out_dir, "gelman_plot"))
    
    # --- 5. League Table → xlsx ------------------------------------------
    raw_league <- relative.effect.table(mcmc_nma)
    
    if (data_type == "binary") {
      league_tbl <- exp_league_table(as.data.frame(raw_league))
    } else {
      league_tbl <- as.data.frame(raw_league)
    }
    
    write.xlsx(
      as.data.frame(league_tbl),
      file.path(out_dir, "league_table.xlsx"),
      rowNames = TRUE
    )
    
    # --- 6. Rank Probability & SUCRA → docx ------------------------------
    rank_prob <- rank.probability(mcmc_nma, preferredDirection = direction)
    
    # Trava de segurança: Se existe Placebo na rede, forçamos o pacote dmetar 
    # a respeitar a polaridade da aba (is_lower_better = TRUE para abas negativas).
    has_placebo <- "Placebo" %in% unique(as.character(nma_df$treatment))
    
    if (has_placebo) {
      is_lower_better <- (direction == -1L)
      sucra_res <- sucra(rank_prob, lower.is.better = is_lower_better)
    } else {
      # Comportamento padrão 
      sucra_res <- sucra(rank_prob, lower.is.better = FALSE)
    }
    
    save_docx(
      list(
        list(heading = "Rank Probability", content = rank_prob),
        list(heading = "SUCRA",            content = sucra_res)
      ),
      file.path(out_dir, "rank_sucra.docx")
    )
    
    # --- 7. Node Splitting -----------------------------------------------
    tryCatch({
      
      if (data_type == "continuous") {
        ns <- mtc.nodesplit(
          nma_net,
          linearModel = "random",
          likelihood  = "normal",
          link        = "identity",
          n.adapt     = 200000,
          n.iter      = 1000000,
          thin        = 50
        )
      } else {
        ns <- mtc.nodesplit(
          nma_net,
          linearModel = "random",
          likelihood  = "binom",
          link        = "log",
          n.adapt     = 200000,
          n.iter      = 1000000,
          thin        = 50
        )
      }
      
      ns_sum <- summary(ns)
      
      save_fig(function() plot(ns_sum),
               file.path(out_dir, "nodesplit_plot"))
      
      save_docx(
        list(list(heading = "Node Splitting Summary", content = ns_sum)),
        file.path(out_dir, "nodesplit.docx")
      )
      
    }, error = function(e) {
      
      # Node splitting falha quando não há comparações diretas E indiretas
      # simultaneamente. Fallback: netmeta + decomp.design.
      
      # Força o Placebo como referência para evitar florest plots invertidos
      has_placebo <- "Placebo" %in% unique(as.character(nma_df$treatment))
      ref <- if(has_placebo) "Placebo" else sort(unique(nma_df$treatment))[1]
      
      if (data_type == "binary") {
        pw <- pairwise(
          event   = responders,
          n       = sampleSize,
          treat   = treatment,
          studlab = study,
          data    = nma_df,
          sm      = "RR"
        )
        net_freq <- netmeta(
          TE, seTE, treat1, treat2, studlab,
          data            = pw,
          sm              = "RR",
          random          = TRUE,
          common          = FALSE,
          reference.group = ref,
          method.tau      = "REML"
        )
      } else {
        pw <- pairwise(
          treat   = treatment,
          mean    = mean,
          sd      = `std.dev`,
          n       = sampleSize,
          studlab = study,
          data    = nma_df,
          sm      = "MD"
        )
        net_freq <- netmeta(
          TE, seTE, treat1, treat2, studlab,
          data            = pw,
          sm              = "MD",
          random          = TRUE,
          common          = FALSE,
          reference.group = ref,
          method.tau      = "REML"
        )
      }
      
      qd <- decomp.design(net_freq)
      
      save_docx(
        list(
          list(
            heading = "Inconsistencia - decomp.design (fallback node splitting)",
            content = qd
          )
        ),
        file.path(out_dir, "nodesplit_fallback_inconsistency.docx")
      )
      
    })
    
    TRUE
    
  }, error = function(e) {
    
    call_info <- if (!is.null(e$call)) deparse(e$call)[1] else "desconhecida"
    
    detalhe_erro <- paste0(
      "Desfecho : ", sheet, "\n",
      "Mensagem : ", e$message, "\n",
      "Funcao   : ", call_info, "\n",
      "Data/hora: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    erros[[sheet]] <<- detalhe_erro
    
    log_file <- file.path(log_dir, paste0("ERRO_", safe_name(sheet), ".txt"))
    writeLines(detalhe_erro, log_file)
    
    message("\n[!] ERRO CRITICO em '", sheet, "'. Pulando para o proximo desfecho.")
    message("Motivo: ", e$message, "\n")
    
    FALSE
  })
  
  if (!sucesso) next
}

# ── Relatório final do pipeline ─────────────────────────────

n_total  <- length(sheet_list)
n_erros  <- length(erros)
n_avisos <- length(avisos)
n_ok     <- n_total - n_erros

cat("\n")
cat("╔══════════════════════════════════════════════════╗\n")
cat("║              RESUMO DO PIPELINE                  ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat(sprintf("║  Total de desfechos  : %-26d║\n", n_total))
cat(sprintf("║  Concluidos com exito: %-26d║\n", n_ok))
cat(sprintf("║  Com aviso (rede)    : %-26d║\n", n_avisos))
cat(sprintf("║  Com ERRO (pulados)  : %-26d║\n", n_erros))
cat("╚══════════════════════════════════════════════════╝\n")

if (n_avisos > 0) {
  cat("\n--- AVISOS (redes podadas) ---\n")
  for (nm in names(avisos)) {
    cat("\n [", nm, "]\n", avisos[[nm]], "\n")
  }
}

if (n_erros > 0) {
  cat("\n--- ERROS CRITICOS ---\n")
  for (nm in names(erros)) {
    cat("\n [", nm, "]\n", erros[[nm]], "\n")
  }
  cat("\nLogs individuais salvos em:", log_dir, "\n")
} else {
  cat("\nTodos os desfechos foram processados sem erros.\n")
}