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
  gemtc,          # Bayesian NMA (mtc.*)
  rjags,          # Backend JAGS para gemtc
  coda,           # Diagnósticos MCMC
  dmetar,         # sucra()
  netmeta,        # NMA frequentista + pairwise + decomp.design
  googlesheets4,  # Leitura do Google Sheets
  openxlsx,       # Escrita de .xlsx
  ragg,           # Dispositivos gráficos de alta qualidade (TIFF)
  magick,         # Conversão TIFF → PNG (primeira página)
  officer,        # Escrita de .docx
  igraph          # Analise de componentes conectados da rede
)

# ==============================================================
#  CONFIGURAÇÃO — edite apenas este bloco
# ==============================================================

SHEETS_URL <- "https://docs.google.com/spreadsheets/d/1tCpDLIzj5LADAISBPm5lQ5QFqID6C2ez52oKYBtrzps/edit?gid=862497479#gid=862497479"
BASE_DIR   <- "/Users/davibarbosa/Desktop/Servidoni I"

# ==============================================================
#  Autenticação Google (abre browser na primeira execução)
# ==============================================================

gs4_auth()
ss         <- as_sheets_id(SHEETS_URL)
sheet_list <- c("Leiomyoma volume change 3mo (-)", "Leiomyoma volume change 6mo (-)", "Leiomyoma volume change 6mo Sub1 (-)", "Leiomyoma volume change 6mo Sub2 (-)", "Uterine volume change 3mo (-)", "Uterine volume change 6mo (-)",
                "Uterine volume change 6mo Sub1 (-)", "Uterine volume change 6mo Sub2 (-)")
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

# Função para exponenciar a matriz de texto do gemtc (evita erro de log/exp em strings)
exp_league_table <- function(league_matrix) {
  apply(league_matrix, c(1, 2), function(cell) {
    if (is.na(cell) || cell == "") return(cell)
    if (grepl("\\(", cell)) {
      nums <- as.numeric(unlist(regmatches(cell, gregexpr("-?\\d+\\.\\d+|-?\\d+", cell))))
      if (length(nums) == 3) {
        nums_exp <- round(exp(nums), 2)
        return(sprintf("%.2f (%.2f, %.2f)", nums_exp[1], nums_exp[2], nums_exp[3]))
      }
    }
    return(cell)
  })
}

# NOVA FUNÇÃO: Identifica e apara redes desconectadas
fix_disconnected_network <- function(df) {
  # Cria lista de todas as comparacoes diretas em cada estudo
  edges_list <- lapply(split(df$treatment, df$study), function(t) {
    t_uni <- unique(t)
    if (length(t_uni) >= 2) {
      cb <- combn(t_uni, 2)
      return(data.frame(from = cb[1, ], to = cb[2, ]))
    }
    return(NULL)
  })
  
  edges <- do.call(rbind, edges_list)
  if (is.null(edges) || nrow(edges) == 0) {
    return(list(data = df, disconnected = FALSE, trts_dropped = NULL, studs_dropped = NULL))
  }
  
  # Cria o grafo e busca componentes conectados
  g <- graph_from_data_frame(edges, directed = FALSE)
  comp <- components(g)
  
  # Se houver mais de 1 componente, a rede esta desconectada
  if (comp$no > 1) {
    # Identifica o maior componente (rede principal)
    largest_id <- which.max(comp$csize)
    trts_to_keep <- names(comp$membership[comp$membership == largest_id])
    
    trts_dropped <- setdiff(unique(df$treatment), trts_to_keep)
    
    # Filtra os tratamentos da rede
    df_clean <- df[df$treatment %in% trts_to_keep, ]
    
    # Verifica se algum estudo ficou com apenas 1 braco apos o corte
    study_counts <- table(df_clean$study)
    valid_studs <- names(study_counts[study_counts >= 2])
    studs_dropped <- setdiff(unique(df$study), valid_studs)
    
    df_clean <- df_clean[df_clean$study %in% valid_studs, ]
    
    return(list(data = df_clean, disconnected = TRUE, 
                trts_dropped = trts_dropped, studs_dropped = studs_dropped))
  }
  
  return(list(data = df, disconnected = FALSE, trts_dropped = NULL, studs_dropped = NULL))
}

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

# ── Loop principal ──────────────────────────────────────────

erros  <- list()
avisos <- list() 

for (sheet in sheet_list) { tryCatch({
  
  out_dir <- file.path(BASE_DIR, safe_name(sheet))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  nma_df    <- as.data.frame(read_sheet(ss, sheet = sheet))
  
  # --- 0. Ajuste de Rede Desconectada -------------------------
  net_check <- fix_disconnected_network(nma_df)
  nma_df    <- net_check$data
  
  if (net_check$disconnected) {
    msg <- paste0("Rede podada para manter o maior componente.\n",
                  "  -> Tratamentos removidos: ", paste(net_check$trts_dropped, collapse = ", "), "\n",
                  "  -> Estudos removidos (<2 bracos restantes): ", 
                  ifelse(length(net_check$studs_dropped) > 0, 
                         paste(net_check$studs_dropped, collapse = ", "), "Nenhum"))
    avisos[[sheet]] <- msg
  }
  
  data_type <- detect_type(nma_df)
  direction <- detect_direction(sheet)
  
  # ============================================================
  # PASSO A (CONDICIONAL): Verifica se há Placebo na rede limpa
  # ============================================================
  has_placebo <- "Placebo" %in% unique(nma_df$treatment)
  
  if (has_placebo) {
    nma_df$treatment <- as.factor(nma_df$treatment)
    nma_df$treatment <- relevel(nma_df$treatment, ref = "Placebo")
  }
  # ============================================================
  
  # --- 1. Rede ------------------------------------------------
  nma_net <- mtc.network(data.ab = nma_df,
                         description = paste("NMA -", sheet))
  
  save_fig(function() plot(nma_net),
           file.path(out_dir, "network_plot"),
           width = 8, height = 8)
  
  # --- 2. Modelo de consistência ------------------------------
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
  
  # --- 3. MCMC ------------------------------------------------
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
  
  # --- 4. Gelman Plot -----------------------------------------
  save_fig(function() gelman.plot(mcmc_nma),
           file.path(out_dir, "gelman_plot"))
  
  # --- 5. League Table → xlsx ---------------------------------
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
  
  # --- 6. Rank Probability & SUCRA → docx --------------------
  rank_prob <- rank.probability(mcmc_nma, preferredDirection = direction)
  
  # ============================================================
  # PASSO B (CONDICIONAL): SUCRA com override se houver Placebo
  # ============================================================
  if (has_placebo) {
    # Converte o -1/+1 do sheet para TRUE/FALSE e calcula
    is_lower_better <- (direction == -1L)
    sucra_res <- sucra(rank_prob, lower.is.better = is_lower_better)
  } else {
    # Comportamento padrão caso não haja placebo
    sucra_res <- sucra(rank_prob)
  }
  # ============================================================
  
  save_docx(
    list(
      list(heading = "Rank Probability", content = rank_prob),
      list(heading = "SUCRA",            content = sucra_res)
    ),
    file.path(out_dir, "rank_sucra.docx")
  )
  
  # --- 7. Node Splitting --------------------------------------
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
    
    # ============================================================
    # BÔNUS: Ajuste do referencial no netmeta usando a flag
    # ============================================================
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
  
}, error = function(e) {
  erros[[sheet]] <<- conditionMessage(e)
})}

# ── Relatório final de erros e avisos ───────────────────────

if (length(erros) == 0 && length(avisos) == 0) {
  message("Todos os desfechos concluidos sem erros ou modificacoes de rede.")
} else {
  if (length(avisos) > 0) {
    message("\n=============================================")
    message("--- AVISOS: REDES DESCONECTADAS AJUSTADAS ---")
    message("=============================================\n")
    for (nm in names(avisos)) {
      message("[", nm, "]\n", avisos[[nm]], "\n")
    }
  }
  
  if (length(erros) > 0) {
    message("\n=============================================")
    message("--------- RELATORIO DE ERROS CRITICOS ---------")
    message("=============================================\n")
    message(length(erros), " desfecho(s) com falha:\n")
    for (nm in names(erros)) {
      message("[", nm, "]\n", erros[[nm]], "\n")
    }
  }
}