# ============================================================
# Single Mean Meta-Analysis — versão otimizada
# googlesheets4 + ragg + meta/dmetar
# Fluxo: (1) análise primária, (2) subgroup1, (3) subgroup2
# Saídas por outcome: Forest, LOO, Funnel, Egger (>=10 estudos)
# ============================================================

# ---- 000) Pacotes ------------------------------------------------------------
# install.packages(c("googlesheets4", "ragg", "meta", "rstudioapi"))
# pak::pak("MathiasHarrer/dmetar")   # macOS
library(googlesheets4)
library(ragg)
library(meta)
library(dmetar)
library(rstudioapi)

# ---- 001) Conexão com o Google Sheets ----------------------------------------
# Planilha pública -> gs4_deauth(); planilha privada -> gs4_auth() (abre o navegador).
gs4_deauth()   # troque por gs4_auth() se a planilha for privada

# Cole a URL (ou o ID) da planilha "stats_bmi_robotics_30":
ss <- "https://docs.google.com/spreadsheets/d/1ZkWa7hBP5R7DtiYSTNERs0XCJpXQbJIkK4RYvqNMXPI/edit?gid=0#gid=0"

# Confira os nomes das abas (outcomes):
print(sheet_names(ss))
# Ex.: sugery_duration, ebl, transfusion, los, conversion,
#      intraop_complications, postop_complications, infectious_comp

# Pasta de saída das figuras:
output <- selectDirectory()

# ---- 002) Função única do fluxo ----------------------------------------------
# sm: "MRAW" (médias simétricas) ou "MLN" (positivas/assimétricas — ex.: tempo, EBL).
# subgroup: nome da coluna de subgrupo como string (NULL = análise primária).
run_meta <- function(data, outcome, output,
                     sm = "MRAW", subgroup = NULL) {
  
  tag <- if (is.null(subgroup)) outcome else paste0(outcome, "_", subgroup)
  
  m <- metamean(
    n = n, mean = mean, sd = sd,
    data      = data,
    studlab   = study,
    sm        = sm,
    method.tau = "REML",
    random = TRUE, common = FALSE, prediction = TRUE,
    subgroup = if (is.null(subgroup)) NULL else data[[subgroup]]
  )
  
  # ---- Altura dinâmica: proporcional ao nº de linhas (estudos + subgrupos) ----
  n_lvl  <- if (is.null(subgroup)) 0 else length(unique(na.omit(data[[subgroup]])))
  n_rows <- m$k + n_lvl * 3           # cada subgrupo acrescenta cabeçalho/subtotal
  h_px   <- round((3.2 + n_rows * 0.30) * 300)   # ~0.30 in por linha, +margem
  
  # ---- Forest -----------------------------------------------------------------
  agg_png(file.path(output, paste0("Forest_", tag, ".png")),
          width = 3200, height = h_px, res = 300)
  forest(m,
         layout   = "RevMan5",
         leftcols = c("studlab", "mean", "sd", "n", "w.random", "effect", "ci"),
         leftlabs = c("Studies", NA, NA, NA, NA, "Mean", NA),
         colgap = "3mm", colgap.forest.left = "6mm",
         digits = 2, digits.sd = 2,
         pooled.totals = TRUE, just = "center",
         col.square = "darkblue", col.square.lines = "black",
         sortvar = TE,
         squaresize = 1.2, fontsize = 11, spacing = 1.1, plotwidth = "7cm",
         print.subgroup.name = FALSE,
         overall             = TRUE,
         overall.hetstat     = TRUE,
         test.overall.random = TRUE)
  dev.off()
  
  # LOO, Funnel e Egger só na análise primária (não fazem sentido por subgrupo)
  if (is.null(subgroup)) {
    
    # ---- Leave-one-out --------------------------------------------------------
    m_loo <- metainf(m, pooled = "random")
    agg_png(file.path(output, paste0("LOO_", tag, ".png")),
            width = 3000, height = 2500, res = 300)
    forest(m_loo,
           col.bg = "lightblue", col.diamond = "gray",
           rightcols = c("effect", "ci", "I2"),
           colgap.right = "6mm", just = "center")
    dev.off()
    
    # ---- Funnel ---------------------------------------------------------------
    agg_png(file.path(output, paste0("Funnel_", tag, ".png")),
            width = 3000, height = 2500, res = 300)
    funnel(m, studlab = TRUE, bg = "red", cex = 2, cex.studlab = 1,
           random = TRUE, common = FALSE, backtransf = FALSE)
    dev.off()
    
    # ---- Egger (só com >= 10 estudos) -----------------------------------------
    if (m$k >= 10) {
      cat("\n== Egger —", outcome, "==\n")
      print(metabias(m, method.bias = "Egger", plotit = FALSE))
    } else {
      message("Egger não realizado (", m$k, " estudos < 10) — ", outcome)
    }
  }
  
  invisible(m)
}

# ---- 003) Rodar um outcome ---------------------------------------------------
# ALWAYS RETURN HERE ao trocar de aba/endpoint:
outcome <- "Surgery Duration"          # nome que aparece nos títulos/arquivos
data    <- read_sheet(ss, sheet = "sugery_duration")   # aba do outcome

# (1) Análise primária
meta_primary <- run_meta(data, outcome, output, sm = "MRAW")

# (2) Subgrupo 1  (coluna subgroup1: <30 / >=30)
run_meta(data, outcome, output, sm = "MRAW", subgroup = "subgroup1")

# (3) Subgrupo 2  (coluna subgroup2: Malignant / Benign / Mixed)
run_meta(data, outcome, output, sm = "MRAW", subgroup = "subgroup2")

# Para outro endpoint: mude 'outcome' e a 'sheet' em (003) e rode de novo.
