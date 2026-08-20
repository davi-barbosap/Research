# ============================================================
# Meta-análise SMC (Cohen's dz -> Hedges' g)
# Leitura: googlesheets4 | Gráficos: ragg (PNG + TIFF, 300 dpi)
# Saída: /Users/davibarbosa/Desktop/JS
# ============================================================

# install.packages(c("meta","googlesheets4","ragg"))
library(meta)
library(googlesheets4)
library(ragg)

out_dir <- "/Users/davibarbosa/Desktop/JS"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ss <- "https://docs.google.com/spreadsheets/d/15YPO_pBP2FIewxXsAM-jBEwCzZtsI3sb9tq6pXm8rcY/edit"
gs4_deauth()

#
run_meta <- function(ss, aba) {
  df <- read_sheet(ss, sheet = aba, col_types = "cnnn")   # força numérico
  
  df$n.e    <- as.numeric(df$n.e)
  df$mean.e <- as.numeric(df$mean.e)
  df$sd.e   <- as.numeric(df$sd.e)
  df <- df[!is.na(df$n.e) & !is.na(df$mean.e) & !is.na(df$sd.e), ]
  
  df$SMC   <- df$mean.e / df$sd.e
  df$J     <- 1 - (3 / (4 * (df$n.e - 1) - 1))          # Hedges (df = n-1)
  df$SMC_g <- df$SMC * df$J
  df$vi    <- 1/df$n.e + (df$SMC_g^2) / (2 * df$n.e)
  df$seTE  <- sqrt(df$vi)
  
  cat("\n== Dados lidos:", aba, "==\n")
  print(df[order(-abs(df$SMC_g)), c("Authors","n.e","mean.e","sd.e","SMC_g")])
  
  ruim <- which(abs(df$SMC_g) > 10)                      # guarda anti-data
  if (length(ruim) > 0)
    stop("Valor implausível em '", aba, "': ", paste(df$Authors[ruim], collapse=", "),
         ". Provável célula formatada como DATA no Sheets — verifique mean.e/sd.e.")
  
  fit <- function(mt) metagen(TE=SMC_g, seTE=seTE, data=df, studlab=Authors, sm="SMC",
                              random=TRUE, common=FALSE, method.tau=mt, prediction=TRUE, control=list(tau2.max=1e6))
  m <- tryCatch(fit("PM"),
                error=function(e){ message("PM falhou (", conditionMessage(e), ") -> REML"); fit("REML") })
  list(m = m, df = df)
}

#
salvar <- function(draw, base, largura = 11, altura = 4.8, dpi = 300) {
  p1 <- file.path(out_dir, paste0(base, ".png")); p2 <- file.path(out_dir, paste0(base, ".tiff"))
  agg_png(p1,  width=largura, height=altura, units="in", res=dpi, background="white"); draw(); dev.off()
  agg_tiff(p2, width=largura, height=altura, units="in", res=dpi, background="white", compression="lzw"); draw(); dev.off()
  message("Salvo: ", p1, "  |  ", p2)
}

# 
save_forest <- function(m, base) {
  salvar(function() forest(m,
                           leftcols  = c("studlab","mean.e","sd.e","n.e","w.random"),
                           leftlabs  = c("Study","Mean change","SD change","N","Weight"),
                           rightcols = c("effect","ci"), rightlabs = c("SMC","95% CI"),
                           colgap = "4mm", pooled.totals = TRUE, digits = 2,
                           col.square = "darkblue", col.square.lines = "darkblue",
                           col.diamond = "black", col.diamond.lines = "black",
                           col.predict = "red"), base)
}

# 
save_loo <- function(m, base) {
  salvar(function() forest(metainf(m, pooled = "random"), prediction = FALSE,
                           rightcols = c("effect","ci"), rightlabs = c("SMC","95% CI"), digits = 2,
                           col.square = "darkblue", col.bg = "darkblue", col.border = "darkblue",
                           col.diamond = "black", col.diamond.lines = "black"), base, altura = 5.0)
}

#
dep <- run_meta(ss, "Depression after Bariatric")
anx <- run_meta(ss, "Anxiety after Bariatric")

resumo <- function(x, nome) {
  m <- x$m
  cat(sprintf("\n=== %s (%d estudos) ===\n", nome, m$k))
  cat(sprintf("g = %.3f [%.3f; %.3f] | PI [%.3f; %.3f] | I2 = %.1f%% | tau2 = %.3f | Q p = %.4f\n",
              m$TE.random, m$lower.random, m$upper.random,
              m$lower.predict, m$upper.predict, m$I2*100, m$tau2, m$pval.Q))
}
resumo(dep, "Depressão"); resumo(anx, "Ansiedade")

#
save_forest(dep$m, "forest_depressao")
save_forest(anx$m, "forest_ansiedade")
save_loo(dep$m,   "loo_depressao")
save_loo(anx$m,   "loo_ansiedade")
