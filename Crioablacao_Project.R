# ==============================================================================
# SINGLE PROPORTIONS META-ANALYSIS SCRIPT
# ==============================================================================

# 001. INSTALL AND LOAD PACKAGES
# Verifica se o pacman está instalado; se não, instala
if (!require("pacman")) install.packages("pacman")

# Instala (se necessário) e carrega os pacotes do CRAN
pacman::p_load(googlesheets4, ragg, meta, devtools)

# Instala (se necessário) e carrega o pacote do GitHub
pacman::p_load_gh("MathiasHarrer/dmetar")

# 002. IMPORT DATAFRAME FROM GOOGLE SHEETS
# gs4_deauth() # Descomente se a planilha for pública (não pedirá login)
gs_url <- "https://docs.google.com/spreadsheets/d/1hmYzg0fDSPuRdiZ76aiPiPXKy5SQ72cLUr8yRSgURHU/edit?gid=56324032#gid=56324032"

sheetsnames <- sheet_names(gs_url)
print(sheetsnames)

ma <- lapply(sheetsnames, function(x) {
  as.data.frame(read_sheet(gs_url, sheet = x))
})
names(ma) <- sheetsnames


# 004. SINGLE PROPORTION META-ANALYSIS (GLMM & PLOGIT)
meta <- metaprop(event, n,
                 studlab = study,
                 data = data,
                 sm = "PLOGIT",
                 method = "GLMM",
                 method.tau = "ML",
                 random = TRUE,
                 common = FALSE)

agg_png(filename = paste0(output, "Forestplot_", outcome, ".png"),
        width = 3000, height = 2500, res = 300)
forest(meta,
       layout = "Revman5",
       leftcols = c("studlab", "event", "n", "w.random", "effect", "ci"),
       leftlabs = c("Studies", NA, NA, NA, "Prevalence", NA),
       colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
       colgap.forest.left = "6mm", just = "center", col.square = "darkblue", 
       col.square.lines = "black", sortvar = TE)
invisible(dev.off())

# 005. SUBGROUP ANALYSIS
# ATENÇÃO: Substitua NOME_DA_COLUNA pela variável categórica do seu banco
subgroup_obj1 <- metaprop(event, n,
                         studlab = study,
                         data = data,
                         sm = "PLOGIT",
                         method = "GLMM",
                         method.tau = "ML",
                         random = TRUE,
                         common = FALSE,
                         prediction = TRUE,
                         subgroup = subgroup1) 

agg_png(filename = paste0(output, "Subgroup_", outcome, ".png"),
        width = 3000, height = 2500, res = 300)
forest(subgroup_obj1,
       layout = "Revman5",
       leftcols = c("studlab", "event", "n", "w.random", "effect", "ci"),
       leftlabs = c("Studies", NA, NA, NA, "Prevalence", NA),
       colgap = "3mm", pscale = 100, pooled.events = TRUE, pooled.totals = TRUE,
       colgap.forest.left = "6mm", just = "center", col.square = "darkblue", 
       col.square.lines = "black", sortvar = TE, print.subgroup.name = FALSE,
       overall = FALSE, overall.hetstat = FALSE, test.overall.random = FALSE)
invisible(dev.off())

# 006. LEAVE-ONE-OUT ANALYSIS
meta_loo <- metainf(meta, pooled = "random")

agg_png(filename = paste0(output, "Leave-one-out_", outcome, ".png"),
        width = 3000, height = 2500, res = 300)
forest(meta_loo,
       col.bg = "lightblue", col.diamond = "gray", pscale = 100, ff.xlab = "bold",
       rightcols = c("effect", "ci", "I2"), colgap.right = "6mm", just = "center")
invisible(dev.off())

# 007. SMALL STUDY EFFECT & BIAS TEST
agg_png(filename = paste0(output, "FunnelPlot_", outcome, ".png"),
        width = 3000, height = 2500, res = 300)
funnel(meta,
       studlab = TRUE, bg = "red", cex = 2, cex.studlab = 1,
       random = TRUE, common = FALSE, backtransf = FALSE)
invisible(dev.off())

# Teste de Peters adequado para prevalências
metabias(meta, method.bias = "peters", plotit = TRUE)

# 1. Aplica o Trim and Fill no objeto original da sua meta-análise
meta_tf <- trimfill(meta)

# 2. Veja o sumário para comparar a prevalência ajustada com a original
summary(meta_tf)

# 3. Gere o novo Funnel Plot (os estudos fantasmas aparecerão como círculos vazios)
agg_png(filename = paste0(output, "FunnelPlot_TrimFill_", outcome, ".png"),
        width = 3000, height = 2500, res = 300)
funnel(meta_tf,
       studlab = TRUE, 
       bg = "red", 
       cex = 2,
       cex.studlab = 1,
       random = TRUE,
       common = FALSE,
       backtransf = FALSE)
invisible(dev.off())

# 008. META-REGRESSION
# ATENÇÃO: Substitua as variáveis após o "~" pelas colunas corretas do seu banco
# 008. META-REGRESSION (Multivariável com 5 Preditores)
# 1. Testando apenas o Tamanho do Tumor
mreg_tumor <- metareg(meta, ~ tumor_size_mm)
summary(mreg_tumor)

# 2. Testando apenas a Proporção Luminal
mreg_luminal <- metareg(meta, ~ luminal_prop)
summary(mreg_luminal)

# 3. Testando apenas o Tempo de Seguimento
mreg_followup <- metareg(meta, ~ follow_up_mo)
summary(mreg_followup)

# 4. Testando apenas a Proporção de Radioterapia
mreg_rt <- metareg(meta, ~ rt_prop)
summary(mreg_rt)

# 5. Testando apenas a Proporção de Terapia Endócrina
mreg_endo <- metareg(meta, ~ endo_prop)
summary(mreg_endo)
