# ==============================================================================
# 1. PREPARAÇÃO DO AMBIENTE E IMPORTAÇÃO
# ==============================================================================
library(meta)
library(dmetar)
library(rstudioapi)
library(readxl)

STATS <- selectFile(caption = "/Users/davibarbosa/Downloads/STATS - Laura.xlsx")

excel_sheets(STATS)
# ==============================================================================
# ANÁLISE 1: GESTATIONAL AGE AT BIRTH (WEEKS)
# ==============================================================================
df_gest_age <- as.data.frame(read_excel(STATS, sheet = "Gestational age at birth (weeks"))

meta_gest_age <- metamean(n = n,
                          mean = mean,
                          sd = sd,
                          data = df_gest_age,
                          sm = "MRAW",
                          studlab = study, 
                          random = TRUE,
                          common = FALSE,
                          method.tau = "REML",
                          method.random.ci = TRUE,           
                          prediction = TRUE)

meta::forest(meta_gest_age,
             layout = "Revman5",
             leftcols = c("studlab", "mean", "sd", "n", "w.random", "effect", "ci"),
             leftlabs = c("Studies", "Mean", "SD", "Total", "Weight", "Mean", "95% CI"),
             colgap = "6mm",
             just = "center",
             col.square = "darkblue", 
             col.square.lines = "black",
             col.diamond = "black",
             col.predict = "red",
             overall = TRUE,
             overall.hetstat = TRUE,
             test.overall.random = TRUE)


# ==============================================================================
# ANÁLISE 2: NEONATAL DEATH (GLMM - Eventos/100 obs)
# ==============================================================================
df_neo_death <- as.data.frame(read_excel(STATS, sheet = "Neonatal death"))

meta_neo_death <- metaprop(event = events,
                           n = n,
                           data = df_neo_death,
                           studlab = study,
                           method = "GLMM",
                           sm = "PLOGIT",
                           random = TRUE,
                           common = FALSE,
                           prediction = TRUE, 
                           pscale = 100) # Transforma em prevalência % (por 100 obs)

meta::forest(meta_neo_death,
             layout = "Revman5",
             leftcols = c("studlab", "event", "n", "effect", "ci"),
             leftlabs = c("Studies", "Events", "Total", "Prevalence", "95% CI"),
             colgap = "6mm",
             just = "center",
             col.square = "darkblue",
             col.square.lines = "black",
             col.diamond = "black",
             col.predict = "red",
             overall = TRUE,
             overall.hetstat = TRUE)


# ==============================================================================
# ANÁLISE 3: BIRTH WEIGHT (g)
# ==============================================================================
df_birth_weight <- as.data.frame(read_excel(STATS, sheet = "Birth weight (g)"))

meta_birth_weight <- metamean(n = n,
                              mean = mean,
                              sd = sd,
                              data = df_birth_weight,
                              sm = "MRAW",
                              studlab = study, 
                              random = TRUE,
                              common = FALSE,
                              method.tau = "REML",
                              method.random.ci = TRUE,           
                              prediction = TRUE)

meta::forest(meta_birth_weight,
             layout = "Revman5",
             leftcols = c("studlab", "mean", "sd", "n", "w.random", "effect", "ci"),
             leftlabs = c("Studies", "Mean", "SD", "Total", "Weight", "Mean", "95% CI"),
             colgap = "6mm",
             just = "center",
             col.square = "darkblue", 
             col.square.lines = "black",
             col.diamond = "black",
             col.predict = "red",
             overall = TRUE,
             overall.hetstat = TRUE,
             test.overall.random = TRUE)