# 
# Supplementary Material: R Code for Statistical Analyses
# Title: Tract Embolization in CT-Guided Percutaneous Biopsy — A Meta-Analysis

#

library(meta)
library(rstudioapi)
library(readxl)
library(dmetar)


#

STATS <- selectFile()

sheetnames <- excel_sheets(STATS)
print(sheetnames)

b <- select.list(sheetnames, title = "Live birth")
b <- as.data.frame(read_excel(STATS, sheet = b))


#

b_sub2 <- subset(b, !is.na(subgroup2) & subgroup2 != "")


# 

ma.b <- metabin(
  event.e, n.e,
  event.c, n.c,
  data             = b,
  studlab          = Study,
  sm               = "RR",
  method           = "Inverse",
  method.tau       = "REML",
  method.I2        = "tau2",
  method.random.ci = "HK",
  method.predict   = "HK",
  random           = TRUE,
  common           = FALSE,
  prediction       = TRUE
)

#

ma.b <- metabin(
  event.e, n.e,
  event.c, n.c,
  data             = b,
  studlab          = Study,
  sm               = "RR",
  method           = "Inverse",
  method.tau       = "REML",
  method.I2        = "tau2",
  method.random.ci = "HK",
  method.predict   = "HK",
  random           = TRUE,
  common           = FALSE,
  prediction       = TRUE,
  subgroup         = subgroup
)


#
tiff(filename = "Figure6.tiff", 
     width = 4500, height = 3500, units = "px", 
     res = 300)

forest(ma.b2,
       layout              = "Revman5",
       sortvar             = studlab,
       label.e             = "Tract embolization",
       label.c             = "No tract embolization",
       label.left          = "Favors tract embolization",
       col.label.left      = "black",
       ff.lr               = "bold",
       label.right         = "Favors no tract embolization",
       col.label.right     = "black",
       leftcols            = c("studlab", "event.e", "n.e", "event.c", "n.c",
                               "w.random", "effect", "ci"),
       rightcols           = FALSE,
       pooled.events       = TRUE,
       pooled.totals       = TRUE,
       random              = TRUE,
       common              = FALSE,
       diamond.random      = TRUE,
       fs.heading          = 12,
       fs.study            = 12,
       fs.hetstat          = 12,
       colgap              = "12mm",
       colgap.forest       = "6mm",
       digits              = 2,
       digits.pval         = 3,
       col.square          = "darkblue",
       col.square.lines    = "black",
       col.diamond         = "black",
       col.diamond.lines   = "black",
       print.Q             = TRUE,
       print.pval.Q        = TRUE,
       print.tau.ci        = TRUE,
       test.overall.random = TRUE,
       overall             = TRUE,
       overall.hetstat     = TRUE)

dev.off()

#

tiff(filename = "SupplementaryFigure3.tiff", 
     width = 4500, height = 3500, units = "px", 
     res = 300)

forest(ma.b,
       layout              = "Revman5",
       sortvar             = studlab,
       label.e             = "Intervention",
       label.c             = "Expectant",
       label.left          = "Favors intervention",
       col.label.left      = "black",
       ff.lr               = "bold",
       label.right         = "Favors expectant",
       col.label.right     = "black",
       leftcols            = c("studlab", "event.e", "n.e", "event.c", "n.c",
                               "w.random", "effect", "ci"),
       rightcols           = FALSE,
       pooled.events       = TRUE,
       pooled.totals       = TRUE,
       random              = TRUE,
       common              = FALSE,
       diamond.random      = TRUE,
       fs.heading          = 12,
       fs.study            = 12,
       fs.hetstat          = 12,
       colgap              = "6mm",
       colgap.forest       = "6mm",
       digits              = 2,
       digits.pval         = 3,
       col.square          = "darkblue",
       col.square.lines    = "black",
       col.diamond         = "black",
       col.diamond.lines   = "black",
       print.subgroup.name = FALSE,
       print.Q             = TRUE,
       print.pval.Q        = TRUE,
       print.tau.ci        = TRUE,
       test.overall.random = TRUE,
       overall             = TRUE,
       overall.hetstat     = TRUE)

dev.off()

#

tf.b <- trimfill(ma.b)
summary(tf.b)

dev.off()

funnel(tf.b,
       studlab     = TRUE,
       bg          = "lightblue",
       cex         = 1,
       cex.studlab = 0.5,
       random      = TRUE,
       common      = FALSE,
       backtransf  = FALSE,
       contour     = c(0.95, 0.99))

eggers.test(ma.b)


# CER, ARR, NNT

rr     <- exp(ma.b$TE.random)
rr_inf <- exp(ma.b$lower.random)
rr_sup <- exp(ma.b$upper.random)

control_risks <- b$event.c / b$n.c
cer           <- median(control_risks, na.rm = TRUE)

aar        <- cer * (1 - rr)
aar_ic_inf <- cer * (1 - rr_sup)
aar_ic_sup <- cer * (1 - rr_inf)

nnt        <- ceiling(1 / aar)
nnt_ic_inf <- ceiling(1 / aar_ic_sup)
nnt_ic_sup <- ceiling(1 / aar_ic_inf)

cat(sprintf("Assumed Control Risk (CER): %.2f%%\n", cer * 100))
cat(sprintf("Pooled Risk Ratio (RR): %.2f (95%% CI: %.2f to %.2f)\n",
            rr, rr_inf, rr_sup))
cat(sprintf("Absolute Risk Reduction (ARR): %.2f%% (95%% CI: %.2f%% to %.2f%%)\n",
            aar * 100, aar_ic_inf * 100, aar_ic_sup * 100))
cat(sprintf("Number Needed to Treat (NNT): %d (95%% CI: %d to %d)\n",
            nnt, nnt_ic_inf, nnt_ic_sup))
cat("----------------------------------------------------\n")