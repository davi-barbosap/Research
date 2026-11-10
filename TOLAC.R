# ==============================================================================
# MetaBayes — TOLAC vs ERCS | Revisão (resposta aos revisores R1–R4)
# Binary Outcome (Risk Ratio) — modelo normal-normal hierárquico (bayesmeta)
#
# O que mudou em relação à versão original (ver também o README da resposta):
#   [A] Leitura robusta: coerção numérica, linhas com NA registradas no log
#   [B] Zeros duplos removidos de fato (drop00 = TRUE). Na versão original o
#       escalc() somava 0,5 às duas caselas e o estudo ENTRAVA com yi = log(n2/n1)
#   [C] Rótulos TOLAC / ERCS no forest; eixos dos gráficos posteriores se
#       ajustam aos dados (RR de ruptura ~4 ficava fora do eixo 0,4–2,5)
#   [D] Mediana posterior usada em todo lugar (antes: média no forest, mediana no LOO)
#   [E] Probabilidades posteriores para vários limiares (RR > 1, 1,25, 1,5, 2 ...)
#       para o efeito médio e para um novo estudo (preditivo)                  [R2]
#   [F] Riscos absolutos: risco basal no ERCS + diferença de risco por 1.000   [R1, R2, R4]
#   [G] Sensibilidades por dados (grupo II), todas bayesianas: LOO sempre, 1 vs ≥2 CS,
#       só termo, sem gestação múltipla, sem populações especiais, sem sobreposição
#       parcial de bases, ERCS com mulheres inelegíveis ao parto, sem megarregistros,
#       estimativas ajustadas                                                   [R1, R2]
#   [H] Funnel plot + meta-regressão bayesiana tipo Peters (efeito ~ 1/N; k ≥ 10) [R2]
#   [I] Gráficos das priors com BayesianPriorsTool (mu_dist_viz / tau_dist_viz) [R2]
#   [J] Tabela-resumo de todos os desfechos + inventário de estudos por desfecho [R1]
#   [K] Regras por desfecho (§0c): estudos só em sensibilidade por definição
#       não padronizada, sensibilidades específicas e subgrupos de limiar da HPP
#   [L] Subgrupos 1 vs ≥2 cesáreas com saída completa + comparação bayesiana entre
#       subgrupos (meta-regressão bmr)                                          [R1 pt 2]
#   [M] Modelo bayesiano Poisson-normal por MCMC (Stan/rstan): verossimilhança exata,
#       sem correção de continuidade, com zeros duplos; diagnósticos completos
#       (trace, R-hat, ESS, autocorrelação, divergências)                  [R1 pt 8, R2]
#   [N] Nenhum método frequentista na inferência: risco basal também bayesiano
# ==============================================================================


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0 — CONFIGURAÇÃO
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# Planilha final (TOLAC - Stats). Cada aba = um desfecho; colunas study, ai, n1i, ci, n2i
# ai/n1i = TOLAC planejado ; ci/n2i = ERCS
url        <- "https://docs.google.com/spreadsheets/d/1tW2ozCH1n1KkAEolTbQ1X7P8Bu1RdjezLhaRQ1dAXHI/edit"

# Alternativa sem Google: pasta com um CSV por aba (nome do arquivo = nome da aba)
local_csv_dir <- NULL   # ex.: "/Users/davibarbosa/Desktop/TOLAC_csv"

output_dir <- "/Users/davibarbosa/Desktop/TOLAC_revisao"

label_e <- "TOLAC"
label_c <- "ERCS"

# Zeros duplos: TRUE = removidos da análise principal (recomendado; descrito no Métodos).
# A análise com eles incluídos sai como sensibilidade.
drop_double_zero <- TRUE

# Limiares para as probabilidades posteriores P(RR > x)
prob_thresholds <- c(0.80, 1.00, 1.25, 1.50, 2.00)

# Mínimo de estudos para rodar cada análise
k_min_bayes  <- 2
k_min_loo    <- 3
k_min_funnel <- 10

# Sensibilidade por MCMC (Stan/rstan). Compila uma vez (~1 min) e reaproveita.
run_mcmc     <- TRUE
mcmc_chains  <- 4
mcmc_iter    <- 4000     # por cadeia, incluindo aquecimento
mcmc_warmup  <- 1000     # sem thinning
mcmc_seed    <- 2026

# Megarregistros: estudos com mais de mega_n mulheres saem nesta sensibilidade
mega_n <- 50000


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0b — CARACTERÍSTICAS DOS ESTUDOS (para as sensibilidades do grupo II)
# Fonte: auditoria dos PDFs. Revisar se a equipe tiver decidido diferente.
#   prior_cs : "1" | "2+" | "misto"
#   term     : TRUE se a população é restrita a ≥37 semanas
#   multiple : TRUE se gestações múltiplas foram incluídas
#   special  : população especial (PIG, obesas, gemelares) — "" se não
#   overlap  : TRUE se o estudo sai na sensibilidade de sobreposição PARCIAL de bases
#              (Dombrowski × Yao: Califórnia 2011–2012; Uno × Takeya: hospital de Uno
#              pode ter contribuído para a base JSOG 2013–2015).
#              As sobreposições totais (Kok ⊂ Koorn; Studsgaard ⊂ O'Neill) já foram
#              resolvidas na planilha: o estudo menor saiu das abas em que os dois apareciam.
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

study_meta <- read.csv(text = "
study,prior_cs,term,multiple,special,overlap
Attanasio 2025,1,FALSE,FALSE,,FALSE
Balachandran 2014,1,FALSE,TRUE,,FALSE
Blanchette 2001,misto,FALSE,TRUE,,FALSE
Boatin 2017,1,FALSE,TRUE,,FALSE
Boucherie 2025,1,FALSE,FALSE,,FALSE
Chatzistergiou 2025,1,TRUE,FALSE,,FALSE
Chen 2022,misto,FALSE,TRUE,,FALSE
Crowther 2012,1,TRUE,FALSE,,FALSE
Denjean 2024,2+,TRUE,FALSE,,FALSE
Dombrowski 2020,2+,TRUE,FALSE,,TRUE
Fitzpatrick 2019,misto,TRUE,FALSE,,FALSE
Gonen 2006,1,FALSE,FALSE,,FALSE
İzbudak 2021,1,TRUE,FALSE,,FALSE
Kamath 2009,1,TRUE,FALSE,,FALSE
Kok 2015,1,TRUE,FALSE,,FALSE
Koorn 2024,1,TRUE,FALSE,,FALSE
Macones 2005,2+,FALSE,FALSE,,FALSE
McMullan 2024,2+,FALSE,FALSE,,FALSE
O'Neill 2017,1,TRUE,FALSE,,FALSE
Obara 1998,1,FALSE,FALSE,,FALSE
Phillips 2024,misto,FALSE,FALSE,,FALSE
Socol 1999,misto,FALSE,FALSE,,FALSE
Spaans 2003,2+,FALSE,FALSE,,FALSE
Studsgaard 2013,1,TRUE,FALSE,,FALSE
Takeya 2020,1,TRUE,FALSE,,FALSE
Tsai 2017,1,TRUE,FALSE,,FALSE
Turitz 2015,1,FALSE,FALSE,Só PIG,FALSE
Uno 2020,1,FALSE,FALSE,,TRUE
Varner 2005,misto,FALSE,TRUE,Só gemelares,FALSE
Yao 2017,misto,TRUE,FALSE,Só obesas,FALSE
", stringsAsFactors = FALSE, encoding = "UTF-8")

# ercs_inelig: o braço ERCS incluiu mulheres que NÃO seriam elegíveis ao trabalho de
# parto (pélvicas, gemelares, cicatriz desconhecida, macrossomia) — PROSPERO exige
# elegibilidade para as duas vias                                          [R1 pt 2]
study_meta$ercs_inelig <- study_meta$study %in% c("Balachandran 2014", "Gonen 2006", "Spaans 2003")


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §0c — REGRAS POR DESFECHO (decisões da auditoria; a planilha não é alterada)
# Chave = nome da aba (sem diferenciar maiúsculas/minúsculas)
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# Estudos que ficam na planilha mas saem da ANÁLISE PRINCIPAL (entram só em sensibilidade)
main_exclude <- list(
  "neonatal death" = c(
    "Fitzpatrick 2019",  # natimorto intraparto + óbito neonatal (não separável)
    "Varner 2005"),      # só subgrupo ≥34 sem; óbito ERCS por anomalia
  "pph" = c(
    "Balachandran 2014") # numerador só de VBAC alcançado (denominador ITT)
)

# Sensibilidades específicas de cada desfecho
#   exclude = estudos retirados; replace = valores alternativos para estudos da aba
outcome_sens <- list(
  "uterine rupture" = list(
    list(label   = "Complete rupture only (excluding rupture-or-dehiscence: Socol, Turitz)",
         exclude = c("Socol 1999", "Turitz 2015")),
    list(label   = "Excluding Takeya 2020 (vague registry definition; implausible ERCS rate)",
         exclude = "Takeya 2020")),
  "perinatal death" = list(
    list(label   = "Boatin 2017 excluding macerated stillbirths (12/431 vs 2/377, from %)",
         replace = data.frame(study = "Boatin 2017", ai = 12, n1i = 431, ci = 2, n2i = 377)))
)

# Limiar de definição da hemorragia pós-parto (subgrupos na aba PPH)
pph_threshold <- c(
  "Balachandran 2014" = "Not specified", "Blanchette 2001" = "Not specified",
  "Boucherie 2025" = "≥500 mL (vaginal) / ≥1000 mL (CS)", "Chatzistergiou 2025" = ">500 mL",
  "Chen 2022" = "Not specified", "Crowther 2012" = ">500 mL", "Kok 2015" = "≥1000 mL",
  "McMullan 2024" = "≥1000 mL", "Obara 1998" = "≥1500 mL", "Phillips 2024" = "Not specified",
  "Studsgaard 2013" = "≥1000 mL", "Takeya 2020" = "≥1000 mL", "Tsai 2017" = "Not specified",
  "İzbudak 2021" = "Not specified")

# Estimativas AJUSTADAS publicadas (para a sensibilidade "ajustada")             [R1 pt 4]
# Substituem o RR bruto do estudo; os demais estudos entram com o RR bruto.
# aOR ≈ RR para desfechos raros; para desfechos comuns (UTIN, HPP) a aproximação é
# conservadora (OR afasta-se de 1 mais que o RR). Fonte = tabela/trecho do artigo.
adjusted_estimates <- read.csv(text = "
outcome,study,measure,est,lo,hi,source
uterine rupture,Fitzpatrick 2019,aRR,7.33,3.88,13.88,Table 2 (model B)
uterine rupture,Yao 2017,aOR,8.89,6.99,11.32,Table 2
uterine rupture,Chatzistergiou 2025,aOR,2.08,1.24,3.58,Results / Table 2
uterine rupture,Phillips 2024,aOR,2.0,0.8,5.5,Table 2
uterine rupture,Macones 2005,aOR,29.1,3.4,246.3,Table V
neonatal death,O'Neill 2017,aOR,1.87,1.12,3.12,Abstract / Table 3
neonatal death,Yao 2017,aOR,1.37,1.03,1.82,Table 2
neonatal death,Phillips 2024,aOR,0.6,0.2,1.4,Table 2
blood transfusion,Fitzpatrick 2019,aRR,2.29,1.88,2.79,Table 2 (model B)
blood transfusion,Yao 2017,aOR,1.32,1.16,1.50,Table 2
blood transfusion,Phillips 2024,aOR,2.3,1.6,3.2,Table 2
blood transfusion,Macones 2005,aOR,0.54,0.23,1.27,Table V
hysterectomy,Yao 2017,aOR,1.03,0.75,1.41,Table 2
hysterectomy,Phillips 2024,aOR,0.1,0.01,0.3,Table 2
pph,Phillips 2024,aOR,1.6,1.3,2.0,Table 2
endometritis,Phillips 2024,aOR,0.5,0.2,0.9,Table 2
maternal morbidity,Phillips 2024,aOR,1.5,1.2,1.8,Table 2
nicu admission,Yao 2017,aOR,1.29,1.25,1.33,Table 2
nicu admission,Phillips 2024,aOR,1.2,1.0,1.4,Table 2
nicu admission,Chatzistergiou 2025,aOR,0.45,0.31,0.69,Abstract
neonatal respiratory complication,Yao 2017,aOR,1.17,1.06,1.30,Table 2
neonatal respiratory complication,Phillips 2024,aOR,0.8,0.6,1.1,Table 2
5min apgar <7,Yao 2017,aOR,1.82,1.71,1.94,Table 2
", stringsAsFactors = FALSE, encoding = "UTF-8")

rule_for <- function(lst, sheet) lst[[tolower(trimws(sheet))]]
in_list  <- function(study, ref) study_key(study) %in% study_key(ref)


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §1 — PACOTES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

if (!require("devtools")) install.packages("devtools")
if (!require("pacman"))   install.packages("pacman")

# BayesianPriorsTool — disponível apenas no GitHub
if (!requireNamespace("BayesianPriorsTool", quietly = TRUE))
  devtools::install_github("arthur-albuquerque/BayesianPriorsTool")

pacman::p_load(
  googlesheets4, ragg, officer, bayesmeta, BayesianPriorsTool, metafor, meta,
  ggplot2, patchwork, janitor, future, future.apply
)
# metafor/meta: só cálculo dos tamanhos de efeito por estudo (escalc) e desenho dos
# gráficos (forest/funnel). Toda a inferência é bayesiana.
if (run_mcmc) pacman::p_load(rstan, bayesplot, posterior)


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §2 — TEMA
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

theme_set(
  theme(
    plot.title.position = "plot",
    axis.ticks.x        = element_blank(),
    axis.ticks.y        = element_blank(),
    axis.text.x         = element_text(size = 12),
    axis.text.y         = element_text(size = 12),
    axis.title.x        = element_text(size = 14),
    panel.background    = element_blank(),
    panel.grid.major.x  = element_line(color = "gray80", linewidth = 0.3)
  )
)


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §3 — FUNÇÕES AUXILIARES
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

log_msg <- function(context, msg) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", context, " — ", msg),
        file = file.path(output_dir, "pipeline_log.txt"), append = TRUE)
}

save_plot <- function(filename_base, plot_fn, out_dir, width = 16, height = 10) {
  agg_tiff(file.path(out_dir, paste0(filename_base, ".tiff")),
           width = width, height = height, units = "in", res = 300, compression = "lzw")
  plot_fn(); dev.off()
  agg_png(file.path(out_dir, paste0(filename_base, ".png")),
          width = width, height = height, units = "in", res = 300)
  plot_fn(); dev.off()
}

save_docx <- function(filename_base, lines, out_dir) {
  prop <- officer::fp_text(font.family = "Courier New", font.size = 9)
  doc  <- officer::read_docx()
  for (line in lines)
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(as.character(line), prop)))
  print(doc, target = file.path(out_dir, paste0(filename_base, ".docx")))
}

fmt_rr <- function(m, lo, hi, d = 2)
  sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), exp(m), exp(lo), exp(hi))

norm_study <- function(x) gsub("\\s+", " ", trimws(enc2utf8(as.character(x))))
# chave de junção sem acentos/maiúsculas (evita falha com "İzbudak" em locales não UTF-8)
study_key <- function(x) tolower(gsub("[^A-Za-z0-9]", "", iconv(norm_study(x), "UTF-8", "ASCII//TRANSLIT", sub = "")))

to_num <- function(x) {
  if (is.list(x)) x <- vapply(x, function(v) if (length(v) == 0 || is.null(v)) NA_character_
                              else as.character(v[[1]]), character(1))
  x <- trimws(as.character(x))
  x <- gsub("\\.(?=\\d{3}(\\D|$))", "", x, perl = TRUE)  # "40.685" -> 40685 (milhar pt-BR)
  x <- gsub(",(?=\\d{3}(\\D|$))",  "", x, perl = TRUE)   # "1,016"  -> 1016  (milhar en)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

# ── [A] Leitura ────────────────────────────────────────────────────────────────
list_outcomes <- function() {
  if (!is.null(local_csv_dir))
    return(tools::file_path_sans_ext(list.files(local_csv_dir, pattern = "\\.csv$")))
  googlesheets4::sheet_names(url)
}

read_outcome <- function(sheet) {
  raw <- if (!is.null(local_csv_dir)) {
    read.csv(file.path(local_csv_dir, paste0(sheet, ".csv")),
             colClasses = "character", check.names = FALSE, encoding = "UTF-8")
  } else {
    as.data.frame(googlesheets4::read_sheet(url, sheet = sheet, col_types = "c"))
  }
  dat <- janitor::clean_names(raw)
  need <- c("study", "ai", "n1i", "ci", "n2i")
  miss <- setdiff(need, names(dat))
  if (length(miss)) stop("Colunas ausentes na aba '", sheet, "': ", paste(miss, collapse = ", "))
  dat <- dat[!is.na(dat$study) & nzchar(trimws(dat$study)), need]
  dat$study <- norm_study(dat$study)
  for (v in need[-1]) dat[[v]] <- to_num(dat[[v]])
  
  bad <- !stats::complete.cases(dat[, need[-1]]) |
    dat$ai > dat$n1i | dat$ci > dat$n2i | dat$n1i <= 0 | dat$n2i <= 0
  bad[is.na(bad)] <- TRUE
  if (any(bad)) {
    log_msg(sheet, paste("Linhas excluídas por valor ausente/inválido:",
                         paste(dat$study[bad], collapse = "; ")))
    warning("Aba '", sheet, "': linha(s) inválida(s) excluída(s): ",
            paste(dat$study[bad], collapse = "; "), call. = FALSE)
  }
  dat <- dat[!bad, ]
  dat$key <- study_key(dat$study)
  sm  <- study_meta; sm$key <- study_key(sm$study); sm$study <- NULL
  dat <- merge(dat, sm, by = "key", all.x = TRUE, sort = FALSE); dat$key <- NULL
  nm <- is.na(dat$prior_cs)
  if (any(nm)) log_msg(sheet, paste("Sem metadados (fora das sensibilidades por subgrupo):",
                                    paste(dat$study[nm], collapse = "; ")))
  dat$double_zero <- dat$ai == 0 & dat$ci == 0
  dat
}

# ── Tamanhos de efeito ─────────────────────────────────────────────────────────
make_es <- function(dat, drop00 = drop_double_zero) {
  es <- escalc(measure = "RR", ai = ai, n1i = n1i, ci = ci, n2i = n2i,
               data = dat, add = 1/2, to = "only0", drop00 = drop00) |> data.frame()
  es$sei <- sqrt(es$vi)
  es[is.finite(es$yi) & is.finite(es$sei) & es$sei > 0, ]
}

# ── Priors ─────────────────────────────────────────────────────────────────────
# mu ~ N(0, 2.7²): vago. 95% da massa a priori entre RR 0,005 e 200; centrado na
#   ausência de efeito, praticamente sem influência com k moderado.
# tau ~ Half-Normal(0.5): fracamente informativo. Mediana a priori ≈ 0,34 e 95% < 0,98,
#   cobrindo heterogeneidade de "baixa" a "muito alta" para log-RR (Röver et al.,
#   Res Synth Methods 2021;12:448–474). Sensibilidades: tau HN(1.5) e mu N(0, 0.82²)
#   (cético: 95% dos RR a priori entre 0,2 e 5).
priors <- list(
  primary = list(name = "Primary: mu N(0, 2.7²), tau HN(0.5)",          mu_sd = 2.7,  tau = 0.5),
  sens1   = list(name = "Sensitivity 1: vague tau HN(1.5)",              mu_sd = 2.7,  tau = 1.5),
  sens2   = list(name = "Sensitivity 2: weakly-informative mu N(0, 0.82²)", mu_sd = 0.82, tau = 0.5)
)

fit_bm <- function(es, pr = priors$primary) {
  bayesmeta(y = es$yi, sigma = es$sei, labels = es$study,
            mu.prior.mean = 0, mu.prior.sd = pr$mu_sd,
            tau.prior = function(x) dhalfnormal(x, scale = pr$tau))
}

# [D] mediana posterior + intervalo de credibilidade de 95% (mais curto, padrão do bayesmeta)
bm_sum <- function(fit) {
  s <- fit$summary
  list(mu = s["median", "mu"], mu_lo = s["95% lower", "mu"], mu_hi = s["95% upper", "mu"],
       tau = s["median", "tau"], tau_lo = s["95% lower", "tau"], tau_hi = s["95% upper", "tau"],
       th = s["median", "theta"], th_lo = s["95% lower", "theta"], th_hi = s["95% upper", "theta"])
}

# [E] P(RR > x) para o efeito médio (mu) e para um novo estudo (theta, preditivo)
post_probs <- function(fit, thr = prob_thresholds) {
  data.frame(
    threshold   = thr,
    P_mu_gt     = 1 - fit$pposterior(mu = log(thr)),
    P_theta_gt  = 1 - fit$pposterior(theta = log(thr), predict = TRUE)
  )
}

# [F] Riscos absolutos. Risco basal = mediana posterior da proporção nos braços ERCS
# (meta-análise bayesiana de logitos; mu ~ N(0, 10²), tau ~ HN(1)).
# DR por 1.000 = p0 × (RR − 1) × 1000.
absolute_risk <- function(fit, dat) {
  p0_fit <- tryCatch({
    pl <- escalc(measure = "PLO", xi = ci, ni = n2i, data = dat, add = 1/2, to = "only0")
    bayesmeta(y = pl$yi, sigma = sqrt(pl$vi), mu.prior.mean = 0, mu.prior.sd = 10,
              tau.prior = function(x) dhalfnormal(x, scale = 1))
  }, error = function(e) NULL)
  p0 <- if (is.null(p0_fit)) sum(dat$ci) / sum(dat$n2i) else plogis(p0_fit$summary["median", "mu"])
  # DR é função monótona de mu: os limites do intervalo de mu viram limites da DR
  # (cálculo exato, sem amostragem — rposterior com tau amostrado é muito lento)
  s   <- bm_sum(fit)
  ard <- p0 * (exp(c(s$mu, s$mu_lo, s$mu_hi)) - 1) * 1000
  mu_cut <- log(1 + 1 / (1000 * p0))           # mu acima do qual o excesso > 1/1.000
  list(p0_per1000 = p0 * 1000,
       ard = ard[1], ard_lo = ard[2], ard_hi = ard[3],
       P_ard_gt_1per1000 = 1 - fit$pposterior(mu = mu_cut),
       nnh = if (ard[1] > 0) 1000 / ard[1] else NA,
       baseline_method = if (is.null(p0_fit)) "crude pooled" else "Bayesian random-effects logit")
}

# ── [C] Gráficos posteriores com eixo ajustado aos dados ───────────────────────
log_breaks <- function(lims) {
  cand <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 10, 20, 50, 100)
  b <- cand[cand >= lims[1] & cand <= lims[2]]
  if (length(b) > 7) b <- b[unique(round(seq(1, length(b), length.out = 7)))]
  unique(sort(c(b, 1)))
}

# Densidades calculadas analiticamente (dposterior), sem amostragem
generate_posterior_plots <- function(fit, fill_color, is_predict = FALSE) {
  q <- if (is_predict) fit$qposterior(theta.p = c(0.002, 0.998), predict = TRUE)
  else            fit$qposterior(mu.p = c(0.002, 0.998))
  lims <- c(min(exp(q[1]), 0.8), max(exp(q[2]), 1.25))
  brk  <- log_breaks(lims)
  s    <- bm_sum(fit)
  ci   <- if (is_predict) c(s$th, s$th_lo, s$th_hi) else c(s$mu, s$mu_lo, s$mu_hi)
  
  xg <- seq(log(lims[1]), log(lims[2]), length.out = 500)
  dg <- if (is_predict) fit$dposterior(theta = xg, predict = TRUE) else fit$dposterior(mu = xg)
  dd <- data.frame(x = xg, d = dg, inside = xg >= ci[2] & xg <= ci[3])
  
  p_dens <- ggplot(dd, aes(x, d)) +
    geom_area(fill = fill_color, alpha = 0.25) +
    geom_area(data = dd[dd$inside, ], fill = fill_color, alpha = 0.8) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = 2, colour = "gray40") +
    geom_vline(xintercept = ci[1], colour = "black") +
    scale_x_continuous(breaks = log(brk), labels = brk) +
    labs(x = "\nRisk Ratio (TOLAC vs ERCS)", y = "Density",
         subtitle = sprintf("%s: median %.2f, 95%% %s %.2f–%.2f (dark area)",
                            if (is_predict) "New study" else "Average effect",
                            exp(ci[1]), if (is_predict) "PI" else "CrI", exp(ci[2]), exp(ci[3]))) +
    theme(axis.text.y = element_blank())
  
  grid <- exp(seq(log(lims[1]), log(lims[2]), length.out = 400))
  pr   <- if (is_predict) 1 - fit$pposterior(theta = log(grid), predict = TRUE)
  else            1 - fit$pposterior(mu = log(grid))
  p_prob <- ggplot(data.frame(x = grid, p = pr), aes(x, p)) +
    geom_line() +
    geom_vline(xintercept = 1, linetype = 2, colour = "gray40") +
    scale_x_log10(breaks = brk, labels = brk) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    labs(x = "\nRisk Ratio (TOLAC vs ERCS)", y = "Probability RR > X") +
    theme(panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3))
  
  list(density = p_dens, probability = p_prob)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §4 — SENSIBILIDADES (grupo II) [G]
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_loo <- function(es, full_fit, out_dir, sheet) {
  K <- nrow(es)
  plan(multisession); on.exit(plan(sequential), add = TRUE)
  res <- future_lapply(seq_len(K), function(i) {
    f <- fit_bm(es[-i, ])
    s <- f$summary
    data.frame(study_excluded = es$study[i], mu = s["median", "mu"],
               lo = s["95% lower", "mu"], hi = s["95% upper", "mu"], tau = s["median", "tau"],
               P_gt1 = 1 - f$pposterior(mu = 0))
  }, future.seed = TRUE)
  loo <- do.call(rbind, res)
  fs  <- bm_sum(full_fit)
  
  m_loo <- metagen(TE = loo$mu, lower = loo$lo, upper = loo$hi, sm = "RR",
                   studlab = paste0("Omitting ", loo$study_excluded),
                   common = FALSE, random = FALSE)
  m_loo <- metaadd(m_loo, type = "random", TE = fs$mu, lower = fs$mu_lo, upper = fs$mu_hi,
                   text = "All studies (reference)")
  save_plot("Supp_Fig_LOO_Forest", function() {
    meta::forest(m_loo, leftcols = "studlab", leftlabs = "Omitted study",
                 rightcols = c("effect", "ci"), rightlabs = c("RR", "95% CrI"),
                 smlab = "", weight.study = "same", hetstat = FALSE,
                 col.square = "steelblue", col.square.lines = "steelblue")
  }, out_dir, height = max(8, K * 0.45 + 4))
  
  loo$RR_CrI <- fmt_rr(loo$mu, loo$lo, loo$hi)
  write.csv(loo, file.path(out_dir, "Supp_Table_LOO.csv"), row.names = FALSE)
  save_docx("Supp_Table_LOO", c(
    paste0("Leave-one-out sensitivity analysis — ", sheet),
    sprintf("%-28s | %-22s | %6s | %s", "Study omitted", "RR [95% CrI]", "tau", "P(RR>1)"),
    paste(rep("-", 76), collapse = ""),
    sprintf("%-28s | %-22s | %6.2f | %5.1f%%", loo$study_excluded, loo$RR_CrI, loo$tau, 100 * loo$P_gt1),
    paste(rep("-", 76), collapse = ""),
    sprintf("%-28s | %-22s | %6.2f |", "All studies", fmt_rr(fs$mu, fs$mu_lo, fs$mu_hi), fs$tau)
  ), out_dir)
  loo
}

run_data_sensitivity <- function(dat, dat_all, out_dir, sheet, mcmc_row = NULL) {
  row_b <- function(label, d, drop00 = drop_double_zero) {
    es <- if (nrow(d) > 0) make_es(d, drop00 = drop00) else d[0, ]
    if (nrow(es) < k_min_bayes)
      return(data.frame(analysis = label, k = nrow(es), estimate = NA, method = "Bayesian NNHM",
                        RR_CI = "k insuficiente", tau = NA, P_RR_gt1 = NA))
    f <- fit_bm(es); s <- bm_sum(f)
    data.frame(analysis = label, k = nrow(es), estimate = exp(s$mu), method = "Bayesian NNHM",
               RR_CI = fmt_rr(s$mu, s$mu_lo, s$mu_hi), tau = round(s$tau, 2),
               P_RR_gt1 = round(1 - f$pposterior(mu = 0), 3))
  }
  m <- dat
  out <- list(
    row_b("Primary analysis", m),
    row_b("Including double-zero studies (0.5 correction)", m, drop00 = FALSE),
    row_b("Excluding single-zero studies", m[!(xor(m$ai == 0, m$ci == 0)), ]),
    row_b("Only 1 previous CS", m[m$prior_cs %in% "1", ]),
    row_b("Only ≥2 previous CS", m[m$prior_cs %in% "2+", ]),
    row_b("Term-only populations (≥37 wk)", m[m$term %in% TRUE, ]),
    row_b("Excluding multiple gestations", m[!(m$multiple %in% TRUE), ]),
    row_b("Excluding special populations (SGA, obese, twins)",
          m[is.na(m$special) | !nzchar(m$special), ]),
    row_b("Excluding partial database overlap (Dombrowski [× Yao], Uno [× Takeya])",
          m[!(m$overlap %in% TRUE), ]),
    row_b("Excluding studies whose ERCS arm included women ineligible for labor (Balachandran, Gonen, Spaans)",
          m[!(m$ercs_inelig %in% TRUE), ]),
    row_b(paste0("Excluding mega-registries (>", format(mega_n, big.mark = ","), " women: ",
                 paste(m$study[(m$n1i + m$n2i) > mega_n], collapse = ", "), ")"),
          m[(m$n1i + m$n2i) <= mega_n, ])
  )
  
  # Estimativas ajustadas no lugar das brutas, quando o estudo publica
  adj <- adjusted_estimates[tolower(adjusted_estimates$outcome) == tolower(trimws(sheet)), ]
  if (nrow(adj)) {
    es_adj <- make_es(m)
    hit <- match(study_key(es_adj$study), study_key(adj$study))
    if (any(!is.na(hit))) {
      w <- which(!is.na(hit))
      es_adj$yi[w]  <- log(adj$est[hit[w]])
      es_adj$sei[w] <- (log(adj$hi[hit[w]]) - log(adj$lo[hit[w]])) / (2 * qnorm(0.975))
      f <- fit_bm(es_adj); sa <- bm_sum(f)
      out <- c(out, list(data.frame(
        analysis = paste0("Adjusted estimates where published (", paste(es_adj$study[w], collapse = ", "),
                          "); crude RR for the others"),
        k = nrow(es_adj), estimate = exp(sa$mu), method = "Bayesian NNHM",
        RR_CI = fmt_rr(sa$mu, sa$mu_lo, sa$mu_hi), tau = round(sa$tau, 2),
        P_RR_gt1 = round(1 - f$pposterior(mu = 0), 3))))
    }
  }
  
  # Estudos fora da análise principal por definição → reincluídos aqui
  excl <- rule_for(main_exclude, sheet)
  if (length(excl) && any(in_list(dat_all$study, excl)))
    out <- c(out, list(row_b(paste0("Including studies with non-standard definitions (",
                                    paste(dat_all$study[in_list(dat_all$study, excl)], collapse = ", "), ")"),
                             dat_all)))
  
  # Sensibilidades específicas do desfecho
  for (r in rule_for(outcome_sens, sheet)) {
    d <- m
    if (!is.null(r$exclude)) d <- d[!in_list(d$study, r$exclude), ]
    if (!is.null(r$replace)) for (j in seq_len(nrow(r$replace))) {
      w <- in_list(d$study, r$replace$study[j])
      if (any(w)) d[w, c("ai", "n1i", "ci", "n2i")] <- r$replace[j, c("ai", "n1i", "ci", "n2i")]
    }
    d$double_zero <- d$ai == 0 & d$ci == 0
    out <- c(out, list(row_b(r$label, d)))
  }
  
  # Hemorragia pós-parto: subgrupos pelo limiar de definição
  if (tolower(trimws(sheet)) == "pph") {
    thr <- unname(pph_threshold[match(study_key(m$study), study_key(names(pph_threshold)))])
    thr[is.na(thr)] <- "Not specified"
    for (g in unique(thr)) out <- c(out, list(row_b(paste0("PPH subgroup — definition: ", g), m[thr == g, ])))
  }
  
  # Eventos raros: modelo bayesiano de verossimilhança exata por MCMC (calculado fora)
  tab <- do.call(rbind, c(out, list(mcmc_row)))
  write.csv(tab, file.path(out_dir, "Supp_Table_Sensitivity_Data.csv"), row.names = FALSE)
  save_docx("Supp_Table_Sensitivity_Data", c(
    paste0("Sensitivity analyses (study-level) — ", sheet),
    sprintf("%-68s | %3s | %-22s | %5s | %s", "Analysis", "k", "RR [95% CrI/CI]", "tau", "P(RR>1)"),
    paste(rep("-", 118), collapse = ""),
    sprintf("%-68s | %3d | %-22s | %5s | %s", tab$analysis, tab$k, tab$RR_CI,
            ifelse(is.na(tab$tau), "", sprintf("%.2f", tab$tau)),
            ifelse(is.na(tab$P_RR_gt1), "", sprintf("%.1f%%", 100 * tab$P_RR_gt1)))
  ), out_dir)
  tab
}

# [H] Funnel plot + teste bayesiano de efeitos de estudos pequenos (análogo de Peters:
# meta-regressão bayesiana do log-RR em 1/N total, bayesmeta::bmr, mesma prior de tau).
# Inclinação > 0 = estudos menores com RR maior. Com RR e eventos raros, o Egger (EP como
# preditor) tem falso-positivo inflado; 1/N evita a correlação artificial yi–EP.
run_funnel <- function(es, fit, out_dir, sheet) {
  mu <- bm_sum(fit)$mu
  res <- NULL
  if (nrow(es) >= k_min_funnel) {
    X <- cbind(intercept = 1, inv_n = 1 / (es$n1i + es$n2i))
    b <- bmr(y = es$yi, sigma = es$sei, labels = es$study, X = X,
             tau.prior = function(x) dhalfnormal(x, scale = priors$primary$tau))
    sb <- b$summary
    res <- list(lo = sb["95% lower", "inv_n"], hi = sb["95% upper", "inv_n"],
                P_pos = 1 - b$pposterior(beta = 0, which.beta = 2))
  }
  save_plot("Supp_Fig_Funnel", function() {
    funnel(x = es$yi, vi = es$vi, refline = mu, atransf = exp, legend = FALSE,
           xlab = "Risk Ratio (log scale); line = Bayesian posterior median",
           main = paste0(sheet, if (!is.null(res))
             sprintf(" — small-study slope 95%% CrI [%.0f, %.0f]; P(slope > 0) = %.2f", res$lo, res$hi, res$P_pos)
             else sprintf(" — k = %d (<%d: regression not performed)", nrow(es), k_min_funnel)))
  }, out_dir, width = 10, height = 7)
  if (is.null(res)) NA else res$P_pos
}



# ── Forest + posterior + probabilidades + risco absoluto para um conjunto de estudos ──
tau_txt <- function(s) sprintf("τ = %.2f [95%% CrI %.2f, %.2f]", s$tau, s$tau_lo, s$tau_hi)

report_primary <- function(es, fit, out_dir, title) {
  s  <- bm_sum(fit)
  fd <- metabin(event.e = ai, n.e = n1i, event.c = ci, n.c = n2i, studlab = study,
                data = es, sm = "RR", common = FALSE, random = FALSE)
  mf <- metaadd(fd, type = "random", TE = s$mu, lower = s$mu_lo, upper = s$mu_hi,
                text = "Bayesian summary (median, 95% CrI)")
  mf <- metaadd(mf, type = "prediction", TE = s$th, lower = s$th_lo, upper = s$th_hi,
                text = "Bayesian 95% prediction interval")
  save_plot("Figure_Forest", function() {
    meta::forest(mf, layout = "RevMan5", text.addline1 = tau_txt(s),
                 text.random = "Bayesian summary (median, 95% CrI)",
                 text.predict = "Bayesian 95% prediction interval",
                 smlab = "Risk Ratio", pooled.events = TRUE,
                 leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"),
                 leftlabs = c("Study", "Events", "Total", "Events", "Total", "RR [95% CI]"),
                 label.e = label_e, label.c = label_c,
                 label.left = paste("Favors", label_e), label.right = paste("Favors", label_c),
                 colgap = "6mm")
  }, out_dir, width = 12, height = max(6, nrow(es) * 0.3 + 3))
  
  p_mu <- generate_posterior_plots(fit, "skyblue")
  save_plot("Figure_Posterior_Dist", function() print(p_mu$density + p_mu$probability), out_dir)
  
  pp <- post_probs(fit)
  write.csv(pp, file.path(out_dir, "Posterior_Probabilities.csv"), row.names = FALSE)
  ar <- absolute_risk(fit, es)
  save_docx("Posterior_Probabilities_and_Absolute_Risk", c(
    paste0("Outcome: ", title, "  (", label_e, " vs ", label_c, "; RR > 1 = more events with ", label_e, ")"),
    paste(rep("=", 78), collapse = ""),
    sprintf("k = %d | RR %s | 95%% PI %s | %s", nrow(es),
            fmt_rr(s$mu, s$mu_lo, s$mu_hi), fmt_rr(s$th, s$th_lo, s$th_hi), tau_txt(s)),
    "",
    sprintf("%-12s | %-24s | %-24s", "Threshold", "P(RR > x), average effect", "P(RR > x), new study"),
    sprintf("RR > %-7.2f | %23.1f%% | %23.1f%%", pp$threshold, 100 * pp$P_mu_gt, 100 * pp$P_theta_gt),
    "",
    sprintf("Baseline risk in %s arms (%s): %.2f per 1,000", label_c, ar$baseline_method, ar$p0_per1000),
    sprintf("Absolute risk difference: %+.2f per 1,000 [95%% CrI %+.2f, %+.2f]", ar$ard, ar$ard_lo, ar$ard_hi),
    sprintf("P(excess > 1 per 1,000): %.1f%%", 100 * ar$P_ard_gt_1per1000),
    if (!is.na(ar$nnh)) sprintf("Number needed to harm (median): %.0f", ar$nnh) else ""
  ), out_dir)
  list(s = s, pp = pp, ar = ar)
}

# ── [L] Subgrupos 1 vs ≥2 cesáreas: saída completa + comparação bayesiana ────────
run_cs_subgroups <- function(dat, out_main, sheet) {
  groups <- list("1 previous CS" = "1", "2+ previous CS" = "2+")
  res <- list(); es_l <- list()
  for (g in names(groups)) {
    d  <- dat[dat$prior_cs %in% groups[[g]], ]
    es <- if (nrow(d)) make_es(d) else d[0, ]
    if (nrow(es) < k_min_bayes) { res[[g]] <- NULL; next }
    od <- file.path(out_main, paste0("Subgroup_", gsub("[^A-Za-z0-9]", "", g)))
    dir.create(od, showWarnings = FALSE, recursive = TRUE)
    f  <- fit_bm(es)
    r  <- report_primary(es, f, od, paste0(sheet, " — ", g))
    res[[g]] <- list(k = nrow(es), s = r$s, P = 1 - f$pposterior(mu = 0)); es_l[[g]] <- es
  }
  # Comparação: meta-regressão bayesiana com indicador de ≥2 CS (razão de RRs)
  cmp <- NULL
  if (length(es_l) == 2) {
    e2 <- rbind(transform(es_l[[1]], g2 = 0), transform(es_l[[2]], g2 = 1))
    b  <- bmr(y = e2$yi, sigma = e2$sei, labels = e2$study, X = cbind(intercept = 1, two_plus = e2$g2),
              tau.prior = function(x) dhalfnormal(x, scale = priors$primary$tau))
    sb <- b$summary
    cmp <- list(ratio = exp(sb["median", "two_plus"]), lo = exp(sb["95% lower", "two_plus"]),
                hi = exp(sb["95% upper", "two_plus"]), P = 1 - b$pposterior(beta = 0, which.beta = 2))
  }
  lines <- c(paste0("Subgroups by number of previous cesareans — ", sheet),
             "Studies with mixed populations (1 and ≥2 CS not separable) are not part of either subgroup.",
             paste(rep("-", 90), collapse = ""))
  for (g in names(groups)) lines <- c(lines, if (is.null(res[[g]])) sprintf("%-16s | k insuficiente", g)
                                      else sprintf("%-16s | k = %2d | RR %s | P(RR>1) = %.1f%%", g, res[[g]]$k,
                                                   fmt_rr(res[[g]]$s$mu, res[[g]]$s$mu_lo, res[[g]]$s$mu_hi), 100 * res[[g]]$P))
  if (!is.null(cmp)) lines <- c(lines, "",
                                sprintf("Ratio of RRs (≥2 CS / 1 CS), Bayesian meta-regression: %.2f [95%% CrI %.2f, %.2f]; P(ratio > 1) = %.1f%%",
                                        cmp$ratio, cmp$lo, cmp$hi, 100 * cmp$P))
  save_docx("Subgroups_previous_CS", lines, out_main)
  list(res = res, cmp = cmp)
}

# ── [M] Sensibilidade por MCMC: modelo Poisson-normal (log-RR), verossimilhança exata ──
# y_ij ~ Poisson(n_ij · exp(α_i + (θ + δ_i)·x_ij)), x = ±0,5; δ_i = τ·z_i, z_i ~ N(0, 1)
# Priors iguais às da principal: θ ~ N(0, 2,7²), τ ~ HN(0,5); α_i ~ N(0, 10²).
# Sem correção de continuidade; estudos com zero eventos entram com sua verossimilhança.
# Modelo em Stan (rstan) compilado UMA vez e reamostrado para cada desfecho; a
# parametrização não centrada evita transições divergentes quando τ é pequeno.
mcmc_stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  array[N] int<lower=1, upper=K> study;
  vector[N] x;
  array[N] int<lower=0> y;
  vector[N] ln;
}
parameters {
  vector[K] alpha;
  real theta;
  real<lower=0> tau;
  vector[K] z;
}
model {
  alpha ~ normal(0, 10);
  theta ~ normal(0, 2.7);
  tau   ~ normal(0, 0.5);
  z     ~ std_normal();
  y ~ poisson_log(alpha[study] + (theta + tau * z[study]) .* x + ln);
}
"
.mcmc_cache <- new.env()
run_mcmc_sens <- function(dat, out_dir, sheet) {
  d <- dat[(dat$n1i > 0 & dat$n2i > 0), ]
  if (nrow(d) < k_min_bayes || sum(d$ai + d$ci) == 0) return(NULL)
  K  <- nrow(d)
  sd <- list(N = 2L * K, K = K, study = rep(seq_len(K), 2), x = c(rep(0.5, K), rep(-0.5, K)),
             y = as.integer(c(d$ai, d$ci)), ln = log(c(d$n1i, d$n2i)))
  if (is.null(.mcmc_cache$sm)) .mcmc_cache$sm <- rstan::stan_model(model_code = mcmc_stan_code)
  run_chains <- function(cores) rstan::sampling(
    .mcmc_cache$sm, data = sd, chains = mcmc_chains, iter = mcmc_iter, warmup = mcmc_warmup,
    thin = 1, seed = mcmc_seed, cores = cores, refresh = 0, pars = c("theta", "tau"),
    control = list(adapt_delta = 0.99, max_treedepth = 12))
  fit <- tryCatch(run_chains(min(mcmc_chains, parallel::detectCores())), error = function(e) NULL)
  # Se as cadeias em paralelo falharem (comum no RStudio/macOS), roda em série
  if (is.null(fit) || length(fit@sim) == 0 || fit@mode != 0) fit <- run_chains(1)
  if (length(fit@sim) == 0 || fit@mode != 0) stop("MCMC sem amostras posteriores")
  
  arr  <- posterior::as_draws_array(as.array(fit, pars = c("theta", "tau")))
  th   <- as.vector(posterior::extract_variable(arr, "theta"))
  tau  <- as.vector(posterior::extract_variable(arr, "tau"))
  rr   <- quantile(exp(th), c(0.5, 0.025, 0.975))
  summ <- posterior::summarise_draws(arr, "mean", "median", "sd", ~quantile(.x, c(0.025, 0.975)),
                                     "rhat", "ess_bulk", "ess_tail")
  sp   <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  ndiv <- sum(sapply(sp, function(m) sum(m[, "divergent__"])))
  
  # Diagnósticos
  dd <- file.path(out_dir, "MCMC_Diagnostics"); dir.create(dd, showWarnings = FALSE, recursive = TRUE)
  save_plot("MCMC_Trace", function() print(bayesplot::mcmc_trace(arr)), dd, width = 12, height = 6)
  save_plot("MCMC_Autocorrelation", function() print(bayesplot::mcmc_acf(arr, lags = 20)), dd, width = 12, height = 6)
  save_plot("MCMC_Posterior_Density", function() print(bayesplot::mcmc_dens_overlay(arr)), dd, width = 12, height = 6)
  save_plot("MCMC_Rhat", function() print(bayesplot::mcmc_rhat(setNames(summ$rhat, summ$variable))), dd, width = 8, height = 6)
  save_docx("MCMC_Diagnostics", c(
    paste0("MCMC sensitivity model — ", sheet),
    "Model: Bayesian Poisson-normal random-effects model on the log-RR scale (Stan via rstan, NUTS;",
    "non-centred parameterisation),",
    "exact likelihood, no continuity correction, studies with zero events retained.",
    "Priors: theta ~ N(0, 2.7^2); tau ~ Half-Normal(0.5); study baselines ~ N(0, 10^2).",
    sprintf("Sampling: %d chains x %d iterations (%d warm-up), no thinning; %d post-warm-up draws.",
            mcmc_chains, mcmc_iter, mcmc_warmup, mcmc_chains * (mcmc_iter - mcmc_warmup)),
    sprintf("adapt_delta = 0.99; max_treedepth = 12; divergent transitions = %d", ndiv),
    "", "theta = log-RR (TOLAC vs ERCS); tau = between-study SD",
    capture.output(print(as.data.frame(summ), digits = 3)),
    "", sprintf("RR (posterior median, 95%% equal-tailed CrI): %.2f [%.2f, %.2f]; P(RR > 1) = %.1f%%",
                rr[1], rr[2], rr[3], 100 * mean(th > 0)),
    "Convergence criteria: R-hat < 1.01 and bulk/tail ESS > 400 for all parameters."
  ), dd)
  
  data.frame(analysis = sprintf("Bayesian Poisson-normal model, MCMC (exact likelihood, no continuity correction; incl. zero-event studies) — max R-hat %.3f, min ESS %.0f, divergences %d",
                                max(summ$rhat), min(summ$ess_bulk, summ$ess_tail), ndiv),
             k = nrow(d), estimate = rr[[1]], method = "Bayesian MCMC (Stan)",
             RR_CI = sprintf("%.2f [%.2f, %.2f]", rr[1], rr[2], rr[3]),
             tau = round(median(tau), 2), P_RR_gt1 = round(mean(th > 0), 3))
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# §5 — ANÁLISE COMPLETA POR DESFECHO
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

run_bayesian_meta <- function(sheet, base_dir = output_dir) {
  safe <- gsub("[^A-Za-z0-9_ <>-]", "_", sheet)
  out_main <- file.path(base_dir, safe)
  out_meth <- file.path(out_main, "Supplementary_Methods")
  out_resu <- file.path(out_main, "Supplementary_Results")
  dir.create(out_meth, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_resu, showWarnings = FALSE, recursive = TRUE)
  
  dat_all <- read_outcome(sheet)
  excl    <- rule_for(main_exclude, sheet)
  sens_only <- if (length(excl)) in_list(dat_all$study, excl) else rep(FALSE, nrow(dat_all))
  dat     <- dat_all[!sens_only, ]
  es      <- make_es(dat)
  
  # Inventário (R1 pt 7–8): quem entrou, quem saiu e por quê
  inv <- dat_all[, c("study", "ai", "n1i", "ci", "n2i", "prior_cs", "term", "multiple", "special", "overlap")]
  inv$in_primary <- inv$study %in% es$study
  inv$reason_out <- ifelse(inv$in_primary, "",
                           ifelse(sens_only, "Sensitivity only (non-standard outcome definition)",
                                  ifelse(dat_all$double_zero, "Zero events in both arms", "Invalid effect size")))
  write.csv(inv, file.path(out_main, "Study_inventory.csv"), row.names = FALSE)
  
  if (nrow(es) < k_min_bayes)
    stop("Menos de ", k_min_bayes, " estudos válidos em '", sheet, "'.")
  
  fits <- lapply(priors, function(p) fit_bm(es, p))
  sm   <- lapply(fits, bm_sum)
  f0   <- fits$primary; s0 <- sm$primary
  
  # ── Forest, posteriores, probabilidades e risco absoluto (análise principal) ──
  prim <- report_primary(es, f0, out_main, sheet)
  pp <- prim$pp; ar <- prim$ar
  
  p_pred <- generate_posterior_plots(f0, "firebrick", is_predict = TRUE)
  p_s1   <- generate_posterior_plots(fits$sens1, "forestgreen")
  p_s2   <- generate_posterior_plots(fits$sens2, "blue")
  save_plot("Supp_Fig_Posterior_Predictive", function() print(p_pred$density + p_pred$probability), out_resu)
  save_plot("Supp_Fig_Sens_Prior_Distributions", function()
    print((p_s1$density + p_s1$probability) / (p_s2$density + p_s2$probability)), out_resu, height = 12)
  
  # ── Resumos dos modelos ──────────────────────────────────────────────────────
  save_docx("Model_Summaries", c(
    paste0("Outcome: ", sheet),
    "Model: Bayesian normal-normal hierarchical model (random effects), R package bayesmeta",
    "Posterior computed by numerical integration (DIRECT algorithm) — no MCMC sampling, hence",
    "no chains, trace plots, R-hat or ESS apply (Röver C. J Stat Softw 2020;93(6)).",
    "MCMC diagnostics refer to the Poisson-normal sensitivity model (Supplementary_Results/MCMC_Diagnostics).",
    "", unlist(lapply(names(fits), function(n) c(paste("===", priors[[n]]$name, "==="),
                                                 capture.output(print(fits[[n]]$summary)), "")))
  ), out_main)
  
  # ── Priors (Supplementary Methods) ───────────────────────────────────────────
  # BayesianPriorsTool (como no script original)
  pr_mu_vague  <- mu_dist_viz(mean = 0, sd = priors$primary$mu_sd, scale = "relative", xlabel = "Risk Ratio")
  pr_mu_weakly <- mu_dist_viz(mean = 0, sd = priors$sens2$mu_sd,   scale = "relative", xlabel = "Risk Ratio")
  pr_tau_weak  <- tau_dist_viz(dist = "half-normal", scale = priors$primary$tau)
  pr_tau_vague <- tau_dist_viz(dist = "half-normal", scale = priors$sens1$tau)
  save_plot("Priors_Primary",       function() print(pr_mu_vague  / pr_tau_weak),  out_meth, height = 12)
  save_plot("Priors_Sensitivity_1", function() print(pr_mu_vague  / pr_tau_vague), out_meth, height = 12)
  save_plot("Priors_Sensitivity_2", function() print(pr_mu_weakly / pr_tau_weak),  out_meth, height = 12)
  
  # ── Sensibilidade às priors ──────────────────────────────────────────────────
  prior_tab <- data.frame(
    analysis = sapply(priors, `[[`, "name"),
    RR_CrI   = sapply(sm, function(s) fmt_rr(s$mu, s$mu_lo, s$mu_hi)),
    PI       = sapply(sm, function(s) fmt_rr(s$th, s$th_lo, s$th_hi)),
    tau      = sapply(sm, tau_txt),
    P_RR_gt1 = sapply(fits, function(f) round(1 - f$pposterior(mu = 0), 3)))
  write.csv(prior_tab, file.path(out_resu, "Supp_Table_Sensitivity_Priors.csv"), row.names = FALSE)
  save_docx("Supp_Table_Sensitivity_Priors", c(
    paste0("Prior sensitivity — ", sheet),
    sprintf("%-50s | %-22s | %-22s | %s", "Analysis", "RR [95% CrI]", "95% PI", "P(RR>1)"),
    paste(rep("-", 112), collapse = ""),
    sprintf("%-50s | %-22s | %-22s | %5.1f%%", prior_tab$analysis, prior_tab$RR_CrI, prior_tab$PI,
            100 * prior_tab$P_RR_gt1)), out_resu)
  
  # ── [G] e [H] ────────────────────────────────────────────────────────────────
  mcmc_row <- if (run_mcmc) tryCatch(run_mcmc_sens(dat, out_resu, sheet),
                                     error = function(e) { log_msg(paste("MCMC —", sheet), e$message); NULL }) else NULL
  sub  <- tryCatch(run_cs_subgroups(dat, out_main, sheet),
                   error = function(e) { log_msg(paste("Subgroups —", sheet), e$message); NULL })
  sens <- tryCatch(run_data_sensitivity(dat, dat_all, out_resu, sheet, mcmc_row),
                   error = function(e) { log_msg(paste("Sensitivity —", sheet), e$message); NULL })
  if (nrow(es) >= k_min_loo)
    tryCatch(run_loo(es, f0, out_resu, sheet),
             error = function(e) log_msg(paste("LOO —", sheet), e$message))
  smallstudy_P <- tryCatch(run_funnel(es, f0, out_resu, sheet),
                           error = function(e) { log_msg(paste("Funnel —", sheet), e$message); NA })
  
  # Linha da tabela-resumo geral [J]
  data.frame(
    outcome = sheet, k = nrow(es), k_total = nrow(dat_all),
    k_sensitivity_only = sum(sens_only),
    k_double_zero_removed = sum(dat$double_zero) * drop_double_zero,
    events_TOLAC = sprintf("%d/%d", sum(es$ai), sum(es$n1i)),
    events_ERCS  = sprintf("%d/%d", sum(es$ci), sum(es$n2i)),
    RR_95CrI     = fmt_rr(s0$mu, s0$mu_lo, s0$mu_hi),
    PI_95        = fmt_rr(s0$th, s0$th_lo, s0$th_hi),
    tau          = sprintf("%.2f [%.2f, %.2f]", s0$tau, s0$tau_lo, s0$tau_hi),
    P_RR_gt1     = round(pp$P_mu_gt[pp$threshold == 1], 3),
    P_RR_gt1.25  = round(pp$P_mu_gt[pp$threshold == 1.25], 3),
    P_RR_gt2     = round(pp$P_mu_gt[pp$threshold == 2], 3),
    baseline_ERCS_per1000 = round(ar$p0_per1000, 2),
    ARD_per1000_95CrI = sprintf("%+.2f [%+.2f, %+.2f]", ar$ard, ar$ard_lo, ar$ard_hi),
    P_smallstudy_slope_gt0 = round(smallstudy_P, 3),
    RR_MCMC_PoissonNormal = if (is.null(mcmc_row)) NA else mcmc_row$RR_CI,
    RR_1CS  = if (is.null(sub$res[["1 previous CS"]])) NA else with(sub$res[["1 previous CS"]]$s, fmt_rr(mu, mu_lo, mu_hi)),
    RR_2plusCS = if (is.null(sub$res[["2+ previous CS"]])) NA else with(sub$res[["2+ previous CS"]]$s, fmt_rr(mu, mu_lo, mu_hi)),
    ratio_2plus_vs_1CS = if (is.null(sub$cmp)) NA else sprintf("%.2f [%.2f, %.2f]", sub$cmp$ratio, sub$cmp$lo, sub$cmp$hi),
    stringsAsFactors = FALSE
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# ► EXECUTAR
# ══════════════════════════════════════════════════════════════════════════════

# Planilha lida sem login: compartilhe como "Qualquer pessoa com o link pode ver"
if (is.null(local_csv_dir)) googlesheets4::gs4_deauth()
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

summary_rows <- list()
for (aba in list_outcomes()) {
  message("▶ ", aba)
  summary_rows[[aba]] <- tryCatch(
    run_bayesian_meta(aba),
    error = function(e) { log_msg(aba, e$message); message("  ✖ ", e$message); NULL })
}
summary_all <- do.call(rbind, summary_rows)
write.csv(summary_all, file.path(output_dir, "Summary_all_outcomes.csv"), row.names = FALSE)
save_docx("Summary_all_outcomes", c(
  paste0("Summary of findings (", label_e, " vs ", label_c, ") — Bayesian NNHM, median [95% CrI]"),
  paste(rep("-", 150), collapse = ""),
  sprintf("%-34s | %3s | %-22s | %-22s | %-18s | %7s | %7s | %8s | %s",
          "Outcome", "k", "RR [95% CrI]", "95% PI", "tau", "P(RR>1)", "P(RR>2)", "ERCS/1000", "ARD/1000 [95% CrI]"),
  paste(rep("-", 150), collapse = ""),
  sprintf("%-34s | %3d | %-22s | %-22s | %-18s | %6.1f%% | %6.1f%% | %8.2f | %s",
          summary_all$outcome, summary_all$k, summary_all$RR_95CrI, summary_all$PI_95, summary_all$tau,
          100 * summary_all$P_RR_gt1, 100 * summary_all$P_RR_gt2,
          summary_all$baseline_ERCS_per1000, summary_all$ARD_per1000_95CrI)
), output_dir)
message("Concluído. Resultados em: ", output_dir)
