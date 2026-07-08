# ==============================================================================
# PROJETO : Análise Epidemiológica Nacional do AVC no Brasil (2015–2025)
# OBJETIVO: Impacto da distância geográfica na letalidade e acesso à trombólise
# ARQUITETURA: Checkpoints em Parquet por semestre × UF para controle de RAM
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DEPENDÊNCIAS
# ------------------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(
  dplyr, stringr, arrow, purrr, sf, geobr,
  sandwich, lmtest, splines,
  gtsummary, ggplot2, broom, ragg, tidyr, forcats, flextable, labelled, officer
)

p_load_gh("rfsaldanha/microdatasus")

# ------------------------------------------------------------------------------
# 2. PARÂMETROS DA COORTE
# ------------------------------------------------------------------------------
anos     <- 2015:2025
cids_avc <- c("I60", "I61", "I62", "I63", "I64")

dir_parquet <- "/Users/davibarbosa/Desktop/TCC Nat/checkpoints_avc"
dir_out     <- "/Users/davibarbosa/Desktop/TCC Nat"

if (!dir.exists(dir_parquet)) dir.create(dir_parquet, recursive = TRUE)

# ------------------------------------------------------------------------------
# 3. FUNÇÃO DE PROCESSAMENTO POR SEMESTRE (BATCH POR UF)
#
# Granularidade: um checkpoint por semestre (S1 = jan–jun, S2 = jul–dez).
# Dentro de cada semestre, itera UF por UF — a RAM nunca carrega mais do que
# os dados brutos de um único estado por vez. Filtragem ocorre imediatamente
# após o download, antes de qualquer acumulação em memória.
# ------------------------------------------------------------------------------
processar_sih_semestre <- function(ano, semestre) {
  
  mes_ini <- if (semestre == 1L) 1L else 7L
  mes_fim <- if (semestre == 1L) 6L else 12L
  
  arquivo_out <- file.path(
    dir_parquet, sprintf("avc_%d_S%d.parquet", ano, semestre)
  )
  
  if (file.exists(arquivo_out)) {
    message(sprintf("[%d S%d] Checkpoint existente — pulando.", ano, semestre))
    return(invisible(NULL))
  }
  
  message(sprintf("[%d S%d] Iniciando (%02d/%d → %02d/%d).",
                  ano, semestre, mes_ini, ano, mes_fim, ano))
  
  ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
           "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
           "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
  processar_uf <- function(uf) {
    
    message(sprintf("  [%d S%d | %s] Baixando...", ano, semestre, uf))
    
    df_uf <- tryCatch(
      fetch_datasus(
        year_start = ano, month_start = mes_ini,
        year_end   = ano, month_end   = mes_fim,
        uf         = uf,
        information_system = "SIH-RD"
      ),
      error = function(e) {
        message(sprintf("  [%d S%d | %s] ERRO: %s", ano, semestre, uf, e$message))
        NULL
      }
    )
    
    if (is.null(df_uf) || nrow(df_uf) == 0) { gc(); return(NULL) }
    
    # ── A. Filtros de inclusão ────────────────────────────────────────────────
    df_uf <- filter(df_uf, CAR_INT %in% c("02", "05"))
    df_uf <- filter(df_uf, COD_IDADE == "4", as.numeric(IDADE) >= 18)
    df_uf <- df_uf |>
      mutate(DIAG_LIMPO = gsub("\\.", "", toupper(DIAG_PRINC))) |>
      filter(str_detect(DIAG_LIMPO, paste(cids_avc, collapse = "|")))
    
    n_avc <- nrow(df_uf)
    message(sprintf("  [%d S%d | %s] %d registros AVC.", ano, semestre, uf, n_avc))
    if (n_avc == 0) { gc(); return(NULL) }
    
    # ── B. Diagnósticos secundários ──────────────────────────────────────────
    cols_diagsec <- grep("^DIAG_SECUN$|^DIAGSEC[0-9]+$", names(df_uf), value = TRUE)
    
    df_uf$diag_concat <- if (length(cols_diagsec) > 0) {
      apply(df_uf[, cols_diagsec, drop = FALSE], 1,
            function(x) paste(x[!is.na(x) & x != ""], collapse = " "))
    } else ""
    
    # ── C. Engenharia de variáveis e seleção ─────────────────────────────────
    df_limpo <- df_uf |>
      mutate(
        ANO      = as.integer(ano),
        SEMESTRE = as.integer(semestre),
        
        flag_hipertensao = str_detect(diag_concat, "I1[0-5]"),
        flag_diabetes    = str_detect(diag_concat, "E1[0-4]"),
        flag_fib_atrial  = str_detect(diag_concat, "I48"),
        flag_renal_cron  = str_detect(diag_concat, "N18"),
        flag_insuf_card  = str_detect(diag_concat, "I50"),
        
        # Portaria GM/MS 665/2012
        fez_trombolise   = as.integer(PROC_REA %in% c("0303040233", "0303040300")),
        fez_trombectomia = as.integer(PROC_REA == "0403070171"),
        obito_hospitalar = as.integer(MORTE == "1"),
        
        coorte_avc = case_when(
          str_detect(DIAG_LIMPO, "^I63")     ~ "isquemico",
          str_detect(DIAG_LIMPO, "^I6[012]") ~ "hemorragico",
          str_detect(DIAG_LIMPO, "^I64")     ~ "nao_especificado",
          TRUE                               ~ NA_character_
        )
      ) |>
      select(
        N_AIH, ANO, SEMESTRE, CNES,
        MUNIC_RES, MUNIC_MOV,
        IDADE, SEXO, RACA_COR,
        DIAG_PRINC, coorte_avc,
        starts_with("flag_"),
        fez_trombolise, fez_trombectomia, obito_hospitalar,
        DIAS_PERM
      )
    
    rm(df_uf); gc()
    df_limpo
  }
  
  lista_ufs <- map(ufs, processar_uf)
  lista_ufs <- Filter(Negate(is.null), lista_ufs)
  
  if (length(lista_ufs) == 0) {
    message(sprintf("[%d S%d] Nenhum registro AVC encontrado.", ano, semestre))
    return(invisible(NULL))
  }
  
  df_sem <- bind_rows(lista_ufs)
  rm(lista_ufs); gc()
  
  write_parquet(df_sem, arquivo_out)
  message(sprintf("[%d S%d] Checkpoint salvo: %d registros.", ano, semestre, nrow(df_sem)))
  rm(df_sem); gc()
}

# ------------------------------------------------------------------------------
# 4. CÁLCULO DE DISTÂNCIAS GEODÉSICAS
# SIRGAS 2000 (EPSG:4674): st_distance calcula distância geodésica em metros.
# by_element = TRUE: linha-a-linha sem gerar matriz N×N (evita alocação destrutiva).
# ------------------------------------------------------------------------------
calcular_distancias <- function(df) {
  
  message("Carregando centroides municipais (geobr)...")
  
  municipios_sf <- suppressMessages(
    read_municipality(code_muni = "all", year = 2020, showProgress = FALSE)
  )
  
  centroides <- municipios_sf |>
    mutate(
      cod6      = as.character(substr(code_muni, 1, 6)),
      centroide = st_centroid(geom)
    ) |>
    as_tibble() |>
    select(cod6, centroide)
  
  df <- df |>
    mutate(
      cod_res = str_pad(as.character(MUNIC_RES), 6, "left", "0"),
      cod_mov = str_pad(as.character(MUNIC_MOV), 6, "left", "0")
    ) |>
    left_join(rename(centroides, c_res = centroide), by = c("cod_res" = "cod6")) |>
    left_join(rename(centroides, c_mov = centroide), by = c("cod_mov" = "cod6"))
  
  idx <- !is.na(df$c_res) & !is.na(df$c_mov)
  df$distancia_km <- NA_real_
  
  if (any(idx)) {
    pts_res <- st_sfc(df$c_res[idx], crs = 4674)
    pts_mov <- st_sfc(df$c_mov[idx], crs = 4674)
    df$distancia_km[idx] <- as.numeric(
      st_distance(pts_res, pts_mov, by_element = TRUE)
    ) / 1000
  }
  
  n_missing <- sum(!idx)
  if (n_missing > 0)
    message(sprintf("Aviso: %d registros sem distância (município ausente no shapefile).", n_missing))
  
  select(df, -c_res, -c_mov, -cod_res, -cod_mov)
}

# ------------------------------------------------------------------------------
# 5. CONSOLIDAÇÃO
# ------------------------------------------------------------------------------
consolidar_parquets <- function() {
  
  caminho_final <- file.path(dir_out, "df_final_avc.parquet")
  if (file.exists(caminho_final)) {
    message("Dataset final encontrado — carregando do cache.")
    return(read_parquet(caminho_final))
  }
  
  arquivos <- list.files(dir_parquet, pattern = "\\.parquet$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum checkpoint encontrado em '", dir_parquet, "'.")
  
  message("Consolidando ", length(arquivos), " arquivos...")
  df <- open_dataset(arquivos) |> collect()
  message("Total consolidado: ", nrow(df), " registros.")
  
  df <- calcular_distancias(df)
  write_parquet(df, caminho_final)
  message("Dataset final salvo: ", caminho_final)
  df
}

# ------------------------------------------------------------------------------
# 6. MODELOS ESTATÍSTICOS
#
# RACA_COR "99" e NA recodificados como "Ignorado" (MNAR — não excluídos).
# ESTRATIFICAÇÃO POR COORTE: modelos separados por subtipo.
# CLUSTERING: EP robustos clusterizados por CNES (sandwich).
# fez_trombolise AUSENTE dos modelos de mortalidade (mediador no caminho causal).
# ------------------------------------------------------------------------------
ajustar_modelos <- function(df) {
  
  df_mod <- df |>
    filter(!is.na(distancia_km)) |>
    mutate(
      IDADE_NUM  = as.numeric(IDADE),
      ANO_F      = factor(ANO),
      SEXO_F     = factor(SEXO),
      CNES_F     = factor(CNES),
      RACA_COR_F = factor(
        ifelse(is.na(RACA_COR) | RACA_COR == "99", "Ignorado", RACA_COR)
      )
    )
  
  # environment(fit$terms) <- environment(): solução definitiva para o escopo
  # de vcovCL — sem isso, model.frame(fit) não encontra 'dados' e gera erro.
  ajustar_glm_cluster <- function(formula, dados) {
    fit <- glm(formula, data = dados, family = binomial(link = "logit"))
    environment(fit$terms) <- environment()
    tbl <- coeftest(fit, vcov = vcovCL(fit, cluster = dados$CNES_F))
    list(fit = fit, tbl = tbl)
  }
  
  df_isq  <- filter(df_mod, coorte_avc == "isquemico")
  df_hem  <- filter(df_mod, coorte_avc == "hemorragico")
  df_ndef <- filter(df_mod, coorte_avc == "nao_especificado")
  
  # Modelo 1: Trombólise — apenas isquêmico (I63); contraindicada nas demais CIDs
  m1 <- ajustar_glm_cluster(
    fez_trombolise ~ distancia_km + IDADE_NUM + SEXO_F + RACA_COR_F +
      flag_hipertensao + flag_diabetes + flag_fib_atrial + ANO_F,
    df_isq
  )
  
  # Modelos 2a/2b/2c: Mortalidade por coorte (efeito total)
  formula_obito <- obito_hospitalar ~
    distancia_km + IDADE_NUM + SEXO_F + RACA_COR_F +
    flag_hipertensao + flag_diabetes + flag_fib_atrial +
    flag_renal_cron  + flag_insuf_card + ANO_F
  
  list(
    m1_trombolise        = m1,
    m2_obito_isquemico   = ajustar_glm_cluster(formula_obito, df_isq),
    m2_obito_hemorragico = ajustar_glm_cluster(formula_obito, df_hem),
    m2_obito_nao_espec   = ajustar_glm_cluster(formula_obito, df_ndef)
  )
}

# ------------------------------------------------------------------------------
# 7. PIPELINE DE EXECUÇÃO
# ------------------------------------------------------------------------------
grade <- expand.grid(ano = anos, semestre = 1:2) |> arrange(ano, semestre)
walk2(grade$ano, grade$semestre, processar_sih_semestre)

df_final <- consolidar_parquets()

# Verificação: sobreposição entre trombólise e trombectomia
cat("\nVerificação — sobreposição trombólise & trombectomia:\n")
print(with(df_final, table(fez_trombolise, fez_trombectomia)))

resultados <- ajustar_modelos(df_final)

print(resultados$m1_trombolise$tbl)
print(resultados$m2_obito_isquemico$tbl)
print(resultados$m2_obito_hemorragico$tbl)
print(resultados$m2_obito_nao_espec$tbl)

# ==============================================================================
# FASE 3: COMUNICAÇÃO VISUAL E TABELAS CLÍNICAS — PT (ABNT) + EN
# ==============================================================================

dir_pt <- file.path(dir_out, "PT_ABNT")
dir_en <- file.path(dir_out, "EN")
dir.create(dir_pt, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_en, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# FUNÇÃO DE SALVAMENTO
# PNG: ggsave + ragg::agg_png (funciona corretamente).
# TIFF: ragg::agg_tiff direto com dimensões em pixels — ggsave com lambda
#       falha silenciosamente ao abrir o device sem gravar conteúdo.
# ------------------------------------------------------------------------------
salvar_figura <- function(plot_obj, nome_base, dir,
                          width_in = 16, height_in = 10, res = 300) {
  
  path_png  <- file.path(dir, paste0(nome_base, ".png"))
  path_tiff <- file.path(dir, paste0(nome_base, ".tiff"))
  
  ggsave(
    filename  = path_png,
    plot      = plot_obj,
    device    = ragg::agg_png,
    dpi       = res,
    width     = width_in, height = height_in, units = "in",
    bg        = "white",  limitsize = FALSE
  )
  
  # TIFF direto: ragg::agg_tiff() recebe pixels (não polegadas)
  ragg::agg_tiff(
    filename    = path_tiff,
    width       = round(width_in  * res),
    height      = round(height_in * res),
    units       = "px",
    res         = res,
    background  = "white",
    compression = "lzw"
  )
  print(plot_obj)
  dev.off()
}

# ABNT NBR 14724:2011: identificação (Figura X —) no topo; Nota e Fonte abaixo.
tema_abnt <- theme_minimal(base_size = 13) +
  theme(
    legend.position       = "bottom",
    panel.grid.minor      = element_blank(),
    plot.title            = element_text(hjust = 0.5, face = "bold", size = 12,
                                         margin = margin(b = 6)),
    plot.subtitle         = element_blank(),
    plot.caption          = element_text(hjust = 0, size = 9, color = "black",
                                         margin = margin(t = 8)),
    plot.caption.position = "plot"
  )

tema_en <- theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey40", size = 11),
    plot.caption     = element_text(color = "grey50", size = 9, hjust = 0,
                                    margin = margin(t = 8))
  )

# ------------------------------------------------------------------------------
# PILAR 1: TABELA 1
# ------------------------------------------------------------------------------
construir_tabela1 <- function(lang = "pt") {
  
  df_tab <- df_final |>
    select(coorte_avc, IDADE, SEXO, RACA_COR, distancia_km,
           flag_hipertensao, flag_diabetes, flag_fib_atrial,
           flag_renal_cron, flag_insuf_card,
           fez_trombolise, fez_trombectomia, obito_hospitalar, DIAS_PERM) |>
    mutate(IDADE = as.numeric(IDADE))
  
  if (lang == "pt") {
    df_tab <- df_tab |>
      mutate(
        SEXO = case_when(
          SEXO == "1" ~ "Masculino",
          SEXO == "3" ~ "Feminino",
          TRUE        ~ NA_character_
        ),
        RACA_COR = case_when(
          RACA_COR == "01" ~ "Branca",
          RACA_COR == "02" ~ "Preta",
          RACA_COR == "03" ~ "Parda",
          RACA_COR == "04" ~ "Amarela",
          RACA_COR == "05" ~ "Indígena",
          TRUE             ~ "Ignorado/Sem Info"
        ),
        coorte_avc = case_when(
          coorte_avc == "isquemico"        ~ "Isquêmico",
          coorte_avc == "hemorragico"      ~ "Hemorrágico",
          coorte_avc == "nao_especificado" ~ "Não Especificado",
          TRUE ~ coorte_avc
        )
      )
    labels_list <- list(
      IDADE            ~ "Idade (anos)",
      SEXO             ~ "Sexo",
      RACA_COR         ~ "Raça/Cor (autodeclarada)",
      distancia_km     ~ "Distância Município de Residência → Hospital (km)",
      flag_hipertensao ~ "Hipertensão Arterial",
      flag_diabetes    ~ "Diabetes Mellitus",
      flag_fib_atrial  ~ "Fibrilação Atrial",
      flag_renal_cron  ~ "Doença Renal Crônica",
      flag_insuf_card  ~ "Insuficiência Cardíaca",
      fez_trombolise   ~ "Terapia Trombolítica (Alteplase IV)",
      fez_trombectomia ~ "Trombectomia Mecânica (SIGTAP 0403070171)",
      obito_hospitalar ~ "Óbito Intra-hospitalar",
      DIAS_PERM        ~ "Tempo de Internação (dias)"
    )
    caption <- paste0(
      "**Tabela 1** — Características clínicas e demográficas dos pacientes internados ",
      "por AVC segundo subtipo etiológico. Brasil, 2015–2025."
    )
    col_label <- "**Subtipo de AVC**"
    miss_text <- "Ignorado/Ausente"
  } else {
    df_tab <- df_tab |>
      mutate(
        SEXO = case_when(
          SEXO == "1" ~ "Male",
          SEXO == "3" ~ "Female",
          TRUE        ~ NA_character_
        ),
        RACA_COR = case_when(
          RACA_COR == "01" ~ "White (Branca)",
          RACA_COR == "02" ~ "Black (Preta)",
          RACA_COR == "03" ~ "Mixed/Brown (Parda)",
          RACA_COR == "04" ~ "Asian/Yellow (Amarela)",
          RACA_COR == "05" ~ "Indigenous (Indígena)",
          TRUE             ~ "Unknown/Unreported"
        ),
        coorte_avc = case_when(
          coorte_avc == "isquemico"        ~ "Ischemic",
          coorte_avc == "hemorragico"      ~ "Hemorrhagic",
          coorte_avc == "nao_especificado" ~ "Unspecified",
          TRUE ~ coorte_avc
        )
      )
    labels_list <- list(
      IDADE            ~ "Age (years)",
      SEXO             ~ "Sex",
      RACA_COR         ~ "Race/Ethnicity (self-reported)",
      distancia_km     ~ "Distance from municipality of residence to hospital (km)",
      flag_hipertensao ~ "Arterial Hypertension",
      flag_diabetes    ~ "Diabetes Mellitus",
      flag_fib_atrial  ~ "Atrial Fibrillation",
      flag_renal_cron  ~ "Chronic Kidney Disease",
      flag_insuf_card  ~ "Heart Failure",
      fez_trombolise   ~ "IV Thrombolysis (Alteplase)",
      fez_trombectomia ~ "Mechanical Thrombectomy (SIGTAP 0403070171)",
      obito_hospitalar ~ "In-hospital Mortality",
      DIAS_PERM        ~ "Length of Stay (days)"
    )
    caption <- paste0(
      "**Table 1** — Clinical and demographic characteristics of stroke admissions ",
      "by etiological subtype. Brazil, 2015–2025."
    )
    col_label <- "**Stroke Subtype**"
    miss_text <- "Unknown/Missing"
  }
  
  # Configuração estrutural: Força o documento Word a abrir em modo Paisagem
  sect_properties <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape")
  )
  
  tbl_flex <- df_tab |>
    # O Pulo do Gato: Passar os labels DENTRO do tbl_summary 
    tbl_summary(
      by           = coorte_avc,
      label        = labels_list, 
      statistic    = list(all_continuous()  ~ "{median} ({p25}–{p75})",
                          all_categorical() ~ "{n} ({p}%)"),
      missing      = "ifany",
      missing_text = miss_text
    ) |>
    add_overall() |>
    bold_labels() |>
    modify_caption(caption) |>
    modify_spanning_header(all_stat_cols() ~ col_label) |>
    as_flex_table() |>
    flextable::autofit() # Expande as colunas respeitando a tela em paisagem
  
  # Exportação vinculada à configuração de página
  flextable::save_as_docx(
    tbl_flex,
    path = file.path(if (lang == "pt") dir_pt else dir_en, "Tabela1_Baseline.docx"),
    pr_section = sect_properties
  )
}

construir_tabela1("pt")
construir_tabela1("en")

# ------------------------------------------------------------------------------
# PILAR 2: FOREST PLOT — Mortalidade hospitalar
# ------------------------------------------------------------------------------
extrair_or_cluster <- function(resultado, nome_coorte) {
  tbl <- resultado$tbl
  tibble(
    term       = rownames(tbl),
    estimate   = exp(tbl[, "Estimate"]),
    conf.low   = exp(tbl[, "Estimate"] - 1.96 * tbl[, "Std. Error"]),
    conf.high  = exp(tbl[, "Estimate"] + 1.96 * tbl[, "Std. Error"]),
    p.value    = tbl[, "Pr(>|z|)"],
    coorte_raw = nome_coorte
  ) |>
    filter(term != "(Intercept)", !grepl("^ANO_F", term))
}

df_forest_base <- bind_rows(
  extrair_or_cluster(resultados$m2_obito_isquemico,   "isquemico"),
  extrair_or_cluster(resultados$m2_obito_hemorragico, "hemorragico"),
  extrair_or_cluster(resultados$m2_obito_nao_espec,   "nao_especificado")
)

construir_forest <- function(lang = "pt") {
  
  if (lang == "pt") {
    df <- df_forest_base |>
      mutate(
        coorte = case_when(
          coorte_raw == "isquemico"        ~ "Isquêmico",
          coorte_raw == "hemorragico"      ~ "Hemorrágico",
          coorte_raw == "nao_especificado" ~ "Não Especificado"
        ),
        label = case_when(
          term == "distancia_km"         ~ "Distância ao hospital (km)",
          term == "IDADE_NUM"            ~ "Idade (anos)",
          term == "SEXO_F3"              ~ "Sexo Feminino",
          term == "RACA_COR_F02"         ~ "Raça: Preta",
          term == "RACA_COR_F03"         ~ "Raça: Parda",
          term == "RACA_COR_F04"         ~ "Raça: Amarela",
          term == "RACA_COR_F05"         ~ "Raça: Indígena",
          term == "RACA_COR_FIgnorado"   ~ "Raça: Ignorada",
          term == "flag_hipertensaoTRUE" ~ "Hipertensão Arterial",
          term == "flag_diabetesTRUE"    ~ "Diabetes Mellitus",
          term == "flag_fib_atrialTRUE"  ~ "Fibrilação Atrial",
          term == "flag_renal_cronTRUE"  ~ "Doença Renal Crônica",
          term == "flag_insuf_cardTRUE"  ~ "Insuficiência Cardíaca",
          TRUE ~ term
        ),
        significante = if_else(p.value < 0.05, "p < 0,05", "p ≥ 0,05"),
        coorte = factor(coorte, levels = c("Isquêmico", "Hemorrágico", "Não Especificado"))
      )
    cores     <- c("Isquêmico" = "#2166ac", "Hemorrágico" = "#d6604d", "Não Especificado" = "#4dac26")
    sig_shape <- c("p < 0,05" = 16, "p ≥ 0,05" = 1)
    x_labels  <- c("0,25", "0,50", "1,00", "2,00", "4,00", "8,00")
    labs_cfg  <- labs(
      title   = "Figura 1 — Preditores de óbito intra-hospitalar por AVC. Brasil, 2015–2025.",
      x       = "Odds Ratio (escala logarítmica)", y = NULL,
      color   = "Subtipo de AVC", shape = "Significância",
      caption = paste0(
        "Nota: IC 95% de Wald com erros-padrão robustos clusterizados por estabelecimento de saúde ",
        "[Cadastro Nacional de Estabelecimentos de Saúde (CNES)], estimador sanduíche. ",
        "Efeitos de ano omitidos do gráfico.\n",
        "Fonte: Sistema de Informação Hospitalar — Reduzida (SIH-RD)/DATASUS, 2015–2025."
      )
    )
    tema <- tema_abnt
  } else {
    df <- df_forest_base |>
      mutate(
        coorte = case_when(
          coorte_raw == "isquemico"        ~ "Ischemic",
          coorte_raw == "hemorragico"      ~ "Hemorrhagic",
          coorte_raw == "nao_especificado" ~ "Unspecified"
        ),
        label = case_when(
          term == "distancia_km"         ~ "Distance to hospital (km)",
          term == "IDADE_NUM"            ~ "Age (years)",
          term == "SEXO_F3"              ~ "Female Sex",
          term == "RACA_COR_F02"         ~ "Race: Black (Preta)",
          term == "RACA_COR_F03"         ~ "Race: Mixed/Brown (Parda)",
          term == "RACA_COR_F04"         ~ "Race: Asian/Yellow (Amarela)",
          term == "RACA_COR_F05"         ~ "Race: Indigenous (Indígena)",
          term == "RACA_COR_FIgnorado"   ~ "Race: Unknown/Unreported",
          term == "flag_hipertensaoTRUE" ~ "Arterial Hypertension",
          term == "flag_diabetesTRUE"    ~ "Diabetes Mellitus",
          term == "flag_fib_atrialTRUE"  ~ "Atrial Fibrillation",
          term == "flag_renal_cronTRUE"  ~ "Chronic Kidney Disease",
          term == "flag_insuf_cardTRUE"  ~ "Heart Failure",
          TRUE ~ term
        ),
        significante = if_else(p.value < 0.05, "p < 0.05", "p ≥ 0.05"),
        coorte = factor(coorte, levels = c("Ischemic", "Hemorrhagic", "Unspecified"))
      )
    cores     <- c("Ischemic" = "#2166ac", "Hemorrhagic" = "#d6604d", "Unspecified" = "#4dac26")
    sig_shape <- c("p < 0.05" = 16, "p ≥ 0.05" = 1)
    x_labels  <- c("0.25", "0.50", "1.00", "2.00", "4.00", "8.00")
    labs_cfg  <- labs(
      title    = "Predictors of In-hospital Mortality from Stroke — Brazil, 2015–2025",
      subtitle = "Adjusted Odds Ratios with cluster-robust standard errors by health facility",
      x = "Odds Ratio (log scale)", y = NULL,
      color = "Stroke Subtype", shape = "Significance",
      caption = paste0(
        "95% Wald CI; sandwich variance estimator clustered by health facility ",
        "[CNES — Brazilian National Health Facility Registry]. Year fixed effects excluded.\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025."
      )
    )
    tema <- tema_en
  }
  
  df <- df |> mutate(label = fct_reorder(label, estimate, .fun = median))
  
  ggplot(df, aes(x = estimate, y = label, color = coorte, shape = significante)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6),
                    linewidth = 0.6, size = 0.5) +
    scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8), labels = x_labels) +
    scale_shape_manual(values = sig_shape) +
    scale_color_manual(values = cores) +
    labs_cfg + tema +
    theme(axis.text.y = element_text(size = 11))
}

salvar_figura(construir_forest("pt"), "ForestPlot_Letalidade", dir_pt, height_in = 12)
salvar_figura(construir_forest("en"), "ForestPlot_Mortality",  dir_en, height_in = 12)

# ------------------------------------------------------------------------------
# PILAR 3: MAPA
# coord_sf com xlim/ylim exclui ilhas remotas que expandem o bbox do shapefile.
# ------------------------------------------------------------------------------
mapa_dados <- df_final |>
  group_by(MUNIC_RES) |>
  summarise(
    total_avc           = n(),
    tx_nao_especificado = mean(coorte_avc == "nao_especificado", na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  filter(total_avc >= 50) |>
  mutate(code_muni = str_pad(as.character(MUNIC_RES), 6, "left", "0"))

obter_malha <- function(tipo) {
  arquivo_cache <- file.path(dir_out, paste0("malha_", tipo, "_2020.rds"))
  if (file.exists(arquivo_cache)) return(readRDS(arquivo_cache))
  malha <- tryCatch(
    suppressMessages(
      if (tipo == "municipios")
        geobr::read_municipality(code_muni = "all", year = 2020, showProgress = FALSE)
      else
        geobr::read_state(code_state = "all", year = 2020, showProgress = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(malha)) stop("Servidor do IPEA indisponível. Tente mais tarde.")
  saveRDS(malha, arquivo_cache)
  malha
}

municipios_br <- obter_malha("municipios")
estados_br    <- obter_malha("estados")

mapa_sf <- municipios_br |>
  mutate(code_muni_6 = substr(as.character(code_muni), 1, 6)) |>
  left_join(mapa_dados, by = c("code_muni_6" = "code_muni"))

construir_mapa <- function(lang = "pt") {
  
  if (lang == "pt") {
    fill_name <- "AVC sem subtipagem (%)"
    labs_cfg  <- labs(
      title   = "Figura 2 — Proporção de AVC sem subtipagem etiológica (CID-10: I64)\npor município. Brasil, 2015–2025.",
      caption = paste0(
        "Nota: municípios com < 50 internações por AVC na década excluídos por instabilidade da estimativa. ",
        "Proxy de acesso à tomografia computadorizada de crânio.\n",
        "Fonte: SIH-RD/DATASUS, 2015–2025. Limites: IBGE/geobr 2020 (SIRGAS 2000)."
      )
    )
  } else {
    fill_name <- "Unspecified Stroke (%)"
    labs_cfg  <- labs(
      title    = "Proportion of Unspecified Stroke Diagnosis (ICD-10: I64) by Municipality",
      subtitle = "Brazil, 2015–2025 | Municipalities with ≥ 50 stroke admissions over the decade",
      caption  = paste0(
        "Proxy for access to brain CT imaging. ",
        "Gray: municipalities with < 50 stroke admissions over the study period (estimate suppressed).\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025. ",
        "Boundaries: IBGE/geobr 2020 (SIRGAS 2000)."
      )
    )
  }
  
  ggplot() +
    geom_sf(data = mapa_sf, aes(fill = tx_nao_especificado), color = NA) +
    geom_sf(data = estados_br, fill = NA, color = "white", linewidth = 0.3) +
    coord_sf(xlim = c(-74, -28), ylim = c(-34, 6), expand = FALSE) +
    scale_fill_viridis_c(
      option    = "magma", direction = -1, na.value = "grey88",
      name      = fill_name, limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)
    ) +
    labs_cfg +
    theme_void(base_size = 13) +
    theme(
      legend.position  = "bottom",
      legend.key.width = unit(2.5, "cm"),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 12,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(hjust = 0.5, color = "grey40", size = 10),
      plot.caption     = element_text(hjust = 0.5, size = 9, color = "black",
                                      margin = margin(t = 8))
    )
}

salvar_figura(construir_mapa("pt"), "Mapa_Deserto_Neuroimagem", dir_pt, width_in = 10, height_in = 13)
salvar_figura(construir_mapa("en"), "Map_Neuroimaging_Desert",  dir_en, width_in = 10, height_in = 13)

# ------------------------------------------------------------------------------
# PILAR 4: TENDÊNCIA — Trombólise e Mortalidade (isquêmico)
# ------------------------------------------------------------------------------
tend_isq_raw <- df_final |>
  filter(coorte_avc == "isquemico") |>
  group_by(ANO) |>
  summarise(
    tx_trombolise = mean(fez_trombolise,   na.rm = TRUE) * 100,
    tx_obito      = mean(obito_hospitalar, na.rm = TRUE) * 100,
    .groups = "drop"
  )

construir_tend_trombolise <- function(lang = "pt") {
  
  if (lang == "pt") {
    df <- tend_isq_raw |>
      pivot_longer(c(tx_trombolise, tx_obito), names_to = "indicador", values_to = "taxa") |>
      mutate(indicador = case_when(
        indicador == "tx_trombolise" ~ "Taxa de Trombólise (%)",
        indicador == "tx_obito"      ~ "Letalidade Hospitalar (%)"
      ))
    cores    <- c("Taxa de Trombólise (%)" = "#2166ac", "Letalidade Hospitalar (%)" = "#d6604d")
    labs_cfg <- labs(
      title   = "Figura 3 — Evolução temporal da taxa de trombólise e da letalidade\nhospitalar no AVC isquêmico. Brasil, 2015–2025.",
      x       = "Ano", y = "Taxa (%)", color = NULL,
      caption = paste0(
        "Nota: taxas brutas, sem ajuste de case-mix. ",
        "Dados de 2025 sujeitos à defasagem de notificação do SIH-RD (6–18 meses).\n",
        "Fonte: SIH-RD/DATASUS, 2015–2025."
      )
    )
    tema <- tema_abnt
  } else {
    df <- tend_isq_raw |>
      pivot_longer(c(tx_trombolise, tx_obito), names_to = "indicador", values_to = "taxa") |>
      mutate(indicador = case_when(
        indicador == "tx_trombolise" ~ "Thrombolysis Rate (%)",
        indicador == "tx_obito"      ~ "In-hospital Mortality (%)"
      ))
    cores    <- c("Thrombolysis Rate (%)" = "#2166ac", "In-hospital Mortality (%)" = "#d6604d")
    labs_cfg <- labs(
      title    = "Trends in IV Thrombolysis Rate and In-hospital Mortality — Ischemic Stroke",
      subtitle = "Brazil, 2015–2025 | Hospital Information System (SIH-RD/DATASUS) | Unadjusted rates",
      x = "Year", y = "Rate (%)", color = NULL,
      caption  = paste0(
        "2025 data subject to SIH-RD administrative reporting lag (6–18 months).\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025."
      )
    )
    tema <- tema_en
  }
  
  ggplot(df, aes(x = ANO, y = taxa, color = indicador, group = indicador)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = 2015:2025) +
    scale_color_manual(values = cores) +
    labs_cfg + tema
}

salvar_figura(construir_tend_trombolise("pt"), "Tendencia_Trombolise_Isquemico", dir_pt)
salvar_figura(construir_tend_trombolise("en"), "Trend_Thrombolysis_Ischemic",    dir_en)

# ------------------------------------------------------------------------------
# PILAR 5: TENDÊNCIA — Trombectomia e Mortalidade (isquêmico)
# Facetas com escalas Y independentes — eliminam comparação visual entre
# séries com magnitudes incomparáveis (0–3% vs 12–17%).
# ------------------------------------------------------------------------------
tend_tromb_raw <- df_final |>
  filter(coorte_avc == "isquemico") |>
  group_by(ANO) |>
  summarise(
    tx_trombectomia = mean(fez_trombectomia, na.rm = TRUE) * 100,
    tx_obito        = mean(obito_hospitalar, na.rm = TRUE) * 100,
    .groups = "drop"
  )

construir_tend_trombectomia <- function(lang = "pt") {
  
  df_long <- tend_tromb_raw |>
    pivot_longer(c(tx_trombectomia, tx_obito), names_to = "serie", values_to = "taxa")
  
  if (lang == "pt") {
    df_long <- df_long |>
      mutate(serie = case_when(
        serie == "tx_trombectomia" ~ "Taxa de Trombectomia (%)",
        serie == "tx_obito"        ~ "Letalidade Hospitalar (%)"
      ))
    cores    <- c("Taxa de Trombectomia (%)" = "#2166ac", "Letalidade Hospitalar (%)" = "#d6604d")
    rot_inst <- "Institucionalização\nno SUS (2023)"
    labs_cfg <- labs(
      title   = "Figura 4 — Evolução temporal da trombectomia mecânica e da letalidade\nhospitalar no AVC isquêmico. Brasil, 2015–2025.",
      x       = "Ano", y = "Taxa (%)",
      caption = paste0(
        "Nota: painéis com escalas Y independentes — magnitudes não são comparáveis entre painéis. ",
        "Subnotificação estrutural do SIGTAP 0403070171 [Sistema de Gerenciamento da ",
        "Tabela de Procedimentos] — taxa real de trombectomia superior ao registrado. ",
        "Dados de 2025 sujeitos à defasagem de notificação (6–18 meses).\n",
        "Fonte: SIH-RD/DATASUS, 2015–2025."
      )
    )
    tema <- tema_abnt
  } else {
    df_long <- df_long |>
      mutate(serie = case_when(
        serie == "tx_trombectomia" ~ "Thrombectomy Rate (%)",
        serie == "tx_obito"        ~ "In-hospital Mortality (%)"
      ))
    cores    <- c("Thrombectomy Rate (%)" = "#2166ac", "In-hospital Mortality (%)" = "#d6604d")
    rot_inst <- "SUS Implementation\n(2023)"
    labs_cfg <- labs(
      title    = "Trends in Mechanical Thrombectomy and In-hospital Mortality — Ischemic Stroke",
      subtitle = "Brazil, 2015–2025 | SIH-RD/DATASUS | Unadjusted rates | Independent Y-axes per panel",
      x = "Year", y = "Rate (%)",
      caption  = paste0(
        "Note: Y-axes are panel-specific and not directly comparable across panels. ",
        "Structural underreporting of SIGTAP procedure code 0403070171 ",
        "[Brazilian Procedure Classification System] — actual rates likely exceed recorded figures. ",
        "2025 data subject to reporting lag (6–18 months).\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025."
      )
    )
    tema <- tema_en
  }
  
  df_ann <- df_long |>
    group_by(serie) |>
    summarise(y_top = max(taxa, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_long, aes(x = ANO, y = taxa, color = serie, group = serie)) +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50", linewidth = 0.6) +
    geom_text(
      data = df_ann,
      aes(x = 2022.6, y = y_top, label = rot_inst),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1, size = 3, color = "grey40", fontface = "italic"
    ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    facet_wrap(~ serie, scales = "free_y", ncol = 1) +
    scale_x_continuous(breaks = 2015:2025) +
    scale_color_manual(values = cores, guide = "none") +
    labs_cfg + tema
}

salvar_figura(construir_tend_trombectomia("pt"), "Tendencia_Trombectomia_Isquemico", dir_pt, width_in = 14, height_in = 10)
salvar_figura(construir_tend_trombectomia("en"), "Trend_Thrombectomy_Ischemic",      dir_en, width_in = 14, height_in = 10)

# ==============================================================================
# ANÁLISES DE SENSIBILIDADE — Resposta ao parecer da banca
# ==============================================================================

df_sens <- df_final |>
  filter(!is.na(distancia_km)) |>
  mutate(
    IDADE_NUM  = as.numeric(IDADE),
    ANO_F      = factor(ANO),
    SEXO_F     = factor(SEXO),
    CNES_F     = factor(CNES),
    RACA_COR_F = factor(
      ifelse(is.na(RACA_COR) | RACA_COR == "99", "Ignorado", RACA_COR)
    ),
    dist_cat = cut(
      distancia_km,
      breaks         = c(-Inf, 0, 25, 50, 100, Inf),
      labels         = c("0 km (mesmo município)",
                         "1–25 km", "26–50 km", "51–100 km", "> 100 km"),
      right          = TRUE,
      include.lowest = TRUE
    )
  )

df_isq_sens  <- filter(df_sens, coorte_avc == "isquemico")
df_hem_sens  <- filter(df_sens, coorte_avc == "hemorragico")
df_ndef_sens <- filter(df_sens, coorte_avc == "nao_especificado")

ajustar_glm_cluster_sens <- function(formula, dados) {
  fit <- glm(formula, data = dados, family = binomial(link = "logit"))
  environment(fit$terms) <- environment()
  tbl <- coeftest(fit, vcov = vcovCL(fit, cluster = dados$CNES_F))
  list(fit = fit, tbl = tbl)
}

# ------------------------------------------------------------------------------
# S1. Distância categorizada (modelo primário de sensibilidade)
# ------------------------------------------------------------------------------
m1_cat <- ajustar_glm_cluster_sens(
  fez_trombolise ~ dist_cat + IDADE_NUM + SEXO_F + RACA_COR_F +
    flag_hipertensao + flag_diabetes + flag_fib_atrial + ANO_F,
  df_isq_sens
)

formula_obito_cat <- obito_hospitalar ~
  dist_cat + IDADE_NUM + SEXO_F + RACA_COR_F +
  flag_hipertensao + flag_diabetes + flag_fib_atrial +
  flag_renal_cron  + flag_insuf_card + ANO_F

m2_isq_cat  <- ajustar_glm_cluster_sens(formula_obito_cat, df_isq_sens)
m2_hem_cat  <- ajustar_glm_cluster_sens(formula_obito_cat, df_hem_sens)
m2_ndef_cat <- ajustar_glm_cluster_sens(formula_obito_cat, df_ndef_sens)

print(m1_cat$tbl)
print(m2_isq_cat$tbl)

# ------------------------------------------------------------------------------
# S2. Splines naturais — teste de não-linearidade e curva dose-resposta
#
# CORREÇÃO 1 (banca): nós calculados APENAS sobre distâncias > 0.
#   ~55% das internações ocorrem no mesmo município (distância = 0 km), então
#   quantile(distancia_km, c(0.25, 0.5, 0.75)) retorna 0, 0, X — gerando
#   nós degenerados que invalidam o teste de não-linearidade.
#   Excluir o spike em zero resolve o problema sem alterar a estimativa;
#   a variável entra no modelo incluindo os zeros (apenas o posicionamento
#   dos nós é calculado sobre positivos).
#
# CORREÇÃO 2 (banca): IC da curva obtido via predict(se.fit=TRUE) na escala
#   de ligação (logit), depois transformado via plogis().
#   O IC anterior (prob_pred ± 0.3) era uma banda constante de ±0.3 pp,
#   sem fundamento estatístico — não alarga nas regiões de dados escassos.
# ------------------------------------------------------------------------------

# --- Nós calculados apenas sobre distâncias positivas -----------------------
nos_dist <- quantile(
  df_isq_sens$distancia_km[df_isq_sens$distancia_km > 0],
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

cat(sprintf(
  "\nNós da spline (P25/P50/P75 de distâncias > 0 km): %.1f, %.1f, %.1f km\n",
  nos_dist[1], nos_dist[2], nos_dist[3]
))

formula_sp_trombolise <- as.formula(paste0(
  "fez_trombolise ~ ns(distancia_km, knots = c(",
  paste(nos_dist, collapse = ", "), ")) + ",
  "IDADE_NUM + SEXO_F + RACA_COR_F + ",
  "flag_hipertensao + flag_diabetes + flag_fib_atrial + ANO_F"
))

# Modelo linear (base) para teste LRT
m1_linear_glm <- glm(
  fez_trombolise ~ distancia_km + IDADE_NUM + SEXO_F + RACA_COR_F +
    flag_hipertensao + flag_diabetes + flag_fib_atrial + ANO_F,
  data = df_isq_sens, family = binomial()
)

m1_sp_glm <- glm(formula_sp_trombolise, data = df_isq_sens, family = binomial())

cat("\n=== Teste de não-linearidade (LRT): Trombólise vs Distância ===\n")
lrt_result <- anova(m1_linear_glm, m1_sp_glm, test = "Chisq")
print(lrt_result)

# Capturar p-valor do LRT para embuti-lo na legenda da figura
lrt_pval  <- lrt_result[2, "Pr(>Chi)"]
lrt_df    <- lrt_result[2, "Df"]
lrt_label <- if (lrt_pval < 0.001) {
  sprintf("LRT p < 0,001 (gl = %d)", lrt_df)
} else {
  sprintf("LRT p = %.3f (gl = %d)", lrt_pval, lrt_df)
}
lrt_label_en <- if (lrt_pval < 0.001) {
  sprintf("LRT p < 0.001 (df = %d)", lrt_df)
} else {
  sprintf("LRT p = %.3f (df = %d)", lrt_pval, lrt_df)
}

# --- Grade de predição (0–200 km) ------------------------------------------
dist_max_plot <- 200

ref_sexo <- levels(df_isq_sens$SEXO_F)[1]
ref_raca <- levels(df_isq_sens$RACA_COR_F)[1]
ref_ano  <- "2019"

df_pred_sp <- tibble(
  distancia_km     = seq(0, dist_max_plot, by = 0.5),
  IDADE_NUM        = median(df_isq_sens$IDADE_NUM, na.rm = TRUE),
  SEXO_F           = factor(ref_sexo, levels = levels(df_isq_sens$SEXO_F)),
  RACA_COR_F       = factor(ref_raca, levels = levels(df_isq_sens$RACA_COR_F)),
  flag_hipertensao = FALSE,
  flag_diabetes    = FALSE,
  flag_fib_atrial  = FALSE,
  ANO_F            = factor(ref_ano, levels = levels(df_isq_sens$ANO_F))
)

# --- IC na escala de ligação (logit), transformado por plogis() -------------
# Esta é a única forma estatisticamente correta: o IC simétrico na escala de
# ligação torna-se assimétrico na escala de probabilidade, e alarga onde há
# poucos dados (distâncias > 100 km), como esperado pela banca.
pred_link <- predict(m1_sp_glm, newdata = df_pred_sp, type = "link", se.fit = TRUE)

df_pred_sp$prob_pred <- plogis(pred_link$fit)         * 100
df_pred_sp$lo        <- plogis(pred_link$fit - 1.96 * pred_link$se.fit) * 100
df_pred_sp$hi        <- plogis(pred_link$fit + 1.96 * pred_link$se.fit) * 100

# Rug restrito ao intervalo do gráfico (evita plotar outliers de 3.800 km)
df_rug <- df_isq_sens |>
  filter(distancia_km <= dist_max_plot) |>
  slice_sample(n = min(5000, sum(df_isq_sens$distancia_km <= dist_max_plot, na.rm = TRUE)))

construir_spline <- function(lang = "pt") {
  
  if (lang == "pt") {
    labs_cfg <- labs(
      title   = paste0(
        "Figura S1 — Probabilidade predita de trombólise por distância ao hospital\n",
        "(AVC isquêmico, spline natural, demais covariáveis na mediana). Brasil, 2015–2025."
      ),
      x       = "Distância ao hospital (km)",
      y       = "Probabilidade predita de trombólise (%)",
      caption = paste0(
        "Nota: demais covariáveis fixas na mediana/moda (ano de referência = 2019). ",
        "Nós da spline natural (calculados sobre distâncias > 0): ",
        paste(round(nos_dist, 1), collapse = ", "), " km. ",
        lrt_label, ". ",
        "Banda de IC 95% obtida na escala logit e transformada por plogis() — ",
        "alarga em regiões de dados escassos (distâncias > 100 km). ",
        "Eixo X truncado em 200 km (≈ P97); outliers acima provavelmente refletem ",
        "erro de codificação de município de residência.\n",
        "Fonte: SIH-RD/DATASUS, 2015–2025."
      )
    )
    tema <- tema_abnt
  } else {
    labs_cfg <- labs(
      title    = "Figure S1 — Predicted probability of thrombolysis by distance to hospital",
      subtitle = "Ischemic stroke | Natural spline | Covariates at median | Brazil, 2015–2025",
      x        = "Distance to hospital (km)",
      y        = "Predicted probability of thrombolysis (%)",
      caption  = paste0(
        "Covariates fixed at median/mode (reference year = 2019). ",
        "Natural spline knots (computed on distances > 0 km): ",
        paste(round(nos_dist, 1), collapse = ", "), " km. ",
        lrt_label_en, ". ",
        "95% CI band derived on the logit scale and back-transformed via plogis() — ",
        "widens in data-sparse regions (distances > 100 km). ",
        "X-axis truncated at 200 km (≈ P97); higher values likely reflect municipality miscoding.\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025."
      )
    )
    tema <- tema_en
  }
  
  ggplot(df_pred_sp, aes(x = distancia_km, y = prob_pred)) +
    geom_ribbon(
      aes(ymin = lo, ymax = hi),
      fill = "#2166ac", alpha = 0.15
    ) +
    geom_line(linewidth = 1.3, color = "#2166ac") +
    geom_rug(
      data  = df_rug,
      aes(x = distancia_km, y = NULL),
      sides = "b", alpha = 0.03, color = "grey40", length = unit(0.02, "npc")
    ) +
    scale_x_continuous(
      limits = c(0, dist_max_plot),
      breaks = seq(0, dist_max_plot, by = 25)
    ) +
    labs_cfg + tema
}

salvar_figura(construir_spline("pt"), "FigS1_Spline_Trombolise",   dir_pt, width_in = 12, height_in = 7)
salvar_figura(construir_spline("en"), "FigS1_Spline_Thrombolysis", dir_en, width_in = 12, height_in = 7)

# ------------------------------------------------------------------------------
# S3. Forest plot de sensibilidade — distância categorizada × mortalidade
# ------------------------------------------------------------------------------
extrair_or_cat <- function(resultado, nome_coorte) {
  tbl <- resultado$tbl
  tibble(
    term       = rownames(tbl),
    estimate   = exp(tbl[, "Estimate"]),
    conf.low   = exp(tbl[, "Estimate"] - 1.96 * tbl[, "Std. Error"]),
    conf.high  = exp(tbl[, "Estimate"] + 1.96 * tbl[, "Std. Error"]),
    p.value    = tbl[, "Pr(>|z|)"],
    coorte_raw = nome_coorte
  ) |>
    filter(grepl("^dist_cat", term))
}

df_forest_cat <- bind_rows(
  extrair_or_cat(m2_isq_cat,  "isquemico"),
  extrair_or_cat(m2_hem_cat,  "hemorragico"),
  extrair_or_cat(m2_ndef_cat, "nao_especificado")
)

construir_forest_cat <- function(lang = "pt") {
  
  niveis_dist <- c("1–25 km", "26–50 km", "51–100 km", "> 100 km")
  
  if (lang == "pt") {
    df <- df_forest_cat |>
      mutate(
        coorte = case_when(
          coorte_raw == "isquemico"        ~ "Isquêmico",
          coorte_raw == "hemorragico"      ~ "Hemorrágico",
          coorte_raw == "nao_especificado" ~ "Não Especificado"
        ),
        label = case_when(
          term == "dist_cat1–25 km"    ~ "1–25 km",
          term == "dist_cat26–50 km"   ~ "26–50 km",
          term == "dist_cat51–100 km"  ~ "51–100 km",
          term == "dist_cat> 100 km"   ~ "> 100 km",
          TRUE ~ term
        ),
        significante = if_else(p.value < 0.05, "p < 0,05", "p ≥ 0,05"),
        coorte = factor(coorte, levels = c("Isquêmico", "Hemorrágico", "Não Especificado")),
        label  = factor(label, levels = rev(niveis_dist))
      )
    cores     <- c("Isquêmico" = "#2166ac", "Hemorrágico" = "#d6604d", "Não Especificado" = "#4dac26")
    sig_shape <- c("p < 0,05" = 16, "p ≥ 0,05" = 1)
    x_labels  <- c("0,50", "1,00", "2,00", "4,00")
    labs_cfg  <- labs(
      title   = paste0(
        "Figura S2 — OR para óbito hospitalar por faixa de distância (análise de sensibilidade).\n",
        "Referência: 0 km (mesmo município de residência e internação). Brasil, 2015–2025."
      ),
      x       = "Odds Ratio (escala logarítmica)", y = NULL,
      color   = "Subtipo de AVC", shape = "Significância",
      caption = paste0(
        "Nota: analisa gradiente dose-resposta da distância sem impor linearidade. ",
        "IC 95% de Wald, estimador sanduíche, clusterizado por CNES ",
        "[Cadastro Nacional de Estabelecimentos de Saúde].\n",
        "Fonte: SIH-RD/DATASUS, 2015–2025."
      )
    )
    tema <- tema_abnt
  } else {
    df <- df_forest_cat |>
      mutate(
        coorte = case_when(
          coorte_raw == "isquemico"        ~ "Ischemic",
          coorte_raw == "hemorragico"      ~ "Hemorrhagic",
          coorte_raw == "nao_especificado" ~ "Unspecified"
        ),
        label = case_when(
          term == "dist_cat1–25 km"    ~ "1–25 km",
          term == "dist_cat26–50 km"   ~ "26–50 km",
          term == "dist_cat51–100 km"  ~ "51–100 km",
          term == "dist_cat> 100 km"   ~ "> 100 km",
          TRUE ~ term
        ),
        significante = if_else(p.value < 0.05, "p < 0.05", "p ≥ 0.05"),
        coorte = factor(coorte, levels = c("Ischemic", "Hemorrhagic", "Unspecified")),
        label  = factor(label, levels = rev(niveis_dist))
      )
    cores     <- c("Ischemic" = "#2166ac", "Hemorrhagic" = "#d6604d", "Unspecified" = "#4dac26")
    sig_shape <- c("p < 0.05" = 16, "p ≥ 0.05" = 1)
    x_labels  <- c("0.50", "1.00", "2.00", "4.00")
    labs_cfg  <- labs(
      title    = "Figure S2 — OR for in-hospital mortality by distance category (sensitivity analysis)",
      subtitle = "Reference: 0 km (same municipality of residence and admission). Brazil, 2015–2025.",
      x = "Odds Ratio (log scale)", y = NULL,
      color = "Stroke Subtype", shape = "Significance",
      caption = paste0(
        "Tests dose-response gradient without imposing linearity. ",
        "95% Wald CI, sandwich estimator clustered by CNES ",
        "[Brazilian National Health Facility Registry].\n",
        "Source: Hospital Information System (SIH-RD/DATASUS), Brazil, 2015–2025."
      )
    )
    tema <- tema_en
  }
  
  ggplot(df, aes(x = estimate, y = label, color = coorte, shape = significante)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.6),
                    linewidth = 0.7, size = 0.6) +
    scale_x_log10(breaks = c(0.5, 1, 2, 4), labels = x_labels) +
    scale_shape_manual(values = sig_shape) +
    scale_color_manual(values = cores) +
    labs_cfg + tema +
    theme(axis.text.y = element_text(size = 12))
}

salvar_figura(construir_forest_cat("pt"), "FigS2_Forest_DistCat_Mortalidade", dir_pt, height_in = 7)
salvar_figura(construir_forest_cat("en"), "FigS2_Forest_DistCat_Mortality",   dir_en, height_in = 7)

cat("\n=== Figuras salvas com sucesso. ===\n")
cat(sprintf("PT/ABNT: %s\n", dir_pt))
cat(sprintf("EN:      %s\n", dir_en))

# ------------------------------------------------------------------------------
# 8. EXPORTAÇÃO DOS ORs PARA DOCX (Geração automática dos valores para o texto)
# ------------------------------------------------------------------------------
tabela_or_texto <- bind_rows(
  df_forest_base |> mutate(Modelo = "Principal (Variáveis Contínuas/Demográficas)"),
  df_forest_cat |> mutate(Modelo = "Sensibilidade (Distância Categórica)")
) |>
  mutate(
    ORa = round(estimate, 2),
    IC_Inf = round(conf.low, 2),
    IC_Sup = round(conf.high, 2),
    `Valor p` = round(p.value, 3),
    `Texto Sugerido` = sprintf("(ORa, %.2f; IC 95%%, %.2f-%.2f)", ORa, IC_Inf, IC_Sup)
  ) |>
  select(Modelo, Coorte = coorte_raw, Variavel = term, ORa, `IC 95% Inf` = IC_Inf, `IC 95% Sup` = IC_Sup, `Valor p`, `Texto Sugerido`)

tabela_or_texto |>
  flextable() |>
  autofit() |>
  flextable::save_as_docx(path = file.path(dir_out, "Valores_ORa_Para_Texto.docx"))

cat(sprintf("\nArquivo com os ORa exatos e formatados para o texto salvo em:\n%s\n", file.path(dir_out, "Valores_ORa_Para_Texto.docx")))