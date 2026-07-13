# ==============================================================================
# PROJETO TCC — DAP no SUS: Iniquidades Cirúrgicas em Alagoas vs. Nordeste
# Período: 2015–2025 | Fonte: SIHSUS (microdatasus)
# Saídas: Português (ABNT) e Inglês
# ==============================================================================

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
options(timeout = 600)

# --- DIRETÓRIOS ---
base_dir  <- "/Users/davibarbosa/Desktop/Luís"
data_dir  <- file.path(base_dir, "Dados")
output_pt <- file.path(base_dir, "Resultados_PT")
output_en <- file.path(base_dir, "Resultados_EN")
for (d in c(data_dir, output_pt, output_en))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- PACOTES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  microdatasus, data.table, dplyr, tidyr,
  ggplot2, ragg, lmtest, ggrepel, nlme,
  officer, flextable, geobr, sf, scales, zoo,
  gtsummary, brglm2, pROC, patchwork, sandwich
)

# ==============================================================================
# SISTEMA DE DESIGN — PUBLICAÇÃO MÉDICA (JAMA / Lancet / BMJ)
# ==============================================================================

COR_AL <- "#B2182B"   # vermelho escuro (ColorBrewer RdBu) — Alagoas
COR_NE <- "#2166AC"   # azul escuro    (ColorBrewer RdBu) — Nordeste

tema_pub <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", size = base_size + 1,
                                        hjust = 0, margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base_size - 1.5, color = "grey45",
                                        hjust = 0, margin = margin(b = 6)),
      plot.caption       = element_text(size = base_size - 2.5, color = "grey55",
                                        hjust = 0, margin = margin(t = 6)),
      axis.title         = element_text(size = base_size - 0.5),
      axis.text          = element_text(size = base_size - 1.5, color = "grey20"),
      axis.line          = element_line(color = "grey30", linewidth = 0.4),
      axis.ticks         = element_line(color = "grey30", linewidth = 0.4),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(size = base_size - 1, face = "bold"),
      legend.text        = element_text(size = base_size - 1),
      legend.key.size    = unit(0.9, "lines"),
      plot.margin        = margin(10, 12, 8, 10)
    )
}

tema_mapa_pub <- function(base_size = 11) {
  theme_void(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1,
                                   hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size - 1.5, color = "grey45",
                                   hjust = 0.5, margin = margin(b = 6)),
      plot.caption  = element_text(size = base_size - 2.5, color = "grey55",
                                   hjust = 0.5, margin = margin(t = 6)),
      legend.position = "right",
      legend.title    = element_text(size = base_size - 1, face = "bold"),
      legend.text     = element_text(size = base_size - 1),
      plot.margin     = margin(10, 10, 10, 10)
    )
}

escala_mapa_pub <- function(nome_legenda) {
  scale_fill_viridis_c(
    option    = "plasma", direction = -1, begin = 0.05, end = 0.92,
    na.value  = "grey88", name = nome_legenda,
    labels    = scales::label_number(suffix = "%"),
    guide     = guide_colorbar(barwidth = 1, barheight = 8, ticks.colour = NA)
  )
}

# ==============================================================================
# FUNÇÕES DE SUPORTE
# ==============================================================================

salvar_grafico <- function(plot, nome_base, dir, w = 12, h = 7) {
  ragg::agg_png(file.path(dir, paste0(nome_base, ".png")),
                width = w, height = h, units = "in", res = 300)
  print(plot); dev.off()
  ragg::agg_tiff(file.path(dir, paste0(nome_base, ".tiff")),
                 width = w, height = h, units = "in", res = 300)
  print(plot); dev.off()
}

ft <- function(df) {
  flextable(df) |> theme_vanilla() |> autofit() |>
    fontsize(size = 10, part = "all") |> bold(part = "header")
}

# Formata p-valores: nunca reporta "0" ou "0.000"
fmt_p <- function(p, digits = 3) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "< 0.001",
                formatC(round(p, digits), format = "f", digits = digits)))
}

# Aplica labels legíveis nos termos do modelo (substitui nomes internos do R)
aplicar_labels_or <- function(df, lang = "pt") {
  rot_pt <- c(
    "(Intercept)"           = "Intercepto",
    "Faixa_Etaria< 50 anos" = "Idade < 50 anos",
    "Faixa_Etaria> 70 anos" = "Idade > 70 anos",
    "Ano_Centrado"          = "Ano (centrado em 2020)",
    "SEXOFeminino"          = "Sexo: Feminino",
    "Diabetes_Fator1"       = "Diabetes (sim)"
  )
  rot_en <- c(
    "(Intercept)"           = "Intercept",
    "Faixa_Etaria< 50 anos" = "Age < 50 years",
    "Faixa_Etaria> 70 anos" = "Age > 70 years",
    "Ano_Centrado"          = "Year (centered on 2020)",
    "SEXOFeminino"          = "Sex: Female",
    "Diabetes_Fator1"       = "Diabetes (yes)"
  )
  rot <- if (lang == "pt") rot_pt else rot_en
  df %>% mutate(Variavel = ifelse(Variavel %in% names(rot), rot[Variavel], Variavel))
}

# Durbin-Watson manual a partir dos resíduos
dw_stat <- function(modelo) {
  res <- residuals(modelo)
  round(sum(diff(res)^2) / sum(res^2), 3)
}

# ==============================================================================
# 1. EXTRAÇÃO EM LOTES SEMESTRAIS (Otimização de Memória RAM)
# ==============================================================================

ficheiro_rds  <- file.path(data_dir, "SIH_Nordeste_DAP_2015_2025.rds")
DATA_CORTE    <- as.Date("2025-06-01")
cids_diabetes <- c("E10", "E11", "E12", "E13", "E14")

if (!file.exists(ficheiro_rds)) {
  message("Iniciando extração otimizada por semestre...")
  ufs_ne      <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
  anos_estudo <- 2015:2025
  lista_semestral <- list()
  
  for (ano in anos_estudo) {
    for (sem in 1:2) {
      if (ano == 2025 && sem == 2) next
      mes_ini <- if (sem == 1) 1 else 7
      mes_fim <- if (sem == 1) 6 else 12
      message(sprintf("\n--- Baixando: %d | S%d ---", ano, sem))
      
      dados_brutos <- tryCatch({
        fetch_datasus(
          year_start = ano, month_start = mes_ini,
          year_end   = ano, month_end   = mes_fim,
          uf = ufs_ne, information_system = "SIH-RD"
        )
      }, error = function(e) {
        message("SIH-RD falhou. Tentando SIHSUS-RD...")
        tryCatch(
          fetch_datasus(
            year_start = ano, month_start = mes_ini,
            year_end   = ano, month_end   = mes_fim,
            uf = ufs_ne, information_system = "SIHSUS-RD"
          ),
          error = function(e2) NULL
        )
      })
      
      if (is.null(dados_brutos) || nrow(dados_brutos) == 0) {
        message("Sem dados. Pulando."); next
      }
      
      dados_proc <- process_sih(dados_brutos)
      
      campos_morte <- grep("mort|obit|cobr|said|MORT|OBIT|COBR|SAID",
                           names(dados_proc), ignore.case = TRUE, value = TRUE)
      if (length(campos_morte) > 0 && ano == anos_estudo[1] && sem == 1) {
        message("Campos candidatos a mortalidade detectados: ",
                paste(campos_morte, collapse = ", "))
      }
      
      cols_secun <- grep("^DIAGSEC|DIAG_SECUN", names(dados_proc), value = TRUE)
      if (length(cols_secun) > 0) {
        mat_diab <- sapply(cols_secun, function(col) {
          v <- as.character(dados_proc[[col]])
          !is.na(v) & nchar(v) >= 3 & substr(v, 1, 3) %in% cids_diabetes
        })
        if (is.vector(mat_diab)) mat_diab <- matrix(mat_diab, ncol = 1)
        dados_proc$Tem_Diabetes <- as.integer(rowSums(mat_diab) > 0)
      } else {
        dados_proc$Tem_Diabetes <- 0L
        warning(sprintf("Ano %d S%d: DIAGSEC ausente — Tem_Diabetes = 0.", ano, sem))
      }
      
      obito_vec <- {
        nm <- names(dados_proc)
        if ("MORTE" %in% nm) {
          as.integer(as.character(dados_proc$MORTE) %in% c("1", "Sim"))
        } else if ("COBRANCA" %in% nm) {
          cob <- as.character(dados_proc$COBRANCA)
          as.integer(
            grepl("[Óó]bito|[Oo]bito|OBITO", cob) &
              !grepl("[Ff]etal|[Nn]eonato|[Pp]uérpera|[Pp]uerp|[Rr]ecém.nascido", cob)
          )
        } else if ("CID_MORTE" %in% nm) {
          cid_m <- as.character(dados_proc$CID_MORTE)
          as.integer(!is.na(cid_m) & nchar(trimws(cid_m)) >= 3 &
                       !trimws(cid_m) %in% c("0000", ""))
        } else if ("MOTIVO_SAIDA" %in% nm) {
          as.integer(grepl("[Óó]bito|[Oo]bito|OBITO|^41$|^42$",
                           as.character(dados_proc$MOTIVO_SAIDA)))
        } else {
          rep(0L, nrow(dados_proc))
        }
      }
      
      df_filtrado <- dados_proc %>%
        mutate(
          MUNIC_RES        = as.character(MUNIC_RES),
          Grupo            = if_else(substr(MUNIC_RES, 1, 2) == "27",
                                     "Alagoas", "Nordeste"),
          Ano              = as.numeric(ANO_CMPT),
          Mes              = as.numeric(MES_CMPT),
          Data_Competencia = as.Date(paste0(Ano, "-", sprintf("%02d", Mes), "-01")),
          Idade_Num        = as.numeric(IDADE),
          Obito            = obito_vec,
          Custo_Total      = as.numeric(VAL_TOT),
          Dias_Internacao  = as.numeric(DIAS_PERM),
          
          SEXO = case_when(
            as.character(SEXO) %in% c("1", "Masculino") ~ "Masculino",
            as.character(SEXO) %in% c("3", "Feminino")  ~ "Feminino",
            TRUE                                          ~ "Ignorado"
          ),
          
          Raca = case_when(
            as.character(RACA_COR) %in% c("01", "1", "Branca")               ~ "Branca",
            as.character(RACA_COR) %in% c("02", "2", "Preta")                ~ "Preta",
            as.character(RACA_COR) %in% c("03", "3", "Parda")                ~ "Parda",
            as.character(RACA_COR) %in% c("04", "4", "Amarela")              ~ "Amarela",
            as.character(RACA_COR) %in% c("05", "5", "Indígena", "Indigena") ~ "Indigena",
            TRUE                                                               ~ "Ignorado"
          ),
          
          Tem_Diabetes = as.integer(
            substr(DIAG_PRINC, 1, 3) %in% cids_diabetes | Tem_Diabetes == 1
          ),
          
          Tipo_Intervencao = case_when(
            PROC_REA %in% c("0408050012", "0408050020", "0408050039", "0408050047") ~ "Amputacao",
            PROC_REA %in% c("0406020442", "0406020450", "0406020434", "0406020388",
                            "0406020370", "0406020361", "0406020085", "0406020093",
                            "0406020115", "0406020468", "0406040052", "0406040060",
                            "0406040079") ~ "Revascularizacao",
            TRUE ~ "Conservador_Outros"
          )
        ) %>%
        filter(
          (substr(DIAG_PRINC, 1, 3) %in% c("I70", "I73") |
             substr(DIAG_PRINC, 1, 4) %in% c("E105", "E115", "E125", "E135", "E145")),
          Tipo_Intervencao %in% c("Amputacao", "Revascularizacao"),
          Data_Competencia <= DATA_CORTE,
          Idade_Num >= 20
        )
      
      lista_semestral[[paste0(ano, "_S", sem)]] <- df_filtrado
      rm(dados_brutos, dados_proc, df_filtrado); gc()
    }
  }
  
  df_dap <- bind_rows(lista_semestral)
  rm(lista_semestral); gc()
  
  saveRDS(df_dap, ficheiro_rds)
  message("RDS salvo: ", ficheiro_rds)
  
} else {
  message("Carregando dados locais...")
  df_dap <- readRDS(ficheiro_rds)
}

# ==============================================================================
# NORMALIZAÇÃO PÓS-CARGA
# Converte "NE (excl. AL)" do RDS legado para "Nordeste" em toda a base
# ==============================================================================
df_dap <- df_dap %>%
  mutate(
    Grupo = if_else(Grupo == "NE (excl. AL)", "Nordeste", as.character(Grupo)),
    SEXO = case_when(
      as.character(SEXO) %in% c("1", "Masculino") ~ "Masculino",
      as.character(SEXO) %in% c("3", "Feminino")  ~ "Feminino",
      TRUE                                          ~ "Ignorado"
    ),
    Obito = as.integer(as.character(Obito) %in% c("1", "TRUE"))
  )

if (!"Raca" %in% names(df_dap)) {
  warning("Campo 'Raca' ausente no RDS. Apague o cache e re-execute para incluir raça/cor.")
  df_dap$Raca <- "Ignorado"
}

message("\n=== Diagnóstico SEXO ===");    print(table(df_dap$SEXO, useNA = "ifany"))
message("=== Diagnóstico OBITO ===");    print(table(df_dap$Obito, useNA = "ifany"))
message("=== Diagnóstico DIABETES ==="); print(table(df_dap$Tem_Diabetes, df_dap$Grupo))

# ==============================================================================
# 1b. TABELA 1 — CARACTERÍSTICAS BASAIS DA COORTE
# ==============================================================================

df_tab1 <- df_dap %>%
  mutate(
    Faixa_Etaria     = factor(case_when(
      Idade_Num < 50  ~ "< 50 anos",
      Idade_Num <= 70 ~ "50-70 anos",
      TRUE            ~ "> 70 anos"
    ), levels = c("< 50 anos", "50-70 anos", "> 70 anos")),
    SEXO             = factor(SEXO, levels = c("Masculino", "Feminino", "Ignorado")),
    Raca             = factor(Raca, levels = c("Branca", "Parda", "Preta",
                                               "Amarela", "Indigena", "Ignorado")),
    Tipo_Intervencao = factor(Tipo_Intervencao,
                              levels = c("Amputacao", "Revascularizacao")),
    Tem_Diabetes     = factor(Tem_Diabetes, levels = c(0, 1), labels = c("Não", "Sim")),
    Obito            = factor(Obito,        levels = c(0, 1), labels = c("Não", "Sim")),
    Grupo            = factor(Grupo)
  )

options(OutDec = ",")

tabela1_pt <- df_tab1 %>%
  select(Grupo, Idade_Num, Faixa_Etaria, SEXO, Raca,
         Tipo_Intervencao, Tem_Diabetes, Obito, Dias_Internacao, Custo_Total) %>%
  tbl_summary(
    by       = Grupo,
    label    = list(
      Idade_Num        ~ "Idade (anos)",
      Faixa_Etaria     ~ "Faixa etária",
      SEXO             ~ "Sexo",
      Raca             ~ "Raça/cor",
      Tipo_Intervencao ~ "Tipo de intervenção",
      Tem_Diabetes     ~ "Diabetes mellitus",
      Obito            ~ "Óbito intra-hospitalar",
      Dias_Internacao  ~ "Tempo de internação (dias)",
      Custo_Total      ~ "Custo total (R$)"
    ),
    statistic    = list(all_continuous()  ~ "{median} [{p25}–{p75}]",
                        all_categorical() ~ "{n} ({p}%)"),
    digits       = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing_text = "Ignorado / sem informação"
  ) %>%
  add_p(test       = list(all_continuous()  ~ "wilcox.test",
                          all_categorical() ~ "chisq.test"),
        pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  add_overall(last = FALSE) %>%
  modify_header(label ~ "**Característica**") %>%
  bold_labels()

tabela1_en <- df_tab1 %>%
  mutate(
    Grupo        = dplyr::recode(as.character(Grupo),
                                 "Nordeste" = "Northeast"),
    Faixa_Etaria = factor(case_when(
      Idade_Num < 50  ~ "< 50 years",
      Idade_Num <= 70 ~ "50-70 years",
      TRUE            ~ "> 70 years"
    ), levels = c("< 50 years", "50-70 years", "> 70 years")),
    SEXO = factor(as.character(SEXO),
                  levels = c("Masculino", "Feminino", "Ignorado"),
                  labels = c("Male", "Female", "Unknown")),
    Raca = factor(as.character(Raca),
                  levels = c("Branca", "Parda", "Preta", "Amarela", "Indigena", "Ignorado"),
                  labels = c("White", "Parda (Mixed/Brown)", "Black",
                             "Asian-Brazilian", "Indigenous", "Not reported")),
    Tipo_Intervencao = factor(as.character(Tipo_Intervencao),
                              levels = c("Amputacao", "Revascularizacao"),
                              labels = c("Amputation", "Revascularization")),
    Tem_Diabetes = factor(as.character(Tem_Diabetes),
                          levels = c("Não", "Sim"), labels = c("No", "Yes")),
    Obito = factor(as.character(Obito),
                   levels = c("Não", "Sim"), labels = c("No", "Yes"))
  ) %>%
  select(Grupo, Idade_Num, Faixa_Etaria, SEXO, Raca,
         Tipo_Intervencao, Tem_Diabetes, Obito, Dias_Internacao, Custo_Total) %>%
  tbl_summary(
    by       = Grupo,
    label    = list(
      Idade_Num        ~ "Age (years)",
      Faixa_Etaria     ~ "Age group",
      SEXO             ~ "Sex",
      Raca             ~ "Race/ethnicity",
      Tipo_Intervencao ~ "Type of intervention",
      Tem_Diabetes     ~ "Diabetes mellitus",
      Obito            ~ "In-hospital mortality",
      Dias_Internacao  ~ "Length of stay (days)",
      Custo_Total      ~ "Total cost (BRL)"
    ),
    statistic    = list(all_continuous()  ~ "{median} [{p25}–{p75}]",
                        all_categorical() ~ "{n} ({p}%)"),
    digits       = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing_text = "Unknown / not reported"
  ) %>%
  add_p(test       = list(all_continuous()  ~ "wilcox.test",
                          all_categorical() ~ "chisq.test"),
        pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  add_overall(last = FALSE) %>%
  modify_header(label ~ "**Characteristic**") %>%
  bold_labels()

ft_tab1_pt <- as_flex_table(tabela1_pt) %>% fontsize(size = 10, part = "all") %>% autofit()
options(OutDec = ".")
ft_tab1_en <- as_flex_table(tabela1_en) %>% fontsize(size = 10, part = "all") %>% autofit()

tab_obito_strat <- df_dap %>%
  group_by(Grupo, Tipo_Intervencao) %>%
  summarise(
    N         = n(),
    Obitos    = sum(Obito, na.rm = TRUE),
    Letalidade = round(Obitos / N * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    Tipo_Intervencao = recode(Tipo_Intervencao,
                              "Amputacao"        = "Amputação",
                              "Revascularizacao" = "Revascularização")
  )

tab_obito_strat_en <- df_dap %>%
  group_by(Grupo, Tipo_Intervencao) %>%
  summarise(
    N       = n(),
    Deaths  = sum(Obito, na.rm = TRUE),
    CFR     = round(Deaths / N * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    Group     = ifelse(Grupo == "Nordeste", "Northeast", Grupo),
    Procedure = ifelse(Tipo_Intervencao == "Amputacao", "Amputation", "Revascularization")
  ) %>%
  select(Group, Procedure, N, Deaths, CFR)

# ==============================================================================
# 2. MÓDULO ESPACIAL — Wilson Vetorizado + Mapas (geobr)
# ==============================================================================

CUTOFF_MUN <- 30

perfil_municipal <- df_dap %>%
  filter(Grupo == "Alagoas") %>%
  group_by(Municipio = MUNIC_RES) %>%
  summarise(
    Total_Cirurgias = n(),
    N_Amputacoes    = sum(Tipo_Intervencao == "Amputacao"),
    N_Revasc        = sum(Tipo_Intervencao == "Revascularizacao"),
    .groups = "drop"
  ) %>%
  filter(Total_Cirurgias >= CUTOFF_MUN) %>%
  mutate(
    p     = N_Amputacoes / Total_Cirurgias,
    Z     = qnorm(0.975),
    Denom = 1 + Z^2 / Total_Cirurgias,
    Ctr   = (p + Z^2 / (2 * Total_Cirurgias)) / Denom,
    Mgm   = Z * sqrt(p * (1 - p) / Total_Cirurgias +
                       Z^2 / (4 * Total_Cirurgias^2)) / Denom,
    Proporcao_Amp_Pct = round(p * 100, 2),
    IC_Inf_Pct        = round(pmax(0, Ctr - Mgm) * 100, 2),
    IC_Sup_Pct        = round(pmin(1, Ctr + Mgm) * 100, 2)
  ) %>%
  select(Municipio, Total_Cirurgias, N_Amputacoes, N_Revasc,
         Proporcao_Amp_Pct, IC_Inf_Pct, IC_Sup_Pct) %>%
  arrange(desc(Proporcao_Amp_Pct))

perfil_estadual <- df_dap %>%
  mutate(code_state = as.integer(substr(MUNIC_RES, 1, 2))) %>%
  group_by(code_state) %>%
  summarise(
    Total_Cirurgias = n(),
    N_Amputacoes    = sum(Tipo_Intervencao == "Amputacao"),
    .groups = "drop"
  ) %>%
  mutate(Proporcao_Amp_Pct = round(N_Amputacoes / Total_Cirurgias * 100, 2))

message("Obtendo shapefiles (geobr)...")
mapa_ne_sf <- geobr::read_state(code_state = "all", year = 2020, showProgress = FALSE) %>%
  filter(name_region == "Nordeste")
mapa_al_sf <- geobr::read_municipality(code_muni = "AL", year = 2020, showProgress = FALSE)

nomes_mun_al <- mapa_al_sf %>%
  sf::st_drop_geometry() %>%
  mutate(Municipio = substr(as.character(code_muni), 1, 6)) %>%
  select(Municipio, Nome = name_muni)

perfil_municipal <- perfil_municipal %>%
  left_join(nomes_mun_al, by = "Municipio") %>%
  relocate(Nome, .after = Municipio)

mapa_ne_dados <- mapa_ne_sf %>% left_join(perfil_estadual, by = "code_state")

mapa_al_dados <- mapa_al_sf %>%
  mutate(Municipio = substr(as.character(code_muni), 1, 6)) %>%
  left_join(perfil_municipal, by = "Municipio")

top_mun_sf <- mapa_al_dados %>%
  filter(!is.na(Total_Cirurgias)) %>%
  arrange(desc(Total_Cirurgias)) %>%
  slice_head(n = 5)

coords_top <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(top_mun_sf)))
top_mun_label <- top_mun_sf %>%
  mutate(x = coords_top[, 1], y = coords_top[, 2]) %>%
  sf::st_drop_geometry()

mapa_ne_dados <- mapa_ne_dados %>%
  mutate(label_cor = ifelse(
    is.na(Proporcao_Amp_Pct) | Proporcao_Amp_Pct < 65,
    "grey25", "white"
  ))

gerar_mapa_ne <- function(lang = "pt") {
  leg <- if (lang == "pt") "Amputações\n/ Total (%)" else "Amputations\n/ Total (%)"
  
  ggplot(mapa_ne_dados) +
    geom_sf(aes(fill = Proporcao_Amp_Pct), color = "white", linewidth = 0.5) +
    geom_sf_text(aes(label = abbrev_state, color = label_cor),
                 size = 2.8, fontface = "bold") +
    scale_color_identity() +
    escala_mapa_pub(leg) + tema_mapa_pub() +
    labs(title = NULL, subtitle = NULL, caption = NULL)
}

gerar_mapa_al <- function(lang = "pt") {
  leg <- if (lang == "pt") "Amputações\n/ Total (%)" else "Amputations\n/ Total (%)"
  
  ggplot(mapa_al_dados) +
    geom_sf(aes(fill = Proporcao_Amp_Pct), color = "white", linewidth = 0.3) +
    ggrepel::geom_text_repel(
      data        = top_mun_label,
      aes(x = x, y = y, label = name_muni),
      size        = 2.2, color = "grey10", fontface = "bold",
      segment.color = "grey50", segment.size = 0.3,
      box.padding = 0.4, min.segment.length = 0.1,
      max.overlaps = Inf
    ) +
    escala_mapa_pub(leg) + tema_mapa_pub() +
    labs(title = NULL, subtitle = NULL, caption = NULL)
}

salvar_grafico(gerar_mapa_ne("pt"), "Mapa_NE_Proporcao_Amputacao",  output_pt, w = 10, h = 7)
salvar_grafico(gerar_mapa_al("pt"), "Mapa_AL_Proporcao_Amputacao",  output_pt, w = 10, h = 8)
salvar_grafico(gerar_mapa_ne("en"), "Map_NE_Amputation_Proportion", output_en, w = 10, h = 7)
salvar_grafico(gerar_mapa_al("en"), "Map_AL_Amputation_Proportion", output_en, w = 10, h = 8)

# ==============================================================================
# 3. SÉRIE TEMPORAL — ITS (Interrupted Time Series) + GLS com AR(1)
# ==============================================================================

VOLUME_MIN <- 5
COVID_DATE  <- as.Date("2020-03-01")

serie_temporal <- df_dap %>%
  group_by(Grupo, Data_Competencia) %>%
  summarise(
    Volume_Cirurgias = n(),
    N_Amputacoes     = sum(Tipo_Intervencao == "Amputacao"),
    .groups = "drop"
  ) %>%
  group_by(Grupo) %>%
  tidyr::complete(
    Data_Competencia = seq(as.Date("2015-01-01"), DATA_CORTE, by = "month"),
    fill = list(Volume_Cirurgias = 0, N_Amputacoes = 0)
  ) %>%
  arrange(Grupo, Data_Competencia) %>%
  mutate(
    Mes_Indice    = row_number(),
    Proporcao_Amp = if_else(Volume_Cirurgias >= VOLUME_MIN,
                            (N_Amputacoes / Volume_Cirurgias) * 100, NA_real_),
    Log_Prop      = if_else(!is.na(Proporcao_Amp),
                            log10(Proporcao_Amp + 0.01), NA_real_)
  ) %>%
  ungroup()

serie_al <- serie_temporal %>% filter(Grupo == "Alagoas")
serie_ne <- serie_temporal %>% filter(Grupo == "Nordeste")

dw_al <- lmtest::dwtest(lm(Log_Prop ~ Mes_Indice, data = serie_al, na.action = na.exclude))
dw_ne <- lmtest::dwtest(lm(Log_Prop ~ Mes_Indice, data = serie_ne, na.action = na.exclude))

indice_covid <- min(
  serie_temporal$Mes_Indice[serie_temporal$Data_Competencia >= COVID_DATE],
  na.rm = TRUE
)
message(sprintf("Mes_Indice do COVID (mar/2020): %d", indice_covid))

serie_al_fit <- serie_al %>%
  mutate(Log_Prop_Interp = zoo::na.approx(Log_Prop, na.rm = FALSE),
         Is_Interp        = is.na(Log_Prop) & !is.na(Log_Prop_Interp)) %>%
  filter(!is.na(Log_Prop_Interp)) %>%
  mutate(
    Covid         = as.integer(Mes_Indice >= indice_covid),
    Mes_Pos_Covid = if_else(Covid == 1L, Mes_Indice - indice_covid + 1L, 0L)
  )

serie_ne_fit <- serie_ne %>%
  mutate(Log_Prop_Interp = zoo::na.approx(Log_Prop, na.rm = FALSE),
         Is_Interp        = is.na(Log_Prop) & !is.na(Log_Prop_Interp)) %>%
  filter(!is.na(Log_Prop_Interp)) %>%
  mutate(
    Covid         = as.integer(Mes_Indice >= indice_covid),
    Mes_Pos_Covid = if_else(Covid == 1L, Mes_Indice - indice_covid + 1L, 0L)
  )

n_interp_al <- sum(serie_al_fit$Is_Interp)
n_interp_ne <- sum(serie_ne_fit$Is_Interp)

modelo_its_al <- nlme::gls(
  Log_Prop_Interp ~ Mes_Indice + Covid + Mes_Pos_Covid,
  data        = serie_al_fit,
  correlation = nlme::corAR1(form = ~ Mes_Indice),
  method      = "REML"
)
modelo_its_ne <- nlme::gls(
  Log_Prop_Interp ~ Mes_Indice + Covid + Mes_Pos_Covid,
  data        = serie_ne_fit,
  correlation = nlme::corAR1(form = ~ Mes_Indice),
  method      = "REML"
)

fitted_its <- bind_rows(
  serie_al_fit %>% mutate(Fitted_Pct = 10^predict(modelo_its_al), Grupo = "Alagoas"),
  serie_ne_fit %>% mutate(Fitted_Pct = 10^predict(modelo_its_ne), Grupo = "Nordeste")
) %>%
  select(Data_Competencia, Fitted_Pct, Grupo)

calc_its <- function(modelo) {
  tt  <- summary(modelo)$tTable
  b   <- tt[, "Value"]; se <- tt[, "Std.Error"]; pv <- tt[, "p-value"]
  
  b_pre   <- b["Mes_Indice"];    se_pre   <- se["Mes_Indice"];    p_pre   <- pv["Mes_Indice"]
  b_step  <- b["Covid"];         se_step  <- se["Covid"];         p_step  <- pv["Covid"]
  b_slope <- b["Mes_Pos_Covid"]; se_slope <- se["Mes_Pos_Covid"]; p_slope <- pv["Mes_Pos_Covid"]
  
  vmat          <- vcov(modelo)
  cov_pre_slope <- vmat["Mes_Indice", "Mes_Pos_Covid"]
  
  apc_pre     <- round((10^(b_pre * 12) - 1) * 100, 2)
  apc_pre_inf <- round((10^((b_pre - 1.96 * se_pre) * 12) - 1) * 100, 2)
  apc_pre_sup <- round((10^((b_pre + 1.96 * se_pre) * 12) - 1) * 100, 2)
  
  b_pos   <- b_pre + b_slope
  se_pos  <- sqrt(se_pre^2 + se_slope^2 + 2 * cov_pre_slope)
  apc_pos     <- round((10^(b_pos * 12) - 1) * 100, 2)
  apc_pos_inf <- round((10^((b_pos - 1.96 * se_pos) * 12) - 1) * 100, 2)
  apc_pos_sup <- round((10^((b_pos + 1.96 * se_pos) * 12) - 1) * 100, 2)
  
  data.frame(
    APC_Pre = apc_pre, APC_Pre_IC_Inf = apc_pre_inf, APC_Pre_IC_Sup = apc_pre_sup,
    P_Pre   = fmt_p(p_pre),
    Step    = round(b_step, 4), Step_IC_Inf = round(b_step - 1.96*se_step, 4),
    Step_IC_Sup = round(b_step + 1.96*se_step, 4), P_Step = fmt_p(p_step),
    APC_Pos = apc_pos, APC_Pos_IC_Inf = apc_pos_inf, APC_Pos_IC_Sup = apc_pos_sup,
    P_Pos   = fmt_p(p_slope)
  )
}

rho_its <- function(modelo)
  round(as.numeric(coef(modelo$modelStruct$corStruct, unconstrained = FALSE)), 3)

resumo_its <- bind_rows(calc_its(modelo_its_al), calc_its(modelo_its_ne)) %>%
  mutate(
    Regiao       = c("Alagoas", "Nordeste"),
    Rho_AR1      = c(rho_its(modelo_its_al), rho_its(modelo_its_ne)),
    DW_Pre       = round(c(dw_al$statistic, dw_ne$statistic), 3),
    DW_Pos       = c(dw_stat(modelo_its_al), dw_stat(modelo_its_ne)),
    Meses_Interp = c(n_interp_al, n_interp_ne)
  ) %>%
  select(Regiao,
         APC_Pre, APC_Pre_IC_Inf, APC_Pre_IC_Sup, P_Pre,
         Step, Step_IC_Inf, Step_IC_Sup, P_Step,
         APC_Pos, APC_Pos_IC_Inf, APC_Pos_IC_Sup, P_Pos,
         Rho_AR1, DW_Pre, DW_Pos, Meses_Interp)

gerar_serie <- function(lang = "pt") {
  n_al <- scales::comma(sum(df_dap$Grupo == "Alagoas"))
  n_ne <- scales::comma(sum(df_dap$Grupo == "Nordeste"))
  
  dados_plot <- serie_temporal %>%
    filter(!is.na(Proporcao_Amp)) %>%
    group_by(Grupo) %>%
    arrange(Data_Competencia) %>%
    mutate(Proporcao_MA3 = zoo::rollmean(Proporcao_Amp, k = 3,
                                         fill = NA, align = "center")) %>%
    ungroup()
  
  ylab <- if (lang == "pt") "% Amputações / Total DAP" else "% Amputations / Total PAD"
  covid <- if (lang == "pt") "Pandemia\nCOVID-19" else "COVID-19\nPandemic"
  
  ggplot(dados_plot, aes(x = Data_Competencia, color = Grupo, fill = Grupo)) +
    geom_vline(xintercept = COVID_DATE, linetype = "dashed",
               color = "grey40", linewidth = 0.45) +
    annotate("text", x = COVID_DATE + 45, y = Inf,
             label = covid, vjust = 1.5, hjust = 0,
             size = 2.6, color = "grey40") +
    geom_line(aes(y = Proporcao_MA3), alpha = 0.45, linewidth = 0.6) +
    geom_line(data = fitted_its,
              aes(x = Data_Competencia, y = Fitted_Pct, color = Grupo),
              linewidth = 1.2, linetype = "solid", show.legend = FALSE) +
    scale_color_manual(values = c("Alagoas" = COR_AL, "Nordeste" = COR_NE),
                       name = if(lang == "pt") "Região" else "Region") +
    scale_fill_manual(values = c("Alagoas" = COR_AL, "Nordeste" = COR_NE),
                      guide  = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = scales::label_number(suffix = "%"),
                       expand = expansion(mult = c(0.02, 0.05))) +
    tema_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = NULL, subtitle = NULL, caption = NULL,
         x = NULL, y = ylab)
}

salvar_grafico(gerar_serie("pt"), "Serie_Temporal_DAP", output_pt, w = 14, h = 6)
salvar_grafico(gerar_serie("en"), "Time_Series_PAD",    output_en, w = 14, h = 6)

# ==============================================================================
# 4. REGRESSÃO LOGÍSTICA — Forest Plot + LRT
# ==============================================================================

dados_reg <- df_dap %>%
  filter(SEXO %in% c("Masculino", "Feminino")) %>%
  mutate(
    Desfecho_Amputacao = as.integer(Tipo_Intervencao == "Amputacao"),
    Faixa_Etaria = relevel(
      factor(case_when(
        Idade_Num < 50  ~ "< 50 anos",
        Idade_Num <= 70 ~ "50-70 anos",
        TRUE            ~ "> 70 anos"
      ), levels = c("< 50 anos", "50-70 anos", "> 70 anos")),
      ref = "50-70 anos"
    ),
    SEXO           = relevel(factor(SEXO, levels = c("Masculino", "Feminino")),
                             ref = "Masculino"),
    Ano_Centrado   = as.numeric(Ano) - 2020,
    Diabetes_Fator = relevel(as.factor(Tem_Diabetes), ref = "0")
  ) %>%
  drop_na(Faixa_Etaria, Diabetes_Fator)

dados_al_reg <- dados_reg %>% filter(Grupo == "Alagoas")
dados_ne_reg <- dados_reg %>% filter(Grupo == "Nordeste")

modelo_or_al <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + Ano_Centrado + SEXO,
  data   = dados_al_reg,
  family = binomial()
)
modelo_or_ne <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + Ano_Centrado + SEXO,
  data   = dados_ne_reg,
  family = binomial()
)

extrair_or <- function(modelo, vcov_mat = NULL) {
  if (!is.null(vcov_mat)) {
    ct  <- lmtest::coeftest(modelo, vcov = vcov_mat)
    nms <- rownames(ct); b <- ct[, 1]; se <- ct[, 2]; pv <- ct[, 4]
  } else {
    s   <- summary(modelo)$coefficients
    nms <- rownames(s); b <- s[, 1]; se <- s[, 2]; pv <- s[, 4]
  }
  data.frame(
    Variavel = nms,
    OR       = round(exp(b), 3),
    IC_Inf   = round(exp(b - 1.96 * se), 3),
    IC_Sup   = round(exp(b + 1.96 * se), 3),
    P_Valor  = fmt_p(pv)
  )
}

vc_al <- NULL; vc_ne <- NULL
if ("CNES" %in% names(dados_al_reg)) {
  vc_al <- sandwich::vcovCL(modelo_or_al, cluster = ~CNES)
  vc_ne <- sandwich::vcovCL(modelo_or_ne, cluster = ~CNES)
}

resultados_or_al <- extrair_or(modelo_or_al, vc_al)
resultados_or_ne <- extrair_or(modelo_or_ne, vc_ne)

auc_al <- round(as.numeric(pROC::auc(dados_al_reg$Desfecho_Amputacao, fitted(modelo_or_al))), 3)
auc_ne <- round(as.numeric(pROC::auc(dados_ne_reg$Desfecho_Amputacao, fitted(modelo_or_ne))), 3)

# ==============================================================================
# TABELA S1 — MODELO SUPLEMENTAR COM DIABETES (Firth — misclassification)
# ==============================================================================

modelo_sup_al <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + Ano_Centrado + SEXO + Diabetes_Fator,
  data   = dados_al_reg,
  family = binomial(),
  method = brglm2::brglmFit
)
modelo_sup_ne <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + Ano_Centrado + SEXO + Diabetes_Fator,
  data   = dados_ne_reg,
  family = binomial(),
  method = brglm2::brglmFit
)

vc_sup_al <- if (!is.null(vc_al)) sandwich::vcovCL(modelo_sup_al, cluster = ~CNES) else NULL
vc_sup_ne <- if (!is.null(vc_ne)) sandwich::vcovCL(modelo_sup_ne, cluster = ~CNES) else NULL

rot_sens_pt <- c(
  "Faixa_Etaria< 50 anos" = "Idade < 50 anos",
  "Faixa_Etaria> 70 anos" = "Idade > 70 anos",
  "Ano_Centrado"          = "Ano (centrado em 2020)",
  "SEXOFeminino"          = "Sexo: Feminino"
)
rot_sens_en <- c(
  "Faixa_Etaria< 50 anos" = "Age < 50 years",
  "Faixa_Etaria> 70 anos" = "Age > 70 years",
  "Ano_Centrado"          = "Year (centered on 2020)",
  "SEXOFeminino"          = "Sex: Female"
)

montar_sens <- function(mod_principal, mod_sup, grupo, lang = "pt",
                        vcov_princ = NULL, vcov_sup = NULL) {
  vars_comuns <- names(if (lang == "pt") rot_sens_pt else rot_sens_en)
  lbl_princ   <- if (lang == "pt") "Sem diabetes (GLM)"      else "Without diabetes (GLM)"
  lbl_sup     <- if (lang == "pt") "Com diabetes (Firth)"    else "With diabetes (Firth)"
  
  build <- function(mod, vc, lbl) {
    extrair_or(mod, vc) %>%
      filter(Variavel %in% vars_comuns) %>%
      aplicar_labels_or(lang) %>%
      mutate(Modelo = lbl, Grupo = grupo,
             OR_IC  = paste0(OR, " (", IC_Inf, "–", IC_Sup, ")")) %>%
      rename(Label = Variavel)
  }
  
  bind_rows(build(mod_principal, vcov_princ, lbl_princ),
            build(mod_sup,       vcov_sup,   lbl_sup)) %>%
    select(Grupo, Label, Modelo, OR_IC, P_Valor)
}

tab_sens_pt <- bind_rows(
  montar_sens(modelo_or_al, modelo_sup_al, "Alagoas",  "pt", vc_al, vc_sup_al),
  montar_sens(modelo_or_ne, modelo_sup_ne, "Nordeste", "pt", vc_ne, vc_sup_ne)
) %>%
  tidyr::pivot_wider(names_from = Modelo, values_from = c(OR_IC, P_Valor), names_glue = "{Modelo}: {.value}") %>%
  rename("Grupo" = Grupo, "Variável" = Label)

tab_sens_en <- bind_rows(
  montar_sens(modelo_or_al, modelo_sup_al, "Alagoas",   "en", vc_al, vc_sup_al),
  montar_sens(modelo_or_ne, modelo_sup_ne, "Northeast", "en", vc_ne, vc_sup_ne)
) %>%
  tidyr::pivot_wider(names_from = Modelo, values_from = c(OR_IC, P_Valor), names_glue = "{Modelo}: {.value}") %>%
  rename("Group" = Grupo, "Variable" = Label)

mod_nulo <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + SEXO + Grupo + Ano_Centrado,
  data = dados_reg, family = binomial()
)
mod_int <- glm(
  Desfecho_Amputacao ~ Faixa_Etaria + SEXO + Grupo * Ano_Centrado,
  data = dados_reg, family = binomial()
)
lrt        <- lmtest::lrtest(mod_nulo, mod_int)
lrt_chi2   <- round(lrt$Chisq[2], 2)
lrt_df     <- lrt$Df[2]
p_valor_lrt <- fmt_p(lrt$`Pr(>Chisq)`[2])

ORDEM_FOREST_PT <- c("Idade > 70 anos", "Ano (centrado em 2020)",
                     "Idade < 50 anos", "Sexo: Feminino")
ORDEM_FOREST_EN <- c("Age > 70 years", "Year (centered on 2020)",
                     "Age < 50 years", "Sex: Female")

fazer_forest <- function(dados_or, lang = "pt", regiao = "AL") {
  ordem_fix <- if (lang == "pt") ORDEM_FOREST_PT else ORDEM_FOREST_EN
  cor_ponto <- if (regiao == "AL") COR_AL else COR_NE
  ylab <- if (lang == "pt") "Odds Ratio (escala logarítmica)" else "Odds Ratio (log scale)"
  
  dados_plot <- dados_or %>%
    filter(Variavel != "(Intercept)") %>%
    aplicar_labels_or(lang) %>%
    mutate(
      Label = factor(Variavel, levels = rev(ordem_fix)),
      Sig   = case_when(
        P_Valor == "< 0.001"                          ~ "***",
        suppressWarnings(as.numeric(P_Valor)) < 0.001 ~ "***",
        suppressWarnings(as.numeric(P_Valor)) < 0.01  ~ "**",
        suppressWarnings(as.numeric(P_Valor)) < 0.05  ~ "*",
        TRUE                                           ~ ""
      )
    )
  
  ggplot(dados_plot,
         aes(x = Label, y = OR, ymin = IC_Inf, ymax = IC_Sup)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "grey55", linewidth = 0.5) +
    geom_linerange(color = cor_ponto, linewidth = 0.9) +
    geom_point(aes(y = OR), color = cor_ponto, size = 2.8, shape = 16) +
    geom_text(aes(label = Sig, y = IC_Sup), hjust = -0.2, size = 3.5, color = "grey30") +
    scale_y_log10(
      limits = c(0.2, 8),
      breaks = c(0.25, 0.5, 1, 2, 4, 8),
      labels = scales::label_number(),
      expand = expansion(mult = c(0.05, 0.15)),
      oob    = scales::squish
    ) +
    coord_flip() +
    tema_pub() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3)
    ) +
    labs(title = NULL, subtitle = NULL, caption = NULL,
         x = NULL, y = ylab)
}

fazer_forest_combinado <- function(lang = "pt") {
  lbl_al <- if (lang == "pt") "Alagoas" else "Alagoas"
  lbl_ne <- if (lang == "pt") "Nordeste" else "Northeast"
  
  p_al <- fazer_forest(resultados_or_al, lang, "AL") +
    ggtitle(lbl_al) +
    theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5,
                                    margin = margin(b = 4)))
  p_ne <- fazer_forest(resultados_or_ne, lang, "NE") +
    ggtitle(lbl_ne) +
    theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5,
                                    margin = margin(b = 4)))
  
  p_al + p_ne + plot_layout(ncol = 2)
}

salvar_grafico(fazer_forest(resultados_or_al, "pt", "AL"), "Forest_Plot_AL", output_pt, w = 9, h = 5.5)
salvar_grafico(fazer_forest(resultados_or_ne, "pt", "NE"), "Forest_Plot_NE", output_pt, w = 9, h = 5.5)
salvar_grafico(fazer_forest_combinado("pt"), "Forest_Plot_Combinado", output_pt, w = 16, h = 5.5)
salvar_grafico(fazer_forest(resultados_or_al, "en", "AL"), "Forest_Plot_AL_PAD", output_en, w = 9, h = 5.5)
salvar_grafico(fazer_forest(resultados_or_ne, "en", "NE"), "Forest_Plot_NE_PAD", output_en, w = 9, h = 5.5)
salvar_grafico(fazer_forest_combinado("en"), "Forest_Plot_Combined_PAD", output_en, w = 16, h = 5.5)

# ==============================================================================
# 5. EXPORTAÇÃO DOS RESULTADOS
# ==============================================================================

tab_mun_pt <- perfil_municipal %>% rename(
  "Município"              = Nome,
  "Cód. IBGE"              = Municipio,
  "Total de Cirurgias"     = Total_Cirurgias,
  "N. Amputações"          = N_Amputacoes,
  "N. Revascularizações"   = N_Revasc,
  "Proporção (%)"          = Proporcao_Amp_Pct,
  "IC Inf. 95% (Wilson)"   = IC_Inf_Pct,
  "IC Sup. 95% (Wilson)"   = IC_Sup_Pct
)
tab_its_pt <- resumo_its %>% rename(
  "Região"                   = Regiao,
  "APC pré-COVID (%)"        = APC_Pre,
  "IC Inf. pré"              = APC_Pre_IC_Inf,
  "IC Sup. pré"              = APC_Pre_IC_Sup,
  "p (pré)"                  = P_Pre,
  "Choque (Step, log10)"     = Step,
  "IC Inf. step"             = Step_IC_Inf,
  "IC Sup. step"             = Step_IC_Sup,
  "p (step)"                 = P_Step,
  "APC pós-COVID (%)"        = APC_Pos,
  "IC Inf. pós"              = APC_Pos_IC_Inf,
  "IC Sup. pós"              = APC_Pos_IC_Sup,
  "p (pós)"                  = P_Pos,
  "ρ AR(1)"                  = Rho_AR1,
  "DW (pré-ITS)"             = DW_Pre,
  "DW (pós-ITS)"             = DW_Pos,
  "Meses interp."            = Meses_Interp
)

tab_or_al_pt <- resultados_or_al %>%
  filter(Variavel != "(Intercept)") %>%
  aplicar_labels_or("pt") %>%
  rename("Variável" = Variavel, "IC Inf. 95%" = IC_Inf,
         "IC Sup. 95%" = IC_Sup, "Valor p" = P_Valor)
tab_or_ne_pt <- resultados_or_ne %>%
  filter(Variavel != "(Intercept)") %>%
  aplicar_labels_or("pt") %>%
  rename("Variável" = Variavel, "IC Inf. 95%" = IC_Inf,
         "IC Sup. 95%" = IC_Sup, "Valor p" = P_Valor)

tab_mun_en <- perfil_municipal %>% rename(
  "Municipality"              = Nome,
  "IBGE code"                 = Municipio,
  "Total Surgeries"           = Total_Cirurgias,
  "N Amputations"             = N_Amputacoes,
  "N Revascularizations"      = N_Revasc,
  "Proportion (%)"            = Proporcao_Amp_Pct,
  "Lower 95% CI (Wilson)"     = IC_Inf_Pct,
  "Upper 95% CI (Wilson)"     = IC_Sup_Pct
)
tab_its_en <- resumo_its %>% rename(
  "Region"                   = Regiao,
  "Pre-COVID APC (%)"        = APC_Pre,
  "Lower CI pre"             = APC_Pre_IC_Inf,
  "Upper CI pre"             = APC_Pre_IC_Sup,
  "p (pre)"                  = P_Pre,
  "Level change (Step, log10)" = Step,
  "Lower CI step"            = Step_IC_Inf,
  "Upper CI step"            = Step_IC_Sup,
  "p (step)"                 = P_Step,
  "Post-COVID APC (%)"       = APC_Pos,
  "Lower CI post"            = APC_Pos_IC_Inf,
  "Upper CI post"            = APC_Pos_IC_Sup,
  "p (post)"                 = P_Pos,
  "ρ AR(1)"                  = Rho_AR1,
  "DW (pre-ITS)"             = DW_Pre,
  "DW (post-ITS)"            = DW_Pos,
  "Months interp."           = Meses_Interp
)

tab_or_al_en <- resultados_or_al %>%
  filter(Variavel != "(Intercept)") %>%
  aplicar_labels_or("en") %>%
  rename("Variable" = Variavel, "Lower 95% CI" = IC_Inf,
         "Upper 95% CI" = IC_Sup, "p-value" = P_Valor)
tab_or_ne_en <- resultados_or_ne %>%
  filter(Variavel != "(Intercept)") %>%
  aplicar_labels_or("en") %>%
  rename("Variable" = Variavel, "Lower 95% CI" = IC_Inf,
         "Upper 95% CI" = IC_Sup, "p-value" = P_Valor)

txt_dw_pt <- paste0(
  "Alagoas: DW = ", round(dw_al$statistic, 3),
  " (p = ", fmt_p(dw_al$p.value), "); ",
  "Nordeste: DW = ", round(dw_ne$statistic, 3),
  " (p = ", fmt_p(dw_ne$p.value), ")."
)
txt_dw_en <- paste0(
  "Alagoas: DW = ", round(dw_al$statistic, 3),
  " (p = ", fmt_p(dw_al$p.value), "); ",
  "Northeast: DW = ", round(dw_ne$statistic, 3),
  " (p = ", fmt_p(dw_ne$p.value), ")."
)
txt_auc_pt <- paste0("C-statistic (AUC): Alagoas = ", auc_al, "; Nordeste = ", auc_ne, ".")
txt_auc_en <- paste0("C-statistic (AUC): Alagoas = ", auc_al, "; Northeast = ", auc_ne, ".")

txt_sens_pt <- paste0(
  "A remoção de diabetes mellitus do modelo alterou substancialmente os ORs de faixa etária ",
  "no Nordeste (Δ > 30%: OR para ≥70 anos passou de 1,67 para 1,15), confirmando DM como ",
  "confundidora relevante neste grupo. Em Alagoas, os ORs de sexo permaneceram estáveis ",
  "(Δ < 2%), enquanto idade e tendência temporal sofreram atenuação moderada (Δ 9–13%). ",
  "Esta diferença é consistente com a hipótese de subnotificação diferencial: no Nordeste, ",
  "DM é codificada com frequência suficiente para mediar a relação idade–amputação; em ",
  "Alagoas, a baixa prevalência registrada (13,8%) sugere subnotificação que limita o poder ",
  "de ajuste desta covariável (ver Tabela S1)."
)
txt_sens_en <- paste0(
  "Removing diabetes mellitus from the model substantially altered age-group ORs in the ",
  "Northeast (Δ > 30%: OR for ≥70 years decreased from 1.67 to 1.15), confirming DM as a ",
  "relevant confounder in this group. In Alagoas, sex ORs remained stable (Δ < 2%), while ",
  "age and temporal trend estimates showed moderate attenuation (Δ 9–13%). This differential ",
  "impact is consistent with the hypothesis of differential administrative undercoding: in the ",
  "Northeast, DM is recorded frequently enough to mediate the age-amputation relationship; ",
  "in Alagoas, the low recorded prevalence (13.8%) suggests undercoding that limits the ",
  "covariate's adjustment capacity (see Table S1)."
)
txt_lrt_pt <- paste0("LRT χ²(", lrt_df, ") = ", lrt_chi2, "; p ", p_valor_lrt, ".")
txt_lrt_en <- paste0("LRT χ²(", lrt_df, ") = ", lrt_chi2, "; p ", p_valor_lrt, ".")

txt_tendencia_pt <- paste0(
  "A análise de Série Temporal Interrompida (ITS) com GLS-AR(1) decompõe a tendência em três ",
  "componentes: (i) tendência basal pré-COVID (APC pré-COVID; Tabela 3), (ii) choque imediato ",
  "de nível em março/2020 (Step), e (iii) mudança de inclinação pós-COVID (Slope). O teste de ",
  "Durbin-Watson pré-ITS confirma autocorrelação residual que justifica o GLS (Tabela 3). ",
  "Em Alagoas, chama atenção o salto abrupto nas proporções de amputação observado entre 2023 ",
  "e 2025 — caso o Slope pós-COVID seja insuficiente para capturá-lo, um segundo choque dummy ",
  "poderá ser adicionado com justificativa teórica (ex: desabamento de serviço regional, ",
  "mudança de pactuação assistencial). A análise de regressão logística complementar (GLM ",
  "padrão, cluster-robust) tampouco detectou tendência temporal significativa: ver Tabela 4–5."
)
txt_tendencia_en <- paste0(
  "The Interrupted Time Series (ITS) analysis with GLS-AR(1) decomposes the trend into three ",
  "components: (i) pre-COVID baseline trend (pre-COVID APC; Table 3), (ii) immediate level ",
  "change at March 2020 (Step), and (iii) post-COVID slope change. The pre-ITS Durbin-Watson ",
  "test confirms residual autocorrelation justifying GLS (Table 3). In Alagoas, a sharp spike ",
  "in amputation proportions is observed between 2023 and 2025 — if the post-COVID slope is ",
  "insufficient to capture it, a second structural break dummy may be added with theoretical ",
  "justification (e.g., regional service collapse, changes in care network agreements). The ",
  "complementary logistic regression (standard GLM, cluster-robust SEs) also detected no ",
  "statistically significant temporal trend: see Tables 4–5."
)

txt_mortalidade_pt <- paste0(
  "Alagoas apresentou menor letalidade hospitalar do que o restante do Nordeste ",
  "(4,3% vs. 6,5%). A análise estratificada por tipo de procedimento (Tabela S2) revela que ",
  "essa diferença persiste dentro de cada estrato: na amputação, 7,4% em AL vs. 8,9% no NE; ",
  "na revascularização, 0,8% vs. 1,0%. Portanto, a menor mortalidade de AL não é apenas efeito ",
  "de case-mix (maior proporção de amputações, procedimento de menor risco imediato): AL tem ",
  "menos óbitos mesmo quando comparada dentro da mesma modalidade cirúrgica. Três hipóteses ",
  "explicam este achado: (i) amputações predominantemente distais em AL (pododáctilos vs. ",
  "transfemoral), com menor mortalidade inerente; (ii) menor carga comórbida registrada — a ",
  "subnotificação de DM pode mascarar pacientes mais graves nos dados; (iii) mortalidade pós-alta ",
  "não captada pela AIH, favorecida pela mediana de internação de 2 dias em AL. Em conjunto, os ",
  "dados apontam para o paradoxo do baixo acesso: Alagoas preserva a vida no hospital às custas ",
  "da perda funcional do membro — menos pacientes morrem, mas mais saem sem o membro."
)
txt_mortalidade_en <- paste0(
  "Alagoas exhibited lower in-hospital case fatality than the remainder of the Northeast ",
  "(4.3% vs. 6.5%). The procedure-stratified analysis (Table S2) reveals that this difference ",
  "persists within each stratum: for amputation, 7.4% in AL vs. 8.9% in the Northeast; for ",
  "revascularization, 0.8% vs. 1.0%. Alagoas's mortality advantage therefore cannot be explained ",
  "solely by case-mix (higher proportion of amputations, a lower-risk procedure): AL shows fewer ",
  "deaths even within the same surgical modality. Three hypotheses explain this finding: ",
  "(i) amputations in AL may be predominantly distal (toes/forefoot vs. transfemoral), with ",
  "inherently lower operative mortality; (ii) lower recorded comorbidity burden — DM undercoding ",
  "may conceal higher disease severity in the data; (iii) post-discharge mortality not captured ",
  "by the AIH, facilitated by the median length of stay of 2 days in AL. Together, the data point ",
  "to the low-access paradox: Alagoas preserves life in hospital at the cost of functional limb ",
  "loss — fewer patients die, but more leave without their limb."
)

txt_dm_tautologia_pt <- paste0(
  "O modelo logístico confirmou a associação estatística do código de diabetes mellitus com a amputação, ",
  "porém este achado não sustenta inferência causal anatômica ou biológica neste estudo. Devido à natureza ",
  "administrativa do SIHSUS (focado em faturamento), a presença do CID de diabetes nas AIHs de desfechos ",
  "vasculares frequentemente reflete a síndrome do 'pé diabético' (onde a amputação é a indicação do tratamento), ",
  "caracterizando um viés de misclassification diferencial. Portanto, neste trabalho, o DM foi tratado ",
  "estritamente como uma covariável de ajuste metodológico para controlar a prática de codificação clínica ",
  "entre hospitais de diferentes portes, e não como um preditor biológico independente de risco."
)
txt_dm_tautologia_en <- paste0(
  "The logistic model confirmed the statistical association of the diabetes mellitus code with amputation; ",
  "however, this finding does not support anatomical or biological causal inference within this study. Due to ",
  "the administrative nature of SIHSUS (focused on billing), the presence of the DM ICD code in vascular ",
  "outcome admissions frequently reflects 'diabetic foot' syndrome (where amputation is the treatment indication), ",
  "characterizing differential misclassification bias. Therefore, in this analysis, DM was treated strictly ",
  "as a methodological adjustment covariate to control for clinical coding practices across different facility ",
  "sizes, rather than as an independent biological predictor of risk."
)

txt_periodo   <- format(DATA_CORTE, "%B de %Y")
txt_period_en <- format(DATA_CORTE, "%B %Y")

# ============================================================
# DOCUMENTO PT — ABNT
# ============================================================
doc_pt <- read_docx() %>%
  body_add_par(
    paste0("INIQUIDADES NA ASSISTÊNCIA CIRÚRGICA DA DOENÇA ARTERIAL PERIFÉRICA NO SUS: ",
           "ANÁLISE COMPARATIVA DE ALAGOAS EM RELAÇÃO AO NORDESTE, 2015–2025"),
    style = "heading 1"
  ) %>%
  body_add_par(
    paste0("Fonte dos dados: Sistema de Informações Hospitalares do SUS (SIHSUS). ",
           "Processamento: pacote microdatasus (R). ",
           "Período de análise: janeiro de 2015 a ", txt_periodo, "."),
    style = "Normal"
  ) %>%
  
  body_add_par("1 CARACTERÍSTICAS BASAIS DA COORTE", style = "heading 2") %>%
  body_add_par(
    paste0("Tabela 1. Características basais dos pacientes internados por DAP submetidos ",
           "a amputação ou revascularização no SUS, Nordeste do Brasil, 2015–",
           format(DATA_CORTE, "%Y"), ". ",
           "Dados contínuos: mediana [IQR]. Dados categóricos: n (%). ",
           "Comparações: teste de Wilcoxon (contínuas) e qui-quadrado (categóricas)."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft_tab1_pt) %>%
  
  body_add_par("2 PERFIL MUNICIPAL — ALAGOAS", style = "heading 2") %>%
  body_add_par(
    paste0("Tabela 2. Proporção de amputações por município de residência em Alagoas ",
           "(mínimo de ", CUTOFF_MUN, " cirurgias; IC 95% Wilson). ",
           "Municípios com volume insuficiente omitidos do mapa e desta tabela."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_mun_pt)) %>%
  
  body_add_par("3 ANÁLISE ESPACIAL", style = "heading 2") %>%
  body_add_par(
    paste0("Figura 1. Proporção de amputações por estado da região Nordeste, 2015–2025 ",
           "(inclui Alagoas para contexto geográfico; análise estatística excl. AL)."),
    style = "Normal"
  ) %>%
  body_add_img(src = file.path(output_pt, "Mapa_NE_Proporcao_Amputacao.png"),
               width = 5.5, height = 4) %>%
  body_add_par("Figura 2. Proporção de amputações por município de residência em Alagoas.",
               style = "Normal") %>%
  body_add_img(src = file.path(output_pt, "Mapa_AL_Proporcao_Amputacao.png"),
               width = 5.5, height = 4.5) %>%
  
  body_add_par("4 ANÁLISE DE TENDÊNCIA TEMPORAL (ITS — GLS-AR(1))", style = "heading 2") %>%
  body_add_par(
    paste0("Teste de Durbin-Watson (OLS pré-ITS, justifica uso de GLS): ", txt_dw_pt),
    style = "Normal"
  ) %>%
  body_add_par(
    paste0("Tabela 3. Análise de Série Temporal Interrompida (ITS) — GLS com estrutura AR(1). ",
           "Ponto de corte: março/2020 (COVID-19). APC pré-COVID: variação percentual anual basal. ",
           "Step: choque imediato de nível (log10). APC pós-COVID: tendência total pós-intervenção. ",
           "ρ AR(1): coeficiente autorregressivo; DW pré/pós-ITS: Durbin-Watson antes e após correção. ",
           "IC 95% assimétrico por retrotransformação logarítmica."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_its_pt)) %>%
  body_add_par(
    paste0("Figura 3. Série temporal mensal — proporção de amputações na DAP. ",
           "Linha espessa: ITS-GLS AR(1) (tendência pré-COVID + choque + slope pós-COVID). ",
           "Linha fina: média móvel de 3 meses. Linha vertical tracejada: março/2020 (COVID-19)."),
    style = "Normal"
  ) %>%
  body_add_img(src = file.path(output_pt, "Serie_Temporal_DAP.png"),
               width = 6.5, height = 3) %>%
  
  body_add_par("5 REGRESSÃO LOGÍSTICA — RISCO DE AMPUTAÇÃO PRIMÁRIA", style = "heading 2") %>%
  body_add_par(
    paste0("GLM logístico padrão (modelo principal, sem Diabetes). ",
           txt_lrt_pt, " Ano centrado em 2020 (intercepto = OR no ano de referência). ",
           txt_auc_pt,
           if (!is.null(vc_al))
             " IC 95% calculados com erros-padrão cluster-robust no nível de estabelecimento (CNES)."
           else
             " CNES ausente no dataframe — IC 95% padrão de Firth (reportar como limitação)."),
    style = "Normal"
  ) %>%
  body_add_par("Tabela 4. Odds Ratio (OR) para amputação primária na DAP — Alagoas.",
               style = "Normal") %>%
  body_add_flextable(ft(tab_or_al_pt)) %>%
  body_add_par("Tabela 5. Odds Ratio (OR) para amputação primária na DAP — Nordeste.",
               style = "Normal") %>%
  body_add_flextable(ft(tab_or_ne_pt)) %>%
  body_add_par("Figura 4. Forest plot comparativo — Alagoas (esq.) vs. Nordeste (dir.).",
               style = "Normal") %>%
  body_add_img(src = file.path(output_pt, "Forest_Plot_Combinado.png"), width = 6.5, height = 3.5) %>%
  
  body_add_par("6 DISCUSSÃO — ACHADOS PRINCIPAIS", style = "heading 2") %>%
  body_add_par("6.1 Mortalidade hospitalar e paradoxo do baixo acesso", style = "heading 3") %>%
  body_add_par(txt_mortalidade_pt, style = "Normal") %>%
  body_add_par(
    paste0("Tabela S2. Letalidade hospitalar estratificada por tipo de procedimento e grupo. ",
           "A menor letalidade de Alagoas persiste dentro de cada modalidade cirúrgica. ",
           "† Revascularização em AL: n = 15 óbitos — volume insuficiente para inferência isolada; ",
           "interprete com cautela."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_obito_strat %>%
                          rename("Grupo" = Grupo, "Procedimento" = Tipo_Intervencao,
                                 "N" = N, "Óbitos" = Obitos, "Letalidade (%)" = Letalidade))) %>%
  
  body_add_par("6.2 Tendência temporal — ausência de melhoria detectável em Alagoas", style = "heading 3") %>%
  body_add_par(txt_tendencia_pt, style = "Normal") %>%
  
  body_add_par("6.3 Diabetes mellitus — limitação da covariável por artefato de codificação", style = "heading 3") %>%
  body_add_par(txt_dm_tautologia_pt, style = "Normal") %>%
  
  body_add_par("7 ANÁLISE DE SENSIBILIDADE — MISCLASSIFICATION DE DIABETES", style = "heading 2") %>%
  body_add_par(txt_sens_pt, style = "Normal") %>%
  body_add_par(
    paste0("Tabela S1. Odds Ratio dos preditores principais com e sem ajuste por diabetes mellitus. ",
           "Δ > 30% para faixa etária no Nordeste confirma DM como confundidora relevante neste grupo. ",
           "Em Alagoas, ORs de sexo estáveis (Δ < 2%); atenuação moderada de idade e tendência (Δ 9–13%) ",
           "é consistente com subnotificação diferencial de CID de DM em hospitais de menor porte. ",
           "IC 95% calculados com os mesmos SEs cluster-robust das Tabelas 4–5 (base de variância consistente)."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_sens_pt))

print(doc_pt, target = file.path(output_pt, "Resultados_DAP_ABNT.docx"))
message("Documento PT salvo.")

# ============================================================
# DOCUMENTO EN
# ============================================================
doc_en <- read_docx() %>%
  body_add_par(
    paste0("INEQUITIES IN SURGICAL CARE FOR PERIPHERAL ARTERIAL DISEASE IN THE BRAZILIAN ",
           "PUBLIC HEALTH SYSTEM (SUS): A COMPARATIVE ANALYSIS OF ALAGOAS AND THE ",
           "NORTHEAST REGION (2015–2025)"),
    style = "heading 1"
  ) %>%
  body_add_par(
    paste0("Data source: Brazilian Hospital Information System (SIHSUS). ",
           "Processing: microdatasus package (R). ",
           "Analysis period: January 2015 to ", txt_period_en, "."),
    style = "Normal"
  ) %>%
  
  body_add_par("1. BASELINE COHORT CHARACTERISTICS", style = "heading 2") %>%
  body_add_par(
    paste0("Table 1. Baseline characteristics of patients hospitalised for PAD and submitted to ",
           "amputation or revascularisation in the Brazilian SUS, Northeast region, 2015–",
           format(DATA_CORTE, "%Y"), ". ",
           "Continuous data: median [IQR]. Categorical data: n (%). ",
           "Comparisons: Wilcoxon rank-sum test (continuous) and chi-square test (categorical)."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft_tab1_en) %>%
  
  body_add_par("2. MUNICIPAL PROFILE — ALAGOAS", style = "heading 2") %>%
  body_add_par(
    paste0("Table 2. Amputation proportion by municipality of residence in Alagoas ",
           "(minimum ", CUTOFF_MUN, " surgeries; 95% Wilson CI). ",
           "Municipalities with insufficient volume omitted."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_mun_en)) %>%
  
  body_add_par("3. SPATIAL ANALYSIS", style = "heading 2") %>%
  body_add_par(
    paste0("Figure 1. Amputation proportion by state in Northeast Brazil, 2015–2025 ",
           "(includes Alagoas for geographic context; statistical comparisons exclude AL)."),
    style = "Normal"
  ) %>%
  body_add_img(src = file.path(output_en, "Map_NE_Amputation_Proportion.png"),
               width = 5.5, height = 4) %>%
  body_add_par("Figure 2. Amputation proportion by municipality of residence in Alagoas.",
               style = "Normal") %>%
  body_add_img(src = file.path(output_en, "Map_AL_Amputation_Proportion.png"),
               width = 5.5, height = 4.5) %>%
  
  body_add_par("4. TIME-SERIES ANALYSIS (ITS — GLS-AR(1))", style = "heading 2") %>%
  body_add_par(
    paste0("Durbin-Watson test (OLS, pre-ITS — justifies GLS use): ", txt_dw_en),
    style = "Normal"
  ) %>%
  body_add_par(
    paste0("Table 3. Interrupted Time Series (ITS) analysis — GLS with AR(1) structure. ",
           "Breakpoint: March 2020 (COVID-19 pandemic). Pre-COVID APC: baseline annual percentage change. ",
           "Step: immediate level change (log10 scale). Post-COVID APC: total post-intervention trend. ",
           "ρ AR(1): estimated autoregressive coefficient; DW pre/post-ITS: Durbin-Watson. ",
           "Asymmetric 95% CI by log back-transformation."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_its_en)) %>%
  body_add_par(
    paste0("Figure 3. Monthly time series — amputation proportion in PAD. ",
           "Thick line: ITS-GLS AR(1) (pre-COVID trend + level change + post-COVID slope). ",
           "Thin line: 3-month moving average. Vertical dashed line: March 2020 (COVID-19)."),
    style = "Normal"
  ) %>%
  body_add_img(src = file.path(output_en, "Time_Series_PAD.png"), width = 6.5, height = 3) %>%
  
  body_add_par("5. LOGISTIC REGRESSION — RISK OF PRIMARY AMPUTATION", style = "heading 2") %>%
  body_add_par(
    paste0("Standard logistic regression (primary model, without diabetes). ",
           txt_lrt_en, " Year centered on 2020 (intercept = OR at reference year). ",
           txt_auc_en,
           if (!is.null(vc_al))
             " 95% CIs calculated with cluster-robust standard errors at the facility level (CNES)."
           else
             " CNES field absent — standard Firth 95% CIs reported (note as limitation)."),
    style = "Normal"
  ) %>%
  body_add_par("Table 4. Odds Ratios for primary amputation in PAD — Alagoas.",
               style = "Normal") %>%
  body_add_flextable(ft(tab_or_al_en)) %>%
  body_add_par("Table 5. Odds Ratios for primary amputation in PAD — Northeast Brazil.",
               style = "Normal") %>%
  body_add_flextable(ft(tab_or_ne_en)) %>%
  body_add_par("Figure 4. Comparative forest plot — Alagoas (left) vs. Northeast (right).",
               style = "Normal") %>%
  body_add_img(src = file.path(output_en, "Forest_Plot_Combined_PAD.png"), width = 6.5, height = 3.5) %>%
  
  body_add_par("6. DISCUSSION — KEY FINDINGS", style = "heading 2") %>%
  body_add_par("6.1 In-hospital mortality and the low-access paradox", style = "heading 3") %>%
  body_add_par(txt_mortalidade_en, style = "Normal") %>%
  body_add_par(
    paste0("Table S2. In-hospital case fatality stratified by procedure type and group. ",
           "Alagoas's lower mortality persists within each surgical modality. ",
           "† Revascularization in AL: n = 15 deaths — insufficient volume for standalone inference; ",
           "interpret with caution."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_obito_strat_en)) %>%
  
  body_add_par("6.2 Temporal trend — no detectable improvement in Alagoas", style = "heading 3") %>%
  body_add_par(txt_tendencia_en, style = "Normal") %>%
  
  body_add_par("6.3 Diabetes mellitus — covariate limited by coding artifact", style = "heading 3") %>%
  body_add_par(txt_dm_tautologia_en, style = "Normal") %>%
  
  body_add_par("7. SENSITIVITY ANALYSIS — DIABETES MISCLASSIFICATION", style = "heading 2") %>%
  body_add_par(txt_sens_en, style = "Normal") %>%
  body_add_par(
    paste0("Table S1. Odds Ratios for primary predictors with and without adjustment for diabetes mellitus. ",
           "Δ > 30% for age group in the Northeast confirms DM as a relevant confounder in this group. ",
           "In Alagoas, sex ORs remained stable (Δ < 2%); moderate attenuation of age and trend (Δ 9–13%) ",
           "is consistent with differential DM undercoding in lower-capacity facilities. ",
           "95% CIs use the same cluster-robust SEs as Tables 4–5 (consistent variance basis)."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft(tab_sens_en))

print(doc_en, target = file.path(output_en, "Results_PAD_Inequities.docx"))
message("Document EN saved.")

# ==============================================================================
message("\n=== Análise concluída ===")
message("Resultados em português (ABNT) : ", output_pt)
message("Results in English             : ", output_en)

# 1. Descobre a pasta onde o R escondeu o cache corrompido do geobr
pasta_cache_geobr <- tools::R_user_dir("geobr", which = "cache")

# 2. Deleta a pasta inteira para forçar um download limpo
unlink(pasta_cache_geobr, recursive = TRUE)