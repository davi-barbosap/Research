# ==============================================================================
# SCRIPT 02: RAIO-X DA QUALIDADE (MIQ) — CITOPATOLÓGICO ALAGOAS
# ==============================================================================

library(data.table)   # leitura e filtro pesado
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

caminho_cito_rds <- "/Users/davibarbosa/Desktop/IC/Citopatológico/Unificado/Base_Cito_Nordeste_15_25.rds"

# Leitura e isolamento de Alagoas em sequência imediata
dados <- readRDS(caminho_cito_rds)
dados_al_miq <- as_tibble(dados[CO_UF_PREST_SERVICO == 27])
rm(dados); gc()   # 14 GB liberados antes de qualquer análise


# ==============================================================================
# PARTE 1: INDICADORES DO INCA POR LABORATÓRIO
# ==============================================================================

tabela1_miq_labs <- dados_al_miq %>%
  group_by(CO_PREST_SERVICO) %>%
  summarise(
    Volume_Total    = n(),
    Satisfatorias   = sum(NU_AMOSTRA_SATISFATORIA == 1,                     na.rm = TRUE),
    Insatisfatorias = sum(NU_AMOSTRA_REJEITADA == 1,                        na.rm = TRUE),
    Normais         = sum(ST_DENTRO_LIMITE_NORMALIDADE %in% c("S", "1", 1), na.rm = TRUE),
    ASC_US          = sum(TP_CEL_ESCAM_SIGN_INDETER %in% c("S", "1", 1),   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Volume_Total > 50) %>%
  mutate(
    Alterados           = pmax(Satisfatorias - Normais, 0L),
    Pct_Insatisfatorias = round((Insatisfatorias / Volume_Total) * 100, 2),
    Pct_Positividade    = ifelse(Satisfatorias > 0,
                                 round((Alterados / Satisfatorias) * 100, 2), NA_real_),
    Pct_ASCUS           = ifelse(Alterados > 0,
                                 round((ASC_US / Alterados) * 100, 2), NA_real_)
  ) %>%
  arrange(desc(Volume_Total))

print(
  tabela1_miq_labs %>%
    select(CO_PREST_SERVICO, Volume_Total,
           Pct_Insatisfatorias, Pct_Positividade, Pct_ASCUS) %>%
    head(10)
)


# ==============================================================================
# PARTE 2: CAUSA-RAIZ DAS REJEIÇÕES
# ==============================================================================

tabela2_causas <- dados_al_miq %>%
  filter(NU_AMOSTRA_REJEITADA == 1) %>%
  summarise(
    Total_Rejeitadas     = n(),
    Dessecamento         = sum(CO_INS_ARTEF_DES     %in% c("S", "1", 1), na.rm = TRUE),
    Sangue               = sum(CO_INS_SANGUE         %in% c("S", "1", 1), na.rm = TRUE),
    Piocitos             = sum(CO_INS_PIOCITOS        %in% c("S", "1", 1), na.rm = TRUE),
    Intensa_Superposicao = sum(CO_INS_INTENSA_SUPER  %in% c("S", "1", 1), na.rm = TRUE),
    Pouco_Material       = sum(CO_INS_MAT_CELULAR    %in% c("S", "1", 1), na.rm = TRUE),
    Contaminacao         = sum(CO_INS_CONTAM_EXTERNOS %in% c("S", "1", 1), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = Dessecamento:Contaminacao,
               names_to = "Motivo", values_to = "Quantidade") %>%
  mutate(Porcentagem = round((Quantidade / Total_Rejeitadas) * 100, 2)) %>%
  arrange(desc(Quantidade))

print(tabela2_causas)


# ==============================================================================
# PARTE 3: REGRESSÃO LOGÍSTICA — FALHA NA COLETA POR FAIXA ETÁRIA
# Nota: ajuste o nome da coluna de idade se o campo no CSV for diferente
#       de CO_IDADE_PACIENTE (algumas exportações do SISCAN usam "IDADE")
# ==============================================================================

dados_regressao <- dados_al_miq %>%
  filter(!is.na(CO_IDADE_PACIENTE)) %>%
  mutate(
    Falha_Coleta = as.integer(NU_AMOSTRA_REJEITADA == 1),
    Faixa_Etaria = case_when(
      CO_IDADE_PACIENTE < 25                                  ~ "< 25 anos",
      CO_IDADE_PACIENTE >= 25 & CO_IDADE_PACIENTE <= 64      ~ "25 a 64 anos (Alvo)",
      CO_IDADE_PACIENTE > 64                                  ~ "> 64 anos (Idosas)"
    ),
    Faixa_Etaria = relevel(factor(Faixa_Etaria), ref = "25 a 64 anos (Alvo)")
  )

modelo_falha <- glm(Falha_Coleta ~ Faixa_Etaria,
                    data = dados_regressao, family = binomial(link = "logit"))
rm(dados_regressao); gc()

tabela3_or <- tidy(modelo_falha, exponentiate = TRUE, conf.int = TRUE) %>%
  select(Termo = term, Odds_Ratio = estimate,
         IC_Inferior = conf.low, IC_Superior = conf.high, P_Valor = p.value) %>%
  mutate(across(c(Odds_Ratio, IC_Inferior, IC_Superior), ~ round(., 2)))

print(tabela3_or)
rm(modelo_falha); gc()


# ==============================================================================
# PARTE 4: CORRELAÇÃO ECOLÓGICA — TEMPO DE ENTREGA vs. POSITIVIDADE (Spearman)
# ==============================================================================

tempo_por_lab <- dados_al_miq %>%
  group_by(CO_PREST_SERVICO) %>%
  summarise(
    Tempo_Mediano_Dias = median(as.numeric(CO_TEMPO_EXAME), na.rm = TRUE),
    .groups = "drop"
  )

correlacao_dados <- tabela1_miq_labs %>%
  left_join(tempo_por_lab, by = "CO_PREST_SERVICO") %>%
  filter(!is.na(Tempo_Mediano_Dias), !is.na(Pct_Positividade))

rm(tempo_por_lab); gc()

teste_correlacao <- cor.test(correlacao_dados$Pct_Positividade,
                             correlacao_dados$Tempo_Mediano_Dias,
                             method = "spearman")

print(data.frame(
  rho     = round(teste_correlacao$estimate, 3),
  p_valor = round(teste_correlacao$p.value, 4),
  conclusao = ifelse(
    teste_correlacao$p.value < 0.05,
    "Associação significativa entre positividade e tempo de entrega",
    "Sem correlação significativa entre tempo de espera e positividade"
  )
))
rm(correlacao_dados); gc()


# ==============================================================================
# PARTE 5: SÉRIE TEMPORAL — EVOLUÇÃO DA QUALIDADE (2015–2025)
# ==============================================================================

tendencia_tempo <- dados_al_miq %>%
  filter(!is.na(CO_ANO_MES_LIBERACAO), CO_ANO_MES_LIBERACAO >= 201501) %>%
  group_by(CO_ANO_MES_LIBERACAO) %>%
  summarise(
    Total           = n(),
    Insatisfatorias = sum(NU_AMOSTRA_REJEITADA == 1,                        na.rm = TRUE),
    Satisfatorias   = sum(NU_AMOSTRA_SATISFATORIA == 1,                     na.rm = TRUE),
    Normais         = sum(ST_DENTRO_LIMITE_NORMALIDADE %in% c("S", "1", 1), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Alterados           = pmax(Satisfatorias - Normais, 0L),
    Pct_Insatisfatorias = (Insatisfatorias / Total) * 100,
    Pct_Positividade    = ifelse(Satisfatorias > 0,
                                 (Alterados / Satisfatorias) * 100, NA_real_),
    Data = as.Date(paste0(
      substr(as.character(CO_ANO_MES_LIBERACAO), 1, 4), "-",
      substr(as.character(CO_ANO_MES_LIBERACAO), 5, 6), "-01"
    ))
  )

rm(dados_al_miq); gc()   # todas as partes já foram calculadas

print(
  ggplot(tendencia_tempo, aes(x = Data)) +
    geom_line(aes(y = Pct_Positividade,    color = "Índice de Positividade (%)"),    linewidth = 1) +
    geom_line(aes(y = Pct_Insatisfatorias, color = "Amostras Insatisfatórias (%)"), linewidth = 1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red",  linewidth = 0.7) +
    geom_hline(yintercept = 3, linetype = "dashed", color = "blue", linewidth = 0.7) +
    annotate("text", x = min(tendencia_tempo$Data, na.rm = TRUE),
             y = 5.4, label = "Limite máx. insatisfatórias (INCA: 5%)",
             hjust = 0, size = 3, color = "red") +
    annotate("text", x = min(tendencia_tempo$Data, na.rm = TRUE),
             y = 3.4, label = "Limite mín. positividade (INCA: 3%)",
             hjust = 0, size = 3, color = "blue") +
    scale_color_manual(values = c("Índice de Positividade (%)"    = "#2c3e50",
                                  "Amostras Insatisfatórias (%)" = "#e74c3c")) +
    labs(
      title   = "Evolução da Qualidade Diagnóstica — Câncer do Colo do Útero em Alagoas (2015–2025)",
      x       = NULL,
      y       = "Taxa (%)",
      color   = "Indicador MIQ",
      caption = "Fonte: SISCAN/DATASUS. Linhas tracejadas = metas INCA. Elaboração própria, 2026."
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 12))
)

rm(tendencia_tempo); gc()

dados_temp <- readRDS(caminho_cito_rds)

# ANTES (campo vazio no SISCAN):
dados_al_miq <- as_tibble(dados[CO_UF_PREST_SERVICO == 27])

# DEPOIS (campo correto — UF da unidade executora):
dados_al_miq <- as_tibble(dados[CO_UF_UNIDADE_SAUDE == 27])