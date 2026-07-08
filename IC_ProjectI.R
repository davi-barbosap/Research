# ==============================================================================
# PROJETO IC 1: ATENÇÃO PRIMÁRIA E QUALIDADE PRÉ-ANALÍTICA (REVISADO PÓS-BANCA)
# Foco: Alagoas (CO_UF_RESIDENCIA == 27)
# Benchmark: Nordeste excluindo Alagoas
#
# Correções pós-banca:
#   (1) Bug 2018 corrigido: CO_IDADE_PACIENTE é CHARACTER zero-padded em 2018
#       ("017", "030", "064"). Comparação numérica sobre string retornava
#       100% na faixa "< 25 anos". Solução: as.numeric(CO_IDADE_PACIENTE)
#       converte corretamente "017" → 17; 2018 mantido na análise.
#   (2) Fator etário em ordem natural: < 25 → 25–64 → > 64
#   (3) Forest plot em escala logarítmica (ORs são multiplicativos)
#   (4) Labels do forest plot limpos (sem sintaxe R interna)
#   (5) Reordenação forest plot: faixas no topo, anos cronológicos abaixo
#   (6) Limitação UF residência ≠ UF atendimento explicitada no DOCX
#   (7) Modelo pooled com teste LRT de interação Grupo × Ano
#   (8) Fig 3: eixo Y com zoom ao intervalo real + Qualicito anotado
#
# Limitação estrutural (declarar no artigo): CO_UF_PREST_SERVICO indisponível
# na base exportada. Filtro por CO_UF_RESIDENCIA classifica pacientes alagoanas
# atendidas fora do estado como NE — possível atenuação do efeito de exposição.
# ==============================================================================

# --- PARÂMETRO: pasta de saída ---
output_dir <- "/Users/davibarbosa/Desktop/IC/IC1"   # ← altere aqui

# --- BIBLIOTECAS ---
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(ragg)
library(officer)
library(flextable)

# --- FUNÇÃO: salvar gráfico em TIFF e PNG (300 dpi) ---
# ragg::agg_png / agg_tiff renderizam UTF-8 corretamente
# independente do locale do sistema (LANG="C" no ambiente Rscript).
salvar_grafico <- function(plot, nome_base, w = 10, h = 6) {
  ragg::agg_tiff(
    filename    = file.path(output_dir, paste0(nome_base, ".tiff")),
    width = w, height = h, units = "in", res = 300, compression = "lzw"
  )
  print(plot)
  dev.off()
  
  ragg::agg_png(
    filename = file.path(output_dir, paste0(nome_base, ".png")),
    width = w, height = h, units = "in", res = 300
  )
  print(plot)
  dev.off()
}

# --- FUNÇÃO: formatar tabela para DOCX ---
ft <- function(df) {
  flextable(df) %>%
    theme_vanilla() %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header")
}

# ==============================================================================
# DADOS
# ==============================================================================

dados <- readRDS("/Users/davibarbosa/Desktop/IC/Dados/Citopatológico/Unificado/Base_Cito_Nordeste_15_25.rds")

dados <- dados %>%
  mutate(
    Grupo     = if_else(CO_UF_RESIDENCIA == 27, "Alagoas", "NE (excl. AL)"),
    # CO_IDADE_PACIENTE é CHARACTER em todos os anos; em 2018 os valores são
    # zero-padded ("017", "030") — comparação numérica direta retorna resultado
    # incorreto. as.numeric() normaliza todos os anos corretamente.
    Idade_Num = as.numeric(CO_IDADE_PACIENTE)
  )

# ==============================================================================
# 1. ADEQUAÇÃO ETÁRIA
# ==============================================================================

niveis_faixa <- c("< 25 anos (não recomendado)",
                  "25 a 64 anos (alvo)",
                  "> 64 anos (reavaliação indicada)")

# Subsetting de colunas antes do mutate evita cópia de 6,9 GB × 86 colunas
tmp_etaria <- dados[, c("Grupo", "Idade_Num", "CO_ANO_LIBERACAO")]

tabela_adequacao <- tmp_etaria %>%
  mutate(Faixa_Diretriz = factor(case_when(
    Idade_Num < 25                    ~ "< 25 anos (não recomendado)",
    Idade_Num >= 25 & Idade_Num <= 64 ~ "25 a 64 anos (alvo)",
    Idade_Num > 64                    ~ "> 64 anos (reavaliação indicada)"
  ), levels = niveis_faixa)) %>%
  filter(!is.na(Faixa_Diretriz)) %>%
  group_by(Grupo, Faixa_Diretriz) %>%
  summarise(Total_Exames = n(), .groups = "drop") %>%
  group_by(Grupo) %>%
  mutate(Proporcao_Pct = round(Total_Exames / sum(Total_Exames) * 100, 2)) %>%
  ungroup()

tabela_adequacao_wide <- tabela_adequacao %>%
  select(Faixa_Diretriz, Grupo, Proporcao_Pct) %>%
  pivot_wider(names_from = Grupo, values_from = Proporcao_Pct)

niveis_faixa_curto <- c("< 25 anos", "25–64 anos", "> 64 anos")

tendencia_idade <- tmp_etaria %>%
  mutate(Faixa_Etaria = factor(case_when(
    Idade_Num < 25                    ~ "< 25 anos",
    Idade_Num >= 25 & Idade_Num <= 64 ~ "25–64 anos",
    Idade_Num > 64                    ~ "> 64 anos"
  ), levels = niveis_faixa_curto)) %>%
  filter(!is.na(Faixa_Etaria)) %>%
  group_by(Grupo, CO_ANO_LIBERACAO, Faixa_Etaria) %>%
  summarise(Total = n(), .groups = "drop") %>%
  group_by(Grupo, CO_ANO_LIBERACAO) %>%
  mutate(Pct = round(Total / sum(Total) * 100, 2)) %>%
  ungroup()

rm(tmp_etaria)

# ==============================================================================
# 2. TAXA GLOBAL DE FALHA PRÉ-ANALÍTICA
# ==============================================================================

tmp_avaliacao <- dados[, c("Grupo", "TP_AVALIACAO_AMOSTRA", "CO_ANO_LIBERACAO")]

taxa_global <- tmp_avaliacao %>%
  group_by(Grupo) %>%
  summarise(
    Total                = n(),
    N_Satisfatorias      = sum(TP_AVALIACAO_AMOSTRA == 2),
    N_Insatisfatorias    = sum(TP_AVALIACAO_AMOSTRA == 3),
    N_Rejeitadas         = sum(TP_AVALIACAO_AMOSTRA == 1),
    Taxa_Satisf_Pct      = round(N_Satisfatorias   / Total * 100, 3),
    Taxa_Insatis_Pct     = round(N_Insatisfatorias / Total * 100, 3),
    Taxa_Rejeit_Pct      = round(N_Rejeitadas      / Total * 100, 3),
    Taxa_Falha_Total_Pct = round((N_Insatisfatorias + N_Rejeitadas) / Total * 100, 3),
    .groups              = "drop"
  )

taxa_por_ano <- tmp_avaliacao %>%
  group_by(Grupo, Ano = CO_ANO_LIBERACAO) %>%
  summarise(
    Total             = n(),
    N_Insatisfatorias = sum(TP_AVALIACAO_AMOSTRA == 3),
    N_Rejeitadas      = sum(TP_AVALIACAO_AMOSTRA == 1),
    Taxa_Insatis_Pct  = round(N_Insatisfatorias / Total * 100, 3),
    Taxa_Rejeit_Pct   = round(N_Rejeitadas      / Total * 100, 3),
    .groups           = "drop"
  )

rm(tmp_avaliacao)

# ==============================================================================
# 3. CAUSA-RAIZ: REJEITADAS
# ==============================================================================

tmp_rej <- dados[which(dados$TP_AVALIACAO_AMOSTRA == 1),
                 c("Grupo", "CO_REJ_AUSENCIA", "CO_REJ_LAMINA",
                   "CO_REJ_CAUSA_ALHEIA", "CO_REJ_OUT_CAUSAS")]

causas_rejeicao <- tmp_rej %>%
  group_by(Grupo) %>%
  summarise(
    Total        = n(),
    Ausencia_Id  = sum(CO_REJ_AUSENCIA     == 1, na.rm = TRUE),
    Lamina       = sum(CO_REJ_LAMINA       == 1, na.rm = TRUE),
    Causa_Alheia = sum(CO_REJ_CAUSA_ALHEIA == 1, na.rm = TRUE),
    Outras       = sum(CO_REJ_OUT_CAUSAS   == 1, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  pivot_longer(cols = Ausencia_Id:Outras, names_to = "Causa", values_to = "N") %>%
  mutate(
    Pct   = round(N / Total * 100, 2),
    Causa = dplyr::recode(Causa,
                          Ausencia_Id  = "Ausência de identificação",
                          Lamina       = "Lâmina danificada ou ausente",
                          Causa_Alheia = "Causa alheia ao usuário",
                          Outras       = "Outras causas"
    )
  ) %>%
  select(-Total)
rm(tmp_rej)

causas_rejeicao_wide <- causas_rejeicao %>%
  select(Causa, Grupo, Pct) %>%
  pivot_wider(names_from = Grupo, values_from = Pct)

# ==============================================================================
# 4. CAUSA-RAIZ: INSATISFATÓRIAS
# ==============================================================================

tmp_ins <- dados[which(dados$TP_AVALIACAO_AMOSTRA == 3),
                 c("Grupo", "CO_INS_MAT_CELULAR", "CO_INS_ARTEF_DES",
                   "CO_INS_SANGUE", "CO_INS_PIOCITOS", "CO_INS_OUTROS",
                   "CO_INS_INTENSA_SUPER", "CO_INS_CONTAM_EXTERNOS")]

causas_insatis <- tmp_ins %>%
  group_by(Grupo) %>%
  summarise(
    Total        = n(),
    Pouca_Celula = sum(CO_INS_MAT_CELULAR     == 1, na.rm = TRUE),
    Dessecamento = sum(CO_INS_ARTEF_DES       == 1, na.rm = TRUE),
    Sangue       = sum(CO_INS_SANGUE          == 1, na.rm = TRUE),
    Piocitos     = sum(CO_INS_PIOCITOS        == 1, na.rm = TRUE),
    Outras       = sum(CO_INS_OUTROS          == 1, na.rm = TRUE),
    Superposicao = sum(CO_INS_INTENSA_SUPER   == 1, na.rm = TRUE),
    Contaminacao = sum(CO_INS_CONTAM_EXTERNOS == 1, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  pivot_longer(cols = Pouca_Celula:Contaminacao, names_to = "Causa", values_to = "N") %>%
  mutate(
    Pct   = round(N / Total * 100, 2),
    Causa = dplyr::recode(Causa,
                          Pouca_Celula = "Material celular insuficiente",
                          Dessecamento = "Artefato de dessecamento",
                          Sangue       = "Infiltrado intenso de sangue",
                          Piocitos     = "Infiltrado intenso de piócitos",
                          Outras       = "Outras causas",
                          Superposicao = "Superposição celular intensa",
                          Contaminacao = "Contaminação por elementos externos"
    )
  ) %>%
  select(-Total)
rm(tmp_ins)

causas_insatis_wide <- causas_insatis %>%
  select(Causa, Grupo, Pct) %>%
  pivot_wider(names_from = Grupo, values_from = Pct)

# ==============================================================================
# 5. REGRESSÃO LOGÍSTICA — modelos separados AL e NE
# Extrair subsets com apenas as colunas necessárias antes de rm(dados)
# NE: amostra aleatória de 500K (18.1M linhas; restrição computacional)
# ==============================================================================

prep_reg <- function(df) {
  df %>%
    mutate(
      Falha        = as.integer(TP_AVALIACAO_AMOSTRA == 3),
      Faixa_Etaria = case_when(
        Idade_Num < 25                    ~ "< 25",
        Idade_Num >= 25 & Idade_Num <= 64 ~ "25-64",
        Idade_Num > 64                    ~ "> 64"
      ),
      Ano = relevel(as.factor(CO_ANO_LIBERACAO), ref = "2015")
    ) %>%
    filter(!is.na(Faixa_Etaria)) %>%
    mutate(Faixa_Etaria = relevel(as.factor(Faixa_Etaria), ref = "25-64")) %>%
    select(Falha, Faixa_Etaria, Ano, Grupo)
}

# Indexação base R: evita cópia de 18,1M linhas × todas as colunas
# que o dplyr::filter() criaria antes do slice_sample
colunas_reg <- c("TP_AVALIACAO_AMOSTRA", "Idade_Num", "CO_ANO_LIBERACAO", "Grupo")

al_mask      <- with(dados, CO_UF_RESIDENCIA == 27 &
                       TP_MOTIVO_EXAME == 1 &
                       TP_AVALIACAO_AMOSTRA %in% c(2L, 3L))
dados_reg_al <- dados[which(al_mask), ..colunas_reg] %>% prep_reg()
rm(al_mask)

set.seed(42)
ne_mask      <- with(dados, CO_UF_RESIDENCIA != 27 &
                       TP_MOTIVO_EXAME == 1 &
                       TP_AVALIACAO_AMOSTRA %in% c(2L, 3L))
ne_idx       <- which(ne_mask); rm(ne_mask)
ne_samp      <- sample(ne_idx, min(500000L, length(ne_idx))); rm(ne_idx)
dados_reg_ne <- dados[ne_samp, ..colunas_reg] %>% prep_reg()
rm(ne_samp, colunas_reg)

rm(dados); gc()

# Guardar contagens antes de criar o pooled — evita manter três datasets
# simultâneos (al 1,47M + ne 500K + pooled 2M = estouro de 16 GB RAM)
n_falhas_al <- sum(dados_reg_al$Falha)
n_falhas_ne <- sum(dados_reg_ne$Falha)

modelo_al <- glm(Falha ~ Faixa_Etaria + Ano, data = dados_reg_al, family = binomial())
modelo_ne <- glm(Falha ~ Faixa_Etaria + Ano, data = dados_reg_ne, family = binomial())

# ==============================================================================
# 6. MODELO POOLED — teste formal de interação Grupo × Ano (LRT)
# Testa se a trajetória temporal de AL difere significativamente de NE
# ==============================================================================

dados_reg_pooled <- bind_rows(dados_reg_al, dados_reg_ne) %>%
  mutate(Grupo = relevel(as.factor(Grupo), ref = "NE (excl. AL)"))
rm(dados_reg_al, dados_reg_ne); gc()

modelo_sem_inter <- glm(Falha ~ Grupo + Faixa_Etaria + Ano,
                        data = dados_reg_pooled, family = binomial())
modelo_com_inter <- glm(Falha ~ Grupo * Ano + Faixa_Etaria,
                        data = dados_reg_pooled, family = binomial())
rm(dados_reg_pooled); gc()

lrt_interacao <- anova(modelo_sem_inter, modelo_com_inter, test = "LRT")
tabela_lrt <- tibble(
  Modelo        = c("Sem interação (Grupo + Faixa + Ano)",
                    "Com interação (Grupo × Ano + Faixa)"),
  GL_Residual   = lrt_interacao$`Resid. Df`,
  Dev_Residual  = round(lrt_interacao$`Resid. Dev`, 2),
  GL_LRT        = c(NA, lrt_interacao$Df[2]),
  Deviance_LRT  = c(NA, round(lrt_interacao$Deviance[2], 2)),
  P_Valor_LRT   = c(NA, round(lrt_interacao$`Pr(>Chi)`[2], 4))
)

# ==============================================================================
# EXTRAÇÃO DE RESULTADOS
# ==============================================================================

format_or <- function(modelo) {
  tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
    select(Variavel = term, OR = estimate,
           IC_Inf = conf.low, IC_Sup = conf.high, P_Valor = p.value) %>%
    mutate(across(c(OR, IC_Inf, IC_Sup), ~round(., 3)),
           P_Valor = round(P_Valor, 4))
}

resultados_or_al <- format_or(modelo_al) %>% mutate(Grupo = "Alagoas")
resultados_or_ne <- format_or(modelo_ne) %>% mutate(Grupo = "NE (excl. AL)")
resultados_or    <- bind_rows(resultados_or_al, resultados_or_ne) %>%
  select(Grupo, everything())

metricas_modelos <- tibble(
  Grupo         = c("Alagoas", "NE (excl. AL)"),
  AIC           = round(c(AIC(modelo_al), AIC(modelo_ne)), 1),
  N_Observacoes = c(nobs(modelo_al), nobs(modelo_ne)),
  N_Falhas      = c(n_falhas_al, n_falhas_ne),
  Nota          = c("Amostra completa AL",
                    "Amostra aleatória 500K de 18,1M (set.seed=42)")
)

# ==============================================================================
# GRÁFICOS
# ==============================================================================

cores_grupo <- c("Alagoas" = "#D55E00", "NE (excl. AL)" = "#0072B2")

# --- 1. Adequação etária ---

p_adequacao_etaria <- ggplot(
  tabela_adequacao,
  aes(x = Faixa_Diretriz, y = Proporcao_Pct, fill = Grupo)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = paste0(Proporcao_Pct, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = cores_grupo) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Adequação Etária dos Exames Citopatológicos",
    subtitle = "Alagoas vs. Nordeste excl. AL, 2015–2025",
    x        = NULL, y = "Proporção (%)", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10))

# --- 2. Tendência faixa-alvo (25–64) ---

p_tendencia_alvo <- tendencia_idade %>%
  filter(Faixa_Etaria == "25–64 anos") %>%
  ggplot(aes(x = CO_ANO_LIBERACAO, y = Pct,
             color = Grupo, linetype = Grupo, group = Grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_grupo) +
  scale_linetype_manual(values = c("Alagoas" = "solid", "NE (excl. AL)" = "dashed")) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(
    title    = "Proporção de Exames na Faixa-alvo (25–64 anos)",
    subtitle = "Alagoas vs. Nordeste excl. AL, 2015–2025",
    x        = "Ano", y = "Proporção (%)", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- 3. Taxa de insatisfatoriedade por ano — zoom ao intervalo real ---

ylim_insatis <- max(taxa_por_ano$Taxa_Insatis_Pct, na.rm = TRUE) * 1.15

p_taxa_insatis_anual <- taxa_por_ano %>%
  ggplot(aes(x = Ano, y = Taxa_Insatis_Pct,
             color = Grupo, linetype = Grupo, group = Grupo)) +
  geom_hline(yintercept = 5, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 2015.2, y = 5 * (1 + 0.04 * (5 / ylim_insatis)),
           label = "Limite Qualicito (5%)", hjust = 0, size = 3.5, color = "gray40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_grupo) +
  scale_linetype_manual(values = c("Alagoas" = "solid", "NE (excl. AL)" = "dashed")) +
  scale_x_continuous(breaks = 2015:2025) +
  coord_cartesian(ylim = c(0, ylim_insatis)) +
  labs(
    title    = "Taxa de Amostras Insatisfatórias por Ano",
    subtitle = "Alagoas vs. Nordeste excl. AL | Referência Qualicito ≤ 5% (linha pontilhada)",
    x        = "Ano", y = "Taxa (%)", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- 4. Taxa de rejeição por ano ---

p_taxa_rejeit_anual <- taxa_por_ano %>%
  ggplot(aes(x = Ano, y = Taxa_Rejeit_Pct,
             color = Grupo, linetype = Grupo, group = Grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_grupo) +
  scale_linetype_manual(values = c("Alagoas" = "solid", "NE (excl. AL)" = "dashed")) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(
    title    = "Taxa de Amostras Rejeitadas por Ano",
    subtitle = "Alagoas vs. Nordeste excl. AL, 2015–2025",
    x        = "Ano", y = "Taxa (%)", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- 5. Causas de rejeição ---

p_causas_rejeicao <- ggplot(
  causas_rejeicao,
  aes(x = reorder(Causa, Pct), y = Pct, fill = Grupo)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.8, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = cores_grupo) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Causas-raiz das Amostras Rejeitadas",
    subtitle = "Alagoas vs. Nordeste excl. AL — causas não mutuamente exclusivas",
    x        = NULL, y = "% entre amostras rejeitadas", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- 6. Causas de insatisfatoriedade ---

p_causas_insatis <- ggplot(
  causas_insatis,
  aes(x = reorder(Causa, Pct), y = Pct, fill = Grupo)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.8, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = cores_grupo) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Causas-raiz das Amostras Insatisfatórias",
    subtitle = "Alagoas vs. Nordeste excl. AL — causas não mutuamente exclusivas",
    x        = NULL, y = "% entre amostras insatisfatórias", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- 7. Forest plot de Odds Ratio — escala log, labels limpos, bandas ordenadas ---
# Ordem: faixas etárias no topo, anos cronológicos abaixo (após coord_flip).
# Escala log: ORs são multiplicativos; log10 simetriza OR=0,5 e OR=2,0.

ordem_forest <- c(
  paste0("Ano: ", 2016:2025),
  "Faixa: > 64 anos",
  "Faixa: < 25 anos"
)

p_odds_ratio <- resultados_or %>%
  filter(Variavel != "(Intercept)") %>%
  mutate(
    Variavel_Label = case_when(
      Variavel == "Faixa_Etaria< 25" ~ "Faixa: < 25 anos",
      Variavel == "Faixa_Etaria> 64" ~ "Faixa: > 64 anos",
      grepl("^Ano[0-9]", Variavel)   ~ paste0("Ano: ", sub("^Ano", "", Variavel)),
      TRUE ~ Variavel
    ),
    Variavel_Label = factor(Variavel_Label, levels = ordem_forest)
  ) %>%
  ggplot(aes(x = Variavel_Label, y = OR, ymin = IC_Inf, ymax = IC_Sup, color = Grupo)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_pointrange(size = 0.5, position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_y_log10(
    breaks = c(0.25, 0.5, 1, 2, 4, 8),
    labels = c("0,25", "0,50", "1,00", "2,00", "4,00", "8,00")
  ) +
  scale_color_manual(values = cores_grupo) +
  labs(
    title    = "Odds Ratio — Risco de Amostra Insatisfatória",
    subtitle = "Ref.: faixa 25–64 anos / 2015 | Escala log | Somente rastreamento",
    x        = NULL, y = "OR (IC 95%) — escala logarítmica", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==============================================================================
# EXPORTAÇÃO
# ==============================================================================

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

salvar_grafico(p_adequacao_etaria,   "IC1_01_adequacao_etaria")
salvar_grafico(p_tendencia_alvo,     "IC1_02_tendencia_faixa_alvo")
salvar_grafico(p_taxa_insatis_anual, "IC1_03_taxa_insatisfatorias_anual")
salvar_grafico(p_taxa_rejeit_anual,  "IC1_04_taxa_rejeicao_anual")
salvar_grafico(p_causas_rejeicao,    "IC1_05_causas_rejeicao")
salvar_grafico(p_causas_insatis,     "IC1_06_causas_insatisfatorias")
salvar_grafico(p_odds_ratio,         "IC1_07_odds_ratio",         w = 11, h = 8)

doc <- read_docx() %>%
  
  body_add_par("IC 1 — Qualidade Pré-Analítica na Atenção Primária (Revisado)", style = "heading 1") %>%
  body_add_par("Alagoas vs. Nordeste (excl. AL), 2015–2025. Fonte: SISCAN/INCA.", style = "Normal") %>%
  body_add_par(paste0(
    "LIMITAÇÕES METODOLÓGICAS: (1) Classificação geográfica por CO_UF_RESIDENCIA ",
    "(CO_UF_PREST_SERVICO indisponível na base exportada) — pacientes alagoanas ",
    "atendidas em PE/SE são classificadas como NE, e vice-versa; o efeito de ",
    "exposição ao sistema APS de Alagoas pode estar atenuado por essa misclassificação. ",
    "(2) Subamostragem do NE para 500K observações na regressão (restrição computacional; ",
    "set.seed=42; ~2,8% da base NE de 18,1M). ",
    "(3) GLM assume independência entre observações; não-independência por cluster ",
    "(laboratório/município) não foi modelada — ICs do OR podem ser subestimados. ",
    "(4) Bug histórico: CO_IDADE_PACIENTE estava zero-padded em 2018 ('017', '030'); ",
    "corrigido via as.numeric() — 2018 mantido na análise após validação."
  ), style = "Normal") %>%
  body_add_par("") %>%
  
  body_add_par("1. Adequação Etária — Comparação AL vs. NE excl. AL", style = "heading 2") %>%
  body_add_flextable(ft(tabela_adequacao_wide)) %>%
  body_add_par("") %>%
  
  body_add_par("2. Tendência Temporal por Faixa Etária", style = "heading 2") %>%
  body_add_flextable(ft(tendencia_idade)) %>%
  body_add_par("") %>%
  
  body_add_par("3. Taxa Global de Falha Pré-Analítica", style = "heading 2") %>%
  body_add_par("Referência Qualicito/INCA: taxa de insatisfatoriedade ≤ 5%.", style = "Normal") %>%
  body_add_flextable(ft(taxa_global)) %>%
  body_add_par("") %>%
  
  body_add_par("4. Taxa de Falha por Ano", style = "heading 2") %>%
  body_add_flextable(ft(taxa_por_ano)) %>%
  body_add_par("") %>%
  
  body_add_par("5. Causa-Raiz — Amostras Rejeitadas", style = "heading 2") %>%
  body_add_par("Causas não são mutuamente exclusivas; soma pode exceder 100%.", style = "Normal") %>%
  body_add_flextable(ft(causas_rejeicao_wide)) %>%
  body_add_par("") %>%
  
  body_add_par("6. Causa-Raiz — Amostras Insatisfatórias", style = "heading 2") %>%
  body_add_par("Causas não são mutuamente exclusivas; soma pode exceder 100%.", style = "Normal") %>%
  body_add_flextable(ft(causas_insatis_wide)) %>%
  body_add_par("") %>%
  
  body_add_par("7. Regressão Logística — Modelos Separados AL e NE", style = "heading 2") %>%
  body_add_par(paste0(
    "Desfecho: amostra insatisfatória. Somente rastreamento (TP_MOTIVO_EXAME=1). ",
    "Referência: faixa 25–64 anos e ano 2015. OR na escala natural (não log)."
  ), style = "Normal") %>%
  body_add_flextable(ft(metricas_modelos)) %>%
  body_add_par("") %>%
  body_add_flextable(ft(resultados_or)) %>%
  body_add_par("") %>%
  
  body_add_par("8. Teste de Interação Grupo × Ano (Modelo Pooled — LRT)", style = "heading 2") %>%
  body_add_par(paste0(
    "Testa se a trajetória temporal de AL difere significativamente de NE. ",
    "Hipótese nula: os coeficientes Ano_t são iguais nos dois grupos. ",
    "Rejeição (p < 0,05) indica que AL e NE têm tendências temporais distintas."
  ), style = "Normal") %>%
  body_add_flextable(ft(tabela_lrt))

print(doc, target = file.path(output_dir, "IC1_Resultados.docx"))