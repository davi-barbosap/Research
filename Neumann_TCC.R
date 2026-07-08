library(geobr)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ragg)
library(RColorBrewer)
library(ggspatial)
library(ggrepel)

caminho_salvar <- "/Users/davibarbosa/Desktop/jr/"
dir.create(caminho_salvar, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. FUNÇÕES AUXILIARES
# ==============================================================================

# Calcula o coeficiente de Gini a partir de vetores de população e médicos
calcula_gini <- function(pop, med) {
  ord     <- order(med / pop)
  pop     <- pop[ord]
  med     <- med[ord]
  x       <- c(0, cumsum(pop) / sum(pop))
  y       <- c(0, cumsum(med) / sum(med))
  n       <- length(x)
  area    <- sum((x[-1] - x[-n]) * (y[-1] + y[-n]) / 2)
  round(1 - 2 * area, 2)
}

formata_decimal <- function(x) gsub("\\.", ",", sprintf("%.2f", x))

# ==============================================================================
# 2. DADOS
# ==============================================================================
dados_alagoas <- data.frame(
  regiao = 1:10,
  porte = factor(
    c("Muito Grande", "Médio", "Médio", "Médio", "Médio",
      "Médio", "Grande", "Médio", "Médio", "Médio"),
    levels = c("Médio", "Grande", "Muito Grande")
  ),
  pop_2015     = c(1221428, 166666, 200000, 133333, 217647, 194871, 502702, 157142, 236363, 140000),
  pop_2025     = c(1401715, 207407, 203225, 134782, 227906, 208196, 525385, 183333, 246153, 145631),
  medicos_2015 = c(1197, 5, 24, 24, 37, 76, 186, 22, 26, 7),
  medicos_2025 = c(2667, 112, 63, 31, 98, 127, 384, 33, 32, 150)
) %>%
  mutate(
    densidade_2015 = round((medicos_2015 / pop_2015) * 1000, 2),
    densidade_2025 = round((medicos_2025 / pop_2025) * 1000, 2),
    pct_2015       = round((medicos_2015 / sum(medicos_2015)) * 100, 1),
    pct_2025       = round((medicos_2025 / sum(medicos_2025)) * 100, 1),
    label_2015     = paste0(regiao, "ª Região\n", medicos_2015, " méd. (", pct_2015, "%)\nTaxa: ", densidade_2015),
    label_2025     = paste0(regiao, "ª Região\n", medicos_2025, " méd. (", pct_2025, "%)\nTaxa: ", densidade_2025),
    label_porte    = paste0(regiao, "ª Região\nPop: ", scales::number(pop_2025, big.mark = ".", decimal.mark = ","))
  )

# Gini calculado dinamicamente — não hardcoded
gini_2015   <- calcula_gini(dados_alagoas$pop_2015, dados_alagoas$medicos_2015)
gini_2025   <- calcula_gini(dados_alagoas$pop_2025, dados_alagoas$medicos_2025)
label_g2015 <- paste0("2015 (Gini = ", formata_decimal(gini_2015), ")")
label_g2025 <- paste0("2025 (Gini = ", formata_decimal(gini_2025), ")")

# ==============================================================================
# 3. MALHA GEOGRÁFICA
# ==============================================================================
malha_rs <- read_health_region(year = 2013, showProgress = FALSE) %>%
  filter(abbrev_state == "AL") %>%
  mutate(regiao = as.numeric(substr(code_health_region, 3, 5)))

mapa_dados <- malha_rs %>% left_join(dados_alagoas, by = "regiao")

# ==============================================================================
# 4. TEMAS EDITORIAIS
# ==============================================================================
tema_mapa_premium <- theme_void(base_family = "Times New Roman", base_size = 10) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 10, vjust = 0.5),
    legend.text        = element_text(size = 9),
    legend.key.width   = unit(2.0, "cm"),
    legend.key.height  = unit(0.3, "cm"),
    strip.text         = element_text(face = "bold", size = 12, hjust = 0),
    plot.margin        = margin(15, 15, 15, 15)
  )

tema_grafico_premium <- theme_classic(base_family = "Times New Roman", base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    legend.position    = c(0.25, 0.85),
    legend.title       = element_blank(),
    legend.background  = element_rect(fill = "white", color = NA),
    panel.grid.major   = element_line(color = "gray92", linewidth = 0.5),
    axis.text          = element_text(color = "black"),
    axis.title         = element_text(face = "bold"),
    plot.margin        = margin(15, 15, 15, 15)
  )

# ==============================================================================
# FIGURA 1: MAPA DE DENSIDADE
# ==============================================================================
mapa_long <- mapa_dados %>%
  select(regiao, geom, densidade_2015, densidade_2025, label_2015, label_2025) %>%
  pivot_longer(
    cols      = c(densidade_2015, densidade_2025),
    names_to  = "Ano",
    values_to = "Densidade"
  ) %>%
  mutate(
    Painel      = ifelse(Ano == "densidade_2015", "A. Cenário de 2015", "B. Cenário de 2025"),
    Label_Final = ifelse(Ano == "densidade_2015", label_2015, label_2025)
  )

figura_1 <- ggplot(mapa_long) +
  geom_sf(aes(fill = Densidade), color = "white", linewidth = 0.4) +
  geom_label_repel(
    aes(label = Label_Final, geometry = geom),
    stat               = "sf_coordinates",
    family             = "Times New Roman", size = 2.4, fontface = "bold",
    fill               = alpha("white", 0.9), label.size = 0.15,
    min.segment.length = 0, segment.color = "black", segment.size = 0.3,
    max.overlaps       = Inf
  ) +
  scale_fill_fermenter(
    palette   = "YlGnBu", direction = 1,
    name      = "Razão (Médicos/1.000 hab.)",
    breaks    = c(0.1, 0.3, 0.6, 1.0, 1.5)
  ) +
  facet_wrap(~ Painel, ncol = 1) +
  annotation_scale(location = "br", width_hint = 0.15,
                   text_family = "Times New Roman", text_cex = 0.7) +
  annotation_north_arrow(
    location    = "tl", which_north = "true",
    height      = unit(0.8, "cm"), width = unit(0.6, "cm"),
    style       = north_arrow_minimal(text_family = "Times New Roman")
  ) +
  tema_mapa_premium

ggsave(paste0(caminho_salvar, "Figura_1_Densidade_Premium.png"),
       plot = figura_1, device = agg_png,
       width = 15, height = 24, units = "cm", dpi = 300)
ggsave(paste0(caminho_salvar, "Figura_1_Densidade_Premium.tiff"),
       plot = figura_1, device = agg_tiff,
       width = 15, height = 24, units = "cm", dpi = 300,
       compression = "lzw")

# ==============================================================================
# FIGURA 2: PORTE POPULACIONAL
# ==============================================================================
figura_2 <- ggplot(mapa_dados) +
  geom_sf(aes(fill = porte), color = "white", linewidth = 0.5) +
  geom_label_repel(
    aes(label = label_porte, geometry = geom),
    stat               = "sf_coordinates",
    family             = "Times New Roman", size = 2.6, fontface = "bold",
    fill               = alpha("white", 0.9), label.size = 0.15,
    min.segment.length = 0, max.overlaps = Inf
  ) +
  scale_fill_brewer(palette = "Blues", name = "Porte Populacional") +
  annotation_scale(location = "br", width_hint = 0.15,
                   text_family = "Times New Roman", text_cex = 0.7) +
  annotation_north_arrow(
    location    = "tl", which_north = "true",
    height      = unit(0.8, "cm"), width = unit(0.6, "cm"),
    style       = north_arrow_minimal(text_family = "Times New Roman")
  ) +
  tema_mapa_premium

ggsave(paste0(caminho_salvar, "Figura_2_Porte_Premium.png"),
       plot = figura_2, device = agg_png,
       width = 15, height = 13, units = "cm", dpi = 300)
ggsave(paste0(caminho_salvar, "Figura_2_Porte_Premium.tiff"),
       plot = figura_2, device = agg_tiff,
       width = 15, height = 13, units = "cm", dpi = 300,
       compression = "lzw")

# ==============================================================================
# FIGURA 3: CURVA DE LORENZ
# ==============================================================================
calcula_lorenz <- function(df, pop_col, med_col, ano_label) {
  resultado <- df %>%
    mutate(densidade = !!sym(med_col) / !!sym(pop_col)) %>%
    arrange(densidade) %>%
    mutate(
      prop_pop = !!sym(pop_col) / sum(!!sym(pop_col)),
      prop_med = !!sym(med_col) / sum(!!sym(med_col)),
      cum_pop  = cumsum(prop_pop),
      cum_med  = cumsum(prop_med),
      Ano      = ano_label
    )
  
  # Origem inserida antes dos dados; arrange garante sequência correta
  bind_rows(
    data.frame(cum_pop = 0, cum_med = 0, Ano = ano_label),
    resultado
  ) %>%
    arrange(cum_pop)
}

dados_lorenz <- bind_rows(
  calcula_lorenz(dados_alagoas, "pop_2015", "medicos_2015", label_g2015),
  calcula_lorenz(dados_alagoas, "pop_2025", "medicos_2025", label_g2025)
) %>%
  mutate(
    label_y = ifelse(
      cum_pop > 0 & cum_pop < 1 & cum_med > 0 & cum_med < 1,
      scales::number(cum_med, accuracy = 0.0001, decimal.mark = ","),
      NA
    )
  )

# Separação por ano para nudge direcional independente
dados_2015 <- dados_lorenz %>% filter(Ano == label_g2015)
dados_2025 <- dados_lorenz %>% filter(Ano == label_g2025)

figura_3 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_line(
    data = dados_lorenz,
    aes(x = cum_pop, y = cum_med, group = Ano, color = Ano),
    linewidth = 0.8
  ) +
  geom_point(
    data = dados_lorenz,
    aes(x = cum_pop, y = cum_med, shape = Ano, color = Ano),
    size = 2.0, fill = "white", stroke = 1.2
  ) +
  # 2015: rótulos abaixo da curva; clip = "off" permite renderizar abaixo de y=0
  geom_text_repel(
    data    = dados_2015,
    aes(x = cum_pop, y = cum_med, label = label_y),
    color   = "#2171b5", family = "Times New Roman",
    size    = 2.6, fontface = "bold",
    nudge_y = -0.05, nudge_x = 0.02, direction = "y",
    segment.color = "gray65", segment.size = 0.3, segment.curvature = 0,
    min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE
  ) +
  # 2025: rótulos acima da curva
  geom_text_repel(
    data    = dados_2025,
    aes(x = cum_pop, y = cum_med, label = label_y),
    color   = "#cb181d", family = "Times New Roman",
    size    = 2.6, fontface = "bold",
    nudge_y = 0.05, nudge_x = -0.02, direction = "y",
    segment.color = "gray65", segment.size = 0.3, segment.curvature = 0,
    min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE
  ) +
  scale_color_manual(values = c("#2171b5", "#cb181d")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::number_format(decimal.mark = ","),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::number_format(decimal.mark = ","),
    expand = c(0.02, 0.02)
  ) +
  # clip = "off" permite que rótulos próximos a y=0 apareçam abaixo do eixo
  coord_cartesian(clip = "off") +
  labs(
    x = "Proporção acumulada da população",
    y = "Proporção acumulada de médicos"
  ) +
  tema_grafico_premium +
  theme(plot.margin = margin(15, 25, 35, 15))

ggsave(paste0(caminho_salvar, "Figura_3_Lorenz_Premium.png"),
       plot = figura_3, device = agg_png,
       width = 15, height = 12, units = "cm", dpi = 300)
ggsave(paste0(caminho_salvar, "Figura_3_Lorenz_Premium.tiff"),
       plot = figura_3, device = agg_tiff,
       width = 15, height = 12, units = "cm", dpi = 300,
       compression = "lzw")