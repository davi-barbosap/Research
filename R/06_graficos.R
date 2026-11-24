# ==============================================================================
# GRÁFICOS — padrão único (tema_asma em 00_config.R): título alinhado à figura,
# vírgula decimal, nomes em vez de códigos, n informado, fonte na legenda.
# ==============================================================================
periodo  <- sprintf("%d–%d", min(anos), max(anos))
fonte_bi <- "Fonte: SIASUS/DATASUS, BPA individualizado (BI). Crianças e adolescentes de 0–19 anos, CID-10 J45/J46."
fonte_am <- "Fonte: SIASUS/DATASUS, APAC de medicamentos (Componente Especializado — CEAF). 0–19 anos."
fonte_sih <- "Fonte: SIH/DATASUS (AIH reduzida). Internações com CID-10 principal J45/J46, 0–19 anos, residentes e internados em Alagoas."
n_br <- function(x) num_br(x, 0)

# 1. Indicadores dos atendimentos ------------------------------------------------
ordem_ind <- unname(rotulo_ind)
d_ind <- indicadores %>%
  filter(Estratificacao %in% c("Total", "Local")) %>%
  mutate(Grupo = factor(Grupo, levels = names(cores_local)),
         Indicador = factor(sub(" \\(%\\)$", "", Indicador), levels = sub(" \\(%\\)$", "", ordem_ind)))
n_total_atend <- nrow(atend); n_total_pac <- n_distinct(atend$CNS_PAC[atend$CNS_Valido])
p_indicadores <- ggplot(d_ind, aes(Grupo, Pct, fill = Grupo)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct), width = 0.18, color = "gray25") +
  geom_text(aes(y = IC_Sup_Pct, label = num_br(Pct, 1)), vjust = -0.5, size = 3.3) +
  facet_wrap(~Indicador, scales = "free_y", ncol = 3, labeller = label_wrap_gen(32)) +
  scale_fill_manual(values = cores_local, name = NULL) +
  scale_y_continuous(labels = eixo_br, expand = expansion(mult = c(0, 0.22))) +
  labs(title = "Perfil dos atendimentos ambulatoriais por asma — 0 a 19 anos, Alagoas",
       subtitle = sprintf("%s atendimentos de %s pacientes com CNS válido, %s | IC 95%% considera atendimentos repetidos do mesmo paciente",
                          n_br(n_total_atend), n_br(n_total_pac), periodo),
       caption = fonte_bi, x = NULL, y = "% dos atendimentos") +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

# 2. Pirâmide de pacientes -------------------------------------------------------
d_pir <- pacientes %>% filter(Sexo != "Ignorado", Faixa_Etaria %in% niveis_faixa) %>%
  count(Faixa_Etaria, Sexo) %>%
  mutate(Pct = n / sum(n) * 100, Pct_Plot = if_else(Sexo == "Masculino", -Pct, Pct))
lim_pir <- ceiling((max(d_pir$Pct) + 4) / 10) * 10
p_piramide <- ggplot(d_pir, aes(Faixa_Etaria, Pct_Plot, fill = Sexo)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(num_br(Pct, 1), "%\n(n = ", n_br(n), ")"),
                hjust = if_else(Sexo == "Masculino", 1.08, -0.08)), size = 3.2, lineheight = 0.9) +
  geom_hline(yintercept = 0, color = "gray30") +
  coord_flip() +
  scale_y_continuous(limits = c(-lim_pir, lim_pir), breaks = seq(-lim_pir, lim_pir, 10),
                     labels = function(x) paste0(num_br(abs(x), 0), "%")) +
  scale_fill_manual(values = c("Masculino" = pal$celeste, "Feminino" = pal$rosa), name = NULL) +
  labs(title = "Crianças e adolescentes atendidos por asma, segundo sexo e faixa etária",
       subtitle = sprintf("%s pacientes (CNS), idade no 1º atendimento | %s", n_br(sum(d_pir$n)), periodo),
       caption = fonte_bi, x = "Faixa etária (anos)", y = "% do total de pacientes")

# 3. Urgência por tipo de serviço --------------------------------------------------
d_serv <- urgencia_servico %>% filter(n >= 30) %>%
  mutate(Tipo_Servico = fct_reorder(Tipo_Servico, Pct_Urgencia, .fun = max),
         Local = factor(Local, levels = c("Maceió", "Interior")),
         Rotulo = paste0(num_br(Pct_Urgencia, 1), "% (n = ", n_br(n), ")"))
p_urg_servico <- ggplot(d_serv, aes(Tipo_Servico, Pct_Urgencia, fill = Local)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = IC_Inf_Pct, ymax = IC_Sup_Pct), position = position_dodge(0.8),
                width = 0.2, color = "gray25") +
  geom_text(aes(y = pmax(IC_Sup_Pct, Pct_Urgencia) + 2, label = Rotulo), position = position_dodge(0.8),
            hjust = 0, size = 3.1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 100, 25), labels = eixo_pct) +
  scale_fill_manual(values = cores_local[c("Maceió", "Interior")], name = "Residência") +
  labs(title = "Atendimentos registrados como urgência, segundo o tipo de serviço",
       subtitle = "O caráter de urgência acompanha o tipo de estabelecimento (CNES). Estratos com ≥ 30 atendimentos; IC 95% por paciente",
       caption = fonte_bi, x = NULL, y = "% dos atendimentos com caráter de urgência")

# 4. Tipo de atendimento por faixa -------------------------------------------------
n_faixa_atend <- atend %>% filter(Faixa_Etaria %in% niveis_faixa) %>% count(Faixa_Etaria)
d_tipo <- tipo_atend_faixa %>% filter(Faixa_Etaria %in% niveis_faixa) %>%
  left_join(n_faixa_atend %>% rename(N = n), by = "Faixa_Etaria") %>%
  mutate(Faixa = factor(paste0(Faixa_Etaria, "\n(n = ", n_br(N), ")"),
                        levels = paste0(n_faixa_atend$Faixa_Etaria, "\n(n = ", n_br(n_faixa_atend$n), ")")),
         Tipo_Atend = factor(Tipo_Atend, levels = c("Urgência/observação", "Consulta", "Fisioterapia",
                                                    "Diagnóstico", "Outros procedimentos")))
p_tipo_atend <- ggplot(d_tipo, aes(Faixa, Pct, fill = Tipo_Atend)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(Pct >= 5, paste0(num_br(Pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.2, color = "white") +
  scale_fill_manual(values = c(pal$laranja, pal$azul, pal$verde, pal$amarelo, pal$cinza), name = NULL) +
  scale_y_continuous(labels = eixo_pct) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Tipo de atendimento ambulatorial por faixa etária",
       subtitle = "Classificação pelo procedimento (SIGTAP) e pela ocupação do profissional (CBO)",
       caption = fonte_bi, x = "Faixa etária (anos)", y = "% dos atendimentos")

# 5. Taxas anuais ------------------------------------------------------------------
series_taxa <- c(Taxa_Pacientes_10mil = "Crianças atendidas no ambulatório (BI)",
                 Taxa_CEAF_10mil = "Crianças em tratamento no CEAF",
                 Taxa_Intern_10mil = "Internações por asma (SIH)")
d_tend <- tendencia %>% select(Ano, any_of(names(series_taxa))) %>%
  pivot_longer(-Ano, names_to = "Serie", values_to = "Taxa") %>%
  mutate(Serie = factor(series_taxa[Serie], levels = series_taxa))
p_tendencia <- ggplot(d_tend, aes(Ano, Taxa, color = Serie)) +
  annotate("rect", xmin = min(anos_covid) - 0.5, xmax = max(anos_covid) + 0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "gray50") +
  annotate("text", x = mean(anos_covid), y = Inf, label = "COVID-19", vjust = 1.6, size = 3.3, color = "gray30") +
  geom_vline(xintercept = min(anos_tend) - 0.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = min(anos_tend) - 0.45, y = Inf, label = "início da série\nambulatorial estável",
           hjust = 0, vjust = 1.3, size = 3, color = "gray30", lineheight = 0.9) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = anos) +
  scale_y_continuous(labels = eixo_br, limits = c(0, NA)) +
  scale_color_manual(values = setNames(c(pal$azul, pal$verde, pal$laranja), series_taxa), name = NULL) +
  labs(title = "Asma em crianças e adolescentes por 10 mil habitantes de 0–19 anos — Alagoas",
       subtitle = sprintf("Pacientes únicos por ano (ambulatório e CEAF) e internações | população DATASUS; %s repetida em 2025",
                          pop_ano_disponivel),
       caption = "Fonte: SIASUS e SIH/DATASUS; população: estimativas DATASUS (pacote brpop).",
       x = NULL, y = "Por 10 mil habitantes")

# 6. Urgência bruta × ajustada -------------------------------------------------------
d_urg_plot <- urgencia_ajustada %>%
  select(Ano, `Bruta (todos os estabelecimentos)` = Pct_Bruta,
         `Sem o maior estabelecimento do ano` = Pct_Urg_Sem_Maior_Estab,
         `Ajustada por estabelecimento` = Pct_Ajustada) %>%
  pivot_longer(-Ano, names_to = "Serie", values_to = "Pct") %>%
  mutate(Serie = factor(Serie, levels = c("Bruta (todos os estabelecimentos)",
                                          "Sem o maior estabelecimento do ano", "Ajustada por estabelecimento")))
p_urg_tend <- ggplot(d_urg_plot, aes(Ano, Pct, color = Serie)) +
  annotate("rect", xmin = min(anos) - 0.5, xmax = min(anos_tend) - 0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.10, fill = "gray50") +
  annotate("text", x = mean(c(min(anos), min(anos_tend) - 1)), y = Inf, vjust = 1.5, size = 3, color = "gray30",
           label = "poucos registros;\nfora do ajuste", lineheight = 0.9) +
  geom_ribbon(data = urgencia_ajustada %>% filter(!is.na(Ajustada_IC_Inf)),
              aes(x = Ano, ymin = Ajustada_IC_Inf, ymax = Ajustada_IC_Sup), inherit.aes = FALSE,
              fill = pal$verde, alpha = 0.18) +
  geom_line(linewidth = 1.1, na.rm = TRUE) + geom_point(size = 2.2, na.rm = TRUE) +
  scale_x_continuous(breaks = anos) +
  scale_y_continuous(limits = c(0, 100), labels = eixo_pct) +
  scale_color_manual(values = c(pal$cinza, pal$azul, pal$verde), name = NULL) +
  labs(title = "Atendimentos de urgência por asma (0–19 anos): bruta × ajustada por estabelecimento",
       subtitle = sprintf("Ajuste: padronização marginal com efeito fixo do estabelecimento, %d–%d; faixa = IC 95%% bootstrap por paciente (%d réplicas)",
                          min(anos_tend), max(anos_tend), n_bootstrap),
       caption = fonte_bi, x = NULL, y = "% dos atendimentos")

# 7. Volume mensal de APAC ------------------------------------------------------------
d_vol <- volume_mensal %>%
  mutate(Data = idx_data(Mes),
         Status = factor(case_when(Apos_Fim ~ "Após o fim dos dados completos",
                                   Suspeito ~ "Queda abrupta (não conta como interrupção)",
                                   TRUE ~ "Mês regular"),
                         levels = c("Mês regular", "Queda abrupta (não conta como interrupção)",
                                    "Após o fim dos dados completos")))
p_volume <- ggplot(d_vol, aes(Data, APAC)) +
  geom_line(color = "gray55") +
  geom_point(aes(color = Status), size = 1.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = eixo_br, limits = c(0, NA)) +
  scale_color_manual(values = c(pal$cinza, pal$laranja, pal$celeste), name = NULL) +
  labs(title = "APAC de medicamentos para asma por mês (todas as idades) — Alagoas",
       subtitle = sprintf("Fim dos dados completos: %s | meses com queda abrupta são tratados como não observados",
                          format(idx_data(mes_fim_valido), "%m/%Y")),
       caption = "Fonte: SIASUS/DATASUS, APAC de medicamentos (CEAF).", x = NULL, y = "APAC no mês")

# 8. Apresentação dispensada por faixa ------------------------------------------------
d_apr <- apresentacao_faixa %>% filter(Faixa_Etaria %in% niveis_faixa, N_Faixa >= 10) %>%
  mutate(Faixa = paste0(Faixa_Etaria, "\n(n = ", n_br(N_Faixa), ")"),
         Faixa = factor(Faixa, levels = unique(Faixa[order(match(Faixa_Etaria, niveis_faixa))])),
         Apresentacao = factor(Apresentacao, levels = c("Budesonida 200 + formoterol",
                                                        "Budesonida 400 + formoterol", "Outros")))
p_apresentacao <- ggplot(d_apr, aes(Faixa, Pct, fill = Apresentacao)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(Pct >= 5, paste0(num_br(Pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.3, color = "white") +
  scale_fill_manual(values = c(pal$celeste, pal$azul, pal$cinza_claro), name = NULL) +
  scale_y_continuous(labels = eixo_pct) +
  labs(title = "Medicamento dispensado no CEAF por faixa etária",
       subtitle = sprintf("%% dos pacientes que receberam cada apresentação (um paciente pode receber mais de uma); faixas com < 10 pacientes omitidas (0–4 anos: n = %d)",
                          as.integer(max(c(0, apresentacao_faixa$N_Faixa[apresentacao_faixa$Faixa_Etaria == "0–4"])))),
       caption = paste(fonte_am, "Cápsulas de formoterol + budesonida (µg por cápsula)."),
       x = "Faixa etária (anos)", y = "% dos pacientes")

# 9. Estado nutricional ---------------------------------------------------------------
d_nut <- pacientes_am %>%
  filter(Estado_Nutricional != "Sem dado válido", Sexo != "Ignorado", Faixa_Etaria %in% niveis_faixa) %>%
  count(Sexo, Faixa_Etaria, Estado_Nutricional) %>%
  group_by(Sexo, Faixa_Etaria) %>% mutate(N = sum(n), Pct = n / N * 100) %>% ungroup() %>%
  filter(N >= 10) %>%
  arrange(Sexo, match(Faixa_Etaria, niveis_faixa)) %>%
  mutate(Faixa = paste0(Faixa_Etaria, "\n(n = ", N, ")"),
         Faixa = factor(Faixa, levels = unique(Faixa[order(match(Faixa_Etaria, niveis_faixa))])),
         Estado_Nutricional = factor(Estado_Nutricional, c("Magreza", "Eutrofia", "Sobrepeso", "Obesidade")))
p_nutricional <- ggplot(d_nut, aes(Faixa, Pct, fill = Estado_Nutricional)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(Pct >= 6, paste0(num_br(Pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.1, color = "white") +
  facet_wrap(~Sexo, scales = "free_x") +
  scale_fill_manual(values = c("Magreza" = pal$celeste, "Eutrofia" = pal$verde,
                               "Sobrepeso" = pal$amarelo, "Obesidade" = pal$laranja), name = NULL) +
  scale_y_continuous(labels = eixo_pct) +
  labs(title = "Estado nutricional das crianças e adolescentes em tratamento no CEAF",
       subtitle = "Escore-z de IMC-para-idade (OMS), mediana do paciente; gestantes excluídas; estratos com < 10 pacientes omitidos",
       caption = fonte_am, x = "Faixa etária (anos)", y = "% dos pacientes")

# 10. Persistência (Kaplan-Meier) ----------------------------------------------------
estratos_km <- episodios %>% count(Apresentacao) %>% filter(n >= 20)
rotulo_km <- setNames(paste0(sub(" \\(cápsula\\)", "", estratos_km$Apresentacao), " (n = ", estratos_km$n, ")"),
                      estratos_km$Apresentacao)
d_km <- km_curvas %>%
  bind_rows(tibble(Apresentacao = unique(km_curvas$Apresentacao), Meses = 0, Persistentes = 100)) %>%
  filter(Apresentacao %in% estratos_km$Apresentacao) %>%
  mutate(Apresentacao = rotulo_km[Apresentacao])
p_km <- ggplot(d_km, aes(Meses, Persistentes, color = Apresentacao)) +
  geom_step(linewidth = 1.1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray50") +
  scale_x_continuous(breaks = seq(0, 36, 6), limits = c(0, 36)) +
  scale_y_continuous(limits = c(0, 100), labels = eixo_pct) +
  scale_color_manual(values = c(pal$azul, pal$laranja), name = NULL) +
  labs(title = "Permanência no tratamento do CEAF (Kaplan-Meier) — 0 a 19 anos",
       subtitle = "Evento: ≥ 3 meses sem dispensação; censura: em uso no fim, troca de medicamento, limite de idade ou óbito",
       caption = fonte_am, x = "Meses desde o início do tratamento", y = "% ainda em tratamento")

# 11. Desfecho por medicamento --------------------------------------------------------
cores_desfecho <- setNames(c(pal$verde, pal$amarelo, pal$laranja, pal$celeste, "#F0E442", pal$rosa,
                             "gray55", "gray80", "black"), niveis_desfecho)
d_desf <- desfecho_medicamento %>% filter(Episodios >= 20, n > 0) %>%
  mutate(Apresentacao = paste0(sub(" \\(cápsula\\)", "", Apresentacao), "\n(n = ", Episodios, " episódios)"),
         Desfecho = droplevels(Desfecho))
p_desfecho <- ggplot(d_desf, aes(Apresentacao, Pct, fill = Desfecho)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = if_else(Pct >= 4, paste0(num_br(Pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = cores_desfecho, name = NULL) +
  scale_y_continuous(labels = eixo_pct) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Desfecho de cada tratamento no CEAF, por medicamento — 0 a 19 anos",
       subtitle = sprintf("Episódio = paciente × medicamento; dados completos até %s; medicamentos com ≥ 20 episódios",
                          format(idx_data(mes_fim_valido), "%m/%Y")),
       caption = fonte_am, x = NULL, y = "% dos episódios")

# 12. Razão de chances (vínculo BI × CEAF) ------------------------------------------
d_or <- vinculo_modelo %>%
  mutate(Modelo = factor(Modelo, levels = rev(Modelo)),
         Rotulo = sprintf("OR %s (%s–%s)", num_br(OR, 2), num_br(IC_Inf, 2), num_br(IC_Sup, 2)))
p_vinculo <- ggplot(d_or, aes(OR, Modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_errorbar(aes(xmin = IC_Inf, xmax = IC_Sup), width = 0.15, orientation = "y") +
  geom_point(size = 3.2, color = pal$azul) +
  geom_text(aes(label = Rotulo), vjust = -1.1, size = 3.3) +
  scale_x_log10(labels = eixo_br, breaks = c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2)) +
  scale_y_discrete(labels = label_wrap(28)) +
  labs(title = "Tratamento no CEAF e atendimento de urgência por asma (0–19 anos)",
       subtitle = sprintf("Chance de ≥ 1 urgência entre os %s pacientes do ambulatório: recebe × não recebe medicamento do CEAF",
                          n_br(nrow(pacientes))),
       caption = "Fonte: SIASUS/DATASUS (BI ligado à APAC de medicamentos pelo CNS). Regressão logística, erro-padrão robusto.",
       x = "Razão de chances (escala logarítmica)", y = NULL)

# 13. Antes × depois ---------------------------------------------------------------------
d_ad <- bind_rows(resumo_janelas %>% mutate(Grupo = sprintf("Casos: todos (n = %d)", max(Pacientes))),
                  resumo_did %>% mutate(Grupo = if_else(grepl("Caso", Grupo),
                                                        sprintf("Casos pareados (n = %d)", max(Pacientes[grepl("Caso", Grupo)])),
                                                        sprintf("Controles pareados (n = %d)", max(Pacientes[!grepl("Caso", Grupo)])))))
niveis_janela <- c("−24 a −13", "−12 a −1", "+1 a +12", "+13 a +24")
d_ad <- d_ad %>% mutate(X = match(as.character(Janela), niveis_janela))
p_antes_depois <- ggplot(d_ad, aes(X, Urg_por_Paciente, color = Grupo, group = Grupo)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2.45, y = Inf, label = "início do\ntratamento", hjust = 1, vjust = 1.3, size = 3,
           color = "gray30", lineheight = 0.9) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.6) +
  scale_x_continuous(breaks = 1:4, labels = niveis_janela, limits = c(0.8, 4.2)) +
  scale_y_continuous(labels = eixo_br, limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  scale_color_manual(values = c(pal$azul, pal$celeste, pal$cinza), name = NULL) +
  labs(title = "Atendimentos de urgência por asma antes e depois do início do tratamento no CEAF",
       subtitle = "Controles: crianças sem medicamento do CEAF pareadas por sexo, faixa etária, mês e nº de urgências no ano anterior",
       caption = "Fonte: SIASUS/DATASUS (BI ligado à APAC de medicamentos pelo CNS). Análise exploratória; não indica efeito causal.",
       x = "Janela (meses em relação ao início do tratamento)", y = "Urgências por paciente na janela")

# 14. Internações: série e rede hospitalar ------------------------------------------------
d_rede <- intern_rede %>%
  select(Ano, any_of(c(`Todos os hospitais` = "Taxa_10mil_Todas",
                       `Sem o hospital dominante de 2015–2017` = "Taxa_10mil_Sem_Dominante",
                       `Painel de hospitais estáveis` = "Taxa_10mil_Painel"))) %>%
  pivot_longer(-Ano, names_to = "Serie", values_to = "Taxa") %>%
  mutate(Serie = factor(Serie, levels = c("Todos os hospitais", "Sem o hospital dominante de 2015–2017",
                                          "Painel de hospitais estáveis")))
p_intern_rede <- ggplot(d_rede, aes(Ano, Taxa, color = Serie)) +
  annotate("rect", xmin = min(anos_covid) - 0.5, xmax = max(anos_covid) + 0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "gray50") +
  annotate("text", x = mean(anos_covid), y = Inf, label = "COVID-19", vjust = 1.6, size = 3.3, color = "gray30") +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = anos) +
  scale_y_continuous(labels = eixo_br, limits = c(0, NA)) +
  scale_color_manual(values = c(pal$laranja, pal$azul, pal$verde), name = NULL) +
  labs(title = "Internações por asma (0–19 anos) por 10 mil habitantes, segundo a rede hospitalar",
       subtitle = paste0(criterio_painel_hosp, "."),
       caption = fonte_sih, x = NULL, y = "Internações por 10 mil habitantes")

# 15. Sazonalidade -----------------------------------------------------------------------
p_sazonal <- ggplot(sazonalidade, aes(Mes_Cal, Media)) +
  geom_col(fill = pal$laranja, width = 0.7) +
  geom_errorbar(aes(ymin = pmax(Media - DP, 0), ymax = Media + DP), width = 0.2, color = "gray30") +
  geom_hline(yintercept = mean(sazonalidade$Media), linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:12, labels = c("jan", "fev", "mar", "abr", "mai", "jun",
                                               "jul", "ago", "set", "out", "nov", "dez")) +
  scale_y_continuous(labels = eixo_br, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Sazonalidade das internações por asma (0–19 anos) — Alagoas",
       subtitle = "Média de internações em cada mês do ano (barras: ± 1 desvio-padrão; tracejado: média geral); exclui 2020–2021",
       caption = fonte_sih, x = NULL, y = "Internações por mês (média)")

# 16. Regiões de saúde -------------------------------------------------------------------
p_regiao <- if (exists("taxas_regiao")) {
  d_reg <- taxas_regiao %>%
    select(Regiao_Nome, `Crianças atendidas no ambulatório` = Taxa_Pac_BI_10mil_ano,
           `Crianças em tratamento no CEAF` = Taxa_Pac_AM_10mil_ano,
           `Internações por asma` = Taxa_Intern_10mil_ano) %>%
    pivot_longer(-Regiao_Nome, names_to = "Indicador", values_to = "Taxa") %>%
    mutate(Indicador = factor(Indicador, levels = c("Crianças atendidas no ambulatório",
                                                    "Crianças em tratamento no CEAF", "Internações por asma")),
           Regiao_Nome = reorder(Regiao_Nome, -as.integer(sub("ª.*", "", Regiao_Nome))))
  ggplot(d_reg, aes(Taxa, Regiao_Nome, fill = Indicador)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = num_br(Taxa, 1)), hjust = -0.15, size = 3) +
    facet_wrap(~Indicador, scales = "free_x") +
    scale_x_continuous(labels = eixo_br, n.breaks = 4, expand = expansion(mult = c(0, 0.25))) +
    theme(panel.spacing = unit(2, "lines")) +
    scale_fill_manual(values = c(pal$azul, pal$verde, pal$laranja)) +
    labs(title = "Asma em crianças e adolescentes por região de saúde de residência — Alagoas",
         subtitle = sprintf("Por 10 mil habitantes de 0–19 anos por ano, %s | entre parênteses: município mais populoso da região", periodo),
         caption = "Fonte: SIASUS e SIH/DATASUS; regiões de saúde e população: pacote brpop (DATASUS).",
         x = "Por 10 mil habitantes-ano", y = NULL)
} else NULL

# 17. CEAF × internação por município ----------------------------------------------------------
p_eco_int <- if (exists("d_eco_int")) {
  d_e <- d_eco_int %>% mutate(Local = if_else(Municipio == cod_maceio, "Maceió", "Interior"))
  destaques <- d_e %>% filter(Municipio == cod_maceio | rank(-Taxa_Intern_10mil_ano) <= 6 |
                                rank(-Taxa_Pac_AM_10mil_ano) <= 4 | rank(-Pop_Media) <= 5)
  ggplot(d_e, aes(Taxa_Pac_AM_10mil_ano, Taxa_Intern_10mil_ano)) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "gray40", fill = "gray85", linewidth = 0.8) +
    geom_point(aes(size = Pop_Media, color = Local), alpha = 0.75) +
    geom_text_repel(data = destaques, aes(label = Municipio_Nome), size = 3, seed = 1,
                    min.segment.length = 0, box.padding = 0.4, max.overlaps = 30) +
    scale_color_manual(values = c("Maceió" = pal$laranja, "Interior" = pal$azul), name = NULL) +
    scale_size_continuous(range = c(1.5, 10), labels = eixo_br, name = "População 0–19 anos") +
    scale_x_continuous(labels = eixo_br) + scale_y_continuous(labels = eixo_br) +
    labs(title = "Tratamento no CEAF × internação por asma, por município de residência (0–19 anos)",
         subtitle = sprintf("%d municípios; curva LOESS com IC 95%% | taxas por 10 mil habitantes de 0–19 anos por ano", nrow(d_e)),
         caption = "Fonte: SIASUS (APAC de medicamentos) e SIH/DATASUS. Associação ecológica: não se aplica a indivíduos.",
         x = "Crianças em tratamento no CEAF por 10 mil", y = "Internações por 10 mil")
} else NULL
