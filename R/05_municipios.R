# ==============================================================================
# 10. NÍVEL MUNICIPAL, REGIÕES DE SAÚDE E ANÁLISE ECOLÓGICA [8][16]
# ==============================================================================
pop_mun <- if (!is.null(pop_ped)) pop_ped %>% group_by(Municipio = CO_MUN) %>%
  summarise(Pop_Anos = sum(Pop), Pop_Media = sum(Pop) / length(anos)) else NULL

asma_mun <- atend %>%
  group_by(Municipio = Mun_Res) %>%
  summarise(N_Atendimentos = n(),
            Pct_Urgencia = round(mean(Urgencia) * 100, 2),
            Urg = sum(Urgencia),
            Pct_Fora_Municipio = round(mean(Fora_Municipio) * 100, 2),
            Pct_Serv_Urg = round(mean(Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2),
            .groups = "drop") %>%
  full_join(pacientes %>% count(Municipio = Mun_Res, name = "Pacientes_BI"), by = "Municipio") %>%
  full_join(pacientes_am %>% count(Municipio = Mun_Res, name = "Pacientes_AM"), by = "Municipio") %>%
  full_join(intern %>% count(Municipio = Mun_Res, name = "Internacoes"), by = "Municipio") %>%
  mutate(across(c(N_Atendimentos, Pacientes_BI, Pacientes_AM, Internacoes), ~ coalesce(.x, 0L))) %>%
  left_join(nomes_mun, by = "Municipio") %>%
  mutate(Municipio_Nome = coalesce(Municipio_Nome, Municipio)) %>%
  relocate(Municipio_Nome, .after = Municipio)

regioes <- tryCatch(brpop::mun_reg_saude_449 %>%
                      transmute(Municipio = substr(as.character(code_muni), 1, 6),
                                Regiao_Saude = as.character(codi_reg_saude)) %>%
                      filter(substr(Municipio, 1, 2) == cod_uf_al),
                    error = function(e) NULL)
if (!is.null(pop_mun)) {
  asma_mun <- asma_mun %>% left_join(pop_mun, by = "Municipio") %>%
    mutate(Taxa_Pac_BI_10mil_ano = round(Pacientes_BI / Pop_Anos * 1e4, 2),
           Taxa_Pac_AM_10mil_ano = round(Pacientes_AM / Pop_Anos * 1e4, 2),
           Taxa_Intern_10mil_ano = round(Internacoes / Pop_Anos * 1e4, 2))
  # Maceió × interior: razão de taxas (quase-Poisson com offset) [8]
  rr_local <- function(var) {
    d <- asma_mun %>% filter(!is.na(Pop_Anos)) %>%
      mutate(Local = factor(if_else(Municipio == cod_maceio, "Maceió", "Interior"),
                            levels = c("Interior", "Maceió")))
    m <- glm(d[[var]] ~ Local + offset(log(Pop_Anos)), family = quasipoisson, data = d)
    cf <- coef(summary(m))["LocalMaceió", ]
    tibble(Desfecho = var, RR_Maceio_vs_Interior = round(exp(cf[[1]]), 2),
           IC_Inf = round(exp(cf[[1]] - 1.96 * cf[[2]]), 2),
           IC_Sup = round(exp(cf[[1]] + 1.96 * cf[[2]]), 2), P_Valor = signif(cf[[4]], 3))
  }
  razao_taxas_local <- bind_rows(rr_local("Pacientes_BI"), rr_local("Pacientes_AM"),
                                 rr_local("Internacoes")) %>%
    mutate(Desfecho = recode(Desfecho, Pacientes_BI = "Pacientes atendidos (BI) por habitante 0–19",
                             Pacientes_AM = "Pacientes no CEAF por habitante 0–19",
                             Internacoes = "Internações por asma por habitante 0–19"))
  # Ecológico: municípios com mais crianças no CEAF / atendidas internam menos?
  # Quase-Poisson com offset da população; RR por +1 paciente/10 mil hab.-ano.
  d_eco_int <- asma_mun %>% filter(!is.na(Pop_Anos), Pop_Anos > 0)
  m_int <- glm(Internacoes ~ Taxa_Pac_AM_10mil_ano + Taxa_Pac_BI_10mil_ano + offset(log(Pop_Anos)),
               family = quasipoisson, data = d_eco_int)
  cf_int <- coef(summary(m_int))[-1, , drop = FALSE]
  sp_int <- cor.test(d_eco_int$Taxa_Pac_AM_10mil_ano, d_eco_int$Taxa_Intern_10mil_ano,
                     method = "spearman", exact = FALSE)
  eco_internacao <- bind_rows(
    tibble(Analise = c("RR de internação por +1 paciente/10 mil no CEAF (ajustado por BI)",
                       "RR de internação por +1 paciente/10 mil atendido no BI (ajustado por CEAF)"),
           Estimativa = round(exp(cf_int[, 1]), 3),
           IC_Inf = round(exp(cf_int[, 1] - 1.96 * cf_int[, 2]), 3),
           IC_Sup = round(exp(cf_int[, 1] + 1.96 * cf_int[, 2]), 3),
           P_Valor = signif(cf_int[, 4], 3)),
    tibble(Analise = "Spearman: taxa CEAF × taxa de internação", Estimativa = round(sp_int$estimate, 3),
           P_Valor = signif(sp_int$p.value, 3))) %>%
    mutate(N_Municipios = nrow(d_eco_int))
  if (!is.null(regioes)) {
    taxas_regiao <- asma_mun %>% left_join(regioes, by = "Municipio") %>%
      filter(!is.na(Regiao_Saude)) %>%
      group_by(Regiao_Saude) %>%
      summarise(Sede = Municipio_Nome[which.max(Pop_Media)],
                Municipios = n(), Pop_Media = round(sum(Pop_Media, na.rm = TRUE)),
                Pacientes_BI = sum(Pacientes_BI), Pacientes_AM = sum(Pacientes_AM),
                Taxa_Pac_BI_10mil_ano = round(sum(Pacientes_BI) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                Taxa_Pac_AM_10mil_ano = round(sum(Pacientes_AM) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                Internacoes = sum(Internacoes),
                Taxa_Intern_10mil_ano = round(sum(Internacoes) / sum(Pop_Anos, na.rm = TRUE) * 1e4, 2),
                .groups = "drop") %>%
      # Nome: "1ª Região (sede: maior município)"; código 27001 → 1ª
      mutate(Regiao_Nome = sprintf("%dª Região (%s)", as.integer(substr(Regiao_Saude, 3, 5)), Sede),
             .after = Regiao_Saude) %>% select(-Sede)
  }
}

mun_eco <- asma_mun %>% filter(N_Atendimentos >= min_registros_mun)
eco <- if (nrow(mun_eco) >= 5) {
  sp <- cor.test(mun_eco$Pct_Fora_Municipio, mun_eco$Pct_Urgencia, method = "spearman", exact = FALSE)
  m  <- glm(cbind(Urg, N_Atendimentos - Urg) ~ I(Pct_Fora_Municipio / 10) + I(Pct_Serv_Urg / 10),
            family = quasibinomial, data = mun_eco)
  cf <- coef(summary(m))
  tibble(Analise = c("Spearman (não ponderado)",
                     "OR por +10 p.p. fora do município (quase-binomial ponderada)",
                     "OR por +10 p.p. em serviço de urgência (mesmo modelo)"),
         Estimativa = round(c(sp$estimate, exp(cf[2, 1]), exp(cf[3, 1])), 3),
         IC_Inf = c(NA, round(exp(cf[2, 1] - 1.96 * cf[2, 2]), 3), round(exp(cf[3, 1] - 1.96 * cf[3, 2]), 3)),
         IC_Sup = c(NA, round(exp(cf[2, 1] + 1.96 * cf[2, 2]), 3), round(exp(cf[3, 1] + 1.96 * cf[3, 2]), 3)),
         P_Valor = signif(c(sp$p.value, cf[2, 4], cf[3, 4]), 3),
         N_Municipios = nrow(mun_eco))
} else tibble(Analise = "Municípios insuficientes", N_Municipios = nrow(mun_eco))

# Pacientes no CEAF por ano na tabela de tendência (+ taxa) e APC
tendencia <- tendencia %>%
  left_join(pacientes_am_ano %>% select(Ano, Pacientes_CEAF = Pacientes_Todas_APAC), by = "Ano") %>%
  left_join(intern_ano %>% select(Ano, Internacoes, any_of(c("Taxa_Intern_10mil", "Taxa_Intern_Padronizada_10mil"))),
            by = "Ano")
if (!is.null(pop_ped)) tendencia <- tendencia %>%
  mutate(Taxa_CEAF_10mil = round(Pacientes_CEAF / Pop * 1e4, 2))
apc <- if (!is.null(pop_ped)) bind_rows(
  apc_prais(tendencia, "Taxa_Pacientes_10mil", "Pacientes atendidos (BI) / 10 mil"),
  apc_prais(tendencia, "Taxa_Pacientes_10mil", "Pacientes atendidos (BI) / 10 mil", TRUE),
  apc_prais(tendencia, "Taxa_Pac_Padronizada_10mil", "Pacientes (BI), padronizada por idade"),
  apc_prais(tendencia, "Taxa_CEAF_10mil", "Pacientes no CEAF / 10 mil"),
  apc_prais(tendencia, "Taxa_CEAF_10mil", "Pacientes no CEAF / 10 mil", TRUE)) else tibble()

# ==============================================================================
# CORREÇÃO DE HOLM GLOBAL [15] — todos os testes inferenciais do estudo
# ==============================================================================
coletar_p <- function(df, familia, rotulo) {
  if (is.null(df) || !nrow(df) || !"P_Valor" %in% names(df)) return(NULL)
  tibble(Familia = familia, Teste = rotulo(df), P_Valor = as.numeric(df$P_Valor))
}
testes_globais <- bind_rows(
  coletar_p(testes_local, "Maceió × interior (pacientes)", function(d) d$Variavel),
  coletar_p(testes_tratamento, "Apresentação × perfil (CEAF)", function(d) d$Grupo),
  coletar_p(raca_local_mi, "Raça/cor (imputação)", function(d) d$Analise),
  coletar_p(vinculo_modelo, "CEAF × urgência (logística)", function(d) d$Modelo),
  coletar_p(rr_janelas, "Antes × depois (janelas)", function(d) paste("Janela", d$Janela)),
  coletar_p(did_modelo, "Antes × depois (DiD)", function(d) d$Termo),
  coletar_p(apc, "Tendência ambulatorial/CEAF (APC)", function(d) paste(d$Serie, d$Periodo)),
  coletar_p(if (exists("apc_intern")) apc_intern else NULL, "Tendência de internação (APC)",
            function(d) paste(d$Serie, d$Periodo)),
  coletar_p(if (exists("razao_taxas_local")) razao_taxas_local else NULL, "Maceió × interior (taxas)",
            function(d) d$Desfecho),
  coletar_p(eco, "Ecológico: urgência", function(d) d$Analise),
  coletar_p(if (exists("eco_internacao")) eco_internacao else NULL, "Ecológico: internação",
            function(d) d$Analise)) %>%
  filter(!is.na(P_Valor)) %>%
  mutate(P_Holm_Global = p.adjust(P_Valor, "holm"),
         Significativo_Holm = if_else(P_Holm_Global < 0.05, "Sim", "Não"),
         P_Valor = signif(P_Valor, 3), P_Holm_Global = signif(P_Holm_Global, 3)) %>%
  arrange(P_Valor)

# Nomes legíveis das variáveis testadas
testes_globais <- testes_globais %>% mutate(Teste = coalesce(unname(rotulo_var[Teste]), Teste))
testes_local <- testes_local %>% mutate(Variavel = coalesce(unname(rotulo_var[Variavel]), Variavel))
testes_tratamento <- testes_tratamento %>% mutate(Grupo = coalesce(unname(rotulo_var[Grupo]), Grupo))

# Leva o p de Holm global para as tabelas de testes
testes_local <- testes_local %>% mutate(P_Valor = signif(P_Valor, 3)) %>%
  left_join(testes_globais %>% filter(Familia == "Maceió × interior (pacientes)") %>%
              select(Variavel = Teste, P_Holm_Global), by = "Variavel")
testes_tratamento <- testes_tratamento %>% mutate(P_Valor = signif(P_Valor, 3)) %>%
  left_join(testes_globais %>% filter(Familia == "Apresentação × perfil (CEAF)") %>%
              select(Grupo = Teste, P_Holm_Global), by = "Grupo")
