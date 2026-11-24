# ==============================================================================
# 9b. INTERNAÇÕES POR ASMA (SIH/RD) — 0–19 ANOS
#   Caso: AIH com CID PRINCIPAL J45/J46 (definição usual de internação por asma
#   e da lista de condições sensíveis à atenção primária, Portaria SAS 221/2008).
#   Ano = data de internação. AIH de continuação (IDENT = 5) e reapresentações
#   (mesmo N_AIH) excluídas. Só arquivos de AL: residentes internados em outros
#   estados não entram.
#   Reinternação: os arquivos públicos não têm CNS. Usa-se uma chave
#   determinística (data de nascimento + sexo + CEP + município de residência).
#   Duas AIH da mesma chave com nova internação até 1 dia após a alta anterior
#   são o mesmo episódio (transferência/continuidade). A versão sem CEP serve
#   de limite superior (mais falsos vínculos).
# ==============================================================================
decod_sexo_sih <- function(x) case_when(x == "1" ~ "Masculino", x %in% c("2", "3") ~ "Feminino",
                                        TRUE ~ "Ignorado")
desfecho_aih <- function(cob, morte) case_when(
  morte == "1" | substr(cob, 1, 1) == "4" ~ "Óbito",
  substr(cob, 1, 1) == "1"                ~ "Alta",
  substr(cob, 1, 1) == "3"                ~ "Transferência",
  substr(cob, 1, 1) == "2"                ~ "Permanência (AIH seguinte)",
  TRUE                                    ~ "Outro/encerramento")

sih0 <- sih_raw %>% as_tibble() %>%
  mutate(Data_Inter = as.Date(DT_INTER, "%Y%m%d"), Data_Saida = as.Date(DT_SAIDA, "%Y%m%d"),
         Data_Nasc = as.Date(NASC, "%Y%m%d"), Ano = as.integer(format(Data_Inter, "%Y")),
         Cmpt = paste0(ANO_CMPT, MES_CMPT))
sih1 <- sih0 %>% filter(Ano %in% anos)
sih2 <- sih1 %>% filter(substr(MUNIC_RES, 1, 2) == cod_uf_al)
sih3 <- sih2 %>% filter(IDENT != "5")
sih4 <- sih3 %>% arrange(N_AIH, desc(Cmpt)) %>% distinct(N_AIH, .keep_all = TRUE)
sih5 <- sih4 %>% filter(grepl(regex_asma, DIAG_PRINC))
sih5 <- sih5 %>%
  mutate(Idade = floor(as.numeric(Data_Inter - Data_Nasc) / 365.25),
         Idade_Campo = case_when(COD_IDADE == "4" ~ as.numeric(IDADE),
                                 COD_IDADE == "5" ~ 100 + as.numeric(IDADE),
                                 COD_IDADE %in% c("2", "3") ~ 0, TRUE ~ NA_real_),
         Idade = if_else(is.na(Idade) | Idade < 0 | Idade > idade_max, Idade_Campo, Idade))
sih6 <- sih5 %>% filter(!is.na(Idade), Idade <= idade_ped)

fluxo_sih <- tibble(
  Etapa = c("AIH com J45/J46 em qualquer diagnóstico", sprintf("Internação em %d–%d", min(anos), max(anos)),
            "Residente em AL", "Exclui AIH de continuação (IDENT = 5)",
            "Exclui reapresentação (N_AIH repetido)", "CID principal J45/J46",
            sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(sih0), nrow(sih1), nrow(sih2), nrow(sih3), nrow(sih4), nrow(sih5), nrow(sih6))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# Asma só como diagnóstico secundário (descritivo, não entra na definição)
sih_asma_secundaria <- sih4 %>%
  filter(!grepl(regex_asma, DIAG_PRINC)) %>%
  mutate(Idade = floor(as.numeric(Data_Inter - Data_Nasc) / 365.25)) %>%
  filter(!is.na(Idade), Idade <= idade_ped) %>%
  count(Capitulo_CID_Principal = substr(DIAG_PRINC, 1, 1), name = "AIH") %>%
  arrange(desc(AIH))

intern <- sih6 %>%
  mutate(Sexo = decod_sexo_sih(SEXO), Raca_Cor = decod_raca(RACA_COR),
         Faixa_Etaria = faixa_ped(Idade),
         Mun_Res = substr(MUNIC_RES, 1, 6),
         Local = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
         Fora_Municipio = substr(MUNIC_MOV, 1, 6) != Mun_Res,
         CID_Asma = cid_asma(DIAG_PRINC),
         Carater = decod_carater(CAR_INT),
         Dias = suppressWarnings(as.numeric(DIAS_PERM)),
         UTI = coalesce(suppressWarnings(as.numeric(UTI_MES_TO)), 0) > 0 |
               !MARCA_UTI %in% c("", "00", NA),
         Desfecho = desfecho_aih(COBRANCA, MORTE),
         Obito = Desfecho == "Óbito",
         Mes_Inter = mes_idx(format(Data_Inter, "%Y%m")),
         Covid = Ano %in% anos_covid,
         Chave     = paste(NASC, Sexo, CEP, Mun_Res),
         Chave_sem_CEP = paste(NASC, Sexo, Mun_Res))

resumo_intern <- intern %>%
  summarise(Internacoes = n(), Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Idade_Mediana = median(Idade), Pct_Menor_5 = round(mean(Idade < 5) * 100, 2),
            Pct_J46 = round(mean(CID_Asma == "J46 Estado de mal asmático") * 100, 2),
            Pct_Urgencia = round(mean(Carater == "Urgência") * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE),
            Permanencia_P75 = quantile(Dias, 0.75, na.rm = TRUE),
            Pct_UTI = round(mean(UTI) * 100, 2),
            Obitos = sum(Obito), Letalidade_Pct = round(mean(Obito) * 100, 3),
            Pct_Fora_Municipio = round(mean(Fora_Municipio) * 100, 2))

perfil_intern_faixa <- intern %>%
  group_by(Faixa_Etaria) %>%
  summarise(Internacoes = n(), Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE),
            k_UTI = sum(UTI), Obitos = sum(Obito), .groups = "drop") %>%
  mutate(Pct_UTI = round(k_UTI / Internacoes * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$k_UTI, .$Internacoes) %>% round(2) %>%
              rename(UTI_IC_Inf = IC_Inf_Pct, UTI_IC_Sup = IC_Sup_Pct)) %>%
  select(-k_UTI)

desfecho_hospitalar <- intern %>% count(Desfecho, name = "Internacoes") %>%
  mutate(Pct = round(Internacoes / sum(Internacoes) * 100, 2))

dist_intern <- bind_rows(lapply(c("Sexo", "Faixa_Etaria", "Raca_Cor", "CID_Asma", "Carater", "Local"),
  function(v) intern %>% count(Categoria = as.character(.data[[v]])) %>%
    mutate(Variavel = v, Pct = round(n / sum(n) * 100, 2), .before = 1)))

# Série anual e taxas por 10 mil hab. de 0–19 anos (bruta e padronizada)
intern_ano <- intern %>% group_by(Ano) %>%
  summarise(Internacoes = n(), Pct_UTI = round(mean(UTI) * 100, 2),
            Permanencia_Mediana = median(Dias, na.rm = TRUE), Obitos = sum(Obito), .groups = "drop")
if (!is.null(pop_ped)) {
  intern_ano <- intern_ano %>% left_join(pop_ano, by = "Ano") %>%
    mutate(Taxa_Intern_10mil = round(Internacoes / Pop * 1e4, 2)) %>%
    left_join(intern %>% filter(Faixa_Etaria %in% niveis_faixa) %>%
                count(Ano, Faixa = as.character(Faixa_Etaria), name = "n") %>%
                left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
                left_join(pop_padrao, by = "Faixa") %>%
                group_by(Ano) %>%
                summarise(Taxa_Intern_Padronizada_10mil = round(sum(n / Pop * w) * 1e4, 2), .groups = "drop"),
              by = "Ano")
  taxa_intern_faixa <- intern %>% count(Ano, Faixa = as.character(Faixa_Etaria), name = "Internacoes") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    mutate(Taxa_10mil = round(Internacoes / Pop * 1e4, 2))
  apc_intern <- bind_rows(
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", TRUE, periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_Padronizada_10mil", "Internações, padronizada por idade", periodo_anos = anos),
    apc_prais(intern_ano, "Taxa_Intern_10mil", "Internações / 10 mil", periodo_anos = anos_tend))
}

# Concentração por hospital: mudanças na rede explicam parte da série
completar_cnes(intern$CNES)
concentracao_hosp <- intern %>% count(Ano, CNES) %>%
  group_by(Ano) %>%
  summarise(Internacoes = sum(n), Hospitais = n(),
            Maior_Hospital = nome_estab(CNES[which.max(n)]), Pct_Maior_Hospital = round(max(n) / sum(n) * 100, 1),
            .groups = "drop") %>%
  left_join(intern %>% group_by(Ano) %>% summarise(Pct_UTI = round(mean(UTI) * 100, 2)), by = "Ano")

# [3] Tendência controlada pela rede hospitalar
#  (a) painel: hospitais com internação pediátrica por asma em ≥ 80% dos anos;
#  (b) sem o hospital dominante de 2015–2017 (maior participação no período);
#  (c) UTI ajustada por hospital (padronização marginal, só hospitais de ≥ 2 anos).
anos_por_hosp <- intern %>% distinct(CNES, Ano) %>% count(CNES)
painel_hosp <- anos_por_hosp %>% filter(n >= ceiling(0.8 * length(anos))) %>% pull(CNES)
hosp_dominante <- intern %>% filter(Ano <= 2017) %>% count(CNES) %>% slice_max(n, n = 1) %>% pull(CNES)
intern_rede <- intern %>%
  group_by(Ano) %>%
  summarise(Internacoes_Todas = n(),
            Internacoes_Painel = sum(CNES %in% painel_hosp),
            Internacoes_Sem_Dominante = sum(CNES != hosp_dominante), .groups = "drop")
if (!is.null(pop_ped)) intern_rede <- intern_rede %>% left_join(pop_ano, by = "Ano") %>%
  mutate(across(starts_with("Internacoes_"), ~ round(.x / Pop * 1e4, 2), .names = "Taxa_{.col}")) %>%
  rename_with(~ sub("Taxa_Internacoes_", "Taxa_10mil_", .x)) %>% select(-Pop)
hosp_2anos <- anos_por_hosp %>% filter(n >= 2) %>% pull(CNES)
d_uti <- intern %>% filter(CNES %in% hosp_2anos) %>% mutate(Ano_f = factor(Ano), Hosp = factor(CNES))
m_uti <- suppressWarnings(glm(UTI ~ Ano_f + Hosp, family = binomial, data = d_uti))
uti_ajustada <- tibble(Ano = as.integer(levels(d_uti$Ano_f)),
  Pct_UTI_Ajustada = sapply(levels(d_uti$Ano_f), function(a)
    round(mean(predict(m_uti, d_uti %>% mutate(Ano_f = factor(a, levels = levels(d_uti$Ano_f))),
                       type = "response")) * 100, 2))) %>%
  left_join(intern %>% group_by(Ano) %>% summarise(Pct_UTI_Bruta = round(mean(UTI) * 100, 2)), by = "Ano") %>%
  relocate(Ano, Pct_UTI_Bruta)
intern_rede <- intern_rede %>% left_join(uti_ajustada, by = "Ano")
criterio_painel_hosp <- sprintf("Painel: %d hospitais com internação em ≥ %d dos %d anos; hospital dominante 2015–2017: %s",
                                length(painel_hosp), ceiling(0.8 * length(anos)), length(anos), nome_estab(hosp_dominante))
if (!is.null(pop_ped)) apc_intern <- bind_rows(apc_intern,
  apc_prais(intern_rede, "Taxa_10mil_Painel", "Internações / 10 mil — painel de hospitais", periodo_anos = anos),
  apc_prais(intern_rede, "Taxa_10mil_Sem_Dominante", "Internações / 10 mil — sem hospital dominante", periodo_anos = anos))

# Sazonalidade: média de internações por mês do calendário (exclui anos COVID)
sazonalidade <- intern %>% filter(!Covid) %>%
  mutate(Mes_Cal = as.integer(format(Data_Inter, "%m"))) %>%
  count(Ano, Mes_Cal) %>%
  complete(Ano, Mes_Cal = 1:12, fill = list(n = 0)) %>%
  group_by(Mes_Cal) %>%
  summarise(Media = round(mean(n), 2), DP = round(sd(n), 2), .groups = "drop") %>%
  mutate(Indice_Sazonal = round(Media / mean(Media) * 100, 1))

# Reinternação pela chave determinística
reinternacao <- function(df, chave, rotulo) {
  ep <- df %>%
    transmute(Chave = .data[[chave]], Data_Inter, Data_Saida, Obito) %>%
    filter(!is.na(Data_Inter), !is.na(Data_Saida)) %>%
    arrange(Chave, Data_Inter) %>%
    group_by(Chave) %>%
    mutate(Saida_Max_Ant = lag(cummax(as.numeric(Data_Saida))),
           Novo_Ep = is.na(Saida_Max_Ant) | as.numeric(Data_Inter) > Saida_Max_Ant + 1,
           Ep = cumsum(Novo_Ep)) %>%
    group_by(Chave, Ep) %>%
    summarise(Ini = min(Data_Inter), Fim = max(Data_Saida), Obito = any(Obito), .groups = "drop") %>%
    arrange(Chave, Ini) %>%
    group_by(Chave) %>%
    mutate(Prox_Ini = lead(Ini), Dias_Ate_Prox = as.numeric(Prox_Ini - Fim)) %>%
    ungroup()
  fim_obs <- max(df$Data_Saida, na.rm = TRUE)
  idx <- ep %>% filter(!Obito, Fim <= fim_obs - 30)
  idx12 <- ep %>% filter(!Obito, Fim <= fim_obs - 365)
  pac <- ep %>% count(Chave, name = "Episodios")
  r30 <- sum(!is.na(idx$Dias_Ate_Prox) & idx$Dias_Ate_Prox <= 30)
  r365 <- sum(!is.na(idx12$Dias_Ate_Prox) & idx12$Dias_Ate_Prox <= 365)
  tibble(Chave = rotulo, AIH = nrow(df), Episodios = nrow(ep), Criancas = nrow(pac),
         Pct_Criancas_2mais_Episodios = round(mean(pac$Episodios >= 2) * 100, 2),
         Reintern_30d_Pct = round(r30 / nrow(idx) * 100, 2),
         Reintern_30d_IC_Inf = round(wilson_ci_pct(r30, nrow(idx))$IC_Inf_Pct, 2),
         Reintern_30d_IC_Sup = round(wilson_ci_pct(r30, nrow(idx))$IC_Sup_Pct, 2),
         Reintern_12m_Pct = round(r365 / nrow(idx12) * 100, 2),
         Reintern_12m_IC_Inf = round(wilson_ci_pct(r365, nrow(idx12))$IC_Inf_Pct, 2),
         Reintern_12m_IC_Sup = round(wilson_ci_pct(r365, nrow(idx12))$IC_Sup_Pct, 2))
}
reinternacoes <- bind_rows(
  reinternacao(intern, "Chave", "Nascimento + sexo + CEP + município (principal)"),
  reinternacao(intern, "Chave_sem_CEP", "Nascimento + sexo + município (limite superior)"))
reintern_faixa <- bind_rows(lapply(niveis_faixa, function(f)
  reinternacao(intern %>% filter(Faixa_Etaria == f), "Chave", f) %>% rename(Faixa_Etaria = Chave)))
