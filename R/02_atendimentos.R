# ==============================================================================
# 2. PA — FLUXOGRAMA, EXCLUSÃO DAS APAC DE MEDICAMENTO, MAPA DE ESTABELECIMENTOS
# ==============================================================================

pa0 <- pa_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(PA_CMP, 1, 4)), Mun_Res = substr(PA_MUNPCN, 1, 6),
         DOCORIG = toupper(PA_DOCORIG), Idade = suppressWarnings(as.integer(PA_IDADE)),
         Idade = if_else(coalesce(PA_FLIDADE %in% c("3", "4"), FALSE) |
                           is.na(Idade) | Idade < 0 | Idade > idade_max, NA_integer_, Idade))
pa1 <- pa0 %>% filter(Ano %in% anos)
pa2 <- pa1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
pa3 <- pa2 %>% filter(DOCORIG %in% c("I", "P", "S"))

# [2] APAC de medicamento identificadas por QUALQUER linha do grupo 06, antes
# do filtro de aprovação (antes: só linha P aprovada → 1.889 vazavam).
chaves_apac_med <- pa3 %>%
  filter(DOCORIG %in% c("P", "S"), substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP)

pa4 <- pa3 %>% filter(PA_INDICA %in% c("5", "6"))
pa5 <- pa4 %>% anti_join(chaves_apac_med, by = c("PA_AUTORIZ", "PA_CMP"))
pa6 <- pa5 %>% filter(!is.na(Idade))
pa7 <- pa6 %>% filter(Idade <= idade_ped)

fluxograma_pa <- tibble(
  Etapa = c("Linhas do PA com CID J45/J46 (principal ou secundário)",
            sprintf("Competência em %d–%d", min(anos), max(anos)),
            "Residente em Alagoas", "Instrumento BPA-I ou APAC (exclui BPA-C e RAAS)",
            "Produção aprovada (PA_INDICA 5/6)",
            "Sem APAC de medicamento (qualquer linha do grupo 06 → módulo AM)",
            "Idade válida", sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(pa0), nrow(pa1), nrow(pa2), nrow(pa3), nrow(pa4), nrow(pa5),
             nrow(pa6), nrow(pa7))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# O que sobra no PA pediátrico: BPA-I (analisado pelo BI) e APAC não medicamento
pa_ped_instrumento <- pa7 %>%
  mutate(Instrumento = if_else(DOCORIG == "I", "BPA-I", "APAC (não medicamento)")) %>%
  group_by(Instrumento) %>%
  summarise(Linhas = n(),
            Atendimentos = if (first(Instrumento) == "BPA-I") n()
                           else n_distinct(paste(PA_AUTORIZ, PA_CMP)),
            .groups = "drop")

# Tipo de estabelecimento (CNES) por código de unidade — usado no BI
mapa_tpups <- pa0 %>%
  filter(!is.na(PA_TPUPS), PA_TPUPS != "") %>%
  count(PA_CODUNI, PA_TPUPS) %>%
  group_by(PA_CODUNI) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(CODUNI = PA_CODUNI, TPUPS = PA_TPUPS)

# ==============================================================================
# 3. ATENDIMENTOS PEDIÁTRICOS — arquivo BI (= BPA-I do PA, com CNS) [4]
# ==============================================================================

bi <- bi_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(DT_ATEND, 1, 4)),
         Mes = mes_idx(substr(DT_ATEND, 1, 6)),
         Data_Atend = as.Date(paste0(substr(DT_ATEND, 1, 6), "15"), "%Y%m%d"),
         Data_Nasc  = as.Date(DTNASC, "%Y%m%d"),
         Idade      = floor(as.numeric(Data_Atend - Data_Nasc) / 365.25),
         Idade      = if_else(is.na(Idade) | Idade < 0 | Idade > idade_max, NA_real_, Idade))

bi1 <- bi  %>% filter(Ano %in% anos)
bi2 <- bi1 %>% filter(substr(MUNPAC, 1, 2) == cod_uf_al)
bi3 <- bi2 %>% filter(suppressWarnings(as.numeric(QT_APROV)) > 0)
bi4 <- bi3 %>% filter(!is.na(Idade))
bi5 <- bi4 %>% filter(Idade <= idade_ped)

fluxo_bi <- tibble(
  Etapa  = c("Linhas do BI com CIDPRI J45/J46", "Competência no período",
             "Residente em AL", "Quantidade aprovada > 0",
             "Idade válida (data de nascimento)", sprintf("Idade 0–%d anos", idade_ped)),
  Linhas = c(nrow(bi), nrow(bi1), nrow(bi2), nrow(bi3), nrow(bi4), nrow(bi5))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

# Consistência PA × BI (todas as idades): o BI deve reproduzir o BPA-I do PA
consistencia_pa_bi <- full_join(
  pa4 %>% filter(DOCORIG == "I") %>% count(Ano, name = "PA_BPA_I"),
  bi3 %>% count(Ano, name = "BI_aprovado"), by = "Ano") %>%
  mutate(Diferenca_Pct = round((BI_aprovado - PA_BPA_I) / PA_BPA_I * 100, 2)) %>%
  arrange(Ano)

atend <- bi5 %>%
  left_join(mapa_tpups, by = "CODUNI") %>%
  mutate(
    CNS_Valido   = cns_valido(CNS_PAC),
    Cluster      = if_else(CNS_Valido, CNS_PAC, paste0("linha_", row_number())),
    Faixa_Etaria = faixa_ped(Idade),
    Sexo         = decod_sexo(SEXOPAC),
    Raca_Cor     = decod_raca(RACACOR),
    Mun_Res      = substr(MUNPAC, 1, 6),
    Local        = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
    Fora_Municipio = coalesce(MNDIF == "1", FALSE),
    CID_Asma     = cid_asma(CIDPRI),
    Mal_Asmatico = substr(CIDPRI, 1, 3) == "J46",
    Carater      = decod_carater(CATEND),
    Urgencia     = Carater == "Urgência",
    Tipo_Servico = tipo_servico(TPUPS),
    Tipo_Atend   = tipo_atendimento(PROC_ID, CBOPROF),
    Covid        = Ano %in% anos_covid)

completude <- atend %>%
  summarise(N_Atendimentos           = n(),
            Pct_CNS_Invalido         = round(mean(!CNS_Valido) * 100, 2),
            Pct_Sexo_Ignorado        = round(mean(Sexo == "Ignorado") * 100, 2),
            Pct_Raca_Sem_Info        = round(mean(Raca_Cor == "Sem informação") * 100, 2),
            Pct_Carater_Ignorado     = round(mean(Carater == "Ignorado") * 100, 2),
            Pct_CID_Sem_Subcategoria = round(mean(CID_Asma == "J45 sem subcategoria válida") * 100, 2),
            Pct_Servico_Sem_Info     = round(mean(Tipo_Servico == "Sem informação") * 100, 2))

# ==============================================================================
# 4. PERFIL DOS ATENDIMENTOS — descritivo, IC robusto a cluster por paciente
# ==============================================================================

atend_ind <- atend %>%
  mutate(I_Feminino   = if_else(Sexo == "Ignorado", NA, Sexo == "Feminino"),
         I_Urgencia   = if_else(Carater == "Ignorado", NA, Urgencia),
         I_J46        = Mal_Asmatico,
         I_Fora_Mun   = Fora_Municipio,
         I_Serv_Urg   = Tipo_Servico == "Urgência/pronto-atendimento",
         I_Fisio      = Tipo_Atend == "Fisioterapia")

rotulo_ind <- c(I_Feminino = "Sexo feminino (%)",
                I_Urgencia = "Caráter de urgência (%)",
                I_Serv_Urg = "Em serviço de urgência/pronto-atendimento (%)",
                I_Fisio    = "Sessão de fisioterapia (%)",
                I_J46      = "Estado de mal asmático — J46 (%)",
                I_Fora_Mun = "Atendido fora do município de residência (%)")

indicadores_por <- function(df, grupo) {
  bind_rows(lapply(names(rotulo_ind), function(v) {
    df %>% group_by(Grupo = as.character(.data[[grupo]])) %>%
      group_modify(~ prop_cluster(.x[[v]], .x$Cluster)) %>%
      ungroup() %>%
      mutate(Indicador = rotulo_ind[[v]], Estratificacao = grupo, .before = 1)
  }))
}

indicadores <- bind_rows(
  indicadores_por(atend_ind %>% mutate(Total = "Alagoas (0–19 anos)"), "Total"),
  indicadores_por(atend_ind, "Local"),
  indicadores_por(atend_ind, "Faixa_Etaria")) %>%
  mutate(across(c(Pct, IC_Inf_Pct, IC_Sup_Pct), ~ round(.x, 2)))

# [3] Urgência depende do serviço: estratificação obrigatória
urgencia_servico <- atend_ind %>%
  filter(!is.na(I_Urgencia)) %>%
  group_by(Local, Tipo_Servico) %>%
  group_modify(~ prop_cluster(.x$I_Urgencia, .x$Cluster)) %>%
  ungroup() %>%
  group_by(Local) %>% mutate(Pct_dos_Atend_do_Local = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
  mutate(across(c(Pct, IC_Inf_Pct, IC_Sup_Pct), ~ round(.x, 2))) %>%
  rename(Pct_Urgencia = Pct)

# Concentração por estabelecimento
completar_cnes(atend$CODUNI)
concentracao_estab <- atend %>%
  count(Ano, CODUNI) %>%
  group_by(Ano) %>%
  summarise(Atendimentos = sum(n), Estabelecimentos = n(),
            Pct_Maior_Estab = round(max(n) / sum(n) * 100, 1),
            Pct_4_Maiores = round(sum(sort(n, decreasing = TRUE)[1:min(4, n())]) / sum(n) * 100, 1),
            Maior_Estabelecimento = CODUNI[which.max(n)],
            .groups = "drop") %>%
  mutate(Maior_Estabelecimento = nome_estab(Maior_Estabelecimento))

distribuicao <- function(df, var) {
  bind_rows(df %>% mutate(Local = "Alagoas (total)"), df) %>%
    count(Local, Categoria = as.character(.data[[var]])) %>%
    group_by(Local) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
    mutate(Variavel = var, .before = 1)
}
vars_dist <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "CID_Asma", "Carater",
               "Tipo_Servico", "Tipo_Atend")
distribuicoes <- bind_rows(lapply(vars_dist, function(v) distribuicao(atend, v)))

tipo_atend_faixa <- atend %>%
  count(Faixa_Etaria, Tipo_Atend) %>%
  group_by(Faixa_Etaria) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup()

# ==============================================================================
# 5. PACIENTES PEDIÁTRICOS (BI, CNS válido) — unidade de inferência [4]
# ==============================================================================

atend_cns <- atend %>% filter(CNS_Valido)
mes_ini_bi <- mes_idx(sprintf("%d01", min(anos)))

pacientes <- atend_cns %>%
  arrange(CNS_PAC, Mes) %>%
  group_by(CNS_PAC) %>%
  summarise(
    Mes_Primeiro     = first(Mes),
    Ano_Primeiro     = first(Ano),
    Idade_Primeiro   = first(Idade),
    Sexo             = first(Sexo),
    Raca_Cor         = first(Raca_Cor),
    Local            = first(Local),
    Mun_Res          = first(Mun_Res),
    Servico_1o       = first(Tipo_Servico),
    Tipo_Atend_1o    = first(Tipo_Atend),
    N_Atendimentos   = n(),
    N_Contatos       = n_distinct(paste(CODUNI, Mes)),   # paciente × estab. × mês [7]
    Teve_J46         = any(Mal_Asmatico),
    Teve_Urgencia    = any(Urgencia),
    Teve_Serv_Urg    = any(Tipo_Servico == "Urgência/pronto-atendimento"),
    Teve_Fisio       = any(Tipo_Atend == "Fisioterapia"),
    .groups = "drop") %>%
  mutate(Faixa_Etaria = faixa_ped(Idade_Primeiro),
         Novo = Mes_Primeiro >= mes_ini_bi + washout_meses,       # [12]
         Freq_Contatos = cut(N_Contatos, c(0, 1, 2, 5, Inf), labels = c("1", "2", "3–5", "6+")))

resumo_pacientes <- pacientes %>%
  summarise(N_Pacientes          = n(),
            Atend_Mediana        = median(N_Atendimentos),
            Contatos_Mediana     = median(N_Contatos),
            Pct_Retorno          = round(mean(N_Contatos > 1) * 100, 2),
            Pct_Feminino         = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
            Idade_Mediana        = median(Idade_Primeiro),
            Pct_Teve_Urgencia    = round(mean(Teve_Urgencia) * 100, 2),
            Pct_Teve_Fisio       = round(mean(Teve_Fisio) * 100, 2),
            Pct_Teve_J46         = round(mean(Teve_J46) * 100, 2))

dist_pacientes <- bind_rows(lapply(
  c("Sexo", "Faixa_Etaria", "Raca_Cor", "Servico_1o", "Freq_Contatos"), function(v)
    pacientes %>% count(Local, Categoria = as.character(.data[[v]])) %>%
      bind_rows(pacientes %>% count(Categoria = as.character(.data[[v]])) %>%
                  mutate(Local = "Alagoas (total)")) %>%
      group_by(Local) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup() %>%
      mutate(Variavel = v, .before = 1)))

# Maceió × interior no nível do paciente, com correção de Holm [4][15]
ign <- c("Ignorado", "Ignorada", "Sem informação")
pac_teste <- pacientes %>%
  mutate(across(c(Teve_Urgencia, Teve_J46, Teve_Fisio, Teve_Serv_Urg),
                ~ if_else(.x, "Sim", "Não")))
vars_teste_pac <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "Servico_1o", "Freq_Contatos",
                    "Teve_Urgencia", "Teve_Serv_Urg", "Teve_Fisio", "Teve_J46")
testes_local <- bind_rows(lapply(vars_teste_pac, function(v)
  teste_distribuicao(pac_teste %>% filter(!as.character(.data[[v]]) %in% ign), v, "Local")))
# (correção de Holm é global, sobre todos os testes do estudo — ver 05_municipios.R)

# [18] Raça/cor ausente: quem fica sem informação é diferente?
perfil_raca_missing <- pacientes %>%
  group_by(Raca = if_else(Raca_Cor == "Sem informação", "Sem informação", "Com informação")) %>%
  summarise(Pacientes = n(),
            Idade_Mediana = median(Idade_Primeiro),
            Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Pct_Maceio = round(mean(Local == "Maceió") * 100, 2),
            Pct_Serv_Urg_1o = round(mean(Servico_1o == "Urgência/pronto-atendimento") * 100, 2),
            Pct_Teve_Urgencia = round(mean(Teve_Urgencia) * 100, 2), .groups = "drop")

# [18] Imputação múltipla da raça/cor (sensibilidade): 20 imputações por
# regressão politômica (mice), com sexo, faixa, local, serviço do 1º
# atendimento, ano e desfechos. Compara a distribuição e a associação
# Maceió × interior (preta/parda) com a análise de casos completos.
dados_mi <- pacientes %>%
  transmute(Raca = factor(if_else(Raca_Cor == "Sem informação", NA_character_, Raca_Cor)),
            Sexo = factor(Sexo), Faixa_Etaria = droplevels(Faixa_Etaria), Local = factor(Local),
            Servico_1o = factor(Servico_1o), Ano = factor(Ano_Primeiro),
            Teve_Urgencia, Teve_J46, Contatos = pmin(N_Contatos, 6))
metodo_mi <- mice::make.method(dados_mi); metodo_mi[] <- ""; metodo_mi["Raca"] <- "polyreg"
imp_raca <- suppressWarnings(mice::mice(dados_mi, m = 20, method = metodo_mi,
                                        seed = semente_principal, printFlag = FALSE))
dist_raca_mi <- bind_rows(lapply(seq_len(imp_raca$m), function(i)
  mice::complete(imp_raca, i) %>% count(Raca) %>% mutate(p = n / sum(n), Imp = i))) %>%
  group_by(Raca) %>%
  summarise(Pct_MI = round(mean(p) * 100, 2),
            # Rubin: variância dentro (média de p(1-p)/n) + (1 + 1/m) × entre
            EP = sqrt(mean(p * (1 - p) / nrow(dados_mi)) + (1 + 1 / imp_raca$m) * var(p)) * 100,
            .groups = "drop") %>%
  mutate(IC_Inf = round(Pct_MI - 1.96 * EP, 2), IC_Sup = round(Pct_MI + 1.96 * EP, 2)) %>%
  select(-EP) %>%
  left_join(dados_mi %>% filter(!is.na(Raca)) %>% count(Raca) %>%
              mutate(Pct_Casos_Completos = round(n / sum(n) * 100, 2)) %>% select(-n), by = "Raca") %>%
  rename(Raca_Cor = Raca)
ajuste_or_local <- function(d) glm(I(Raca %in% c("Preta", "Parda")) ~ Local, family = binomial, data = d)
or_cc <- coef(summary(ajuste_or_local(dados_mi %>% filter(!is.na(Raca)))))["LocalMaceió", ]
pool_mi <- summary(mice::pool(with(imp_raca, glm(I(Raca %in% c("Preta", "Parda")) ~ Local, family = binomial))))
pool_mi <- pool_mi[pool_mi$term == "LocalMaceió", ]
raca_local_mi <- tibble(
  Analise = c("Casos completos", "Imputação múltipla (m = 20)"),
  OR_Preta_Parda_Maceio_vs_Interior = round(exp(c(or_cc[[1]], pool_mi$estimate)), 2),
  IC_Inf = round(exp(c(or_cc[[1]] - 1.96 * or_cc[[2]], pool_mi$estimate - 1.96 * pool_mi$std.error)), 2),
  IC_Sup = round(exp(c(or_cc[[1]] + 1.96 * or_cc[[2]], pool_mi$estimate + 1.96 * pool_mi$std.error)), 2),
  P_Valor = c(or_cc[[4]], pool_mi$p.value))

# Sensibilidade de idade e de período [9]
resumo_cenario <- function(df_atend, nome) {
  p <- df_atend %>% filter(CNS_Valido) %>% group_by(CNS_PAC) %>%
    summarise(Sexo = first(Sexo), Urg = any(Urgencia), J46 = any(Mal_Asmatico),
              N = n_distinct(paste(CODUNI, Mes)), .groups = "drop")
  tibble(Cenario = nome, Atendimentos = nrow(df_atend), Pacientes = nrow(p),
         Pct_Feminino = round(mean(p$Sexo == "Feminino") * 100, 2),
         Pct_Pac_Urgencia = round(mean(p$Urg) * 100, 2),
         Pct_Pac_J46 = round(mean(p$J46) * 100, 2),
         Contatos_por_Paciente = round(mean(p$N), 2),
         Pct_Atend_Serv_Urg = round(mean(df_atend$Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2))
}
sensibilidade <- bind_rows(
  resumo_cenario(atend, sprintf("Principal: 0–%d anos", idade_ped)),
  resumo_cenario(atend %>% filter(Idade <= idade_eca), sprintf("0–%d anos (ECA)", idade_eca)),
  resumo_cenario(atend %>% filter(Idade >= idade_min_sensibilidade),
                 sprintf("%d–%d anos (sem pré-escolares)", idade_min_sensibilidade, idade_ped)),
  resumo_cenario(atend %>% filter(!Covid), "Sem 2020–2021 (COVID)"))

# ==============================================================================
# 6. TENDÊNCIA E TAXAS POPULACIONAIS [3][8][9]
# ==============================================================================

pop_ano <- if (!is.null(pop_ped)) pop_ped %>% group_by(Ano) %>% summarise(Pop = sum(Pop)) else NULL
pop_faixa_ano <- if (!is.null(pop_ped)) pop_ped %>% group_by(Ano, Faixa) %>% summarise(Pop = sum(Pop), .groups = "drop") else NULL
# Padrão para taxa padronizada: população 0–19 de AL em 2022 (Censo)
pop_padrao <- if (!is.null(pop_ped)) pop_faixa_ano %>% filter(Ano == 2022) %>%
  mutate(w = Pop / sum(Pop)) %>% select(Faixa, w) else NULL

# Painel estável: estabelecimentos com atendimento pediátrico em todos os anos
# de anos_tend (relaxa para n-1, n-2 anos se ninguém cumprir)
anos_por_estab <- atend %>% filter(Ano %in% anos_tend) %>% distinct(CODUNI, Ano) %>% count(CODUNI)
for (folga in 0:3) {
  painel_estab <- anos_por_estab %>% filter(n >= length(anos_tend) - folga) %>% pull(CODUNI)
  if (length(painel_estab) >= 3) break
}
criterio_painel <- sprintf("Estabelecimentos com atendimento em ≥ %d dos %d anos de %d–%d (n = %d)",
                           length(anos_tend) - folga, length(anos_tend), min(anos_tend),
                           max(anos_tend), length(painel_estab))

# Pacientes no CEAF por ano são calculados na seção 7 e acrescentados depois
tendencia <- atend %>%
  group_by(Ano) %>%
  summarise(Atendimentos     = n(),
            Contatos         = n_distinct(paste(Cluster, CODUNI, Mes)),
            Pacientes        = n_distinct(CNS_PAC[CNS_Valido]),
            Atend_Painel     = sum(CODUNI %in% painel_estab),
            Pct_Urgencia     = round(mean(Urgencia) * 100, 2),
            Pct_Serv_Urg     = round(mean(Tipo_Servico == "Urgência/pronto-atendimento") * 100, 2),
            Pct_Fisio        = round(mean(Tipo_Atend == "Fisioterapia") * 100, 2),
            .groups = "drop") %>%
  left_join(pacientes %>% filter(Novo) %>% count(Ano = Ano_Primeiro, name = "Pacientes_Novos"), by = "Ano") %>%
  left_join(concentracao_estab %>% select(Ano, Estabelecimentos, Pct_Maior_Estab), by = "Ano")

if (!is.null(pop_ped)) {
  tendencia <- tendencia %>% left_join(pop_ano, by = "Ano") %>%
    mutate(Taxa_Atend_10mil     = round(Atendimentos / Pop * 1e4, 2),
           Taxa_Pacientes_10mil = round(Pacientes / Pop * 1e4, 2))
  # Taxa de pacientes padronizada por idade (método direto)
  taxa_padronizada <- atend_cns %>%
    filter(Faixa_Etaria %in% niveis_faixa) %>%
    group_by(Ano, Faixa = as.character(Faixa_Etaria)) %>%
    summarise(Pacientes = n_distinct(CNS_PAC), .groups = "drop") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    left_join(pop_padrao, by = "Faixa") %>%
    group_by(Ano) %>%
    summarise(Taxa_Pac_Padronizada_10mil = round(sum(Pacientes / Pop * w) * 1e4, 2), .groups = "drop")
  tendencia <- tendencia %>% left_join(taxa_padronizada, by = "Ano")
  taxa_faixa <- atend_cns %>%
    group_by(Ano, Faixa = as.character(Faixa_Etaria)) %>%
    summarise(Pacientes = n_distinct(CNS_PAC), .groups = "drop") %>%
    left_join(pop_faixa_ano, by = c("Ano", "Faixa")) %>%
    mutate(Taxa_10mil = round(Pacientes / Pop * 1e4, 2))
}

# Variação percentual anual (APC) por Prais-Winsten sobre log(taxa) [9]
apc_prais <- function(df, var, nome, excluir_covid = FALSE, periodo_anos = anos_tend) {
  d <- df %>% filter(Ano %in% periodo_anos, !is.na(.data[[var]]), .data[[var]] > 0)
  if (excluir_covid) d <- d %>% filter(!Ano %in% anos_covid)
  if (nrow(d) < 5) return(NULL)
  d$y <- log10(d[[var]]); d$id <- 1L
  m <- tryCatch(suppressMessages(prais::prais_winsten(y ~ Ano, data = d, index = c("id", "Ano"))),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  cf <- summary(m)$coefficients["Ano", ]
  b <- cf[[1]]; se <- cf[[2]]
  tibble(Serie = nome, Periodo = paste0(min(d$Ano), "–", max(d$Ano),
                                        if (excluir_covid) " (sem 2020–21)" else ""),
         APC_Pct = round((10^b - 1) * 100, 2),
         IC_Inf = round((10^(b - 1.96 * se) - 1) * 100, 2),
         IC_Sup = round((10^(b + 1.96 * se) - 1) * 100, 2),
         P_Valor = signif(cf[[4]], 3),
         Tendencia = case_when(cf[[4]] >= 0.05 ~ "Estacionária",
                               b > 0 ~ "Crescente", TRUE ~ "Decrescente"))
}

# Urgência: bruta × ajustada por estabelecimento (padronização marginal) [3]
# Só anos da série estável e estabelecimentos presentes em ≥ 2 anos: um
# estabelecimento de um ano só é inteiramente confundido com o ano.
estab_2anos <- atend %>% filter(Ano %in% anos_tend) %>% distinct(CODUNI, Ano) %>%
  count(CODUNI) %>% filter(n >= 2) %>% pull(CODUNI)
d_urg <- atend %>% filter(Carater != "Ignorado", Ano %in% anos_tend, CODUNI %in% estab_2anos) %>%
  mutate(Ano_f = factor(Ano), Estab = factor(CODUNI))
m_urg <- suppressWarnings(glm(Urgencia ~ Ano_f + Estab, family = binomial, data = d_urg))
urgencia_ajustada <- bind_rows(lapply(levels(d_urg$Ano_f), function(a) {
  nd <- d_urg %>% mutate(Ano_f = factor(a, levels = levels(d_urg$Ano_f)))
  tibble(Ano = as.integer(a),
         Pct_Ajustada = round(mean(predict(m_urg, nd, type = "response")) * 100, 2))
})) %>%
  right_join(atend %>% filter(Carater != "Ignorado") %>% group_by(Ano) %>%
               summarise(Pct_Bruta = round(mean(Urgencia) * 100, 2),
                         Pct_Painel = if (any(CODUNI %in% painel_estab))
                           round(mean(Urgencia[CODUNI %in% painel_estab]) * 100, 2) else NA_real_),
             by = "Ano") %>%
  arrange(Ano) %>% relocate(Ano, Pct_Bruta)

# [5] IC 95% da urgência ajustada: bootstrap por paciente (cluster)
padronizar_urg <- function(d) {
  m <- suppressWarnings(glm(Urgencia ~ Ano_f + Estab, family = binomial, data = d))
  if (!m$converged) return(rep(NA_real_, nlevels(d_urg$Ano_f)))   # réplica sem convergência
  sapply(levels(d_urg$Ano_f), function(a) {
    nd <- d %>% mutate(Ano_f = factor(a, levels = levels(d_urg$Ano_f)))
    mean(predict(m, nd, type = "response"))
  })
}
clusters_urg <- split(seq_len(nrow(d_urg)), d_urg$Cluster)
set.seed(semente_principal)
boot_urg <- t(replicate(n_bootstrap, {
  idx <- unlist(sample(clusters_urg, length(clusters_urg), replace = TRUE), use.names = FALSE)
  tryCatch(padronizar_urg(d_urg[idx, ] %>% mutate(Estab = droplevels(Estab))),
           error = function(e) rep(NA_real_, nlevels(d_urg$Ano_f)))
}))
# IC na escala logit com desvio-padrão robusto (MAD) das réplicas: algumas
# réplicas têm separação quase completa e distorcem os percentis
logit <- function(p) log(p / (1 - p)); expit <- function(x) 1 / (1 + exp(-x))
ponto_urg <- padronizar_urg(d_urg)
sd_rob <- apply(logit(pmin(pmax(boot_urg, 1e-4), 1 - 1e-4)), 2, mad, na.rm = TRUE)
urgencia_ajustada <- urgencia_ajustada %>%
  left_join(tibble(Ano = as.integer(levels(d_urg$Ano_f)),
                   Ajustada_IC_Inf = round(expit(logit(ponto_urg) - 1.96 * sd_rob) * 100, 2),
                   Ajustada_IC_Sup = round(expit(logit(ponto_urg) + 1.96 * sd_rob) * 100, 2)),
            by = "Ano") %>%
  relocate(Ajustada_IC_Inf, Ajustada_IC_Sup, .after = Pct_Ajustada)

# Sensibilidade: sem o maior estabelecimento de cada ano
maior_estab <- atend %>% count(Ano, CODUNI) %>% group_by(Ano) %>% slice_max(n, n = 1, with_ties = FALSE)
urgencia_sem_maior <- atend %>% anti_join(maior_estab, by = c("Ano", "CODUNI")) %>%
  group_by(Ano) %>% summarise(Pct_Urg_Sem_Maior_Estab = round(mean(Urgencia) * 100, 2))
urgencia_ajustada <- urgencia_ajustada %>% left_join(urgencia_sem_maior, by = "Ano")
