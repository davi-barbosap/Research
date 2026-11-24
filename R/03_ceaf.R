# ==============================================================================
# 7. TRATAMENTO NO CEAF (APAC DE MEDICAMENTOS) — 0–19 ANOS
# ==============================================================================

classe_med <- function(nome) {
  n <- toupper(coalesce(nome, ""))
  case_when(
    n == ""                                                       ~ "Sem nome (SIGTAP indisponível)",
    grepl("OMALIZUMABE|MEPOLIZUMABE|BENRALIZUMABE|DUPILUMABE", n) ~ "Imunobiológico",
    grepl("FORMOTEROL|SALMETEROL|VILANTEROL", n) &
      grepl("BUDESONIDA|FLUTICASONA|BECLOMETASONA|MOMETASONA", n) ~ "Corticoide inalatório + LABA",
    grepl("FORMOTEROL|SALMETEROL", n)                             ~ "LABA isolado",
    grepl("BUDESONIDA|BECLOMETASONA|FLUTICASONA|CICLESONIDA", n)  ~ "Corticoide inalatório",
    TRUE                                                          ~ "Outro")
}
med_curto <- function(m) {
  m <- toupper(coalesce(m, ""))
  case_when(
    grepl("FORMOTEROL 12.*BUDESONIDA 400.*CAPSULA", m) ~ "Formoterol 12 + budesonida 400 (cápsula)",
    grepl("FORMOTEROL 6.*BUDESONIDA 200", m)           ~ "Formoterol 6 + budesonida 200 (cápsula)",
    grepl("OMALIZUMABE|MEPOLIZUMABE", m)               ~ "Imunobiológico",
    TRUE ~ "Outros (budesonida, salmeterol, formoterol isolados; pó inalante)")
}

# Classe e apresentação pelo CÓDIGO SIGTAP (não depende do download dos nomes)
classe_cod <- function(cod) case_when(
  substr(cod, 1, 8) == "06048400"                      ~ "Imunobiológico",
  cod %in% c("0604040040", "0604040059", "0604040075") ~ "Corticoide inalatório + LABA",
  cod %in% c("0604040024", "0604040091")               ~ "LABA isolado",
  substr(cod, 1, 8) == "06042800"                      ~ "Corticoide inalatório",
  TRUE                                                 ~ "Outro")
apresentacao_cod <- function(cod) case_when(
  cod == "0604040040"                ~ "Formoterol 12 + budesonida 400 (cápsula)",
  cod == "0604040075"                ~ "Formoterol 6 + budesonida 200 (cápsula)",
  substr(cod, 1, 8) == "06048400"    ~ "Imunobiológico",
  TRUE ~ "Outros (budesonida, salmeterol, formoterol isolados; pó inalante)")

am0 <- am_raw %>% as_tibble() %>%
  mutate(Ano = as.integer(substr(AP_CMP, 1, 4)), Mes = mes_idx(AP_CMP),
         Mun_Res = substr(AP_MUNPCN, 1, 6))
am1 <- am0 %>% filter(Ano %in% anos)
am2 <- am1 %>% filter(substr(Mun_Res, 1, 2) == cod_uf_al)
am3 <- am2 %>% distinct(AP_AUTORIZ, AP_CMP, AP_PRIPAL, .keep_all = TRUE) %>%
  mutate(Num_Idade = suppressWarnings(as.integer(AP_NUIDADE)),
         Idade = case_when(AP_COIDADE == "4"           ~ Num_Idade,
                           AP_COIDADE == "5"           ~ 100L + Num_Idade,
                           AP_COIDADE %in% c("2", "3") ~ 0L,      # dias/meses (a confirmar no .cnv)
                           TRUE                        ~ NA_integer_),
         Idade = if_else(!is.na(Idade) & (Idade < 0 | Idade > idade_max), NA_integer_, Idade))

# --- [1] Volume mensal: fim dos dados válidos e meses suspeitos ---------------
volume_mensal <- am3 %>% count(Mes, name = "APAC") %>%
  complete(Mes = seq(min(Mes), max(Mes)), fill = list(APAC = 0)) %>% arrange(Mes) %>%
  mutate(Mediana_12_Ant = sapply(seq_along(Mes), function(i)
           if (i <= 12) NA_real_ else median(APAC[(i - 12):(i - 1)])),
         Mediana_Local  = sapply(seq_along(Mes), function(i) {
           j <- setdiff(max(1, i - 6):min(length(Mes), i + 6), i); median(APAC[j]) }))
mes_fim_valido <- volume_mensal %>%
  filter(!is.na(Mediana_12_Ant), APAC >= limiar_volume_fim * Mediana_12_Ant) %>%
  pull(Mes) %>% max()
volume_mensal <- volume_mensal %>%
  mutate(Competencia = format(idx_data(Mes), "%Y-%m"),
         Apos_Fim    = Mes > mes_fim_valido,
         Suspeito    = !Apos_Fim & APAC < limiar_volume_suspeito * Mediana_Local)
meses_suspeitos <- volume_mensal$Mes[volume_mensal$Suspeito]

# Aprovação vem do PA (o AM não tem campo de aprovação): só para sensibilidade
chaves_aprovadas <- pa4 %>%
  filter(DOCORIG %in% c("P", "S"), substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP)
# [5] Quantidade apresentada por APAC × competência × medicamento
qtd_med <- pa3 %>%
  filter(substr(PA_PROC_ID, 1, 2) == "06") %>%
  group_by(AP_AUTORIZ = PA_AUTORIZ, AP_CMP = PA_CMP, AP_PRIPAL = PA_PROC_ID) %>%
  summarise(Qtd = sum(suppressWarnings(as.numeric(PA_QTDPRO)), na.rm = TRUE), .groups = "drop")

am_todas <- am3 %>%
  mutate(CNS_OK = cns_valido(AP_CNSPCN)) %>%
  left_join(chaves_aprovadas %>% mutate(Aprovada = TRUE),
            by = c("AP_AUTORIZ" = "PA_AUTORIZ", "AP_CMP" = "PA_CMP")) %>%
  mutate(Aprovada = coalesce(Aprovada, FALSE)) %>%
  left_join(qtd_med, by = c("AP_AUTORIZ", "AP_CMP", "AP_PRIPAL"))

am4 <- am_todas %>% filter(CNS_OK)
# Entrada: paciente com 1ª APAC do período aos 0–19 anos. Seguimento: até
# idade_ped + margem_idade (quem entra aos 19 e faz 20 durante o tratamento
# continua sendo acompanhado; o corte por idade só ocorre aos 21).
cns_entrada_ped <- am4 %>% filter(!is.na(Idade)) %>% group_by(AP_CNSPCN) %>%
  summarise(Idade_Entrada = Idade[which.min(Mes)], .groups = "drop") %>%
  filter(Idade_Entrada <= idade_ped) %>% pull(AP_CNSPCN)
am5 <- am4 %>% filter(AP_CNSPCN %in% cns_entrada_ped, !is.na(Idade),
                      Idade <= idade_ped + margem_idade)
am6 <- am5 %>% filter(Mes <= mes_fim_valido)

fluxo_am <- tibble(
  Etapa = c("Registros AM com CID J45/J46", "Competência no período", "Residente em AL",
            "Sem duplicatas (AUTORIZ × CMP × medicamento)", "CNS válido",
            sprintf("Entrada aos 0–%d anos; seguimento até %d anos (margem de %d ano)",
                    idade_ped, idade_ped + margem_idade, margem_idade),
            sprintf("Até o fim dos dados completos (%s)", format(idx_data(mes_fim_valido), "%m/%Y"))),
  Linhas = c(nrow(am0), nrow(am1), nrow(am2), nrow(am3), nrow(am4), nrow(am5), nrow(am6))) %>%
  mutate(Excluidas = lag(Linhas) - Linhas)

classe_dose_gina <- function(idade, ug_dia) case_when(
  is.na(ug_dia)          ~ "Dose não informada",
  idade < 6              ~ "< 6 anos (fora da faixa do PCDT)",
  idade <= 11 & ug_dia <= 200 ~ "Baixa",
  idade <= 11 & ug_dia <= 400 ~ "Média",
  idade <= 11            ~ "Alta",
  ug_dia <= 400          ~ "Baixa",
  ug_dia <= 800          ~ "Média",
  TRUE                   ~ "Alta")
niveis_intensidade <- c("Baixa", "Média", "Alta", "Imunobiológico",
                        "< 6 anos (fora da faixa do PCDT)", "Dose não informada", "Outro")
ordem_intensidade <- c("Baixa" = 1, "Média" = 2, "Alta" = 3, "Imunobiológico" = 4)

am <- am6 %>%
  mutate(Sexo     = decod_sexo(AP_SEXO),
         Raca_Cor = decod_raca(AP_RACACOR),
         Local    = if_else(Mun_Res == cod_maceio, "Maceió", "Interior"),
         Margem   = Idade > idade_ped,
         Faixa_Etaria = if_else(Margem, sprintf("%d (margem de seguimento)", idade_ped + 1),
                                as.character(faixa_ped(Idade))),
         Faixa_Etaria = factor(Faixa_Etaria, levels = c(niveis_faixa,
                               sprintf("%d (margem de seguimento)", idade_ped + 1), "Ignorada")),
         Peso     = suppressWarnings(as.numeric(AM_PESO)),
         Peso     = if_else(between(Peso, faixa_peso[1], faixa_peso[2]), Peso, NA_real_),
         Altura   = suppressWarnings(as.numeric(AM_ALTURA)),
         Altura   = if_else(between(Altura, faixa_altura[1], faixa_altura[2]), Altura, NA_real_),
         Gestante = coalesce(toupper(AM_GESTANT) == "S", FALSE),
         Obito    = AP_OBITO == "1" | AP_MOTSAI %in% c("41", "42", "43"),
         Saida    = AP_MOTSAI %in% c("31", "51"),
         Ug_Bud   = unname(ug_budesonida[AP_PRIPAL]),
         Ug_Dia   = if_else(!is.na(Ug_Bud) & coalesce(Qtd, 0) > 0, Qtd * Ug_Bud / 30, NA_real_)) %>%
  nomear_proc("AP_PRIPAL") %>%
  rename(Medicamento = Nome) %>%
  mutate(Classe = classe_cod(AP_PRIPAL),
         Apresentacao = apresentacao_cod(AP_PRIPAL),
         Intensidade = case_when(
           Classe == "Imunobiológico" ~ "Imunobiológico",
           !is.na(Ug_Bud)             ~ classe_dose_gina(Idade, Ug_Dia),
           TRUE                       ~ "Outro"))

# Escore-z de IMC-para-idade (OMS): idade em meses = ponto médio do ano [11]
z_imc <- function(sexo, idade, peso, altura) {
  s  <- if_else(sexo == "Masculino", 1L, if_else(sexo == "Feminino", 2L, NA_integer_))
  ok <- !is.na(s) & !is.na(peso) & !is.na(altura) & !is.na(idade)
  meses <- idade * 12 + 6
  z <- rep(NA_real_, length(idade))
  i5 <- which(ok & idade < 5)
  if (length(i5)) {
    r <- suppressWarnings(anthro::anthro_zscores(sex = s[i5], age = meses[i5], is_age_in_month = TRUE,
                                                 weight = peso[i5], lenhei = altura[i5]))
    z[i5] <- if_else(r$fbmi == 1, NA_real_, r$zbmi)
  }
  i19 <- which(ok & idade >= 5)
  if (length(i19)) {
    r <- suppressWarnings(anthroplus::anthroplus_zscores(sex = s[i19], age_in_months = pmin(meses[i19], 228),
                                                         height_in_cm = altura[i19], weight_in_kg = peso[i19]))
    z[i19] <- if_else(r$fbfa == 1, NA_real_, r$zbfa)
  }
  z
}
am <- am %>% mutate(Z_IMC = if_else(Gestante, NA_real_, z_imc(Sexo, Idade, Peso, Altura)))

completude_am <- am %>%
  summarise(N_Registros = n(),
            Pct_Raca_Sem_Info = round(mean(Raca_Cor == "Sem informação") * 100, 2),
            Pct_Peso_Invalido = round(mean(is.na(Peso)) * 100, 2),
            Pct_Altura_Invalida = round(mean(is.na(Altura)) * 100, 2),
            Pct_Z_IMC_Ausente = round(mean(is.na(Z_IMC)) * 100, 2),
            Pct_Dose_Nao_Informada = round(mean(Intensidade == "Dose não informada") * 100, 2),
            Pct_APAC_Aprovada = round(mean(Aprovada) * 100, 2))

# Paciente novo (washout): sem APAC (qualquer idade) nos 12 meses anteriores [6]
primeira_apac <- am_todas %>% filter(CNS_OK) %>% group_by(AP_CNSPCN) %>% summarise(Mes_1a_APAC = min(Mes))

# Características do paciente: só meses aos 0–19 anos (a margem vale só para
# o seguimento dos episódios de tratamento)
pacientes_am <- am %>%
  filter(!Margem) %>%
  arrange(AP_CNSPCN, Mes) %>%
  group_by(AP_CNSPCN) %>%
  summarise(
    Mes_Primeiro  = first(Mes),
    Ano_Primeiro  = first(Ano),
    Idade         = first(Idade),
    Sexo          = first(Sexo),
    Raca_Cor      = first(Raca_Cor),
    Local         = first(Local),
    Mun_Res       = first(Mun_Res),
    Meses_Com_APAC = n_distinct(Mes),
    Apresentacao_Principal = names(which.max(table(Apresentacao))),
    Usou_Biologico = any(Classe == "Imunobiológico"),
    Intensidade_Max = {
      v <- Intensidade[Intensidade %in% names(ordem_intensidade)]
      if (length(v)) names(ordem_intensidade)[max(ordem_intensidade[v])]
      else names(which.max(table(Intensidade)))
    },
    Fora_PCDT_6anos = any(Intensidade == "< 6 anos (fora da faixa do PCDT)"),
    Z_IMC = median(Z_IMC, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(primeira_apac, by = "AP_CNSPCN") %>%
  mutate(
    Z_IMC = if_else(is.nan(Z_IMC), NA_real_, Z_IMC),
    Novo  = Mes_1a_APAC == Mes_Primeiro & Mes_Primeiro >= mes_idx(sprintf("%d01", min(anos))) + washout_meses,
    Faixa_Etaria = faixa_ped(Idade),
    Intensidade_Max = factor(Intensidade_Max, levels = niveis_intensidade),
    Estado_Nutricional = case_when(
      is.na(Z_IMC)              ~ "Sem dado válido",
      Idade < 5 & Z_IMC > 3     ~ "Obesidade",
      Idade < 5 & Z_IMC > 2     ~ "Sobrepeso",
      Idade >= 5 & Z_IMC > 2    ~ "Obesidade",
      Idade >= 5 & Z_IMC > 1    ~ "Sobrepeso",
      Z_IMC < -2                ~ "Magreza",
      TRUE                      ~ "Eutrofia"),
    Duracao = cut(Meses_Com_APAC, c(0, 1, 5, 11, 23, Inf),
                  labels = c("1 mês", "2–5", "6–11", "12–23", "24+ meses")))

resumo_am <- pacientes_am %>%
  summarise(N_Pacientes = n(),
            Pct_Novos = round(mean(Novo) * 100, 2),
            Pct_Feminino = round(sum(Sexo == "Feminino") / sum(Sexo != "Ignorado") * 100, 2),
            Idade_Mediana = median(Idade),
            Meses_APAC_Mediana = median(Meses_Com_APAC),
            Pct_Biologico = round(mean(Usou_Biologico) * 100, 2),
            N_Fora_PCDT_6anos = sum(Fora_PCDT_6anos),
            N_Com_IMC_Valido = sum(Estado_Nutricional != "Sem dado válido"),
            Pct_Excesso_Peso = round(sum(Estado_Nutricional %in% c("Sobrepeso", "Obesidade")) /
                                       pmax(N_Com_IMC_Valido, 1) * 100, 2))

# Produção por ano: todas as APAC × só aprovadas (o filtro antigo) [1]
# Só meses aos 0–19 anos: é o numerador das taxas por habitante de 0–19
pacientes_am_ano <- am %>%
  filter(!Margem) %>%
  group_by(Ano) %>%
  summarise(Pacientes_Todas_APAC = n_distinct(AP_CNSPCN),
            Pacientes_So_Aprovadas = n_distinct(AP_CNSPCN[Aprovada]),
            Pct_APAC_Aprovadas = round(mean(Aprovada) * 100, 1), .groups = "drop") %>%
  left_join(pacientes_am %>% filter(Novo) %>% count(Ano = Ano_Primeiro, name = "Pacientes_Novos"), by = "Ano")

dist_am <- bind_rows(lapply(c("Sexo", "Faixa_Etaria", "Raca_Cor", "Local", "Apresentacao_Principal",
                              "Intensidade_Max", "Estado_Nutricional", "Duracao"), function(v)
  pacientes_am %>% count(Categoria = as.character(.data[[v]])) %>%
    mutate(Variavel = v, Pct = round(n / sum(n) * 100, 2), .before = 1)))

# Apresentação e dose por faixa etária (pacientes-mês) [5]
dose_faixa <- am %>%
  count(Faixa_Etaria, Apresentacao, Intensidade) %>%
  group_by(Faixa_Etaria) %>% mutate(Pct = round(n / sum(n) * 100, 2)) %>% ungroup()
dose_resumo <- am %>% filter(!is.na(Ug_Bud)) %>%
  group_by(Apresentacao, Faixa_Etaria) %>%
  summarise(Pacientes_Mes = n(), Pct_Dose_Informada = round(mean(!is.na(Ug_Dia)) * 100, 1),
            Ug_Dia_Mediana = median(Ug_Dia, na.rm = TRUE),
            Ug_Dia_P25 = quantile(Ug_Dia, 0.25, na.rm = TRUE),
            Ug_Dia_P75 = quantile(Ug_Dia, 0.75, na.rm = TRUE), .groups = "drop")

# Apresentação × perfil (paciente), Holm [5][15]
# A quantidade registrada é quase sempre 60 cápsulas/mês: a "dose" estimada
# não acrescenta informação além da apresentação. Por isso o teste é feito
# com a apresentação principal (400 × 200 µg); a dose fica só descritiva.
pac_apr <- pacientes_am %>%
  filter(Apresentacao_Principal %in% c("Formoterol 12 + budesonida 400 (cápsula)",
                                       "Formoterol 6 + budesonida 200 (cápsula)")) %>%
  mutate(Apresentacao = factor(if_else(grepl("400", Apresentacao_Principal), "Budesonida 400", "Budesonida 200"),
                               levels = c("Budesonida 200", "Budesonida 400")))
vars_perfil_am <- c("Sexo", "Faixa_Etaria", "Raca_Cor", "Local", "Estado_Nutricional")
tratamento_perfil <- bind_rows(lapply(vars_perfil_am, function(v)
  pac_apr %>% filter(!as.character(.data[[v]]) %in% c(ign, "Sem dado válido")) %>%
    count(Categoria = as.character(.data[[v]]), Apresentacao) %>%
    group_by(Categoria) %>% mutate(N_Categoria = sum(n), Pct = round(n / N_Categoria * 100, 2)) %>%
    ungroup() %>% mutate(Variavel = v, .before = 1)))
testes_tratamento <- bind_rows(lapply(vars_perfil_am, function(v)
  teste_distribuicao(pac_apr %>% filter(!as.character(.data[[v]]) %in% c(ign, "Sem dado válido")),
                     "Apresentacao", v)))
apresentacao_faixa <- am %>% filter(!Margem) %>%
  mutate(Apresentacao = case_when(grepl("400", Apresentacao) ~ "Budesonida 400 + formoterol",
                                  grepl("200", Apresentacao) ~ "Budesonida 200 + formoterol",
                                  TRUE ~ "Outros")) %>%
  group_by(Faixa_Etaria, Apresentacao) %>% summarise(Pacientes = n_distinct(AP_CNSPCN), .groups = "drop") %>%
  group_by(Faixa_Etaria) %>% mutate(N_Faixa = sum(Pacientes), Pct = round(Pacientes / N_Faixa * 100, 2)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 7b. EPISÓDIOS, PERSISTÊNCIA (KAPLAN-MEIER) E DESFECHO DO TRATAMENTO [1][13][14]
#   Episódio = paciente × medicamento; lacuna ≥ gap_meses meses OBSERVÁVEIS sem
#   APAC abre novo episódio. Meses suspeitos (queda abrupta de volume) não
#   contam como lacuna. Desfecho (hierárquico): óbito > atingiu o limite de
#   idade do seguimento (21 anos, com a margem) e seguiu no CEAF > troca de
#   medicamento até gap_meses após o fim >
#   em uso no fim dos dados > saída registrada > interrompeu.
# ------------------------------------------------------------------------------
meses_todos <- seq(min(am3$Mes), mes_fim_valido)
obs_cum <- setNames(cumsum(!meses_todos %in% meses_suspeitos), meses_todos)
meses_obs_entre <- function(m1, m2) {           # meses observáveis estritamente entre m1 e m2
  as.integer(obs_cum[as.character(m2 - 1)] - obs_cum[as.character(m1)])
}

# idade_limite = última idade acompanhada; registros acima dela (mesmos CNS)
# indicam que o tratamento continuou, só que fora do seguimento.
construir_episodios <- function(df, idade_limite = idade_ped + margem_idade) {
  adulto <- am_todas %>% filter(CNS_OK, !is.na(Idade), Idade > idade_limite, Mes <= mes_fim_valido) %>%
    distinct(AP_CNSPCN, Mes)
  base <- df %>%
    distinct(AP_CNSPCN, AP_PRIPAL, Apresentacao, Ug_Bud, Mes, .keep_all = TRUE) %>%
    arrange(AP_CNSPCN, AP_PRIPAL, Mes) %>%
    group_by(AP_CNSPCN, AP_PRIPAL) %>%
    mutate(Lacuna = if_else(row_number() == 1, NA_integer_, meses_obs_entre(lag(Mes), Mes)),
           Episodio = cumsum(is.na(Lacuna) | Lacuna >= gap_meses)) %>%
    ungroup()
  ep <- base %>%
    group_by(AP_CNSPCN, AP_PRIPAL, Apresentacao, Ug_Bud, Episodio) %>%
    summarise(Ini = min(Mes), Fim = max(Mes), Meses_Dispensados = n(),
              Idade_Ini = first(Idade), Obito = any(Obito), Saida = any(Saida),
              Classe = first(Classe), .groups = "drop")
  # Troca: outro medicamento do paciente iniciado entre Ini e Fim + gap
  troca <- ep %>% select(AP_CNSPCN, AP_PRIPAL, Episodio, Ini, Fim) %>%
    inner_join(ep %>% select(AP_CNSPCN, P2 = AP_PRIPAL, Ini2 = Ini, Ug2 = Ug_Bud, Classe2 = Classe),
               by = "AP_CNSPCN", relationship = "many-to-many") %>%
    filter(P2 != AP_PRIPAL, Ini2 > Ini, Ini2 <= Fim + gap_meses) %>%
    group_by(AP_CNSPCN, AP_PRIPAL, Episodio) %>%
    slice_min(Ini2, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(AP_CNSPCN, AP_PRIPAL, Episodio, Ug2, Classe2)
  virou_adulto <- ep %>% select(AP_CNSPCN, AP_PRIPAL, Episodio, Fim) %>%
    inner_join(adulto, by = "AP_CNSPCN", relationship = "many-to-many") %>%
    filter(Mes > Fim, Mes <= Fim + gap_meses + 1) %>%
    distinct(AP_CNSPCN, AP_PRIPAL, Episodio) %>% mutate(Virou_Adulto = TRUE)
  ep %>%
    left_join(troca, by = c("AP_CNSPCN", "AP_PRIPAL", "Episodio")) %>%
    left_join(virou_adulto, by = c("AP_CNSPCN", "AP_PRIPAL", "Episodio")) %>%
    mutate(
      Virou_Adulto = coalesce(Virou_Adulto, FALSE),
      Desfecho = case_when(
        Obito                                         ~ "Óbito registrado",
        Virou_Adulto                                  ~ "Atingiu o limite de idade do seguimento (segue no CEAF)",
        !is.na(Classe2) & Classe2 == "Imunobiológico" ~ "Trocou para imunobiológico",
        !is.na(Classe2) & coalesce(Ug2 > Ug_Bud, FALSE) ~ "Trocou para cápsula de maior dose",
        !is.na(Classe2) & coalesce(Ug2 < Ug_Bud, FALSE) ~ "Trocou para cápsula de menor dose",
        !is.na(Classe2)                               ~ "Trocou (outro medicamento)",
        Fim >= mes_fim_valido - (gap_meses - 1)       ~ "Em uso no fim dos dados",
        Saida                                         ~ "Saída registrada (encerramento/transferência)",
        TRUE                                          ~ "Interrompeu (≥ 3 meses sem dispensação)"),
      Meses_Seguimento = Fim - Ini + 1,
      Evento_Interrupcao = Desfecho == "Interrompeu (≥ 3 meses sem dispensação)") %>%
    # Margem só prolonga episódios iniciados até 19 anos; episódio que começa
    # na margem (20 anos) é início de tratamento adulto e sai da análise
    filter(Idade_Ini <= idade_ped)
}

niveis_desfecho <- c("Em uso no fim dos dados", "Trocou para cápsula de maior dose",
                     "Trocou para imunobiológico", "Trocou para cápsula de menor dose",
                     "Trocou (outro medicamento)", "Atingiu o limite de idade do seguimento (segue no CEAF)",
                     "Saída registrada (encerramento/transferência)",
                     "Interrompeu (≥ 3 meses sem dispensação)", "Óbito registrado")

episodios <- construir_episodios(am) %>% mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))
episodios_aprov <- construir_episodios(am %>% filter(Aprovada)) %>%
  mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))
# Sensibilidade: sem a margem (seguimento cortado aos 20 anos, versão anterior)
episodios_sem_margem <- construir_episodios(am %>% filter(!Margem), idade_limite = idade_ped) %>%
  mutate(Desfecho = factor(Desfecho, levels = niveis_desfecho))

resumir_desfecho <- function(ep, rotulo) ep %>%
  count(Apresentacao, Desfecho, .drop = FALSE) %>%
  group_by(Apresentacao) %>% filter(sum(n) > 0) %>%
  mutate(Episodios = sum(n), Pct = round(n / Episodios * 100, 2)) %>% ungroup() %>%
  mutate(Base = rotulo, .before = 1)

desfecho_medicamento <- resumir_desfecho(episodios, "Todas as APAC (principal)")
desfecho_sensibilidade <- bind_rows(
  desfecho_medicamento,
  resumir_desfecho(episodios_aprov, "Só APAC aprovadas (sensibilidade)"),
  resumir_desfecho(episodios_sem_margem, sprintf("Sem margem: seguimento só até %d anos", idade_ped))) %>%
  filter(Desfecho %in% c("Em uso no fim dos dados", "Interrompeu (≥ 3 meses sem dispensação)",
                         "Atingiu o limite de idade do seguimento (segue no CEAF)")) %>%
  select(Base, Apresentacao, Desfecho, Episodios, Pct)

motivo_saida_am <- am %>%
  mutate(Motivo = case_when(AP_MOTSAI == "21" ~ "Permanência (característica da doença)",
                            AP_MOTSAI == "31" ~ "Transferência",
                            AP_MOTSAI %in% c("41", "42", "43") ~ "Óbito",
                            AP_MOTSAI == "51" ~ "Encerramento administrativo",
                            TRUE ~ paste("Código", AP_MOTSAI))) %>%
  count(Motivo, name = "APAC") %>% mutate(Pct = round(APAC / sum(APAC) * 100, 3))

# Kaplan-Meier: tempo até interrupção (censura: em uso, troca, 20 anos, óbito)
km_fit <- survfit(Surv(Meses_Seguimento, Evento_Interrupcao) ~ Apresentacao,
                  data = episodios)
km_tab <- summary(km_fit, times = c(3, 6, 12, 24), extend = TRUE)
nomes_estratos <- if (is.null(km_fit$strata)) unique(episodios$Apresentacao) else
  sub("Apresentacao=", "", names(km_fit$strata))
tab_km <- summary(km_fit)$table
medianas_km <- if (is.matrix(tab_km)) tab_km[, "median"] else tab_km[["median"]]
persistencia_km <- tibble(
  Apresentacao = if (is.null(km_tab$strata)) nomes_estratos else sub("Apresentacao=", "", as.character(km_tab$strata)),
  Meses = km_tab$time, Em_Risco = km_tab$n.risk,
  Persistentes_Pct = round(km_tab$surv * 100, 1),
  IC_Inf = round(km_tab$lower * 100, 1), IC_Sup = round(km_tab$upper * 100, 1)) %>%
  left_join(tibble(Apresentacao = nomes_estratos, Mediana_Meses = unname(medianas_km)), by = "Apresentacao")
km_curvas <- tibble(
  Apresentacao = if (is.null(km_fit$strata)) nomes_estratos else rep(nomes_estratos, km_fit$strata),
  Meses = km_fit$time, Persistentes = km_fit$surv * 100)

# ==============================================================================
# 8. VÍNCULO ATENDIMENTO (BI) × TRATAMENTO (AM) PELO CNS [10]
# ==============================================================================
cns_am_ped <- unique(am$AP_CNSPCN)
cns_am_qualquer <- unique(am_todas$AP_CNSPCN[am_todas$CNS_OK])

pacientes <- pacientes %>%
  mutate(Em_AM = CNS_PAC %in% cns_am_ped,
         Em_Tratamento_AM = if_else(Em_AM, "Recebe medicamento (CEAF)", "Sem APAC de medicamento"))

vinculo_bi_am <- tibble(
  Pacientes_BI = nrow(pacientes), Pacientes_AM = length(cns_am_ped),
  Em_Ambos = sum(pacientes$Em_AM),
  Pct_BI_com_AM = round(Em_Ambos / Pacientes_BI * 100, 2),
  Pct_AM_com_BI = round(Em_Ambos / Pacientes_AM * 100, 2))

vinculo_perfil <- pacientes %>%
  group_by(Grupo = Em_Tratamento_AM) %>%
  summarise(Pacientes = n(), Idade_Mediana = median(Idade_Primeiro),
            Pct_Feminino = round(mean(Sexo == "Feminino") * 100, 2),
            Pct_Serv_Urg_1o = round(mean(Servico_1o == "Urgência/pronto-atendimento") * 100, 2),
            Contatos_Mediana = median(N_Contatos),
            k = sum(Teve_Urgencia), .groups = "drop") %>%
  mutate(Pct_Teve_Urgencia = round(k / Pacientes * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$k, .$Pacientes) %>% round(2)) %>% select(-k)

d_vinc <- pacientes %>%
  filter(Sexo != "Ignorado") %>%
  mutate(Faixa_Etaria = droplevels(Faixa_Etaria), Ano_1o = factor(Ano_Primeiro),
         Servico_1o = relevel(factor(Servico_1o), ref = "Ambulatório especializado"))
or_tab <- function(m, rotulo) {
  cf <- coeftest(m, vcov = vcovHC(m, type = "HC0"))["Em_AMTRUE", ]
  tibble(Modelo = rotulo, OR = round(exp(cf[[1]]), 2),
         IC_Inf = round(exp(cf[[1]] - 1.96 * cf[[2]]), 2),
         IC_Sup = round(exp(cf[[1]] + 1.96 * cf[[2]]), 2), P_Valor = signif(cf[[4]], 3))
}
vinculo_modelo <- bind_rows(
  or_tab(glm(Teve_Urgencia ~ Em_AM, binomial, d_vinc), "Bruto"),
  or_tab(glm(Teve_Urgencia ~ Em_AM + Faixa_Etaria + Sexo + Local + Ano_1o, binomial, d_vinc),
         "Ajustado: faixa, sexo, local, ano do 1º atendimento"),
  or_tab(suppressWarnings(glm(Teve_Urgencia ~ Em_AM + Faixa_Etaria + Sexo + Local + Ano_1o + Servico_1o,
                              binomial, d_vinc)),
         "Ajustado + tipo de serviço do 1º atendimento")) %>%
  mutate(Desfecho = "Teve ≥ 1 atendimento de urgência por asma", .before = 1)

vinculo_faixa <- pacientes %>%
  group_by(Faixa_Etaria) %>%
  summarise(Pacientes_BI = n(), Com_AM = sum(Em_AM), .groups = "drop") %>%
  mutate(Pct = round(Com_AM / Pacientes_BI * 100, 2)) %>%
  bind_cols(wilson_ci_pct(.$Com_AM, .$Pacientes_BI) %>% round(2))

# ==============================================================================
# 9. ANTES × DEPOIS DO INÍCIO DO TRATAMENTO — janelas múltiplas + controles [10]
#   Casos: pacientes NOVOS no CEAF (washout) presentes no BI (qualquer idade),
#   com 24 meses observáveis antes e depois do início. Desfecho: atendimentos
#   de urgência por asma no BI por janela de 12 meses.
#   Controles: crianças do BI SEM nenhuma APAC de medicamento, pareadas por
#   sexo, faixa etária e nº de urgências nos 12 meses antes do mês índice.
# ------------------------------------------------------------------------------
bi_todos <- bi3 %>% filter(cns_valido(CNS_PAC)) %>%
  transmute(CNS_PAC, Mes, Urgencia = decod_carater(CATEND) == "Urgência")
mes_fim_bi <- max(bi_todos$Mes)
janelas <- tibble(Janela = c("−24 a −13", "−12 a −1", "+1 a +12", "+13 a +24"),
                  De = c(-24, -12, 1, 13), Ate = c(-13, -1, 12, 24))

contar_janelas <- function(ids_index) {
  ids_index %>%
    left_join(bi_todos, by = "CNS_PAC", relationship = "many-to-many") %>%
    mutate(d = Mes - Mes_Index) %>%
    tidyr::crossing(janelas) %>%
    group_by(ID, CNS_PAC, Grupo, Janela, De) %>%
    summarise(Urg = sum(Urgencia & d >= De & d <= Ate, na.rm = TRUE),
              Atend = sum(d >= De & d <= Ate, na.rm = TRUE), .groups = "drop") %>%
    mutate(Janela = factor(Janela, levels = janelas$Janela))
}

casos <- pacientes_am %>%
  filter(Novo, AP_CNSPCN %in% bi_todos$CNS_PAC,
         Mes_Primeiro >= mes_ini_bi + 24, Mes_Primeiro <= mes_fim_bi - 24) %>%
  transmute(ID = paste0("caso_", row_number()), CNS_PAC = AP_CNSPCN, Grupo = "Caso (inicia CEAF)",
            Mes_Index = Mes_Primeiro, Sexo, Faixa_Etaria)

janelas_casos <- contar_janelas(casos)

resumo_janelas <- janelas_casos %>%
  group_by(Grupo, Janela) %>%
  summarise(Pacientes = n_distinct(ID), Pct_com_Urgencia = round(mean(Urg > 0) * 100, 2),
            Urg_por_Paciente = round(mean(Urg), 3), Atend_por_Paciente = round(mean(Atend), 3),
            .groups = "drop")

rr_janelas <- tryCatch({
  d <- janelas_casos %>% group_by(ID) %>% filter(sum(Urg) > 0) %>% ungroup() %>%
    mutate(Janela = relevel(Janela, ref = "−12 a −1"))
  m <- glm(Urg ~ Janela + factor(ID), family = poisson, data = d)
  cf <- coef(summary(m)); cf <- cf[grepl("^Janela", rownames(cf)), , drop = FALSE]
  tibble(Janela = sub("^Janela", "", rownames(cf)), RR = round(exp(cf[, 1]), 2),
         IC_Inf = round(exp(cf[, 1] - 1.96 * cf[, 2]), 2),
         IC_Sup = round(exp(cf[, 1] + 1.96 * cf[, 2]), 2), P_Valor = signif(cf[, 4], 3),
         Pacientes_com_Evento = n_distinct(d$ID), Referencia = "−12 a −1")
}, error = function(e) tibble(Janela = NA, Observacao = conditionMessage(e)))

# Controles pareados e diferença-em-diferenças (janelas −12..−1 × +1..+12)
# Pareamento: sexo, faixa etária no mês índice e Nº DE URGÊNCIAS nos 12 meses
# anteriores (0, 1, 2+), além de ≥ 1 atendimento no BI nesse período (casos e
# controles). Parear pela urgência prévia evita que os controles "melhorem"
# só por regressão à média. Sensibilidade: o sorteio é repetido com várias
# sementes e se reporta a distribuição da razão de taxas.
idade_no_mes <- bi %>% filter(cns_valido(CNS_PAC), !is.na(Data_Nasc)) %>%
  distinct(CNS_PAC, Data_Nasc) %>% group_by(CNS_PAC) %>% slice(1) %>% ungroup()
sexo_bi <- atend_cns %>% distinct(CNS_PAC, Sexo) %>% group_by(CNS_PAC) %>% slice(1) %>% ungroup()
pool_controles <- pacientes %>% filter(!CNS_PAC %in% cns_am_qualquer) %>%
  select(CNS_PAC) %>% left_join(idade_no_mes, by = "CNS_PAC") %>% left_join(sexo_bi, by = "CNS_PAC")

historico_pre <- function(cns, m) {
  bi_todos %>% filter(CNS_PAC %in% cns, Mes >= m - 12, Mes <= m - 1) %>%
    group_by(CNS_PAC) %>%
    summarise(Urg_Pre_Cat = cut(sum(Urgencia), c(-1, 0, 1, Inf), labels = c("0", "1", "2+")),
              .groups = "drop")
}
casos_did <- bind_rows(lapply(seq_len(nrow(casos)), function(i)
  casos[i, ] %>% inner_join(historico_pre(casos$CNS_PAC[i], casos$Mes_Index[i]), by = "CNS_PAC")))

# Candidatos de cada caso (fixos); o sorteio é que muda com a semente
candidatos <- lapply(seq_len(nrow(casos_did)), function(i) {
  cs <- casos_did[i, ]
  cand <- pool_controles %>%
    mutate(Idade_Index = floor(as.numeric(idx_data(cs$Mes_Index) - Data_Nasc) / 365.25),
           Faixa_Index = faixa_ped(Idade_Index)) %>%
    filter(Sexo == cs$Sexo, Faixa_Index == cs$Faixa_Etaria)
  cand %>% inner_join(historico_pre(cand$CNS_PAC, cs$Mes_Index), by = "CNS_PAC") %>%
    filter(Urg_Pre_Cat == cs$Urg_Pre_Cat)
})
janelas_did_base <- NULL
sortear_controles <- function(semente) {
  set.seed(semente)
  bind_rows(lapply(seq_len(nrow(casos_did)), function(i) {
    cand <- candidatos[[i]]; cs <- casos_did[i, ]
    if (!nrow(cand)) return(NULL)
    cand %>% slice_sample(n = min(controles_por_caso, nrow(cand))) %>%
      transmute(ID = paste0(cs$ID, "_ctrl", row_number()), CNS_PAC, Grupo = "Controle pareado",
                Mes_Index = cs$Mes_Index, Sexo, Faixa_Etaria = Faixa_Index)
  }))
}
estimar_did <- function(controles) {
  jd <- contar_janelas(bind_rows(casos_did %>% select(-Urg_Pre_Cat) %>%
                                   mutate(Grupo = "Caso (inicia CEAF)"), controles)) %>%
    filter(Janela %in% c("−12 a −1", "+1 a +12")) %>%
    mutate(Pos = Janela == "+1 a +12", Caso = Grupo == "Caso (inicia CEAF)")
  d <- jd %>% group_by(ID) %>% filter(sum(Urg) > 0) %>% ungroup()  # Poisson condicional
  m <- suppressWarnings(glm(Urg ~ Pos * Caso + factor(ID), family = poisson, data = d))
  b <- coef(m)[["PosTRUE:CasoTRUE"]]
  v <- tryCatch(vcovCL(m, cluster = ~ID)["PosTRUE:CasoTRUE", "PosTRUE:CasoTRUE"], error = function(e) NA)
  se <- if (is.finite(v) && v > 0) sqrt(v) else coef(summary(m))["PosTRUE:CasoTRUE", 2]
  list(janelas = jd, b = b, se = se)
}

controles <- sortear_controles(semente_principal)
did_principal <- tryCatch(estimar_did(controles), error = function(e) NULL)
janelas_did <- if (!is.null(did_principal)) did_principal$janelas else tibble()
resumo_did <- janelas_did %>% group_by(Grupo, Janela) %>%
  summarise(Pacientes = n_distinct(ID), Pct_com_Urgencia = round(mean(Urg > 0) * 100, 2),
            Urg_por_Paciente = round(mean(Urg), 3), .groups = "drop")
did_modelo <- if (!is.null(did_principal) && is.finite(did_principal$b)) with(did_principal, tibble(
  Termo = "Razão de taxas (DiD): mudança pós/pré nos casos ÷ nos controles",
  RR = round(exp(b), 2), IC_Inf = round(exp(b - 1.96 * se), 2), IC_Sup = round(exp(b + 1.96 * se), 2),
  P_Valor = signif(2 * pnorm(-abs(b / se)), 3),
  Casos = nrow(casos_did), Casos_Com_Controle = n_distinct(sub("_ctrl.*", "", controles$ID)),
  Controles = nrow(controles))) else
  tibble(Termo = "DiD não estimável", Casos = nrow(casos_did), Controles = nrow(controles))

# Sensibilidade ao sorteio dos controles
rr_sementes <- sapply(seq_len(n_sementes_did), function(s)
  tryCatch(exp(estimar_did(sortear_controles(1000 + s))$b), error = function(e) NA_real_))
did_sementes <- tibble(Sorteios = sum(is.finite(rr_sementes)),
                       RR_Mediana = round(median(rr_sementes, na.rm = TRUE), 2),
                       RR_P25 = round(quantile(rr_sementes, 0.25, na.rm = TRUE), 2),
                       RR_P75 = round(quantile(rr_sementes, 0.75, na.rm = TRUE), 2),
                       Pct_RR_Maior_1 = round(mean(rr_sementes > 1, na.rm = TRUE) * 100, 1))
