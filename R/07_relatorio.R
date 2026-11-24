# ==============================================================================
# EXPORTAÇÃO: figuras, bases, anexo Excel e relatório DOCX
# ==============================================================================

# --- Figuras (numeradas na ordem em que aparecem no relatório) -----------------
figuras <- list(
  "Fig01_indicadores_atendimentos"   = list(p_indicadores, 12, 7.5),
  "Fig02_urgencia_por_servico"       = list(p_urg_servico, 11, 5.5),
  "Fig03_tipo_atendimento"           = list(p_tipo_atend, 10, 6),
  "Fig04_piramide_pacientes"         = list(p_piramide, 10, 6),
  "Fig05_taxas_anuais"               = list(p_tendencia, 11, 6.5),
  "Fig06_urgencia_ajustada"          = list(p_urg_tend, 11, 6),
  "Fig07_volume_mensal_APAC"         = list(p_volume, 11, 5.5),
  "Fig08_medicamento_por_faixa"      = list(p_apresentacao, 10, 6),
  "Fig09_estado_nutricional"         = list(p_nutricional, 11, 6),
  "Fig10_desfecho_tratamento"        = list(p_desfecho, 11, 5.5),
  "Fig11_permanencia_tratamento_KM"  = list(p_km, 10, 6),
  "Fig12_CEAF_x_urgencia_OR"         = list(p_vinculo, 10, 5),
  "Fig13_antes_depois_tratamento"    = list(p_antes_depois, 11, 6),
  "Fig14_internacoes_rede"           = list(p_intern_rede, 11, 6),
  "Fig15_internacoes_sazonalidade"   = list(p_sazonal, 10, 5.5),
  "Fig16_regioes_saude"              = list(p_regiao, 12, 6.5),
  "Fig17_CEAF_x_internacao_municipios" = list(p_eco_int, 11, 7))
figuras <- Filter(function(f) !is.null(f[[1]]), figuras)
invisible(mapply(function(nome, f) salvar_grafico(f[[1]], nome, w = f[[2]], h = f[[3]]),
                 names(figuras), figuras))

# --- Bases (ficam fora do git) -------------------------------------------------
saveRDS(atend, file.path(output_dir, "Base_Asma_Ped_atendimentos.rds"))
saveRDS(pacientes, file.path(output_dir, "Base_Asma_Ped_pacientes_BI.rds"))
saveRDS(pacientes_am, file.path(output_dir, "Base_Asma_Ped_pacientes_AM.rds"))
saveRDS(episodios, file.path(output_dir, "Base_Asma_Ped_episodios.rds"))
saveRDS(intern, file.path(output_dir, "Base_Asma_Ped_internacoes.rds"))

# --- Anexo Excel: todas as tabelas completas -----------------------------------
x_ <- function(nome) if (exists(nome)) get(nome) else NULL
anexo <- list(
  "Fluxo PA" = fluxograma_pa, "Fluxo BI" = fluxo_bi, "Consistencia PA x BI" = consistencia_pa_bi,
  "Completude atendimentos" = completude, "Concentracao estab" = concentracao_estab,
  "Indicadores" = indicadores, "Urgencia por servico" = urgencia_servico,
  "Distribuicoes atendimentos" = distribuicoes, "Tipo atendimento faixa" = tipo_atend_faixa,
  "Resumo pacientes" = resumo_pacientes, "Distribuicoes pacientes" = dist_pacientes,
  "Testes Maceio x interior" = testes_local, "Raca sem informacao" = perfil_raca_missing,
  "Raca imputacao" = dist_raca_mi, "Raca imputacao OR" = raca_local_mi,
  "Sensibilidade" = sensibilidade, "Serie anual" = tendencia, "Taxa por faixa" = x_("taxa_faixa"),
  "APC ambulatorio CEAF" = apc, "Urgencia ajustada" = urgencia_ajustada,
  "Volume mensal APAC" = volume_mensal %>% select(Competencia, APAC, Suspeito, Apos_Fim),
  "Fluxo AM" = fluxo_am, "Completude AM" = completude_am, "Resumo CEAF" = resumo_am,
  "CEAF por ano" = pacientes_am_ano, "Distribuicoes CEAF" = dist_am,
  "Medicamento por faixa" = apresentacao_faixa, "Dose estimada" = dose_resumo,
  "Apresentacao x perfil" = tratamento_perfil, "Testes apresentacao" = testes_tratamento,
  "Motivo saida APAC" = motivo_saida_am, "Desfecho tratamento" = desfecho_medicamento,
  "Desfecho sensibilidade" = desfecho_sensibilidade, "Persistencia KM" = persistencia_km,
  "Vinculo BI x CEAF" = vinculo_bi_am, "Vinculo perfil" = vinculo_perfil,
  "Vinculo regressao" = vinculo_modelo, "Vinculo por faixa" = vinculo_faixa,
  "Antes depois janelas" = resumo_janelas, "Antes depois RR" = rr_janelas,
  "DiD resumo" = resumo_did, "DiD modelo" = did_modelo, "DiD sorteios" = did_sementes,
  "Fluxo SIH" = fluxo_sih, "Resumo internacoes" = resumo_intern,
  "Internacao por faixa" = perfil_intern_faixa, "Desfecho hospitalar" = desfecho_hospitalar,
  "Distribuicoes internacao" = dist_intern, "Internacoes por ano" = intern_ano,
  "Internacoes rede" = intern_rede, "Concentracao hospitais" = concentracao_hosp,
  "APC internacao" = x_("apc_intern"), "Sazonalidade" = sazonalidade,
  "Reinternacao" = reinternacoes, "Reinternacao por faixa" = reintern_faixa,
  "Asma diag secundario SIH" = sih_asma_secundaria, "Municipios" = asma_mun,
  "Regioes de saude" = x_("taxas_regiao"), "Maceio x interior taxas" = x_("razao_taxas_local"),
  "Ecologico urgencia" = eco, "Ecologico internacao" = x_("eco_internacao"),
  "Testes Holm global" = testes_globais, "Arquivos com falha" = arquivos_falha)
anexo <- Filter(function(d) !is.null(d) && nrow(d) > 0, anexo)
wb <- openxlsx::createWorkbook()
estilo_cab <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "bottom", wrapText = TRUE)
for (aba in names(anexo)) {
  d <- as.data.frame(anexo[[aba]]); names(d) <- rotulo_coluna(names(d))
  openxlsx::addWorksheet(wb, substr(aba, 1, 31))
  openxlsx::writeData(wb, substr(aba, 1, 31), d, headerStyle = estilo_cab)
  openxlsx::setColWidths(wb, substr(aba, 1, 31), cols = seq_along(d), widths = "auto")
  openxlsx::freezePane(wb, substr(aba, 1, 31), firstRow = TRUE)
}
openxlsx::saveWorkbook(wb, file.path(output_dir, "Asma_Ped_Anexo_Tabelas.xlsx"), overwrite = TRUE)
saveRDS(anexo, file.path(output_dir, "Asma_Ped_resultados.rds"))

# ==============================================================================
# RELATÓRIO DOCX — tabelas e figuras numeradas, com fonte e nota
# ==============================================================================
mes_fim_txt <- format(idx_data(mes_fim_valido), "%m/%Y")
susp_txt <- if (length(meses_suspeitos)) paste(format(idx_data(meses_suspeitos), "%m/%Y"), collapse = ", ") else "nenhum"
cont <- new.env(); cont$tab <- 0; cont$fig <- 0

par_negrito <- function(doc, rotulo, texto) {
  body_add_fpar(doc, fpar(ftext(rotulo, fp_text(bold = TRUE, font.size = 10)),
                          ftext(texto, fp_text(font.size = 10))))
}
nota <- function(doc, texto) body_add_fpar(doc, fpar(ftext(texto, fp_text(italic = TRUE, font.size = 8.5, color = "#404040"))))
texto <- function(doc, ...) body_add_par(doc, paste0(...), style = "Normal")
secao <- function(doc, titulo) body_add_par(doc, titulo, style = "heading 2")

add_tab <- function(doc, df, titulo, nota_txt = NULL, fonte = NULL) {
  force(doc)                       # garante numeração na ordem do documento
  if (is.null(df) || !nrow(df)) return(doc)
  cont$tab <- cont$tab + 1
  doc <- par_negrito(doc, sprintf("Tabela %d. ", cont$tab), titulo)
  doc <- body_add_flextable(doc, ft(df, larg_max = 6.5))
  if (!is.null(nota_txt)) doc <- nota(doc, paste("Nota:", nota_txt))
  if (!is.null(fonte)) doc <- nota(doc, fonte)
  body_add_par(doc, "")
}
add_fig <- function(doc, nome, titulo) {
  force(doc)
  f <- file.path(dir_figuras, paste0(nome, ".png"))
  if (!file.exists(f) || is.null(figuras[[nome]])) return(doc)
  cont$fig <- cont$fig + 1
  d <- figuras[[nome]]
  doc %>% body_add_img(f, width = 6.5, height = 6.5 * d[[3]] / d[[2]]) %>%
    par_negrito(sprintf("Figura %d. ", cont$fig), titulo) %>% body_add_par("")
}
transpor <- function(df, rotulos = NULL) {          # tabela de 1 linha → Indicador | Valor
  v <- unlist(lapply(df[1, ], function(x) if (is.numeric(x)) num_br(x, if (all(x == round(x))) 0 else 2) else as.character(x)))
  tibble(Indicador = rotulo_coluna(names(df)), Valor = v)
}
f_bi <- "Fonte: SIASUS/DATASUS (BPA individualizado)."
f_am <- "Fonte: SIASUS/DATASUS (APAC de medicamentos, CEAF)."
f_sih <- "Fonte: SIH/DATASUS (AIH reduzida)."

doc <- read_docx() %>%
  body_add_par("Asma em crianças e adolescentes (0–19 anos) no SUS — Alagoas, 2015–2025", style = "heading 1") %>%
  texto("Atendimentos ambulatoriais (SIASUS, BPA individualizado), tratamento no Componente Especializado da ",
        "Assistência Farmacêutica (CEAF, APAC de medicamentos) e internações hospitalares (SIH) de residentes ",
        "em Alagoas. A idade é medida na data de cada registro. A atenção primária (SISAB) não está incluída. ",
        "Métodos revisados após auditoria (Auditoria_Asma_SIASUS.md). Tabelas completas no arquivo ",
        "Asma_Ped_Anexo_Tabelas.xlsx; figuras em alta resolução (TIFF, 300 dpi) na pasta figuras/.") %>%

  # 1 ---------------------------------------------------------------------------
  secao("1. Seleção dos registros e qualidade dos dados") %>%
  add_tab(fluxo_bi, "Seleção dos atendimentos ambulatoriais (BPA individualizado).", fonte = f_bi) %>%
  add_tab(transpor(completude), "Completude dos atendimentos de 0–19 anos.", fonte = f_bi) %>%
  add_tab(concentracao_estab %>% select(Ano, Atendimentos, Estabelecimentos, Maior_Estabelecimento, Pct_Maior_Estab),
          "Concentração dos atendimentos por estabelecimento, por ano.",
          "A troca do estabelecimento dominante entre os anos altera o perfil de registro (ver seção 4).", f_bi) %>%

  # 2 ---------------------------------------------------------------------------
  secao("2. Perfil dos atendimentos") %>%
  add_fig("Fig01_indicadores_atendimentos", "Perfil dos atendimentos por asma, Alagoas, Maceió e interior.") %>%
  add_tab(indicadores %>% filter(Estratificacao != "Faixa_Etaria") %>%
            select(Indicador, Grupo, k, n, Pct, IC_Inf_Pct, IC_Sup_Pct),
          "Indicadores dos atendimentos com IC 95% robusto a atendimentos repetidos do mesmo paciente.",
          "Descritivo; o intervalo usa o paciente (CNS) como conglomerado.", f_bi) %>%
  add_fig("Fig02_urgencia_por_servico", "Atendimentos de caráter de urgência segundo o tipo de serviço e a residência.") %>%
  add_fig("Fig03_tipo_atendimento", "Tipo de atendimento segundo a faixa etária.") %>%

  # 3 ---------------------------------------------------------------------------
  secao("3. Pacientes atendidos (unidade de inferência)") %>%
  add_tab(transpor(resumo_pacientes), "Características dos pacientes atendidos no ambulatório.",
          "Contato = paciente × estabelecimento × mês (evita contar várias sessões de fisioterapia).", f_bi) %>%
  add_fig("Fig04_piramide_pacientes", "Pacientes atendidos por asma segundo sexo e faixa etária.") %>%
  add_tab(testes_local %>% select(Variavel, N, Qui2, gl, P_Valor, P_Holm_Global, V_Cramer, Magnitude),
          "Comparação Maceió × interior no nível do paciente.",
          "Qui-quadrado (Monte Carlo quando > 20% das caselas têm esperado < 5); V de Cramér com limiares de Cohen ajustados pelos graus de liberdade; p corrigido por Holm sobre todos os testes do estudo (seção 10).",
          f_bi) %>%
  add_tab(dist_raca_mi, "Raça/cor dos pacientes: casos completos × imputação múltipla.",
          sprintf("%s%% dos pacientes sem raça/cor; imputação por regressão politômica (20 imputações), regras de Rubin.",
                  num_br(mean(pacientes$Raca_Cor == "Sem informação") * 100, 1)), f_bi) %>%
  add_tab(raca_local_mi, "Raça/cor preta ou parda em Maceió × interior: casos completos e imputação múltipla.", fonte = f_bi) %>%
  add_tab(sensibilidade, "Análises de sensibilidade (idade e período).", fonte = f_bi) %>%

  # 4 ---------------------------------------------------------------------------
  secao("4. Tendência e taxas populacionais") %>%
  add_fig("Fig05_taxas_anuais", "Crianças atendidas, em tratamento no CEAF e internações por 10 mil habitantes de 0–19 anos.") %>%
  add_tab(tendencia %>% select(Ano, Pacientes, Taxa_Pacientes_10mil, Taxa_Pac_Padronizada_10mil, Pacientes_CEAF,
                               Taxa_CEAF_10mil, Internacoes, Taxa_Intern_10mil),
          "Série anual de pacientes e internações por 10 mil habitantes de 0–19 anos.",
          sprintf("Padronização direta por idade (população de 0–19 anos de Alagoas, 2022). População de %s repetida em 2025.",
                  pop_ano_disponivel), "Fonte: SIASUS e SIH/DATASUS; população DATASUS (brpop).") %>%
  add_tab(apc,
          "Variação percentual anual (APC) por regressão de Prais-Winsten.",
          sprintf("Período %d–%d (cobertura ambulatorial estável); sensibilidade sem 2020–2021.", min(anos_tend), max(anos_tend)),
          "Fonte: SIASUS/DATASUS.") %>%
  add_fig("Fig06_urgencia_ajustada", "Proporção de atendimentos de urgência: bruta, sem o maior estabelecimento e ajustada por estabelecimento.") %>%
  add_tab(urgencia_ajustada %>% select(Ano, Pct_Bruta, Pct_Urg_Sem_Maior_Estab, Pct_Ajustada, Ajustada_IC_Inf, Ajustada_IC_Sup),
          "Urgência bruta e ajustada por estabelecimento, por ano.",
          sprintf("Ajuste: regressão logística com efeito fixo do estabelecimento e padronização marginal (%d–%d); IC 95%% por bootstrap de pacientes.",
                  min(anos_tend), max(anos_tend)), f_bi) %>%

  # 5 ---------------------------------------------------------------------------
  secao("5. Tratamento no Componente Especializado (CEAF)") %>%
  add_tab(fluxo_am, "Seleção das APAC de medicamentos.",
          sprintf("Todas as APAC entram (aprovação é evento de faturamento). Fim dos dados completos: %s. Meses com queda abrupta (não contam como interrupção): %s.",
                  mes_fim_txt, susp_txt), f_am) %>%
  add_fig("Fig07_volume_mensal_APAC", "APAC de medicamentos para asma por mês (todas as idades).") %>%
  add_tab(transpor(resumo_am), "Características das crianças e adolescentes em tratamento no CEAF.",
          "Paciente novo = primeira APAC após ≥ 12 meses sem APAC.", f_am) %>%
  add_fig("Fig08_medicamento_por_faixa", "Medicamento dispensado segundo a faixa etária.") %>%
  add_tab(testes_tratamento %>% select(Grupo, N, Qui2, gl, P_Valor, P_Holm_Global, V_Cramer, Magnitude) %>%
            rename(Variavel = Grupo),
          "Apresentação dispensada (budesonida 200 × 400) segundo o perfil do paciente.",
          "A quantidade registrada é quase sempre 60 cápsulas/mês: a dose diária estimada não acrescenta informação à apresentação e é apenas descrita no anexo.",
          f_am) %>%
  add_fig("Fig09_estado_nutricional", "Estado nutricional (escore-z de IMC-para-idade) dos pacientes no CEAF.") %>%

  # 6 ---------------------------------------------------------------------------
  secao("6. Desfechos do tratamento no CEAF") %>%
  texto("O SIASUS não registra alta nem reinternação. Episódio = paciente × medicamento; lacuna de ≥ 3 meses ",
        "observáveis abre novo episódio. ",
        sprintf("Entrada aos 0–%d anos e seguimento até %d anos (margem de %d ano). ", idade_ped, idade_ped + margem_idade, margem_idade),
        "Desfecho hierárquico: óbito > limite de idade > troca de medicamento > em uso no fim > saída registrada > interrupção.") %>%
  add_fig("Fig10_desfecho_tratamento", "Desfecho dos episódios de tratamento por medicamento.") %>%
  add_tab(desfecho_sensibilidade %>% filter(grepl("400|200", Apresentacao)) %>%
            mutate(Apresentacao = sub(" \\(cápsula\\)", "", Apresentacao)) %>%
            pivot_wider(names_from = Desfecho, values_from = Pct),
          "Desfechos principais segundo a base de APAC e a margem de idade (% dos episódios).", fonte = f_am) %>%
  add_fig("Fig11_permanencia_tratamento_KM", "Permanência no tratamento (Kaplan-Meier).") %>%
  add_tab(persistencia_km %>% filter(Em_Risco > 0 | Meses <= 12) %>%
            mutate(Apresentacao = sub(" \\(cápsula\\)", "", Apresentacao)),
          "Percentual de pacientes ainda em tratamento aos 3, 6, 12 e 24 meses (Kaplan-Meier).", fonte = f_am) %>%

  # 7 ---------------------------------------------------------------------------
  secao("7. Tratamento no CEAF e atendimentos (ligação pelo CNS)") %>%
  add_tab(vinculo_perfil, "Pacientes do ambulatório com e sem medicamento do CEAF.", fonte = "Fonte: SIASUS/DATASUS (BI × APAC pelo CNS).") %>%
  add_fig("Fig12_CEAF_x_urgencia_OR", "Razão de chances de atendimento de urgência segundo o tratamento no CEAF.") %>%
  add_tab(vinculo_modelo %>% select(-Desfecho), "Regressão logística: ≥ 1 urgência × tratamento no CEAF.",
          "Associação transversal; não indica efeito do tratamento.", "Fonte: SIASUS/DATASUS (BI × APAC pelo CNS).") %>%
  add_fig("Fig13_antes_depois_tratamento", "Urgências antes e depois do início do tratamento: casos e controles pareados.") %>%
  add_tab(rr_janelas, "Urgências por janela em relação ao início do tratamento (Poisson condicional ao paciente; referência −12 a −1).",
          fonte = "Fonte: SIASUS/DATASUS (BI × APAC pelo CNS).") %>%
  add_tab(did_modelo, "Diferença-em-diferenças: casos × controles pareados por sexo, faixa, mês e urgências no ano anterior.",
          sprintf("Sensibilidade a %d sorteios dos controles: RR mediano %s (intervalo interquartil %s–%s); RR > 1 em %s%% dos sorteios. Poucos eventos: estimativa instável.",
                  did_sementes$Sorteios, num_br(did_sementes$RR_Mediana, 2), num_br(did_sementes$RR_P25, 2),
                  num_br(did_sementes$RR_P75, 2), num_br(did_sementes$Pct_RR_Maior_1, 0)),
          "Fonte: SIASUS/DATASUS (BI × APAC pelo CNS). Análise exploratória.") %>%

  # 8 ---------------------------------------------------------------------------
  secao("8. Internações por asma (SIH)") %>%
  texto("AIH com CID-10 principal J45/J46 (condição sensível à atenção primária, Portaria SAS 221/2008), por ano de ",
        "internação, de residentes em Alagoas internados em Alagoas. Os arquivos públicos não têm CNS: a reinternação ",
        "é estimada por chave determinística (nascimento, sexo, CEP e município); a chave sem CEP dá o limite superior.") %>%
  add_tab(fluxo_sih, "Seleção das internações.", fonte = f_sih) %>%
  add_tab(transpor(resumo_intern), "Características das internações por asma de 0–19 anos.", fonte = f_sih) %>%
  add_tab(perfil_intern_faixa, "Permanência, uso de UTI e óbitos por faixa etária.", fonte = f_sih) %>%
  add_fig("Fig14_internacoes_rede", "Internações por 10 mil habitantes segundo a rede hospitalar considerada.") %>%
  add_tab(intern_rede, "Internações por ano: todos os hospitais, sem o hospital dominante e painel estável; UTI bruta e ajustada.",
          criterio_painel_hosp, f_sih) %>%
  add_tab(x_("apc_intern"), "Variação percentual anual das internações (Prais-Winsten).", fonte = f_sih) %>%
  add_fig("Fig15_internacoes_sazonalidade", "Sazonalidade das internações por asma.") %>%
  add_tab(reinternacoes, "Reinternação em 30 dias e em 12 meses (chave determinística).",
          "Internações até 1 dia após a alta anterior são o mesmo episódio (transferência).", f_sih) %>%

  # 9 ---------------------------------------------------------------------------
  secao("9. Municípios e regiões de saúde") %>%
  add_tab(x_("razao_taxas_local"), "Maceió × interior: razão de taxas por habitante de 0–19 anos (quase-Poisson).",
          fonte = "Fonte: SIASUS e SIH/DATASUS; população DATASUS.") %>%
  add_fig("Fig16_regioes_saude", "Taxas por região de saúde de residência.") %>%
  add_tab(x_("taxas_regiao") %>% select(Regiao_Nome, Municipios, Pop_Media, Taxa_Pac_BI_10mil_ano,
                                        Taxa_Pac_AM_10mil_ano, Taxa_Intern_10mil_ano),
          "Taxas por 10 mil habitantes de 0–19 anos por ano, segundo a região de saúde.",
          "Entre parênteses: município mais populoso da região.", "Fonte: SIASUS e SIH/DATASUS; regiões e população: brpop.") %>%
  add_fig("Fig17_CEAF_x_internacao_municipios", "Tratamento no CEAF × internação por asma, por município.") %>%
  add_tab(x_("eco_internacao"), "Análise ecológica: tratamento no CEAF e atendimento ambulatorial × internação.",
          "Quase-Poisson com a população como offset; associação entre municípios, não entre pacientes.",
          "Fonte: SIASUS e SIH/DATASUS.") %>%
  add_tab(eco, "Análise ecológica: atendimento fora do município × urgência.",
          sprintf("Municípios com ≥ %d atendimentos.", min_registros_mun), f_bi) %>%

  # 10 --------------------------------------------------------------------------
  secao("10. Todos os testes com correção de Holm") %>%
  add_tab(testes_globais, "Testes inferenciais do estudo com correção de Holm global.",
          "Correção sobre todos os testes do relatório, não só dentro de cada tabela.") %>%

  # 11 --------------------------------------------------------------------------
  secao("11. Reprodutibilidade") %>%
  add_tab(tibble(Parametro = c("Período", "Idade", "Margem de seguimento no CEAF", "Série estável",
                               "Washout (paciente novo)", "Lacuna de interrupção", "Fim dos dados do CEAF",
                               "Controles por caso", "Sorteios (DiD)", "Réplicas bootstrap", "Semente"),
                 Valor = c(periodo, sprintf("0–%d anos", idade_ped), sprintf("%d ano", margem_idade),
                           sprintf("%d–%d", min(anos_tend), max(anos_tend)), sprintf("%d meses", washout_meses),
                           sprintf("%d meses", gap_meses), mes_fim_txt, as.character(controles_por_caso),
                           as.character(n_sementes_did), as.character(n_bootstrap), as.character(semente_principal))),
          "Parâmetros da análise.") %>%
  add_tab(arquivos_falha, "Arquivos do DATASUS que não puderam ser lidos.") %>%
  add_tab(tibble(Pacote = c("R", sort(c("dplyr", "survey", "survival", "mice", "prais", "anthro", "anthroplus",
                                        "brpop", "read.dbc", "ggplot2", "officer", "flextable"))),
                 Versao = c(paste(R.version$major, R.version$minor, sep = "."),
                            sapply(sort(c("dplyr", "survey", "survival", "mice", "prais", "anthro", "anthroplus",
                                          "brpop", "read.dbc", "ggplot2", "officer", "flextable")),
                                   function(p) as.character(packageVersion(p))))),
          "Versões do R e dos principais pacotes (lista completa em renv.lock).")

print(doc, target = file.path(output_dir, "Asma_Ped_Resultados.docx"))
message("Concluído: ", output_dir)
