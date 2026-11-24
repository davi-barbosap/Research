# ==============================================================================
# ASMA EM CRIANÇAS E ADOLESCENTES (0–19 ANOS) NO SUS — ALAGOAS, 2015–2025
# Script principal: roda as etapas em ordem. Cada etapa está em R/.
#
#   R/00_config.R        parâmetros, pacotes, funções e padrão visual
#   R/01_extracao.R      leitura do DATASUS (PA, BI, AM, SIH), SIGTAP, população,
#                        nomes de municípios (IBGE) e estabelecimentos (CNES)
#   R/02_atendimentos.R  seleção, perfil, pacientes, tendência e urgência ajustada
#   R/03_ceaf.R          tratamento no CEAF, episódios, desfechos, vínculo e
#                        antes × depois com controles pareados
#   R/04_internacoes.R   internações (SIH), rede hospitalar, reinternação
#   R/05_municipios.R    municípios, regiões de saúde, ecológico, Holm global
#   R/06_graficos.R      figuras
#   R/07_relatorio.R     figuras em arquivo, anexo Excel e relatório DOCX
#
# Saídas: figuras/ (PNG e TIFF 300 dpi), Asma_Ped_Resultados.docx,
#         Asma_Ped_Anexo_Tabelas.xlsx, Asma_Ped_resultados.rds.
# Para reproduzir os pacotes: renv::restore() (versões em renv.lock).
# ==============================================================================
setwd("/Users/davibarbosa/Desktop/Henrique")
etapas <- c("00_config", "01_extracao", "02_atendimentos", "03_ceaf", "04_internacoes",
            "05_municipios", "06_graficos", "07_relatorio")
for (e in etapas) {
  message("== ", e)
  source(file.path("R", paste0(e, ".R")), encoding = "UTF-8", local = FALSE)
}
