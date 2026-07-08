# ==============================================================================
# PERFIL EPIDEMIOLÓGICO: DOENÇA DE HUNTINGTON (SIA/SUS - ALAGOAS)
# ==============================================================================

# 1. Instalação e Carregamento de Pacotes
library(microdatasus)
library(tidyverse)
library(gtsummary) 
library(flextable)

# ==============================================================================
# 2. EXTRAÇÃO E FILTRAGEM DOS DADOS BRUTOS (2015 a 2025)
# ==============================================================================

anos_interesse <- 2015:2025

extrair_e_filtrar_alagoas_sia <- function(ano) {
  cat("Extraindo dados ambulatoriais (SIA-PA) de", ano, "...\n")
  
  tryCatch({
    dados_ano_brutos <- fetch_datasus(
      year_start = ano, year_end = ano,
      month_start = 1, month_end = 12,
      uf = "AL", information_system = "SIA-PA"
    )
    
    dados_ano_filtrados <- dados_ano_brutos %>%
      filter(grepl("G10", PA_CIDPRI) | grepl("G10", PA_CIDSEC) | grepl("G10", PA_CIDCAS))
    
    cat("  ->", nrow(dados_ano_filtrados), "registro(s) ambulatorial(is) em", ano, "\n")
    return(dados_ano_filtrados)
    
  }, error = function(e) {
    cat("  -> Erro em", ano, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

casos_brutos_alagoas_sia <- map_dfr(anos_interesse, extrair_e_filtrar_alagoas_sia)

# ==============================================================================
# 3. PROCESSAMENTO E REFINAMENTO DAS VARIÁVEIS CLINICAS E EPIDEMIOLÓGICAS
# ==============================================================================

if (nrow(casos_brutos_alagoas_sia) > 0) {
  
  # Aplica o dicionário do DATASUS
  perfil_huntington <- process_sia(casos_brutos_alagoas_sia)
  
  # Criando as variáveis refinadas
  perfil_huntington <- perfil_huntington %>%
    mutate(
      # Variáveis Temporais e Demográficas
      Ano_Atendimento = as.integer(substr(as.character(PA_CMP), 1, 4)),
      IDADE           = as.numeric(PA_IDADE),
      Faixa_Etaria    = case_when(
        is.na(IDADE)  ~ "Ignorado",
        IDADE < 40    ~ "Menos de 40 anos",
        IDADE < 60    ~ "40 a 59 anos",
        TRUE          ~ "60 anos ou mais"
      ),
      Faixa_Etaria = factor(Faixa_Etaria, levels = c("Menos de 40 anos", "40 a 59 anos", "60 anos ou mais", "Ignorado")),
      SEXO = droplevels(as.factor(PA_SEXO)),
      
      # 1. Refinamento: Fluxo Migratório (Vazios Assistenciais)
      Fluxo_Assistencia = ifelse(as.character(MUNIC_RES) == as.character(MUNIC_MOV), 
                                 "Atendido no próprio município", 
                                 "Viajou para outro município (TFD)"),
      
      # 2. Refinamento: Custos do SUS (Garantindo que seja numérico)
      Custo_Procedimento = as.numeric(PA_VALPRO),
      
      # 3. Refinamento: Identificação de Comorbidades Secundárias
      Tem_Comorbidade_Registrada = ifelse(is.na(PA_CIDSEC) | PA_CIDSEC == "" | PA_CIDSEC == "0000", 
                                          "Não", "Sim")
    )
  
  cat("\nProcessamento concluído! Total:", nrow(perfil_huntington), "procedimentos registrados.\n")
  
  # ==============================================================================
  # 4. GERAÇÃO DE TABELAS (GTSUMMARY) E RESUMOS CLÍNICOS
  # ==============================================================================
  
  # Construindo a Tabela Epidemiológica (Sociodemográfica e Fluxo)
  tabela_perfil <- perfil_huntington %>%
    select(Ano_Atendimento, SEXO, Faixa_Etaria, Fluxo_Assistencia, Tem_Comorbidade_Registrada) %>%
    tbl_summary(
      by = Ano_Atendimento, # Estratifica a tabela pelas colunas de Anos
      label = list(
        SEXO ~ "Sexo",
        Faixa_Etaria ~ "Faixa Etária",
        Fluxo_Assistencia ~ "Perfil de Deslocamento do Paciente",
        Tem_Comorbidade_Registrada ~ "Presença de CID Secundário"
      ),
      statistic = list(all_categorical() ~ "{n} ({p}%)"),
      missing_text = "Sem informação"
    ) %>%
    add_overall() %>%
    modify_header(label = "**Características Epidemiológicas**") %>%
    bold_labels()
  
  # Resumo Financeiro Anual (Custos Totais)
  resumo_custos <- perfil_huntington %>%
    group_by(Ano_Atendimento) %>%
    summarise(Custo_Total_Reais = sum(Custo_Procedimento, na.rm = TRUE))
  
  # Top 10 Procedimentos mais realizados nesses pacientes
  top_procedimentos <- perfil_huntington %>%
    count(PA_PROC_ID, name = "Total_Realizado", sort = TRUE) %>%
    slice_head(n = 10)
  
  # ==============================================================================
  # 5. BLOCO DE EXPORTAÇÃO AUTOMATIZADA COM TIMESTAMP
  # ==============================================================================
  
  # Defina o caminho onde quer salvar os arquivos
  caminho_pasta <- "C:/Users/Davi/Documents/TCC/Analises_Huntington_SIA"
  
  # Cria a pasta caso não exista
  if (!dir.exists(caminho_pasta)) {
    dir.create(caminho_pasta, recursive = TRUE)
  }
  
  # Gera o sufixo único (ex: 20260514_143000)
  sufixo <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Exportando o banco de dados completo refinado (CSV)
  write_csv2(perfil_huntington, 
             file.path(caminho_pasta, paste0("dados_completos_AL_", sufixo, ".csv")))
  
  # Exportando os resumos (Top Procedimentos e Custos)
  write_csv2(top_procedimentos, 
             file.path(caminho_pasta, paste0("top_procedimentos_", sufixo, ".csv")))
  write_csv2(resumo_custos, 
             file.path(caminho_pasta, paste0("resumo_custos_anuais_", sufixo, ".csv")))
  
  # Exportando a Tabela Epidemiológica para o Word
  tabela_perfil %>% 
    as_flex_table() %>% 
    save_as_docx(path = file.path(caminho_pasta, paste0("Tabela_Perfil_Clinico_", sufixo, ".docx")))
  
  cat("\n=======================================================\n")
  cat("SUCESSO! Todos os arquivos foram salvos na pasta:\n", caminho_pasta, "\n")
  cat("Sufixo utilizado nesta extração:", sufixo, "\n")
  cat("=======================================================\n")
  
} else {
  cat("\nZero registros ambulatoriais relacionados à Doença de Huntington em Alagoas no período selecionado.\n")
}