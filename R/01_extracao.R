# ==============================================================================
# 1. EXTRAÇÃO — arquivo a arquivo, guardando só os registros de asma
# ==============================================================================

cols_pa <- c("PA_CMP", "PA_MVM", "PA_UFMUN", "PA_MUNPCN", "PA_PROC_ID",
             "PA_DOCORIG", "PA_AUTORIZ", "PA_CATEND", "PA_CIDPRI", "PA_CIDSEC",
             "PA_IDADE", "PA_FLIDADE", "PA_SEXO", "PA_RACACOR", "PA_ETNIA",
             "PA_QTDPRO", "PA_QTDAPR", "PA_VALAPR", "PA_INDICA", "PA_MNDIF",
             "PA_UFDIF", "PA_CBOCOD", "PA_TPUPS", "PA_CODUNI")
cols_bi <- c("DT_ATEND", "DT_PROCESS", "UFMUN", "MUNPAC", "PROC_ID", "CIDPRI",
             "CATEND", "CNS_PAC", "DTNASC", "TPIDADEPAC", "IDADEPAC", "SEXOPAC",
             "RACACOR", "QT_APROV", "MNDIF", "CBOPROF", "CODUNI")
cols_am <- c("AP_MVM", "AP_CMP", "AP_AUTORIZ", "AP_PRIPAL", "AP_VL_AP", "AP_UFMUN",
             "AP_CODUNI", "AP_CNSPCN", "AP_COIDADE", "AP_NUIDADE", "AP_SEXO",
             "AP_RACACOR", "AP_MUNPCN", "AP_MNDIF", "AP_DTINIC", "AP_DTFIM",
             "AP_TPAPAC", "AP_MOTSAI", "AP_OBITO", "AP_ENCERR", "AP_CATEND",
             "AP_APACANT", "AP_CIDPRI", "AP_CIDSEC", "AP_CIDCAS", "AM_PESO",
             "AM_ALTURA", "AM_GESTANT")

# Cache validado pelos parâmetros de extração [20]: se o filtro de CID ou as
# colunas mudarem, o cache antigo deixa de valer (evita resultado silencioso).
verificar_cache <- function(dir_cache, cols, cid_cols) {
  dir.create(dir_cache, showWarnings = FALSE)
  hash <- digest::digest(list(regex_asma, cols, cid_cols))
  f    <- file.path(dir_cache, "PARAMETROS_EXTRACAO.txt")
  if (file.exists(f) && readLines(f, warn = FALSE)[1] != hash)
    stop("Parâmetros de extração mudaram desde o cache em ", dir_cache,
         ". Apague a pasta para extrair de novo.")
  writeLines(hash, f)
}

utf8_seguro <- function(x) {
  x  <- as.character(x)
  ok <- is.na(x) | validUTF8(x)
  x[!ok] <- iconv(x[!ok], from = "latin1", to = "UTF-8")
  x
}

filtrar_asma <- function(df, cols, cid_cols) {
  dt <- as.data.table(df)
  dt <- dt[, intersect(cols, names(dt)), with = FALSE]
  dt[, (names(dt)) := lapply(.SD, function(x) trimws(utf8_seguro(x)))]
  cid_cols <- intersect(cid_cols, names(dt))
  dt[Reduce(`|`, lapply(cid_cols, function(cc) grepl(regex_asma, dt[[cc]])))]
}

periodos <- format(seq(as.Date(sprintf("%d-01-01", min(anos))), by = "month",
                       length.out = 12 * (max(anos) - min(anos) + 1) + meses_extra), "%y%m")

# Lista os arquivos (inclui partes a, b, c...). Sem a pasta montada, lista o
# diretório do FTP; se a listagem falhar, monta os nomes sem partes. [20]
listar_arquivos <- function(prefixo, url = url_ftp, pasta = dir_dbc) {
  padrao <- sprintf("^%sAL(%s)[a-z]?\\.dbc$", prefixo, paste(periodos, collapse = "|"))
  if (!is.null(pasta) && dir.exists(pasta))
    return(list.files(pasta, pattern = padrao, full.names = TRUE, ignore.case = TRUE))
  nomes <- tryCatch(strsplit(RCurl::getURL(url, dirlistonly = TRUE,
                                           ftp.use.epsv = FALSE), "\r?\n")[[1]],
                    error = function(e) NULL)
  if (!is.null(nomes)) return(paste0(url, grep(padrao, nomes, value = TRUE, ignore.case = TRUE)))
  paste0(url, prefixo, "AL", periodos, ".dbc")
}

# O FTP do DATASUS recusa muitas conexões de dados: até 6 tentativas
ler_dbc <- function(caminho, tentativas = 6) {
  if (file.exists(caminho)) return(read.dbc(caminho, as.is = TRUE))
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp))
  for (i in seq_len(tentativas)) {
    ok <- tryCatch({ download.file(caminho, tmp, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) e, warning = function(w) w)
    if (isTRUE(ok) && file.exists(tmp) && file.size(tmp) > 0) return(read.dbc(tmp, as.is = TRUE))
    if (inherits(ok, "condition") && grepl("550|unavailable", conditionMessage(ok))) break
    Sys.sleep(3)
  }
  stop("download falhou após ", tentativas, " tentativas")
}

arquivos_falha <- tibble(Arquivo = character(), Motivo = character())

extrair <- function(prefixo, dir_cache, cols, cid_cols, url = url_ftp, pasta = dir_dbc) {
  verificar_cache(dir_cache, cols, cid_cols)
  arquivos <- listar_arquivos(prefixo, url, pasta)
  message(prefixo, ": ", length(arquivos), " arquivos")
  for (f in arquivos) {
    cache <- file.path(dir_cache, sub("\\.dbc$", ".rds", basename(f), ignore.case = TRUE))
    if (file.exists(cache)) next
    message("  lendo ", basename(f))
    res <- tryCatch(filtrar_asma(ler_dbc(f), cols, cid_cols),
                    error = function(e) {
                      arquivos_falha <<- bind_rows(arquivos_falha,
                        tibble(Arquivo = basename(f), Motivo = conditionMessage(e)))
                      NULL })
    if (!is.null(res)) saveRDS(res, cache)
    gc()
  }
  rbindlist(lapply(list.files(dir_cache, "\\.rds$", full.names = TRUE), readRDS), fill = TRUE)
}

pa_raw <- extrair("PA", file.path(output_dir, "cache_asma_PA"), cols_pa, c("PA_CIDPRI", "PA_CIDSEC"))
bi_raw <- extrair("BI", file.path(output_dir, "cache_asma_BI"), cols_bi, "CIDPRI")
am_raw <- extrair("AM", file.path(output_dir, "cache_asma_AM"), cols_am, c("AP_CIDPRI", "AP_CIDSEC"))

# SIH/RD — internações (AIH reduzida). Guarda asma em qualquer posição do CID;
# a definição de caso (CID principal) é aplicada depois.
url_ftp_sih <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
dir_dbc_sih <- "/Volumes/ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados"
cols_sih <- c("ANO_CMPT", "MES_CMPT", "N_AIH", "IDENT", "CEP", "MUNIC_RES", "MUNIC_MOV",
              "NASC", "SEXO", "RACA_COR", "IDADE", "COD_IDADE", "DT_INTER", "DT_SAIDA",
              "DIAS_PERM", "UTI_MES_TO", "MARCA_UTI", "MORTE", "CAR_INT", "COBRANCA",
              "PROC_REA", "CNES", "DIAG_PRINC", "DIAG_SECUN", paste0("DIAGSEC", 1:9),
              "CID_MORTE", "VAL_TOT")
sih_raw <- extrair("RD", file.path(output_dir, "cache_asma_SIH"), cols_sih,
                   c("DIAG_PRINC", "DIAG_SECUN", paste0("DIAGSEC", 1:9)),
                   url = url_ftp_sih, pasta = dir_dbc_sih)

# --- Tabela SIGTAP -------------------------------------------------------------
carregar_sigtap <- function() {
  df <- tryCatch(as.data.frame(microdatasus::fetch_sigtab()), error = function(e) NULL)
  if (is.null(df) && !is.na(arquivo_sigtap) && file.exists(arquivo_sigtap))
    df <- fread(arquivo_sigtap, colClasses = "character")
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- as.data.frame(lapply(df, utf8_seguro), stringsAsFactors = FALSE)
  eh_cod  <- sapply(df, function(x) mean(grepl("^\\d{10}$", trimws(x)), na.rm = TRUE))
  if (max(eh_cod) < 0.5) return(NULL)
  col_cod <- names(df)[which.max(eh_cod)]
  outras  <- setdiff(names(df), col_cod)
  col_nom <- outras[which.max(sapply(df[outras], function(x) mean(nchar(x), na.rm = TRUE)))]
  nome <- gsub("[^ -~À-ÿ]", "", df[[col_nom]])
  tibble(Codigo = trimws(df[[col_cod]]), Nome = trimws(gsub("\\s+", " ", nome))) %>%
    distinct(Codigo, .keep_all = TRUE)
}
sigtap <- suppressMessages(carregar_sigtap())
nomear_proc <- function(df, col) {
  if (is.null(sigtap)) return(df %>% mutate(Nome = NA_character_))
  df %>% left_join(sigtap %>% rename(!!col := Codigo), by = col)
}

# --- População 0–19 anos por município, ano, sexo e faixa [8] ------------------
carregar_pop <- function() {
  if (!is.na(arquivo_pop) && file.exists(arquivo_pop))
    return(fread(arquivo_pop, colClasses = c(CO_MUN = "character")) %>% as_tibble())
  mapa_faixa <- c("From 0 to 4 years" = "0–4", "From 5 to 9 years" = "5–9",
                  "From 10 to 14 years" = "10–14", "From 15 to 19 years" = "15–19")
  ler <- function(f, sexo) f() %>%
    filter(substr(as.character(code_muni), 1, 2) == cod_uf_al,
           age_group %in% names(mapa_faixa), year %in% anos) %>%
    transmute(CO_MUN = substr(as.character(code_muni), 1, 6), Ano = as.integer(year),
              Sexo = sexo, Faixa = unname(mapa_faixa[age_group]), Pop = pop)
  pop <- tryCatch(suppressMessages(bind_rows(
    ler(brpop::datasus2024_mun_female_pop, "Feminino"),
    ler(brpop::datasus2024_mun_male_pop,   "Masculino"))), error = function(e) NULL)
  if (is.null(pop)) return(NULL)
  falta <- setdiff(anos, unique(pop$Ano))              # 2025: repete o último ano
  out <- bind_rows(pop, bind_rows(lapply(falta, function(a)
    pop %>% filter(Ano == max(pop$Ano)) %>% mutate(Ano = a))))
  attr(out, "ano_max") <- max(pop$Ano)
  out
}
pop_ped <- carregar_pop()
pop_ano_disponivel <- if (is.null(pop_ped)) NA else attr(pop_ped, "ano_max")
if (is.null(pop_ped)) message("População indisponível: taxas não serão calculadas.")

# --- Nomes de municípios (IBGE) e de estabelecimentos (CNES) -------------------
# Ficam em referencias/; se faltarem, são baixados das APIs públicas.
arq_mun  <- file.path(dir_referencia, "municipios_AL.csv")
arq_cnes <- file.path(dir_referencia, "cnes_nomes.csv")
if (!file.exists(arq_mun)) {
  d <- tryCatch(jsonlite::fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados/27/municipios"),
                error = function(e) NULL)
  if (!is.null(d)) write.csv(data.frame(CO_MUN7 = d$id, CO_MUN = substr(d$id, 1, 6), Municipio_Nome = d$nome),
                             arq_mun, row.names = FALSE, fileEncoding = "UTF-8")
}
nomes_mun <- if (file.exists(arq_mun)) read.csv(arq_mun, colClasses = "character", encoding = "UTF-8") %>%
  as_tibble() %>% select(Municipio = CO_MUN, Municipio_Nome) else tibble(Municipio = character(), Municipio_Nome = character())

titulo_br <- function(x) {
  x <- tools::toTitleCase(tolower(x))
  gsub("\\b(De|Da|Do|Das|Dos|E|Em)\\b", "\\L\\1", x, perl = TRUE)
}
nomes_cnes <- if (file.exists(arq_cnes)) read.csv(arq_cnes, colClasses = "character", encoding = "UTF-8") %>%
  as_tibble() %>% transmute(CNES = sprintf("%07d", as.integer(CNES)), Estabelecimento = titulo_br(Nome_Fantasia)) else
  tibble(CNES = character(), Estabelecimento = character())
nome_estab <- function(cod) {
  cod <- sprintf("%07d", suppressWarnings(as.integer(cod)))
  n <- nomes_cnes$Estabelecimento[match(cod, nomes_cnes$CNES)]
  if_else(is.na(n) | n == "", paste("CNES", cod), n)
}
# Completa nomes de CNES que faltarem (novos estabelecimentos) pela API do CNES
completar_cnes <- function(codigos) {
  falta <- setdiff(sprintf("%07d", as.integer(unique(codigos))), nomes_cnes$CNES)
  if (!length(falta)) return(invisible())
  novos <- bind_rows(lapply(falta, function(c) {
    d <- tryCatch(jsonlite::fromJSON(paste0("https://apidadosabertos.saude.gov.br/cnes/estabelecimentos/", as.integer(c))),
                  error = function(e) NULL)
    tibble(CNES = c, Nome_Fantasia = if (is.null(d$nome_fantasia)) "" else d$nome_fantasia)
  }))
  old <- if (file.exists(arq_cnes)) read.csv(arq_cnes, colClasses = "character", encoding = "UTF-8") else NULL
  write.csv(bind_rows(old, novos), arq_cnes, row.names = FALSE, fileEncoding = "UTF-8")
  nomes_cnes <<- bind_rows(nomes_cnes, novos %>% transmute(CNES, Estabelecimento = titulo_br(Nome_Fantasia)))
}
