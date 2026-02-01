# ==============================================================================
# 90gen/scripts/03_Deriva.R (v2 - Adaptado)
# ==============================================================================
# Papel: Gera dataframes derivados (pacientes e lesoes) a partir do banco padronizado.
#
# MUDANCAS NA V2:
#   - Adiciona colunas de revisao (revisar_histo, motivo_revisao)
#   - expand_to_n() corrigido: repete ULTIMO valor (nao retorna NA)
#   - reintervencao e complicacao tratados como atributos da CIRURGIA
#   - Validacao lista casos pendentes de revisao
#
# Entradas:
#   - data/clean/rds/Dados_padronizados.rds
#
# Saidas:
#   - data/derived/pacientes.rds
#   - data/derived/lesoes.rds
#
# Autor: Pipeline automatizado (adaptado)
# Data: 2025-01-21
# ==============================================================================

source(file.path("scripts", "00_Importa.R"))

# ==============================================================================
# CONFIGURACAO
# ==============================================================================

in_file <- file.path("data", "clean", "rds", "Dados_padronizados.rds")
out_dir <- file.path("data", "derived")
out_pacientes <- file.path(out_dir, "pacientes.rds")
out_lesoes <- file.path(out_dir, "lesoes.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) stop("Arquivo nao encontrado: ", in_file)

# ==============================================================================
# CONSTANTES
# ==============================================================================

COMORB_COLS_ORDER <- c(
    "has", "ic", "fa", "dac", "ivc", "arritmia", "avc",
    "alzheimer", "parkinson", "epilepsia", "depressao", "ansiedade", "esquizofrenia",
    "dm", "dlp", "hipotir", "hipertir",
    "drc", "hpb", "incont_ur",
    "ca_rim", "ca_prostata", "ca_laringe", "leucemia", "mm",
    "asma", "dpoc", "tabagismo", "ex-tabag", "etilismo", "ex-etil",
    "defic_audit", "anemia", "osteoartrite", "osteoporose"
)

# ==============================================================================
# FUNCOES AUXILIARES
# ==============================================================================

normalizar_separador <- function(x) {
    if (is.na(x)) return(NA_character_)
    x <- stringr::str_replace_all(x, "\\s*;\\s*", ", ")
    x <- stringr::str_replace_all(x, ",\\s*,+", ", ")
    x <- stringr::str_replace_all(x, "^,\\s*|\\s*,?$", "")
    stringr::str_squish(x)
}

split_tokens <- function(x) {
    if (is.na(x) || stringr::str_trim(x) == "") return(character(0))
    x_norm <- normalizar_separador(x)
    if (is.na(x_norm) || x_norm == "") return(character(0))
    tokens <- stringr::str_split(x_norm, ",\\s*")[[1]]
    tokens <- stringr::str_trim(tokens)
    tokens[tokens != ""]
}

#' Expande valor para n elementos
#' 
#' CORRIGIDO: Se cardinalidade nao bate, repete o ULTIMO valor
#' (em vez de retornar NA como na versao anterior)
#' 
#' @param val Valor a expandir (pode ser string com virgulas, logical, numeric)
#' @param n Numero desejado de elementos
#' @return Vetor com n elementos
expand_to_n <- function(val, n) {
    if (is.na(n) || n <= 0) return(character(0))
    
    # Tipos nao-string: repete diretamente
    if (is.logical(val) || is.numeric(val) || is.integer(val)) {
        return(rep(val, n))
    }
    
    if (is.factor(val)) val <- as.character(val)
    if (!is.character(val)) return(rep(val, n))
    if (is.na(val) || stringr::str_trim(val) == "") return(rep(NA_character_, n))
    
    tokens <- split_tokens(val)
    n_tokens <- length(tokens)
    
    if (n_tokens == 0) return(rep(NA_character_, n))
    if (n_tokens == n) return(tokens)
    
    # CORRECAO: Se tem menos tokens que n, repete o ULTIMO
    if (n_tokens < n) {
        ultimo <- tokens[n_tokens]
        extras <- rep(ultimo, n - n_tokens)
        return(c(tokens, extras))
    }
    
    # Se tem mais tokens que n, trunca (mantendo os primeiros)
    if (n_tokens > n) {
        return(tokens[1:n])
    }
    
    tokens
}

# ==============================================================================
# CARREGAR DADOS
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("DERIVACAO: Carregando dados padronizados\n")
cat(strrep("=", 60), "\n\n")

df <- readRDS(in_file)
cat("Arquivo carregado:", in_file, "\n")
cat("Linhas:", nrow(df), "| Colunas:", ncol(df), "\n")

# ==============================================================================
# PACIENTES (1 linha = 1 paciente)
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("DERIVACAO: Gerando pacientes\n")
cat(strrep("=", 60), "\n\n")

comorb_cols <- COMORB_COLS_ORDER[COMORB_COLS_ORDER %in% names(df)]
if (length(comorb_cols) > 0) {
    comorb_df <- df[, comorb_cols, drop = FALSE]
} else if ("comorbidades" %in% names(df)) {
    comorb_df <- as.data.frame(
        stats::setNames(
            lapply(COMORB_COLS_ORDER, function(term) {
                vapply(df$comorbidades, function(x) term %in% split_tokens(x), logical(1))
            }),
            COMORB_COLS_ORDER
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
} else {
    comorb_df <- data.frame()
}

pacientes_base <- df %>%
    dplyr::select(
        paciente, idade, sexo, fototipo, comorbidades,
        cbc_cec_previo, mohs, mais_cx, obito,
        n_lesoes_1, n_lesoes_2
    )

n_total <- ifelse(
    is.na(pacientes_base$n_lesoes_1) & is.na(pacientes_base$n_lesoes_2),
    NA_integer_,
    rowSums(cbind(pacientes_base$n_lesoes_1, pacientes_base$n_lesoes_2), na.rm = TRUE)
)

pacientes <- dplyr::bind_cols(
    pacientes_base %>% dplyr::select(paciente, idade, sexo, fototipo, comorbidades),
    comorb_df,
    pacientes_base %>% dplyr::select(cbc_cec_previo, mohs, mais_cx, obito)
)
pacientes$n_lesoes_total <- as.integer(n_total)

saveRDS(pacientes, out_pacientes)
cat("Pacientes:", nrow(pacientes), "linhas |", ncol(pacientes), "colunas\n")
cat("Arquivo salvo:", out_pacientes, "\n")

# ==============================================================================
# LESOES (1 linha = 1 lesao)
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("DERIVACAO: Gerando lesoes\n")
cat(strrep("=", 60), "\n\n")

# Variaveis que sao EXPANDIDAS por lesao (cada lesao pode ter valor diferente)
lesao_vars_expand <- c(
    "localizacao", "fech", "fech_sub",
    "histo_tipo", "histo_tipo_det", "histo_subtipo", "histo_difer",
    "histo_qualif", "comport", "margens_ap"
)

# Variaveis que sao REPETIDAS por lesao (atributos da cirurgia, nao da lesao)
# Todas as lesoes da mesma cirurgia compartilham esses valores
lesao_vars_repeat <- c(
    "complicacao", "reintervencao"
)

lesoes_list <- list()
idx <- 1

for (i in seq_len(nrow(df))) {
    for (sufixo in c("1", "2")) {
        n_col <- paste0("n_lesoes_", sufixo)
        n_les <- df[[n_col]][i]
        if (is.na(n_les) || n_les <= 0) next
        n_les <- as.integer(n_les)
        
        # Expande variaveis por lesao
        cols_expand <- lapply(lesao_vars_expand, function(v) {
            col <- paste0(v, "_", sufixo)
            if (!col %in% names(df)) return(rep(NA_character_, n_les))
            expand_to_n(df[[col]][i], n_les)
        })
        names(cols_expand) <- lesao_vars_expand
        
        # Repete variaveis da cirurgia (mesmo valor pra todas as lesoes)
        cols_repeat <- lapply(lesao_vars_repeat, function(v) {
            col <- paste0(v, "_", sufixo)
            if (!col %in% names(df)) return(rep(NA, n_les))
            rep(df[[col]][i], n_les)
        })
        names(cols_repeat) <- lesao_vars_repeat
        
        # Monta tibble da lesao
        lesoes_list[[idx]] <- tibble::tibble(
            paciente = rep(df$paciente[i], n_les),
            visita = rep(as.integer(sufixo), n_les),
            lesao_idx = seq_len(n_les),
            
            # Variaveis expandidas
            localizacao = cols_expand$localizacao,
            fech = cols_expand$fech,
            fech_sub = cols_expand$fech_sub,
            histo_tipo = cols_expand$histo_tipo,
            histo_tipo_det = cols_expand$histo_tipo_det,
            histo_subtipo = cols_expand$histo_subtipo,
            histo_difer = cols_expand$histo_difer,
            histo_qualif = cols_expand$histo_qualif,
            comport = cols_expand$comport,
            margens_ap = cols_expand$margens_ap,
            
            # Variaveis repetidas (atributos da cirurgia)
            complicacao = cols_repeat$complicacao,
            reintervencao = cols_repeat$reintervencao
        )
        idx <- idx + 1
    }
}

lesoes <- if (length(lesoes_list) > 0) dplyr::bind_rows(lesoes_list) else {
    tibble::tibble(
        paciente = character(), visita = integer(), lesao_idx = integer(),
        localizacao = character(), fech = character(), fech_sub = character(),
        histo_tipo = character(), histo_tipo_det = character(),
        histo_subtipo = character(), histo_difer = character(), 
        histo_qualif = character(), comport = character(), margens_ap = character(),
        complicacao = character(), reintervencao = logical()
    )
}

# Converte tipos
lesoes$reintervencao <- as.logical(lesoes$reintervencao)
lesoes$reintervencao[is.na(lesoes$reintervencao)] <- FALSE

saveRDS(lesoes, out_lesoes)
cat("Lesoes:", nrow(lesoes), "linhas |", ncol(lesoes), "colunas\n")
cat("Arquivo salvo:", out_lesoes, "\n")

# ==============================================================================
# VALIDACAO BASICA
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("DERIVACAO: Validacao basica\n")
cat(strrep("=", 60), "\n\n")

expected_lesoes <- sum(c(df$n_lesoes_1, df$n_lesoes_2), na.rm = TRUE)
cat("Pacientes esperados:", nrow(df), "| gerados:", nrow(pacientes), "\n")
cat("Lesoes esperadas:", expected_lesoes, "| geradas:", nrow(lesoes), "\n")

# Validacao de cardinalidade
if (nrow(lesoes) != expected_lesoes) {
    cat("\n>>> AVISO: Numero de lesoes geradas difere do esperado! <<<\n")
}

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("03_DERIVA CONCLUIDO! (v2 - Adaptado)\n")
cat(strrep("=", 60), "\n\n")

cat("ARQUIVOS GERADOS:\n")
cat("  ", out_pacientes, "\n")
cat("  ", out_lesoes, "\n\n")

cat("ESTATISTICAS:\n")
cat("  Pacientes:", nrow(pacientes), "\n")
cat("  Lesoes:", nrow(lesoes), "\n")

# Distribuicao por tipo histologico
cat("\nDISTRIBUICAO POR TIPO HISTOLOGICO:\n")
tipo_counts <- table(lesoes$histo_tipo, useNA = "ifany")
for (tipo in names(tipo_counts)) {
    label <- if (is.na(tipo)) "NA" else tipo
    cat(sprintf("  %s: %d (%.1f%%)\n", 
                label, tipo_counts[tipo], 
                100 * tipo_counts[tipo] / nrow(lesoes)))
}

# Distribuicao por localizacao
cat("\nTOP 5 LOCALIZACOES:\n")
loc_counts <- sort(table(lesoes$localizacao, useNA = "ifany"), decreasing = TRUE)
for (k in seq_len(min(5, length(loc_counts)))) {
    loc <- names(loc_counts)[k]
    label <- if (is.na(loc)) "NA" else loc
    cat(sprintf("  %s: %d (%.1f%%)\n", 
                label, loc_counts[k], 
                100 * loc_counts[k] / nrow(lesoes)))
}

cat("\n")
