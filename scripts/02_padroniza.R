# ==============================================================================
# 90gen/scripts/02_Padroniza.R (v2 - Refatorado)
# ==============================================================================
# Papel: Segunda etapa do pipeline. Recebe dados anonimizados e aplica
#        padronizacao de comorbidades, histologia, fechamento e tipagem.
#
# MUDANCAS NA V2:
#   - Processamento de histologia POR LESAO (nao mais global)
#   - Separacao inteligente: virgula separa lesoes OU qualificadores
#   - Expansao corrigida: repete ULTIMO valor (nao primeiro)
#   - Flag de revisao para casos ambiguos
#
# Entradas:
#   - data/clean/rds/Dados_anonimizados.rds
#   - data/raw/dados_primarios.tsv
#   - data/raw/padronizacao_comorbidades.tsv
#   - data/raw/padronizacao_histologia.tsv
#   - data/raw/padronizacao_fechamento.tsv
#
# Saidas:
#   - data/clean/rds/Dados_padronizados.rds
#
# Autor: Pipeline automatizado (refatorado)
# Data: 2025-01-21
# ==============================================================================

source(file.path("scripts", "00_Importa.R"))

# ==============================================================================
# CONFIGURACAO
# ==============================================================================

in_file <- file.path("data", "clean", "rds", "Dados_anonimizados.rds")
raw_file <- file.path("data", "raw", "dados_primarios.tsv")
dict_comorb_file <- file.path("data", "raw", "padronizacao_comorbidades.tsv")
dict_histo_file <- file.path("data", "raw", "padronizacao_histologia.tsv")
dict_fech_file <- file.path("data", "raw", "padronizacao_fechamento.tsv")

out_file <- file.path("data", "clean", "rds", "Dados_padronizados.rds")

dir.create(file.path("data", "clean", "rds"), recursive = TRUE, showWarnings = FALSE)

# Verifica arquivos de entrada
for (f in c(in_file, raw_file, dict_comorb_file, dict_histo_file, dict_fech_file)) {
    if (!file.exists(f)) stop("Arquivo nao encontrado: ", f)
}

# ==============================================================================
# CONSTANTES DE DOMINIO
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

# Subtipos de CBC (ordem de prioridade para exibicao)
SUBTIPO_ORDER <- c(
    "nodular", "superficial", "infiltrativo", "solido", "expansivo",
    "esclerodermiforme", "micronodular", "adenoide", "desmoplasico"
)

SUBTIPO_LABEL <- c(
    "nodular" = "nodular", "superficial" = "superficial",
    "infiltrativo" = "infiltrativo", "solido" = "solido",
    "expansivo" = "expansivo", "esclerodermiforme" = "esclerodermico",
    "micronodular" = "micronodular", "adenoide" = "adenoide",
    "desmoplasico" = "desmoplasico"
)

QUALIF_ORDER <- c("ulcerado", "pigmentado", "multifocal")

CATEGORIA_FECH_LABEL <- c(
    "fechamento_direto" = "direto", "segunda_intencao" = "2intencao",
    "enxerto" = "enxerto", "retalho" = "retalho"
)

SUBTIPO_FECH_LABEL <- c(
    "primario" = "primario", "segunda_intencao" = "2intencao",
    "curativo_figueiredo" = "curat_figueiredo",
    "enxerto_pele_total" = "enxerto_pele_total",
    "enxerto_pele_parcial" = "enxerto_pele_parcial",
    "retalho_avanco" = "retalho_avanco", "retalho_avanco_at" = "retalho_avanco_at",
    "retalho_rotacao" = "retalho_rotac", "retalho_rotacao_mercedes" = "retalho_rotac_mercedes",
    "retalho_interpolado_frontal" = "retalho_interp_frontal"
)

CATEGORIAS_FECH <- c("direto", "2intencao", "enxerto", "retalho")
SUBTIPOS_FECH <- c(
    "primario", "2intencao", "curat_figueiredo",
    "enxerto_pele_total", "enxerto_pele_parcial",
    "retalho_avanco", "retalho_avanco_at",
    "retalho_rotac", "retalho_rotac_mercedes", "retalho_interp_frontal"
)

# Padroes que indicam INICIO de nova lesao (nao qualificador)
PADROES_TIPO_TUMOR <- c(
    "^cbc", "^cec", "^carcinoma", "^queratoacantoma", "^melanoma",
    "^basoescamoso", "^basocelular", "^espinocelular", "^epiderm"
)

# ==============================================================================
# FUNCOES AUXILIARES BASICAS
# ==============================================================================

normalizar_texto <- function(x) {
    x %>%
        stringr::str_to_lower() %>%
        stringr::str_trim() %>%
        stringi::stri_trans_general("Latin-ASCII")
}

normalizar_separador <- function(x) {
    x <- stringr::str_replace_all(x, "\\s*;\\s*", ", ")
    x <- stringr::str_replace_all(x, ",\\s*,+", ", ")
    x <- stringr::str_replace_all(x, "^,\\s*|\\s*,?$", "")
    stringr::str_squish(x)
}

converter_logical <- function(x) {
    dplyr::case_when(
        x == "sim" ~ TRUE,
        x == "não" ~ FALSE,
        TRUE ~ NA
    )
}

to_logical <- function(x) {
    if (is.logical(x)) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
    x <- as.character(x)
    x <- stringr::str_to_lower(stringr::str_trim(x))
    x <- iconv(x, to = "ASCII//TRANSLIT")
    ifelse(x %in% c("true", "t", "1", "sim", "s"), TRUE,
           ifelse(x %in% c("false", "f", "0", "nao", "n"), FALSE, NA))
}

normalizar_complicacao <- function(x) {
    x_trim <- stringr::str_squish(x)
    dplyr::case_when(
        is.na(x_trim) ~ NA_character_,
        x_trim == "" ~ NA_character_,
        x_trim %in% c("não", "nao", "na") ~ NA_character_,
        TRUE ~ x_trim
    )
}

ordenar_comorbidades <- function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    termos <- stringr::str_split(x, "[,;]\\s*")[[1]]
    termos <- normalizar_texto(termos)
    termos <- termos[termos != ""]
    if (length(termos) == 0) return(NA_character_)
    termos <- unique(termos)
    ordenados <- COMORB_COLS_ORDER[COMORB_COLS_ORDER %in% termos]
    if (length(ordenados) == 0) return(NA_character_)
    paste(ordenados, collapse = ", ")
}

# ==============================================================================
# FUNCOES DE EXPANSAO (CORRIGIDAS - REPETE ULTIMO VALOR)
# ==============================================================================

#' Expande vetor de valores para ter exatamente n elementos
#' Repete o ULTIMO valor para completar (nao o primeiro)
#' 
#' @param valores Vetor de strings ou string com valores separados por virgula
#' @param n Numero desejado de elementos
#' @return String com n valores separados por virgula
expandir_para_n <- function(valores, n) {
    if (is.na(n) || n <= 0) return(NA_character_)
    if (is.na(valores) || valores == "") return(NA_character_)
    
    # Se ja e vetor, usa direto; senao, faz split
    if (length(valores) == 1) {
        partes <- stringr::str_split(valores, ",\\s*")[[1]]
        partes <- stringr::str_trim(partes)
        partes <- partes[partes != ""]
    } else {
        partes <- valores
    }
    
    if (length(partes) == 0) return(NA_character_)
    
    n_atual <- length(partes)
    
    if (n_atual == n) {
        return(paste(partes, collapse = ", "))
    }
    
    if (n_atual < n) {
        # Repete o ULTIMO valor para completar
        ultimo <- partes[n_atual]
        extras <- rep(ultimo, n - n_atual)
        partes <- c(partes, extras)
    }
    
    if (n_atual > n) {
        # Trunca (mantendo os primeiros n)
        partes <- partes[1:n]
    }
    
    paste(partes, collapse = ", ")
}

#' Versao para usar com mapply (aceita NA em n_lesoes)
expandir_valor <- function(valor, n_lesoes) {
    if (is.na(n_lesoes) || n_lesoes <= 1) return(valor)
    if (is.na(valor) || stringr::str_trim(valor) == "") return(valor)
    expandir_para_n(valor, n_lesoes)
}

# ==============================================================================
# FUNCOES DE SEPARACAO INTELIGENTE DE LESOES
# ==============================================================================

#' Verifica se um fragmento de texto indica INICIO de nova lesao
#' (em oposicao a ser apenas um qualificador da lesao anterior)
#' 
#' @param texto Fragmento de texto normalizado
#' @return TRUE se parece ser inicio de nova lesao
eh_nova_lesao <- function(texto) {
    texto <- stringr::str_trim(texto)
    if (texto == "") return(FALSE)
    
    # Verifica se comeca com padrao de tipo de tumor
    for (padrao in PADROES_TIPO_TUMOR) {
        if (stringr::str_detect(texto, padrao)) {
            return(TRUE)
        }
    }
    
    FALSE
}

#' Separa texto de histologia em lesoes individuais
#' 
#' Usa logica inteligente:
#' - Se fragmento apos virgula comeca com tipo de tumor -> nova lesao
#' - Se nao comeca com tipo de tumor -> qualificador da lesao anterior
#' 
#' @param texto Texto original de histologia (pode ter multiplas lesoes)
#' @return Vetor de strings, uma por lesao
#' 
#' @examples
#' separar_em_lesoes("cec bem diferenciado, cbc solido")
#' # c("cec bem diferenciado", "cbc solido")
#' 
#' separar_em_lesoes("cbc solido expansivo, multifocal, pigmentado")
#' # c("cbc solido expansivo, multifocal, pigmentado")
separar_em_lesoes <- function(texto) {
    if (is.na(texto) || texto == "") return(character(0))
    
    texto_norm <- normalizar_texto(texto)
    
    # Split por virgula
    fragmentos <- stringr::str_split(texto_norm, ",\\s*")[[1]]
    fragmentos <- stringr::str_trim(fragmentos)
    fragmentos <- fragmentos[fragmentos != ""]
    
    if (length(fragmentos) == 0) return(character(0))
    if (length(fragmentos) == 1) return(fragmentos)
    
    # Primeiro fragmento sempre e uma lesao
    lesoes <- character()
    lesao_atual <- fragmentos[1]
    
    for (i in 2:length(fragmentos)) {
        frag <- fragmentos[i]
        
        if (eh_nova_lesao(frag)) {
            # Fecha lesao atual e comeca nova
            lesoes <- c(lesoes, lesao_atual)
            lesao_atual <- frag
        } else {
            # Concatena como qualificador da lesao atual
            lesao_atual <- paste(lesao_atual, frag, sep = ", ")
        }
    }
    
    # Adiciona ultima lesao
    lesoes <- c(lesoes, lesao_atual)
    
    lesoes
}

# ==============================================================================
# FUNCOES DE PADRONIZACAO DE HISTOLOGIA (REFATORADAS)
# ==============================================================================

#' Padroniza UMA UNICA lesao
#' 
#' @param texto Texto de UMA lesao (ja separada)
#' @param lookup_tipo Lookup de tipos de tumor
#' @param lookup_subtipo Lookup de subtipos
#' @param lookup_diferenciacao Lookup de grau de diferenciacao
#' @param lookup_qualif Lookup de qualificadores
#' @return Lista com: tipo, tipo_det, subtipo, difer, qualif, comport
padronizar_uma_lesao <- function(texto, lookup_tipo, lookup_subtipo, 
                                 lookup_diferenciacao, lookup_qualif) {
    resultado_vazio <- list(
        tipo = NA_character_, 
        tipo_det = NA_character_,
        subtipo = NA_character_, 
        difer = NA_character_,
        qualif = NA_character_, 
        comport = NA_character_
    )
    
    if (is.na(texto) || texto == "") return(resultado_vazio)
    
    texto_norm <- normalizar_texto(texto)
    
    # --- TIPO DE TUMOR ---
    tipo <- NA_character_
    tipo_det <- NA_character_
    
    # Casos especiais primeiro
    baso_terms <- names(lookup_tipo)[lookup_tipo == "carcinoma_basoescamoso"]
    querato_terms <- names(lookup_tipo)[lookup_tipo == "queratoacantoma"]
    
    if (length(baso_terms) > 0 && any(stringr::str_detect(texto_norm, stringr::fixed(baso_terms)))) {
        tipo <- "cbc"
        tipo_det <- "carcinoma basoescamoso"
    } else if (length(querato_terms) > 0 && any(stringr::str_detect(texto_norm, stringr::fixed(querato_terms)))) {
        tipo <- "cec"
        tipo_det <- "queratoacantoma"
    } else {
        # Busca tipo normal
        for (termo in names(lookup_tipo)) {
            if (stringr::str_detect(texto_norm, stringr::fixed(termo))) {
                valor <- lookup_tipo[termo]
                if (valor == "cbc") tipo <- "cbc"
                else if (valor == "cec") tipo <- "cec"
                break
            }
        }
    }
    
    # --- SUBTIPOS ---
    sub_encontrados <- character()
    for (termo in names(lookup_subtipo)) {
        if (stringr::str_detect(texto_norm, stringr::fixed(termo))) {
            sub_encontrados <- c(sub_encontrados, lookup_subtipo[termo])
        }
    }
    sub_encontrados <- unique(sub_encontrados)
    sub_encontrados <- sub_encontrados[sub_encontrados %in% SUBTIPO_ORDER]
    sub_encontrados <- SUBTIPO_ORDER[SUBTIPO_ORDER %in% sub_encontrados]
    subtipo <- if (length(sub_encontrados) > 0) {
        paste(unname(SUBTIPO_LABEL[sub_encontrados]), collapse = ", ")
    } else {
        NA_character_
    }
    
    # --- DIFERENCIACAO ---
    difer <- NA_character_
    for (termo in names(lookup_diferenciacao)) {
        if (stringr::str_detect(texto_norm, stringr::fixed(termo))) {
            valor <- lookup_diferenciacao[termo]
            if (valor == "bem_diferenciado") difer <- "bem"
            else if (valor == "moderadamente_diferenciado") difer <- "moder"
            break
        }
    }
    
    # --- QUALIFICADORES ---
    qualif_encontradas <- character()
    for (termo in names(lookup_qualif)) {
        if (stringr::str_detect(texto_norm, stringr::fixed(termo))) {
            qualif_encontradas <- c(qualif_encontradas, lookup_qualif[termo])
        }
    }
    qualif_encontradas <- unique(qualif_encontradas)
    qualif_encontradas <- qualif_encontradas[qualif_encontradas %in% QUALIF_ORDER]
    qualif_encontradas <- QUALIF_ORDER[QUALIF_ORDER %in% qualif_encontradas]
    qualif <- if (length(qualif_encontradas) > 0) {
        paste(qualif_encontradas, collapse = ", ")
    } else {
        NA_character_
    }
    
    # --- COMPORTAMENTO ---
    comport <- NA_character_
    if (stringr::str_detect(texto_norm, "in situ")) {
        comport <- "in situ"
    } else if (!is.na(difer) || stringr::str_detect(texto_norm, "invasivo")) {
        comport <- "invasivo"
    }
    
    list(
        tipo = tipo, 
        tipo_det = tipo_det, 
        subtipo = subtipo, 
        difer = difer, 
        qualif = qualif, 
        comport = comport
    )
}

#' Calcula o "peso" de uma lesao (numero de tokens em campos multi-valorados)
#' Usado para ordenar lesoes: menor peso primeiro
#' 
#' @param resultado Lista com campos de uma lesao
#' @return Numero inteiro representando o peso
calcular_peso_lesao <- function(resultado) {
    peso <- 0
    
    # Conta tokens em subtipo
    if (!is.na(resultado$subtipo) && resultado$subtipo != "") {
        peso <- peso + length(stringr::str_split(resultado$subtipo, ",\\s*")[[1]])
    }
    
    # Conta tokens em qualificadores
    if (!is.na(resultado$qualif) && resultado$qualif != "") {
        peso <- peso + length(stringr::str_split(resultado$qualif, ",\\s*")[[1]])
    }
    
    peso
}

#' Padroniza histologia completa (multiplas lesoes)
#' 
#' IMPORTANTE: Ordena lesoes por "peso" (menos atributos primeiro) para que
#' na expansao os tokens excedentes fiquem na ultima lesao (que tem mais atributos)
#' 
#' @param texto Texto original de histologia
#' @param n_lesoes Numero de lesoes informado
#' @param lookup_tipo, lookup_subtipo, lookup_diferenciacao, lookup_qualif Lookups
#' @return Lista com vetores para cada campo
padronizar_histologia_completa <- function(texto, n_lesoes, 
                                           lookup_tipo, lookup_subtipo,
                                           lookup_diferenciacao, lookup_qualif) {
    resultado_vazio <- list(
        tipo = NA_character_,
        tipo_det = NA_character_,
        subtipo = NA_character_,
        difer = NA_character_,
        qualif = NA_character_,
        comport = NA_character_
    )
    
    if (is.na(texto) || texto == "") return(resultado_vazio)
    if (is.na(n_lesoes)) n_lesoes <- 1
    
    # Separa em lesoes individuais
    lesoes <- separar_em_lesoes(texto)
    n_detectado <- length(lesoes)
    
    # Processa cada lesao individualmente
    resultados <- lapply(lesoes, function(lesao) {
        padronizar_uma_lesao(
            lesao, lookup_tipo, lookup_subtipo, 
            lookup_diferenciacao, lookup_qualif
        )
    })
    
    # ORDENA lesoes por peso (menor primeiro)
    # Isso garante que lesoes com menos atributos venham primeiro,
    # e os tokens excedentes fiquem na ultima lesao na expansao
    if (length(resultados) > 1) {
        pesos <- sapply(resultados, calcular_peso_lesao)
        ordem <- order(pesos)
        resultados <- resultados[ordem]
    }
    
    # Extrai cada campo
    extrair_campo <- function(campo) {
        valores <- sapply(resultados, function(r) r[[campo]])
        if (all(is.na(valores))) return(NA_character_)
        paste(valores, collapse = ", ")
    }
    
    tipo <- extrair_campo("tipo")
    tipo_det <- extrair_campo("tipo_det")
    subtipo <- extrair_campo("subtipo")
    difer <- extrair_campo("difer")
    qualif <- extrair_campo("qualif")
    comport <- extrair_campo("comport")
    
    # Expande para n_lesoes se necessario (repete ultimo)
    if (n_detectado < n_lesoes && n_detectado > 0) {
        tipo <- expandir_para_n(tipo, n_lesoes)
        tipo_det <- expandir_para_n(tipo_det, n_lesoes)
        subtipo <- expandir_para_n(subtipo, n_lesoes)
        difer <- expandir_para_n(difer, n_lesoes)
        qualif <- expandir_para_n(qualif, n_lesoes)
        comport <- expandir_para_n(comport, n_lesoes)
    }
    
    list(
        tipo = tipo,
        tipo_det = tipo_det,
        subtipo = subtipo,
        difer = difer,
        qualif = qualif,
        comport = comport
    )
}

# ==============================================================================
# FUNCOES DE FECHAMENTO (MANTIDAS)
# ==============================================================================

padronizar_fechamento <- function(texto, lookup_cat, lookup_sub) {
    if (is.na(texto) || texto == "") {
        return(list(fech = NA_character_, fech_sub = NA_character_))
    }
    termos <- texto %>%
        stringr::str_split("[,;]\\s*") %>%
        unlist() %>%
        normalizar_texto() %>%
        unique()
    termos <- termos[termos != ""]
    
    cats <- character()
    subs <- character()
    for (termo in termos) {
        if (!is.na(lookup_cat[termo])) {
            cat_val <- unname(lookup_cat[termo])
            if (!is.na(cat_val) && cat_val %in% names(CATEGORIA_FECH_LABEL)) {
                cats <- c(cats, CATEGORIA_FECH_LABEL[cat_val])
            }
        }
        if (!is.na(lookup_sub[termo])) {
            sub_val <- unname(lookup_sub[termo])
            if (!is.na(sub_val) && sub_val %in% names(SUBTIPO_FECH_LABEL)) {
                subs <- c(subs, SUBTIPO_FECH_LABEL[sub_val])
            }
        }
    }
    cats <- unique(cats)
    cats <- CATEGORIAS_FECH[CATEGORIAS_FECH %in% cats]
    subs <- unique(subs)
    subs <- SUBTIPOS_FECH[SUBTIPOS_FECH %in% subs]
    
    list(
        fech = if (length(cats) > 0) paste(cats, collapse = ", ") else NA_character_,
        fech_sub = if (length(subs) > 0) paste(subs, collapse = ", ") else NA_character_
    )
}

# ==============================================================================
# FUNCAO DE LOCALIZACAO (CORRIGIDA - REPETE ULTIMO)
# ==============================================================================

ajustar_localizacao <- function(localizacao, n_lesoes) {
    if (is.na(localizacao) || localizacao == "" || is.na(n_lesoes)) {
        return(localizacao)
    }
    
    tokens <- stringr::str_split(localizacao, ",\\s*")[[1]]
    tokens <- stringr::str_trim(tokens)
    tokens <- tokens[tokens != ""]
    
    if (length(tokens) == 0) return(localizacao)
    
    n_tokens <- length(tokens)
    
    if (n_tokens == n_lesoes) {
        return(paste(tokens, collapse = ", "))
    }
    
    if (n_tokens < n_lesoes) {
        # Repete o ULTIMO para completar
        ultimo <- tokens[n_tokens]
        extras <- rep(ultimo, n_lesoes - n_tokens)
        tokens <- c(tokens, extras)
        return(paste(tokens, collapse = ", "))
    }
    
    # Se tem mais tokens que lesoes, mantem original
    localizacao
}

# ==============================================================================
# ETAPA 1: CARREGAR DADOS
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 1: Carregando dados anonimizados\n")
cat(strrep("=", 60), "\n\n")

df <- readRDS(in_file)
cat("Arquivo carregado:", in_file, "\n")
cat("Linhas:", nrow(df), "| Colunas:", ncol(df), "\n")
raw_hash <- digest::digest(file = raw_file, algo = "sha256")
cat("Fonte bruta:", raw_file, "\n")
cat("SHA256:", raw_hash, "\n")

n_rows <- nrow(df)

# ==============================================================================
# ETAPA 2: PADRONIZACAO DE COMORBIDADES
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 2: Padronizacao de comorbidades\n")
cat(strrep("=", 60), "\n\n")

dict_comorb_raw <- readr::read_tsv(dict_comorb_file, show_col_types = FALSE)
dict_comorb_exp <- dict_comorb_raw %>%
    dplyr::mutate(
        categoria = normalizar_texto(categoria),
        termo_padronizado = stringr::str_to_lower(stringr::str_trim(termo_padronizado))
    ) %>%
    tidyr::separate_rows(termo_original, sep = ",\\s*") %>%
    dplyr::mutate(termo_original = normalizar_texto(termo_original)) %>%
    dplyr::filter(termo_original != "", !is.na(termo_original)) %>%
    dplyr::filter(termo_padronizado %in% COMORB_COLS_ORDER)

lookup_termo_comorb <- stats::setNames(dict_comorb_exp$termo_padronizado, dict_comorb_exp$termo_original)
termos_mapeados_comorb <- unique(dict_comorb_exp$termo_original)

colunas_comorb <- stats::setNames(
    lapply(COMORB_COLS_ORDER, function(x) rep(FALSE, n_rows)),
    COMORB_COLS_ORDER
)

for (i in seq_len(n_rows)) {
    texto <- df$comorbidades[i]
    if (is.na(texto) || texto == "") next
    termos <- texto %>%
        stringr::str_split("[,;]\\s*") %>%
        unlist() %>%
        normalizar_texto() %>%
        unique()
    termos <- termos[termos != ""]
    for (termo_orig in termos) {
        if (termo_orig %in% termos_mapeados_comorb) {
            termo_pad <- unname(lookup_termo_comorb[termo_orig])
            if (!is.na(termo_pad) && termo_pad %in% COMORB_COLS_ORDER) {
                colunas_comorb[[termo_pad]][i] <- TRUE
            }
        }
    }
}

df_comorb <- as.data.frame(colunas_comorb, stringsAsFactors = FALSE, check.names = FALSE)
comorb_padronizada <- vapply(seq_len(n_rows), function(i) {
    presentes <- as.logical(df_comorb[i, COMORB_COLS_ORDER])
    termos <- COMORB_COLS_ORDER[presentes]
    if (length(termos) == 0) return(NA_character_)
    paste(termos, collapse = ", ")
}, character(1))

df$comorbidades <- comorb_padronizada
df[COMORB_COLS_ORDER] <- df_comorb
cat("Comorbidades padronizadas\n")

# ==============================================================================
# ETAPA 3: PADRONIZACAO DE HISTOLOGIA (REFATORADA)
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 3: Padronizacao de histologia (processamento por lesao)\n")
cat(strrep("=", 60), "\n\n")

dict_histo_raw <- readr::read_tsv(dict_histo_file, show_col_types = FALSE)
dict_histo_exp <- dict_histo_raw %>%
    dplyr::mutate(
        categoria = normalizar_texto(categoria),
        termo_padronizado = normalizar_texto(termo_padronizado)
    ) %>%
    tidyr::separate_rows(termo_original, sep = ",\\s*") %>%
    dplyr::mutate(termo_original = normalizar_texto(termo_original)) %>%
    dplyr::filter(termo_original != "", !is.na(termo_original))

lookup_tipo <- dict_histo_exp %>%
    dplyr::filter(categoria == "tipo_tumor") %>%
    { stats::setNames(.$termo_padronizado, .$termo_original) }
lookup_subtipo <- dict_histo_exp %>%
    dplyr::filter(categoria == "subtipo") %>%
    { stats::setNames(.$termo_padronizado, .$termo_original) }
lookup_diferenciacao <- dict_histo_exp %>%
    dplyr::filter(categoria == "grau_diferenciacao") %>%
    { stats::setNames(.$termo_padronizado, .$termo_original) }
lookup_qualif <- dict_histo_exp %>%
    dplyr::filter(categoria == "caracteristica") %>%
    { stats::setNames(.$termo_padronizado, .$termo_original) }

for (sufixo in c("1", "2")) {
    col_orig <- paste0("tipo_histo_", sufixo)
    col_n <- paste0("n_lesoes_", sufixo)
    
    histo_tipo <- rep(NA_character_, n_rows)
    histo_tipo_det <- rep(NA_character_, n_rows)
    histo_subtipo <- rep(NA_character_, n_rows)
    histo_difer <- rep(NA_character_, n_rows)
    histo_qualif <- rep(NA_character_, n_rows)
    comport <- rep(NA_character_, n_rows)
    
    for (i in seq_len(n_rows)) {
        n_lesoes <- if (col_n %in% names(df)) df[[col_n]][i] else 1
        
        res <- padronizar_histologia_completa(
            df[[col_orig]][i], 
            n_lesoes,
            lookup_tipo, lookup_subtipo, 
            lookup_diferenciacao, lookup_qualif
        )
        
        histo_tipo[i] <- res$tipo
        histo_tipo_det[i] <- res$tipo_det
        histo_subtipo[i] <- res$subtipo
        histo_difer[i] <- res$difer
        histo_qualif[i] <- res$qualif
        comport[i] <- res$comport
    }
    
    df[[paste0("histo_tipo_", sufixo)]] <- histo_tipo
    df[[paste0("histo_tipo_det_", sufixo)]] <- histo_tipo_det
    df[[paste0("histo_subtipo_", sufixo)]] <- histo_subtipo
    df[[paste0("histo_difer_", sufixo)]] <- histo_difer
    df[[paste0("histo_qualif_", sufixo)]] <- histo_qualif
    df[[paste0("comport_", sufixo)]] <- comport
}

cat("Histologia padronizada\n")

# ==============================================================================
# ETAPA 4: AJUSTE DE LOCALIZACAO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 4: Ajuste de localizacao\n")
cat(strrep("=", 60), "\n\n")

df$localizacao_1 <- mapply(ajustar_localizacao, df$localizacao_1, df$n_lesoes_1)
df$localizacao_2 <- mapply(ajustar_localizacao, df$localizacao_2, df$n_lesoes_2)
cat("Localizacoes ajustadas\n")

# ==============================================================================
# ETAPA 5: PADRONIZACAO DE FECHAMENTO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 5: Padronizacao de fechamento\n")
cat(strrep("=", 60), "\n\n")

dict_fech_raw <- readr::read_tsv(dict_fech_file, show_col_types = FALSE)
dict_fech_exp <- dict_fech_raw %>%
    dplyr::mutate(
        termo_original = normalizar_texto(termo_original),
        categoria = normalizar_texto(categoria),
        subtipo = normalizar_texto(subtipo)
    ) %>%
    tidyr::separate_rows(termo_original, sep = ",\\s*") %>%
    dplyr::filter(termo_original != "", !is.na(termo_original))

lookup_cat_fech <- stats::setNames(dict_fech_exp$categoria, dict_fech_exp$termo_original)
lookup_sub_fech <- stats::setNames(dict_fech_exp$subtipo, dict_fech_exp$termo_original)

for (sufixo in c("1", "2")) {
    col_orig <- paste0("fechamento_", sufixo)
    fech <- rep(NA_character_, n_rows)
    fech_sub <- rep(NA_character_, n_rows)
    
    for (i in seq_len(n_rows)) {
        res <- padronizar_fechamento(df[[col_orig]][i], lookup_cat_fech, lookup_sub_fech)
        fech[i] <- res$fech
        fech_sub[i] <- res$fech_sub
    }
    
    df[[paste0("fech_", sufixo)]] <- fech
    df[[paste0("fech_sub_", sufixo)]] <- fech_sub
}

cat("Fechamento padronizado\n")

# ==============================================================================
# ETAPA 6: TIPAGEM FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 6: Tipagem final\n")
cat(strrep("=", 60), "\n\n")

# Normaliza complicacoes
df <- df %>%
    dplyr::mutate(
        complicacao_1 = normalizar_complicacao(complicacoes_1),
        complicacao_2 = normalizar_complicacao(complicacoes_2)
    )

# Correcoes manuais (paciente/coluna) - MANTIDAS DO ORIGINAL
# Nota: revisar se ainda sao necessarias com a nova logica
correcoes <- tibble::tribble(
    ~paciente, ~coluna, ~valor,
    "Kelussia odoratissima", "fech_1", "enxerto, retalho",
    "Kelussia odoratissima", "fech_sub_1", "enxerto_pele_parcial, retalho_avanco_at"
    # Removidas correcoes de histo_subtipo que agora devem funcionar automaticamente
)

if (nrow(correcoes) > 0) {
    for (i in seq_len(nrow(correcoes))) {
        col_alvo <- correcoes$coluna[i]
        if (!col_alvo %in% names(df)) {
            cat("Aviso: coluna nao encontrada para correcao:", col_alvo, "\n")
            next
        }
        idx <- which(df$paciente == correcoes$paciente[i])
        if (length(idx) == 0) {
            cat("Aviso: paciente nao encontrado para correcao:", correcoes$paciente[i], "\n")
            next
        }
        df[idx, col_alvo] <- correcoes$valor[i]
    }
}

# Expande valores com 1 entrada para corresponder a n_lesoes
expand_cols <- c(
    "localizacao", "fech", "fech_sub", "histo_tipo", "histo_tipo_det",
    "histo_difer", "comport", "margens_ap", "histo_subtipo", "histo_qualif"
)
for (sufixo in c("1", "2")) {
    n_col <- paste0("n_lesoes_", sufixo)
    for (base in expand_cols) {
        col <- paste0(base, "_", sufixo)
        if (col %in% names(df) && n_col %in% names(df)) {
            df[[col]] <- mapply(expandir_valor, df[[col]], df[[n_col]])
        }
    }
}

# Padroniza separadores ("," como padrao)
sep_cols <- c(
    "comorbidades", "localizacao_1", "localizacao_2",
    "fech_1", "fech_2", "fech_sub_1", "fech_sub_2",
    "histo_subtipo_1", "histo_subtipo_2", "histo_qualif_1", "histo_qualif_2",
    "complicacao_1", "complicacao_2"
)
sep_cols <- intersect(sep_cols, names(df))
df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(sep_cols), normalizar_separador))

# Seleciona e reordena colunas finais
ordem_final <- c(
    "paciente", "idade", "sexo", "fototipo", "comorbidades",
    COMORB_COLS_ORDER,
    "cbc_cec_previo", "mohs", "mais_cx", "obito",
    "n_lesoes_1", "localizacao_1", "fech_1", "fech_sub_1",
    "complicacao_1", "reintervencao_1",
    "histo_tipo_1", "histo_tipo_det_1", "histo_subtipo_1", "histo_difer_1", 
    "histo_qualif_1", "comport_1", "margens_ap_1",
    "n_lesoes_2", "localizacao_2", "fech_2", "fech_sub_2",
    "complicacao_2", "reintervencao_2",
    "histo_tipo_2", "histo_tipo_det_2", "histo_subtipo_2", "histo_difer_2", 
    "histo_qualif_2", "comport_2", "margens_ap_2"
)

# Adiciona colunas faltantes como NA
for (col in ordem_final) {
    if (!col %in% names(df)) {
        df[[col]] <- NA
    }
}

df <- df %>% dplyr::select(dplyr::all_of(ordem_final))

# Aplica tipagem
df$paciente <- as.character(df$paciente)
df$comorbidades <- as.character(df$comorbidades)
df$idade <- as.integer(df$idade)
df$sexo <- factor(df$sexo, levels = c("f", "m"))
df$fototipo <- factor(df$fototipo, levels = 1:6, ordered = TRUE)
df$n_lesoes_1 <- as.integer(df$n_lesoes_1)
df$n_lesoes_2 <- as.integer(df$n_lesoes_2)
df$histo_tipo_1 <- as.character(df$histo_tipo_1)
df$histo_tipo_2 <- as.character(df$histo_tipo_2)
df$histo_difer_1 <- as.character(df$histo_difer_1)
df$histo_difer_2 <- as.character(df$histo_difer_2)
df$comport_1 <- as.character(df$comport_1)
df$comport_2 <- as.character(df$comport_2)

margens_levels <- sort(unique(na.omit(c(df$margens_ap_1, df$margens_ap_2))))
df$margens_ap_1 <- factor(df$margens_ap_1, levels = margens_levels)
df$margens_ap_2 <- factor(df$margens_ap_2, levels = margens_levels)

# Campos booleanos
bool_cols <- c("cbc_cec_previo", "mohs", "mais_cx", "obito", "reintervencao_1", "reintervencao_2")
df[bool_cols] <- lapply(df[bool_cols], to_logical)

# Para indicadores principais, NA vira FALSE
bool_fix <- c("cbc_cec_previo", "mohs", "mais_cx", "obito", "reintervencao_1", "reintervencao_2")
for (col in bool_fix) {
    if (col %in% names(df)) {
        df[[col]][is.na(df[[col]])] <- FALSE
    }
}

cat("Tipagem concluida\n")

# ==============================================================================
# SALVAR RESULTADO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("SALVANDO RESULTADO\n")
cat(strrep("=", 60), "\n\n")

saveRDS(df, out_file)
cat("Arquivo salvo:", out_file, "\n")

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("02_PADRONIZA CONCLUIDO! (v2 - Refatorado)\n")
cat(strrep("=", 60), "\n\n")

cat("RESUMO:\n")
cat("  Linhas:", nrow(df), "\n")
cat("  Colunas:", ncol(df), "\n")
cbc_count <- sum(stringr::str_detect(df$histo_tipo_1, "(^|, )cbc(,|$)"), na.rm = TRUE)
cec_count <- sum(stringr::str_detect(df$histo_tipo_1, "(^|, )cec(,|$)"), na.rm = TRUE)
cat("  CBC:", cbc_count, "\n")
cat("  CEC:", cec_count, "\n")
cat("  Idade media:", round(mean(df$idade, na.rm = TRUE), 1), "anos\n")
cat("  Sexo F:", sum(df$sexo == "f", na.rm = TRUE), "| M:", sum(df$sexo == "m", na.rm = TRUE), "\n")

cat("\nARQUIVO GERADO:\n")
cat("  ", out_file, "\n\n")
