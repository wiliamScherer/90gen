# ==============================================================================
# 90gen/scripts/01_Anonimiza.R
# ==============================================================================
# Papel: Primeira etapa do pipeline. Le os dados primarios, faz limpeza inicial,
#        deduplica registros e anonimiza pacientes com nomes de plantas.
#
# Entradas:
#   - data/raw/dados_primarios.tsv
#   - data/raw/padronizacao_ortografia.tsv
#   - data/raw/wfo_plantlist_2025-06/taxon.tsv
#   - data/raw/wfo_plantlist_2025-06/name.tsv
#   - data/raw/wfo_plantlist_2025-06/synonym.tsv
#
# Saidas:
#   - data/clean/rds/Dados_anonimizados.rds
#   - docs/Plantas.rds
#   - data/clean/tsv/plantas.tsv
#
# Etapas:
#   1. Leitura e limpeza inicial
#   2. Renomeacao posicional
#   3. Normalizacao basica de texto
#   4. Deduplicacao
#   5. Geracao de lista de plantas
#   6. Mapeamento e anonimizacao
#
# Autor: Pipeline automatizado
# Data: 2026-01-19
# ==============================================================================

source(file.path("scripts", "00_Importa.R"))

# ==============================================================================
# CONFIGURACAO
# ==============================================================================

in_file <- file.path("data", "raw", "dados_primarios.tsv")
ortografia_file <- file.path("data", "raw", "padronizacao_ortografia.tsv")
raw_dir <- file.path("data", "raw", "wfo_plantlist_2025-06")

out_file <- file.path("data", "clean", "rds", "Dados_anonimizados.rds")
plant_tsv <- file.path("data", "clean", "tsv", "plantas.tsv")
plant_rds <- file.path("docs", "Plantas.rds")

n_plantas <- 7000

dir.create(file.path("data", "clean", "rds"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("data", "clean", "tsv"), recursive = TRUE, showWarnings = FALSE)
dir.create("docs", recursive = TRUE, showWarnings = FALSE)

# Verifica arquivos de entrada
if (!file.exists(in_file)) stop("Arquivo nao encontrado: ", in_file)
if (!file.exists(ortografia_file)) stop("Arquivo nao encontrado: ", ortografia_file)

# ==============================================================================
# SECRET (obrigatorio para anonimizacao deterministica)
# ==============================================================================

secret <- Sys.getenv("ANON_SECRET")
if (secret == "") stop("Defina ANON_SECRET no ambiente antes de rodar este script.")

# ==============================================================================
# FUNCOES AUXILIARES - TEXTO
# ==============================================================================

escape_regex <- function(x) {
    stringr::str_replace_all(x, "([\\\\.^$|()*+?\\[\\]{}])", "\\\\\\1")
}

normalizar_texto <- function(x) {
    x %>%
        stringr::str_to_lower() %>%
        stringr::str_trim() %>%
        stringi::stri_trans_general("Latin-ASCII")
}

normalize_text <- function(x) {
    x <- stringr::str_to_lower(x)
    x <- stringr::str_squish(x)
    x
}

normalize_separators <- function(x) {
    x <- stringr::str_replace_all(x, "\\s*;\\s*", "|||")
    x <- stringr::str_replace_all(x, "\\s*,\\s*", "|||")
    x <- stringr::str_replace_all(x, "\\s+e\\s+", "|||")
    x <- stringr::str_replace_all(x, "\\s*\\+\\s*", "|||")
    x <- stringr::str_replace_all(x, "\\s*/\\s*", "|||")
    x <- stringr::str_replace_all(x, "\\|\\|\\|", ", ")
    x <- stringr::str_replace_all(x, ",\\s*,+", ",")
    x <- stringr::str_replace_all(x, "^,\\s*|\\s*,$", "")
    x
}

normalize_multiresponse <- function(x) {
    x <- normalize_text(x)
    x <- normalize_separators(x)
    x
}

# ==============================================================================
# FUNCOES AUXILIARES - ORTOGRAFIA
# ==============================================================================

carregar_ortografia <- function(path) {
    dict <- readr::read_tsv(path, show_col_types = FALSE, na = character())
    required_cols <- c("termo_original", "termo_corrigido")
    missing_cols <- setdiff(required_cols, names(dict))
    if (length(missing_cols) > 0) {
        stop("Faltam colunas no dicionario: ", paste(missing_cols, collapse = ", "))
    }
    dict %>%
        dplyr::rename(de = termo_original, para = termo_corrigido) %>%
        dplyr::mutate(
            de = stringr::str_to_lower(stringr::str_squish(de)),
            para = stringr::str_to_lower(stringr::str_squish(para)),
            is_exclusao = (para == "na")
        ) %>%
        dplyr::filter(!is.na(de), de != "") %>%
        dplyr::distinct(de, .keep_all = TRUE)
}

aplicar_ortografia <- function(x, dict) {
    if (is.null(dict) || nrow(dict) == 0) return(stringr::str_to_lower(x))
    out <- stringr::str_to_lower(x)
    exclusoes <- dict %>% dplyr::filter(is_exclusao)
    substituicoes <- dict %>% dplyr::filter(!is_exclusao)
    out_trimmed <- stringr::str_squish(out)
    out <- ifelse(out_trimmed %in% exclusoes$de, NA_character_, out)
    for (i in seq_len(nrow(substituicoes))) {
        de <- substituicoes$de[i]
        para <- substituicoes$para[i]
        if (is.na(de) || de == "") next
        pattern <- paste0("(?<![[:alnum:]])", escape_regex(de), "(?![[:alnum:]])")
        out <- ifelse(is.na(out), NA_character_, stringr::str_replace_all(out, pattern, para))
    }
    out
}

# ==============================================================================
# FUNCOES AUXILIARES - DEDUPLICACAO
# ==============================================================================

merge_group <- function(g, conflicts) {
    out <- g[1, , drop = FALSE]
    for (col in names(g)) {
        vals <- g[[col]]
        if (is.character(vals)) {
            vals <- stringr::str_trim(vals)
            vals[vals == ""] <- NA_character_
        }
        uniq <- unique(vals[!is.na(vals)])
        if (length(uniq) == 0) {
            out[[col]] <- NA
        } else if (length(uniq) == 1) {
            out[[col]] <- uniq[1]
        } else {
            if (is.logical(g[[col]])) {
                out[[col]] <- isTRUE(any(uniq == TRUE))
            } else if (is.numeric(g[[col]]) || is.integer(g[[col]])) {
                out[[col]] <- max(as.numeric(uniq), na.rm = TRUE)
            } else if (is.character(g[[col]])) {
                out[[col]] <- uniq[which.max(nchar(uniq))]
            } else {
                out[[col]] <- uniq[1]
            }
            conflicts[[length(conflicts) + 1]] <- data.frame(
                paciente = g$id_key[1],
                coluna = col,
                valores = paste(uniq, collapse = " | "),
                stringsAsFactors = FALSE
            )
        }
    }
    list(row = out, conflicts = conflicts)
}

# ==============================================================================
# FUNCOES AUXILIARES - PLANTAS
# ==============================================================================

hash_id <- function(id, secret) {
    digest::digest(paste0(secret, "::", id), algo = "sha256", serialize = FALSE)
}

shuffle_with_secret <- function(x, secret) {
    seed_hex <- substr(digest::digest(secret, algo = "sha256", serialize = FALSE), 1, 8)
    seed_int <- strtoi(seed_hex, base = 16)
    set.seed(seed_int)
    sample(x, size = length(x), replace = FALSE)
}

gerar_lista_plantas <- function(raw_dir, n_out, out_tsv, out_rds) {
    taxon_file <- file.path(raw_dir, "taxon.tsv")
    name_file <- file.path(raw_dir, "name.tsv")
    syn_file <- file.path(raw_dir, "synonym.tsv")

    if (!file.exists(taxon_file)) stop("Arquivo nao encontrado: ", taxon_file)
    if (!file.exists(name_file)) stop("Arquivo nao encontrado: ", name_file)
    if (!file.exists(syn_file)) stop("Arquivo nao encontrado: ", syn_file)

    taxon <- readr::read_tsv(taxon_file, show_col_types = FALSE) %>%
        dplyr::mutate(dplyr::across(c(ID, nameID), as.character))
    name <- readr::read_tsv(name_file, show_col_types = FALSE) %>%
        dplyr::mutate(dplyr::across(ID, as.character))
    synonym <- readr::read_tsv(syn_file, show_col_types = FALSE) %>%
        dplyr::mutate(dplyr::across(c(taxonID, nameID), as.character))

    name_min <- name %>%
        dplyr::transmute(nameID = ID, rank, genus, specificEpithet)
    syn_taxon_ids <- synonym %>%
        dplyr::distinct(taxonID) %>%
        dplyr::pull(taxonID)

    pool <- taxon %>%
        dplyr::filter(!ID %in% syn_taxon_ids) %>%
        dplyr::left_join(name_min, by = "nameID") %>%
        dplyr::mutate(rank = stringr::str_to_upper(stringr::str_trim(rank))) %>%
        dplyr::filter(rank == "SPECIES") %>%
        dplyr::transmute(
            genero = stringr::str_trim(genus),
            especie = stringr::str_trim(specificEpithet)
        ) %>%
        dplyr::filter(!is.na(genero), genero != "", !is.na(especie), especie != "") %>%
        dplyr::distinct(genero, especie) %>%
        dplyr::arrange(genero, especie)

    if (nrow(pool) == 0) stop("Pool vazio apos filtros.")

    used_genero <- character()
    used_especie <- character()
    selected <- logical(nrow(pool))
    count <- 0

    for (i in seq_len(nrow(pool))) {
        g <- pool$genero[i]
        e <- pool$especie[i]
        if (!(g %in% used_genero) && !(e %in% used_especie)) {
            selected[i] <- TRUE
            used_genero <- c(used_genero, g)
            used_especie <- c(used_especie, e)
            count <- count + 1
        }
        if (count >= n_out) break
    }

    final <- pool[selected, , drop = FALSE]
    if (nrow(final) < n_out) {
        stop("Nao foi possivel selecionar ", n_out, " nomes unicos. Obtidos: ", nrow(final))
    }

    final <- final %>%
        dplyr::mutate(nome_planta = paste(genero, especie)) %>%
        dplyr::arrange(nome_planta) %>%
        dplyr::select(nome_planta)

    readr::write_tsv(final, out_tsv)
    saveRDS(final, out_rds)
    final$nome_planta
}

# ==============================================================================
# ETAPA 1: LEITURA E LIMPEZA INICIAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 1: Leitura e limpeza inicial\n")
cat(strrep("=", 60), "\n\n")

header <- readr::read_lines(in_file, n_max = 1)
n_tab <- stringr::str_count(header, "\t")
n_semicolon <- stringr::str_count(header, ";")
n_comma <- stringr::str_count(header, ",")
delim <- if (n_tab > n_semicolon && n_tab > n_comma) "\t" else if (n_semicolon > n_comma) ";" else ","

cat("Delimitador detectado:", ifelse(delim == "\t", "TAB", delim), "\n")

df <- readr::read_delim(
    file = in_file,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    name_repair = "unique",
    na = c("", "NA"),
    show_col_types = FALSE
)

cat("Linhas lidas:", nrow(df), "\n")
cat("Colunas lidas:", ncol(df), "\n")

# Remove colunas vazias
is_empty_col <- function(x) all(is.na(x) | stringr::str_trim(x) == "")
drop_idx <- vapply(df, is_empty_col, logical(1)) | (stringr::str_trim(names(df)) == "")
df <- df[, !drop_idx, drop = FALSE]
cat("Colunas apos remover vazias:", ncol(df), "\n")

# Limpeza de celulas
clean_cell <- function(x) {
    x <- stringr::str_replace_all(x, "[\r\n]+", " ")
    x <- stringr::str_replace_all(x, '^"|"$', "")
    stringr::str_trim(x)
}
df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), clean_cell))

# ==============================================================================
# ETAPA 2: RENOMEACAO POSICIONAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 2: Renomeacao posicional\n")
cat(strrep("=", 60), "\n\n")

target_names <- c(
    "iniciais", "prontuario", "sexo", "fototipo", "idade", "comorbidades",
    "cbc_cec_previo", "tipo_histo_1", "localizacao_1", "mohs", "n_lesoes_1",
    "margens_ap_1", "fechamento_1", "complicacoes_1", "reintervencao_1",
    "mais_cx", "obito", "tipo_histo_2", "localizacao_2", "n_lesoes_2",
    "margens_ap_2", "fechamento_2", "complicacoes_2", "reintervencao_2"
)

if (ncol(df) != length(target_names)) {
    stop("Esperava ", length(target_names), " colunas, encontrei ", ncol(df))
}
names(df) <- target_names
cat("Colunas renomeadas com sucesso\n")

# ==============================================================================
# ETAPA 3: NORMALIZACAO BASICA
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 3: Normalizacao basica de texto\n")
cat(strrep("=", 60), "\n\n")

ortografia_dict <- carregar_ortografia(ortografia_file)
cat("Dicionario de ortografia carregado:", nrow(ortografia_dict), "termos\n")

# Normaliza colunas de texto
multi_cols <- c("comorbidades", "tipo_histo_1", "tipo_histo_2", "localizacao_1", "localizacao_2",
                "margens_ap_1", "margens_ap_2", "fechamento_1", "fechamento_2",
                "complicacoes_1", "complicacoes_2")
multi_cols_exist <- intersect(multi_cols, names(df))
df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(multi_cols_exist), normalize_multiresponse))

simple_cols <- c("cbc_cec_previo", "mohs", "reintervencao_1", "reintervencao_2",
                 "mais_cx", "obito", "sexo", "iniciais")
simple_cols_exist <- intersect(simple_cols, names(df))
df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(simple_cols_exist), normalize_text))

# Converte numericos
df <- df %>%
    dplyr::mutate(
        idade = readr::parse_integer(idade),
        fototipo = readr::parse_integer(fototipo),
        n_lesoes_1 = readr::parse_integer(n_lesoes_1),
        n_lesoes_2 = readr::parse_integer(n_lesoes_2)
    )

# Aplica ortografia
text_cols <- names(df)[vapply(df, is.character, logical(1))]
text_cols <- setdiff(text_cols, c("prontuario", "iniciais"))
df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(text_cols), ~aplicar_ortografia(.x, ortografia_dict)))

cat("Normalizacao concluida\n")

# ==============================================================================
# ETAPA 4: DEDUPLICACAO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 4: Deduplicacao\n")
cat(strrep("=", 60), "\n\n")

df <- df %>%
    dplyr::mutate(id_key = normalizar_texto(paste0(prontuario, "::", iniciais)))

n_antes <- nrow(df)

conflicts <- list()
groups <- split(df, df$id_key)
merged_list <- lapply(groups, function(g) {
    res <- merge_group(g, conflicts)
    conflicts <<- res$conflicts
    res$row
})
df_merged <- dplyr::bind_rows(merged_list)

n_depois <- nrow(df_merged)
cat("Linhas antes:", n_antes, "\n")
cat("Linhas depois:", n_depois, "\n")
cat("Duplicados removidos:", n_antes - n_depois, "\n")

if (length(conflicts) > 0) {
    cat("\nConflitos resolvidos:", length(conflicts), "\n")
}

# ==============================================================================
# ETAPA 5: GERACAO DE LISTA DE PLANTAS
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 5: Gerando lista de plantas\n")
cat(strrep("=", 60), "\n\n")

plant_names <- gerar_lista_plantas(raw_dir, n_plantas, plant_tsv, plant_rds)
plant_names <- stringr::str_squish(plant_names)
plant_names <- plant_names[!is.na(plant_names) & plant_names != ""]
plant_names <- sort(unique(plant_names))
cat("Plantas geradas:", length(plant_names), "\n")

# ==============================================================================
# ETAPA 6: MAPEAMENTO E ANONIMIZACAO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("ETAPA 6: Mapeamento e anonimizacao\n")
cat(strrep("=", 60), "\n\n")

df_merged <- df_merged %>%
    dplyr::mutate(
        id_real = dplyr::if_else(
            is.na(prontuario) | is.na(iniciais) | prontuario == "" | iniciais == "",
            NA_character_,
            paste0(prontuario, "::", iniciais)
        )
    )

ids <- df_merged %>%
    dplyr::distinct(id_real) %>%
    dplyr::filter(!is.na(id_real)) %>%
    dplyr::pull(id_real)

cat("IDs unicos:", length(ids), "\n")

# Mapeamento deterministico (sem arquivo persistente)
ids_tbl <- tibble::tibble(id_real = ids) %>%
    dplyr::mutate(h = vapply(id_real, hash_id, character(1), secret = secret)) %>%
    dplyr::arrange(h)

shuffled_plants <- shuffle_with_secret(plant_names, secret)
if (length(shuffled_plants) < nrow(ids_tbl)) {
    stop("Plantas insuficientes. Preciso de ", nrow(ids_tbl), ", tenho ", length(shuffled_plants))
}

mapping <- ids_tbl %>%
    dplyr::mutate(plant_name = shuffled_plants[seq_len(dplyr::n())]) %>%
    dplyr::select(id_real, plant_name)

# Aplica anonimizacao
df_anon <- df_merged %>%
    dplyr::left_join(mapping, by = "id_real") %>%
    dplyr::mutate(paciente = plant_name) %>%
    dplyr::select(paciente, dplyr::everything(), -plant_name, -id_real, -id_key, -prontuario, -iniciais)

# ==============================================================================
# SALVAR RESULTADO
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("SALVANDO RESULTADO\n")
cat(strrep("=", 60), "\n\n")

saveRDS(df_anon, out_file)
cat("Arquivo salvo:", out_file, "\n")

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("01_ANONIMIZA CONCLUIDO!\n")
cat(strrep("=", 60), "\n\n")

cat("RESUMO:\n")
cat("  Linhas:", nrow(df_anon), "\n")
cat("  Colunas:", ncol(df_anon), "\n")
cat("  Pacientes unicos:", length(ids), "\n\n")

cat("ARQUIVOS GERADOS:\n")
cat("  Dados:", out_file, "\n")
cat("  Plantas:", plant_rds, "\n\n")
