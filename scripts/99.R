# ============================================================
# 99_utils.R
# Funções utilitárias para reset do ambiente e execução do pipeline
#
# Uso no console:
#   source("scripts/99_utils.R")
#   resenv()
#   runpip()
# ============================================================

source(file.path("scripts", "00_libs.R"))

# ------------------------------------------------------------
# resenv(): limpa ambiente + remove artefatos intermediários
# ------------------------------------------------------------
resenv <- function(
        remove_files = TRUE,
        files = c(
            file.path("data", "clean", "rds", "90arios_normalizado.rds"),
            file.path("data", "clean", "rds", "90arios_padronizado.rds"),
            file.path("data", "clean", "tsv", "90arios_normalizado.tsv"),
            file.path("data", "clean", "tsv", "90arios_padronizado.tsv")
        ),
        verbose = TRUE
) {
    # 1) Limpa o ambiente, mas preserva as utilitárias
    keep <- c("resenv", "runpip")
    
    objs <- ls(envir = .GlobalEnv, all.names = TRUE)
    drop <- setdiff(objs, keep)
    
    if (length(drop) > 0) {
        rm(list = drop, envir = .GlobalEnv)
    }
    gc()
    
    if (verbose) {
        cat("\n", strrep("=", 60), "\n", sep = "")
        cat("✓ Ambiente limpo (preservando: ", paste(keep, collapse = ", "), ")\n", sep = "")
    }
    
    # 2) Remove arquivos intermediários, se pedido
    removed <- character()
    missing <- character()
    
    if (remove_files) {
        for (f in files) {
            if (file.exists(f)) {
                ok <- file.remove(f)
                if (isTRUE(ok)) removed <- c(removed, f) else warning("Não consegui remover: ", f)
            } else {
                missing <- c(missing, f)
            }
        }
        
        if (verbose) {
            cat("\n🧹 Limpeza de arquivos:\n")
            if (length(removed) > 0) {
                cat("Arquivos removidos:\n")
                for (f in removed) cat("  ✓ ", f, "\n", sep = "")
            } else {
                cat("Nenhum arquivo precisou ser removido.\n")
            }
            
            if (length(missing) > 0) {
                cat("\nArquivos já inexistentes (ok):\n")
                for (f in missing) cat("  • ", f, "\n", sep = "")
            }
        }
    } else if (verbose) {
        cat("\n(remoção de arquivos desativada: remove_files = FALSE)\n")
    }
    
    if (verbose) {
        cat(strrep("=", 60), "\n\n", sep = "")
        cat("Pronto. Comandos disponíveis no console: resenv(), runpip()\n\n")
    }
    
    invisible(list(
        removed = removed,
        missing = missing,
        kept_objects = keep
    ))
}

# ------------------------------------------------------------
# runpip(): roda o pipeline completo (opcionalmente resetando antes)
# ------------------------------------------------------------

runpip <- function(
        reset_first = TRUE,
        remove_files = TRUE,
        steps = c(
            file.path("scripts", "01_normaliza.R"),
            file.path("scripts", "02_padroniza.R"),
            file.path("scripts", "03_tipa.R"),
            file.path("scripts", "04_anonimiza.R")
        ),
        open_rds = NULL,       # caminho do .rds para abrir ao final
        assign_name = "final", # nome do objeto no .GlobalEnv (ex.: final)
        preview = TRUE,        # mostra glimpse/head
        verbose = FALSE
) {
    if (reset_first) {
        resenv(remove_files = remove_files, verbose = verbose)
    }
    
    if (verbose) {
        cat("\n", strrep("=", 60), "\n", sep = "")
        cat("▶ Rodando pipeline...\n")
        cat(strrep("=", 60), "\n\n", sep = "")
    }
    
    for (s in steps) {
        if (!file.exists(s)) stop("Script não encontrado: ", s)
        
        if (verbose) cat("→ source('", s, "')\n", sep = "")
        source(s, local = .GlobalEnv)
        
        if (verbose) cat("  ✓ OK: ", s, "\n\n", sep = "")
    }
    
    if (verbose) {
        cat(strrep("=", 60), "\n")
        cat("✓ Pipeline concluído.\n")
        cat(strrep("=", 60), "\n\n")
    }
    
    # ----------------------------------------------------------
    # Pós-pipeline: abrir .rds final
    # ----------------------------------------------------------
    if (!is.null(open_rds)) {
        if (!file.exists(open_rds)) {
            stop("open_rds foi definido, mas o arquivo não existe: ", open_rds)
        }
        
        obj <- readRDS(open_rds)
        assign(assign_name, obj, envir = .GlobalEnv)
        
        if (verbose) {
            cat("📦 Output final carregado:\n")
            cat("  Arquivo: ", open_rds, "\n", sep = "")
            cat("  Objeto:  ", assign_name, " (no .GlobalEnv)\n\n", sep = "")
        }
        
        if (preview) {
            # preview “seguro” para diferentes tipos
            if (inherits(obj, "data.frame")) {
                cat("📋 PREVIEW:\n")
                cat("  Linhas: ", nrow(obj), " | Colunas: ", ncol(obj), "\n\n", sep = "")
                print(utils::head(obj, 10))
                cat("\n")
                # se tiver dplyr carregado, glimpse fica lindo
                if ("dplyr" %in% loadedNamespaces()) {
                    dplyr::glimpse(obj)
                }
            } else {
                cat("📋 PREVIEW (str):\n")
                utils::str(obj, max.level = 2)
            }
            cat("\n")
        }
    }
    
    invisible(TRUE)
}

unicos <- function(coluna) {
    coluna <- coluna[!is.na(coluna)]
    coluna_split <- strsplit(coluna, ",")
    coluna <- unlist(coluna_split)
    coluna <- trimws(coluna)
    coluna <- tolower(coluna)
    coluna <- sort(unique(coluna))
    
    clipr::write_clip(coluna)
    return(coluna)
}

conflitos_n_lesoes <- function(
        df = NULL,
        path = file.path("data", "clean", "rds", "03_Dados_padronizados.rds"),
        cols_base = c(
            "localizacao", "fech", "fech_sub", "histo_tipo", "histo_tipo_det",
            "histo_subtipo", "histo_difer", "histo_qualif", "comport", "margens_ap"
        )
) {
    if (is.null(df)) {
        if (!file.exists(path)) stop("Arquivo nao encontrado: ", path)
        df <- readRDS(path)
    }
    if (!"paciente" %in% names(df)) stop("Coluna 'paciente' nao encontrada.")
    
    normalizar_separador <- function(x) {
        if (is.na(x)) return(NA_character_)
        x <- gsub("\\s*;\\s*", ", ", x)
        x <- gsub(",\\s*,+", ", ", x)
        x <- gsub("^,\\s*|\\s*,?$", "", x)
        x <- gsub("\\s+", " ", x)
        trimws(x)
    }
    
    contar_valores <- function(x) {
        if (is.factor(x)) x <- as.character(x)
        if (is.na(x) || trimws(as.character(x)) == "") return(0L)
        x <- normalizar_separador(as.character(x))
        if (is.na(x) || x == "") return(0L)
        tokens <- strsplit(x, ",\\s*")[[1]]
        tokens <- trimws(tokens)
        tokens <- tokens[tokens != ""]
        length(tokens)
    }
    
    out <- list()
    idx <- 1
    
    for (sufixo in c("1", "2")) {
        n_col <- paste0("n_lesoes_", sufixo)
        if (!n_col %in% names(df)) next
        
        for (base in cols_base) {
            col <- paste0(base, "_", sufixo)
            if (!col %in% names(df)) next
            
            for (i in seq_len(nrow(df))) {
                n_les <- df[[n_col]][i]
                if (is.na(n_les) || n_les <= 0) next
                
                n_val <- contar_valores(df[[col]][i])
                if (n_val == 0) next
                if (!(n_val %in% c(1L, as.integer(n_les)))) {
                    out[[idx]] <- data.frame(
                        linha = i,
                        paciente = df$paciente[i],
                        visita = as.integer(sufixo),
                        coluna = col,
                        n_lesoes = as.integer(n_les),
                        n_valores = as.integer(n_val),
                        valor = as.character(df[[col]][i]),
                        stringsAsFactors = FALSE
                    )
                    idx <- idx + 1
                }
            }
        }
    }
    
    if (length(out) == 0) {
        return(data.frame(
            linha = integer(),
            paciente = character(),
            visita = integer(),
            coluna = character(),
            n_lesoes = integer(),
            n_valores = integer(),
            valor = character(),
            stringsAsFactors = FALSE
        ))
    }
    
    do.call(rbind, out)
}

rload <- function(arquivo, path = "data/clean/rds", v = FALSE) {
    keep <- c("rload")
    rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
    gc()
    
    dados <- readRDS(file.path(path, paste0(arquivo, ".rds")))
    
    if (isTRUE(v)) View(dados)
    
    return(dados)
}

verdf <- function(num) {
    if (num == 1) {
        readRDS("data/clean/rds/90arios_normalizado.rds")
    }
    else if (num == 2) {
        readRDS("data/clean/rds/90arios_padronizado.rds")
    }
    else if (num == 3) {
        readRDS("data/clean/rds/90arios_tipado.rds")
    }
    else if (num == 4) {
        readRDS("data/clean/rds/90arios_anon.rds")
    }
}

runpip()

packtest <- function(pkg_name) {
    # Verifica se o pacote NÃO está instalado/carregado
    if (!require(pkg_name, character.only = TRUE)) {
        
        message(paste("Opa! O pacote", pkg_name, "não está aqui. Instalando agora..."))
        
        # O dependencies=TRUE é o "Y" automático para dependências
        install.packages(pkg_name, dependencies = TRUE)
        
        # Carrega a biblioteca logo após instalar
        library(pkg_name, character.only = TRUE)
        
        message("Pronto! Instalado e carregado.")
        
    } else {
        message(paste("O pacote", pkg_name, "já estava na mão. Carregado!"))
    }
}

# ---------------------------------------------------------------
# Mensagem de carregamento (pra ficar gostoso de usar no console)
# ---------------------------------------------------------------

cat("Utilitários carregados: resenv(), runpip(), unicos(), rload()\n")
