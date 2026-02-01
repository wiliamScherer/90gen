# ==============================================================================
# 90gen/scripts/00_Importa.R
# ==============================================================================
# Papel: Import universal de dependencias do projeto via renv.
# Entradas: (nenhuma)
# Saidas: (nenhuma - apenas carrega pacotes no ambiente)
# ==============================================================================

packages <- c(
    # Manipulação de dados
    "dplyr",
    "tidyr",
    "readr",
    "stringr",
    "stringi",
    "tibble",
    "janitor",
    
    # Utilitários
    "digest",
    "jsonlite",
    "clipr",
    
    # Tabelas
    "knitr",
    "kableExtra",
    "gtsummary",
    "gt",
    "flextable",
    
    # Visualização
    "ggplot2",
    "ggpubr",
    "scales",
    "patchwork",
    "RColorBrewer"
)

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
    renv::install(missing, prompt = FALSE, quiet = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))