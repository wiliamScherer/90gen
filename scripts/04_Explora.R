# ==============================================================================
# 90gen/scripts/04_Explora.R
# ==============================================================================
# Papel: Análise exploratória completa dos dados de cirurgia dermatológica
#        em pacientes idosos (≥90 anos). Gera tabelas e figuras para publicação.
#
# Entradas:
#   - data/derived/pacientes.rds
#   - data/derived/lesoes.rds
#
# Saídas:
#   - output/tabelas/table1_pacientes.docx
#   - output/tabelas/table2_lesoes.docx
#   - output/tabelas/table3_bivariada.docx
#   - output/figuras/fig1_demographics.png
#   - output/figuras/fig2_lesoes.png
#   - output/figuras/fig3_localizacao.png
#   - output/figuras/fig4_complicacoes.png
#
# Autor: Pipeline automatizado
# Data: 2025-01-22
# ==============================================================================

# ==============================================================================
# CONFIGURAÇÃO E PACOTES
# ==============================================================================

source(file.path("scripts", "00_Importa.R"))

# Configurações globais
theme_set(theme_pubr(base_size = 12))
options(gtsummary.tbl_summary.percent_fun = function(x) sprintf("%.1f", x * 100))

# Paleta de cores institucional
cores <- list(
    cbc = "#2E86AB",
    cec = "#A23B72",
    sexo = c(f = "#E85D75", m = "#4A90A4"),
    fototipo = brewer.pal(6, "YlOrBr"),
    complicacao = c(sim = "#D64045", nao = "#7FB069")
)

# Diretórios
dir_input <- file.path("data", "derived")
dir_tabelas <- file.path("output", "tabelas")
dir_figuras <- file.path("output", "figuras")

dir.create(dir_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# CARREGAR DADOS
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("04_EXPLORA: Carregando dados\n")
cat(strrep("=", 70), "\n\n")

pacientes <- readRDS(file.path(dir_input, "pacientes.rds"))
lesoes <- readRDS(file.path(dir_input, "lesoes.rds"))

cat("Pacientes:", nrow(pacientes), "\n")
cat("Lesões:", nrow(lesoes), "\n")

# ==============================================================================
# PRÉ-PROCESSAMENTO
# ==============================================================================

# Cria variáveis derivadas em pacientes
pacientes <- pacientes %>%
    mutate(
        # Faixa etária
        faixa_etaria = cut(
            idade,
            breaks = c(89, 92, 95, 98, Inf),
            labels = c("90-92", "93-95", "96-98", "≥99"),
            right = TRUE
        ),
        # Número de comorbidades
        n_comorbidades = rowSums(across(
            c(has, ic, fa, dac, ivc, arritmia, avc,
              alzheimer, parkinson, epilepsia, depressao, ansiedade, esquizofrenia,
              dm, dlp, hipotir, hipertir,
              drc, hpb, incont_ur,
              ca_rim, ca_prostata, ca_laringe, leucemia, mm,
              asma, dpoc, tabagismo, `ex-tabag`, etilismo,
              defic_audit, anemia, osteoartrite, osteoporose),
            ~ . == TRUE
        ), na.rm = TRUE),
        # Categorias de comorbidades
        cat_comorbidades = cut(
            n_comorbidades,
            breaks = c(-1, 0, 2, 4, Inf),
            labels = c("Nenhuma", "1-2", "3-4", "≥5")
        ),
        # Sexo formatado
        sexo_label = factor(sexo, levels = c("f", "m"), labels = c("Feminino", "Masculino")),
        # Teve complicação em alguma cirurgia?
        teve_complicacao = !is.na(n_lesoes_total) # placeholder, será calculado depois
    )

# Cria variáveis derivadas em lesões
lesoes <- lesoes %>%
    mutate(
        # Tipo formatado
        tipo_label = factor(
            histo_tipo,
            levels = c("cbc", "cec"),
            labels = c("CBC", "CEC")
        ),
        # Subtipo agrupado (principais)
        subtipo_grupo = case_when(
            is.na(histo_subtipo) ~ "Não especificado",
            str_detect(histo_subtipo, "nodular") ~ "Nodular",
            str_detect(histo_subtipo, "superficial") ~ "Superficial",
            str_detect(histo_subtipo, "infiltrativo|solido|desmoplasico") ~ "Infiltrativo/Sólido",
            str_detect(histo_subtipo, "esclerodermico") ~ "Esclerodermiforme",
            str_detect(histo_subtipo, "micronodular") ~ "Micronodular",
            TRUE ~ "Outro"
        ),
        # Localização agrupada
        local_grupo = case_when(
            localizacao %in% c("face", "orelha") ~ "Face/Orelha",
            localizacao == "couro cabeludo" ~ "Couro cabeludo",
            localizacao %in% c("tórax", "dorso") ~ "Tronco",
            localizacao == "cervical" ~ "Cervical",
            str_detect(localizacao, "membro superior|antebraço") ~ "Membro superior",
            str_detect(localizacao, "membro inferior|pé") ~ "Membro inferior",
            TRUE ~ "Outro"
        ),
        # Margens comprometidas
        margens_comprom = case_when(
            is.na(margens_ap) ~ NA,
            str_detect(margens_ap, "comprometidas") ~ TRUE,
            margens_ap == "livres" ~ FALSE,
            TRUE ~ NA
        ),
        # Teve complicação
        teve_complicacao = !is.na(complicacao) & complicacao != "",
        # Comportamento formatado
        comport_label = case_when(
            comport == "in situ" ~ "In situ",
            comport == "invasivo" ~ "Invasivo",
            TRUE ~ NA_character_
        )
    )

# Junta informações do paciente nas lesões
lesoes <- lesoes %>%
    left_join(
        pacientes %>% select(paciente, idade, sexo_label, fototipo, faixa_etaria, cat_comorbidades),
        by = "paciente"
    )

# ==============================================================================
# TABLE 1: CARACTERÍSTICAS DOS PACIENTES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("TABLE 1: Características dos pacientes\n")
cat(strrep("=", 70), "\n\n")

table1 <- pacientes %>%
    select(
        idade, sexo_label, fototipo, faixa_etaria,
        n_comorbidades, cat_comorbidades,
        # Comorbidades mais relevantes
        has, dm, dlp, ic, fa, drc,
        alzheimer, parkinson, dpoc,
        cbc_cec_previo, n_lesoes_total
    ) %>%
    tbl_summary(
        label = list(
            idade ~ "Idade (anos)",
            sexo_label ~ "Sexo",
            fototipo ~ "Fototipo (Fitzpatrick)",
            faixa_etaria ~ "Faixa etária",
            n_comorbidades ~ "Número de comorbidades",
            cat_comorbidades ~ "Comorbidades (categorias)",
            has ~ "Hipertensão arterial",
            dm ~ "Diabetes mellitus",
            dlp ~ "Dislipidemia",
            ic ~ "Insuficiência cardíaca",
            fa ~ "Fibrilação atrial",
            drc ~ "Doença renal crônica",
            alzheimer ~ "Doença de Alzheimer",
            parkinson ~ "Doença de Parkinson",
            dpoc ~ "DPOC",
            cbc_cec_previo ~ "CBC/CEC prévio",
            n_lesoes_total ~ "Total de lesões operadas"
        ),
        type = list(
            fototipo ~ "categorical",
            n_comorbidades ~ "continuous",
            n_lesoes_total ~ "continuous"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}–{p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(0, 1)
        ),
        missing = "no"
    ) %>%
    bold_labels() %>%
    modify_header(label = "**Característica**") %>%
    modify_caption("**Tabela 1.** Características demográficas e clínicas dos pacientes (N = {N})")

print(table1)

# Salva em Word
table1 %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = file.path(dir_tabelas, "table1_pacientes.docx"))

cat("\nTabela salva:", file.path(dir_tabelas, "table1_pacientes.docx"), "\n")

# ==============================================================================
# TABLE 2: CARACTERÍSTICAS DAS LESÕES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("TABLE 2: Características das lesões\n")
cat(strrep("=", 70), "\n\n")

table2 <- lesoes %>%
    select(
        tipo_label, local_grupo, subtipo_grupo,
        comport_label, margens_comprom, teve_complicacao
    ) %>%
    tbl_summary(
        by = tipo_label,
        label = list(
            local_grupo ~ "Localização",
            subtipo_grupo ~ "Subtipo histológico",
            comport_label ~ "Comportamento",
            margens_comprom ~ "Margens comprometidas",
            teve_complicacao ~ "Complicação pós-operatória"
        ),
        statistic = all_categorical() ~ "{n} ({p}%)",
        digits = all_categorical() ~ c(0, 1),
        missing = "no"
    ) %>%
    add_overall(col_label = "**Total**") %>%
    add_p(
        test = all_categorical() ~ "chisq.test",
        pvalue_fun = function(x) style_pvalue(x, digits = 3)
    ) %>%
    bold_labels() %>%
    bold_p(t = 0.05) %>%
    modify_header(label = "**Característica**") %>%
    modify_caption("**Tabela 2.** Características das lesões por tipo histológico (N = {N})")

print(table2)

# Salva em Word
table2 %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = file.path(dir_tabelas, "table2_lesoes.docx"))

cat("\nTabela salva:", file.path(dir_tabelas, "table2_lesoes.docx"), "\n")

# ==============================================================================
# TABLE 3: ANÁLISE BIVARIADA - FATORES ASSOCIADOS A COMPLICAÇÕES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("TABLE 3: Fatores associados a complicações\n")
cat(strrep("=", 70), "\n\n")

table3 <- lesoes %>%
    filter(!is.na(teve_complicacao)) %>%
    select(
        teve_complicacao,
        idade, sexo_label, fototipo, cat_comorbidades,
        tipo_label, local_grupo, margens_comprom
    ) %>%
    tbl_summary(
        by = teve_complicacao,
        label = list(
            idade ~ "Idade (anos)",
            sexo_label ~ "Sexo",
            fototipo ~ "Fototipo",
            cat_comorbidades ~ "Comorbidades",
            tipo_label ~ "Tipo histológico",
            local_grupo ~ "Localização",
            margens_comprom ~ "Margens comprometidas"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}–{p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(0, 1)
        ),
        missing = "no"
    ) %>%
    add_p(
        test = list(
            all_continuous() ~ "wilcox.test",
            all_categorical() ~ "chisq.test"
        ),
        pvalue_fun = function(x) style_pvalue(x, digits = 3)
    ) %>%
    bold_labels() %>%
    bold_p(t = 0.05) %>%
    modify_header(
        label = "**Fator**",
        stat_1 = "**Sem complicação**",
        stat_2 = "**Com complicação**"
    ) %>%
    modify_caption("**Tabela 3.** Fatores associados a complicações pós-operatórias")

print(table3)

# Salva em Word
table3 %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = file.path(dir_tabelas, "table3_bivariada.docx"))

cat("\nTabela salva:", file.path(dir_tabelas, "table3_bivariada.docx"), "\n")

# ==============================================================================
# FIGURA 1: CARACTERÍSTICAS DEMOGRÁFICAS
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FIGURA 1: Características demográficas\n")
cat(strrep("=", 70), "\n\n")

# 1A: Distribuição de idade por sexo (pirâmide etária adaptada)
piramide_data <- pacientes %>%
    count(faixa_etaria, sexo_label) %>%
    mutate(n_plot = ifelse(sexo_label == "Masculino", -n, n))

fig1a <- piramide_data %>%
    ggplot(aes(x = faixa_etaria, y = n_plot, fill = sexo_label)) +
    geom_col(width = 0.7) +
    geom_text(
        aes(label = abs(n_plot), hjust = ifelse(n_plot < 0, 1.2, -0.2)),
        size = 3.5
    ) +
    coord_flip() +
    scale_y_continuous(
        labels = abs,
        limits = c(-max(abs(piramide_data$n_plot)) - 3,
                   max(abs(piramide_data$n_plot)) + 3)
    ) +
    scale_fill_manual(values = cores$sexo) +
    labs(
        x = "Faixa etária (anos)",
        y = "Número de pacientes",
        fill = "Sexo",
        title = "A"
    ) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)
    )

# 1B: Distribuição de fototipo
fig1b <- pacientes %>%
    count(fototipo) %>%
    filter(!is.na(fototipo)) %>%
    ggplot(aes(x = fototipo, y = n, fill = as.factor(fototipo))) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = cores$fototipo[1:6]) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Fototipo (Fitzpatrick)",
        y = "Número de pacientes",
        title = "B"
    ) +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14)
    )

# 1C: Distribuição de comorbidades
fig1c <- pacientes %>%
    count(cat_comorbidades) %>%
    filter(!is.na(cat_comorbidades)) %>%
    ggplot(aes(x = cat_comorbidades, y = n, fill = cat_comorbidades)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
    scale_fill_brewer(palette = "Blues") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Número de comorbidades",
        y = "Número de pacientes",
        title = "C"
    ) +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14)
    )

# 1D: Comorbidades mais frequentes
top_comorb <- pacientes %>%
    summarise(across(
        c(has, dm, dlp, ic, dpoc, hipotir, alzheimer, parkinson, drc, fa),
        ~ sum(. == TRUE, na.rm = TRUE)
    )) %>%
    pivot_longer(everything(), names_to = "comorbidade", values_to = "n") %>%
    mutate(
        comorbidade = recode(comorbidade,
                             has = "HAS", dm = "DM", dlp = "Dislipidemia",
                             ic = "IC", dpoc = "DPOC", hipotir = "Hipotireoidismo",
                             alzheimer = "Alzheimer", parkinson = "Parkinson",
                             drc = "DRC", fa = "FA"
        )
    ) %>%
    arrange(desc(n)) %>%
    head(8)

fig1d <- top_comorb %>%
    mutate(comorbidade = fct_reorder(comorbidade, n)) %>%
    ggplot(aes(x = comorbidade, y = n, fill = n)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#BDD7E7", high = "#2171B5") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = NULL,
        y = "Número de pacientes",
        title = "D"
    ) +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14)
    )

# Combina painéis
fig1 <- (fig1a | fig1b) / (fig1c | fig1d) +
    plot_annotation(
        title = "Características demográficas e clínicas dos pacientes",
        theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )

ggsave(
    file.path(dir_figuras, "fig1_demographics.png"),
    fig1, width = 12, height = 10, dpi = 300, bg = "white"
)

cat("Figura salva:", file.path(dir_figuras, "fig1_demographics.png"), "\n")

# ==============================================================================
# FIGURA 2: CARACTERÍSTICAS DAS LESÕES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FIGURA 2: Características das lesões\n")
cat(strrep("=", 70), "\n\n")

# 2A: Tipo histológico
fig2a <- lesoes %>%
    filter(!is.na(tipo_label)) %>%
    count(tipo_label) %>%
    mutate(
        pct = n / sum(n) * 100,
        label = sprintf("%d\n(%.1f%%)", n, pct)
    ) %>%
    ggplot(aes(x = tipo_label, y = n, fill = tipo_label)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = label), vjust = -0.3, size = 4) +
    scale_fill_manual(values = c(cores$cbc, cores$cec)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de lesões", title = "A. Tipo histológico") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 2B: Subtipos de CBC
fig2b <- lesoes %>%
    filter(histo_tipo == "cbc", !is.na(subtipo_grupo)) %>%
    count(subtipo_grupo) %>%
    mutate(
        pct = n / sum(n) * 100,
        subtipo_grupo = fct_reorder(subtipo_grupo, n)
    ) %>%
    ggplot(aes(x = subtipo_grupo, y = n, fill = subtipo_grupo)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%d", n)), hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_fill_brewer(palette = "Blues") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Número de lesões", title = "B. Subtipos de CBC") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 2C: Comportamento do CEC
fig2c <- lesoes %>%
    filter(histo_tipo == "cec", !is.na(comport_label)) %>%
    count(comport_label) %>%
    mutate(
        pct = n / sum(n) * 100,
        label = sprintf("%d (%.1f%%)", n, pct)
    ) %>%
    ggplot(aes(x = comport_label, y = n, fill = comport_label)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = label), vjust = -0.3, size = 4) +
    scale_fill_manual(values = c("#F4A261", "#E76F51")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de lesões", title = "C. Comportamento do CEC") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 2D: Status das margens
fig2d <- lesoes %>%
    filter(!is.na(margens_comprom)) %>%
    count(margens_comprom) %>%
    mutate(
        status = ifelse(margens_comprom, "Comprometidas", "Livres"),
        pct = n / sum(n) * 100,
        label = sprintf("%d (%.1f%%)", n, pct)
    ) %>%
    ggplot(aes(x = status, y = n, fill = status)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = label), vjust = -0.3, size = 4) +
    scale_fill_manual(values = c("Comprometidas" = "#E63946", "Livres" = "#2A9D8F")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de lesões", title = "D. Status das margens") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# Combina painéis
fig2 <- (fig2a | fig2b) / (fig2c | fig2d) +
    plot_annotation(
        title = "Características histopatológicas das lesões",
        theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )

ggsave(
    file.path(dir_figuras, "fig2_lesoes.png"),
    fig2, width = 12, height = 10, dpi = 300, bg = "white"
)

cat("Figura salva:", file.path(dir_figuras, "fig2_lesoes.png"), "\n")

# ==============================================================================
# FIGURA 3: LOCALIZAÇÃO DAS LESÕES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FIGURA 3: Localização das lesões\n")
cat(strrep("=", 70), "\n\n")

# 3A: Localização geral
fig3a <- lesoes %>%
    filter(!is.na(local_grupo)) %>%
    count(local_grupo) %>%
    mutate(
        pct = n / sum(n) * 100,
        local_grupo = fct_reorder(local_grupo, n)
    ) %>%
    ggplot(aes(x = local_grupo, y = n, fill = local_grupo)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%d (%.0f%%)", n, pct)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de lesões", title = "A. Localização anatômica") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 3B: Localização por tipo histológico
fig3b <- lesoes %>%
    filter(!is.na(local_grupo), !is.na(tipo_label)) %>%
    count(local_grupo, tipo_label) %>%
    group_by(local_grupo) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(local_grupo = fct_reorder(local_grupo, n, .fun = sum)) %>%
    ggplot(aes(x = local_grupo, y = n, fill = tipo_label)) +
    geom_col(width = 0.7, position = "stack") +
    geom_text(
        aes(label = n),
        position = position_stack(vjust = 0.5),
        size = 3, color = "white", fontface = "bold"
    ) +
    coord_flip() +
    scale_fill_manual(values = c(cores$cbc, cores$cec)) +
    labs(
        x = NULL, y = "Número de lesões", fill = "Tipo",
        title = "B. Localização por tipo histológico"
    ) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
    )

# Combina
fig3 <- fig3a + fig3b +
    plot_annotation(
        title = "Distribuição anatômica das lesões",
        theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )

ggsave(
    file.path(dir_figuras, "fig3_localizacao.png"),
    fig3, width = 14, height = 6, dpi = 300, bg = "white"
)

cat("Figura salva:", file.path(dir_figuras, "fig3_localizacao.png"), "\n")

# ==============================================================================
# FIGURA 4: DESFECHOS CIRÚRGICOS
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FIGURA 4: Desfechos cirúrgicos\n")
cat(strrep("=", 70), "\n\n")

# 4A: Tipo de fechamento
fig4a <- lesoes %>%
    filter(!is.na(fech)) %>%
    count(fech) %>%
    mutate(
        pct = n / sum(n) * 100,
        fech_label = recode(fech,
                            "direto" = "Fechamento\ndireto",
                            "retalho" = "Retalho",
                            "enxerto" = "Enxerto",
                            "2intencao" = "Segunda\nintenção"
        ),
        fech_label = fct_reorder(fech_label, n, .desc = TRUE)
    ) %>%
    ggplot(aes(x = fech_label, y = n, fill = fech)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%d\n(%.0f%%)", n, pct)), vjust = -0.2, size = 3.5) +
    scale_fill_brewer(palette = "Pastel1") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de lesões", title = "A. Tipo de fechamento") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 4B: Complicações por tipo de fechamento
fig4b <- lesoes %>%
    filter(!is.na(fech)) %>%
    group_by(fech) %>%
    summarise(
        total = n(),
        complicacoes = sum(teve_complicacao, na.rm = TRUE),
        taxa = complicacoes / total * 100,
        .groups = "drop"
    ) %>%
    mutate(fech_label = recode(fech,
                               "direto" = "Direto", "retalho" = "Retalho",
                               "enxerto" = "Enxerto", "2intencao" = "2ª intenção"
    )) %>%
    ggplot(aes(x = fct_reorder(fech_label, taxa), y = taxa, fill = taxa)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", taxa, complicacoes)),
              vjust = -0.2, size = 3.5) +
    scale_fill_gradient(low = "#FFF3B0", high = "#E63946") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = NULL, y = "Taxa de complicações (%)",
         title = "B. Complicações por fechamento") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 4C: Complicações por tipo histológico
fig4c <- lesoes %>%
    filter(!is.na(tipo_label)) %>%
    group_by(tipo_label) %>%
    summarise(
        total = n(),
        complicacoes = sum(teve_complicacao, na.rm = TRUE),
        taxa = complicacoes / total * 100,
        .groups = "drop"
    ) %>%
    ggplot(aes(x = tipo_label, y = taxa, fill = tipo_label)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", taxa, complicacoes)),
              vjust = -0.2, size = 4) +
    scale_fill_manual(values = c(cores$cbc, cores$cec)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = NULL, y = "Taxa de complicações (%)",
         title = "C. Complicações por tipo histológico") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# 4D: Tipos de complicações
fig4d <- lesoes %>%
    filter(teve_complicacao) %>%
    separate_rows(complicacao, sep = ",\\s*") %>%
    count(complicacao) %>%
    mutate(complicacao = fct_reorder(complicacao, n)) %>%
    ggplot(aes(x = complicacao, y = n, fill = complicacao)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = n), hjust = -0.3, size = 4) +
    coord_flip() +
    scale_fill_brewer(palette = "Reds") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = "Número de casos", title = "D. Tipos de complicações") +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

# Combina
fig4 <- (fig4a | fig4b) / (fig4c | fig4d) +
    plot_annotation(
        title = "Desfechos cirúrgicos e complicações",
        theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )

ggsave(
    file.path(dir_figuras, "fig4_complicacoes.png"),
    fig4, width = 12, height = 10, dpi = 300, bg = "white"
)

cat("Figura salva:", file.path(dir_figuras, "fig4_complicacoes.png"), "\n")

# ==============================================================================
# RESUMO ESTATÍSTICO FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("RESUMO ESTATÍSTICO\n")
cat(strrep("=", 70), "\n\n")

cat("PACIENTES:\n")
cat(sprintf("  N total: %d\n", nrow(pacientes)))
cat(sprintf("  Idade: %.1f ± %.1f anos (mediana: %.0f)\n",
            mean(pacientes$idade, na.rm = TRUE),
            sd(pacientes$idade, na.rm = TRUE),
            median(pacientes$idade, na.rm = TRUE)))
cat(sprintf("  Sexo feminino: %d (%.1f%%)\n",
            sum(pacientes$sexo == "f", na.rm = TRUE),
            100 * mean(pacientes$sexo == "f", na.rm = TRUE)))
cat(sprintf("  Comorbidades (mediana): %.0f\n",
            median(pacientes$n_comorbidades, na.rm = TRUE)))

cat("\nLESÕES:\n")
cat(sprintf("  N total: %d\n", nrow(lesoes)))
cat(sprintf("  CBC: %d (%.1f%%)\n",
            sum(lesoes$histo_tipo == "cbc", na.rm = TRUE),
            100 * mean(lesoes$histo_tipo == "cbc", na.rm = TRUE)))
cat(sprintf("  CEC: %d (%.1f%%)\n",
            sum(lesoes$histo_tipo == "cec", na.rm = TRUE),
            100 * mean(lesoes$histo_tipo == "cec", na.rm = TRUE)))
cat(sprintf("  Face/Orelha: %d (%.1f%%)\n",
            sum(lesoes$local_grupo == "Face/Orelha", na.rm = TRUE),
            100 * mean(lesoes$local_grupo == "Face/Orelha", na.rm = TRUE)))

cat("\nDESFECHOS:\n")
cat(sprintf("  Complicações: %d (%.1f%%)\n",
            sum(lesoes$teve_complicacao, na.rm = TRUE),
            100 * mean(lesoes$teve_complicacao, na.rm = TRUE)))
cat(sprintf("  Margens comprometidas: %d (%.1f%%)\n",
            sum(lesoes$margens_comprom == TRUE, na.rm = TRUE),
            100 * mean(lesoes$margens_comprom == TRUE, na.rm = TRUE)))

cat("\n")
cat(strrep("=", 70), "\n")
cat("04_EXPLORA CONCLUÍDO!\n")
cat(strrep("=", 70), "\n\n")

cat("ARQUIVOS GERADOS:\n")
cat("  Tabelas:\n")
cat("    -", file.path(dir_tabelas, "table1_pacientes.docx"), "\n")
cat("    -", file.path(dir_tabelas, "table2_lesoes.docx"), "\n")
cat("    -", file.path(dir_tabelas, "table3_bivariada.docx"), "\n")
cat("  Figuras:\n")
cat("    -", file.path(dir_figuras, "fig1_demographics.png"), "\n")
cat("    -", file.path(dir_figuras, "fig2_lesoes.png"), "\n")
cat("    -", file.path(dir_figuras, "fig3_localizacao.png"), "\n")
cat("    -", file.path(dir_figuras, "fig4_complicacoes.png"), "\n")
cat("\n")