
library(readr)

# Deriva colunas relevantes do dataframe anonimos

relevantes <- c(
    "n_lesoes_1",
    "localizacao_1",
    "tipo_histo_1",
    "margens_ap_1",
    "fechamento_1",
    "complicacoes_1",
    "reintervencao_1",
    "n_lesoes_2",
    "localizacao_2",
    "tipo_histo_2",
    "margens_ap_2",
    "fechamento_2",
    "complicacoes_2",
    "reintervencao_2",
    "mais_cx"
)

raw <- anonimos[, relevantes]

# Deriva colunas relevantes do dataframe padroes

relevantes_2 <- c(
    "paciente",
    "n_lesoes_1",
    "localizacao_1",
    "fech_1",
    "fech_sub_1",
    "complicacao_1",
    "reintervencao_1",
    "histo_tipo_1",
    "histo_tipo_det_1",
    "histo_subtipo_1",
    "histo_difer_1",
    "histo_qualif_1",
    "comport_1",
    "margens_ap_1",
    "n_lesoes_2",
    "localizacao_2",
    "fech_2",
    "fech_sub_2",
    "complicacao_2",
    "reintervencao_2",
    "histo_tipo_2",
    "histo_tipo_det_2",
    "histo_subtipo_2",
    "histo_difer_2",
    "histo_qualif_2",
    "comport_2",
    "margens_ap_2"
)

clean <- padroes[, relevantes_2]

write_csv(raw, "data/derived/raw_lesoes.csv")
write_csv(clean, "data/derived/clean_lesoes.csv")

write_tsv(anonimos, "anon_full.tsv")
