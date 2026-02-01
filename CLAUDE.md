# CLAUDE.md

## Visao geral (estado atual)
Este repositorio padroniza um banco clinico de cirurgias dermatologicas (cbc/cec) com alto volume de texto livre. O pipeline foi refatorado para dois passos oficiais, com entradas/saidas deterministicas, sem dependencia de estado em `.GlobalEnv`.

Principios:
- Previsibilidade: transformacoes deterministicas e documentadas.
- Rastreabilidade: cada etapa registra entradas/saidas (incluindo hash do bruto).
- Independencia de estado: nenhum script assume objetos no ambiente.

## Estrutura atual
- `scripts/`: pipeline e utilitarios.
- `data/raw/`: entradas brutas e dicionarios (fonte de verdade).
- `data/clean/rds/`: saidas canonicas.
- `data/clean/tsv/`: saidas para inspecao.
- `docs/`: documentacao e `Plantas.rds`.
- `outputs/`: relatorios e validacoes.
- `renv/` e `renv.lock`: dependencias isoladas.

Fluxo unidirecional: `data/raw` -> `data/clean`.

## Import universal
- **`scripts/00_Importa.R`**: import universal de dependencias. Deve ser a primeira linha executavel de cada script do pipeline.
- **`scripts/00_libs.R`**: wrapper de compatibilidade que apenas chama `00_Importa.R`.

## Pipeline oficial (atual)
Ordem recomendada:
1) `scripts/01_Anonimiza.R`
2) `scripts/02_Padroniza.R`
3) `scripts/05_Valida.R` (validacao opcional)

### 01_Anonimiza.R
Entradas:
- `data/raw/dados_primarios.tsv`
- `data/raw/padronizacao_ortografia.tsv`
- `data/raw/wfo_plantlist_2025-06/` (taxon/name/synonym)

Processo:
- Leitura e limpeza inicial, renomeacao posicional, normalizacao basica.
- Deduplicacao deterministica.
- Geracao de pool de plantas (WFO).
- Anonimizacao deterministica com `ANON_SECRET` (sem mapping persistente).

Saidas:
- `data/clean/rds/02_Dados_anonimizados.rds`
- `data/clean/tsv/plantas.tsv`
- `docs/Plantas.rds`

### 02_Padroniza.R
Entradas:
- `data/clean/rds/02_Dados_anonimizados.rds`
- `data/raw/dados_primarios.tsv` (hash no log)
- `data/raw/padronizacao_comorbidades.tsv`
- `data/raw/padronizacao_histologia.tsv`
- `data/raw/padronizacao_fechamento.tsv`

Processo (principais pontos):
- Padroniza comorbidades e histologia (tipo, subtipo, diferenciacao).
- **Adiciona `histo_qualif_*`** (ulcerado/pigmentado/multifocal).
- Padroniza fechamento e localizacao.
- **Separadores padrao `", "`** (substitui `;`).
- **Expande valores unicos** para casar com `n_lesoes_*` nas colunas 1:1:
  `localizacao`, `fech`, `fech_sub`, `histo_tipo`, `histo_tipo_det`, `histo_difer`, `comport`, `margens_ap`, `histo_subtipo`.
- Correcoes manuais aplicadas para casos pontuais de paciente/coluna.

Saida:
- `data/clean/rds/03_Dados_padronizados.rds`

### 05_Valida.R (opcional)
Entrada:
- `data/clean/rds/03_Dados_padronizados.rds`

Saida:
- `outputs/inconsistencias.tsv`

Checagens:
- Relacao 1:1 com `n_lesoes_*` (1 ou n_lesoes).
- Separadores invalidos, tokens vazios, espacos irregulares.
- Dominios invalidos (sexo/fototipo/histo/fech).
- `n_lesoes` NA/0 com valores preenchidos e `n_lesoes` > 0 com tudo vazio.

## Scripts auxiliares
- `scripts/03_Explora.R`: exploratorio; pode estar defasado (revisar entradas antes de usar).
- `scripts/99.R`: utilitarios antigos; atualmente referencia scripts antigos (nao usar para rodar o pipeline sem atualizar).

## Comandos de execucao
Etapas oficiais:
- `Rscript scripts/01_Anonimiza.R`
- `Rscript scripts/02_Padroniza.R`
- `Rscript scripts/05_Valida.R`

## Convencoes de codigo
- Primeira linha executavel: `source(file.path("scripts", "00_Importa.R"))`.
- Cabecalho completo (arquivo, papel, entradas, saidas).
- Secoes: `# ---- Nome da secao ----`.
- Indentacao 4 espacos; atribuicao com `<-`.
- Nomes em `snake_case`.
- Criar diretorios com `dir.create(..., recursive = TRUE, showWarnings = FALSE)`.
- Usar `cat()` para logs.

## Seguranca e privacidade
- `ANON_SECRET` e obrigatorio em `01_Anonimiza.R`.
- Nunca commitar segredos ou dados identificaveis.
- Nao ha mais `patient_to_plant.tsv` persistente.

## Nota final
O pipeline oficial atual e **01_Anonimiza -> 02_Padroniza**, com validacao via **05_Valida**. Qualquer alteracao deve manter a rastreabilidade e o fluxo unidirecional `data/raw` -> `data/clean`.
