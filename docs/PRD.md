# PRD — 90gen: Pipeline de Análise de Cirurgias Dermatológicas em Nonagenários

> **Versão:** 2.0  
> **Última atualização:** 2025-01-23  
> **Mantido por:** Wiliam + colaboradores

---

## Sumário

1. [Contexto Clínico](#1-contexto-clínico)
2. [Objetivo do Projeto](#2-objetivo-do-projeto)
3. [Modelo de Dados](#3-modelo-de-dados)
4. [Regras de Negócio Clínicas](#4-regras-de-negócio-clínicas)
5. [Arquitetura do Pipeline](#5-arquitetura-do-pipeline)
6. [Especificação dos Scripts](#6-especificação-dos-scripts)
7. [Análise e Apresentação](#7-análise-e-apresentação)
8. [Qualidade e Validação](#8-qualidade-e-validação)
9. [Convenções de Código](#9-convenções-de-código)
10. [Riscos e Pendências](#10-riscos-e-pendências)
11. [Glossário](#11-glossário)

---

## 1. Contexto Clínico

### 1.1 Relevância Epidemiológica

O câncer de pele não-melanoma (CPNM) é a neoplasia maligna mais comum no mundo, representando cerca de 30% de todos os tumores malignos no Brasil. Os dois principais tipos são:

| Tipo | Prevalência | Comportamento | Mortalidade |
|------|-------------|---------------|-------------|
| **Carcinoma Basocelular (CBC)** | ~70% dos CPNM | Crescimento lento, raramente metastatiza | Muito baixa |
| **Carcinoma Espinocelular (CEC)** | ~25% dos CPNM | Mais agressivo, pode metastatizar | Moderada |

### 1.2 Por que Estudar Nonagenários (≥90 anos)?

A população de idosos muito idosos (≥90 anos) representa um desafio único:

- **Exposição solar acumulada:** Décadas de dano actínico resultam em alta incidência de CPNM.
- **Comorbidades múltiplas:** Decisões terapêuticas devem considerar estado funcional e expectativa de vida.
- **Literatura escassa:** Poucos estudos focam especificamente nessa faixa etária.
- **Dilemas éticos:** Quando operar? Qual técnica de fechamento? Vale a pena reintervenção?

Este projeto visa preencher essa lacuna com dados brasileiros de um serviço de referência.

### 1.3 Anatomia dos Dados Clínicos

#### 1.3.1 Carcinoma Basocelular (CBC)

O CBC é classificado por **subtipo histológico**, que determina agressividade e conduta:

| Subtipo | Comportamento | Margens Recomendadas |
|---------|---------------|----------------------|
| Nodular | Indolente, bem delimitado | 4 mm |
| Superficial | Crescimento horizontal | 4 mm |
| Micronodular | Infiltrativo, bordas mal definidas | 5-10 mm |
| Infiltrativo | Agressivo localmente | 5-10 mm |
| Esclerodermiforme | Alto risco de recidiva | Mohs ou 10+ mm |
| Basoescamoso | Comportamento misto CBC/CEC | Tratar como CEC |

**Qualificadores adicionais:** ulcerado, pigmentado, multifocal.

#### 1.3.2 Carcinoma Espinocelular (CEC)

O CEC é classificado por **grau de diferenciação** e **comportamento**:

| Diferenciação | Significado |
|---------------|-------------|
| Bem diferenciado | Células semelhantes à pele normal, menos agressivo |
| Moderadamente diferenciado | Agressividade intermediária |
| Pouco diferenciado | Células atípicas, mais agressivo |

| Comportamento | Significado |
|---------------|-------------|
| In situ | Confinado à epiderme, não invasivo |
| Invasivo | Atravessou a membrana basal, potencial metastático |

**Variantes especiais:**
- **Queratoacantoma:** Considerado CEC de baixo grau; pode regredir espontaneamente.

#### 1.3.3 Técnicas de Fechamento

| Técnica | Indicação Típica | Complexidade |
|---------|------------------|--------------|
| Fechamento direto (primário) | Defeitos pequenos, pele elástica | Baixa |
| Segunda intenção | Áreas côncavas, pacientes frágeis | Baixa |
| Enxerto de pele | Defeitos maiores, áreas sem mobilidade | Média |
| Retalho local | Defeitos em áreas nobres (face) | Alta |
| Mohs | Tumores de alto risco, áreas críticas | Muito alta |

#### 1.3.4 Desfechos de Interesse

- **Margens cirúrgicas:** Livres vs. comprometidas (fator prognóstico principal).
- **Complicações:** Deiscência, infecção, necrose de enxerto/retalho, hematoma.
- **Reintervenção:** Necessidade de nova cirurgia (indica falha do tratamento inicial).

---

## 2. Objetivo do Projeto

### 2.1 Missão

Construir um pipeline reprodutível para **limpar, padronizar e analisar dados de cirurgias dermatológicas em nonagenários**, gerando tabelas derivadas, análises descritivas e relatórios científicos a partir de uma base clínica originalmente heterogênea.

### 2.2 Problema Central

A tabela original apresenta:

- Nomes de colunas inconsistentes ao longo do tempo.
- Valores textuais com variações de ortografia e sinônimos.
- Múltiplas lesões por paciente (até 2 cirurgias, cada uma com múltiplas lesões).
- Mistura de atributos específicos de CBC e CEC no mesmo campo.
- Necessidade de preservar informação clínica máxima mantendo analisabilidade.

### 2.3 Foco Atual

O pipeline de preparo (scripts 00-03) está **estável**. O foco agora é:

- Consumir `data/derived/pacientes.rds` e `data/derived/lesoes.rds`.
- Produzir análises em `04_Explora.R` e documentos `.qmd` (Quarto).
- Gerar tabelas e gráficos interativos para apresentação e publicação.

---

## 3. Modelo de Dados

### 3.1 Entidades e Níveis

```
┌─────────────────────────────────────────────────────────────┐
│                        PACIENTE                             │
│  (1 linha = 1 indivíduo)                                    │
│  - Demográficos: idade, sexo, fototipo                      │
│  - Comorbidades: HAS, DM, Alzheimer, etc. (booleanas)       │
│  - Indicadores: cbc_cec_previo, mohs, mais_cx, obito        │
└─────────────────────┬───────────────────────────────────────┘
                      │ 1:N
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                     CIRURGIA (visita)                       │
│  (sufixo _1 ou _2 no formato wide)                          │
│  - n_lesoes: quantidade de lesões operadas                  │
│  - complicacao: atributo DA CIRURGIA (não da lesão)         │
│  - reintervencao: atributo DA CIRURGIA                      │
└─────────────────────┬───────────────────────────────────────┘
                      │ 1:N
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                        LESÃO                                │
│  (1 linha no formato long)                                  │
│  - Localização: face, tórax, membro superior, etc.          │
│  - Histologia: tipo, tipo_det, subtipo, difer, qualif       │
│  - Fechamento: técnica principal e subtipo                  │
│  - Desfecho: margens_ap (livres/comprometidas)              │
└─────────────────────────────────────────────────────────────┘
```

### 3.2 Formatos de Dados

| Formato | Arquivo | Uso |
|---------|---------|-----|
| **Wide** | `Dados_padronizados.rds` | SSOT (Single Source of Truth), colunas com sufixo `_1`, `_2` |
| **Long (pacientes)** | `pacientes.rds` | Análises demográficas, 1 linha = 1 paciente |
| **Long (lesões)** | `lesoes.rds` | Análises por lesão, 1 linha = 1 lesão |

### 3.3 Esquema de Colunas: Lesões

| Coluna | Tipo | Domínio | Descrição |
|--------|------|---------|-----------|
| `paciente` | chr | — | Identificador anonimizado (nome de planta) |
| `visita` | int | 1, 2 | Número da cirurgia |
| `lesao_idx` | int | 1..n | Índice da lesão dentro da cirurgia |
| `localizacao` | chr | face, tórax, etc. | Localização anatômica |
| `fech` | chr | direto, retalho, enxerto, 2intencao | Técnica de fechamento |
| `fech_sub` | chr | — | Subtipo do fechamento |
| `histo_tipo` | chr | cbc, cec | Tipo histológico principal |
| `histo_tipo_det` | chr | carcinoma basoescamoso, queratoacantoma, NA | Detalhe preservado |
| `histo_subtipo` | chr | nodular, infiltrativo, etc. | **Apenas CBC** |
| `histo_difer` | chr | bem, moder, pouco | **Apenas CEC** |
| `histo_qualif` | chr | ulcerado, pigmentado, multifocal | Qualificadores |
| `comport` | chr | in situ, invasivo | **Apenas CEC** |
| `margens_ap` | chr | livres, comprometidas, comprometidas focalmente | Resultado anatomopatológico |
| `complicacao` | chr | deiscência, infecção, etc. | Atributo da cirurgia |
| `reintervencao` | lgl | TRUE/FALSE | Atributo da cirurgia |

---

## 4. Regras de Negócio Clínicas

### 4.1 Atributos Exclusivos por Tipo Histológico

Esta é uma **regra fundamental** que guia toda a padronização:

| Atributo | CBC | CEC |
|----------|:---:|:---:|
| `histo_subtipo` (nodular, infiltrativo, etc.) | ✅ | ❌ |
| `histo_difer` (bem, moder, pouco) | ❌ | ✅ |
| `comport` (in situ, invasivo) | ❌ | ✅ |
| `histo_qualif` (ulcerado, pigmentado) | ✅ | ✅ |

**Implicação técnica:** Se um CEC aparece com `subtipo`, há erro de atribuição.

### 4.2 Mapeamento de Tipos Especiais

| Termo Original | `histo_tipo` | `histo_tipo_det` |
|----------------|--------------|------------------|
| carcinoma basoescamoso | cbc | carcinoma basoescamoso |
| queratoacantoma | cec | queratoacantoma |
| CBC (qualquer subtipo) | cbc | NA |
| CEC (qualquer diferenciação) | cec | NA |

### 4.3 Regra de Múltiplas Lesões

Quando o texto de histologia contém múltiplas lesões separadas por vírgula:

**Exemplo:** `"cbc sólido infiltrativo, cec in situ"` com `n_lesoes = 2`

**Problema:** Como saber se "infiltrativo" pertence à lesão 1 (CBC) ou lesão 2 (CEC)?

**Solução implementada (em duas partes):**

1. **Separação por marcadores de tumor:** Nova lesão inicia quando detecta padrão de tipo tumoral (`cbc`, `cec`, `carcinoma`, `queratoacantoma`).

2. **Ordenação por peso:** Lesões com menos atributos vêm primeiro. Isso garante que na expansão, os tokens excedentes fiquem na última lesão (a que tem mais atributos).

3. **Agrupamento de excedentes:** Se há mais tokens que lesões, agrupa os extras na última posição.

**Resultado:**
```
Input:  "cbc sólido infiltrativo, cec in situ"
Lesões: ["cec in situ", "cbc sólido infiltrativo"]  (CEC primeiro, menos peso)
Tipos:  "cec, cbc"
Subtipos: "NA, sólido, infiltrativo"  (3 tokens)

Expansão para n=2:
  Lesão 1 (CEC): subtipo = NA ✓
  Lesão 2 (CBC): subtipo = "sólido, infiltrativo" ✓
```

### 4.4 Regra de Expansão

Quando `n_lesoes > 1` e há menos valores que lesões:

| Situação | Ação |
|----------|------|
| 1 valor, n lesões | Replica o valor para todas |
| k valores, n lesões (k < n) | Repete o **ÚLTIMO** valor |
| k valores, n lesões (k > n) | Agrupa os últimos k-n+1 na última posição |
| k valores, n lesões (k = n) | Atribui 1:1 |

### 4.5 Atributos da Cirurgia vs. da Lesão

| Atributo | Nível | Comportamento na Expansão |
|----------|-------|---------------------------|
| `localizacao` | Lesão | Expande (cada lesão pode ter local diferente) |
| `histo_*` | Lesão | Expande |
| `margens_ap` | Lesão | Expande |
| `fech`, `fech_sub` | Lesão | Expande |
| `complicacao` | **Cirurgia** | Replica para todas as lesões da visita |
| `reintervencao` | **Cirurgia** | Replica para todas as lesões da visita |

---

## 5. Arquitetura do Pipeline

### 5.1 Estrutura de Pastas

```
90gen/
├── scripts/
│   ├── 00_Importa.R          # Import universal de dependências
│   ├── 01_Anonimiza.R        # Anonimização determinística
│   ├── 02_Padroniza.R        # Padronização e normalização
│   ├── 03_Deriva.R           # Geração de tabelas derivadas
│   ├── 04_Explora.R          # Análise exploratória
│   └── 05_Valida.R           # Validação de qualidade
├── data/
│   ├── raw/                  # Dados brutos (fonte de verdade)
│   ├── clean/
│   │   ├── rds/              # Dados limpos (formato R)
│   │   └── tsv/              # Dados limpos (inspeção)
│   └── derived/              # Tabelas derivadas para análise
├── output/
│   ├── tabelas/              # Tabelas exportadas (.docx)
│   └── figuras/              # Gráficos (.png)
├── outputs/                  # Validações e logs
├── docs/                     # Documentação e auxiliares
└── renv/                     # Dependências isoladas
```

### 5.2 Fluxo de Dados

```
data/raw/dados_primarios.tsv
         │
         ▼
    01_Anonimiza.R ──► data/clean/rds/Dados_anonimizados.rds
         │
         ▼
    02_Padroniza.R ──► data/clean/rds/Dados_padronizados.rds  (SSOT)
         │
         ├──► 05_Valida.R ──► outputs/inconsistencias.tsv
         │
         ▼
    03_Deriva.R ──► data/derived/pacientes.rds
                 ──► data/derived/lesoes.rds
         │
         ▼
    04_Explora.R ──► output/tabelas/*.docx
                  ──► output/figuras/*.png
         │
         ▼
      *.qmd ──► Relatórios interativos
```

### 5.3 Dependências (00_Importa.R)

```r
packages <- c(
    # Manipulação de dados
    "dplyr", "tidyr", "readr", "stringr", "stringi", 
    "tibble", "janitor", "forcats",
    
    # Utilitários
    "digest", "jsonlite", "clipr",
    
    # Tabelas
    "knitr", "kableExtra", "gtsummary", "gt", "flextable",
    
    # Visualização
    "ggplot2", "ggpubr", "scales", "patchwork", "RColorBrewer"
)
```

---

## 6. Especificação dos Scripts

### 6.1 `01_Anonimiza.R`

**Papel:** Anonimiza pacientes de forma determinística usando nomes de plantas.

**Entradas:**
- `data/raw/dados_primarios.tsv`
- `data/raw/padronizacao_ortografia.tsv`
- `data/raw/wfo_plantlist_2025-06/` (taxon.tsv, name.tsv, synonym.tsv)
- Variável de ambiente `ANON_SECRET`

**Processo:**
1. Leitura com detecção de delimitador.
2. Limpeza de células e renomeação posicional.
3. Normalização básica (texto e multi-resposta).
4. Aplicação de dicionário de ortografia.
5. Deduplicação determinística com merge de conflitos.
6. Geração de pool de plantas WFO.
7. Mapeamento paciente → planta via hash + ANON_SECRET.

**Saídas:**
- `data/clean/rds/Dados_anonimizados.rds`
- `data/clean/tsv/plantas.tsv`
- `docs/Plantas.rds`

### 6.2 `02_Padroniza.R`

**Papel:** Padroniza valores textuais usando dicionários e regras clínicas.

**Entradas:**
- `data/clean/rds/Dados_anonimizados.rds`
- `data/raw/padronizacao_comorbidades.tsv`
- `data/raw/padronizacao_histologia.tsv`
- `data/raw/padronizacao_fechamento.tsv`

**Processo:**
1. Expande comorbidades e ordena por `COMORB_COLS_ORDER`.
2. Separa lesões por marcadores de tipo tumoral.
3. Processa cada lesão individualmente (evita mistura de atributos).
4. Ordena lesões por peso (menos atributos primeiro).
5. Padroniza fechamento via dicionário.
6. Loga hash SHA256 do arquivo bruto para auditoria.

**Saída:**
- `data/clean/rds/Dados_padronizados.rds`

### 6.3 `03_Deriva.R`

**Papel:** Gera tabelas analíticas em formato long.

**Entrada:**
- `data/clean/rds/Dados_padronizados.rds`

**Processo:**
1. Extrai tabela de pacientes (1 linha = 1 paciente).
2. Expande lesões para formato long (1 linha = 1 lesão).
3. Aplica regra de expansão:
   - Menos tokens que lesões → repete último.
   - Mais tokens que lesões → agrupa excedentes na última.
4. Replica atributos de cirurgia para todas as lesões da visita.
5. Valida contagem total de lesões.

**Saídas:**
- `data/derived/pacientes.rds`
- `data/derived/lesoes.rds`

### 6.4 `04_Explora.R`

**Papel:** Gera análises descritivas, tabelas e figuras.

**Entradas:**
- `data/derived/pacientes.rds`
- `data/derived/lesoes.rds`

**Saídas:**
- `output/tabelas/table1_pacientes.docx`
- `output/tabelas/table2_lesoes.docx`
- `output/tabelas/table3_bivariada.docx`
- `output/figuras/fig1_demographics.png`
- `output/figuras/fig2_lesoes.png`
- `output/figuras/fig3_localizacao.png`
- `output/figuras/fig4_complicacoes.png`

### 6.5 `05_Valida.R`

**Papel:** Detecta inconsistências nos dados padronizados.

**Entrada:**
- `data/clean/rds/Dados_padronizados.rds`

**Critérios de validação:**
1. Cardinalidade 1:1 com `n_lesoes`.
2. Separadores inválidos (`;`, `,,`, espaços irregulares).
3. Tokens vazios.
4. Domínios inválidos (sexo, fototipo, histo, fech).
5. `n_lesoes` NA/0 com valores preenchidos.
6. `n_lesoes` > 0 com tudo vazio.

**Saída:**
- `outputs/inconsistencias.tsv`

---

## 7. Análise e Apresentação

### 7.1 Análises Planejadas

#### Nível Paciente
- Perfil demográfico: idade, sexo, fototipo.
- Prevalência de comorbidades.
- Distribuição do número de lesões por paciente.

#### Nível Lesão
- Distribuição CBC vs. CEC.
- Subtipos de CBC mais frequentes.
- Comportamento do CEC (in situ vs. invasivo).
- Localização anatômica.
- Técnicas de fechamento utilizadas.

#### Desfechos
- Taxa de complicações (geral e por técnica).
- Taxa de margens comprometidas.
- Fatores associados a complicações (análise bivariada).
- Fatores associados a margens comprometidas.

### 7.2 Considerações Metodológicas

⚠️ **Dados correlacionados:** Pacientes com múltiplas lesões violam independência das observações.

**Estratégias de análise:**
- Análises demográficas: usar tabela de pacientes (1 linha = 1 paciente).
- Análises por lesão: considerar clustering ou usar modelos mistos (GEE, GLMM).
- Alternativa: analisar por "tipo predominante" no paciente.

### 7.3 Apresentação em Quarto (.qmd)

Os documentos `.qmd` devem:
- Consumir `data/derived/*.rds` diretamente.
- Priorizar tabelas interativas (`gt`, `DT`).
- Usar gráficos interativos quando apropriado (`plotly`).
- Manter narrativa epidemiológica clara.

---

## 8. Qualidade e Validação

### 8.1 Princípios

| Princípio | Descrição |
|-----------|-----------|
| **Previsibilidade** | Transformações determinísticas e documentadas |
| **Rastreabilidade** | Entradas/saídas explícitas; hash do dado bruto |
| **Independência** | Scripts não dependem de `.GlobalEnv` |
| **Não-destrutividade** | Nunca perder informação sem documentar |

### 8.2 Checks Obrigatórios

Cada script deve verificar:

- [ ] Número de linhas entrada vs. saída.
- [ ] Número de pacientes únicos preservado.
- [ ] Domínios de variáveis categóricas.
- [ ] Ausência de NAs inesperados.

### 8.3 Relatório de Integridade

Ao final de cada script, logar:

```
=====================================
RESUMO: [nome do script]
=====================================
  Linhas entrada:    N
  Linhas saída:      N
  Pacientes únicos:  N
  Lesões geradas:    N (se aplicável)
  Warnings:          N
=====================================
```

---

## 9. Convenções de Código

### 9.1 Estrutura de Script

```r
# ==============================================================================
# 90gen/scripts/XX_Nome.R
# ==============================================================================
# Papel: [descrição breve]
#
# Entradas:
#   - [arquivo 1]
#   - [arquivo 2]
#
# Saídas:
#   - [arquivo 1]
#
# Autor: Pipeline automatizado
# Data: YYYY-MM-DD
# ==============================================================================

source(file.path("scripts", "00_Importa.R"))

# ==============================================================================
# CONFIGURAÇÃO
# ==============================================================================

# ... código ...
```

### 9.2 Estilo

| Elemento | Convenção |
|----------|-----------|
| Indentação | 4 espaços |
| Atribuição | `<-` |
| Nomes | `snake_case` |
| Seções | `# ==== Nome ====` |
| Logs | `cat()` |
| Diretórios | `dir.create(..., recursive = TRUE, showWarnings = FALSE)` |

### 9.3 Execução

```bash
Rscript scripts/01_Anonimiza.R
Rscript scripts/02_Padroniza.R
Rscript scripts/03_Deriva.R
Rscript scripts/04_Explora.R
Rscript scripts/05_Valida.R
```

---

## 10. Riscos e Pendências

### 10.1 Conhecidos

| Item | Status | Ação |
|------|--------|------|
| `scripts/99.R` desatualizado | ⚠️ | Revisar ou deprecar |
| `output/` vs `outputs/` (inconsistência) | ⚠️ | Padronizar nomenclatura |
| `renv` "out-of-sync" frequente | ⚠️ | Rodar `renv::snapshot()` |
| Path legado em `05_Valida.R` | ⚠️ | Verificar antes de usar |

### 10.2 Regras de Privacidade

- ❌ Nunca commitar dados identificáveis.
- ❌ Nunca commitar `ANON_SECRET`.
- ❌ Não existe mais `patient_to_plant.tsv` persistente.
- ✅ Anonimização é determinística (reprodutível com a mesma secret).

---

## 11. Glossário

### Termos Técnicos

| Termo | Definição |
|-------|-----------|
| **SSOT** | Single Source of Truth — a base "clean" padronizada que serve de origem para tudo |
| **Wide** | Formato com colunas `_1`, `_2` para cada lesão/cirurgia |
| **Long** | Formato com uma linha por lesão/evento |
| **Derived** | Tabelas construídas a partir da SSOT |
| **renv** | Sistema de gerenciamento de dependências R |

### Termos Clínicos

| Termo | Definição |
|-------|-----------|
| **CBC** | Carcinoma Basocelular |
| **CEC** | Carcinoma Espinocelular (ou Carcinoma de Células Escamosas) |
| **CPNM** | Câncer de Pele Não-Melanoma |
| **Mohs** | Cirurgia micrográfica de Mohs (remoção controlada por margens) |
| **In situ** | Tumor confinado à epiderme, não invasivo |
| **Invasivo** | Tumor que atravessou a membrana basal |
| **Margens livres** | Bordas da peça cirúrgica sem tumor |
| **Margens comprometidas** | Tumor presente nas bordas (risco de recidiva) |
| **Retalho** | Técnica de fechamento com pele adjacente vascularizada |
| **Enxerto** | Técnica de fechamento com pele de área doadora |
| **Fototipo** | Classificação de Fitzpatrick (I-VI) para cor de pele |

---

## Histórico de Versões

| Versão | Data | Mudanças |
|--------|------|----------|
| 1.0 | 2025-01-21 | Versão inicial (PRD.md + PRD2.md separados) |
| 2.0 | 2025-01-23 | Unificação, contexto clínico, atualização da lógica de expansão |

---

*Este documento é a fonte de verdade para o projeto 90gen. Atualize-o sempre que houver mudanças significativas no pipeline ou nas regras de negócio.*
