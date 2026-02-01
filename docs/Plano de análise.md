# Plano de Análise – Estudo de Exérese de Câncer de Pele

## Objetivo geral
Descrever as características dos pacientes e das lesões submetidas à exérese cirúrgica de câncer de pele, bem como os desfechos imediatos associados ao procedimento, sem realização de análises inferenciais nesta etapa.

Este plano refere-se exclusivamente à **análise descritiva**, com foco exploratório e epidemiológico, servindo de base para formulação de hipóteses e definição das análises subsequentes.

---

## Premissas metodológicas

- A base de dados utilizada já foi submetida a:
  - normalização;
  - padronização semântica;
  - tipagem explícita das variáveis;
  - anonimização reprodutível.
- Não haverá modificação do dataset original durante esta etapa.
- Todas as saídas geradas terão caráter descritivo e serão armazenadas em diretório específico de outputs.

---

## Unidades de análise

### Unidade paciente
Indivíduo único identificado pela variável `paciente`.  
Cada paciente pode apresentar uma ou mais lesões.

### Unidade lesão
Evento cirúrgico individual correspondente a uma lesão cutânea específica.  
Um mesmo paciente pode contribuir com mais de uma observação neste nível.

A separação entre níveis de análise é fundamental para evitar interpretações estatísticas inadequadas.

---

## Análise descritiva – Nível paciente

### Variáveis incluídas
- Idade
- Sexo
- Fototipo
- Presença de comorbidades
- Histórico prévio de CBC/CEC
- Realização de cirurgia de Mohs
- Histórico de cirurgias prévias adicionais
- Óbito

### Objetivos
- Caracterizar o perfil demográfico e clínico da população estudada.
- Fornecer uma visão geral da carga clínica dos pacientes.

### Produtos esperados
- Tabela descritiva (Tabela 1) com medidas de tendência central e dispersão para variáveis contínuas e frequências absolutas e relativas para variáveis categóricas.
- Distribuição gráfica da idade.

---

## Quantificação da carga de lesões por paciente

### Variáveis
- Número de lesões por paciente

### Objetivos
- Avaliar a proporção de pacientes com múltiplas lesões.
- Identificar a complexidade global da população estudada.

### Produtos esperados
- Distribuição do número de lesões por paciente.
- Frequência de pacientes com uma versus múltiplas lesões.

---

## Construção de dataset derivado no nível lesão

Será criado um objeto derivado em formato longo (long format), no qual:
- cada linha representa uma lesão;
- a variável `paciente` é mantida como identificador;
- apenas variáveis intrínsecas à lesão são incluídas.

Este objeto será utilizado exclusivamente para fins descritivos e exploratórios.

---

## Análise descritiva – Nível lesão

### Biologia tumoral
- Tipo histológico (CBC, CEC)
- Subtipo histológico
- Grau de diferenciação
- Comportamento tumoral

### Topografia
- Localização das lesões

### Conduta cirúrgica
- Tipo de fechamento
- Subtipo de fechamento

### Desfechos imediatos
- Margens cirúrgicas comprometidas
- Complicações pós-operatórias
- Reintervenções cirúrgicas

### Objetivos
- Caracterizar o perfil das lesões tratadas.
- Descrever a distribuição dos desfechos cirúrgicos.

### Produtos esperados
- Tabelas de frequência absolutas e relativas.
- Gráficos descritivos simples, quando apropriado.

---

## Verificações de consistência clínica

Durante a análise exploratória, serão realizadas verificações qualitativas para identificar:
- combinações biologicamente implausíveis;
- frequências inesperadas de eventos;
- possíveis inconsistências remanescentes nos dados.

Este processo tem caráter clínico-epidemiológico e não estatístico.

---

## Encerramento da etapa descritiva

Ao final da análise descritiva, serão registradas:
- perguntas clínicas e epidemiológicas emergentes;
- sugestões de variáveis derivadas;
- potenciais estratificações relevantes.

Esses elementos servirão de base para a construção dos scripts de derivação de variáveis e análise inferencial subsequentes.

---