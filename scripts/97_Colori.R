# ============================================================================
# 🎨 colori.R - Canivete suíço de cores
# ============================================================================
# Autor: Wiliam + Claudinho
# Uso:
#   source("R/colori.R")
#   hue("azul")                   → "#4285F4"
#   hue("azul", "rosa")           → c("#4285F4", "#D6A3CC")
#   hue("vermelho10")             → vermelho saturado
#   hue("vermelho5")              → vermelho médio
#   hue("azul claro")             → azul clarinho
#   hue("marinho")                → azul escuro (sinônimo)
#   hue("verde bem escuro")       → verde com intensidade máxima
#   hue("azul", ph = 0.5)      → "#4285F480" (com transparência)
#   hue_show("azul", n = TRUE)    → mostra gradiente 0-10
# ============================================================================

# ============================================================================
# FUNÇÃO PRINCIPAL
# ============================================================================

#' Retorna código(s) hexadecimal de cor(es)
#'
#' @param ... Nomes de cores (strings). Aceita modificadores e intensidades.
#' @param ph Transparência (0 = invisível, 1 = opaco). Default = 1.
#' @return Vetor de strings hexadecimais (ou string única se só uma cor)
#' @examples
#' hue("azul")
#' hue("azul", "rosa", "verde")
#' hue("vermelho7")
#' hue("azul bem claro")
#' hue("marinho")
#' hue("azul", ph = 0.5)
#' hue("azul", "verde", ph = 0.8)
hue <- function(..., ph = 1) {
  args <- c(...)
  if (length(args) == 0) return(NULL)

  # Validar ph
  ph <- pmax(0, pmin(1, ph))

  cores <- vapply(args, .hue_parse, FUN.VALUE = character(1), USE.NAMES = FALSE)

  # Aplicar ph se necessário
  if (ph < 1) {
    cores <- vapply(cores, function(hex) .hue_add_ph(hex, ph),
                    FUN.VALUE = character(1), USE.NAMES = FALSE)
  }

  if (length(cores) == 1) cores[[1]] else cores
}

# ============================================================================
# DICIONÁRIOS
# ============================================================================

# --- Cores base (intensidade 7 = "padrão") ---
.hue_base <- c(
  # Primárias
  azul      = "#4285F4",
  vermelho  = "#E63946",
  amarelo   = "#F4B400",
  verde     = "#2A9D8F",

  # Secundárias
  rosa      = "#D6A3CC",
  roxo      = "#7B68EE",
  laranja   = "#E76F51",
  ciano     = "#00BCD4",

  # Neutras
  cinza     = "#808080",
  preto     = "#1A1A1A",
  branco    = "#FAFAFA",

  # Extras úteis
  marrom    = "#8B4513",
  bege      = "#D4C4A8",
  dourado   = "#DAA520",
  prata     = "#C0C0C0"
)

# --- Sinônimos (mapeiam para "cor + intensidade implícita") ---
.hue_sinonimos <- c(
  # Azuis
  marinho       = "azul10",
  naval         = "azul10",
  celeste       = "azul3",
  "azul bebê"   = "azul2",
  royal         = "azul8",
  turquesa      = "ciano6",
  "azul petróleo" = "ciano9",
  petróleo      = "ciano9",

  # Vermelhos
  vinho         = "vermelho9",
  bordô         = "vermelho10",
  salmão        = "vermelho3",
  coral         = "laranja5",
  cereja        = "vermelho8",
  carmim        = "vermelho10",
  rubi          = "vermelho8",
  tijolo        = "vermelho7",

  # Verdes
  musgo         = "verde9",
  oliva         = "verde8",
  menta         = "verde3",
  esmeralda     = "verde7",
  "verde limão" = "verde4",
  floresta      = "verde10",
  abacate       = "verde6",
  militar       = "verde8",

  # Amarelos
  mostarda      = "amarelo8",
  canário       = "amarelo5",
  creme         = "amarelo2",
  ouro          = "dourado7",
  mel           = "amarelo6",
  palha         = "amarelo3",
  banana        = "amarelo5",

  # Roxos
  lavanda       = "roxo3",
  violeta       = "roxo7",
  lilás         = "roxo4",
  berinjela     = "roxo10",
  ameixa        = "roxo9",
  uva           = "roxo8",
  púrpura       = "roxo8",

  # Rosas
  chiclete      = "rosa5",
  magenta       = "rosa9",
  fúcsia        = "rosa8",
  nude          = "rosa2",
  blush         = "rosa3",
  "rosa bebê"   = "rosa2",
  cerejeira     = "rosa6",

  # Laranjas
  terracota     = "laranja8",
  pêssego       = "laranja3",
  tangerina     = "laranja7",
  ferrugem      = "laranja9",
  abóbora       = "laranja6",
  damasco       = "laranja4",

  # Marrons
  chocolate     = "marrom8",
  café          = "marrom9",
  caramelo      = "marrom5",
  areia         = "marrom3",
  castanho      = "marrom7",
  tabaco        = "marrom8",
  mogno         = "marrom9",

  # Neutros especiais
  carvão        = "cinza9",
  grafite       = "cinza8",
  pérola        = "cinza2",
  fumê          = "cinza7",
  gelo          = "branco",
  neve          = "branco",
  noite         = "preto",
  ônix          = "preto"
)

# --- Modificadores de intensidade ---
.hue_modificadores <- list(
  # Clareadores (reduzem intensidade) - ordenados do mais específico pro mais genérico
  "super claro"   = -6,
  "muito claro"   = -5,
  "bem claro"     = -4,
  "clarinho"      = -4,
  "claro"         = -3,

  # Escurecedores (aumentam intensidade)
  "super escuro"  = +5,
  "muito escuro"  = +4,
  "bem escuro"    = +3,
  "escurinho"     = +2,
  "escuro"        = +2,

  # Intensificadores genéricos
  "super forte"   = +4,
  "bem forte"     = +3,
  "forte"         = +2,
  "intenso"       = +3,
  "vibrante"      = +2,
  "vivo"          = +2,
  "saturado"      = +2,
  "neon"          = +3,
  "profundo"      = +3,

  # Suavizadores
  "bem suave"     = -4,
  "apagado"       = -2,
  "suave"         = -2,
  "pastel"        = -4,
  "fosco"         = -1,
  "pálido"        = -3,
  "desbotado"     = -3,
  "acinzentado"   = -2
)

# ============================================================================
# FUNÇÕES INTERNAS
# ============================================================================

# --- Adicionar ph ao hex ---
.hue_add_ph <- function(hex, ph) {
  # Converte ph (0-1) pra hex (00-FF)
  ph_hex <- sprintf("%02X", round(ph * 255))
  paste0(hex, ph_hex)
}

# --- Parser principal ---
.hue_parse <- function(x) {
  x <- tolower(trimws(x))

  # 1. Verificar sinônimos diretos primeiro (ex: "marinho", "salmão")
  if (x %in% names(.hue_sinonimos)) {
    x <- .hue_sinonimos[[x]]
  }

  # 2. Tentar extrair intensidade numérica (ex: "azul7", "vermelho10")
  match_num <- regmatches(x, regexec("^([a-záéíóúãõç]+)(\\d+)$", x))[[1]]
  if (length(match_num) == 3) {
    cor_base <- match_num[2]
    intensidade <- as.numeric(match_num[3])
    return(.hue_aplicar(cor_base, intensidade))
  }

  # 3. Tentar extrair modificadores textuais (ex: "azul claro", "bem escuro")
  parsed <- .hue_parse_modificadores(x)
  if (!is.null(parsed)) {
    return(.hue_aplicar(parsed$cor, parsed$intensidade))
  }

  # 4. Cor simples sem modificador
  if (x %in% names(.hue_base)) {
    return(.hue_base[[x]])
  }

  # 5. Fallback: cinza + warning
  warning(sprintf("hue(): cor '%s' nao reconhecida, usando cinza", x))
  "#808080"
}

# --- Parser de modificadores textuais ---
.hue_parse_modificadores <- function(x) {
  # Ordenar modificadores por tamanho (maior primeiro) pra evitar match parcial
  mods <- names(.hue_modificadores)
  mods <- mods[order(nchar(mods), decreasing = TRUE)]

  intensidade_delta <- 0

  for (mod in mods) {
    if (grepl(mod, x, fixed = TRUE)) {
      intensidade_delta <- intensidade_delta + .hue_modificadores[[mod]]
      x <- trimws(gsub(mod, "", x, fixed = TRUE))
    }
  }

  # O que sobrou deve ser a cor base
  x <- trimws(x)

  # Checar se é sinônimo
  if (x %in% names(.hue_sinonimos)) {
    x <- .hue_sinonimos[[x]]
  }

  # Tentar extrair número já presente (de sinônimo expandido, ex: "azul10")
  match_num <- regmatches(x, regexec("^([a-záéíóúãõç]+)(\\d+)$", x))[[1]]
  if (length(match_num) == 3) {
    cor_base <- match_num[2]
    intensidade_base <- as.numeric(match_num[3])
    intensidade_final <- pmax(0, pmin(10, intensidade_base + intensidade_delta))
    return(list(cor = cor_base, intensidade = intensidade_final))
  }

  # Checar se é cor base
  if (x %in% names(.hue_base)) {
    # Intensidade padrão é 7, ajustar com delta
    intensidade_final <- pmax(0, pmin(10, 7 + intensidade_delta))
    return(list(cor = x, intensidade = intensidade_final))
  }

  NULL
}

# --- Aplicar intensidade (0 = claríssimo, 10 = saturadíssimo) ---
.hue_aplicar <- function(cor_nome, intensidade) {
  intensidade <- pmax(0, pmin(10, intensidade))

  # Verificar se é sinônimo que precisa ser resolvido
  if (cor_nome %in% names(.hue_sinonimos)) {
    resolved <- .hue_sinonimos[[cor_nome]]
    match_num <- regmatches(resolved, regexec("^([a-záéíóúãõç]+)(\\d+)$", resolved))[[1]]
    if (length(match_num) == 3) {
      cor_nome <- match_num[2]
    } else {
      cor_nome <- resolved
    }
  }

  if (!cor_nome %in% names(.hue_base)) {
    warning(sprintf("hue(): cor base '%s' nao encontrada", cor_nome))
    return("#808080")
  }

  cor_hex <- .hue_base[[cor_nome]]

  # Converter hex para RGB
  rgb_vals <- col2rgb(cor_hex)[, 1]

  # Interpolar:
  # intensidade 0  → quase branco (RGB 250, 250, 250)
  # intensidade 7  → cor original
  # intensidade 10 → cor saturada/escurecida

  if (intensidade <= 7) {
    # Clarear: interpolar entre branco e cor original
    t <- intensidade / 7
    branco <- c(250, 250, 250)
    rgb_final <- round(branco * (1 - t) + rgb_vals * t)
  } else {
    # Escurecer: interpolar entre cor original e versão escura
    t <- (intensidade - 7) / 3
    # Escurecer reduzindo luminosidade (multiplicar por fator)
    fator <- 1 - (t * 0.4)
    rgb_final <- round(rgb_vals * fator)
  }

  rgb_final <- pmax(0, pmin(255, rgb_final))

  sprintf("#%02X%02X%02X", rgb_final[1], rgb_final[2], rgb_final[3])
}

# ============================================================================
# UTILITÁRIOS
# ============================================================================

#' Mostra paleta de cores visualmente
#'
#' @param ... Nomes de cores para mostrar
#' @param n Se TRUE e só uma cor foi passada, mostra gradiente 0-10
#' @return Vetor de cores (invisível)
#' @examples
#' hue_show("azul", "verde", "vermelho")
#' hue_show("azul", n = TRUE)
hue_show <- function(..., n = FALSE) {
  args <- c(...)

  if (n && length(args) == 1) {
    # Mostrar gradiente de uma cor
    cor <- args[[1]]
    cores <- vapply(0:10, function(i) hue(paste0(cor, i)), character(1))
    names(cores) <- paste0(cor, 0:10)
  } else {
    cores <- hue(...)
    names(cores) <- args
  }

  # Plot simples
  n_cores <- length(cores)
  oldpar <- par(mar = c(3, 0.5, 2, 0.5))
  on.exit(par(oldpar))

  barplot(
    rep(1, n_cores),
    col = cores,
    names.arg = names(cores),
    las = 2,
    main = "hue()",
    border = NA,
    yaxt = "n",
    cex.names = 0.8
  )

  # Mostrar hex embaixo
  mtext(cores, side = 1, at = seq(0.7, by = 1.2, length.out = n_cores),
        line = 2, cex = 0.6, col = "gray40")

  invisible(cores)
}

#' Lista todas as cores disponíveis
#'
#' @param tipo "base", "sinonimos", ou "todos"
#' @return Data frame com nomes e hex
hue_lista <- function(tipo = "todos") {
  resultado <- list()

  if (tipo %in% c("base", "todos")) {
    resultado$base <- data.frame(
      nome = names(.hue_base),
      hex = unname(.hue_base),
      tipo = "base",
      stringsAsFactors = FALSE
    )
  }

  if (tipo %in% c("sinonimos", "todos")) {
    resultado$sinonimos <- data.frame(
      nome = names(.hue_sinonimos),
      mapeia_para = unname(.hue_sinonimos),
      tipo = "sinonimo",
      stringsAsFactors = FALSE
    )
  }

  if (tipo == "todos") {
    cat("=== CORES BASE ===\n")
    print(resultado$base, row.names = FALSE)
    cat("\n=== SINÔNIMOS ===\n")
    print(resultado$sinonimos, row.names = FALSE)
    invisible(resultado)
  } else if (tipo == "base") {
    resultado$base
  } else {
    resultado$sinonimos
  }
}

#' Adiciona cor customizada ao dicionário (sessão atual)
#'
#' @param nome Nome da cor
#' @param hex Código hexadecimal
#' @examples
#' hue_add("minhacor", "#FF5733")
#' hue("minhacor")
hue_add <- function(nome, hex) {
  nome <- tolower(trimws(nome))
  # Validar hex
  if (!grepl("^#[0-9A-Fa-f]{6}$", hex)) {
    stop("Hex inválido. Use formato #RRGGBB")
  }
  .hue_base[[nome]] <<- hex
  message(sprintf("Cor '%s' (%s) adicionada!", nome, hex))
  invisible(hex)
}

#' Adiciona sinônimo customizado (sessão atual)
#'
#' @param sinonimo Nome do sinônimo
#' @param mapeia_para Cor + intensidade (ex: "azul8")
#' @examples
#' hue_sinonimo("oceano", "azul6")
#' hue("oceano")
hue_sinonimo <- function(sinonimo, mapeia_para) {
  sinonimo <- tolower(trimws(sinonimo))
  mapeia_para <- tolower(trimws(mapeia_para))
  .hue_sinonimos[[sinonimo]] <<- mapeia_para
  message(sprintf("Sinônimo '%s' -> '%s' adicionado!", sinonimo, mapeia_para))
  invisible(mapeia_para)
}

# ============================================================================
# FIM
# ============================================================================
# Dica: no seu .qmd, use assim:
#
# ```{r setup}
# source(file.path(renv_root, "R", "colori.R"))
#
# # Agora pode usar em qualquer gráfico:
# ggplot(...) +
#   scale_fill_manual(values = hue("azul", "vermelho", "verde"))
#
# # Com transparência:
# geom_col(fill = hue("azul", ph = 0.8))
# ```
# ============================================================================



cor <- c(
    # Identidade visual (autora): azul claro + azul escuro
    cbc = hue("azul4"),
    cec = hue("azul7"),
    visita1 = hue("celeste", ph = 0.75),
    visita2 = hue("marinho", ph = 0.75),
    livres = hue("celeste"),
    comprometidas = hue("marinho"),
    
    # Neutros (também via hue)
    branco = hue("branco"),
    texto = hue("preto", ph = 0.88),
    texto_sutil = hue("cinza9"),
    
    # Gradiente do heatmap (ainda dentro da identidade azul claro/escuro)
    cor_heatmap_low  <- "#00FF00",
    cor_heatmap_high <- "#0000FF"
)

















