
# Bibliotecas

library(shiny)
library(bslib)
library(bsicons)
library(maps)
library(dplyr)
library(ggplot2)
library(ggrepel)


# Aba "Desempenho por seleção" do aplicativo Shiny

raiz_projeto <- if (file.exists("01_read_data.R") && file.exists("../data/raw")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else if (file.exists("code/01_read_data.R") || file.exists("app.R") || file.exists("project.Rproj")) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  stop("Não foi possível localizar a raiz do projeto.")
}

dir_anterior <- getwd()
setwd(raiz_projeto)
on.exit(setwd(dir_anterior), add = TRUE)

source("code/01_read_data.R")
source("code/aba_gols.R", encoding = "UTF-8")
source("code/aba_podio.R", encoding = "UTF-8")

wc_raw <- load_world_cup_data("data/raw")
wc_df <- wc_raw$worldcups
m_df <- wc_raw$wcmatches
m_df$stage <- trimws(as.character(m_df$stage))

traduzir_selecao <- function(x) {
  mapa <- c(
    "Algeria" = "Argélia",
    "Angola" = "Angola",
    "Argentina" = "Argentina",
    "Australia" = "Austrália",
    "Austria" = "Áustria",
    "Belgium" = "Bélgica",
    "Bolivia" = "Bolívia",
    "Bosnia and Herzegovina" = "Bósnia e Herzegovina",
    "Brazil" = "Brasil",
    "Bulgaria" = "Bulgária",
    "Cameroon" = "Camarões",
    "Canada" = "Canadá",
    "Chile" = "Chile",
    "China PR" = "China",
    "Colombia" = "Colômbia",
    "Costa Rica" = "Costa Rica",
    "Croatia" = "Croácia",
    "Cuba" = "Cuba",
    "Czech Republic" = "República Tcheca",
    "Czechoslovakia" = "Tchecoslováquia",
    "Denmark" = "Dinamarca",
    "Dutch West Indies" = "Índias Orientais Holandesas",
    "Ecuador" = "Equador",
    "Egypt" = "Egito",
    "El Salvador" = "El Salvador",
    "England" = "Inglaterra",
    "France" = "França",
    "Germany" = "Alemanha",
    "East Germany" = "Alemanha Oriental",
    "Germany DR" = "Alemanha Oriental",
    "Ghana" = "Gana",
    "Greece" = "Grécia",
    "Haiti" = "Haiti",
    "Honduras" = "Honduras",
    "Hungary" = "Hungria",
    "Iceland" = "Islândia",
    "Iran" = "Irã",
    "Iraq" = "Iraque",
    "Israel" = "Israel",
    "Italy" = "Itália",
    "Ivory Coast" = "Costa do Marfim",
    "Jamaica" = "Jamaica",
    "Japan" = "Japão",
    "Kuwait" = "Kuwait",
    "Mexico" = "México",
    "Morocco" = "Marrocos",
    "Netherlands" = "Países Baixos",
    "New Zealand" = "Nova Zelândia",
    "Nigeria" = "Nigéria",
    "North Korea" = "Coreia do Norte",
    "Northern Ireland" = "Irlanda do Norte",
    "Norway" = "Noruega",
    "Panama" = "Panamá",
    "Paraguay" = "Paraguai",
    "Peru" = "Peru",
    "Poland" = "Polônia",
    "Portugal" = "Portugal",
    "Republic of Ireland" = "Irlanda",
    "FR Yugoslavia" = "RF da Iugoslávia",
    "Romania" = "Romênia",
    "Russia" = "Rússia",
    "Saudi Arabia" = "Arábia Saudita",
    "Scotland" = "Escócia",
    "Senegal" = "Senegal",
    "Serbia" = "Sérvia",
    "Serbia and Montenegro" = "Sérvia e Montenegro",
    "Slovakia" = "Eslováquia",
    "Slovenia" = "Eslovênia",
    "South Africa" = "África do Sul",
    "South Korea" = "Coreia do Sul",
    "Soviet Union" = "União Soviética",
    "Spain" = "Espanha",
    "Sweden" = "Suécia",
    "Switzerland" = "Suíça",
    "Togo" = "Togo",
    "Trinidad and Tobago" = "Trinidad e Tobago",
    "Tunisia" = "Tunísia",
    "Turkey" = "Turquia",
    "Ukraine" = "Ucrânia",
    "United Arab Emirates" = "Emirados Árabes Unidos",
    "United States" = "Estados Unidos",
    "Uruguay" = "Uruguai",
    "Wales" = "País de Gales",
    "West Germany" = "Alemanha Ocidental",
    "Yugoslavia" = "Iugoslávia",
    "Zaire" = "Zaire"
  )
  out <- unname(mapa[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

url_bandeira_selecao <- function(selecao) {
  valores <- c(
    "Algeria" = "dz",
    "Angola" = "ao",
    "Argentina" = "ar",
    "Australia" = "au",
    "Austria" = "at",
    "Belgium" = "be",
    "Bolivia" = "bo",
    "Bosnia and Herzegovina" = "ba",
    "Brazil" = "br",
    "Bulgaria" = "bg",
    "Cameroon" = "cm",
    "Canada" = "ca",
    "Chile" = "cl",
    "China PR" = "cn",
    "Colombia" = "co",
    "Costa Rica" = "cr",
    "Croatia" = "hr",
    "Cuba" = "cu",
    "Czech Republic" = "cz",
    "Denmark" = "dk",
    "Ecuador" = "ec",
    "Egypt" = "eg",
    "El Salvador" = "sv",
    "England" = "gb-eng",
    "France" = "fr",
    "Germany" = "de",
    "Ghana" = "gh",
    "Greece" = "gr",
    "Haiti" = "ht",
    "Honduras" = "hn",
    "Hungary" = "hu",
    "Iceland" = "is",
    "Iran" = "ir",
    "Iraq" = "iq",
    "Israel" = "il",
    "Italy" = "it",
    "Ivory Coast" = "ci",
    "Jamaica" = "jm",
    "Japan" = "jp",
    "Kuwait" = "kw",
    "Mexico" = "mx",
    "Morocco" = "ma",
    "Netherlands" = "nl",
    "New Zealand" = "nz",
    "Nigeria" = "ng",
    "North Korea" = "kp",
    "Northern Ireland" = "gb-nir",
    "Norway" = "no",
    "Panama" = "pa",
    "Paraguay" = "py",
    "Peru" = "pe",
    "Poland" = "pl",
    "Portugal" = "pt",
    "Republic of Ireland" = "ie",
    "Romania" = "ro",
    "Russia" = "ru",
    "Saudi Arabia" = "sa",
    "Scotland" = "gb-sct",
    "Senegal" = "sn",
    "Serbia" = "rs",
    "Slovakia" = "sk",
    "Slovenia" = "si",
    "South Africa" = "za",
    "South Korea" = "kr",
    "Spain" = "es",
    "Sweden" = "se",
    "Switzerland" = "ch",
    "Togo" = "tg",
    "Trinidad and Tobago" = "tt",
    "Tunisia" = "tn",
    "Turkey" = "tr",
    "Ukraine" = "ua",
    "United Arab Emirates" = "ae",
    "United States" = "us",
    "Uruguay" = "uy",
    "Wales" = "gb-wls",
    "West Germany" = "de",
    "Soviet Union" = "https://commons.wikimedia.org/wiki/Special:FilePath/Flag_of_the_Soviet_Union.svg",
    "Czechoslovakia" = "https://commons.wikimedia.org/wiki/Special:FilePath/Flag_of_Czechoslovakia.svg",
    "Yugoslavia" = "https://commons.wikimedia.org/wiki/Special:FilePath/Flag_of_SFR_Yugoslavia.svg",
    "FR Yugoslavia" = "https://commons.wikimedia.org/wiki/Special:FilePath/Flag_of_Serbia_and_Montenegro_(1992%E2%80%932006).svg",
    "Dutch West Indies" = "https://flagcdn.com/w160/id.png",
    "East Germany" = "https://upload.wikimedia.org/wikipedia/commons/a/a1/Flag_of_East_Germany.svg",
    "Zaire" = "https://commons.wikimedia.org/wiki/Special:FilePath/Flag_of_Zaire.svg"
  )
  
  val <- unname(valores[selecao])
  if (is.na(val) || !nzchar(val)) {
    return(NULL)
  }
  if (grepl("^https?://", val)) {
    return(val)
  }
  paste0("https://flagcdn.com/w160/", val, ".png")
}

selecoes_originais <- sort(unique(c(m_df$home_team, m_df$away_team)))
selecoes_pt <- traduzir_selecao(selecoes_originais)
choices_selecao <- setNames(selecoes_originais, selecoes_pt)

colocacao_selecao_ano <- function(selecao, ano) {
  linha_wc <- wc_df[wc_df$year == ano, , drop = FALSE]
  participou_jogos <- any(
    (m_df$year == ano) & (m_df$home_team == selecao | m_df$away_team == selecao)
  )
  colocacao_top4 <- NA_character_
  if (nrow(linha_wc) > 0) {
    if (identical(linha_wc$winner[1], selecao)) colocacao_top4 <- "Campeão"
    else if (identical(linha_wc$second[1], selecao)) colocacao_top4 <- "Vice-campeão"
    else if (identical(linha_wc$third[1], selecao)) colocacao_top4 <- "3º lugar"
    else if (identical(linha_wc$fourth[1], selecao)) colocacao_top4 <- "4º lugar"
  }
  if (!participou_jogos && is.na(colocacao_top4)) {
    return(list(participou = FALSE, colocacao = "Não participou", destaque = "ausente"))
  }
  rot_col <- if (!is.na(colocacao_top4)) colocacao_top4 else "Participou (fora do top 4)"
  destaque_tipo <- if (!is.na(colocacao_top4)) colocacao_top4 else "participou"
  list(participou = TRUE, colocacao = rot_col, destaque = destaque_tipo)
}

calcular_desempenho <- function(selecao) {
  sub_m <- m_df[(m_df$home_team == selecao) | (m_df$away_team == selecao), , drop = FALSE]
  partidas <- nrow(sub_m)
  vitorias <- sum(sub_m$winning_team == selecao, na.rm = TRUE)
  derrotas <- sum(sub_m$losing_team == selecao, na.rm = TRUE)
  empates <- sum(sub_m$outcome == "D", na.rm = TRUE)
  
  gols_pro <- sum(ifelse(sub_m$home_team == selecao, sub_m$home_score, sub_m$away_score), na.rm = TRUE)
  gols_contra <- sum(ifelse(sub_m$home_team == selecao, sub_m$away_score, sub_m$home_score), na.rm = TRUE)
  
  anos_jogados <- unique(sub_m$year)
  anos_top4 <- wc_df$year[
    wc_df$winner == selecao | wc_df$second == selecao |
      wc_df$third == selecao | wc_df$fourth == selecao
  ]
  total_participacoes <- length(unique(c(anos_jogados, anos_top4)))
  
  data.frame(
    "Seleção" = traduzir_selecao(selecao),
    "Participações" = as.integer(total_participacoes),
    "Partidas" = as.integer(partidas),
    "Vitórias" = as.integer(vitorias),
    "Empates" = as.integer(empates),
    "Derrotas" = as.integer(derrotas),
    "Gols pró" = as.integer(gols_pro),
    "Gols contra" = as.integer(gols_contra),
    "Saldo de gols" = as.integer(gols_pro - gols_contra),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

decadas_lista <- lapply(seq(1930, 2010, by = 10), function(d) {
  anos_d <- sort(wc_df$year[wc_df$year >= d & wc_df$year <= d + 9])
  list(
    inicio = d,
    rotulo = paste0(d, " a ", d + 9),
    anos = anos_d
  )
})

#============================================================================
# Interface do usuário (UI) do aplicativo Shiny (Painel Desempenho das seleções)
#============================================================================

ui <- page_navbar(
  title = "Estatísticas das Copas do Mundo de Futebol: 1930-2018",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  #============================================================================
  nav_panel(
    title = "Desempenho por seleção",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        selectInput(
          inputId = "selecao_escolhida",
          label = "Selecione a seleção:",
          choices = choices_selecao,
          selected = if ("Brazil" %in% selecoes_originais) "Brazil" else selecoes_originais[1]
        ),
        value_box(
          title = div(style = "text-align: center; width: 100%;", "Participações totais"),
          value = div(style = "text-align: center; width: 100%;", textOutput("total_participacoes_box", inline = TRUE)),
          showcase = bsicons::bs_icon("trophy", size = "1.75rem"),
          showcase_layout = "left center",
          fill = FALSE
        ),
        card(
          fill = FALSE,
          class = "mt-2 text-center",
          card_body(
            style = "padding: 0.9rem;",
            uiOutput("bandeira_selecao_ui")
          )
        )
      ),
      card(
        fill = FALSE,
        card_header("Histórico de desempeho em todas as participações"),
        card_body(
          style = "overflow-x: auto; padding: 0.85rem;",
          div(
            style = "font-size: 0.95rem; white-space: nowrap; font-variant-numeric: tabular-nums;",
            tags$style(HTML("
              #tabela_desempenho th:not(:first-child),
              #tabela_desempenho td:not(:first-child) {
                text-align: center !important;
                vertical-align: middle;
              }
              #tabela_desempenho th:first-child,
              #tabela_desempenho td:first-child {
                text-align: left !important;
                vertical-align: middle;
              }
            ")),
            tableOutput("tabela_desempenho")
          )
        )
      ),
      card(
        fill = FALSE,
        card_header("Participação e colocação por década e ano de Copa"),
        card_body(
          style = "padding: 0.85rem;",
          uiOutput("grade_decadas_ui")
        )
      )
    )
  ),

  # Interface do usuário (UI) do aplicativo Shiny (Painel País sede e público)
  #===========================================================================
  nav_panel(
    "Países, cidades sedes e Jogos",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        selectInput(
          "edicao_sede",
          "Edição da Copa (Ano - País-Sede):",
          choices = NULL
        ),
        uiOutput("bandeiras_sede_ui"),
        hr(),
        p(strong("Filtrar por Cidade-Sede:"), class = "mb-1"),
        div(
          style = "max-height: 380px; overflow-y: auto;",
          radioButtons(
            "cidade_sede_filtro",
            label = NULL,
            choices = c("Todas as cidades" = "TODAS"),
            selected = "TODAS"
          )
        )
      ),
      
      # Linha 1: Público Total + cards fixos (Jogos, Seleções, Sedes) + Sede
      
      layout_columns(
        col_widths = c(3, 2, 2, 2, 3),
        value_box(
          title = div(
            style = "text-align: center; width: 100%; font-size: 1.00rem; font-weight: 500; opacity: 0.85;",
            "Público Total na edição da Copa"
          ),
          value = div(
            style = "text-align: center; width: 100%; font-size: 2.5rem; font-weight: 700; line-height: 1.15;",
            textOutput("kpi_publico_edicao")
          ),
          showcase = bs_icon("people-fill"),
          theme = "primary",
          max_height = "120px"
        ),
        value_box(
          title = div(
            style = "text-align: center; width: 100%; font-size: 1.0rem; font-weight: 500; opacity: 0.85;",
            "Total de Jogos"
          ),
          value = div(
            style = "text-align: center; width: 100%; font-size: 2.5rem; font-weight: 700; line-height: 1.15;",
            textOutput("kpi_total_jogos")
          ),
          theme = "secondary",
          max_height = "120px"
        ),
        value_box(
          title = div(
            style = "text-align: center; width: 100%; font-size: 1.0rem; font-weight: 500; opacity: 0.85;",
            "Total de Seleções"
          ),
          value = div(
            style = "text-align: center; width: 100%; font-size: 2.5rem; font-weight: 700; line-height: 1.15;",
            textOutput("kpi_total_selecoes")
          ),
          theme = "secondary",
          max_height = "120px"
        ),
        value_box(
          title = div(
            style = "text-align: center; width: 100%; font-size: 1.0rem; font-weight: 500; opacity: 0.85;",
            "Número de Sedes"
          ),
          value = div(
            style = "text-align: center; width: 100%; font-size: 2.5rem; font-weight: 700; line-height: 1.15;",
            textOutput("kpi_num_sedes")
          ),
          theme = "secondary",
          max_height = "120px"
        ),
        value_box(
          title = div(
            style = "text-align: center; width: 100%; font-size: 1.00rem; font-weight: 500; opacity: 0.85;",
            textOutput("titulo_kpi_cidade")
          ),
          value = div(
            style = "text-align: center; width: 100%; line-height: 1.0;",
            div(textOutput("kpi_publico_cidade"), style = "font-size: 2.5rem; font-weight: 700;"),
            div(textOutput("sub_kpi_cidade"), style = "font-size: 1.0rem; opacity: 0.85; margin-top: 2px;")
          ),
          showcase = bs_icon("geo-alt-fill"),
          theme = "info",
          max_height = "120px"
        )
      ),
      # Linha 2: Mapa das cidades-sede + Tabela de partidas
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Mapa das Cidades-Sede"),
          plotOutput("mapa_sedes", height = "480px")
        ),
        card(
          card_header("Partidas, Fases e Placares"),
          div(
            style = "max-height: 480px; overflow-y: auto;",
            tableOutput("tabela_jogos_sede")
          )
        )
      )
    )
  ),
  
# Interface do usuário (UI) do aplicativo Shiny (Desempenho das seleções por copa)
#============================================================================
nav_panel(
  "Gols em Copas",
  layout_sidebar(
    sidebar = sidebar(
      width = 290,
      selectInput(
        inputId = "edicao_gols",
        label = "Edição da Copa (Ano - País-Sede):",
        choices = NULL
      ),
      uiOutput("bandeiras_gols_ui"),
      hr(),
      p(strong("Selecione uma Seleção:"), class = "mb-1"),
      div(
        style = "max-height: 360px; overflow-y: auto;",
        radioButtons(
          inputId = "selecao_gols_filtro",
          label = NULL,
          choices = c("A carregar..." = ""),
          selected = ""
        )
      )
    ),
    
    # Linha 1: 2 cards fixos da edição (Total de Gols e Média de Gols por Jogo)
    layout_columns(
      col_widths = c(6, 6),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.80rem; font-weight: 600;",
          "Total de Gols da Copa"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.6rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_total_gols_copa")
        ),
        showcase = bs_icon("trophy-fill"),
        theme = "primary",
        max_height = "95px"
      ),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.80rem; font-weight: 600;",
          "Média de Gols por Jogo"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.6rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_media_gols_jogo")
        ),
        showcase = bs_icon("bar-chart-fill"),
        theme = "info",
        max_height = "95px"
      )
    ),
    
    # Linha 2: 4 cards filtrados pela seleção clicada na edição
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.78rem; font-weight: 600;",
          "Gols Marcados"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.5rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_gols_marcados")
        ),
        showcase = bs_icon("plus-circle-fill"),
        theme = "success",
        max_height = "95px"
      ),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.78rem; font-weight: 600;",
          "Gols Sofridos"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.5rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_gols_sofridos")
        ),
        showcase = bs_icon("dash-circle-fill"),
        theme = "danger",
        max_height = "95px"
      ),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.78rem; font-weight: 600;",
          "Saldo de Gols"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.5rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_saldo_gols")
        ),
        showcase = bs_icon("arrows-expand"),
        theme = "secondary",
        max_height = "95px"
      ),
      value_box(
        title = div(
          style = "text-align: center; width: 100%; font-size: 0.78rem; font-weight: 600;",
          "Posição Final"
        ),
        value = div(
          style = "text-align: center; width: 100%; font-size: 1.5rem; font-weight: 700; line-height: 1.15;",
          textOutput("kpi_posicao_final")
        ),
        showcase = bs_icon("award-fill"),
        theme = "warning",
        max_height = "95px"
      )
    ),
    
    # Linha 3: Tabela da edição (1.º ao último: posição, seleção, vitórias, derrotas)
    card(
      card_header("Classificação na Edição — Posição, Seleção, Vitórias e Derrotas"),
      div(
        style = "max-height: 460px; overflow-y: auto;",
        tableOutput("tabela_posicoes_edicao")
      )
    )
  )
),
aba_gols_ui("gols"),        # Aba "Gols por Edição"
aba_podio_ui("podio")       # Aba "Campeões e Pódio"
)

#============================================================================
# Servidor do aplicativo Shiny
#============================================================================

## Traduçao dos nomes dos países sedes para português

traduzir_pais_sede <- function(host_str) {
  mapa_sedes <- c(
    "Uruguay" = "Uruguai",
    "Italy" = "Itália",
    "France" = "França",
    "Brazil" = "Brasil",
    "Switzerland" = "Suíça",
    "Sweden" = "Suécia",
    "Chile" = "Chile",
    "England" = "Inglaterra",
    "Mexico" = "México",
    "Germany" = "Alemanha",
    "Germany FR" = "Alemanha Ocidental",
    "Argentina" = "Argentina",
    "Spain" = "Espanha",
    "USA" = "Estados Unidos",
    "United States" = "Estados Unidos",
    "South Korea" = "Coreia do Sul",
    "Japan" = "Japão",
    "South Africa" = "África do Sul",
    "Russia" = "Rússia"
  )
  
  vapply(host_str, function(item) {
    if (is.na(item) || item == "") return(item)
    partes <- trimws(strsplit(item, ",")[[1]])
    partes_pt <- ifelse(partes %in% names(mapa_sedes), mapa_sedes[partes], traduzir_selecao(partes))
    paste(partes_pt, collapse = ", ")
  }, character(1))
}

traduzir_fase <- function(fase_vec) {
  mapa_fases <- c(
    "Group stage" = "Fase de Grupos",
    "Group A" = "Grupo A",
    "Group B" = "Grupo B",
    "Group C" = "Grupo C",
    "Group D" = "Grupo D",
    "Group E" = "Grupo E",
    "Group F" = "Grupo F",
    "Group G" = "Grupo G",
    "Group H" = "Grupo H",
    "Round of 16" = "Oitavas de Final",
    "Quarterfinals" = "Quartas de Final",
    "Quarter-finals" = "Quartas de Final",
    "Semifinals" = "Semifinal",
    "Semi-finals" = "Semifinal",
    "Third place match" = "Disputa do 3º Lugar",
    "Third-place match" = "Disputa do 3º Lugar",
    "Match for third place" = "Disputa do 3º Lugar",
    "Final" = "Final"
  )
  
  vapply(fase_vec, function(f) {
    if (is.na(f) || trimws(f) == "") return(f)
    f_clean <- trimws(f)
    if (f_clean %in% names(mapa_fases)) unname(mapa_fases[f_clean]) else f_clean
  }, character(1))
}

traduzir_condicao_vitoria <- function(cond_vec) {
  vapply(cond_vec, function(txt) {
    if (is.na(txt) || trimws(txt) == "") return("")
    res <- trimws(txt)
    
    res <- sub(
      "^(.+?)\\s+won\\s+(?:in|on)\\s+penalties\\s*\\(([^)]+)\\)$",
      "\\1 venceu nos pênaltis (\\2)",
      res,
      ignore.case = TRUE
    )
    
    res <- sub(
      "^(.+?)\\s+won\\s+(?:in|on)\\s+penalties$",
      "\\1 venceu nos pênaltis",
      res,
      ignore.case = TRUE
    )
    
    res <- sub(
      "^(.+?)\\s+won\\s+(?:in\\s+AET|after\\s+extra\\s+time)$",
      "\\1 venceu na prorrogação",
      res,
      ignore.case = TRUE
    )
    
    m <- regexec("^(.+?)\\s+venceu\\s+(.*)$", res)
    partes <- regmatches(res, m)[[1]]
    if (length(partes) == 3) {
      time_pt <- traduzir_selecao(partes[2])
      res <- paste0(time_pt, " venceu ", partes[3])
    }
    
    res
  }, character(1))
}

#============================================================================
#============================================================================
    
  server <- function(input, output, session) {
  
  # Aba 1 - Desempenho por seleção
  
  resumo_sel <- reactive({
    req(input$selecao_escolhida)
    calcular_desempenho(input$selecao_escolhida)
  })
  
  output$total_participacoes_box <- renderText({
    resumo_sel()[["Participações"]][1]
  })
  
  output$tabela_desempenho <- renderTable({
    resumo_sel()
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lcccccccc", digits = 0)
  
  output$bandeira_selecao_ui <- renderUI({
    req(input$selecao_escolhida)
    sel <- input$selecao_escolhida
    nome_pt <- traduzir_selecao(sel)
    url_img <- url_bandeira_selecao(sel)
    
    if (!is.null(url_img)) {
      tagList(
        tags$div(
          class = "small text-muted mb-2",
          "Bandeira da seleção"
        ),
        tags$img(
          src = url_img,
          alt = paste("Bandeira de", nome_pt),
          style = "display: block; margin: 0 auto; width: 120px; height: auto; border-radius: 6px; border: 1px solid #dee2e6; box-shadow: 0 2px 6px rgba(0,0,0,0.08);"
        ),
        tags$div(
          class = "fw-semibold mt-2",
          nome_pt
        )
      )
    } else {
      tagList(
        tags$div(
          class = "small text-muted mb-1",
          "Seleção histórica"
        ),
        tags$div(
          class = "fw-semibold",
          nome_pt
        ),
        tags$div(
          class = "small text-muted mt-1",
          "Bandeira oficial atual não mapeada"
        )
      )
    }
  })
  
   output$grade_decadas_ui <- renderUI({
    sel <- input$selecao_escolhida
    req(sel)
    
    blocos <- lapply(decadas_lista, function(dec) {
      if (length(dec$anos) == 0) {
        return(
          tags$div(
            class = "mb-3 p-3 border rounded bg-light text-muted",
            tags$strong(paste0("Década ", dec$rotulo, ": ")),
            tags$span("Sem edições da Copa do Mundo no período.")
          )
        )
      }
      
      celulas <- lapply(dec$anos, function(ano) {
        info <- colocacao_selecao_ano(sel, ano)
        
        estilo_inline <- switch(
          info$destaque,
          "Campeão" = "background-color: #fff8db; border: 2px solid #d4a017; color: #3b2d00;",
          "Vice-campeão" = "background-color: #f1f5f9; border: 2px solid #64748b; color: #1e293b;",
          "3º lugar" = "background-color: #fdf1e8; border: 2px solid #a05a2c; color: #3d1e0a;",
          "4º lugar" = "background-color: #e6f4ea; border: 2px solid #1e6b40; color: #0f3d22;",
          "participou" = "background-color: #e7f3fb; border: 1.5px solid #155885; color: #0c3552;",
          "background-color: #f8f9fa; border: 1px solid #dee2e6; color: #6c757d;"
        )
        
        badge_estado <- if (info$participou) {
          cor_badge <- switch(
            info$destaque,
            "Campeão" = "background-color: #b8860b; color: #ffffff;",
            "Vice-campeão" = "background-color: #64748b; color: #ffffff;",
            "3º lugar" = "background-color: #a05a2c; color: #ffffff;",
            "4º lugar" = "background-color: #1e6b40; color: #ffffff;",
            "background-color: #155885; color: #ffffff;"
          )
          tags$span(
            class = "badge mb-1",
            style = paste("font-weight: 600;", cor_badge),
            "Participou"
          )
        } else {
          tags$span(
            class = "badge text-bg-secondary mb-1",
            "Não participou"
          )
        }
        
        tags$div(
          class = "col",
          tags$div(
            class = "h-100 p-3 rounded",
            style = estilo_inline,
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-2",
              tags$span(class = "fw-bold fs-6", paste0("Copa ", ano)),
              badge_estado
            ),
            tags$div(
              class = "small fw-medium",
              if (info$participou) paste0("Colocação: ", info$colocacao) else "Sem registo de participação"
            )
          )
        )
      })
      
      tags$div(
        class = "mb-3 p-3 border rounded bg-white",
        tags$div(class = "fw-bold mb-2 text-primary-emphasis", paste0("Década ", dec$rotulo)),
        tags$div(class = "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-2", celulas)
      )
    })
    
    do.call(tagList, blocos)
  })
   
   # Aba 2 - Público e Jogos
   
   # 1. Preparar opções do dropdown "Ano - País-Sede"
   edicoes_opcoes <- wc_df %>%
     arrange(year) %>%
     mutate(
       label_edicao = paste0(year, " - ", host)
     )
   
   observe({
     req(nrow(wc_df) > 0)
     df_ord <- wc_df %>% arrange(desc(year))
     labels_pt <- paste0(df_ord$year, " - ", traduzir_pais_sede(df_ord$host))
     opcoes_edicao <- setNames(as.character(df_ord$year), labels_pt)
     
     sel_atual <- if (!is.null(input$edicao_sede) && input$edicao_sede %in% opcoes_edicao) {
       input$edicao_sede
     } else {
       opcoes_edicao[1]
     }
     
     updateSelectInput(session, "edicao_sede", choices = opcoes_edicao, selected = sel_atual)
   })
   
   # 2. Bandeiras da(s) sede(s) — trata 2002 (Korea/Japan) com duas bandeiras
   
   output$bandeiras_sede_ui <- renderUI({
     req(input$edicao_sede)
     ano_sel <- as.numeric(input$edicao_sede)
     host_str <- wc_df$host[wc_df$year == ano_sel][1]
     req(!is.na(host_str), host_str != "")
     
     # Dicionário ISO-2 para bandeiras via flagcdn
     
     iso_sedes <- switch(
       as.character(ano_sel),
       "1930" = c("uy" = "Uruguai"),
       "1934" = c("it" = "Itália"),
       "1938" = c("fr" = "França"),
       "1950" = c("br" = "Brasil"),
       "1954" = c("ch" = "Suíça"),
       "1958" = c("se" = "Suécia"),
       "1962" = c("cl" = "Chile"),
       "1966" = c("gb-eng" = "Inglaterra"),
       "1970" = c("mx" = "México"),
       "1974" = c("de" = "Alemanha"),
       "1978" = c("ar" = "Argentina"),
       "1982" = c("es" = "Espanha"),
       "1986" = c("mx" = "México"),
       "1990" = c("it" = "Itália"),
       "1994" = c("us" = "Estados Unidos"),
       "1998" = c("fr" = "França"),
       "2002" = c("kr" = "Coreia do Sul", "jp" = "Japão"),
       "2006" = c("de" = "Alemanha"),
       "2010" = c("za" = "África do Sul"),
       "2014" = c("br" = "Brasil"),
       "2018" = c("ru" = "Rússia"),
       setNames(character(0), character(0))
     )
     
     codigos_iso <- names(iso_sedes)
     nomes_pt <- unname(iso_sedes)
     
     elementos <- mapply(function(cod_iso, nome_pt) {
       tags$div(
         class = "d-flex align-items-center gap-2 mb-1",
         tags$img(
           src = paste0("https://flagcdn.com/w40/", cod_iso, ".png"),
           alt = nome_pt,
           height = "22px",
           style = "border-radius: 2px; border: 1px solid #dee2e6;"
         ),
         tags$span(class = "fw-semibold", nome_pt)
       )
     }, codigos_iso, nomes_pt, SIMPLIFY = FALSE)
     
     tags$div(
       class = "mt-2",
       elementos
     )
   })
   
   # 3. Partidas da edição selecionada + atualização da lista clicável de cidades
   partidas_edicao <- reactive({
     req(input$edicao_sede)
     m_df %>%
       filter(year == as.numeric(input$edicao_sede)) %>%
       mutate(city = trimws(city))
   })
   
   observeEvent(partidas_edicao(), {
     cidades <- sort(unique(na.omit(partidas_edicao()$city)))
     updateRadioButtons(
       session,
       "cidade_sede_filtro",
       choices = c("Todas as cidades" = "TODAS", setNames(cidades, cidades)),
       selected = "TODAS"
     )
   })
   
   # 4. KPI 1: Público total fixo + total de jogos da edição
   
   output$kpi_publico_edicao <- renderText({
     req(input$edicao_sede)
     pub <- wc_df$attendance[wc_df$year == as.numeric(input$edicao_sede)][1]
     format(pub, big.mark = ".", decimal.mark = ",")
   })
   
   output$kpi_total_jogos <- renderText({
     req(input$edicao_sede)
     row_ed <- wc_df[wc_df$year == as.numeric(input$edicao_sede), ][1, ]
     row_ed$games
   })
   
   output$kpi_total_selecoes <- renderText({
     req(input$edicao_sede)
     row_ed <- wc_df[wc_df$year == as.numeric(input$edicao_sede), ][1, ]
     row_ed$teams
   })
   
   output$kpi_num_sedes <- renderText({
     req(input$edicao_sede)
     n_distinct(partidas_edicao()$city, na.rm = TRUE)
   })
   
   # 5. KPI 2 (card azul): Nome da sede e número de jogos realizados na sede
   
   output$titulo_kpi_cidade <- renderText({
     req(input$cidade_sede_filtro)
     if (input$cidade_sede_filtro == "TODAS") "Todas as Sedes" else input$cidade_sede_filtro
   })
   
   output$kpi_publico_cidade <- renderText({
     req(input$edicao_sede, input$cidade_sede_filtro)
     df_ed <- partidas_edicao()
     if (input$cidade_sede_filtro == "TODAS") nrow(df_ed) else sum(df_ed$city == input$cidade_sede_filtro, na.rm = TRUE)
   })
   
   output$sub_kpi_cidade <- renderText({
     req(input$cidade_sede_filtro)
     if (input$cidade_sede_filtro == "TODAS") "total de jogos na edição" else "jogos na sede"
   })
   
   # 6. Mapa do país-sede com correção de nomes de cidades, zoom e rótulos sem sobreposição
   output$mapa_sedes <- renderPlot({
     req(input$edicao_sede)
     ano_sel <- as.numeric(input$edicao_sede)
     df_ed <- partidas_edicao()
     
     regioes_mapa <- switch(
       as.character(ano_sel),
       "1930" = "Uruguay",
       "1934" = "Italy",
       "1938" = "France",
       "1950" = "Brazil",
       "1954" = "Switzerland",
       "1958" = "Sweden",
       "1962" = "Chile",
       "1966" = "UK",
       "1970" = "Mexico",
       "1974" = "Germany",
       "1978" = "Argentina",
       "1982" = "Spain",
       "1986" = "Mexico",
       "1990" = "Italy",
       "1994" = "USA",
       "1998" = "France",
       "2002" = c("South Korea", "North Korea", "Japan"),
       "2006" = "Germany",
       "2010" = "South Africa",
       "2014" = "Brazil",
       "2018" = "Russia",
       "World"
     )
     
     mapa_base <- map_data("world", region = regioes_mapa)
     if (ano_sel == 2018) mapa_base <- mapa_base %>% filter(long > 0)
     
     mapa_nomes_cidades <- c(
       "Ekaterinburg" = "Yekaterinburg",
       "Nizhny Novgorod" = "Nizhniy Novgorod",
       "Rostov-on-Don" = "Rostov-na-Donu",
       "Saint Petersburg" = "Saint Petersburg",
       "St. Petersburg" = "Saint Petersburg",
       "Rio De Janeiro" = "Rio de Janeiro",
       "Sao Paulo" = "Sao Paulo",
       "Brasilia" = "Brasilia",
       "Cuiaba" = "Cuiaba",
       "Foxborough" = "Boston",
       "East Rutherford" = "New York",
       "Pasadena" = "Los Angeles",
       "Stanford" = "San Francisco",
       "Pontiac" = "Detroit",
       "Saint-Denis" = "Paris",
       "Gelsenkirchen" = "Gelsenkirchen",
       "Kaiserslautern" = "Kaiserslautern"
     )
     
     data("world.cities", package = "maps", envir = environment())
     paises_wc <- unique(c(regioes_mapa, "Korea South", "England", "Scotland", "Wales"))
     
     cidades_ed <- df_ed %>%
       count(city, name = "jogos") %>%
       mutate(
         city_lookup = ifelse(city %in% names(mapa_nomes_cidades), mapa_nomes_cidades[city], city)
       ) %>%
       left_join(
         world.cities %>%
           filter(country.etc %in% paises_wc) %>%
           group_by(name) %>%
           slice_max(pop, n = 1, with_ties = FALSE) %>%
           ungroup(),
         by = c("city_lookup" = "name")
       ) %>%
       filter(!is.na(lat), !is.na(long)) %>%
       mutate(
         destaque = ifelse(
           input$cidade_sede_filtro != "TODAS" & city == input$cidade_sede_filtro,
           "Selecionada", "Demais sedes"
         )
       )
     
     xlim_val <- NULL
     ylim_val <- NULL
     if (ano_sel == 2018) {
       xlim_val <- c(19, 63)
       ylim_val <- c(42, 62)
     } else if (ano_sel == 1994) {
       xlim_val <- c(-126, -66)
       ylim_val <- c(24, 50)
     } else if (ano_sel == 1998 || ano_sel == 1938) {
       xlim_val <- c(-5.5, 9.8)
       ylim_val <- c(41.3, 51.3)
     } else if (ano_sel == 1982) {
       xlim_val <- c(-9.5, 4.5)
       ylim_val <- c(35.8, 44.0)
     }
     
     ggplot() +
       geom_polygon(
         data = mapa_base,
         aes(x = long, y = lat, group = group),
         fill = "#f1f3f5", color = "#adb5bd", linewidth = 0.45
       ) +
       geom_point(
         data = cidades_ed,
         aes(x = long, y = lat, color = destaque, size = destaque),
         shape = 17
       ) +
       geom_text_repel(
         data = cidades_ed,
         aes(
           x = long, y = lat, label = city,
           color = destaque,
           fontface = ifelse(destaque == "Selecionada", "bold", "plain")
         ),
         size = 3.6,
         box.padding = 0.35,
         point.padding = 0.25,
         segment.color = "#868e96",
         segment.size = 0.3,
         max.overlaps = Inf
       ) +
       scale_color_manual(values = c("Selecionada" = "#dc3545", "Demais sedes" = "#0d6efd")) +
       scale_size_manual(values = c("Selecionada" = 5.5, "Demais sedes" = 3.5)) +
       coord_quickmap(xlim = xlim_val, ylim = ylim_val) +
       theme_void() +
       theme(legend.position = "none")
   })
   
   # Dicionário de tradução de seleções/países para português
   
   traduzir_selecao <- function(x) {
     mapa_paises <- c(
       "Afghanistan" = "Afeganistão",
       "Algeria" = "Argélia",
       "Angola" = "Angola",
       "Argentina" = "Argentina",
       "Australia" = "Austrália",
       "Austria" = "Áustria",
       "Belgium" = "Bélgica",
       "Bolivia" = "Bolívia",
       "Bosnia and Herzegovina" = "Bósnia e Herzegovina",
       "Brazil" = "Brasil",
       "Bulgaria" = "Bulgária",
       "Cameroon" = "Camarões",
       "Canada" = "Canadá",
       "Chile" = "Chile",
       "China" = "China",
       "China PR" = "China",
       "Colombia" = "Colômbia",
       "Costa Rica" = "Costa Rica",
       "Croatia" = "Croácia",
       "Cuba" = "Cuba",
       "Czech Republic" = "República Tcheca",
       "Czechia" = "Tchéquia",
       "Czechoslovakia" = "Tchecoslováquia",
       "Denmark" = "Dinamarca",
       "Dutch East Indies" = "Índias Orientais Holandesas",
       "Ecuador" = "Equador",
       "Egypt" = "Egito",
       "El Salvador" = "El Salvador",
       "England" = "Inglaterra",
       "France" = "França",
       "Germany" = "Alemanha",
       "Germany FR" = "Alemanha Ocidental",
       "Ghana" = "Gana",
       "Greece" = "Grécia",
       "Haiti" = "Haiti",
       "Honduras" = "Honduras",
       "Hungary" = "Hungria",
       "Iceland" = "Islândia",
       "Iran" = "Irã",
       "Iraq" = "Iraque",
       "Ireland" = "Irlanda",
       "Republic of Ireland" = "Irlanda",
       "Northern Ireland" = "Irlanda do Norte",
       "Israel" = "Israel",
       "Italy" = "Itália",
       "Ivory Coast" = "Costa do Marfim",
       "Côte d'Ivoire" = "Costa do Marfim",
       "Jamaica" = "Jamaica",
       "Japan" = "Japão",
       "Kuwait" = "Kuwait",
       "Mexico" = "México",
       "Morocco" = "Marrocos",
       "Netherlands" = "Holanda",
       "New Zealand" = "Nova Zelândia",
       "Nigeria" = "Nigéria",
       "North Korea" = "Coreia do Norte",
       "Norway" = "Noruega",
       "Panama" = "Panamá",
       "Paraguay" = "Paraguai",
       "Peru" = "Peru",
       "Poland" = "Polônia",
       "Portugal" = "Portugal",
       "Romania" = "Romênia",
       "Russia" = "Rússia",
       "Saudi Arabia" = "Arábia Saudita",
       "Scotland" = "Escócia",
       "Senegal" = "Senegal",
       "Serbia" = "Sérvia",
       "Serbia and Montenegro" = "Sérvia e Montenegro",
       "Slovakia" = "Eslováquia",
       "Slovenia" = "Eslovênia",
       "South Africa" = "África do Sul",
       "South Korea" = "Coreia do Sul",
       "Soviet Union" = "União Soviética",
       "Spain" = "Espanha",
       "Sweden" = "Suécia",
       "Switzerland" = "Suíça",
       "Togo" = "Togo",
       "Trinidad and Tobago" = "Trinidad e Tobago",
       "Tunisia" = "Tunísia",
       "Turkey" = "Turquia",
       "Ukraine" = "Ucrânia",
       "United Arab Emirates" = "Emirados Árabes Unidos",
       "United States" = "Estados Unidos",
       "USA" = "Estados Unidos",
       "Uruguay" = "Uruguai",
       "Wales" = "País de Gales",
       "Yugoslavia" = "Iugoslávia",
       "Zaire" = "Zaire"
     )
     
     res <- unname(mapa_paises[x])
     ifelse(is.na(res), x, res)
   }
   
   # 7. Tabela de jogos: seleções em português, sem coluna de público e células centralizadas
   
   output$tabela_jogos_sede <- renderTable({
     req(input$cidade_sede_filtro)
     df_tab <- partidas_edicao()
     
     if (input$cidade_sede_filtro != "TODAS") {
       df_tab <- df_tab %>% filter(city == input$cidade_sede_filtro)
     }
     if ("date" %in% names(df_tab)) {
       df_tab <- df_tab %>% arrange(date)
     }
     
     tem_win_cond <- "win_conditions" %in% names(df_tab)
     fase_pt <- traduzir_fase(df_tab$stage)
     home_pt <- traduzir_selecao(df_tab$home_team)
     away_pt <- traduzir_selecao(df_tab$away_team)
     cond_pt <- if (tem_win_cond) traduzir_condicao_vitoria(df_tab$win_conditions) else rep("", nrow(df_tab))
     
     placar_col <- ifelse(
       !is.na(cond_pt) & cond_pt != "",
       paste0(df_tab$home_score, " - ", df_tab$away_score, " (", cond_pt, ")"),
       paste0(df_tab$home_score, " - ", df_tab$away_score)
     )
     
     data.frame(
       Fase = fase_pt,
       Partida = paste(home_pt, "x", away_pt),
       Placar = placar_col,
       Sede = df_tab$city,
       check.names = FALSE
     )
   }, striped = TRUE, hover = TRUE, spacing = "xs", align = "c", width = "100%")
   
   # Aba 3 - Desempenho das seleções por copa
   
   # --- Funções auxiliares para a aba Gols em Copas ---
   if (!exists("traduzir_selecao_gols")) {
     traduzir_selecao_gols <- function(x) {
       if (exists("traduzir_selecao", mode = "function")) {
         return(traduzir_selecao(x))
       }
       mapa <- c(
         "Argentina" = "Argentina",
         "Australia" = "Austrália",
         "Austria" = "Áustria",
         "Belgium" = "Bélgica",
         "Bolivia" = "Bolívia",
         "Brazil" = "Brasil",
         "Bulgaria" = "Bulgária",
         "Cameroon" = "Camarões",
         "Canada" = "Canadá",
         "Chile" = "Chile",
         "China PR" = "China",
         "Colombia" = "Colômbia",
         "Costa Rica" = "Costa Rica",
         "Croatia" = "Croácia",
         "Cuba" = "Cuba",
         "Czechoslovakia" = "Tchecoslováquia",
         "Denmark" = "Dinamarca",
         "Ecuador" = "Equador",
         "Egypt" = "Egito",
         "England" = "Inglaterra",
         "France" = "França",
         "Germany" = "Alemanha",
         "Germany FR" = "Alemanha Ocidental",
         "Ghana" = "Gana",
         "Greece" = "Grécia",
         "Hungary" = "Hungria",
         "Iran" = "Irã",
         "Iraq" = "Iraque",
         "Ireland" = "Irlanda",
         "Republic of Ireland" = "Irlanda",
         "Northern Ireland" = "Irlanda do Norte",
         "Israel" = "Israel",
         "Italy" = "Itália",
         "Ivory Coast" = "Costa do Marfim",
         "Côte d'Ivoire" = "Costa do Marfim",
         "Jamaica" = "Jamaica",
         "Japan" = "Japão",
         "Mexico" = "México",
         "Morocco" = "Marrocos",
         "Netherlands" = "Holanda",
         "New Zealand" = "Nova Zelândia",
         "Nigeria" = "Nigéria",
         "North Korea" = "Coreia do Norte",
         "Norway" = "Noruega",
         "Panama" = "Panamá",
         "Paraguay" = "Paraguai",
         "Peru" = "Peru",
         "Poland" = "Polônia",
         "Portugal" = "Portugal",
         "Romania" = "Romênia",
         "Russia" = "Rússia",
         "Saudi Arabia" = "Arábia Saudita",
         "Scotland" = "Escócia",
         "Senegal" = "Senegal",
         "Serbia" = "Sérvia",
         "Serbia and Montenegro" = "Sérvia e Montenegro",
         "Slovakia" = "Eslováquia",
         "Slovenia" = "Eslovênia",
         "South Africa" = "África do Sul",
         "South Korea" = "Coreia do Sul",
         "Soviet Union" = "União Soviética",
         "Spain" = "Espanha",
         "Sweden" = "Suécia",
         "Switzerland" = "Suíça",
         "Togo" = "Togo",
         "Trinidad and Tobago" = "Trinidad e Tobago",
         "Tunisia" = "Tunísia",
         "Turkey" = "Turquia",
         "Ukraine" = "Ucrânia",
         "United Arab Emirates" = "Emirados Árabes Unidos",
         "United States" = "Estados Unidos",
         "USA" = "Estados Unidos",
         "Uruguay" = "Uruguai",
         "Wales" = "País de Gales",
         "Yugoslavia" = "Iugoslávia",
         "Zaire" = "Zaire"
       )
       res <- unname(mapa[x])
       ifelse(is.na(res) | res == "", x, res)
     }
   }
   
   traduzir_sede_gols <- function(host_str, ano = NULL) {
     mapa_sedes <- c(
       "Uruguay" = "Uruguai",
       "Italy" = "Itália",
       "France" = "França",
       "Brazil" = "Brasil",
       "Switzerland" = "Suíça",
       "Sweden" = "Suécia",
       "Chile" = "Chile",
       "England" = "Inglaterra",
       "Mexico" = "México",
       "Germany" = "Alemanha",
       "Germany FR" = "Alemanha Ocidental",
       "Argentina" = "Argentina",
       "Spain" = "Espanha",
       "USA" = "Estados Unidos",
       "United States" = "Estados Unidos",
       "South Korea" = "Coreia do Sul",
       "Korea, South" = "Coreia do Sul",
       "Japan" = "Japão",
       "South Africa" = "África do Sul",
       "Russia" = "Rússia",
       "Qatar" = "Catar"
     )
     vapply(seq_along(host_str), function(i) {
       item <- host_str[i]
       ano_i <- if (!is.null(ano)) ano[i] else NA
       if (!is.na(ano_i) && as.numeric(ano_i) == 2002) {
         return("Coreia do Sul - Japão")
       }
       if (is.na(item) || trimws(item) == "") return("")
       partes <- trimws(unlist(strsplit(item, ",|/|-")))
       partes <- partes[partes != ""]
       partes_pt <- ifelse(partes %in% names(mapa_sedes), unname(mapa_sedes[partes]), traduzir_selecao_gols(partes))
       paste(partes_pt, collapse = " - ")
     }, character(1))
   }
   
   # 1. Popular lista suspensa "Ano - País-Sede"
   observe({
     req(nrow(wc_df) > 0)
     df_ord <- wc_df %>% dplyr::arrange(dplyr::desc(year))
     labels_gols <- paste(df_ord$year, "-", traduzir_sede_gols(df_ord$host, df_ord$year))
     opcoes <- setNames(as.character(df_ord$year), labels_gols)
     
     sel <- if (!is.null(input$edicao_gols) && input$edicao_gols %in% opcoes) {
       input$edicao_gols
     } else {
       opcoes[1]
     }
     
     updateSelectInput(
       session,
       inputId = "edicao_gols",
       choices = opcoes,
       selected = sel
     )
   })
   
   # 2. Bandeiras da(s) sede(s) da edição selecionada
   output$bandeiras_gols_ui <- renderUI({
     req(input$edicao_gols)
     ano_sel <- as.numeric(input$edicao_gols)
     
     sedes_info <- switch(
       as.character(ano_sel),
       "1930" = c("uy" = "Uruguai"),
       "1934" = c("it" = "Itália"),
       "1938" = c("fr" = "França"),
       "1950" = c("br" = "Brasil"),
       "1954" = c("ch" = "Suíça"),
       "1958" = c("se" = "Suécia"),
       "1962" = c("cl" = "Chile"),
       "1966" = c("gb-eng" = "Inglaterra"),
       "1970" = c("mx" = "México"),
       "1974" = c("de" = "Alemanha"),
       "1978" = c("ar" = "Argentina"),
       "1982" = c("es" = "Espanha"),
       "1986" = c("mx" = "México"),
       "1990" = c("it" = "Itália"),
       "1994" = c("us" = "Estados Unidos"),
       "1998" = c("fr" = "França"),
       "2002" = c("kr" = "Coreia do Sul", "jp" = "Japão"),
       "2006" = c("de" = "Alemanha"),
       "2010" = c("za" = "África do Sul"),
       "2014" = c("br" = "Brasil"),
       "2018" = c("ru" = "Rússia"),
       "2022" = c("qa" = "Catar"),
       setNames(character(0), character(0))
     )
     
     codigos <- names(sedes_info)
     nomes_pt <- unname(sedes_info)
     
     elementos <- mapply(function(cod, nome) {
       tags$div(
         class = "d-flex align-items-center gap-2 mb-1",
         tags$img(
           src = paste0("https://flagcdn.com/w40/", cod, ".png"),
           alt = nome,
           height = "22px",
           style = "border-radius: 2px; border: 1px solid #dee2e6;"
         ),
         tags$span(class = "fw-semibold", nome)
       )
     }, codigos, nomes_pt, SIMPLIFY = FALSE)
     
     tags$div(class = "mt-2", elementos)
   })
   
   # 3. Partidas da edição selecionada
   jogos_edicao_gols <- reactive({
     req(input$edicao_gols)
     m_df %>%
       dplyr::filter(year == as.numeric(input$edicao_gols))
   })
   
   # 4. Tabela consolidada de posições, gols e resultados por seleção na edição
   classificacao_edicao_gols <- reactive({
     req(input$edicao_gols)
     ano_sel <- as.numeric(input$edicao_gols)
     df_j <- jogos_edicao_gols()
     req(nrow(df_j) > 0)
     
     mandantes <- df_j %>%
       dplyr::transmute(
         selecao_raw = home_team,
         gols_pro = as.numeric(home_score),
         gols_contra = as.numeric(away_score)
       )
     
     visitantes <- df_j %>%
       dplyr::transmute(
         selecao_raw = away_team,
         gols_pro = as.numeric(away_score),
         gols_contra = as.numeric(home_score)
       )
     
     resumo <- dplyr::bind_rows(mandantes, visitantes) %>%
       dplyr::filter(!is.na(selecao_raw), trimws(selecao_raw) != "") %>%
       dplyr::group_by(selecao_raw) %>%
       dplyr::summarise(
         jogos = dplyr::n(),
         gols_marcados = sum(gols_pro, na.rm = TRUE),
         gols_sofridos = sum(gols_contra, na.rm = TRUE),
         saldo_gols = gols_marcados - gols_sofridos,
         vitorias = sum(gols_pro > gols_contra, na.rm = TRUE),
         empates = sum(gols_pro == gols_contra, na.rm = TRUE),
         derrotas = sum(gols_pro < gols_contra, na.rm = TRUE),
         pontos = vitorias * 3 + empates,
         .groups = "drop"
       )
     
     linha_wc <- wc_df[wc_df$year == ano_sel, , drop = FALSE]
     podio_rank <- rep(NA_integer_, nrow(resumo))
     
     if (nrow(linha_wc) > 0) {
       if ("winner" %in% names(linha_wc) && !is.na(linha_wc$winner[1])) {
         podio_rank[resumo$selecao_raw == linha_wc$winner[1]] <- 1L
       }
       if ("second" %in% names(linha_wc) && !is.na(linha_wc$second[1])) {
         podio_rank[resumo$selecao_raw == linha_wc$second[1]] <- 2L
       }
       if ("third" %in% names(linha_wc) && !is.na(linha_wc$third[1])) {
         podio_rank[resumo$selecao_raw == linha_wc$third[1]] <- 3L
       }
       if ("fourth" %in% names(linha_wc) && !is.na(linha_wc$fourth[1])) {
         podio_rank[resumo$selecao_raw == linha_wc$fourth[1]] <- 4L
       }
     }
     
     resumo %>%
       dplyr::mutate(
         podio_rank = podio_rank,
         selecao_pt = traduzir_selecao_gols(selecao_raw)
       ) %>%
       dplyr::arrange(
         dplyr::desc(!is.na(podio_rank)),
         dplyr::coalesce(podio_rank, 999L),
         dplyr::desc(pontos),
         dplyr::desc(saldo_gols),
         dplyr::desc(gols_marcados),
         selecao_pt
       ) %>%
       dplyr::mutate(
         posicao = dplyr::row_number(),
         posicao_rotulo = paste0(posicao, "º")
       )
   })
   
   # 5. Atualizar lista clicável de seleções participantes da edição
   observeEvent(classificacao_edicao_gols(), {
     df_c <- classificacao_edicao_gols()
     req(nrow(df_c) > 0)
     
     opcoes_sel <- setNames(df_c$selecao_raw, df_c$selecao_pt)
     sel_atual <- if (!is.null(input$selecao_gols_filtro) && input$selecao_gols_filtro %in% opcoes_sel) {
       input$selecao_gols_filtro
     } else {
       opcoes_sel[1]
     }
     
     updateRadioButtons(
       session,
       inputId = "selecao_gols_filtro",
       choices = opcoes_sel,
       selected = sel_atual
     )
   })
   
   # 6. Cards fixos da edição: Total de Gols da Copa e Média de Gols por Jogo
   output$kpi_total_gols_copa <- renderText({
     req(input$edicao_gols)
     ano_sel <- as.numeric(input$edicao_gols)
     total_gols <- wc_df$goals_scored[wc_df$year == ano_sel][1]
     req(!is.na(total_gols))
     format(total_gols, big.mark = ".", decimal.mark = ",")
   })
   
   output$kpi_media_gols_jogo <- renderText({
     req(input$edicao_gols)
     ano_sel <- as.numeric(input$edicao_gols)
     linha_ed <- wc_df[wc_df$year == ano_sel, , drop = FALSE][1, , drop = FALSE]
     req(nrow(linha_ed) > 0, !is.na(linha_ed$goals_scored), !is.na(linha_ed$games), linha_ed$games > 0)
     media_jogo <- linha_ed$goals_scored / linha_ed$games
     format(round(media_jogo, 2), nsmall = 2, decimal.mark = ",")
   })
   
   # 7. Cards filtrados pela seleção clicada na edição
   selecao_stats_gols <- reactive({
     req(input$selecao_gols_filtro)
     df_c <- classificacao_edicao_gols()
     linha <- df_c %>% dplyr::filter(selecao_raw == input$selecao_gols_filtro)
     req(nrow(linha) > 0)
     linha[1, , drop = FALSE]
   })
   
   output$kpi_gols_marcados <- renderText({
     format(selecao_stats_gols()$gols_marcados, big.mark = ".", decimal.mark = ",")
   })
   
   output$kpi_gols_sofridos <- renderText({
     format(selecao_stats_gols()$gols_sofridos, big.mark = ".", decimal.mark = ",")
   })
   
   output$kpi_saldo_gols <- renderText({
     saldo <- selecao_stats_gols()$saldo_gols
     sinal <- if (saldo > 0) "+" else ""
     paste0(sinal, format(saldo, big.mark = ".", decimal.mark = ","))
   })
   
   output$kpi_posicao_final <- renderText({
     selecao_stats_gols()$posicao_rotulo
   })
   
   # 8. Tabela filtrada pela edição: posição (1.º ao último), seleção, vitórias, derrotas
   output$tabela_posicoes_edicao <- renderTable({
     df_c <- classificacao_edicao_gols()
     data.frame(
       `Posição` = df_c$posicao_rotulo,
       `Seleção` = df_c$selecao_pt,
       `Vitórias` = df_c$vitorias,
       `Derrotas` = df_c$derrotas,
       check.names = FALSE
     )
   }, striped = TRUE, hover = TRUE, spacing = "xs", align = "c", width = "100%")
   
   # Aba "Gols por Edição"
   aba_gols_server("gols", wc_df)
   
   # Aba "Campeões e Pódio"
   aba_podio_server("podio", wc_df)
   
}

shinyApp(ui = ui, server = server)
