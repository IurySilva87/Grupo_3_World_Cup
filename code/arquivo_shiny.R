install.packages("bsicons")

library(shiny)
library(bslib)
library(bsicons)

raiz_projeto <- if (file.exists("01_read_data.R") && file.exists("../data/raw")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else if (file.exists("code/01_read_data.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else {
  stop("Não foi possível localizar a raiz do projeto.")
}

dir_anterior <- getwd()
setwd(raiz_projeto)
on.exit(setwd(dir_anterior), add = TRUE)

source("code/01_read_data.R")

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

ui <- page_navbar(
  title = "Estatísticas das Copas do Mundo de Futebol: 1930-2018",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
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
  )
)

server <- function(input, output, session) {
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
}

shinyApp(ui = ui, server = server)