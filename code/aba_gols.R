# =============================================================================
# Aba 3 - Gols por Edição
# Total de gols e média de gols por partida
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)


# Objetos e funções auxiliares ================================================

# Formata números no padrão brasileiro
fmt_num_gols <- function(x, casas = 0) {
  formatC(x, format = "f", digits = casas, big.mark = ".", decimal.mark = ",")
}

# Rótulo com fundo branco e sem borda
rotulo_fundo_branco <- function(...) {
  geom_label(..., linewidth = 0)
}

# Vetor de tradução dos países-sede
dicionario_sedes_gols <- c(
  "Uruguay" = "Uruguai", "Italy" = "Itália", "France" = "França",
  "Brazil" = "Brasil", "Switzerland" = "Suíça", "Sweden" = "Suécia",
  "Chile" = "Chile", "England" = "Inglaterra", "Mexico" = "México",
  "Germany" = "Alemanha", "Argentina" = "Argentina", "Spain" = "Espanha",
  "USA" = "Estados Unidos", "Japan" = "Japão", "South Korea" = "Coreia do Sul",
  "South Africa" = "África do Sul", "Russia" = "Rússia"
)

#função para traduzir vetor de sedes
traduzir_sede_gols <- function(x) {
  vapply(x, function(item) {
    partes <- trimws(strsplit(item, ",")[[1]])
    pt <- unname(dicionario_sedes_gols[partes])
    paste(pt, collapse = " e ")
  }, character(1), USE.NAMES = FALSE)
}

# Tabela base: uma linha por edição, com a média de gols por partida
preparar_gols_edicao <- function(wc_df) {
  wc_df %>%
    transmute(
      year,
      host,
      teams,
      games,
      goals_scored,
      media = goals_scored / games
    ) %>%
    arrange(year)
}


# =============================================================================
# UI 
# =============================================================================

aba_gols_ui <- function(id) {
  ns <- NS(id) #função de namespace

  nav_panel(
    title = "Gols por Edição",
    layout_sidebar(
      fillable = FALSE, # Conteúdo empilhado
      sidebar = sidebar(
        width = 280,
        sliderInput(
          ns("anos"),
          "Período (ano da Copa):",
          min = 1930, max = 2018,
          value = c(1930, 2018),
          step = 4, sep = "", ticks = FALSE
        ),
        radioButtons(
          ns("metrica"),
          "Métrica do gráfico:",
          choices = c(
            "Média de gols por partida" = "media",
            "Total de gols" = "total"
          ),
          selected = "media"
        ),
        radioButtons(
          ns("tipo_grafico"),
          "Tipo de gráfico:",
          choices = c("Barras" = "barras", "Linha" = "linha"),
          selected = "barras",
          inline = TRUE
        ),
        checkboxInput(ns("tendencia"), "Linha de tendência (regressão linear)", value = TRUE),
        checkboxInput(ns("media_periodo"), "Linha da média do período", value = TRUE),
        hr(),
        selectInput(
          ns("ordem_tabela"),
          "Ordenar tabela por:",
          choices = c(
            "Ano (crescente)" = "ano",
            "Média de gols por partida (maior primeiro)" = "media",
            "Total de gols (maior primeiro)" = "total"
          ),
          selected = "ano"
        )
      ),

      # Linha 1: indicadores do período selecionado
      # Caixas de indicadores mais compactas (fonte, ícone e espaçamento menores)
      tags$style(HTML("
        .kpis-compactos .value-box-area { padding: 0.6rem 0.9rem !important; }
        .kpis-compactos .value-box-title { font-size: 0.9rem !important; margin-bottom: 0.1rem !important; }
        .kpis-compactos .value-box-value { font-size: 1.7rem !important; line-height: 1.2 !important; margin-bottom: 0.1rem !important; }
        .kpis-compactos .value-box-area p { font-size: 0.8rem; margin-bottom: 0; }
        .kpis-compactos .value-box-showcase { padding: 0.5rem 0.4rem 0.5rem 0.9rem !important; }
        .kpis-compactos .value-box-showcase .bi { width: 2.3rem !important; height: 2.3rem !important; }
      ")),
      #CSS que diminui fonte, ícone e espaçamento das caixas de indicadores
      layout_columns(
        fill = FALSE,
        class = "kpis-compactos",
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Total de gols no período",
          value = textOutput(ns("kpi_total")),
          showcase = bs_icon("bullseye"),
          theme = "primary"
        ),
        value_box(
          title = "Média de gols por partida",
          value = textOutput(ns("kpi_media")),
          p(textOutput(ns("kpi_media_sub"), inline = TRUE)),
          showcase = bs_icon("calculator"),
          theme = "secondary"
        ),
        value_box(
          title = "Maior média",
          value = textOutput(ns("kpi_max")),
          p(textOutput(ns("kpi_max_sub"), inline = TRUE)),
          showcase = bs_icon("graph-up-arrow"),
          theme = "success"
        ),
        value_box(
          title = "Menor média",
          value = textOutput(ns("kpi_min")),
          p(textOutput(ns("kpi_min_sub"), inline = TRUE)),
          showcase = bs_icon("graph-down-arrow"),
          theme = "danger"
        )
      ),

      # Linha 2: gráfico
      card(
        full_screen = TRUE,
        fill = FALSE,
        card_header(
          class = "bg-primary text-white",
          textOutput(ns("titulo_grafico"), inline = TRUE)
        ),
        plotOutput(ns("grafico"), height = "440px"),
        card_footer(class = "small text-muted", uiOutput(ns("rodape_grafico")))
      ),

      # Linha 3: tabela
      card(
        fill = FALSE,
        card_header(class = "bg-primary text-white", "Gols por edição"),
        div(
          style = "max-height: 420px; overflow-y: auto;",
          tableOutput(ns("tabela"))
        )
      )
    )
  )
}

# =============================================================================
# Server 
# =============================================================================

aba_gols_server <- function(id, wc_df) {
  base_gols <- preparar_gols_edicao(wc_df)

  moduleServer(id, function(input, output, session) {

    dados <- reactive({
      req(input$anos)
      base_gols %>%
        filter(year >= input$anos[1], year <= input$anos[2])
    })

    # Garante que há pelo menos uma edição no período (1942 e 1946 não tiveram Copa)
    dados_validos <- reactive({
      d <- dados()
      validate(need(nrow(d) > 0, "Não houve Copa do Mundo no período selecionado."))
      d
    })

    # Ajuste linear da métrica escolhida em função do ano
    ajuste <- reactive({
      d <- dados_validos()
      if (nrow(d) < 3) return(NULL)
      y <- if (input$metrica == "media") d$media else d$goals_scored
      lm(y ~ d$year)
    })

    # Valor da linha de média: ponderada (gols / partidas) para a média por
    # partida; média simples entre edições para o total de gols
    media_ref <- reactive({
      d <- dados_validos()
      if (input$metrica == "media") sum(d$goals_scored) / sum(d$games) else mean(d$goals_scored)
    })

    # Indicadores ---------------------------------------------------------------

    output$kpi_total <- renderText({
      fmt_num_gols(sum(dados_validos()$goals_scored))
    })

    # Média ponderada: total de gols / total de partidas do período
    output$kpi_media <- renderText({
      d <- dados_validos()
      fmt_num_gols(sum(d$goals_scored) / sum(d$games), 2)
    })

    output$kpi_media_sub <- renderText({
      d <- dados_validos()
      paste0(fmt_num_gols(sum(d$games)), " partidas em ", nrow(d), " edições")
    })

    output$kpi_max <- renderText({
      d <- dados_validos()
      fmt_num_gols(max(d$media), 2)
    })

    output$kpi_max_sub <- renderText({
      d <- dados_validos()
      i <- which.max(d$media)
      paste0(d$year[i], " - ", traduzir_sede_gols(d$host[i]))
    })

    output$kpi_min <- renderText({
      d <- dados_validos()
      fmt_num_gols(min(d$media), 2)
    })

    output$kpi_min_sub <- renderText({
      d <- dados_validos()
      i <- which.min(d$media)
      paste0(d$year[i], " - ", traduzir_sede_gols(d$host[i]))
    })

    # Gráfico -------------------------------------------------------------------

    output$titulo_grafico <- renderText({
      if (input$metrica == "media") {
        "Média de gols por partida em cada edição"
      } else {
        "Total de gols marcados em cada edição"
      }
    })

    output$grafico <- renderPlot({
      d <- dados_validos()
      eh_media <- input$metrica == "media"

      d$valor <- if (eh_media) d$media else d$goals_scored
      d$rotulo <- if (eh_media) fmt_num_gols(d$valor, 2) else fmt_num_gols(d$valor)
      d$destaque <- ifelse(d$valor == max(d$valor), "Maior valor", "Demais edições")

      rotulo_y <- if (eh_media) "Gols por partida" else "Total de gols"

      g <- ggplot(d, aes(x = year, y = valor))

      if (input$tipo_grafico == "barras") {
        g <- g +
          geom_col(aes(fill = destaque), width = 3) +
          scale_fill_manual(
            values = c("Maior valor" = "#d4a017", "Demais edições" = "#2c3e50"),
            name = NULL
          )
      } else {
        g <- g +
          geom_line(colour = "#2c3e50", linewidth = 1) +
          geom_point(aes(colour = destaque), size = 3) +
          scale_colour_manual(
            values = c("Maior valor" = "#d4a017", "Demais edições" = "#2c3e50"),
            name = NULL
          )
      }

      # Linhas de referência: desenhadas antes dos rótulos, para ficarem por baixo
      if (isTRUE(input$media_periodo)) {
        g <- g +
          geom_hline(yintercept = media_ref(), colour = "#e67e22", linewidth = 0.9)
      }

      if (isTRUE(input$tendencia) && nrow(d) >= 3) {
        g <- g +
          geom_smooth(
            method = "lm", formula = y ~ x, se = FALSE,
            colour = "#c0392b", linetype = "dashed", linewidth = 0.9
          )
      }

      # Rótulos com fundo branco por cima das linhas, para não ficarem riscados
      g +
        rotulo_fundo_branco(
          aes(label = rotulo),
          vjust = -0.25, size = 3.4, colour = "#2c3e50", fill = "white",
          label.padding = unit(0.12, "lines")
        ) +
        scale_x_continuous(breaks = d$year) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Ano da Copa", y = rotulo_y) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }, res = 96)

    # Legenda das linhas de referência, abaixo do gráfico
    output$rodape_grafico <- renderUI({
      eh_media <- input$metrica == "media"
      casas <- if (eh_media) 2 else 1
      unidade <- if (eh_media) " gols por partida" else " gols"
      itens <- list()

      if (isTRUE(input$media_periodo)) {
        itens <- c(itens, list(tags$div(
          tags$span(style = "color: #e67e22; font-weight: 700;", "—— "),
          paste0("Média do período: ", fmt_num_gols(media_ref(), casas), unidade, ".")
        )))
      }

      if (isTRUE(input$tendencia)) {
        fit <- ajuste()
        texto <- if (is.null(fit)) {
          "Selecione ao menos 3 edições para calcular a tendência linear."
        } else {
          inclinacao_decada <- unname(coef(fit)[2]) * 10
          r2 <- summary(fit)$r.squared
          p_valor <- summary(fit)$coefficients[2, 4]
          paste0(
            "Tendência linear: ",
            ifelse(inclinacao_decada >= 0, "+", ""),
            fmt_num_gols(inclinacao_decada, casas), unidade, " por década",
            " (R² = ", fmt_num_gols(r2, 2), "; ",
            ifelse(p_valor < 0.001, "p < 0,001", paste0("p = ", fmt_num_gols(p_valor, 3))), ")."
          )
        }
        itens <- c(itens, list(tags$div(
          tags$span(style = "color: #c0392b; font-weight: 700;", "- - - "),
          texto
        )))
      }

      if (length(itens) == 0) return(NULL)
      tagList(itens)
    })

    # Tabela --------------------------------------------------------------------

    output$tabela <- renderTable({
      d <- dados_validos()

      d <- switch(
        input$ordem_tabela,
        "media" = arrange(d, desc(media)),
        "total" = arrange(d, desc(goals_scored)),
        arrange(d, year)
      )

      data.frame(
        "Ano" = as.character(d$year),
        "Sede" = traduzir_sede_gols(d$host),
        "Seleções" = as.integer(d$teams),
        "Partidas" = as.integer(d$games),
        "Gols" = as.integer(d$goals_scored),
        "Média de gols por partida" = fmt_num_gols(d$media, 2),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "llcccc")
  })
}
