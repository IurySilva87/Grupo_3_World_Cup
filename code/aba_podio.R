# =============================================================================
# Aba 4 - Campeões e Pódio
# títulos (winner) e participações no top 4 por seleção nas Copas de 1930 a 2018
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)


# Objetos e funções auxiliares ================================================

niveis_podio <- c("Campeão", "Vice-campeão", "3º lugar", "4º lugar")

# Mesmas cores usadas na aba "Desempenho por seleção"
cores_podio <- c(
  "Campeão"      = "#d4a017",
  "Vice-campeão" = "#64748b",
  "3º lugar"     = "#a05a2c",
  "4º lugar"     = "#1e6b40"
)

# Tradução dos nomes das seleções que aparecem no top 4 (1930-2018).
dicionario_podio <- c(
  "Argentina" = "Argentina", "Austria" = "Áustria", "Belgium" = "Bélgica",
  "Brazil" = "Brasil", "Bulgaria" = "Bulgária", "Chile" = "Chile",
  "Croatia" = "Croácia", "Czechoslovakia" = "Tchecoslováquia",
  "England" = "Inglaterra", "France" = "França", "Germany" = "Alemanha",
  "West Germany" = "Alemanha Ocidental", "Hungary" = "Hungria",
  "Italy" = "Itália", "Netherlands" = "Países Baixos", "Poland" = "Polônia",
  "Portugal" = "Portugal", "South Korea" = "Coreia do Sul",
  "Soviet Union" = "União Soviética", "Spain" = "Espanha", "Sweden" = "Suécia",
  "Turkey" = "Turquia", "Uruguay" = "Uruguai", "USA" = "Estados Unidos",
  "Yugoslavia" = "Iugoslávia"
)

traduzir_selecao_podio <- function(x) {
  out <- unname(dicionario_podio[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}


# Formato longo: uma linha por (edição, posição, seleção)
preparar_podio <- function(wc_df) {
  longo <- bind_rows(
    transmute(wc_df, year, posicao = "Campeão",      selecao = winner),
    transmute(wc_df, year, posicao = "Vice-campeão", selecao = second),
    transmute(wc_df, year, posicao = "3º lugar",     selecao = third),
    transmute(wc_df, year, posicao = "4º lugar",     selecao = fourth)
  )

  # Alemanha Ocidental e Alemanha contadas juntas (decisão do grupo, como a FIFA)
  longo$selecao[longo$selecao == "West Germany"] <- "Germany"

  longo %>%
    mutate(posicao = factor(posicao, levels = niveis_podio)) %>%
    arrange(year, posicao)
}

# Tabela larga: uma linha por seleção, com contagem por posição
resumir_podio <- function(longo, posicoes = niveis_podio) {
  contagem <- function(sel, pos) sum(longo$selecao == sel & longo$posicao == pos)

  selecoes <- unique(longo$selecao)

  resumo <- data.frame(
    selecao = selecoes,
    titulos = vapply(selecoes, contagem, integer(1), pos = "Campeão"),
    vices   = vapply(selecoes, contagem, integer(1), pos = "Vice-campeão"),
    terceiros = vapply(selecoes, contagem, integer(1), pos = "3º lugar"),
    quartos = vapply(selecoes, contagem, integer(1), pos = "4º lugar"),
    stringsAsFactors = FALSE
  )

  colunas_pos <- c(
    "Campeão" = "titulos", "Vice-campeão" = "vices",
    "3º lugar" = "terceiros", "4º lugar" = "quartos"
  )
  resumo$total <- rowSums(resumo[, colunas_pos[posicoes], drop = FALSE])

  resumo$anos_titulos <- vapply(resumo$selecao, function(sel) {
    anos <- longo$year[longo$selecao == sel & longo$posicao == "Campeão"]
    if (length(anos) == 0) "-" else paste(sort(anos), collapse = ", ")
  }, character(1))

  rownames(resumo) <- NULL
  resumo[resumo$total > 0, , drop = FALSE]
}

# Ordena o resumo conforme o critério escolhido
ordenar_podio <- function(resumo, criterio, posicoes = niveis_podio) {
  peso <- function(pos, coluna) {
    if (pos %in% posicoes) resumo[[coluna]] else rep(0, nrow(resumo))
  }
  t1 <- peso("Campeão", "titulos")
  t2 <- peso("Vice-campeão", "vices")
  t3 <- peso("3º lugar", "terceiros")
  t4 <- peso("4º lugar", "quartos")
  nome <- traduzir_selecao_podio(resumo$selecao)
  
  ordem <- if (criterio == "total") {
    order(-resumo$total, -t1, -t2, -t3, -t4, nome)
  } else {
    # Critério "quadro de medalhas": títulos, depois vices, 3º e 4º lugares
    order(-t1, -t2, -t3, -t4, nome)
  }
  resumo[ordem, , drop = FALSE]
}

# "A", "A e B", "A, B e C"
juntar_nomes <- function(x) {
  x <- sort(x)
  if (length(x) <= 1) return(x)
  paste(paste(head(x, -1), collapse = ", "), "e", tail(x, 1))
}

# Todas as seleções empatadas no valor máximo (NULL se o máximo for 0)
lideres <- function(selecoes, valores) {
  if (length(valores) == 0 || max(valores) == 0) return(NULL)
  m <- max(valores)
  list(nomes = traduzir_selecao_podio(selecoes[valores == m]), valor = m)
}

# Texto da caixa: até 2 nomes por extenso; mais que isso, "N empatadas"
texto_lider <- function(l) {
  if (is.null(l)) return("-")
  if (length(l$nomes) <= 2) juntar_nomes(l$nomes) else paste(length(l$nomes), "empatadas")
}

plural <- function(n, singular, plural) paste(n, if (n == 1) singular else plural)


# =============================================================================
# UI 
# =============================================================================


aba_podio_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Campeões e Pódio",
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
        checkboxGroupInput(
          ns("posicoes"),
          "Posições consideradas:",
          choices = niveis_podio,
          selected = niveis_podio
        ),
        radioButtons(
          ns("criterio"),
          "Ordenar ranking por:",
          choices = c(
            "Títulos (desempate: vices, 3º, 4º)" = "titulos",
            "Total de pódios" = "total"
          ),
          selected = "titulos"
        ),
        sliderInput(
          ns("top_n"),
          "Número de seleções no gráfico:",
          min = 3, max = 24, value = 12, step = 1
        ),
        selectInput(
          ns("selecao"),
          "Destacar seleção:",
          choices = c("Nenhuma" = ""),
          selected = ""
        ),
        p(
          class = "small text-muted mt-2",
          "Alemanha Ocidental (1954-1990) e Alemanha são contadas juntas, como faz a FIFA."
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
      layout_columns(
        fill = FALSE,
        class = "kpis-compactos",
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Seleções campeãs",
          value = textOutput(ns("kpi_campeas")),
          p(textOutput(ns("kpi_campeas_sub"), inline = TRUE)),
          showcase = bs_icon("trophy-fill"),
          theme = "warning"
        ),
        value_box(
          title = "Maior campeã",
          value = textOutput(ns("kpi_maior")),
          p(textOutput(ns("kpi_maior_sub"), inline = TRUE)),
          showcase = bs_icon("star-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Mais vezes no pódio",
          value = textOutput(ns("kpi_podios")),
          p(textOutput(ns("kpi_podios_sub"), inline = TRUE)),
          showcase = bs_icon("bar-chart-steps"),
          theme = "secondary"
        ),
        value_box(
          title = "Seleção destacada",
          value = textOutput(ns("kpi_sel")),
          p(textOutput(ns("kpi_sel_sub"), inline = TRUE)),
          showcase = bs_icon("flag-fill"),
          theme = "info"
        )
      ),

      # Linha 2: gráfico
      card(
        full_screen = TRUE,
        fill = FALSE,
        card_header(class = "bg-primary text-white", "Títulos e colocações no top 4 por seleção"),
        # A altura acompanha o número de seleções (definida no servidor)
        plotOutput(ns("grafico"), height = "auto"),
        card_footer(
          class = "small text-muted",
          "Em 1930 não houve disputa de 3º lugar: a ordem EUA (3º) e Iugoslávia (4º) segue a classificação da FIFA. ",
          "Em 1950 não houve final: o título foi decidido num quadrangular final, ",
          "e as posições seguem a classificação daquela fase."
        )
      ),

      # Linha 3: tabela
      card(
        fill = FALSE,
        card_header(class = "bg-primary text-white", "Quadro de colocações por seleção"),
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


aba_podio_server <- function(id, wc_df) {
  base_podio <- preparar_podio(wc_df)

  moduleServer(id, function(input, output, session) {

    longo <- reactive({
      req(input$anos)
      base_podio %>%
        filter(year >= input$anos[1], year <= input$anos[2])
    })

    resumo <- reactive({
      validate(
        need(length(input$posicoes) > 0, "Selecione ao menos uma posição."),
        need(nrow(longo()) > 0, "Não houve Copa do Mundo no período selecionado.")
      )
      r <- resumir_podio(longo(), input$posicoes)
      validate(need(nrow(r) > 0, "Nenhuma seleção nas posições escolhidas."))
      ordenar_podio(r, input$criterio, input$posicoes)
    })

    # Preenche a lista do seletor de destaque (em ordem alfabética, em português)
    sel <- unique(base_podio$selecao)
    sel <- sel[order(traduzir_selecao_podio(sel))]
    updateSelectInput(
      session, "selecao",
      choices = c("Nenhuma" = "", setNames(sel, traduzir_selecao_podio(sel))),
      selected = ""
    )

    # Indicadores ---------------------------------------------------------------

    output$kpi_campeas <- renderText({
      sum(resumo()$titulos > 0)
    })

    output$kpi_campeas_sub <- renderText({
      paste0(length(unique(longo()$year)), " edições no período")
    })

    output$kpi_maior <- renderText({
      r <- resumo()
      validate(need(any(r$titulos > 0), "-"))
      lider <- r[order(-r$titulos, -r$vices), ][1, ]
      traduzir_selecao_podio(lider$selecao)
    })

    output$kpi_maior_sub <- renderText({
      r <- resumo()
      validate(need(any(r$titulos > 0), "Sem títulos no período"))
      lider <- r[order(-r$titulos, -r$vices), ][1, ]
      paste0(lider$titulos, ifelse(lider$titulos == 1, " título", " títulos"))
    })

    output$kpi_podios <- renderText({
      r <- resumo()
      lider <- r[order(-r$total, -r$titulos), ][1, ]
      traduzir_selecao_podio(lider$selecao)
    })

    output$kpi_podios_sub <- renderText({
      r <- resumo()
      lider <- r[order(-r$total, -r$titulos), ][1, ]
      paste0(lider$total, ifelse(lider$total == 1, " colocação", " colocações"))
    })

    output$kpi_sel <- renderText({
      if (is.null(input$selecao) || input$selecao == "") return("Nenhuma")
      traduzir_selecao_podio(input$selecao)
    })

    output$kpi_sel_sub <- renderText({
      if (is.null(input$selecao) || input$selecao == "") {
        return("Escolha uma seleção na barra lateral")
      }
      r <- resumo()
      linha <- r[r$selecao == input$selecao, , drop = FALSE]
      if (nrow(linha) == 0) return("Fora das posições escolhidas no período")
      posicao_rank <- which(r$selecao == input$selecao)
      paste0(posicao_rank, "º no ranking · ", linha$titulos, " título(s) · ",
             linha$total, " colocação(ões)")
    })

    # Gráfico -------------------------------------------------------------------

    output$grafico <- renderPlot({
      r <- head(resumo(), input$top_n)
      ordem <- r$selecao

      dados_graf <- longo() %>%
        filter(selecao %in% ordem, posicao %in% input$posicoes) %>%
        count(selecao, posicao, name = "n") %>%
        mutate(
          rotulo = traduzir_selecao_podio(selecao),
          rotulo = factor(rotulo, levels = rev(traduzir_selecao_podio(ordem))),
          posicao = factor(posicao, levels = rev(niveis_podio))
        )

      totais <- data.frame(
        rotulo = factor(traduzir_selecao_podio(r$selecao), levels = levels(dados_graf$rotulo)),
        total = r$total
      )

      # Só esmaece as demais barras se a seleção destacada estiver no gráfico
      tem_destaque <- !is.null(input$selecao) && input$selecao %in% ordem
      dados_graf$alfa <- if (tem_destaque) {
        ifelse(dados_graf$selecao == input$selecao, 1, 0.35)
      } else {
        1
      }

      ggplot(dados_graf, aes(x = n, y = rotulo)) +
        geom_col(aes(fill = posicao, alpha = alfa), width = 0.7) +
        geom_text(
          data = totais, aes(x = total, y = rotulo, label = total),
          hjust = -0.4, size = 4, fontface = "bold", colour = "#2c3e50",
          inherit.aes = FALSE
        ) +
        scale_fill_manual(
          values = cores_podio, breaks = niveis_podio, name = NULL, drop = FALSE
        ) +
        scale_alpha_identity() +
        scale_x_continuous(
          breaks = function(lim) seq(0, ceiling(lim[2]), by = 1),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Número de colocações", y = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()
        )
    }, res = 96, height = function() {
      # ~34 px por seleção, para os nomes nunca se sobreporem
      n <- tryCatch(min(input$top_n, nrow(resumo())), error = function(e) 5)
      max(300, 130 + 34 * n)
    })

    # Tabela --------------------------------------------------------------------

    output$tabela <- renderTable({
      r <- resumo()
      data.frame(
        "Posição" = seq_len(nrow(r)),
        "Seleção" = traduzir_selecao_podio(r$selecao),
        "Títulos" = r$titulos,
        "Vices" = r$vices,
        "3º lugar" = r$terceiros,
        "4º lugar" = r$quartos,
        "Total (posições escolhidas)" = as.integer(r$total),
        "Anos dos títulos" = r$anos_titulos,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "clcccccl", digits = 0)
  })
}
