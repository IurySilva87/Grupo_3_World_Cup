library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Projeto World Cup - UFPR",
  sidebar = sidebar("Filtros"),
  "Conteudo principal do dashboard"
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
