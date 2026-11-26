# Teste isolado da aba "Campeões e Pódio"
# Funciona com o botão "Run App" do RStudio (que roda a partir da pasta code/)
# e também com source() a partir da raiz do projeto.

# Localiza a raiz do projeto e passa a trabalhar nela
raiz_projeto <- if (file.exists("code/01_read_data.R")) {
  "."
} else if (file.exists("01_read_data.R")) {
  ".."
} else {
  stop("Não encontrei code/01_read_data.R. Abra o project.Rproj e rode de novo.")
}
raiz_projeto <- normalizePath(raiz_projeto, winslash = "/")
setwd(raiz_projeto)

source("code/01_read_data.R", encoding = "UTF-8")
source("code/aba_podio.R", encoding = "UTF-8")

wc_df <- load_world_cup_data("data/raw")$worldcups

ui <- page_navbar(
  title = "Teste - Campeões e Pódio",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  aba_podio_ui("podio")
)

server <- function(input, output, session) {
  aba_podio_server("podio", wc_df)
}

shinyApp(ui, server)
