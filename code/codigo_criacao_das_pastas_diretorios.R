dirs <- c("data/raw", "data/processed", "code", "figs", "results", "reports")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

criar_se_nao_existe <- function(caminho, linhas) {
  if (!file.exists(caminho)) {
    writeLines(linhas, con = caminho)
  }
}

criar_se_nao_existe(
  "README.md",
  c(
    "# Projeto World Cup - UFPR",
    "",
    "Projeto de análise de dados em R utilizando RStudio, R Markdown, Shiny e controle de versão via Git/GitHub.",
    "",
    "## Estrutura do projeto",
    "",
    "- `data/raw/`: dados brutos (não versionados)",
    "- `data/processed/`: dados tratados e prontos para análise",
    "- `code/`: scripts de leitura (`01_read_data.R`), estatísticas (`02_basic_stats.R`) e funções (`99_my_functions.R`)",
    "- `figs/`: gráficos e figuras geradas",
    "- `results/`: tabelas e saídas analíticas",
    "- `reports/`: relatórios reprodutíveis em R Markdown",
    "- `app.R`: aplicação interativa Shiny"
  )
)

criar_se_nao_existe(
  "code/01_read_data.R",
  c(
    "# Leitura e importacao dos dados brutos da World Cup",
    "",
    "source(\"code/99_my_functions.R\")",
    "",
    "load_world_cup_data <- function(path) {",
    "  stop(\"Implementar leitura e limpeza dos dados em: \", path)",
    "}"
  )
)

criar_se_nao_existe(
  "code/02_basic_stats.R",
  c(
    "# Estatisticas basicas e analise exploratoria",
    "",
    "source(\"code/99_my_functions.R\")"
  )
)

criar_se_nao_existe(
  "code/99_my_functions.R",
  c(
    "# Funcoes auxiliares reutilizaveis do projeto"
  )
)

criar_se_nao_existe(
  "app.R",
  c(
    "library(shiny)",
    "library(bslib)",
    "",
    "ui <- page_sidebar(",
    "  title = \"Projeto World Cup - UFPR\",",
    "  sidebar = sidebar(\"Filtros\"),",
    "  \"Conteudo principal do dashboard\"",
    ")",
    "",
    "server <- function(input, output, session) {",
    "}",
    "",
    "shinyApp(ui, server)"
  )
)

criar_se_nao_existe(
  "reports/relatorio_world_cup.Rmd",
  c(
    "---",
    "title: \"Análise Exploratória - World Cup\"",
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)",
    "```",
    "",
    "## Resumo",
    "",
    "Relatório inicial do projeto."
  )
)

ignorar_extra <- c(
  "",
  "# Dados brutos e saidas locais do projeto",
  "data/raw/*.csv",
  "data/raw/*.xlsx",
  "figs/*.png",
  "figs/*.jpg",
  "figs/*.pdf",
  "results/*.csv",
  "results/*.rds",
  "reports/*.html",
  "reports/*_files/"
)

if (file.exists(".gitignore")) {
  atual <- readLines(".gitignore", warn = FALSE)
  novas <- setdiff(ignorar_extra, atual)
  if (length(novas) > 0) {
    writeLines(c(atual, novas), con = ".gitignore")
  }
} else {
  writeLines(ignorar_extra, con = ".gitignore")
}

cat("Estrutura mesclada com sucesso em:", getwd(), "\n")