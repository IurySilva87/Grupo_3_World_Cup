# Funcoes auxiliares reutilizaveis do projeto

# 1 - Leiura segura de arquivos CSV

ler_csv_seguro <- function(caminho) {
  if (!file.exists(caminho)) {
    stop("Arquivo nao encontrado: ", caminho, call. = FALSE)
  }
  read.csv(caminho, stringsAsFactors = FALSE, encoding = "UTF-8")
}