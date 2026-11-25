base_dir <- if (exists("raiz_projeto")) raiz_projeto else "."
raw_dir  <- file.path(base_dir, "data", "raw")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

arquivos_wc <- c(
  "worldcups.csv" = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv",
  "wcmatches.csv" = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv"
)

for (nome in names(arquivos_wc)) {
  destino <- file.path(raw_dir, nome)
  if (!file.exists(destino)) {
    download.file(arquivos_wc[[nome]], destfile = destino, mode = "wb")
  }
}

load_world_cup_data <- function(path_raw = raw_dir) {
  dir.create(path_raw, recursive = TRUE, showWarnings = FALSE)
  
  for (nome in names(arquivos_wc)) {
    destino <- file.path(path_raw, nome)
    if (!file.exists(destino)) {
      download.file(arquivos_wc[[nome]], destfile = destino, mode = "wb")
    }
  }
  
  worldcups_path <- file.path(path_raw, "worldcups.csv")
  wcmatches_path <- file.path(path_raw, "wcmatches.csv")
  
  if (!file.exists(worldcups_path)) {
    stop("Arquivo nao encontrado: ", worldcups_path)
  }
  if (!file.exists(wcmatches_path)) {
    stop("Arquivo nao encontrado: ", wcmatches_path)
  }
  
  list(
    worldcups = utils::read.csv(worldcups_path, stringsAsFactors = FALSE),
    wcmatches = utils::read.csv(wcmatches_path, stringsAsFactors = FALSE)
  )
}