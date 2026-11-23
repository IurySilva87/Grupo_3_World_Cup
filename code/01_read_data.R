# Leitura e importacao dos dados brutos da World Cup

source("code/99_my_functions.R")

load_world_cup_data <- function(raw_dir = "data/raw") {
  worldcups <- ler_csv_seguro(file.path(raw_dir, "worldcups.csv"))
  wcmatches <- ler_csv_seguro(file.path(raw_dir, "wcmatches.csv"))
  
  names(worldcups) <- tolower(trimws(names(worldcups)))
  names(wcmatches) <- tolower(trimws(names(wcmatches)))
  
  if ("year" %in% names(worldcups)) {
    worldcups$year <- as.integer(worldcups$year)
  }
  if ("year" %in% names(wcmatches)) {
    wcmatches$year <- as.integer(wcmatches$year)
  }
  
  list(
    worldcups = worldcups,
    wcmatches = wcmatches
  )
}