url_base <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/f422450f6519830f6ce0312e5294e3cf110c72ed/data/2022/2022-11-29"

download.file(
  file.path(url_base, "worldcups.csv"),
  destfile = "data/raw/worldcups.csv",
  mode = "wb"
)

download.file(
  file.path(url_base, "wcmatches.csv"),
  destfile = "data/raw/wcmatches.csv",
  mode = "wb"
)

list.files("data/raw")