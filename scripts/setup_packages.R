# setup_packages.R
# Run once to install all required packages.

packages <- c(
  "sf", "tmap", "mapview", "stars", "rayshader", "MetBrewer",
  "rayrender", "magick", "tidyverse", "ggplot2", "colorspace",
  "showtext", "RColorBrewer", "crayon", "glue", "png", "raster",
  "jsonlite", "rnaturalearth", "rnaturalearthdata", # web export only
  "h3jsr", # web export of tiled countries (US, China, India)
  "R.utils" # download_kontur.R
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# rayrender sometimes needs separate install
if (!requireNamespace("rayrender", quietly = TRUE)) {
  install.packages("rayrender")
}
