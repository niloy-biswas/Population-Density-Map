# setup_packages.R
# Run this script once to install all required packages for BD-Population-Density.R

packages <- c(
  "sf", "tmap", "mapview", "stars", "rayshader", "MetBrewer", "rayrender", "extrafont", "magick", "tidyverse", "ggplot2", "colorspace", "showtext"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# rayrender sometimes needs to be installed separately
if (!requireNamespace("rayrender", quietly = TRUE)) {
  install.packages("rayrender")
}
# Import Philosopher font for extrafont (run only once)
library(extrafont)
font_import(pattern = "Philosopher", prompt = FALSE)
