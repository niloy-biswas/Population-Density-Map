
# Before running this script, please run setup_packages.R to install all required packages.


options(rgl.useNULL = FALSE)

# Packages

library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(mapview)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(rayrender)
library(magick)

# Source custom functions
source("functions.R")
source("create_pop_matrix.R")

# ---- Color Palette for 3D Plot ----
palette_list <- create_palette()
color <- palette_list$color
subset_colors <- palette_list$subset_colors
palette_texture <- palette_list$tx

# Data
# load population 400m H3 hexagon

bd_hex <-
  st_read("Data/Kontur_Population_20231101.gpkg") %>%
  st_transform(3106)

# load population by administrative boundary
bd_admin <-
  st_read("Data/Bangladesh_boundaries_20230628.gpkg") %>%
  st_transform(3106)

outfile <- "Plots/Dhaka_Benedictus_4R.png"


pop_matrix <- create_pop_matrix(hex=bd_hex, admin =  bd_admin, crs=3106)
plot_population_3d(pop_matrix, palette_texture, subset_colors, outfile)

annotate_population_map(
  input_path = outfile,
  output_path = "Annotated_plot_bd_Benedictus_3.png",
  country_name = "Bangladesh",
  subset_colors = subset_colors
)
