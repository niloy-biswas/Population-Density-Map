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
source("./scripts/helpers.R")
source("./scripts/create_pop_matrix.R")
source("./scripts/plot_population_3d.R")
source("./scripts/annotate_population_map.R")

# ---- Color Palette for 3D Plot ----
palette_list <- create_palette()
color <- palette_list$color
subset_colors <- palette_list$subset_colors
palette_texture <- palette_list$tx

# Data
# load population 400m H3 hexagon

crs <-  3106 # EPSG code for Bangladesh

bd_hex <-
  st_read("Data/Kontur_Population_20231101.gpkg") %>%
  st_transform(crs)

# load population by administrative boundary
bd_admin <-
  st_read("Data/Bangladesh_boundaries_20230628.gpkg") %>%
  st_transform(crs)



pop_matrix <- create_pop_matrix(hex=bd_hex, admin =  bd_admin, crs=3106)

# BD-Population-Density.R
bangladesh_camera <- list(theta = 0.0,
                       phi = 70.0,
                       zoom = 0.55,
                       fov = 100.0)

plotfile <- "Plots/Dhaka_Benedictus_4R.png"

plot_population_3d(pop_matrix = pop_matrix,
                   palette_texture =  palette_texture,
                   subset_colors =  subset_colors,
                   camera= bangladesh_camera, 
                   outfile =  plotfile)

annotate_population_map(
  input_path = outfile,
  output_path = "Annotated_plot_bd_Benedictus_3.png",
  country_name = "Bangladesh",
  subset_colors = subset_colors
)
