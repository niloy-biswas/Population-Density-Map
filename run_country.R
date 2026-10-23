options(rgl.useNULL = TRUE)

library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(MetBrewer)
library(RColorBrewer)
library(colorspace)
library(rayrender)
library(magick)
library(showtext)

source("./scripts/helpers.R")
source("./scripts/check_boundary_plot.R")
source("./scripts/create_pop_matrix.R")
source("./scripts/plot_population_3d.R")
source("./scripts/annotate_population_map.R")
source("./scripts/countries.R")


#' Run the full population density map pipeline for a country
#'
#' @param country_key Key from the countries list (e.g. "bangladesh", "nepal")
#' @examples
#' run_country("bangladesh")
#' run_country("israel_palestine")
run_country <- function(country_key) {
  cfg <- countries[[country_key]]
  if (is.null(cfg)) {
    stop(paste(
      "Unknown country key:", country_key,
      "\nAvailable:", paste(names(countries), collapse = ", ")
    ))
  }
  if (is.null(cfg$crs)) {
    stop(cfg$name, " is web-explorer only: add crs, size, palette, zscale, ",
         "camera, ... to its countries.R entry to render it")
  }

  message("Loading data for: ", cfg$name)
  hex <- st_read(cfg$hex_file) %>% st_transform(cfg$crs)
  admin <- st_read(cfg$admin_file) %>% st_transform(cfg$crs)

  pop_data <- create_pop_matrix(
    hex = hex, admin = admin,
    crs = cfg$crs, size = cfg$size
  )
  pop_matrix <- pop_data$pop_matrix
  boundary <- pop_data$boundary
  bbox <- pop_data$bbox

  pal <- do.call(create_palette, cfg$palette)

  slug <- gsub("[^A-Za-z0-9]", "_", cfg$name)
  plot_file <- paste0("output/plots/", slug, ".png")
  ann_file <- paste0("output/annotated/", slug, "_population_density.png")

  dir.create("output/plots", showWarnings = FALSE, recursive = TRUE)
  dir.create("output/annotated", showWarnings = FALSE, recursive = TRUE)

  plot_population_3d(
    pop_matrix      = pop_matrix,
    palette_texture = pal$tx,
    subset_colors   = pal$subset_colors,
    camera          = cfg$camera,
    outfile         = plot_file,
    zscale          = cfg$zscale,
    light_dir       = cfg$light_dir,
    width           = cfg$width,
    height          = cfg$height,
    solid           = cfg$solid,
    solidcolor      = if (!is.null(cfg$solidcolor)) cfg$solidcolor else "grey20",
    shadow_depth    = cfg$shadow_depth,
    boundary        = if (isTRUE(cfg$show_border)) boundary else NULL,
    bbox            = if (isTRUE(cfg$show_border)) bbox else NULL,
    samples         = if (!is.null(cfg$samples)) cfg$samples else 500,
    light_altitude  = if (!is.null(cfg$light_altitude)) cfg$light_altitude else c(55, 80)
  )

  annotate_population_map(
    input_path    = plot_file,
    output_path   = ann_file,
    country_name  = cfg$name,
    subset_colors = pal$subset_colors
  )

  message("Done. Output saved to: ", ann_file)
}


# ---- Run a country ----
# Uncomment one line below and source this file, or call run_country() from the console.

# run_country("bangladesh")
# run_country("bhutan")
# run_country("nepal")
# run_country("india")
# run_country("myanmar")
# run_country("maldives")
# run_country("sri_lanka")
# run_country("israel")
# run_country("palestine")
# run_country("israel_palestine")
# run_country("finland")
