
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
library(extrafont)


# Data
# load population 400m H3 hexagon

bd_hex <-
  st_read("Data/Kontur_Population_20231101.gpkg") %>%
  st_transform(3106)

# load population by administrative boundary
bd_admin <-
  st_read("Data/Bangladesh_boundaries_20230628.gpkg") %>%
  st_transform(3106)


# Checking 'name_en' column in bd_admin data frame
distinct_names <- bd_admin %>%
  distinct(name_en)
print(distinct_names)


# Creating BD Boundary

bd_boundary <-
  bd_admin %>%
  st_geometry %>%
  st_union %>%
  st_sf %>%
  st_make_valid()


# check the boundary plot
ggplot(bd_hex) +
  geom_sf(aes(fill = population),
          color = "gray66",
          linewidth = 0) +
  geom_sf(
    data = bd_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )

# setting the bd boundary as a bounding box
bbox <- st_bbox(bd_boundary)

# finding the aspect ratio
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 3106)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 3106)
top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 3106)
top_right <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 3106)



width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if(width > height) {
  w_ratio = 1
  h_ratio = height / width
  
} else {
  h_ratio = 1.1
  w_ratio = width / height
}

# convert to raster to convert to matrix
# For interactively checking the 3D plot set the size low it'll help to render in real time.
# For saving the 3D image in better Quality change it to higher.

# size = 100
size = 1000 * 3.5

pop_raster <- st_rasterize(
  bd_hex,
  nx = floor(size * w_ratio) %>% as.numeric(),
  ny = floor(size * h_ratio) %>% as.numeric()
)

pop_matrix <- matrix(pop_raster$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))


# ---- Color Palette for 3D Plot ----
create_palette <- function(palette_name = "Benedictus", bias = 4.5) {
  color <- MetBrewer::met.brewer(name = palette_name, direction = -1)
  subset_colors <- color
  tx <- grDevices::colorRampPalette(subset_colors, bias = bias)(256)
  swatchplot(tx)
  swatchplot(subset_colors)
  list(color = color, subset_colors = subset_colors, tx = tx)
}

palette_list <- create_palette()
color <- palette_list$color
subset_colors <- palette_list$subset_colors
tx <- palette_list$tx

# ---- 3D Plotting Function ----
plot_population_3d <- function(pop_matrix, tx, subset_colors, outfile) {
  rgl::close3d()
  pop_matrix %>%
    height_shade(texture = tx) %>%
    plot_3d(heightmap = pop_matrix,
            zscale = 250 / 4.5,
            solid = F,
            shadowdepth = 0)
  render_camera(theta = 0,
                phi = 70,
                zoom = 0.55,
                fov = 100)
  rgl::rglwidget()
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 55,
    lightaltitude = c(30, 80),
    lightcolor = c("white", "white"),
    lightintensity = c(600, 100),
    width = 1400,
    height = 1580,
    samples = 500
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

# ---- File Existence Check Helper ----
safe_image_read <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  image_read(path)
}
 




# --------------------------- Annotate Function
annotate_population_map <- function(input_path, output_path, country_name = "Bangladesh", subset_colors) {
  library(showtext)
  pop_raster <- safe_image_read(input_path)
  text_color <- darken(subset_colors[3], .4)
  swatchplot(text_color)
  showtext_auto()
  pop_raster %>%
    image_annotate(country_name,
                   gravity = "northeast",
                   location = "+50+50",
                   color = text_color,
                   size = 120,
                   font = "Philosopher",
                   weight = 700) %>%
    image_annotate("POPULATION DENSITY MAP",
                   gravity = "northeast",
                   location = "+50+175",
                   color = text_color,
                   size = 28.5,
                   font = "Philosopher",
                   weight = 500) %>%
    image_annotate("Visualization by: Niloy Biswas with Rayshader\nData: Kontur Population 2023",
                   gravity = "southwest",
                   location = "+20+20",
                   color = alpha(text_color, .8),
                   font = "Philosopher",
                   size = 25) %>%
    image_write(output_path, format = "png", quality = 100)
}

# Example usage:
outfile <- "Plots/Dhaka_Benedictus_4R.png"
plot_population_3d(pop_matrix, tx, subset_colors, outfile)
annotate_population_map(
  input_path = outfile,
  output_path = "Annotated_plot_bd_Benedictus_3.png",
  country_name = "Bangladesh",
  subset_colors = subset_colors
)
