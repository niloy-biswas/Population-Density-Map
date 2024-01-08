options(rgl.useNULL = FALSE)

# Packages

require(tidyverse)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(MetBrewer)
require(colorspace)
require(rayrender)
require(magick)
require(extrafont)


# Data
# load population 400m H3 hexagon
in_hex <-
  st_read("Data/kontur_population_IN_20231101.gpkg") %>%
  st_transform(3106)

# load population by administrative boundary
in_admin <-
  st_read("Data/kontur_boundaries_IN_20230628.gpkg") %>%
  st_transform(3106)


# Checking 'name_en' column in admin data frame
distinct_names <- in_admin %>%
  distinct(name_en)
print(distinct_names)


# Creating India Boundary

in_boundary <-
  in_admin %>%
  st_geometry %>%
  st_union %>%
  st_sf %>%
  st_make_valid()


# check the boundary plot
ggplot(in_hex) +
  geom_sf(aes(fill = population),
          color = "gray66",
          linewidth = 0) +
  geom_sf(
    data = in_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )

# setting the India boundary as a bounding box
bbox <- st_bbox(in_boundary)

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

size = 2500
# size = 1000 * 3.5

pop_raster <- st_rasterize(
  in_hex,
  nx = floor(size * w_ratio) %>% as.numeric(),
  ny = floor(size * h_ratio) %>% as.numeric()
)

pop_matrix <- matrix(pop_raster$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))


# Create color palette from MetBrewer Library
color <- MetBrewer::met.brewer(name="OKeeffe1", direction = -1)
# library(RColorBrewer)
# color <- brewer.pal(n = 9, name = "Purples")

# Define the range of colors you want to exclude (for example, colors 5 to 10)
# exclude_range <- 7
# exclude_indices <- c(1)

# Create a subset of colors excluding the specified indices
# subset_colors <- color[-exclude_indices]

# Create a subset of colors excluding the specified range
# subset_colors <- color[setdiff(seq_along(color), exclude_range)]

# subset_colors <- color[2:9]
# subset_colors <- rev(color[1:9])


tx <- grDevices::colorRampPalette(color, bias = 4.5)(256)
swatchplot(tx)
swatchplot(color)

# plotting 3D

# Close any existing 3D plot before plotting another
rgl::close3d()

pop_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 50,
          solid = F,
          shadowdepth = 0.8)

# Adjusting Camera Angle
render_camera(theta = 0,
              phi = 75,
              zoom = 0.50,
              fov = 100
)

# To interactively view the 3D plot
rgl::rglwidget()

outfile <- glue::glue("Plots/India.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 60, #Degree
    lightaltitude = c(30, 80),
    #lightcolor = c(subset_colors[4], "white"),
    lightcolor = c("white", "white"),  # Set both lights to white
    lightintensity = c(600, 100),
    # width = 1980,
    # height = 1180,
    width = 2600,
    height = 2000,
    samples = 500
    #samples = 2
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}
