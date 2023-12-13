install.packages("sf",dependencies=TRUE)
install.packages("tmap",dependencies=TRUE)
install.packages("mapview",dependencies=TRUE)
install.packages("stars",dependencies=TRUE)
install.packages("rayshader",dependencies=TRUE)
install.packages("MetBrewer",dependencies=TRUE)
install.packages("rayrender")
install.packages("extrafont",dependencies=TRUE)
install.packages("magick",dependencies=TRUE)


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


----------------------------------



# Create color palette from MetBrewer Library
color <- MetBrewer::met.brewer(name="Benedictus", direction = -1)

# Define the range of colors you want to exclude (for example, colors 5 to 10)
# exclude_range <- 7
# exclude_indices <- c(1)

# Create a subset of colors excluding the specified indices
# subset_colors <- color[-exclude_indices]

# Create a subset of colors excluding the specified range
# subset_colors <- color[setdiff(seq_along(color), exclude_range)]

# subset_colors <- color[6:8]
# subset_colors <- rev(color[1:6])


tx <- grDevices::colorRampPalette(subset_colors, bias = 4.5)(256)
swatchplot(tx)
swatchplot(subset_colors)

# plotting 3D

# Close any existing 3D plot before plotting another
rgl::close3d()

pop_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 250 / 4.5,
          solid = F,
          shadowdepth = 0)

# Adjusting Camera Angle
render_camera(theta = 0,
              phi = 70,
              zoom = 0.55,
              fov = 100
)

# To interactively view the 3D plot
rgl::rglwidget()

outfile <- glue::glue("Plots/Dhaka_Benedictus_4.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 55, #Degree
    lightaltitude = c(30, 80),
    #lightcolor = c(subset_colors[4], "white"),
    lightcolor = c("white", "white"),  # Set both lights to white
    lightintensity = c(600, 100),
    # width = 1980,
    # height = 1180,
    width = 1400,
    height = 1580,
    samples = 500
    #samples = 2
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}
 


# ---------------------------Anotate
# Install and load the showtext package
install.packages("showtext")
library(showtext)
install.packages("extrafont")
library(extrafont)
font_import(pattern = "Philosopher")

pop_raster <- image_read("final_plot_bd_Benedictus_3.png")

text_color <- darken(subset_colors[3], .4)
swatchplot(text_color)


# Automatically enable font support
showtext_auto()

# Download and register the Philosopher font from Google Fonts
font_add_google("Philosopher", regular = "400", bold = "700")

pop_raster %>%
  image_annotate("Bangladesh",
                 gravity = "northeast",
                 location = "+50+50",
                 color = text_color,
                 size = 120,
                 font = "Philosopher",
                 weight = 700,
                 # degrees = 0,
  ) %>%
  image_annotate("POPULATION DENSITY MAP",
                 gravity = "northeast",
                 location = "+50+175",
                 color = text_color,
                 size = 28.5,
                 font = "Philosopher",  # Corrected font name
                 weight = 500,
                 # degrees = 0,
  ) %>%
  image_annotate("Visualization by: Niloy Biswas with Rayshader\nData: Kontur Population 2023",
                 gravity = "southwest",
                 location = "+20+20",
                 color = alpha(text_color, .8),
                 font = "Philosopher",  # Corrected font name
                 size = 25,
                 # degrees = 0,
  ) %>%
  image_write("Annotated_plot_bd_Benedictus_3.png", format = "png", quality = 100)
