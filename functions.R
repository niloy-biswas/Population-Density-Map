# All custom function definitions for BD-Population-Density.R

# ---- Color Palette for 3D Plot ----
create_palette <- function(palette_name = "Benedictus", bias = 4.5) {
  color <- MetBrewer::met.brewer(name = palette_name, direction = -1)
  subset_colors <- color
  palette_texture <- grDevices::colorRampPalette(subset_colors, bias = bias)(256)
  swatchplot(palette_texture)
  swatchplot(subset_colors)
  list(color = color, subset_colors = subset_colors, tx = palette_texture)
}

# ---- 3D Plotting Function ----
plot_population_3d <- function(pop_matrix, palette_texture, subset_colors, outfile) {
  rgl::close3d()
  pop_matrix %>%
    height_shade(texture = palette_texture) %>%
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
