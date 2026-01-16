#' Plot a 3D population density map and save a high-quality image
#'
#' @param pop_matrix Matrix of population values
#' @param palette_texture Color texture for height shading
#' @param subset_colors Subset of palette colors (for annotation, etc.)
#' @param camera List with camera parameters: theta, phi, zoom, fov
#' @param outfile Output file path for the rendered image
#' @return None. Side effect: saves a PNG image to outfile
#' @examples
#' plot_population_3d(pop_matrix, palette_texture, subset_colors, camera, "output.png")
plot_population_3d <- function(pop_matrix, palette_texture, subset_colors, camera, outfile) {
  rgl::close3d()
  pop_matrix %>%
    height_shade(texture = palette_texture) %>%
    plot_3d(heightmap = pop_matrix,
            zscale = 250 / 4.5,
            solid = F,
            shadowdepth = 0)
  render_camera(theta = camera$theta,
                phi = camera$phi,
                zoom = camera$zoom,
                fov = camera$fov)
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

