
#' Plot a 3D population density map and save a high-quality image
#'
#' @param pop_matrix Matrix of population values
#' @param palette_texture Color texture for height shading
#' @param subset_colors Subset of palette colors
#' @param camera List: theta, phi, zoom, fov
#' @param outfile Output PNG path
#' @param zscale Vertical exaggeration scale
#' @param light_dir Light direction in degrees
#' @param width Render width in pixels
#' @param height Render height in pixels
#' @param solid Render solid base (default: FALSE)
#' @param shadow_depth Shadow depth (default: 0)
#' @param samples Ray samples for quality (default: 500)
#' @param parallel Use parallel CPU rendering (default: TRUE)
#' @param boundary Optional sf boundary for country outline overlay
#' @param bbox Optional bbox of the raster for overlay alignment
plot_population_3d <- function(pop_matrix, palette_texture, subset_colors, camera, outfile,
                               zscale = 55, light_dir = 55,
                               light_altitude = c(55, 80),
                               width = 1400, height = 1580,
                               solid = FALSE, shadow_depth = 0,
                               solidcolor = "grey20",
                               samples = 500, parallel = TRUE,
                               boundary = NULL, bbox = NULL) {
  rgl::close3d()

  shaded <- pop_matrix %>% height_shade(texture = palette_texture)

  if (!is.null(boundary) && !is.null(bbox)) {
    extent_vec <- c(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
    border_overlay <- tryCatch(
      generate_polygon_overlay(
        geometry  = boundary,
        extent    = extent_vec,
        heightmap = pop_matrix,
        linecolor = "black",
        linewidth = 3,
        palette   = NA
      ),
      error = function(e) { message("Border overlay failed: ", e$message); NULL }
    )
    if (!is.null(border_overlay))
      shaded <- add_overlay(shaded, border_overlay, alphalayer = 0.9,
                            alphacolor = NULL, rescale_original = FALSE)
  }

  shaded %>%
    plot_3d(
      heightmap      = pop_matrix,
      zscale         = zscale,
      solid          = solid,
      solidcolor     = solidcolor,
      solidlinecolor = solidcolor,
      shadowdepth    = shadow_depth,
      background     = "white"
    )

  render_camera(
    theta = camera$theta,
    phi   = camera$phi,
    zoom  = camera$zoom,
    fov   = camera$fov
  )

  rgl::rglwidget()

  if (!file.exists(outfile)) png::writePNG(matrix(1), target = outfile)

  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")

  render_highquality(
    filename       = outfile,
    interactive    = FALSE,
    lightdirection = light_dir,
    lightaltitude  = light_altitude,
    lightcolor     = c("white", "white"),
    lightintensity = c(600, 100),
    width          = width,
    height         = height,
    samples        = samples,
    parallel       = parallel
  )


  cat(crayon::cyan(Sys.time() - start_time), "\n")
}
