# All custom function definitions for BD-Population-Density.R


#' Create a color palette and texture for 3D population density plots
#'
#' @param palette_name Name of the MetBrewer palette to use (default: "Benedictus")
#' @param bias Bias for color ramp interpolation (default: 4.5)
#' @return A list with elements: color (palette colors), subset_colors (subset), tx (palette texture)
#' @examples
#' palette_list <- create_palette()
create_palette <- function(palette_name = "Benedictus", bias = 4.5) {
  color <- MetBrewer::met.brewer(name = palette_name, direction = -1)
  subset_colors <- color
  palette_texture <- grDevices::colorRampPalette(subset_colors, bias = bias)(256)
  swatchplot(palette_texture)
  swatchplot(subset_colors)
  list(color = color, subset_colors = subset_colors, tx = palette_texture)
}



#' Safely read an image file, stopping with an error if not found
#'
#' @param path Path to the image file
#' @return An image object (from magick)
#' @examples
#' img <- safe_image_read("Plots/myplot.png")
safe_image_read <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  image_read(path)
}


