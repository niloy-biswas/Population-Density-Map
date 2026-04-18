
#' Create a color palette and texture for 3D population density plots
#'
#' @param source "metbrewer" or "rcolorbrewer"
#' @param name Palette name
#' @param direction Direction for MetBrewer palettes (default: -1)
#' @param n Number of colors for RColorBrewer palettes (default: 9)
#' @param subset_indices Integer vector to subset colors (NULL = use all)
#' @param reverse_subset Reverse the subset (default: FALSE)
#' @param bias Bias for color ramp interpolation (default: 4.5)
create_palette <- function(source = "metbrewer", name = "Benedictus",
                           direction = -1, n = 9,
                           subset_indices = NULL, reverse_subset = FALSE,
                           bias = 4.5) {
  if (source == "metbrewer") {
    color <- MetBrewer::met.brewer(name = name, direction = direction)
  } else {
    color <- RColorBrewer::brewer.pal(n = n, name = name)
  }

  subset_colors <- if (!is.null(subset_indices)) color[subset_indices] else color
  if (reverse_subset) subset_colors <- rev(subset_colors)

  tx <- grDevices::colorRampPalette(subset_colors, bias = bias)(256)
  list(color = color, subset_colors = subset_colors, tx = tx)
}


#' Safely read an image file, stopping with an error if not found
safe_image_read <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  image_read(path)
}
