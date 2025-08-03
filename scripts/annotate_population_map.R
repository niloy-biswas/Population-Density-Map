#' Annotate a population density map image with country and credits
#'
#' @param input_path Path to the input PNG image
#' @param output_path Path to save the annotated PNG image
#' @param country_name Name of the country (for annotation)
#' @param subset_colors Subset of palette colors (for text color)
#' @return None. Side effect: saves an annotated PNG image
#' @examples
#' annotate_population_map("input.png", "output.png", "Bangladesh", subset_colors)
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
