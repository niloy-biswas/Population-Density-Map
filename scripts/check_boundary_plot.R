library(ggplot2)
library(sf)
##' Plot population hexagons with administrative boundary overlay
##'
##' @param hex An sf object of population hexagons (must have a 'population' column)
##' @return A ggplot object showing the population density and boundary overlay
##' @details Assumes a global variable 'boundary' exists for the overlay
##' @examples
##' check_boundary_plot(bd_hex)

check_boundary_plot <- function(hex, boundary) {
  ggplot(hex) +
    geom_sf(aes(fill = boundary$population),
            color = "gray66",
            linewidth = 0) +
    geom_sf(
      data = boundary,
      fill = NA,
      color = "black",
      linetype = "dashed",
      linewidth = 1
    )
}