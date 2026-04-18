library(sf)
source("./scripts/check_boundary_plot.R")


create_pop_matrix <- function(hex, admin, crs, size = 3500) {
  distinct_names <- admin %>% distinct(name_en)
  message(distinct_names)
  cat("\nAbove are the distinct names in the admin data. Press [enter] to continue, or type 'q' to abort: ")
  user_input <- readline()
  if (tolower(user_input) == "q") stop("Aborted by user.")

  boundary <-
    admin %>%
    st_geometry() %>%
    st_union() %>%
    st_sf() %>%
    st_make_valid()

  check_boundary_plot(hex, boundary)

  bbox <- st_bbox(boundary)

  bottom_left  <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>% st_sfc(crs = crs)
  bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>% st_sfc(crs = crs)
  top_left     <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>% st_sfc(crs = crs)

  width  <- st_distance(bottom_left, bottom_right)
  height <- st_distance(bottom_left, top_left)

  if (width > height) {
    w_ratio <- 1
    h_ratio <- height / width
  } else {
    h_ratio <- 1.1
    w_ratio <- width / height
  }

  pop_raster <- st_rasterize(
    hex,
    nx = floor(size * w_ratio) %>% as.numeric(),
    ny = floor(size * h_ratio) %>% as.numeric()
  )

  pop_matrix <- matrix(pop_raster$population,
                       nrow = floor(size * w_ratio),
                       ncol = floor(size * h_ratio))

  list(pop_matrix = pop_matrix, boundary = boundary, bbox = bbox)
}
