library(sf)
source("./scripts/check_boundary_plot.R")


create_pop_matrix <- function(hex, admin, crs) {
  # Checking 'name_en' column in bd_admin data frame
  distinct_names <- admin %>%
    distinct(name_en)


  message(distinct_names)
  cat("\nAbove are the distinct names in the admin data. Press [enter] to continue, or type 'q' to abort: ")
  user_input <- readline()
  if (tolower(user_input) == 'q') stop('Aborted by user.')


  # Creating BD Boundary

  boundary <-
    admin %>%
    st_geometry %>%
    st_union %>%
    st_sf %>%
    st_make_valid()


  # check the boundary plot
  check_boundary_plot(hex, boundary)


  # setting the bd boundary as a bounding box
  bbox <- st_bbox(boundary)

  # finding the aspect ratio
  bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>%
    st_sfc(crs = crs)
  bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
    st_sfc(crs = crs)
  top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
    st_sfc(crs = crs)
  top_right <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
    st_sfc(crs = crs)


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
    hex,
    nx = floor(size * w_ratio) %>% as.numeric(),
    ny = floor(size * h_ratio) %>% as.numeric()
  )

  pop_matrix <- matrix(pop_raster$population,
                      nrow = floor(size * w_ratio),
                      ncol = floor(size * h_ratio))
                      
  pop_matrix

}
