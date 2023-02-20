#' Geocode thesis locations
#'
#' The function creates two new columns, latitude and longitude,
#' based on a column containing the address of the thesis.
#'
#' @param data Data frame with thesis data
#' @return Data frame with latitude and longitude columns
#'
#' @export
geocode_thesis_locations <- function(data, location = "location") {
  if (!("latitude" %in% names(data)) | !("longitude" %in% names(data))) {
    geocoded_data <- tidygeocoder::geocode(
      data,
      address = !!sym(location),
      method = 'osm',
      lat = latitude ,
      long = longitude
    )
  }
}


#' Visualise thesis locations
#'
#' Show thesis locations on the map of the world.
#'
#' @param data Data frame with thesis data.
#'
#' @return A plot with thesis locations
#' @export
visualize_thesis_locations <- function(data) {
  world <- ggplot2::map_data("world")
  ggplot2::ggplot() +
    ggplot2::geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region),
      color = "black", fill = "lightgray", size = 0.1
    ) +
    ggplot2::geom_point(data = data, aes(longitude, latitude), color = "red") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
}
