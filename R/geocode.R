#' Geocode thesis locations
#'
#' The function creates two new columns, latitude and longitude,
#' based on a column containing the address of the thesis.
#'
#' @param data Data frame with thesis data
#' @return Data frame with latitude and longitude columns
#'
#' @export
geocode_thesis_locations <- function(data) {
  tidygeocoder::geocode(data, "location", method = 'osm', lat = latitude , long = longitude)
}
