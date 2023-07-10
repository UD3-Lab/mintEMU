#' EMU theses raw text data
#'
#' A data frame with text of theses from the EMU program.
#' The data frame can be joined with the `emu_metadata` data frame via ID column.
#'
#' @format ## `emu_raw`
#' A data frame with 83 rows and 2 columns:
#' \describe{
#'   \item{ID}{ID of the thesis}
#'   \item{text_raw}{Raw text of the thesis.
#'   The text has been anonymyzed and Latin special characters have been replaced with 26-letter alphabet counterparts}
#'
#' @source <https://repository.tudelft.nl/islandora/search/?collection=education>
"emu_raw"


#' EMU theses  metadata
#'
#' A data frame with metadata of theses from the EMU program.
#' The data frame can be joined with the `emu_raw` data frame via ID column.
#'
#' @format ## `emu_metadata`
#' A data frame with 83 rows and 8 columns:
#' \describe{
#'   \item{ID}{ID of the thesis}
#'   \item{graduation_year}{Raw text of the thesis.}
#'   \item{graduation_semester}{Raw text of the thesis.}
#'   \item{title}{Title of the thesis.}
#'   \item{location}{The location that the thesis is focused on.}
#'   \item{latitude}{Latitude of the thesis location.}
#'   \item{longitude}{Longitude of the thesis location.}
#'   \item{abstract}{Abstract of the thesis.}
#'
#' @source <https://repository.tudelft.nl/islandora/search/?collection=education>
"emu_raw"
