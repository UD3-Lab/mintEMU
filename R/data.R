#' EMU theses raw text data
#'
#' A data frame with text of theses from the EMU program.
#' The data frame can be joined with the `emu_metadata` data frame via ID column.
#'
#' @format ## `emu_raw`
#' A data frame with 68 rows and 2 columns.
#' \describe{
#'   \item{ID}{ID of the thesis}
#'   \item{text_raw}{Raw text of the thesis.
#'   The text has been anonymized, Latin special characters have been replaced with 26-letter alphabet counterparts,
#'   hyphenation, line breaks and occurrences of the title, often found in the header or footer of theses layouts, have been removed.}
"emu_raw"


#' EMU theses  metadata
#'
#' A data frame with metadata of theses from the EMU program.
#' The data frame can be joined with the `emu_raw` data frame via ID column.
#'
#' @format ## `emu_metadata`
#' A data frame with 68 rows and 10 columns:
#' \describe{
#'   \item{ID}{ID of the thesis}
#'   \item{grad_year}{The year when the thesis was published.}
#'   \item{grad_sem}{The semester when the student graduated.}
#'   \item{full_title}{Full title of the thesis.}
#'   \item{title}{Title of the thesis.}
#'   \item{subtitle}{Subtitle of the thesis.}
#'   \item{loc}{The location that the thesis is focused on.}
#'   \item{lat}{Latitude of the thesis location geocoded from `location`.}
#'   \item{long}{Longitude of the thesis location geocoded from `location`.}
#'   \item{abstract}{Abstract of the thesis.}
#'
#' @source <https://repository.tudelft.nl/islandora/search/?collection=education>
"emu_metadata"
