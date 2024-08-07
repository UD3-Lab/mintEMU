#' Get word counts for corpus
#'
#' @param data A data frame comprising a corpus of texts
#' @param text_col Name of the column in which the text is stored
#'
#' @return A numeric vector of word counts, one for each document in the corpus
#' @export
get_word_counts <- function(data, text_col = NULL) {
  text_col <- rlang::sym(text_col)

  word_counts <- vector(mode = "numeric", length = nrow(data))
  for (i in 1:nrow(data)) {
    word_counts[i] <-
      tidytext::unnest_tokens(tbl = data[i,], output = word, input = !!text_col) |> nrow()
  }

  word_counts
}

#' Get overview of theses in the corpus
#'
#' @param data A data frame comprising counts of theses included in the corpus
#'
#' @return A data frame with an overview of the theses
#' @export
get_theses_overview <- function(data) {
  theses_overview <- data |>
    dplyr::select(grad_year, n_total, n_permissions) |>
    dplyr::mutate(
      grad_year = as.factor(grad_year),
      `% of theses per period` = scales::label_percent()(round(n_permissions / n_total, 4))) |>
    dplyr::bind_rows(
      data.frame(
        grad_year = "2007-2021",
        n_permissions = nrow(emu),
        n_total = sum(theses_all_per_year$n_total),
        `% of theses per period` = scales::label_percent()(nrow(emu) / sum(theses_all_per_year$n_total)),
        check.names = FALSE)) |>
    dplyr::filter(!is.na(grad_year)) |>
    dplyr::rename(`No. theses` = n_total,
                  `Graduation year` = grad_year,
                  `No. theses included` = n_permissions)
}
