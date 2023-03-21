#' Extract top n words with the highest count per document
#'
#' Given a data frame with a column storing the words found in a document
#' identified with a title column and a positive number top_n, this function
#' returns the top n words with the highest count in each document.
#'
#' @param data Data frame with theses, containing a column with words.
#' @param top_n The number of top words to be returned by the function.
#' @param title_col The name of the column from the input data frame containing the title of the document.
#' @param word_col Name of the column from the input data frame containing all words.
#' @param min_count Minimum count to be considered. Words below this value will not be included in the returned data frame.
#'
#' @return A data frame with the top n words with a count above the given minimum.
#' @export
get_top_words_per_document <- function(data, top_n, title_col = NULL, word_col = NULL, min_count = 1) {
  data_top_n <- data |>
    dplyr::group_by(!!sym(title_col)) |>
    dplyr::count(!!sym(word_col), sort = TRUE) |>
    dplyr::slice_max(n, n = top_n) |>
    dplyr::filter(n > min_count) |>
    dplyr::ungroup()

  data_top_n
}

#' Extract top n words with the highest count across all documents
#'
#' Given a data frame with a column storing the words found in a document
#' and a positive number top_n, this function returns the top n words with the
#' highest count across all documents.
#'
#' @param data Data frame with theses, containing a column with words.
#' @param top_n The number of top words to be returned by the function.
#' @param word_col Name of the column from the input data frame containing all words.
#'
#' @return A data frame with the top n words across all documents.
#' @export
get_top_words_per_corpus <- function(data, top_n, word_col = NULL) {
  data_top_n <- data |>
    dplyr::group_by(!!sym(word_col)) |>
    dplyr::count(!!sym(word_col), sort = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice_max(n, n = top_n)

  data_top_n
}

#' Extract top n terms with the highest count across all topics
#'
#' @param data Data frame with topics, terms and beta statistic.
#' @param top_n The number of top words to be returned by the function.
#' @param topic_col Name of the column containing topics.
#' @param beta_col Name of the column containing the beta statistic for all terms.
#'
#' @return A data frame with the top n terms across all topics.
#' @export
get_top_words_per_topic <- function(data, top_n, topic_col = NULL, beta_col = NULL) {
  data_top_terms <- data |>
    dplyr::group_by(!!rlang::sym(topic_col)) |>
    dplyr::top_n(top_n, !!rlang::sym(beta_col)) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!rlang::sym(topic_col), -!!rlang::sym(beta_col))

  data_top_terms
}
