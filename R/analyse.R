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
    dplyr::group_by(!!rlang::sym(title_col)) |>
    dplyr::count(!!rlang::sym(word_col), sort = TRUE) |>
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

#' Convert data frame with word frequencies into document term matrix
#'
#' @param data Data frame containing colomns with titles, words and word counts
#' @param title_col Name of the title column of the input data frame
#' @param word_col Name of the word column of the input data frame
#'
#' @return A document term matrix
#' @export
convert_to_dtm <- function(data, title_col = NULL, word_col = NULL) {
  data_dtm <- data |>
    dplyr::count(!!rlang::sym(title_col), !!rlang::sym(word_col), sort = TRUE) |>
    tidytext::cast_dtm(!!rlang::sym(title_col), !!rlang::sym(word_col), n)

  data_dtm
}

#' Get ngrams from a corpus
#'
#' @param data Data frame of documents containing a text column
#' @param n The number of words to include in the token (e.g., n = 2 for bigram)
#' @param text_col The name of the column where the text to be tokenised is located
#' @param title_col The name of the title column
#' @param stem If `TRUE` create additional columns with stemmed version of tokens
#'
#' @return A data frame with an "ngram" column and columns for its constituent words
#' @export
get_ngrams <-
  function(data,
           n,
           title_col = NULL,
           text_col = NULL,
           stem = FALSE) {
    data_ngrams <- data |>
    select(title_col, text_col) |>
    tidytext::unnest_tokens(ngram, text_col, token = "ngrams", n = n) |>
    tidyr::separate(ngram, into = paste("w" , 1:n, sep = "_")
                    sep = " ", remove = FALSE) |>
    dplyr::anti_join(tidytext::stop_words, by = c("first" = "word")) |>
    dplyr::anti_join(tidytext::stop_words, by = c("second" = "word")) |>
    dplyr::anti_join(tidytext::stop_words, by = c("third" = "word")) |>
    dplyr::filter(str_detect(first, "[a-z]") &
             stringr::str_detect(second, "[a-z]"))

  if (stem) {
    data_ngrams <- data_ngrams |>
      dplyr::mutate(first_stem = SnowballC::wordStem(words = first,
                                                     language = "porter")) |>
      dplyr::mutate(second_stem = SnowballC::wordStem(words = second,
                                                      language = "porter")) |>
      dplyr::mutate(third_stem = SnowballC::wordStem(words = third,
                                                     language = "porter"))
  }

  data_ngrams

}
