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

#' Get topics for a given data frame of documents and words
#'
#' @param data Data frame with theses, containing a column with words.
#' @param k Number of desired clusters.
#' @param matrix Statistics to be computed. Can be "beta" or "gamma"
#' @param title_col The name of the column from the input data frame containing the title of the document.
#' @param word_col Name of the column from the input data frame containing all words.
#' @param seed Seed to control pseudorandomisation.
#'
#' @return An LDA model object.
#' @export
get_topics <-
  function(data,
           k = 2,
           seed = 2023,
           title_col = "title",
           word_col = "word") {
    stopifnot("k must be an integer of at least 2" = k >= 2)

    # The {topicmodels} package requires a document-term matrix as input
    dtm <- convert_to_dtm(data,
                          title_col = title_col,
                          word_col = word_col)

    # We will use the Latent Dirichlet Allocation (LDA) algorithm for topic modeling
    # In LDA, each document is a mixture of topics and each topic is a mixture of words
    lda <- topicmodels::LDA(dtm,
                            k = k,
                            control = list(seed = seed))
}
