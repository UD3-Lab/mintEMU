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
get_top_words_per_document <- function(data,
                                       top_n,
                                       title_col = "title",
                                       word_col = "word",
                                       min_count = 1) {
  title <- sym(title_col)
  word <- sym(word_col)

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
get_top_words_per_corpus <- function(data, top_n, word_col = "word") {
  word <- sym(word_col)

  data_top_n <- data |>
    dplyr::group_by(!!word) |>
    dplyr::count(!!word, sort = TRUE) |>
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
get_top_words_per_topic <- function(data,
                                    top_n,
                                    topic_col = "topic",
                                    beta_col = "beta") {
  topic <- rlang::sym(topic_col)
  beta <- rlang::sym(beta_col)

  data_top_terms <- data |>
    dplyr::group_by(!!topic) |>
    dplyr::top_n(top_n,!!beta) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!topic,-beta)

  data_top_terms
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

#' Get `tf-idf` statistic
#'
#' @param data Data frame containing colomns with titles, words and word counts
#' @param title_col Name of the title column of the input data frame
#' @param word_col Name of the word column of the input data frame
#'
#' @return A data frame including the `tf-idf` statistic for each word
#' @export
get_tf_idf <- function(data, title_col = NULL, word_col = NULL) {
  words_counts <- data |>
    count(!!rlang::sym(title_col), !!rlang::sym(word_col), sort = TRUE) |>
    ungroup()

  words_total <- words_counts |>
    group_by(!!rlang::sym(title_col)) |>
    summarise(total = sum(n))

  words_counts <- left_join(words_counts, words_total)

  words_counts <- words_counts |>
    bind_tf_idf(!!rlang::sym(word_col), !!rlang::sym(title_col), n) |>
    select(-total)

  words_counts
}
