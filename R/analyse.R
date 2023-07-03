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

#' Sliding window
#'
#' Sliding window function used to calculate skipgram probabilities,
#' that is, how often each word occurs near each other word.
#'
#' @param tbl A vector of tokens.
#' @param window_size A small window size of 3-4 shows how a word is used, while a larger window size of, say, 10 gives more information about the domain topic.
#'
#' @return
#' @export
#'
slide_windows <- function(tbl, window_size) {
  # Iterate through an input vector with a fixed window size
  skipgrams <- slider::slide(
    tbl,
    ~.x,
    .after = window_size - 1,
    .step = 1L,
    .complete = TRUE  # Only evaluate function on complete windows
  )

  # Run even when unsuccessful
  safe_mutate <- purrr::safely(mutate)

  # Map safe_mutate to skipgrams
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))

  out |>
    purrr::transpose() |>  # Transpose `result` and `error` into columns,
    purrr::pluck("result") |>  # only keep `result`,
    plyr::compact() |>  # remove any NULL values [is this for the last windows?],
    dplyr::bind_rows()  # and bind all rows into a single data frame [?].
}

#' Pointwise mutual information of pairs of items
#'
#' @param data Data frame of theses
#' @param title_col Name of the title column
#' @param text_col Name of the text column to be tokenised
#' @param parallel If `parallel = TRUE`, compute in parallel
#'
#' @return Data frame with two items and their co-occurrence
#' @export
#'
get_pmi <- function(data,
                    title_col = NULL,
                    text_col = NULL,
                    parallel = TRUE,
                    window_size = 4L) {

  # To reduce computation time, we use the {furrr}
  # and underlying {future} packages for parallel processing
  if (parallel){
    future::plan(multisession)
  }

  # Prepare tidy data frame for word embeddings
  tidy_emu_theses <- data |>
    dplyr::select(!!rlang::sym(title_col), !!rlang::sym(text_col)) |>
    tidytext::unnest_tokens(word, !!rlang::sym(text_col)) |>
    dplyr::add_count(word) |>
    dplyr::filter(n >= 50) |>
    dplyr::select(-n)

  # Create a nested data frame with one row per thesis
  nested_words <- tidy_emu_theses |>
    tidyr::nest(words = c(word))

  # To calculate how often words occur on their own or together with other words
  # we use point-wise mutual information (PMI)
  tidy_pmi <- nested_words |>
    dplyr::mutate(words = furrr::future_map(words, slide_windows, window_size)) |>
    tidyr::unnest(words) |>
    tidyr::unite(window_id, title, window_id) |>
    widyr::pairwise_pmi(word, window_id)

  tidy_pmi

}

#' Get nearest neighbours for a given token
#'
#' @param df Data frame with word vectors
#' @param token Word for which the nearest neighbours are calculated
#'
#' @return Data frame with nearest neighbours
#' @export
#'
nearest_neighbors <- function(df, token) {
  df |>
    widyr::widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))

        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) |>
    dplyr::select(-item2)
}

#' Visualise first n components
#'
#' @param word_vectors Word vectors
#' @param dim Number of dimensions to visualise
#' @param top_n Number of top terms to show for each dimension
#'
#' @return ggplot object
#' @export
#'
visualise_first_n_components <- function(word_vectors, dim = 24, top_n = 12) {

  word_vectors |>
    dplyr::filter(dimension <= dim) |>
    dplyr::group_by(dimension) |>
    dplyr::top_n(top_n, abs(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(item1 = tidytext::reorder_within(item1, value, dimension)) |>
    ggplot2::ggplot(aes(item1, value, fill = dimension)) +
    ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(~dimension, scales = "free_y", ncol = 4) +
    tidytext::scale_x_reordered() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Value",
      title = "First 24 principal components for text of theses",
      subtitle = "Top words contributing to the components that explain the most variation"
    )

}
