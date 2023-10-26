#' Get ngrams from a corpus
#'
#' @param data Data frame of documents containing a text column
#' @param n The number of words to include in the token (e.g., n = 2 for bigrams)
#' @param text_col The name of the text column where to be tokenised
#' @param id_col The name of the ID column
#' @param stem If `TRUE` create additional columns with stemmed tokens
#' @param lemma If `TRUE` create additional columns with lemmatised tokens
#'
#' @return A data frame with an "ngram" column and columns for its constituent words
#' @export
get_ngrams <- function(data,
                       n,
                       text_col = NULL,
                       id_col = NULL,
                       rm_n_short_words = 2,
                       min_freq = 5,
                       stem = FALSE,
                       lemma = FALSE) {

  # Create ngrams column and separate columns for each of the n terms
  data_ngrams <- data |>
    dplyr::select(id_col, text_col) |>
    tidytext::unnest_tokens(ngram, text_col, token = "ngrams", n = n) |>
    tidyr::separate(ngram, into = paste("w" , 1:n, sep = "_"),
                    sep = " ", remove = FALSE)

  # Remove ngrams below a given frequency
  ngram_counts <- data_ngrams |>
    dplyr::count(ngram, sort = TRUE) |>
    dplyr::filter(n > min_freq)

  data_ngrams <- data_ngrams |>
    dplyr::filter(ngram %in% ngram_counts$ngram)

  # Remove stop words from each column starting with "w_"
  w_cols <- names(data_ngrams)[grepl("w_", names(data_ngrams))]

  # TODO short words in a separate function
  short_words <- data |>
    dplyr::select(text_raw) |>
    tidytext::unnest_tokens(output = word, input = text_raw) |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>  # remove stop words
    dplyr::mutate(word = textstem::lemmatize_words(word)) |>  # lemmatise words
    dplyr::filter(nchar(word) <= rm_n_short_words)

  for (i in w_cols) {
    data_ngrams <- data_ngrams |>
      dplyr::anti_join(tidytext::stop_words, by = rlang::set_names("word", i)) |>
      dplyr::anti_join(short_words, by = rlang::set_names("word", i)) |>
      dplyr::anti_join(mintEMU::urbanism_stopwords(add_stopwords = c("emu", "tu", "delft", "ku",
                                                     "leuven", "upc", "barcelona",
                                                     "iuav", "venice"),
                                   convert_to_regex = FALSE) %>%
                  data.frame(word = .),
                by = rlang::set_names("word", i))
  }

  # Add columns with stemmed versions of individual terms if `stem = TRUE`
  if (stem) {
    data_ngrams <- data_ngrams |>
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("w_"),
        ~ SnowballC::wordStem(words = .x, language = "porter"),
        .names = "{.col}_stem"
      ))
  }

  # Add columns with lemmatised versions of individual terms if `lemma = TRUE`
  if (lemma) {
    data_ngrams <- data_ngrams |>
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("w_"),
        ~ textstem::lemmatize_words(.x),
        .names = "{.col}_lemma"
      ))
  }

  data_ngrams
}

#' Concatenate ngrams in a given character vector
#'
#' This function identifies ngrams in a text and replaces them with their
#' concatenated and lemmatised version.
#'
#' @param data Data frame including text column for which ngrams should be concatenated
#' @param n_max Maximum size of ngrams to concatenate
#' @param text_col Name of the text column to search for ngrams
#' @param ... Arguments to be passed to the nested `get_ngrams()` function
#'
#' @return Data frame with text column containing concatenated ngrams
#' @export
#'
c_ngrams <- function(data, n_max, text_col = NULL, ...) {

  # Concatenate ngrams in decreasing order
  for (i in n_max:2) {

    # Get ngrams from the text
    ngrams <- data |>
      mintEMU::get_ngrams(n = i, text_col, ...) |>
      tidyr::unite(ngram_lemma, tidyselect::ends_with("_lemma"),
                   sep = " ", remove = FALSE)

    # Concatenate lemmatised ngrams using a named vector
    c_ngrams <- stringr::str_replace_all(ngrams$ngram_lemma,
                                         pattern = " ", replacement = "_")
    names(c_ngrams) <- ngrams$ngram

    # Replace ngrams in the text with their concatenated version
    data[[text_col]] <- data[[text_col]] |>
      stringr::str_replace_all(stringr::fixed(c_ngrams))
  }

  data[[text_col]]
}
