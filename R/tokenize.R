#' Tokenise a given corpus
#'
#' @param data A data frame containing a text column
#' @param id_col Unique document id
#' @param text_col Name of the column containing the text to be tokenized
#' @param min_length Minimum length of words to be kept
#' @param lemma Lemmatise words
#'
#' @return Data frame with `ID` and `text_clean` columns
#' @export
tokenize <- function(data,
                     id_col = "ID",
                     text_col = "text_clean",
                     min_length = 3,
                     lemma = TRUE) {

  id <- rlang::sym(id_col)
  text <- rlang::sym(text_col)

  words_df <- data |>
    dplyr::select(!!id, !!text) |>
    tidytext::unnest_tokens(output = word, input = !!text) |>  # remove punctuation, convert to lower-case, separate all words
    dplyr::anti_join(tidytext::stop_words, by = "word") |>  # remove stop words
    dplyr::mutate(word = textstem::lemmatize_words(word)) |>  # lemmatise words
    dplyr::filter(nchar(word) >= min_length)  # keep only words that are at least three letters long

  words_df
}
