#' Clean body of text
#'
#' Function removes white space, punctuation and  converts the strings to lowercase
#'
#' @param text A vector string to be converted
#' @return A cleaned vector string
#' @export
#'
clean_basic <- function(text) {
  out_text <- text |>
    tolower() |>
    str_replace_all("\\s{1,}", " ") |>
    str_remove_all("[^0-9a-z ]") |>
    str_squish()

  out_text

}


#' Show the beginning of each of the element of a string vector
#'
#'
#' @param text_vector A string vector
#' @param head_n  Number of elements of the vector. Default is 10
#' @param sub_start starting position within the string. Default is 1.
#' @param sub_end  final position within the string. Default is 500.
#' @return A initial characters of  first elements of the string vector
#' @export
#'
head_text <- function(text_vector, head_n = 10, sub_start = 1 , sub_end = 500 ){
  head_text <- text_vector |>
    head(head_n) |>
    stringr::str_sub(start = sub_start, end = sub_end)

  head_text
}

#' Find custom stop-phrases based on metadata file
#'
#' Function scans the dataset with metadata for fields that can be removed from the body fo text before the text analysis
#' @param metadata A data frame with metadata information about the text
#' @param stop_cols  Names of columns that should be used for extraction of the custom stop words, the vectors need to be named as in the example
#' @return A list of strings to be removed for each of the texts
#' @export
find_meta_stopwords <- function(metadata,
                                stop_cols =  list('author1'= c('author_firstname', 'author_surname'),
                                                  'author2'= c('author_firstname2', 'author_surname2'),
                                                  'title'  = 'title') ) {
  cols_meta <- names(metadata)

  cols_meta <- cols_meta[cols_meta %in% unlist(stop_cols)]

  if (length(cols_meta) == 0)
    stop("The dataset has no stopword columns")

  used_stop_cols <- lapply(stop_cols, function(x) x[x %in% cols_meta])
  used_stop_cols <- used_stop_cols[lengths(used_stop_cols) >0 ]


  stop_words_list <- lapply(used_stop_cols, function(x) metadata[ x]  )

  stop_words_list <-  lapply(names(stop_words_list),
                             function(x) {
                               df <- stop_words_list[[x]]
                               colNames <- names(df)
                               do.call(paste, df[, colNames])
                             })

  stop_words_list

}

#' Adjustable list of stopwords associated with urbanism
#'
#' Function creates a vector of stopwords associated with field of urbanism
#' @param add_stopwords A vector of words to be added to the urbanism stopwords
#' @return A vector of urbanism-related stopwords
#' @export
urbanism_stopwords <- function(add_stopwords = NULL) {
  stop_words <-
    c(add_stopwords, "city", "urban", "urbanism", "hab", "km")

}

#' Adjustable list of stopwords associated with master theses
#'
#' Function creates a vector of stopwords associated with master theses
#' @param add_stopwords A vector of words to be added to the master theses-related stopwords
#' @return A vector of thesis-related stopwords
#' @export
thesis_stopwords <- function(add_stopwords = NULL) {
  stop_words <-
    c(add_stopwords, "preface", "foreword", "introduction", "conclusion", "thesis")

}


# emu_theses$text_clean <- emu_theses$text %>%
#   str_replace_all("\\s", " ") %>%  ### remove all carriage return and newline characters
#   str_squish() %>%
#   str_replace_all("- ", "") %>%       ### remove all hyphens
#   str_replace_all("[0-9]", "") %>%    ### remove all numbers
#   str_replace_all(emu_theses$author_firstname, "") %>%   ### remove author first name
#   str_replace_all(emu_theses$author_surname, "") %>%     ### remove author surname
#   str_replace_all(emu_theses$title, "") %>%              ### remove title
#   # str_replace_all("Claudiu Forgaci", "") %>%
#   # str_replace_all("Bucharest: Between North and South", "") %>%
#   str_replace_all("EMU", "")          ### remove common words
