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
    stringr::str_replace_all("\\s{1,}", " ") |>
    stringr::str_remove_all("[[:punct:]]") |>
    stringr::str_squish()

  out_text

}


#' Show the beginning of each of the element of a string vector
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
#' @param stop_cols  List of names of columns that should be used for extraction of the custom stop words, the vectors need to be named as in the example
#' @param convert_to_regex A logical value determining if the final outcome should be a regex input. Default is TRUE
#' @return A list with strings to be removed for each of the texts
#' @export
find_meta_stopwords <- function(metadata,
                                stop_cols =  list(
                                  'author1' = c('first_name', 'last_name'),
                                  'author2' = c('first_name_2', 'last_name_2'),
                                  'title'  = 'title',
                                  'subtitle' = 'subtitle',
                                  'full_title' = 'full_title'
                                ),
                                convert_to_regex = TRUE) {
  cols_meta <- names(metadata)

  cols_meta <- cols_meta[cols_meta %in% unlist(stop_cols)]

  if (length(cols_meta) == 0)
    stop("The dataset has no stopword columns")

  used_stop_cols <-
    lapply(stop_cols, function(x)
      x[x %in% cols_meta])

  used_stop_cols <- used_stop_cols[lengths(used_stop_cols) > 0]


  stop_words_list <-
    lapply(used_stop_cols, function(x)
      metadata[x])

  stop_words_df <-  sapply(names(stop_words_list),
                           function(x) {
                             df <- stop_words_list[[x]]
                             colNames <- names(df)
                             do.call(paste, df[colNames])
                           }) |>
    as.data.frame() |>
    lapply(clean_basic)

  if (convert_to_regex == TRUE)
    return(stringr::regex(do.call(paste, c(
      stop_words_df, sep = "|"
    ))))
  else
    return(stop_words_df)

}


#' Adjustable list of stopwords associated with urbanism
#'
#' Function creates a vector of stopwords associated with field of urbanism
#' @param add_stopwords A vector of words to be added to the urbanism stopwords
#' @return A vector of urbanism-related stopwords
#' @export
urbanism_stopwords <- function(add_stopwords = NULL, convert_to_regex = TRUE) {
  stop_words <- c(add_stopwords, "city", "cities", "urban", "urbanism", "hab", "km")

  if (convert_to_regex) return(paste(stop_words, collapse = "|"))
  else return(stop_words)
}


#' Adjustable list of stopwords associated with master theses
#'
#' Function creates a vector of stopwords associated with master theses
#' @param add_stopwords A vector of words to be added to the master theses-related stopwords
#' @return A vector of thesis-related stopwords
#' @export
thesis_stopwords <- function(add_stopwords = NULL, convert_to_regex = TRUE) {
  stop_words <- c(
        add_stopwords,
        "preface",
        "foreword",
        "introduction",
        "conclusion",
        "thesis",
        "source",
        "author",
        "colophon",
        "acknowledgements",
        "references"
        )

  if (convert_to_regex) return(paste(stop_words, collapse = "|"))
  else return(stop_words)
}


#' Produces the table with the most popular short words in the body of text
#'
#' Function creates a vector of stopwords associated with master theses
#' @param text_vector A string vector
#' @param l_word length of words to be detected
#' @param l_list number of top words to be retrieved
#' @param scope either 'word' or 'corpus'. should be search be performed on individual words or a corpus?
#' @param print_kable logical value. If TRUE (default), a kable with results is printed out
#' @return A table with the short words and its prevalance in the body of text
#' @export
short_words <-
  function(text_vector,
           l_word = 4,
           l_list = 50,
           scope = 'word',
           print_kable = TRUE) {
    if (scope == 'word') {

      out_table <-
        tibble::tibble(word = text_vector[nchar(text_vector) <= l_word])

    } else if (scope == 'corpus') {
      out_table <-
        tibble::tibble(word = unlist(
          stringr::str_extract_all(text_vector, '\\b[\\w]{1,4}\\b')
        ))
    } else
      stop("The scope needs to be either 'word' or 'corpus'")

    out_table <- out_table |>
      dplyr::count(word) |>
      dplyr::arrange(dplyr::desc(n)) |>
      head(l_list)

    if (print_kable == TRUE)
      print(kableExtra::kable_styling(knitr::kable(out_table)))

    out_table

  }


