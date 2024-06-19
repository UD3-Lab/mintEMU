#' Clean body of text
#'
#' Function removes white space, punctuation and converts the strings to lowercase
#'
#' @param text A vector string to be converted
#' @return A cleaned vector string
#' @export
#'
clean_basic <- function(text) {

  out_text <- text |>
    tolower() |>
    stringr::str_replace_all("\\s{1,}", " ") |>
    stringr::str_remove_all("[^[:alnum:][:space:]'-]") |>
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


# TODO Should we remove the author names from this function as anonymisation has already been performed on the raw data?
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
                                convert_to_regex = TRUE,
                                clean_meta = TRUE ) {
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
                             df[is.na(df)]<-""
                             do.call(paste, df[colNames])
                           }) |>
    as.data.frame()

  if (clean_meta == TRUE)
    stop_words_df <- lapply(stop_words_df, clean_basic)

  if (convert_to_regex == TRUE)
    return(stringr::regex(do.call(paste, c(
      stop_words_df, sep = "|"
    )), ignore_case = TRUE))
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
  stop_words <- paste("\\b",
                      c(add_stopwords |> tolower() |> trimws(), "city", "cities", "urban", "urbanism", "hab", "km"),
                      "\\b", sep = "")

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
  stop_words <- paste("\\b", c(add_stopwords,
                               "preface", "foreword", "acknowledgements",
                               "colophon", "introduction", "conclusion",
                               "thesis",
                               "source",
                               "author", "advisor",
                               "references", "bibliography"),
                      "\\b", sep = "")

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
           print_kable = FALSE) {
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


#' Create a combination of words
#'
#' Function creates a vector that includes combination of words
#' coming from two vectors of words,
#' making sure at least one word from each input vectors is included in the combination
#'
#' @param words1 A vector of single words to be included in the combination
#' @param words2 A vector of single words to be included in the combination
#'
#' @return A vector string of combinations of words coming from the input vectors
#'
word_combos <- function(words1, words2) {

  words1[is.na(words1)] <- ""

  words2[is.na(words2)] <- ""


  # Combine words from both strings
  all_words <- c(words1, words2)

  # Find combinations with the condition

  combinations <- list()
  for (length in 2:length(all_words)) {
    combinations[[length]] <- combn(all_words, length, paste, collapse = " ")

    valid_combinations <- lapply(strsplit(combinations[[length]], "\\s+"),
                                 function(x) sum(x %in% words1 | words1 == "") > 0  & sum(x %in% words2 | words2 == "") > 0 ) |>
      unlist()

    combinations[[length]] <-  combinations[[length]][valid_combinations]

  }

  combinations <- unlist(combinations)

  combinations


}


#' Create a combination of words
#'
#' Function creates a vector that includes combination of words
#' coming from two vector of strings,
#' making sure at least one word from each input strings is included in the combination
#'
#' @param string1 A vector string to be included in the combination
#' @param string2 A vector string to be included in the combination
#' @param empty_replacement a replacement of empty patterns. Default is "XXXXXXXXXXXXXXXXXXXXX"
#' @param convert_to_regex A logical value determining if the final outcome should be a regex input. Default is TRUE
#'
#' @return A vector string consisting of combinations of words coming from input vectors
#' @export
#'
word_vec_combos <- function(string1, string2, empty_replacement = "XXXXXXXXXXXXXXXXXXXXX",  convert_to_regex = TRUE) {

  # Splitting strings into words
  words1 <- strsplit(string1, "\\s+")
  words2 <- strsplit(string2, "\\s+")

  # Combine words from both strings
  all_words <- mapply(c, words1, words2, SIMPLIFY = F)

  # Find combinations with the condition

  combinations <- mapply(word_combos, words1, words2, SIMPLIFY = F )

  combinations <- lapply(combinations, function(x) dplyr::if_else(x == " ", empty_replacement,x))

  if (convert_to_regex)
    return(
      lapply(combinations, paste, collapse = "|") |>
        unlist()
      )
  else
    return(combinations)

}


#' Check for special Latin letters
#'
#' Function checks if the vector of words includes special Latin letters;
#' if so, add a word with a regular Latin counterpart to the vector
#'
#' @param words A vector string to be converted. each element will be treated as a separate word
#'
#' @return A vector string with regular letter counterparts added at the end of the string
#' @export
#'
normalise_words <- function(string_vec) {

  words <- strsplit(string_vec, "\\s+")

  # Remove Latin special characters
  normalised_words <- lapply(words, stringi::stri_trans_general, "Latin-ASCII")

  # Change all the words to lower case
  lowerised_words <- lapply(normalised_words, tolower)

  # Change all the first letters to upper case
  sentesized_words <- lapply(normalised_words, stringr::str_to_title)

  # Final list of words
  final_words <- mapply(c, words, normalised_words, lowerised_words, sentesized_words, SIMPLIFY=FALSE)

  # Remove punctuation
  final_words <- lapply(final_words,
                        stringr::str_remove_all, '[[:punct:]]+')

  # add word boundary markers
  final_words <- lapply(final_words, \(x) dplyr::if_else(is.na(unique(x)), NA_character_,
                                                         unique(paste0("\\b", x, "\\b"))))

  final_words <- lapply(final_words,
                        \(x) dplyr::if_else(is.na(x), NA_character_,
                                            unique(x))) |> unlist()
}


#' Check the result of string replacement
#'
#' Function checks if a specific check pattern is included in the string vector;
#' if so, it would print the pattern and the instances of it's occurance in the string
#'
#' @param text_vector A vector string to be checked
#' @param check_pattern A pattern used to check whether the replacement worked. Can be single value or a vector of the same length as `text_vector`
#' @param char_before Number of characters to be shown before the start of `check_pattern`. Default is 25.
#' @param char_after  Number of characters to be shown after the start of the `check_pattern`. Default is 25.
#'
#' @export
#'
replacement_checker <- function(text_vector, check_pattern, char_before = 25, char_after = 25 ){

  occur <- str_extract_all(text_vector, check_pattern)
  not_removed <- which(lengths(occur) > 0)
  loc <- str_locate_all(text_vector, check_pattern )

  if (!(length(check_pattern) == 1 | length(check_pattern) == length(text_vector) ))
    stop("The `check_pattern` needs to be a string vector of length 1 or of the same length as `text_vector`")


  if(length(check_pattern) == 1 )
    check_pattern <- rep(check_pattern, length(text_vector))

  for (nr in not_removed) {
    n_occs <- nrow(loc[[nr]])

    print(paste("PATTERN:", check_pattern[[nr]]))

    for (occ in 1:n_occs) {
      print(paste(
        "NON_REMOVED,  NR",
        nr,
        ",",
        occ ,
        ":",
        str_sub(text_vector[nr], loc[[nr]][occ] - char_before, loc[[nr]][occ] +
                  char_after)
      ))
    }
  }

}
