#' Clean body of text
#'
#' Function removes white space, punctuation and  converts the strings to lowercase
#'
#' @param text A vector string to be converted
#' @return A cleaned vector string
#' @export
clean_basic <-function(text){
  out_text <- text  %>%
    tolower() %>%
    str_replace_all("\\s"," ") %>%
    str_remove_all("[^0-9a-z ]") %>%
    str_squish()
  out_text

}


#' Find custom stop-phrases based on metadata file
#'
#' Function scans the dataset with metadata for fields that can be removed from the body fo text before the text analysis
#' @param metadata A data frame with metadata information about the text
#' @param stop_cols  Names of columns that should be used for extraction of the custom stop words
#' @return A list of strings to be removed for each of the texts
#' @export
find_meta_stop <- function(metadata, stop_cols =  c('author_firstname', 'author_surname','title') ){

  cols_meta <- names(metadata)

  cols_meta <- cols_meta[cols_meta %in% stop_cols]

  if (length(cols_meta) == 0 ) stop("The dataset has not stop word columns")



}

str_replace_all(emu_theses$author_firstname, "") %>%   ### remove author first name
  str_replace_all(emu_theses$author_firstname, "") %>%     ### remove author surname
  str_replace_all(emu_theses$title, "") %>%



# 3. custom stopwords


# custom stop words
stop_words_custom <- emu_theses %>%
  unnest_tokens(output = word, input = title) %>%   ### add the name of the author and words from the title
  select("word") %>%
  rbind(., data.frame(word = c("preface", "foreword", "introduction", "conclusion", "thesis"))) %>%
  rbind(., data.frame(word = c("city", "urban", "urbanism"))) %>%
  rbind(., data.frame(word = c("hab", "km")))


# remove white space and punctuation


# clean text
emu_theses$text_clean <- emu_theses$text %>%
  str_replace_all("\\s", " ") %>%  ### remove all carriage return and newline characters
  str_squish() %>%
  str_replace_all("- ", "") %>%       ### remove all hyphens
  str_replace_all("[0-9]", "") %>%    ### remove all numbers
  str_replace_all(emu_theses$author_firstname, "") %>%   ### remove author first name
  str_replace_all(emu_theses$author_surname, "") %>%     ### remove author surname
  str_replace_all(emu_theses$title, "") %>%              ### remove title
  # str_replace_all("Claudiu Forgaci", "") %>%
  # str_replace_all("Bucharest: Between North and South", "") %>%
  str_replace_all("EMU", "")          ### remove common words
