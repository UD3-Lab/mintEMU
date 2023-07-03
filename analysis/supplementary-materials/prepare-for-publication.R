# Load packages ---------------------
library(tidyverse)
library(here)
library(mintEMU)

# Load data -----------
data("emu_theses")


# Add ID ( order based on title) ------------
emu_theses <- emu_theses |>
  arrange(title) |>
  mutate(ID  = seq_len(n()),
# remove email addresses
         text_raw = str_replace_all(text,
                                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b" ),
                                    "EMAIL_REMOVED")

# Check if email addresses removed:
str_extract_all(emu_theses$text_raw, "@")

anonymisation_checker <- function(text_vector, check_pattern ){

  occur <- str_extract_all(text_vector, check_pattern)
  not_removed <- which(lengths(occur)>0)
  loc <- str_locate_all(text_vector, check_pattern )

  if(length(check_pattern) == 1 )
    check_pattern <- rep(check_pattern, length(text_vector) )

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
        str_sub(text_vector[nr], loc[[nr]][occ] - 25, loc[[nr]][occ] +
                  25)
      ))
    }
  }

}

anonymisation_checker(emu_theses$text_raw, "@")

# Anonymise raw text -----------
text_raw <- mapply(str_replace_all, emu_theses$text_raw,
                  word_vec_combos(normalise_words(emu_theses$first_name),
                                  normalise_words(emu_theses$last_name)),
                  "AUTHOR_REMOVED")

# Check the Anonymisation
head_text(text_raw)

# Checking if still some first names prevailed
str_extract_all(emu_theses$text, emu_theses$first_name)
str_extract_all(text_raw, emu_theses$first_name)


# For non-empty elements - check the pattern created with the word_vec_combos() versus the thesis text
fname_occur <- str_extract_all(text_raw, emu_theses$first_name)
not_removed <- which(lengths(fname_occur)>0)

fname_loc <- str_locate_all(text_raw, emu_theses$first_name )
pattern <- word_vec_combos(normalise_words(emu_theses$first_name),
                normalise_words(emu_theses$last_name))

# Check based on the example:



for (nr in not_removed) {
  n_occs <- nrow(fname_loc[[nr]])

  print(paste("PATTERN:", pattern[[nr]]))

  for (occ in 1:n_occs) {
    print(paste(
      "NON_REMOVED NAME,  NR",
      nr,
      ",",
      occ ,
      ":",
      str_sub(text_raw[nr], fname_loc[[nr]][occ], fname_loc[[nr]][occ] +
                25)
    ))
  }
}




# str_sub(emu_theses$text_raw[2],fname_matches[[1]][1,1], fname_matches[[1]][1,2])
#
# str_locate_all(emu_theses$text_raw[2], emu_theses$last_name[2] )


# Attach the text raw variable to the main dataset

emu_theses$text_raw <- text_raw



# Make selection of columns ----------------
# Clean text -------------
# Add longitude and latitude columns ---------
# reorder the columns in the dataset ---------
