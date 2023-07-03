# Load packages ---------------------
library(tidyverse)
library(here)
library(mintEMU)

# TODO Extract text from PDFs (move this step from paper.qmd)

# Load data -----------

data("emu_theses")


# Add ID ( order based on title) ------------

# TODO  remove special Latin characters before anonymisation
emu_theses <- emu_theses |>
  arrange(title) |>
  mutate(ID  = seq_len(n()),
# remove email addresses
         text_raw = str_replace_all(text,
                                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b" ,
                                    "EMAIL_REMOVED"))

# Check if email addresses removed:
replacement_checker(emu_theses$text_raw, "@")

# Anonymise raw text -----------

# TODO add combinations of lower and uppercase in word combos
text_raw <- mapply(str_replace_all, emu_theses$text_raw,
                  word_vec_combos(normalise_words(emu_theses$first_name),
                                  normalise_words(emu_theses$last_name)),
                  "AUTHOR_REMOVED")

# Check the Anonymisation
head_text(text_raw)

# Checking if still some first names prevailed
pattern <- word_vec_combos(normalise_words(emu_theses$first_name),
                           normalise_words(emu_theses$last_name))
pattern[32]
replacement_checker(text_raw, emu_theses$first_name, char_before = 0)


# TODO replace firstname with placeholder

# TODO replace lastname with placeholder




# Attach the text raw variable to the main dataset

emu_theses$text_raw <- text_raw



# Make selection of columns ----------------
# Clean text -------------
# Add longitude and latitude columns ---------
# reorder the columns in the dataset ---------
