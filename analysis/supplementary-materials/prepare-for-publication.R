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
         text_raw = str_replace_all(text,
                                word_vec_combos(normalise_words(emu_theses$first_name),
                                                normalise_words(emu_theses$last_name)),
                                "AUTHOR_REMOVED"
                                )
         )

head_text(emu_theses$text)

# Anonymyse raw text -----------

emu_theses



# Make selection of columns ----------------
# Clean text -------------
# Add longitude and latitude columns ---------
# reorder the columns in the dataset ---------
