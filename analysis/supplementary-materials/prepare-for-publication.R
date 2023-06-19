# Load packages ---------------------
library(tidyverse)
library(here)
library(mintEMU)

# Load data -----------
data("emu_theses")


# Add ID ( order based on title) ------------
emu_theses <- emu_theses %>%
  arrange(title) %>%
  mutate(ID  = seq_len(n()),
         text_raw = str_replace_all(text,
                                find_meta_stopwords(.,
                                                    stop_cols =list(
                                                      author1 = c("first_name", "last_name"),
                                                      author2 = c("first_name_2", "last_name_2")
                                                      ),
                                                    clean_meta = FALSE
                                                    ),
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
