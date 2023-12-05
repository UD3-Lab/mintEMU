# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(mintEMU)
library(readxl)


# Read thesis metadata ----------------------------------------------------
data_path <- here("analysis", "data", "raw_data")

## Read only metadata for theses with PDF available
emu_theses <-
  read_csv(here(data_path, "theses-metadata.csv")) |>
  mutate(text = "") |>
  filter(!is.na(pdf_via))


# Extract text from PDFs --------------------------------------------------
pdf_paths <- here(data_path, emu_theses$file_name)
start_pages <- emu_theses$start_page
end_pages <- emu_theses$end_page

emu_theses$text <- convert_pdf_text(pdf_paths, start_pages, end_pages)


# Add ID (order based on title) & perform anonymisation -------------------

## Remove special Latin characters before anonymisation
## Remove email addresses
emu_theses <- emu_theses |>
  arrange(title) |>
  mutate(ID  = seq_len(n()),
         text_raw = stringi::stri_trans_general(text,  "Latin-ASCII")) |>
  mutate(text_raw = str_replace_all(text_raw,
                                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b",
                                    "EMAIL_REMOVED"))

# # head_text(emu_theses$text_raw )[9]
#
# # Check if email addresses removed:
# replacement_checker(emu_theses$text_raw, "@")

# Remove first name / last name combination (for first and second author)

pattern <- word_vec_combos(normalise_words(emu_theses$first_name),
                           normalise_words(emu_theses$last_name))

text_raw <- mapply(str_replace_all, emu_theses$text_raw, pattern,
                  "AUTHOR_REMOVED")

# # Check the Anonymisation
# head_text(text_raw,2 )
# head_text(emu_theses$text_raw ,2)
# replacement_checker(text_raw, emu_theses$first_name, char_before = 0)


# Removing second author
pattern2 <- word_vec_combos(normalise_words(emu_theses$first_name_2),
                           normalise_words(emu_theses$last_name_2))

text_raw <- mapply(str_replace_all, text_raw,
                   pattern2,
                   "AUTHOR_REMOVED")

# head_text(text_raw )
#
# replacement_checker(text_raw, emu_theses$first_name_2, char_before = 0)
#

# Replace firstname with placeholder
pattern_fn <- normalise_words(emu_theses$first_name)
pattern_fn <- strsplit(pattern_fn, "\\s+")
pattern_fn <- lapply(pattern_fn, paste, collapse = "|") |>
  unlist()


text_raw <- mapply(str_replace_all, text_raw, pattern_fn,
                   "FIRST_NAME_REMOVED")



# Replace lastname with placeholder
pattern_ln <- normalise_words(emu_theses$last_name)
pattern_ln <- strsplit(pattern_ln, "\\s+")
pattern_ln <- lapply(pattern_ln, paste, collapse = "|") |>
  unlist()

text_raw <- mapply(str_replace_all, text_raw, pattern_ln,
                   "LAST_NAME_REMOVED")



# replacement_checker(text_raw, emu_theses$last_name, char_before = 0)

# head_text(text_raw)

# incorporate into the main dataset
emu_theses$text_raw <- text_raw


# Make selection of columns -----------------------------------------------
emu_theses <- emu_theses |>
  filter(as.logical(permission_granted) == TRUE) |>
  select(ID, graduation_year,
         graduation_semester,
         full_title, title, subtitle, link,
         location,
         abstract,
         text_raw) |>
  rename(grad_year = graduation_year,
         grad_sem = graduation_semester,
         loc = location)


# Add longitude and latitude columns --------------------------------------
emu_theses <- geocode_thesis_locations(emu_theses)


# Reorder the columns in the dataset --------------------------------------
emu_theses <- emu_theses |>
  select(ID,
         grad_year,
         grad_sem,
         full_title, title, subtitle, link,
         loc, lat, long,
         abstract,
         text_raw)


# Clean raw data for publication ----------------------------------------
meta_sw <- mintEMU::find_meta_stopwords(emu_theses, clean_meta = FALSE)
anonymisation_sw <- c("EMAIL_REMOVED", "AUTHOR_REMOVED", "FIRST_NAME_REMOVED", "LAST_NAME_REMOVED") |>
  paste(collapse = "|")

emu_theses$text_raw <- emu_theses$text_raw |>
  # Remove hyphenation
  stringr::str_replace_all("-\\n", "") |>
  # Remove line breaks
  stringr::str_replace_all(" \\n", " ") |>
  # Remove page numbers
  stringr::str_replace_all("\\\n{0,}\\s{0,}\\d+\\s{0,}\\.?\\\n", "\n") |>
  # Remove text included with anonymisation
  stringr::str_remove_all(anonymisation_sw) |>
  # Remove occurrences of the full title
  stringr::str_remove_all(meta_sw)


# Separate metadata file and text file ------------------------------------
emu_metadata <- emu_theses |>
  select(-text_raw, -link)

emu_raw <- emu_theses |>
  select(ID, text_raw)


# Write CSV files ---------------------------------------------------------
## Metadata file
emu_meta_path <- here("analysis", "data", "derived_data", "emu-metadata.csv")
readr::write_csv(emu_metadata, emu_meta_path)

## Raw text file
emu_raw_path <- here("analysis", "data", "derived_data", "emu-raw.csv")
readr::write_csv(emu_raw, emu_raw_path)


