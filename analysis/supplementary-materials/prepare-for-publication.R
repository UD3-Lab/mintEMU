# Load packages ---------------------
library(tidyverse)
library(here)
library(mintEMU)
library(readxl)

# Extract text from PDFs ----
# Read thesis metadata
data_path <- here("analysis", "data", "raw_data")
pdf_names <- dir(data_path, pattern = "*.pdf")

## Read metadata for all theses
all_theses <-
  read_xlsx(path = here(data_path, "theses-all.xlsx"))

## Read only metadata for theses with PDF available
emu_theses <-
  read_csv(here(data_path, "theses-metadata.csv")) |>
  mutate(text = "") |>
  filter(!is.na(pdf_via))

pdf_paths <- here(data_path, emu_theses$file_name)

emu_theses$text <- convert_pdf_text(pdf_paths)

# Add ID (order based on title) & perform anonymysation ------------

# Remove special Latin characters before anonymisation
# Remove email addresses
emu_theses <- emu_theses |>
  arrange(title) |>
  mutate(ID  = seq_len(n()),
         text_raw = stringi::stri_trans_general(text,  "Latin-ASCII")) |>
  mutate(text_raw = str_replace_all(text_raw,
                                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b" ,
                                    "EMAIL_REMOVED"))

# # head_text(emu_theses$text_raw )[9]
#
# # Check if email addresses removed:
# replacement_checker(emu_theses$text_raw, "@")

# Remove First name/ last name combination ( for first and second author)

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
emu_theses$text_raw  <- text_raw

# Make selection of columns ----------------
emu_theses <- emu_theses |>
  select(ID, graduation_year,
         graduation_semester, exchange_semester,
         title, full_title, subtitle,
         location,
         abstract,
         text_raw)

# Add longitude and latitude columns ---------
emu_theses <- geocode_thesis_locations(emu_theses)

# reorder the columns in the dataset ---------
emu_theses <- emu_theses |>
  select(ID,
         graduation_year,
         graduation_semester, exchange_semester,
         full_title, title, subtitle,
         location, latitude, longitude,
         abstract,
         text_raw)

# separate text file and metadata file ----------------
emu_metadata <- emu_theses |>
  select(-text_raw)

emu_raw <- emu_theses |>
  select(ID, text_raw)

# write CSV files ----------------

# emu raw text file
emu_raw_path <- here("analysis", "data", "derived_data", "emu_raw.csv")

readr::write_csv(emu_raw, emu_raw_path)

# emu metadata file
emu_meta_path <- here("analysis", "data", "derived_data", "emu_metadata.csv")

readr::write_csv(emu_metadata, emu_meta_path)

