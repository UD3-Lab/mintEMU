# Download and import the dataset ----
emu_raw_path <- here::here("analysis", "data", "derived_data", "emu_raw.csv")
emu_raw <- readr::read_csv(emu_raw_path)

# add raw dataset to the package
usethis::use_data(emu_raw, overwrite = TRUE)

# emu metadata file
emu_meta_path <- here::here("analysis", "data", "derived_data", "emu_metadata.csv")
emu_metadata <- readr::read_csv(emu_meta_path)

# add metadata to the package
usethis::use_data(emu_metadata, overwrite = TRUE)

# Clean the dataset ----
emu <- dplyr::left_join(emu_raw, emu_metadata, by  = "ID")

# Prepare lists of stop words
meta_sw <- mintEMU::find_meta_stopwords(emu, clean_meta = FALSE)
emu_sw <- mintEMU::urbanism_stopwords(
  add_stopwords = c("emu", "european post[\\-]?(graduate)?[\\s]?master[s]? in urbanism",
                    "tu delft", "ku leuven", "upc barcelona", "iuav venice"))
thesis_sw <- mintEMU::thesis_stopwords(
  add_stopwords = c("advisor", "prof", "professor", "fig", "ure", "figure", "ning", "dr", "ir", "drir"))
anonymisation_sw <- c("EMAIL_REMOVED", "AUTHOR_REMOVED", "FIRST_NAME_REMOVED", "LAST_NAME_REMOVED") |>
  paste(collapse = "|")
custom_sw <- c("space[s]?", "spatial", "plan", "(plann)\\w{0,}", "public", "development",
               "design", "process[es]?", "project[s]?", "figure[s]?", "map[s]?", "strateg(y|ies)", "intervention[s]?",
               "create", "exist", "analys(i|e)s") |>
  (\(.) paste0("\\b", ., "\\b"))() |>
  paste(collapse = "|")

# # Add cleaned text column
# emu$text_clean <- emu$text_raw |>
#   # Remove hyphenation
#   stringr::str_replace_all("-\\n", "") |>
#   # Remove text included with anonymisation
#   stringr::str_remove_all(anonymisation_sw) |>
#   # Remove white spaces and punctuation, and change text to lower-case
#   mintEMU::clean_basic() |>
#   # Remove title, subtitle and full title
#   stringr::str_remove_all(meta_sw) |>
#   # Remove dataset-specific words
#   stringr::str_remove_all(emu_sw) |>
#   # Remove thesis-specific words
#   stringr::str_remove_all(thesis_sw)

# Add cleaned text column
emu$text_clean <- emu$text_raw |>
  # Remove hyphenation
  stringr::str_replace_all("-\\n", "") |>
  # Remove line breaks
  stringr::str_replace_all(" \\n", " ") |>
  # Remove page numbers
  stringr::str_remove_all("\\n{0,}\\s{0,}\\d+\\n") |>
  # Remove text included with anonymisation
  stringr::str_remove_all(anonymisation_sw) |>
  # Remove occurrences of the full title
  stringr::str_remove_all(meta_sw)

# # Check line breaks
# emu$text_clean[22] |> str_split("\\n")

# Concatenate ngrams
emu$text_clean <- emu |>
  mintEMU::c_ngrams(n_max = 3, text_col = "text_clean",
           id_col = "ID", min_freq = 20, lemma = TRUE)

# Words that were not included in ngrams are safe to remove now
emu$text_clean <- emu$text_clean |>
  stringr::str_remove_all(custom_sw)

# # Get all concatenated ngrams
# unique(unlist(str_match_all(emu$text_clean, "[A-Za-z]+(?:_[A-Za-z]+)+")[1]))

# # Check if the word "space" was removed
# unique(unlist(str_match_all(emu$text_clean, "\\bspace\\b")))
# sort(table(unlist(str_match_all(emu$text_clean, "space(?:_[A-Za-z]+)+"))), decreasing = TRUE)

emu_clean <- emu |>
  dplyr::select(ID, text_clean)

usethis::use_data(emu_clean, overwrite = TRUE)
