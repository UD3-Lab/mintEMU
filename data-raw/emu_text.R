# Download and import the dataset ----
emu_raw_path <- here::here("analysis", "data", "derived_data", "emu_raw.csv")
emu_raw <- readr::read_csv(emu_raw_path)

# add raw dataset to the package
usethis::use_data(emu_raw, overwrite = TRUE)

# emu metadata file
emu_meta_path <- here::here("analysis", "data", "derived_data", "emu_metadata.csv")

emu_metadata <- readr::read_csv(emu_meta_path)

# add metadata dataset to the package
usethis::use_data(emu_metadata, overwrite = TRUE)

# Clean the dataset ----
# TODO adapt cleaning procedure once all the steps are known (#105)

emu_raw$text_clean <- emu_raw$text_raw |>
  mintEMU::clean_basic() |>
  stringr::str_remove_all(mintEMU::find_meta_stopwords(emu_theses)) |>
  stringr::str_remove_all(mintEMU::urbanism_stopwords(
    add_stopwords = c("emu", "european postgraduate master in urbanism"))) |>
  stringr::str_remove_all(mintEMU::thesis_stopwords(
    add_stopwords = c("advisor", "prof")))

emu_clean <- emu_raw |>
  dplyr::select(ID, text_clean)


usethis::use_data(emu_clean, overwrite = TRUE)
