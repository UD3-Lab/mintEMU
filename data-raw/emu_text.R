# Download and import the dataset ----
emu_url <- here::here("analysis", "data", "derived_data", "emu_raw.csv")
emu <- readr::read_csv(emu_url)

# Clean the dataset ----
emu$text_clean <- emu$text_raw |>
  mintEMU::clean_basic() |>
  stringr::str_remove_all(mintEMU::find_meta_stopwords(emu_theses)) |>
  stringr::str_remove_all(mintEMU::urbanism_stopwords(
    add_stopwords = c("emu", "european postgraduate master in urbanism"))) |>
  stringr::str_remove_all(mintEMU::thesis_stopwords(
    add_stopwords = c("advisor", "prof")))

usethis::use_data(emu, overwrite = TRUE)
