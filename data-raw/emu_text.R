# Download and import the dataset -----------------------------------------
emu_text_url <- here::here("analysis", "data", "derived_data", "emu_theses_with_text.csv")
emu_text_raw_df <- readr::read_csv(emu_text_url)

# Clean and geocode the dataset -------------------------------------------

emu_text_df <- mintEMU::geocode_thesis_locations(emu_text_raw_df)

emu_text_df$text_clean <- emu_text_df$text |>
  mintEMU::clean_basic() |>
  stringr::str_remove_all(mintEMU::find_meta_stopwords(emu_theses)) |>
  stringr::str_remove_all(mintEMU::urbanism_stopwords(
    add_stopwords = c("emu", "european postgraduate master in urbanism"))) |>
  stringr::str_remove_all(mintEMU::thesis_stopwords(
    add_stopwords = c("advisor", "prof")))

usethis::use_data(emu_text_df, overwrite = TRUE)
