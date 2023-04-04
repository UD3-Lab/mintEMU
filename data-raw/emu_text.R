# Get dataset URL ---------------------------------------------------------
url_emu_text <- ""

# Download the dataset ----------------------------------------------------
emu_text <- readr::read_csv(url_emu_text)

readr::write_csv(emu_text, here::here("inst", "extdata", "emu_text.csv"))

usethis::use_data(emu_text, overwrite = TRUE)
