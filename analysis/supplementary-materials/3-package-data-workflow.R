# 1. Create data-raw folder to compute package datasets ----------------------
usethis::use_data_raw()

# 2. Create data-raw/emu_text.R to generate the package data -----------------

# 3. Generate the package data ---------
source(here::here("data-raw", "emu_text.R"))
