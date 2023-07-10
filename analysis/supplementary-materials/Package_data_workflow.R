# Create data-raw folder to compute package datasets ----------------------
usethis::use_data_raw()

# Create data folder with data to be made available as a part of the package
usethis::use_data(emu, overwrite = TRUE)
