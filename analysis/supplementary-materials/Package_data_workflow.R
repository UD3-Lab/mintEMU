# Create data-raw folder to compute package datasets ----------------------
usethis::use_data_raw()

# Create data folder with data to be made available asa part of the pacakge
usethis::use_data(emu_theses, overwrite = TRUE)
