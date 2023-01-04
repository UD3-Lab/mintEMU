# Load packages -----------------------------------------------------------
library(here)
library(tidyverse)


# Load raw metadata -------------------------------------------------------

# read csv file with list of theses found in the repository
tud_repo_file <-list.files(path = here("analysis", "data", "raw_data"), pattern="^tudelft_repository")
tud_repo_df <- read_csv(here("analysis", "data", "raw_data", tud_repo_file))


# Clean metadata ----------------------------------------------------------

tud_repo_clean <- tud_repo_df %>%
  filter(!is.na(author),
         `publication type` == "master thesis",
         !programme %in% c("Architecture, Urbanism and Building Sciences | Explorelab",
                           "Explore lab",
                           "Housing Lab",
                           "Delta Interventions",
                           "Complex Cities",
                           "Materialisation",
                           "Architecture, Urbanism and Building Sciences | Design of the Urban Fabric",
                           "Architecture, Urbanism and Building Sciences | Building Technology",
                           "Urbanism and Building Sciences",
                           "Metropolitan Spatial Structures",
                           "Urban Regeneration",
                           "Explorelab",
                           "MSc track Urbanism: Delta interventions graduation studio.",
                           "MSc Spatial Planning and Urban Design - [TISD] Technology in Sustainable Development Specialization"),
         language == "en",
         !str_detect(author, "Civil Engineering")) %>%
  # remove remaining non-EMU entries
  filter(!str_detect(author, "Kalugendo|Brouwer")) %>%
  select(uuid, author, title, abstract, contributor,
         year = `publication year`,
         topic = `subject topic`) %>%
  extract(col = author,
          into = "author_surname",
          regex = "^(.+?),",
          remove = TRUE) %>%
  group_by(year) %>%
  summarise(uuid, year, author_surname, title, contributor, abstract, topic) %>%
  arrange(desc(year))


# Save cleaned dataset ----------------------------------------------------

# write clean and filtered data to a csv file
tud_repo_clean %>%
  write_csv(here("analysis", "data","derived_data","emu-theses-repo-metadata.csv"))
