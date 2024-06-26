# Load packages ----
library(here)
library(readxl)
library(tidyverse)

# Read thesis metadata ----
theses_all <-
  readxl::read_xlsx(path = here("analysis", "data", "raw_data", "theses-all.xlsx")) %>%
  select(-email,
         -contains("pdf_requested"),
         -pdf_exists,
         -`Current Employmment`) %>%
  mutate(file_name = ifelse(is.na(last_name_2),
                            paste0(grad_year, "-", last_name, ".pdf"),
                            paste0(grad_year, "-", last_name, "-", last_name_2, ".pdf")),
         last_name_split = str_split(last_name, "-|\\s")) %>%
  unnest(last_name_split) %>%
  mutate(last_name_split_year = str_to_lower(paste0(last_name_split, "-", grad_year)))

# # Merge separate thesis metadata files and write repo metadata to new file ----
# read_csv(file = here("analysis", "data", "raw_data", "tudelft_repository_1615930007.csv"),
#          locale = locale(encoding = "UTF-8")) %>%
#   rbind(., read_csv(file = here("analysis", "data", "raw_data", "tudelft_repository_1673304910.csv"),
#                     locale = locale(encoding = "UTF-8"))) %>%
#   rbind(., read_csv(file = here("analysis", "data", "raw_data", "tudelft_repository_1673305043.csv"),
#                     locale = locale(encoding = "UTF-8")) %>%
#           mutate(author = "Pisano")) %>%
#   rbind(., read_csv(file = here("analysis", "data", "raw_data", "tudelft_repository_1673433389.csv"),
#                     locale = locale(encoding = "UTF-8"))) %>%
#   write_csv(file = here("analysis", "data", "raw_data", "theses-repository.csv"))

# Read TU Delft repository metadata ----
theses_repo <-
  read_csv(file = here("analysis", "data", "raw_data", "theses-repository.csv"),
           locale = locale(encoding = "UTF-8")) %>%
  mutate(last_name_split = str_extract(string = author, pattern = "\\w+"),
         last_name_split_year = str_to_lower(paste0(last_name_split, "-", `publication year`))) %>%
  filter(
    !`publication type` %in% c("doctoral thesis", "journal article", "book", "conference paper")
  )

theses_repo_one_year_less <- theses_repo %>%
  mutate(`publication year` = `publication year` - 1)

theses_repo <- theses_repo %>%
  bind_rows(theses_repo_one_year_less) %>%
  mutate(last_name_split_year = str_to_lower(paste0(last_name_split, "-", `publication year`)))

# Add metadata from repository ----
theses_meta <- theses_all %>%
  left_join(theses_repo, by = c("last_name_split_year" = "last_name_split_year"))

# Write metadata ----
theses_meta %>%
  mutate(`repository link` = case_when(
    last_name == "Park" ~ "http://resolver.tudelft.nl/uuid:55020771-b07d-4953-b578-37b0be018521",
    .default = `repository link`)) %>%
  mutate(thesis_title = ifelse((!is.na(title) & (str_length(title) > str_length(thesis_title))), title, thesis_title)) %>%  # Update titles from repository
  mutate(count_na = apply(., 1, function(x) sum(is.na(x)))) %>%
  group_by(file_name) %>%
  arrange(count_na, .by_group = TRUE) %>%
  ungroup() %>%
  distinct(file_name, .keep_all = TRUE) %>%  # it only keeps the first occurrence; does not work when the join should be made for second or later last name
  arrange(last_name) %>%
  select(-title, -uuid, -count_na,
         -contains("last_name_split"), -author,
         -contains("permission_re"), -contains("granted_2"),
         -`publication year`,
         -`publication type`,
         -publisher,
         -isbn,
         -issn,
         -patent,
         -`patent status`,
         -`bibliographic note`,
         -project,
         -coordinates,
         -programme,
         -`research group`,
         -department,
         -language,
         -`embargo date`,
         -`access restriction`,
         -faculty,
         "title" = thesis_title,
         "graduation_year" = grad_year,
         "graduation_semester" = grad_sem,
         "link" = `repository link`,
         start_page, end_page) %>%
  rename(full_title = title) %>%
  separate(full_title, c("title", "subtitle"), sep = ":", remove = FALSE) %>%
  write_csv(file = here("analysis", "data", "raw_data", "theses-metadata.csv"))

## Total theses per year
readxl::read_xlsx(path = here("analysis", "data", "raw_data", "theses-all.xlsx")) |>
  # count(grad_year, pdf_exists, permission_granted) |>
  group_by(grad_year) |>
  summarise(n_total = n(),
            n_pdfs = sum(as.logical(pdf_exists), na.rm = TRUE),
            n_permissions = sum(as.logical(permission_granted), na.rm = TRUE)) |>
  filter(!is.na(grad_year)) |>
  write_csv(file = here("analysis", "data", "derived_data", "emu-theses-per-year.csv"))
