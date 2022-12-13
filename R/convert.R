#' Convert pdf to text
#'
#' Function returns a list of files from the directory chosen by the user and to be converted.
#' Checks if the provided file path exists and whether it holds the adequate file formats (pdf).
#'
#' @param pdf_filename_vector  Vector of paths to input (pdf) files
#' @return A vector of body of text one string per file
#' @export
convert_pdf_text <-function(pdf_filename_vector){
  pdf_list<- as.list(pdf_filename_vector)
  text_list <- convert_pdf(pdf_list)

  # Activate the virtual environment for python
  reticulate::use_virtualenv(here::here("R","pyvenv"))

  # Load python script with functions to extract texts
  reticulate::source_python(here::here("R","extract-text-pdf-python.py"))

  text_list <- convert_pdf(pdf_list) %>% unlist()

  test_list

}


# read csv file with pages that are useful for the analysis
# emu_theses <- read_csv(here("data", "raw", "emu-theses.csv")) %>%
#   mutate(pdf_filename = paste0(year, "-", author_surname, ".pdf")) %>%
#   mutate(text = "")
# pdf_list<- as.list( here("data", "raw", emu_theses$"pdf_filename") )



