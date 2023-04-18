#' Convert pdf to text
#'
#' Function returns a list of files from the directory chosen by the user and to be converted.
#' Checks if the provided file path exists and whether it holds the adequate file formats (pdf).
#'
#' @param pdf_filenames  A vector of paths to input (pdf) files
#' @return A vector of body of text; one string per pdf file
#' @export
convert_pdf_text <-function(pdf_filenames, by_page = FALSE){

  pdf_list<- as.list(pdf_filenames)

  # Create virtual environment
  if ( !file.exists(here::here("inst/python", "pyvenv") )) {
    reticulate::install_python(version = '3.9.7')
    reticulate::virtualenv_create(envname = here::here("inst/python","pyvenv"),
                                  version = "3.9.7")
    reticulate::py_install("PyMuPDF == 1.21.0", envname = here::here("inst/python","pyvenv"))
  }

  # Activate the virtual environment for python
  reticulate::use_virtualenv(here::here("inst/python","pyvenv"))

  # Load python script with functions to extract texts
  reticulate::source_python(here::here("inst/python","extract-text-pdf-python.py"))

  if (by_page == FALSE)
    text_list <- convert_pdf(pdf_list) %>% unlist()
  else
    text_list <- convert_pdf(pdf_list, by_page = reticulate::r_to_py(TRUE)) # %>% unlist()

  text_list
}






