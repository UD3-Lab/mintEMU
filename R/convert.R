#' Convert pdf to text
#'
#' Function returns a list of files from the directory chosen by the user and to be converted.
#' Checks if the provided file path exists and whether it holds the adequate file formats (pdf).
#'
#' @param pdf_filenames A vector of paths to input (pdf) files
#' @param start_pages A numeric vector with the start page numbers from where the original text should be converted
#' @param end_pages A numeric vector with the end page numbers until where the original text should be converted
#' @return A vector of body of text; one string per pdf file
#' @export
convert_pdf_text <- function(pdf_filenames, start_pages, end_pages){

  pdf_list<- as.list(pdf_filenames)

  py_path <- system.file("python", package = 'mintEMU' )
  py_env_path <- system.file("python/pyvenv", package = 'mintEMU' )
  pyf_path <- system.file("python","extract-text-pdf-python.py", package = 'mintEMU' )


  # Create virtual environment
  if (!file.exists(py_env_path)) {
    if (is.null(reticulate::py_version())) {
      t <-  try(reticulate::use_python_version("3.9:latest", required = TRUE))

    if (inherits(t, "try-error"))
        reticulate::install_python(version = "3.9:latest")
    }
    else if (reticulate::py_version() != "3.9" )
      stop("An incompatible Python version has been initialised. Restart R session to continue.")


    py_env_path <- file.path(py_path, "pyvenv")
    reticulate::virtualenv_create(envname = py_env_path,
                                  version = "3.9:latest")
    reticulate::py_install("PyMuPDF == 1.21.0", envname = py_env_path)
  }

  # Activate the virtual environment for python
  reticulate::use_virtualenv(py_env_path)

  # Load python script with functions to extract texts
  reticulate::source_python(pyf_path)

  text_list <- convert_pdf(pdf_list, start_pages, end_pages) |> unlist()

  text_list
}






