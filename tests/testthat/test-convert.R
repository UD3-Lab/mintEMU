test_that("convert function works", {
setwd("C:/Users/skubilay/Desktop/mintEMU")
  # Test input
  input_text <- "C:/Users/skubilay/Desktop/mintEMU/analysis/data/raw_data/2007-Umemura.pdf"

  # Expected output
  expected_output <- "regional metropolis and public transport connection"

  # Run the function
  output <- convert_pdf_text(input_text)
  matching_excerpt <-stringr::str_detect(output, expected_output)

  # Check the output against the expected result
  expect_true(matching_excerpt, expected_output,
              info = "Output should be a lowercase string with no punctuation, extra spaces or numbers."
  )

})
