test_that("convert function works", {
  # Test input
  input_text <- here::here("analysis", "data", "raw_data", "2007-Umemura.pdf")

  # Expected output
  expected_output <- strsplit("re-generating the shrinking japanese city urban acupuncture junya umemura mentor luisa calabrese", split = " ") |>
    unlist() |> sort()

  # Run the function
  output <- convert_pdf_text(list(input_text), start_pages = "1", end_pages = "1") |> mintEMU::clean_basic() |> strsplit(split = " ") |>
    unlist() |> sort()
  # matching_excerpt <- stringr::str_detect(output, expected_output)

  # Check the output against the expected result
  expect_in(output, expected_output)

})
