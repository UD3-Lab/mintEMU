test_that("clean_basic function works correctly", {

  # Test input
  input_text <- "  This is A    test!!  123 ab2 żółw"

  # Expected output
  expected_output <- "this is a test ab2 żółw"

  # Run the function
  output <- clean_basic(input_text)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a lowercase string with no punctuation, extra spaces or numbers."
  )

})

test_that("head_text function works correctly", {

  # Test input
  input_text <- c("This is the 1 sentence. This is the 2 sentence.",
                  "This is the 3 sentence.", "This is the 4 sentence." )

  # Expected output
  expected_output <- c("This is the 1 sentence.",
                       "This is the 3 sentence.")

  # Run the function
  output <- head_text(input_text, head_n = 2, sub_start = 1, sub_end = 23)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of length `head_n` with each string truncated to length `sub_end`."
  )

})

test_that("find_meta_stopwords function works correctly", {
  # Test input
  metadata <- data.frame(
    author_firstname = c("John", "Jane", "Bob", "Sue"),
    author_surname = c("Smith", "Doe", "Johnson", "Johnson"),
    author_firstname2 = c(NA, "David", "Jack", ""),
    author_surname2 = c(NA, "Jones", "Johnson", ""),
    title = c(
      "A Tale of Two Cities",
      "To Kill a Mockingbird",
      "The Great Gatsby",
      "The Catcher in the Rye"
    )
  )

  # Expected output
  expected_output <-
    list(
      author1 = c("john smith", "jane doe", "bob johnson", "sue johnson"),
      author2 = c(NA, "david jones", "jack johnson", ""),
      title = c(
        "a tale of two cities",
        "to kill a mockingbird",
        "the great gatsby",
        "the catcher in the rye"
      )
    )

  # Run the function
  output <- find_meta_stopwords(metadata, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of cleaned and combined strings from stopword columns.")

})

