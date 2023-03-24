test_that("clean_basic function works correctly", {

  # Test input
  input_text <- "  This is A    test!!  123 ab2 żółw"

  # Expected output
  expected_output <- "this is a test 123 ab2 żółw"

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

test_that("find_meta_stopwords function produces correct list when all fields present", {
  # Test input
  metadata <- data.frame(
    first_name = c("Kurt  \n", "Oscar", "Ken"),
    last_name = c("Vonnegut", "Wilde", "Kesey"),
    first_name_2 = c("Kilgore", "Henry", "Chief"),
    last_name_2 = c("Trout", "Wotton", "Bromden"),
    title = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )

  # Expected output
  expected_output <-
    list(
      author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
      author2 = c("kilgore trout", "henry wotton", "chief bromden"),
      title = c(
        "breakfast of champions",
        "the picture of dorian gray",
        "one flew over the cuckoos nest"
      )
    )

  # Run the function
  output <- find_meta_stopwords(metadata, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a list of cleaned and combined strings from stopword columns.")

})

test_that("find_meta_stopwords function produces correct list when some fields are empty", {
  # Test input
  metadata <- data.frame(
    first_name = c("Kurt \n", "Oscar", "Ken"),
    last_name = c("Vonnegut", "Wilde", "Kesey"),
    first_name_2 = c("Kilgore", "", ""),
    last_name_2 = c("Trout", "", "Bromden"),
    title = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )

  # Expected output
  expected_output <-
    list(
      author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
      author2 = c("kilgore trout", "", "bromden"),
      title = c(
        "breakfast of champions",
        "the picture of dorian gray",
        "one flew over the cuckoos nest"
      )
    )

  # Run the function
  output <- find_meta_stopwords(metadata, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of cleaned and combined strings from stopword columns.")

})

test_that("find_meta_stopwords function produces correct list when some fields are NA", {
  # Test input
  metadata <- data.frame(
    first_name = c("Kurt \n", "Oscar", "Ken"),
    last_name = c("Vonnegut", "Wilde", "Kesey"),
    first_name_2 = c("Kilgore", NA, NA),
    last_name_2 = c("Trout", NA, "Bromden"),
    title = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )

  # Expected output
  expected_output <-
    list(
      author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
      author2 = c("kilgore trout", "", "bromden"),
      title = c(
        "breakfast of champions",
        "the picture of dorian gray",
        "one flew over the cuckoos nest"
      )
    )

  # Run the function
  output <- find_meta_stopwords(metadata, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of cleaned and combined strings from stopword columns.")

})

test_that("find_meta_stopwords function produces correct list when the names of the metadata fields are adapted",{
    # Test input
    metadata <- data.frame(
      first_author_firstname = c("Kurt  \n", "Oscar", "Ken"),
      first_author_surname = c("Vonnegut", "Wilde", "Kesey"),
      second_author_firstname = c("Kilgore", "Henry", "Chief"),
      second_surname = c("Trout", "Wotton", "Bromden"),
      titles = c(
        "Breakfast of Champions",
        "The Picture of Dorian Gray",
        "One Flew Over the Cuckoo's Nest"
      )
    )

    # Expected output
    expected_output <-
      list(
        author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
        author2 = c("kilgore trout", "henry wotton", "chief bromden"),
        title = c(
          "breakfast of champions",
          "the picture of dorian gray",
          "one flew over the cuckoos nest"
        )
      )

    # Run the function
    output <-
      find_meta_stopwords(
        metadata,
        convert_to_regex = FALSE,
        stop_cols =  list(
          'author1' = c('first_author_firstname', 'first_author_surname'),
          'author2' = c('second_author_firstname', 'second_surname'),
          'title'  = 'titles'
        )
      )

    # Check the output against the expected result
    expect_equal(output, expected_output,
                 info = "Output should be a vector of cleaned and combined strings from stopword columns.")

  })

test_that("find_meta_stopwords function produces correct list when the names of the metadata fields are adapted",{
  # Test input
  metadata <- data.frame(
    first_author_firstname = c("Kurt  \n", "Oscar", "Ken"),
    first_author_surname = c("Vonnegut", "Wilde", "Kesey"),
    second_author_firstname = c("Kilgore", "Henry", "Chief"),
    second_surname = c("Trout", "Wotton", "Bromden"),
    titles = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )

  # Expected output
  expected_output <-
    list(
      author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
      author2 = c("kilgore trout", "henry wotton", "chief bromden"),
      title = c(
        "breakfast of champions",
        "the picture of dorian gray",
        "one flew over the cuckoos nest"
      )
    )

  # Run the function
  output <-
    find_meta_stopwords(
      metadata,
      convert_to_regex = FALSE,
      stop_cols =  list(
        'author1' = c('first_author_firstname', 'first_author_surname'),
        'author2' = c('second_author_firstname', 'second_surname'),
        'title'  = 'titles'
      )
    )

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of cleaned and combined strings from stopword columns.")

})

test_that("find_meta_stopwords function produces correct list when there are additional metadata fields",{
  # Test input
  metadata <- data.frame(
    author_firstname = c("Kurt  \n", "Oscar", "Ken"),
    author_surname = c("Vonnegut", "Wilde", "Kesey"),
    character_firstname = c("Kilgore", "Henry", "Chief"),
    character_surname = c("Trout", "Wotton", "Bromden"),
    character_firstname2 = c("", "Dorian", "Randle"),
    character_surname2 = c("", "Gray", "McMurphy"),
    title = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )


  # Expected output
  expected_output <-
    list(
      author1 = c("kurt vonnegut", "oscar wilde", "ken kesey"),
      character1 = c("kilgore trout", "henry wotton", "chief bromden"),
      character2 = c("", "dorian gray", "randle mcmurphy"),
      title = c(
        "breakfast of champions",
        "the picture of dorian gray",
        "one flew over the cuckoos nest"
      )
    )

  # Run the function
  output <-
    find_meta_stopwords(
      metadata,
      convert_to_regex = FALSE,
      stop_cols =  list(
        'author1' = c('author_firstname', 'author_surname'),
        'character1' = c('character_firstname', 'character_surname'),
        'character2' = c('character_firstname2', 'character_surname2'),
        'title'  = 'title'
      )
    )

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of cleaned and combined strings from stopword columns.")

})

test_that("find_meta_stopwords function produces correct regex when all fields present", {
  # Test input
  metadata <- data.frame(
    author_firstname = c("Kurt  \n", "Oscar", "Ken"),
    author_surname = c("Vonnegut", "Wilde", "Kesey"),
    author_firstname2 = c("Kilgore", "Henry", "Chief"),
    author_surname2 = c("Trout", "Wotton", "Bromden"),
    title = c(
      "Breakfast of Champions",
      "The Picture of Dorian Gray",
      "One Flew Over the Cuckoo's Nest"
    )
  )

  # Expected outpu
  expected_matches <- c("kurt vonnegut|kilgore trout|breakfast of champions",
                       "oscar wilde|henry wotton|the picture of dorian gray",
                       "ken kesey|chief bromden|one flew over the cuckoos nest")

  # Run the function
  output <- find_meta_stopwords(metadata, convert_to_regex = TRUE)

  # Check the output against the expected result
  expect_match(expected_matches[1], output[1],fixed = TRUE,
               info = "Output should be a regexp.")

  expect_match(expected_matches[2], output[2],fixed = TRUE,
               info = "Output should be a regexp.")

  expect_match(expected_matches[3], output[3],fixed = TRUE,
               info = "Output should be a regexp.")

})

test_that("urbanism_stopwords function produces correct vector of stopwords", {

  # Test input
  additional_stopwords <- c("house", "block", "hut", "playground")

  # Expected output
  expected_output <-
    c(
      "house",
      "block",
      "hut",
      "playground",
      "city",
      "cities",
      "urban",
      "urbanism",
      "hab",
      "km"
    )

  # Run the function
  output <- urbanism_stopwords(additional_stopwords, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of words with related to urbanism."
  )

})

test_that("urbanism_stopwords function cleans the additional stopwords before adding them to the list", {

  # Test input
  additional_stopwords <- c("House", "Block", "hut ", "playground")

  # Expected output
  expected_output <-
    c(
      "house",
      "block",
      "hut",
      "playground",
      "city",
      "cities",
      "urban",
      "urbanism",
      "hab",
      "km"
    )

  # Run the function
  output <- urbanism_stopwords(additional_stopwords, convert_to_regex = FALSE)

  # Check the output against the expected result
  expect_equal(output, expected_output,
               info = "Output should be a vector of words with related to urbanism."
  )

})

test_that("urbanism_stopwords function gives warning if the additional stopwords include multiple-word elements", {

  # Test input
  additional_stopwords <- c("house", "block", "hut", "children's playground")

  # Run the function
  output <- urbanism_stopwords(additional_stopwords)

  # Check the output against the expected result
  expect_warning(urbanism_stopwords(additional_stopwords),
               info = "Function should warn the user about adding multiple-word elements to the word-list"
  )

})
