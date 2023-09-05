test_that("get_ngrams returns correct data frame", {
  df_input <- tibble::tibble(
    title = "Title",
    text = "This is a short text preparation test."
  )

  expected_output <- tibble::tibble(
    title = rep("Title", 3),
    text = rep("This is a short text preparation test.", 3),
    ngram = c("short text", "text preparation", "preparation test"),
    w_1 = c("short", "text", "preparation"),
    w_2 = c("text", "preparation", "test")
  )

  df_output <- get_ngrams(df_input, n = 2,
                          title_col = "title",
                          text_col = "text")

  expect_equal(expected_output, df_output)
})
