test_that("get_top_words_per_document returns top word for each title", {
  df_input <-
    tibble::tibble(
      title = c(rep("t1", 6), rep("t2", 6), rep("t3", 7)),
      word = c(
        rep("w1", 3),
        rep("w2", 2),
        rep("w3", 1),
        rep("w1", 2),
        rep("w2", 3),
        rep("w3", 1),
        rep("w1", 4),
        rep("w2", 2),
        rep("w3", 1)
      ))

  expected_output <- tibble::tibble(
    title = c("t1", "t2", "t3"),
    word = c("w1", "w2", "w1"),
    n = c(3, 3, 4))

  df_output <-
    get_top_words_per_document(
      df_input,
      top_n = 1,
      title_col = "title",
      word_col = "word",
      min_count = 1
    )

  expect_equal(df_output, expected_output)

})

test_that("get_topics throws an error if k is less than 2", {
  expect_error(get_topics(mtcars, k = 1))
})
