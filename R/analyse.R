#' Extract top n words with the highest count per document
#'
#' Given a data frame with a column storing the words found in a document
#' identified with a ID column and a positive number top_n, this function
#' returns the top n words with the highest count in each document.
#'
#' @param data Data frame with theses, containing a column with words.
#' @param top_n The number of top words to be returned by the function.
#' @param id_col The name of the column from the input data frame containing the ID of the document.
#' @param word_col Name of the column from the input data frame containing all words.
#' @param min_count Minimum count to be considered. Words below this value will not be included in the returned data frame.
#'
#' @return A data frame with the top n words with a count above the given minimum.
#' @export
get_top_words_per_document <- function(data,
                                       top_n,
                                       id_col = "ID",
                                       word_col = "word",
                                       min_count = 1) {
  ID <- rlang::sym(id_col)
  word <- rlang::sym(word_col)

  data_top_n <- data |>
    dplyr::group_by(!!rlang::sym(id_col)) |>
    dplyr::count(!!rlang::sym(word_col), sort = TRUE) |>
    dplyr::slice_max(n, n = top_n) |>
    dplyr::filter(n > min_count) |>
    dplyr::ungroup()

  data_top_n
}

#' Extract top n words with the highest count across all documents
#'
#' Given a data frame with a column storing the words found in a document
#' and a positive number top_n, this function returns the top n words with the
#' highest count across all documents.
#'
#' @param data Data frame with theses, containing a column with words.
#' @param top_n The number of top words to be returned by the function.
#' @param word_col Name of the column from the input data frame containing all words.
#'
#' @return A data frame with the top n words across all documents.
#' @export
get_top_words_per_corpus <- function(data, top_n, word_col = "word") {
  word <- rlang::sym(word_col)

  data_top_n <- data |>
    dplyr::group_by(!!word) |>
    dplyr::count(!!word, sort = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice_max(n, n = top_n)

  data_top_n
}

#' Extract top n terms with the highest count across all topics
#'
#' @param data Data frame with topics, terms and beta statistic.
#' @param top_n The number of top words to be returned by the function.
#' @param topic_col Name of the column containing topics.
#' @param beta_col Name of the column containing the beta statistic for all terms.
#'
#' @return A data frame with the top n terms across all topics.
#' @export
get_top_words_per_topic <- function(data,
                                    top_n,
                                    topic_col = "topic",
                                    beta_col = "beta") {
  topic <- rlang::sym(topic_col)
  beta <- rlang::sym(beta_col)

  data_top_terms <- data |>
    dplyr::group_by(!!topic) |>
    dplyr::top_n(top_n,!!beta) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!topic,-beta)

  data_top_terms
}

#' Convert data frame with word frequencies into document term matrix
#'
#' @param data Data frame containing colomns with IDs, words and word counts
#' @param id_col Name of the ID column of the input data frame
#' @param word_col Name of the word column of the input data frame
#'
#' @return A document term matrix
#' @export
convert_to_dtm <- function(data, id_col = NULL, word_col = NULL, min_term_freq = 1) {
  data_dtm <- data |>
    dplyr::count(!!rlang::sym(id_col), !!rlang::sym(word_col), sort = TRUE) |>
    dplyr::filter(n > min_term_freq) |>
    tidytext::cast_dtm(!!rlang::sym(id_col), !!rlang::sym(word_col), n)

  data_dtm
}

#' Get topics for a given data frame of documents and words
#'
#' @param data Data frame with theses, containing a column with words.
#' @param k Number of desired clusters.
#' @param matrix Statistics to be computed. Can be "beta" or "gamma"
#' @param id_col The name of the column from the input data frame containing the ID of the document.
#' @param word_col Name of the column from the input data frame containing all words.
#' @param seed Seed to control pseudorandomisation.
#'
#' @return An LDA model object.
#' @export
get_topics <-
  function(data,
           id_col = "ID",
           word_col = "word",
           k = 2,
           min_term_freq = 1,
           seed = 2023,
           ...) {
    stopifnot("k must be an integer of at least 2" = k >= 2)

    # The {topicmodels} package requires a document-term matrix as input
    dtm <- convert_to_dtm(data,
                          id_col = id_col,
                          word_col = word_col,
                          min_term_freq = min_term_freq)

    # We will use the Latent Dirichlet Allocation (LDA) algorithm for topic modeling
    # In LDA, each document is a mixture of topics and each topic is a mixture of words
    lda <- topicmodels::LDA(dtm,
                            k = k,
                            control = list(seed = seed, ...))
  }

#' Add pseudo-names to topics in an LDA model
#'
#' Pseudo-names are obtained by pasting together the top n terms with
#' the highest probability within a topic.
#'
#' @param beta Prior topic distribution over words
#' @param n_words Number of words to include in the pseudo-names
#'
#' @return Character vector of topic pseudo-names
#' @export
#'
name_topics <- function(beta, n_words = 5) {

  topic_names <- c()

  for (i in 1:nrow(beta)) {
    name <- paste(names(head(sort(beta[i, ], decreasing = TRUE), n_words)), collapse = " ")
    topic_names <- c(topic_names, name)
  }

  topic_names
}

#' Get number of topics for LDA model empirically
#'
#' @param dtm Document term matrix
#' @param topics Range of number of topics
#' @param metrics Metrics used to evaluate the number of topics
#' @param method Method used for fitting the LDA model
#' @param control Setting tuning parameters
#' @param verbose Show progress in the console
#'
#' @return Plot and possibly a K value
#' @export
#'
get_number_of_topics <- function(dtm,
                                 topics = seq(from = 2, to = 30, by = 1),
                                 metrics = c("CaoJuan2009",  "Deveaud2014"),
                                 method = "Gibbs",
                                 control = list(seed = 2023),
                                 verbose = TRUE) {

  result <- ldatuning::FindTopicsNumber(
    emu_dtm,
    topics = topics,
    metrics = metrics,
    method = method,
    control = control,
    verbose = verbose
  )

  # ldatuning::FindTopicsNumber_plot(result)

  # if (metrics == c("CaoJuan2009",  "Deveaud2014")) {
    K = result$topic[which.max(result$Deveaud2014 - result$CaoJuan2009)]
  # } else {
    # NULL
  # }
}

#' Sliding window
#'
#' Sliding window function used to calculate skipgram probabilities,
#' that is, how often each word occurs near each other word.
#'
#' @param tbl A vector of tokens.
#' @param window_size A small window size of 3-4 shows how a word is used, while a larger window size of, say, 10 gives more information about the domain topic.
#'
#' @return Data frame with skipgram probabilities
#' @export
#'
slide_windows <- function(tbl, window_size) {
  # Iterate through an input vector with a fixed window size
  skipgrams <- slider::slide(
    tbl,
    ~.x,
    .after = window_size - 1,
    .step = 1L,
    .complete = TRUE  # Only evaluate function on complete windows
  )

  # Run even when unsuccessful
  safe_mutate <- purrr::safely(mutate)

  # Map safe_mutate to skipgrams
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))

  out |>
    purrr::transpose() |>  # Transpose `result` and `error` into columns,
    purrr::pluck("result") |>  # only keep `result`,
    plyr::compact() |>  # remove any NULL values [is this for the last windows?],
    dplyr::bind_rows()  # and bind all rows into a single data frame [?].
}

#' Pointwise mutual information of pairs of items
#'
#' @param data Data frame of theses
#' @param id_col Name of the ID column
#' @param text_col Name of the text column to be tokenised
#' @param parallel If `parallel = TRUE`, compute in parallel
#'
#' @return Data frame with two items and their co-occurrence
#' @export
#'
get_pmi <- function(data,
                    id_col = NULL,
                    text_col = NULL,
                    parallel = TRUE,
                    window_size = 4L) {

  # To reduce computation time, we use the {furrr}
  # and underlying {future} packages for parallel processing
  if (parallel){
    future::plan(multisession)
  }

  # Prepare tidy data frame for word embeddings
  tidy_emu_theses <- data |>
    dplyr::select(!!rlang::sym(id_col), !!rlang::sym(text_col)) |>
    tidytext::unnest_tokens(word, !!rlang::sym(text_col)) |>
    dplyr::add_count(word) |>
    dplyr::filter(n >= 50) |>
    dplyr::select(-n)

  # Create a nested data frame with one row per thesis
  nested_words <- tidy_emu_theses |>
    tidyr::nest(words = c(word))

  # To calculate how often words occur on their own or together with other words
  # we use point-wise mutual information (PMI)
  tidy_pmi <- nested_words |>
    dplyr::mutate(words = furrr::future_map(words, slide_windows, window_size)) |>
    tidyr::unnest(words) |>
    tidyr::unite(window_id, !!rlang::sym(id_col), window_id) |>
    widyr::pairwise_pmi(word, window_id)

  tidy_pmi

}

#' Get nearest neighbours for a given token
#'
#' @param df Data frame with word vectors
#' @param token Word for which the nearest neighbours are calculated
#'
#' @return Data frame with nearest neighbours
#' @export
#'
nearest_neighbors <- function(df, token) {
  df |>
    widyr::widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))

        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) |>
    dplyr::select(-item2)
}

#' Visualise first n components
#'
#' @param word_vectors Word vectors
#' @param dim Number of dimensions to visualise
#' @param top_n Number of top terms to show for each dimension
#'
#' @return ggplot object
#' @export
#'
visualise_first_n_components <- function(word_vectors, dim = 24, top_n = 12) {

  word_vectors |>
    dplyr::filter(dimension <= dim) |>
    dplyr::group_by(dimension) |>
    dplyr::top_n(top_n, abs(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(item1 = tidytext::reorder_within(item1, value, dimension)) |>
    ggplot2::ggplot(aes(item1, value, fill = dimension)) +
    ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(~dimension, scales = "free_y", ncol = 4) +
    tidytext::scale_x_reordered() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Value",
      title = "First 24 principal components for text of theses",
      subtitle = "Top words contributing to the components that explain the most variation"
    )
}

#' Get `tf-idf` statistic
#'
#' @param data Data frame containing colomns with IDs, words and word counts
#' @param id_col Name of the ID column of the input data frame
#' @param word_col Name of the word column of the input data frame
#'
#' @return A data frame including the `tf-idf` statistic for each word
#' @export
get_tf_idf <- function(data, id_col = NULL, word_col = NULL) {
  words_counts <- data |>
    count(!!rlang::sym(id_col), !!rlang::sym(word_col), sort = TRUE) |>
    ungroup()

  words_total <- words_counts |>
    group_by(!!rlang::sym(id_col)) |>
    summarise(total = sum(n))

  words_counts <- left_join(words_counts, words_total)

  words_counts <- words_counts |>
    bind_tf_idf(!!rlang::sym(word_col), !!rlang::sym(id_col), n) |>
    select(-total)

  words_counts
}

#' Get topic proportions in an LDA model
#'
#' @param lda LDA model object
#'
#' @return Data frame with topics and their proportions
#' @export
get_topic_proportions <- function(lda, digits = 2) {

  topic_prob <- posterior(lda)$topics
  term_prob <- posterior(lda)$terms

  ndocs <- length(unique(rownames(topic_prob)))

  # mean probabilities over all documents
  topic_proportions <- colSums(topic_prob) / ndocs
  # assign topic names
  names(topic_proportions) <- mintEMU::name_topics(term_prob)
  # show summed proportions in decreased order
  sorted_topic_proportions <- sort(topic_proportions, decreasing = TRUE) |>
    round(digits = digits)
  data.frame(topic = names(sorted_topic_proportions),
             proportion = sorted_topic_proportions,
             row.names = NULL)
}

#' Count documents per main topic
#'
#' @param lda LDA model
#'
#' @return Data frame with topics and count of documents per main topic
#' @export
count_docs_per_topic <- function(lda) {

  topic_prob <- posterior(lda)$topics
  term_prob <- posterior(lda)$terms

  k <- ncol(topic_prob)
  ndocs <- length(unique(rownames(topic_prob)))

  counts_of_primary_topics <- rep(0, k)
  names(counts_of_primary_topics) <- name_topics(term_prob)

  for (i in 1:ndocs) {
    topics_per_doc <- topic_prob[i,]  # select topic distribution for document i
    # get first element position from ordered list
    primary_topic <- order(topics_per_doc, decreasing = TRUE)[1]

    counts_of_primary_topics[primary_topic] <-
      counts_of_primary_topics[primary_topic] + 1
  }

  sorted_document_counts_per_topic <- sort(counts_of_primary_topics, decreasing = TRUE)
  data.frame(topic = names(sorted_document_counts_per_topic),
             count = sorted_document_counts_per_topic,
             row.names = NULL)
}

#' Visualise LDA model
#'
#' @param phi   A matrix with each row representing the probability
#'              distribution of a topic over terms
#' @param theta A matrix with each row representing the probability
#'              distribution of a documents over topics
#' @param dtm   Document-term matrix used as input to the LDA model
#'
#' @return  Visualise LDA in a browser.
#' @export
visualise_lda <- function(phi, theta, dtm) {

  # Function to approximate the distance between topics
  svd_tsne <- function(x) tsne::tsne(svd(x)$u)

  # Convert DTM into JSON required by the LDAvis package
  json <- LDAvis::createJSON(
    phi = phi,
    theta = theta,
    doc.length = rowSums(as.matrix(dtm)),
    vocab = colnames(dtm),
    term.frequency = colSums(as.matrix(dtm)),
    mds.method = svd_tsne,
    plot.opts = list(xlab="", ylab="")
  )

  # Visualise topics model with LDAvis
  LDAvis::serVis(json)
}

