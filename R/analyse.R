#' Get word counts for corpus
#'
#' @param data A data frame comprising a corpus of texts
#' @param text_col Name of the column in which the text is stored
#'
#' @return A numeric vector of word counts, one for each document in the corpus
#' @export
get_word_counts <- function(data, text_col = NULL) {
  text_col <- rlang::sym(text_col)

  word_counts <- vector(mode = "numeric", length = nrow(data))
  for (i in 1:nrow(data)) {
    word_counts[i] <-
      tidytext::unnest_tokens(tbl = data[i,], output = word, input = !!text_col) |> nrow()
  }

  word_counts
}

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
#' @param words Data frame with theses, containing a column with words.
#' @param top_n The number of top words to be returned by the function.
#' @param word_col Name of the column from the input data frame containing all words.
#'
#' @return A data frame with the top n words across all documents.
#' @export
get_top_words_per_corpus <- function(words, top_n, word_col = "word") {
  word <- rlang::sym(word_col)

  data_top_n <- words |>
    dplyr::group_by(!!word) |>
    dplyr::count(!!word, sort = TRUE) |>
    dplyr::ungroup() |>
    dplyr::slice_max(n, n = top_n)

  data_top_n
}

#' Extract top n terms with the highest count across all topics
#'
#' @param data LDA model.
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

  # Extract data frame with topics, terms and beta statistic from LDA model
  topics <- tidytext::tidy(data, matrix = "beta")

  topic <- rlang::sym(topic_col)
  beta <- rlang::sym(beta_col)

  data_top_terms <- topics |>
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
    dtm,
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
    tidytext::bind_tf_idf(!!rlang::sym(word_col), !!rlang::sym(id_col), n) |>
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

  topic_prob <- modeltools::posterior(lda)$topics
  term_prob <- modeltools::posterior(lda)$terms

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

  topic_prob <- modeltools::posterior(lda)$topics
  term_prob <- modeltools::posterior(lda)$terms

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

# TODO check if this can be included in the Shiny dashboard
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
vis_lda <- function(phi, theta, dtm) {

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

#' Visualise top words per topic
#'
#' @param ... Arguments to be passed to `get_top_words_per_topic()`.
#'
#' @return `ggplot` bar chart of top n words faceted by topic.
#' @export
vis_top_words_per_topic <- function(...) {

  top_terms <- mintEMU::get_top_words_per_topic(...)

  top_terms |>
    ggplot2::ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ topic, scales = "free_y", ncol = 3) +
    ggplot2::ylab("Beta") +
    ggplot2::xlab("Top terms") +
    ggplot2::labs(title = "Top n terms per topic") +
    ggplot2::coord_flip()
}

#' Visualise top words per corpus
#'
#' @param ... Arguments to be passed to `get_top_words_per_corpus()`.
#'
#' @return `ggplot` bar chart of top n words colored by topic.
#' @export
vis_top_words_per_corpus <- function(lda, words, top_n = 20) {

  terms <- as.data.frame(modeltools::posterior(lda)$terms)
  rownames(terms) <- mintEMU::name_topics(modeltools::posterior(lda)$terms)

  terms <- terms |>
    dplyr::mutate(topic = rownames(terms)) |>
    tidyr::pivot_longer(-topic,
                        names_to = "term",
                        values_to = "prob") |>
    dplyr::group_by(term) |>
    dplyr::mutate(max_topic = topic[which.max(prob)]) |>
    dplyr::filter(topic == max_topic) |>
    dplyr::ungroup()

  emu_words_topics <- words |>
    left_join(terms, by = c("word" = "term"))

  top_terms <- mintEMU::get_top_words_per_corpus(words, top_n)

  top_terms |>
    dplyr::left_join(terms, by = c("word" = "term")) |>
    ggplot2::ggplot(aes(reorder(word, n), n)) +
    ggplot2::geom_col(aes(fill = max_topic)) +
    ggplot2::geom_text(ggplot2::aes(label = n), size = 2, hjust = 1.1) +
    ggplot2::coord_flip() +
    ggplot2::xlab("Word") +
    ggplot2::ylab("Frequency") +
    ggplot2::labs(title = paste0("Top ", top_n, " most used words in the corpus of theses")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
