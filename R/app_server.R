#' @export
app_server <- function(session,input, output) {

  ## Use the thematic package to adjust plot styles to app theme ---------------
  thematic::thematic_shiny()
  bs4Dash::useAutoColor()

  ## Get the data ---------------------- ---------------------------------------

  data("emu_clean")
  data("emu_metadata")
  emu_theses <- dplyr::left_join(emu_clean, emu_metadata, by = "ID")

  # Update filters

  shiny::updateSliderInput(session,"year", label = "Select year:", step = 1,
                           min = min(emu_theses$graduation_year),
                           max = max(emu_theses$graduation_year),
                           value = c(min(emu_theses$graduation_year),
                                     median(emu_theses$graduation_year))
                             )

  shiny::updateSelectInput(session,"exchange", label = "Select exchange semester:",
                           choices = c('ALL', emu_theses$graduation_year |>
                                         as.character() |>
                                         unique()|>
                                         sort()
                           ),
                           selected = 'ALL')


  # Tool tips ------------------------------------------------------------------
  bs4Dash::addTooltip(id = "year",
                      options = list(title = "Filter results by graduation year",
                                     placement = "right"))

  bs4Dash::addTooltip(id = "exchange",
                      options = list(
                        title = "Filter results by exchange semester",
                        placement = "right")
                      )

  # shiny::updateSelectInput(session, "exchange", label = "Select exchange semester:",
  #                   choices = c('ALL',emu_theses$exchange_semester |>
  #                                 as.character() |>
  #                                 unique()|>
  #                                 sort()  ),
  #                   selected='ALL')

  # Create reactive data object
  emu_reactive_all <- shiny::reactive({
    emu_theses
    })

  # Create reactive data object without the year filter
  emu_reactive <- shiny::reactive({
    emu_theses |>
      dplyr::filter(graduation_year >= input$year[1] & graduation_year <= input$year[2])
  })
 # Render plots

  # ID of value clicked on the map

  clicked_id <- shiny::reactive({input$thesis_location_map_marker_click$id}) |>
    shiny::bindEvent(input$thesis_location_map_marker_click)

  # Thesis selected on the map
  clicked_thesis <- shiny::reactive({
    emu_reactive() |>
    dplyr::filter(ID  == clicked_id())
  })

 # Plot kit  ------------------------------------------------------------------
  #Define theme here
  theme_mintEMU = ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none", title = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 15))
 # Render outputs  ------------------------------------------------------------

  # Map
   output$thesis_location_map <- leaflet::renderLeaflet({

    set.seed(2023)

    data <-  emu_reactive() |>
      dplyr::mutate(longitude = jitter(longitude, factor = 0.01),
                    latitude  = jitter(latitude , factor = 0.01)
                    )

    icons <- leaflet::awesomeIcons(
      icon = "pen",
      iconColor = "#fff",
      library = "fa",
      markerColor = "blue"
    )

    data |>
      leaflet::leaflet() |>
      leaflet::addProviderTiles("Stamen.TonerLite") |>
      leaflet::addAwesomeMarkers(layerId = data$ID,
                                 icon = icons,
                                 label = data$location,
                                 popup = paste0("<b>Title:</b> ", data$title,
                                                "<br/>",
                                                "<b>Location:</b> ", data$location),
                                 clusterOptions = leaflet::markerClusterOptions()

                                 )
    })

   # Box generated for a single title
   output$clicked_box <- shiny::renderUI({

     empty_box <- is.null(clicked_id())

     bs4Dash::box(title = paste(clicked_thesis()$ID, clicked_thesis()$title),
                  status = "lightblue",
                  collapsed = empty_box,
                  width = NULL,
                  textOutput('clickid'),
                  plotOutput("top_words")
                  )
     })

   # Graph with top 50 words
   output$top_words <- renderPlot({

     clicked_words <- clicked_thesis() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     top_words <-
       get_top_words_per_corpus(clicked_words , 20, 'word')

     ggplot2::ggplot(top_words) +
       ggplot2::aes(x = word, y = n) +
       ggplot2::geom_col()

   })

   # Word cloud of the selected thesis
   output$wordcloud <- wordcloud2::renderWordcloud2({
     clicked_words <- clicked_thesis() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word") |>
       dplyr::count(word) |>
       dplyr::arrange(-n)

     wordcloud2::wordcloud2(clicked_words)

   })


   # Word cloud of the selected thesis
   output$clickid <- renderText({
     clicked_id <- input$thesis_location_map_marker_click$id

     clicked_id

   }) |>
     shiny::bindEvent(input$thesis_location_map_marker_click)


   # Graph withTop N words
   output$Top_n_words <- renderPlot({

     emu_words <- emu_reactive() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     top_words2 = get_top_words_per_corpus(emu_words, 5, word_col = "word")

     ggplot2::ggplot(top_words2) +
       ggplot2::aes(x = reorder(word, n), y = n, fill = reorder(word, n)) +
       ggplot2::geom_col() +
       ggplot2::coord_flip() +
       theme_mintEMU +
       ggplot2::scale_fill_viridis_d(begin = 0.8, end = 0) +
       ggplot2::xlab("Count") + ggplot2::ylab("Word")


   })


   # Graph with Evolution of top words
   output$Evolution_of_top_words <- plotly::renderPlotly({


     emu_words <- emu_reactive_all() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     emu_words_filtered <- emu_reactive() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     top_words2 = get_top_words_per_corpus(emu_words_filtered, 5, word_col = "word")

     word_order <- emu_words |>
       dplyr::group_by(word, graduation_year) |>
       dplyr::count(word, sort = TRUE) |>
       dplyr::ungroup() |>
       dplyr::arrange(graduation_year, -n) |>
       dplyr::group_by(graduation_year) |>
       dplyr::mutate(relative_frequency = n/sum(n)*100) |>
       dplyr::mutate(rank = round(rank(-n), 0)) |>
       dplyr::ungroup() |>
       dplyr::filter(word %in% top_words2$word)

     word_order$word <- factor(word_order$word, levels = top_words2$word, ordered = TRUE)
     highlight <- plotly::highlight_key(word_order, ~word)

     plot <- plotly::ggplotly(
       ggplot2::ggplot(highlight, ggplot2::aes(x = graduation_year, y = relative_frequency, color = word, label = rank)) +
         ggplot2::aes(fill = "white") +
         ggplot2::geom_path(lwd = 0.1) +
         ggplot2::geom_point(size = 8, shape = 21, fill = "white", stroke = 1.2) +
         ggplot2::geom_text(ggplot2::aes(x = graduation_year), color = "black") +
         theme_mintEMU +
         ggplot2::scale_color_viridis_d(begin = 0, end = 0.8)+
         ggplot2::xlab("Year") + ggplot2::ylab("Frequency (%)"))

    plotly::highlight(plot, on = "plotly_click", off = "plotly_deselect")

   })



  }
