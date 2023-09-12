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
  emu_reactive <- shiny::reactive({
    emu_theses |>
      dplyr::filter(graduation_year > input$year[1] & graduation_year < input$year[2])
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

   # Graph with top N words
   output$Top_n_words <- renderPlot({

     emu_words <- emu_reactive() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word)) |>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     words_per_year = data.frame()

     for (grad_year in sort(unique(emu_words$graduation_year))) {
       result <- get_top_words_per_corpus(emu_words[emu_words$graduation_year == grad_year,], 5, word_col = "word")
       result$year <- grad_year
       words_per_year <- rbind(words_per_year, result)
     }

     word_order <- words_per_year |>
       dplyr::group_by(word) |>
       dplyr::mutate(m = sum(n)) |>
       dplyr::select(-year, -n)|>
       unique() |>
       dplyr::arrange(-m) |>
       dplyr::select(word)

     words_per_year$word <- factor(x = words_per_year$word,  levels = word_order[["word"]])

     ggplot2::ggplot(words_per_year) +
       ggplot2::aes(x = word, y = n, fill = as.factor(year)) +
       ggplot2::geom_bar(position = "stack", stat = "identity") +
       colorspace::scale_fill_discrete_sequential(palette = c("BluGrn"), name = "Year") +
       ggplot2::xlab("Words") + ggplot2::ylab("Count") + ggplot2::ggtitle("Most Used 5 Words per Year") +
       ggplot2::theme_minimal() +
       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.4)) +
       ggplot2::theme(axis.line.y = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())

   })



  }
