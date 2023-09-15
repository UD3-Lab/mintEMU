#' @export
app_server <- function(session,input, output) {

  # Use the thematic package to adjust plot styles to app theme ---------------
  thematic::thematic_shiny()
  bs4Dash::useAutoColor()

  # Plot kit  -----------------------------------------------------------------

  # Define color palette
  seq_pal10 <- colorspace::sequential_hcl(10, palette = "BluGrn", rev = TRUE)


  # Get the data --------------------------------------------------------------

  data("emu_clean")
  data("emu_metadata")
  emu_theses <- dplyr::left_join(emu_clean, emu_metadata, by = "ID")

  # Update filters ------------------------------------------------------------

  shiny::updateSliderInput(session,"year",
                           label = "Select graduation year:",
                           step = 1,
                           min = min(emu_theses$graduation_year),
                           max = max(emu_theses$graduation_year),
                           value = c(min(emu_theses$graduation_year),
                                     median(emu_theses$graduation_year))
                             )

  shiny::updateSelectInput(session,"exchange",
                           label = "Select exchange semester:",
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

  # Reactive objects -----------------------------------------------------------

  # Create reactive data object
  emu_reactive <- shiny::reactive({
    emu_theses |>
      dplyr::filter(graduation_year >= input$year[1] & graduation_year <= input$year[2])
   })

  emu_words <- shiny::reactive({
    emu_reactive() |>
      tidytext::unnest_tokens(word, text_clean) |>
      dplyr::mutate(word = textstem::lemmatize_words(word)) |>
      dplyr::anti_join(tidytext::stop_words, by = "word")
  })


  # ID of value clicked on the map
  clicked_id <- shiny::reactive({input$thesis_location_map_marker_click$id}) |>
    shiny::bindEvent(input$thesis_location_map_marker_click)

  # Thesis selected on the map
  clicked_thesis <- shiny::reactive({
    emu_reactive() |>
    dplyr::filter(ID  == clicked_id())
  })

  clicked_title <- shiny::reactive({
    emu_reactive() |>
      dplyr::filter(ID  == clicked_id()) |>
      dplyr::pull(title)

  })



 # Render outputs  ------------------------------------------------------------

 # Leaflet Map ----------------------------------------------------------------
   output$thesis_location_map <- leaflet::renderLeaflet({

    emu_theses |>
      leaflet::leaflet() |>
      leaflet::addProviderTiles("Stamen.TonerLite")

    })


  shiny::observe({
    icons <- leaflet::awesomeIcons(
      icon = "pen",
      iconColor = "#fff",
      library = "fa",
      markerColor = "blue"
      )

    leaflet::leafletProxy("thesis_location_map", session, data = emu_reactive()) |>
      leaflet::clearMarkers() |>
      leaflet::clearMarkerClusters() %>%
      leaflet::addAwesomeMarkers(layerId = emu_reactive()$ID,
                                 icon = icons,
                                 label = emu_reactive()$location,
                                 popup = paste0("<b>Title:</b> ", emu_reactive()$title,
                                                "<br/>",
                                                "<b>Location:</b> ", emu_reactive()$location,
                                                "<br/>",
                                                "<b>Graduation year:</b> ", emu_reactive()$graduation_year
                                                ),

                                 clusterOptions = leaflet::markerClusterOptions()
                                 )
    })

  # Box generated for a single title -------------------------------------------

  shiny::observe({

    empty_box <- is.null(clicked_id())
    box_title <- clicked_title()

  output$clicked_box <- shiny::renderUI({
    bs4Dash::box(
      title = "Selected thesis",#paste(box_title),
      status = "lightblue",
      collapsed = empty_box,
      width = NULL,
      wordcloud2::wordcloud2Output("wordcloud"))
     })
  })

 # Word cloud of the selected thesis -------------------------------------------

  shiny::observe({

    clicked_words <- clicked_thesis() |>
      tidytext::unnest_tokens(word, text_clean) |>
      dplyr::mutate(word = textstem::lemmatize_words(word)) |>
      dplyr::anti_join(tidytext::stop_words, by = "word") |>
      dplyr::count(word) |>
      dplyr::arrange(-n)

   output$wordcloud <- wordcloud2::renderWordcloud2({
     # Make sure that the wordcloud for a thesis always looks the same
     set.seed(12345)

    # color vecotr for the wordcloud
     wc_cols <- c(rep(seq_pal10[5], 5),rep('#000000',500))

     # Create wordcloud
     wordcloud2::wordcloud2(clicked_words, size = 0.7, color = wc_cols )
     })

   })


# Value box with number of theses -----------------------------------------
  shiny::observe({
    n_theses <- nrow(emu_reactive())

    output$vbox_ntheses <- bs4Dash::renderbs4ValueBox({
      bs4Dash::bs4ValueBox(
        value = n_theses,
        subtitle = "Number of theses",
        icon = shiny::icon("book-open"),
        color = "info",
        width = 12
      )
    })

  })

# Value box with number of words -----------------------------------------
  shiny::observe({
    n_words <- prettyNum(nrow(emu_words()), big.mark = ",")

    output$vbox_nwords <- bs4Dash::renderbs4ValueBox({
      bs4Dash::bs4ValueBox(
        value = n_words,
        subtitle = "Number of words",
        icon = shiny::icon("comment-dots"),
        color = "teal",
        width = 12
      )
    })

  })
  shiny::observe({

    top_word <- emu_words() |>
      dplyr::count(word) |>
      dplyr::filter(n == max(n)) |>
      dplyr::pull(word)

    output$vbox_topword <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = top_word,
        subtitle = "Most used word",
        icon = shiny::icon("trophy"),
        color = "primary",
        width = 12
      )
    })
  })

  }
