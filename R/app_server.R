#' @export
app_server <- function(session,input, output) {

  ## Use the thematic package to adjust plot styles to app theme ---------------
  thematic::thematic_shiny()
  bs4Dash::useAutoColor()

  ## Get the data ---------------------- ---------------------------------------

  ## temporarily added data file in the project. this will mimic the packages final behaviour

  # data_path <- here::here("analysis", "data", "derived_data", "emu_theses_with_text.csv")
  # Add latitude and longitude information to theses data frame
  # emu_theses <- readr::read_csv(data_path)
  # emu_theses$text_clean <- emu_theses$text |> clean_basic()
  # emu_theses <- geocode_thesis_locations(emu_theses)
  # usethis::use_data(emu_theses, overwrite = TRUE)
  data("emu_clean")
  data("emu_metadata")
  emu_theses <- dplyr::left_join(emu_clean, emu_metadata, by = "ID")


  # Update filters -------------------------------------------------------------
  shiny::updateSelectInput(session,"year", label = "Select year:",
                           choices = c('ALL', emu_theses$graduation_year |>
                                          as.character() |>
                                          unique()|>
                                          sort() ),
                           selected='ALL')

  # shiny::updateSelectInput(session, "exchange", label = "Select exchange semester:",
  #                          choices = c('ALL',emu_theses$exchange_semester |>
  #                                 as.character() |>
  #                                 unique()|>
  #                                 sort()  ),
  #                          selected='ALL')

  # Tool tips ------------------------------------------------------------------
  bs4Dash::addTooltip(id = "year",
                      options = list(title = "Filter results by graduation year",
                                     placement = "right"))

  bs4Dash::addTooltip(id = "exchange",
                      options = list(title = "Filter results by exchange semester",
                                     placement = "right"))


  # Create reactive data object ------------------------------------------------

  # dataset responding to filters
  emu_reactive <- shiny::reactive({
   emu_theses |>
     dplyr::filter(graduation_year  %in%
                     if('ALL' %in% input$year)
                       unique(emu_theses$graduation_year)
                      else
                       input$year
                   )

   })

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
    data <- emu_reactive()

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
                                                "<b>Location:</b> ", data$location,
                                                "<br/>",
                                                "<b>ID:</b> ", data$ID))
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
                  #,wordcloud2::wordcloud2Output("wordcloud")
                  )
     })

   # Graph with top 50 words
   output$top_words <- renderPlot({

     clicked_words <- clicked_thesis() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word))|>
       dplyr::anti_join(tidytext::stop_words, by = "word")

     top_words <- get_top_words_per_corpus(clicked_words , 20, 'word' )

     ggplot2::ggplot(top_words)+
       ggplot2::aes(x = word, y = n)+
       ggplot2::geom_col()

   })

   # Word cloud of the selected thesis
   output$wordcloud <- wordcloud2::renderWordcloud2({


     clicked_words <- clicked_thesis() |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word))|>
       dplyr::anti_join(tidytext::stop_words, by = "word") |>
       dplyr::count(word)|>
       dplyr::arrange(-n)

     wordcloud2::wordcloud2(clicked_words)

     })


   # Word cloud of the selected thesis
   output$clickid <- renderText({

     clicked_id <- input$thesis_location_map_marker_click$id

     clicked_id

     })|>
     shiny::bindEvent(input$thesis_location_map_marker_click)

  }
