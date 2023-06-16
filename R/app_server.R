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
  data("emu_theses")


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

  emu_reactive <- shiny::reactive({
   emu_theses |>
     dplyr::filter(graduation_year  %in%
                     if('ALL' %in% input$year)
                       unique(emu_theses$graduation_year)
                      else
                       input$year
                   ) |>
     dplyr::mutate(id = seq_len(dplyr::n()))

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
      leaflet::addAwesomeMarkers(layerId = data$id,
                                 icon = icons,
                                 label = data$location,
                                 popup = paste0("<b>Title:</b> ", data$title,
                                                "<br/>",
                                                "<b>Location:</b> ", data$location))
    })

   # Box generated for a single title
   output$clicked_box <- shiny::renderUI({
     clicked_id <- input$thesis_location_map_marker_click$id

     clicked_thesis <- emu_reactive() |>
       dplyr::filter(id  == clicked_id)



     empty_box <- is.null(clicked_id)

     bs4Dash::box(title = paste(clicked_thesis$id, clicked_thesis$title),
                  status = "lightblue",
                  collapsed = empty_box,
                  width = NULL,
                  textOutput('clickid'),
                  wordcloud2::wordcloud2Output("wordcloud"))

     })  |>
     shiny::bindEvent(input$thesis_location_map_marker_click)



   # Word cloud of the selected thesis
   output$wordcloud <- wordcloud2::renderWordcloud2({

     clicked_id <- input$thesis_location_map_marker_click$id

     clicked_words <- emu_reactive() |>
       dplyr::filter(id  == clicked_id) |>
       tidytext::unnest_tokens(word, text_clean) |>
       dplyr::mutate(word = textstem::lemmatize_words(word))|>
       dplyr::anti_join(tidytext::stop_words, by = "word") |>
       dplyr::count(word)|>
       dplyr::arrange(-n)

     wordcloud2::wordcloud2(clicked_words)

     })|>
    shiny::bindEvent(input$thesis_location_map_marker_click)


   # Word cloud of the selected thesis
   output$clickid <- renderText({

     clicked_id <- input$thesis_location_map_marker_click$id

     clicked_id

     })|>
     shiny::bindEvent(input$thesis_location_map_marker_click)

  }
