#' @export
app_server <- function(session,input, output) {

  ## Use the thematic package to adjust plot styles to app theme:
  thematic::thematic_shiny()
  bs4Dash::useAutoColor()

  ## Get the data  - temporarily added data file in the project. this will mimic the packages final behaviour

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

  # Tool tips
  bs4Dash::addTooltip(id = "year",
                      options = list(
                        title = "Filter results by graduation year",
                        placement = "right")
                      )

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

  output$distPlot <- shiny::renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = 30)

    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })


  output$thesis_location <- leaflet::renderLeaflet({
    data <- emu_reactive()

    icons <- leaflet::awesomeIcons(
      icon = "pen",
      iconColor = "#fff",
      library = "fa",
      markerColor = "blue"
    )

    emu_reactive() |>
      leaflet::leaflet() |>
      leaflet::addProviderTiles("Stamen.TonerLite") |>
      leaflet::addAwesomeMarkers(icon = icons,
                                 label = data$location,
                                 popup = paste0("<b>Title:</b> ",
                                                data$title,
                                                "<br/> <b>Location:</b> ",
                                                data$location )
                                 )


  })

  }
