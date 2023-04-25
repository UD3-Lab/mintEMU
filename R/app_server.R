#' @export
app_server <- function(session,input, output) {

  ## Get the data  - temporarily added data file in the project. this will mimic the packages final behaviour

  # data_path <- here::here("analysis", "data", "derived_data", "emu_theses_with_text.csv")
  # emu_theses <- readr::read_csv(data_path)
  # usethis::use_data(emu_theses, overwrite = TRUE)
  data("emu_theses")


  # Update filters

  shiny::updateSelectInput(session,"year", label = "Select year:",
                    choices = c('ALL', emu_theses$graduation_year |>
                                          as.character() |>
                                          unique()|>
                                          sort()
                                ),
                    selected='ALL')

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
     dplyr::filter(graduation_year  %in%
                     if('ALL' %in% input$year)
                       unique(emu_theses$graduation_year)
                     else
                       input$year
                   )

 })

 # Render plots

  output$distPlot <- shiny::renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

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


  # output$thesismap <- shiny::renderPlot({
  #
  #   x <- geocode_thesis_locations(emu_reactive())
  #
  #   visualize_thesis_locations(x)
  #
  # })

  }
