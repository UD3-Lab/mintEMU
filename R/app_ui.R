#' @export
app_ui <- function() {
  # Define UI for application
  bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "mintEMU",
      color = "primary",
      href = "https://urbandesigntudelft.nl/research/rbanism/",
      image = "TUDelft_logo_rgb.png"
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(
                           bs4Dash::sidebarMenu(
                             id = 'tab',
                             bs4Dash::menuItem("Intro",
                                               tabName = "intro",
                                               icon = shiny::icon('globe')
                                               ),
                             bs4Dash::menuItem("Data overview",
                                               tabName = "data",
                                               icon = shiny::icon('magnifying-glass-chart')
                                               ),
                             bs4Dash::menuItem("Topic models",
                                               tabName = "topic",
                                               icon = shiny::icon('diagram-project')
                                               ),

                             shiny::sliderInput("year",
                                                label = "Select graduation year:",
                                                min = 2007,
                                                max = 2021,
                                                value = c(2009, 2011),
                                                dragRange = T,
                                                sep = ""
                                                ),
                             shiny::selectInput("exchange",
                                                label = "Select exchange semester:",
                                                choices = c('ALL'),
                                                multiple = F,
                                                selected = 'ALL'
                                                ))
                           ),

                         body = bs4Dash::dashboardBody(
                           shinyjs::useShinyjs(),
                           bs4Dash::tabItems(
                             bs4Dash::tabItem(tabName = "intro",
                                              shiny::fluidRow(
                                                bs4Dash::column(12,
                                                                bs4Dash::box(title = "Links",
                                                                status = "lightblue",
                                                                width = NULL,
                                                                plotOutput("distPlot")
                                                                )),
                                                bs4Dash::column(3,
                                                       bs4Dash::box(title = "Stats 1",
                                                                    status = "lightblue",
                                                                    width=NULL)),
                                                bs4Dash::column(3,
                                                       bs4Dash::box(title = "Stats 2",
                                                                    status = "lightblue",
                                                                    width=NULL)),
                                                bs4Dash::column(3,
                                                       bs4Dash::box(title = "Stats 3",
                                                                    status = "lightblue",
                                                                    width=NULL)),
                                                bs4Dash::column(3,
                                                       bs4Dash::box(title = "Stats 4",
                                                                    status = "lightblue",
                                                                    width=NULL)),
                                                bs4Dash::column(6,
                                                                bs4Dash::box(title = "Map plot",
                                                                             width = NULL,
                                                                             leaflet::leafletOutput("thesis_location")
                                                                             )),
                                                bs4Dash::column(6,
                                                                bs4Dash::box(title = "Paper carousel",
                                                                             width = NULL)),
                                                bs4Dash::column(12,
                                                                bs4Dash::box(title = "FAIR",
                                                                             status = "teal",
                                                                             width = NULL)),
                                                )
                                              ),
                             bs4Dash::tabItem(tabName = "data",
                                              shiny::fluidRow(
                                                bs4Dash::column(6,
                                                                bs4Dash::box(title = "Top N words",
                                                                             status = "teal",
                                                                             width = NULL)),
                                                bs4Dash::column(6,
                                                                bs4Dash::box(title = "Evolution of top words",
                                                                             status = "teal",
                                                                             width = NULL)),
                                                bs4Dash::column(12,
                                                                bs4Dash::box(title = "Top words per paper",
                                                                             status = "teal",
                                                                             width = NULL))
                                                )
                                              ),
                             bs4Dash::tabItem(tabName = "topic",
                                              shiny::fluidRow(
                                                bs4Dash::column(12,
                                                                bs4Dash::box(title = "Models",
                                                                             status = "teal",
                                                                             width = NULL)),
                                                bs4Dash::column(12,
                                                                bs4Dash::box(title = "Model evolution",
                                                                             status = "teal",
                                                                             width = NULL)),
                                              )
                                              )
                             )
                           )
    )
  }
