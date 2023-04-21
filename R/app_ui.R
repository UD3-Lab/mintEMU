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
                             # Filters
                             shiny::selectInput("year",
                                                label = "Select graduation year:",
                                                choices = c('ALL'),
                                                multiple = F,
                                                selected = 'ALL'
                                                ),
                             shiny::selectInput("exchange",
                                                label = "Select exchange semester:",
                                                choices = c('ALL'),
                                                multiple = F,
                                                selected = 'ALL'
                                                ),
                             # Tool tips
                             shinyBS::bsTooltip("year",
                                                "Filter results by graduation year.",
                                                "right",
                                                options = list(container = "body")
                                                ),
                             shinyBS::bsTooltip("exchange",
                                                "Filter results by exchange semester",
                                                "right",
                                                options = list(container = "body")
                                                )
                             )
                           ),

                         body = bs4Dash::dashboardBody(
                           shinyjs::useShinyjs(),
                           bs4Dash::tabItems(
                             bs4Dash::tabItem(tabName = "intro",
                                              bs4Dash::box(
                                                shiny::plotOutput("thesismap")
                                                )
                                              ),
                             bs4Dash::tabItem(tabName = "data"),
                             bs4Dash::tabItem(tabName = "topic",
                                              shiny::fluidRow(
                                                bs4Dash::box(
                                                  shiny::sidebarPanel(
                                                    shiny::sliderInput("bins",
                                                                "Number of bins:",
                                                                min = 1,
                                                                max = 50,
                                                                value = 30
                                                                ))),
                                                bs4Dash::box(shiny::plotOutput("distPlot"))
                                                )
                                              )
                             )
                           )
  )
  }
