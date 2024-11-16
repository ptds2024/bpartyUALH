#' @title Shiny Application Wrapper
#' @description Runs the Shiny application for weather simulation and ice cream sales analysis.
#' @param API_KEY A string containing the API key for the weather service.
#' @export
runShinyApp <- function(API_KEY) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Weather and Ice Cream Simulation"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Location 1", inputModuleUI("location1")),
          shiny::tabPanel("Location 2", inputModuleUI("location2")),
          shiny::tabPanel("Location 3", inputModuleUI("location3"))
        )
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Location 1", outputModuleUI("location1")),
          shiny::tabPanel("Location 2", outputModuleUI("location2")),
          shiny::tabPanel("Location 3", outputModuleUI("location3"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Location 1
    location1_inputs <- shiny::callModule(inputModule, "location1", API_KEY)
    shiny::callModule(outputModule, "location1",
                      location1_inputs$is_valid_city,
                      location1_inputs$selected_city,
                      location1_inputs$parameter,
                      location1_inputs$simulations,
                      location1_inputs$run_simulation,
                      API_KEY)

    # Location 2
    location2_inputs <- shiny::callModule(inputModule, "location2", API_KEY)
    shiny::callModule(outputModule, "location2",
                      location2_inputs$is_valid_city,
                      location2_inputs$selected_city,
                      location2_inputs$parameter,
                      location2_inputs$simulations,
                      location2_inputs$run_simulation,
                      API_KEY)

    # Location 3
    location3_inputs <- shiny::callModule(inputModule, "location3", API_KEY)
    shiny::callModule(outputModule, "location3",
                      location3_inputs$is_valid_city,
                      location3_inputs$selected_city,
                      location3_inputs$parameter,
                      location3_inputs$simulations,
                      location3_inputs$run_simulation,
                      API_KEY)
  }

  shiny::shinyApp(ui = ui, server = server)
}
