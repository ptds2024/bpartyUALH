#' @title Input Module UI and Server
#' @description This module provides the input interface and server logic for selecting city, weather parameters, and running simulations.
#' @param id A unique identifier for the module.
#' @param API_KEY A string containing the API key for the weather service.
#' @return Reactive values including `is_valid_city`, `selected_city`, `parameter`, `simulations`, and `run_simulation`.
#' @examples
#' # Example usage:
#' # In UI:
#' inputModuleUI("location1")
#' # In Server:
#' inputModule(input, output, session, API_KEY)
#' @export
inputModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("city"), "City", value = "Lausanne"),
    selectInput(
      ns("parameter"),
      "Parameter",
      choices = c("Temperature" = "temperature", "Humidity" = "humidity", "Pressure" = "pressure")
    ),
    actionButton(ns("submit"), "Submit"),
    numericInput(ns("simulations"), "Number of Simulations", value = 10000, min = 1),
    actionButton(ns("run_simulation"), "Run Simulation", icon = icon("play"))
  )
}

#' @export
inputModule <- function(input, output, session, API_KEY) {
  is_valid_city <- reactiveVal(FALSE)
  selected_city <- reactiveVal(NULL)

  observeEvent(input$submit, {
    req(input$city)
    valid_city <- check_city_validity(input$city, API_KEY)

    if (valid_city) {
      is_valid_city(TRUE)
      selected_city(input$city)
    } else {
      is_valid_city(FALSE)
      showModal(modalDialog(
        title = "Invalid City",
        "The city name entered is not valid. Please try again.",
        easyClose = TRUE
      ))
    }
  })

  list(
    is_valid_city = is_valid_city,
    selected_city = selected_city,
    parameter = reactive(input$parameter),
    simulations = reactive(input$simulations),
    run_simulation = reactive(input$run_simulation)
  )
}
