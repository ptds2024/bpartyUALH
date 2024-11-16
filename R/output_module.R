#' @title Output Module UI and Server
#' @description This module displays the current weather, forecast data, and simulation results.
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param is_valid_city Reactive value indicating if the city is valid.
#' @param selected_city Reactive value containing the selected city name.
#' @param parameter Reactive value for the weather parameter.
#' @param simulations Reactive value for the number of simulations.
#' @param run_simulation Reactive event for triggering the simulation.
#' @param API_KEY A string containing the API key for the weather service.
#' @return No direct return; renders UI components dynamically.
#' @export
outputModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("current_weather"))),
    plotOutput(ns("forecast_plot")),
    h3("Simulation Results"),
    plotOutput(ns("volume_histogram")),
    plotOutput(ns("surface_area_histogram")),
    textOutput(ns("volume_summary")),
    textOutput(ns("surface_area_summary")),
    h3("Guest Attendance"),
    plotOutput(ns("guest_attendance_histogram")),
    textOutput(ns("guest_attendance_summary"))
  )
}

#' @export
outputModule <- function(input, output, session, is_valid_city, selected_city, parameter, simulations, run_simulation, API_KEY) {
  current_weather_data <- reactive({
    req(is_valid_city(), selected_city())
    get_current_weather(selected_city(), API_KEY)
  })

  forecast_weather_data <- reactive({
    req(is_valid_city(), selected_city())
    get_weather_forecast(selected_city(), API_KEY)
  })

  simulation_results <- eventReactive(run_simulation(), {
    req(is_valid_city())
    num_simulations <- simulations()
    forecast_data <- forecast_weather_data()
    simulate_parties(num_simulations, forecast_data)
  })

  output$current_weather <- renderText({
    req(is_valid_city(), parameter())
    current_data <- current_weather_data()
    paste("Current", parameter(), "in", current_data$city, "is", round(current_data[[parameter()]], 1))
  })

  output$forecast_plot <- renderPlot({
    req(is_valid_city(), parameter())
    forecast_data <- forecast_weather_data()
    plot.forecast(forecast_data, parameter())
  })

  # Render histograms and summaries
  output$volume_histogram <- renderPlot({ ... })
  output$surface_area_histogram <- renderPlot({ ... })
  output$volume_summary <- renderText({ ... })
  output$surface_area_summary <- renderText({ ... })
  output$guest_attendance_histogram <- renderPlot({ ... })
  output$guest_attendance_summary <- renderText({ ... })
}
