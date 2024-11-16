#' @title Output Module UI
#' @description This function creates the UI components for the output module.
#' @param id A unique identifier for the module.
#' @return A tagList containing the UI components.
#' @export
outputModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::textOutput(ns("current_weather"))),
    shiny::p("This section shows the 5-day forecast for the selected city. The plot displays the chosen parameter (temperature, humidity, or pressure) over time."),
    shiny::plotOutput(ns("forecast_plot")),
    shiny::h3("Simulation Results"),
    shiny::p("This section shows the results of the ice cream simulation based on the weather forecast. The histograms represent the distribution of total ice cream volumes and surface areas across simulations, and the summaries provide 99% confidence intervals for these metrics."),
    shiny::plotOutput(ns("volume_histogram")),
    shiny::plotOutput(ns("surface_area_histogram")),
    shiny::textOutput(ns("volume_summary")),
    shiny::textOutput(ns("surface_area_summary")),
    shiny::h3("Guest Attendance"),
    shiny::p("This section shows the total number of guests attending the party, based on the simulation. The histogram represents the distribution of the number of guests across simulations, and the summary provides a 99% confidence interval for guest attendance."),
    shiny::plotOutput(ns("guest_attendance_histogram")),
    shiny::textOutput(ns("guest_attendance_summary"))
  )
}


#' @title Output Module
#' @description This module displays the current weather, forecast data, and simulation results.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param is_valid_city Reactive value indicating if the city is valid.
#' @param selected_city Reactive value containing the selected city name.
#' @param parameter Reactive value for the weather parameter.
#' @param simulations Reactive value for the number of simulations.
#' @param run_simulation Reactive event for triggering the simulation.
#' @param API_KEY A string containing the API key for the weather service.
#' @return No direct return; renders UI components dynamically.
#' @export
outputModule <- function(input, output, session, is_valid_city, selected_city, parameter, simulations, run_simulation, API_KEY) {
  current_weather_data <- shiny::reactive({
    shiny::req(is_valid_city(), selected_city())
    get_current_weather(selected_city(), API_KEY = API_KEY)
  })

  forecast_weather_data <- shiny::reactive({
    shiny::req(is_valid_city(), selected_city())
    get_weather_forecast(selected_city(), API_KEY)
  })

  simulation_results <- shiny::eventReactive(run_simulation(), {
    shiny::req(is_valid_city())
    num_simulations <- simulations()
    forecast_data <- forecast_weather_data()
    simulate_parties(num_simulations, forecast_data)
  })

  output$current_weather <- shiny::renderText({
    shiny::req(is_valid_city(), parameter())
    current_data <- current_weather_data()
    paste("Current", parameter(), "in", current_data$city, "is", round(current_data[[parameter()]], 1))
  })

  output$forecast_plot <- shiny::renderPlot({
    shiny::req(is_valid_city(), parameter())
    forecast_data <- forecast_weather_data()
    plot.forecast(forecast_data, parameter())
  })

  # Render histogram for volume
  output$volume_histogram <- shiny::renderPlot({
    results <- simulation_results()
    total_volumes <- results$total_volumes[!is.na(results$total_volumes)]
    graphics::hist(total_volumes, main = "Histogram of Total Ice Cream Volumes", xlab = "Volume", col = "skyblue")
  })

  # Render histogram for surface area
  output$surface_area_histogram <- shiny::renderPlot({
    results <- simulation_results()
    total_surface_areas <- results$total_surface_areas[!is.na(results$total_surface_areas)]
    graphics::hist(total_surface_areas, main = "Histogram of Ice Cream Surface Areas", xlab = "Surface Area", col = "lightgreen")
  })

  # Render text summary for volume
  output$volume_summary <- shiny::renderText({
    results <- simulation_results()
    total_volumes <- results$total_volumes[!is.na(results$total_volumes)]
    ci <- stats::quantile(total_volumes, probs = c(0.005, 0.995))
    paste("99% CI for Volume: [", round(ci[1], 2), ", ", round(ci[2], 2), "]")
  })

  # Render text summary for surface area
  output$surface_area_summary <- shiny::renderText({
    results <- simulation_results()
    total_surface_areas <- results$total_surface_areas[!is.na(results$total_surface_areas)]
    ci <- stats::quantile(total_surface_areas, probs = c(0.005, 0.995))
    paste("99% CI for Surface Area: [", round(ci[1], 2), ", ", round(ci[2], 2), "]")
  })

  # Guest attendance
  output$guest_attendance_histogram <- shiny::renderPlot({
    results <- simulation_results()
    guest_counts <- results$guest_counts[!is.na(results$guest_counts)]
    graphics::hist(guest_counts, main = "Histogram of Guest Attendance", xlab = "Number of Guests", col = "lightblue")
  })

  output$guest_attendance_summary <- shiny::renderText({
    results <- simulation_results()
    guest_counts <- results$guest_counts[!is.na(results$guest_counts)]
    ci <- stats::quantile(guest_counts, probs = c(0.005, 0.995))
    paste("99% CI for Guest Attendance: [", round(ci[1], 2), ", ", round(ci[2], 2), "]")
  })
}
