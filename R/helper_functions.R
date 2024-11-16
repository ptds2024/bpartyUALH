#' Check City Validity
#'
#' This function checks if the city name provided is valid by making a request
#' to the OpenWeatherMap API. It returns `TRUE` if the city is valid, and `FALSE` otherwise.
#'
#' @param city_name A character string representing the name of the city.
#' @param API_KEY A character string representing the OpenWeatherMap API key.
#'
#' @return A logical value (`TRUE` if the city is valid, `FALSE` otherwise).
#' @export
check_city_validity <- function(city_name, API_KEY) {
  response <- httr::GET("http://api.openweathermap.org/data/2.5/weather",
                        query = list(q = city_name, appid = API_KEY))
  status <- httr::status_code(response)

  if (status == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Get Current Weather
#'
#' This function retrieves the current weather for a specified city from the OpenWeatherMap API.
#' The data is returned in a tidy format with various weather-related variables.
#'
#' @param city A character string representing the name of the city.
#' @param API_KEY A character string representing the OpenWeatherMap API key.
#' @return A tibble containing the current weather information for the specified city.
#' @export
get_current_weather <- function(city, API_KEY) {

  owmr::owmr_settings(API_KEY)

  current_weather <- owmr::get_current(city, units = "metric")

  weather_df <- dplyr::tibble(
    city = current_weather$name,
    country = current_weather$sys$country,
    temperature = current_weather$main$temp,
    feels_like = current_weather$main$feels_like,
    temp_min = current_weather$main$temp_min,
    temp_max = current_weather$main$temp_max,
    pressure = current_weather$main$pressure,
    humidity = current_weather$main$humidity,
    visibility = current_weather$visibility,
    wind_speed = current_weather$wind$speed,
    wind_deg = current_weather$wind$deg,
    clouds_all = current_weather$clouds$all,
    weather_main = current_weather$weather$main[1],
    weather_description = current_weather$weather$description[1],
    lon = current_weather$coord$lon,
    lat = current_weather$coord$lat,
    sunrise = as.POSIXct(current_weather$sys$sunrise, origin = "1970-01-01", tz = "UTC"),
    sunset = as.POSIXct(current_weather$sys$sunset, origin = "1970-01-01", tz = "UTC"),
    dt = as.POSIXct(current_weather$dt, origin = "1970-01-01", tz = "UTC")
  )

  return(weather_df)
}

#' Get Weather Forecast
#'
#' This function retrieves a 5-day forecast (every 3 hours) for a specified city from the OpenWeatherMap API.
#' The forecast includes temperature, humidity, and pressure data.
#'
#' @param city A character string representing the name of the city.
#'
#' @return A dataframe containing the 5-day weather forecast for the specified city.
#' @export
get_weather_forecast <- function(city, API_KEY) {

  owmr::owmr_settings(API_KEY)

  forecast_df <- data.frame(
    date = character(),
    temperature = numeric(),
    humidity = numeric(),
    pressure = numeric(),
    stringsAsFactors = FALSE
  )

  forecast_weather <- owmr::get_forecast(city, units = "metric")

  for (i in 1:nrow(forecast_weather$list)) {
    forecast_entry <- forecast_weather$list[i, ]

    # Extract relevant fields
    date_time <- lubridate::as_datetime(forecast_entry$dt)
    temp <- forecast_entry$main.temp
    humidity <- forecast_entry$main.humidity
    pressure <- forecast_entry$main.pressure

    # Append to dataframe
    forecast_df <- rbind(forecast_df, data.frame(
      date = date_time,
      temperature = temp,
      humidity = humidity,
      pressure = pressure
    ))
  }

  forecast_df <- dplyr::filter(forecast_df, date <= Sys.Date() + lubridate::days(5))

  class(forecast_df) <- c("forecast", class(forecast_df))

  return(forecast_df)
}

#' Plot Forecast Data
#'
#' This function generates a plot for the 5-day weather forecast of a chosen variable (temperature, humidity, or pressure).
#' The plot includes a line, points, and an area shaded under the curve.
#'
#' @param forecast_data A dataframe or tibble of class `forecast` containing the forecasted weather data.
#' @param variable A character string indicating the variable to be plotted ("temperature", "humidity", or "pressure").
#'
#' @return A `ggplot` object displaying the weather forecast plot.
#' @export
plot.forecast <- function(forecast_data, variable) {
  if (!inherits(forecast_data, "forecast")) {
    stop("The data provided is not of class 'forecast'")
  }

  if (!variable %in% c("temperature", "humidity", "pressure")) {
    stop("Invalid variable. Choose 'temperature', 'humidity', or 'pressure'.")
  }

  forecast_filtered <- dplyr::select(forecast_data, date, value = dplyr::all_of(variable))


  ggplot2::ggplot(forecast_filtered, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "#00BFC4", size = 1.2) +
    ggplot2::geom_point(color = "#F8766D", size = 3) +
    ggplot2::geom_area(fill = "lightblue", alpha = 0.1) +
    ggplot2::labs(
      title = paste("5-Day Forecast -", variable),
      x = "Date",
      y = variable
    ) +
    ggplot2::scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#343A40", size = 16, hjust = 0.5),
      axis.title.x = ggplot2::element_text(color = "#343A40", size = 12, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(color = "#343A40", size = 12, margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(color = "#343A40"),
      panel.grid.major = ggplot2::element_line(color = "#E5E5E5"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#F8F9FA")
    )
}
