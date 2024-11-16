# weather_functions.R

library(owmr)
library(dplyr)
library(lubridate)

# Retrieve and store 5-day, 3-hour forecast data
get_weather_forecast <- function(city) {
  forecast_df <- data.frame(
    date = character(),
    temperature = numeric(),
    humidity = numeric(),
    pressure = numeric(),
    stringsAsFactors = FALSE
  )
  
  forecast_weather <- get_forecast(city, units = "metric")
  for (i in 1:nrow(forecast_weather$list)) {
    forecast_entry <- forecast_weather$list[i, ]
    date_time <- as_datetime(forecast_entry$dt)
    temp <- forecast_entry$main.temp
    humidity <- forecast_entry$main.humidity
    pressure <- forecast_entry$main.pressure
    
    forecast_df <- rbind(forecast_df, data.frame(
      date = date_time,
      temperature = temp,
      humidity = humidity,
      pressure = pressure
    ))
  }
  
  forecast_df <- forecast_df %>%
    filter(date <= Sys.Date() + days(5))
  
  class(forecast_df) <- c("forecast", class(forecast_df))
  
  return(forecast_df)
}
