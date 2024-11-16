# party_simulation_functions.R

#' Calculate party totals
#'
#' @title Calculate Party Totals
#' @description This function calculates the total and average volume and surface area of cones consumed at a party based on the number of guests and cones per guest.
#' @param guest_count An integer representing the number of guests.
#' @param cones_per_guest A numeric vector representing the number of cones consumed per guest.
#' @param sd A numeric value representing the standard deviation for random variations. Default is 0.1.
#' @return A list containing the total volume, total surface area, average volume, average surface area, guest count, and total cones.
#' @examples
#' \dontrun{
#'   guest_count <- 10
#'   cones_per_guest <- simulate_cones_per_guest(guest_count)
#'   totals <- calculate_party_totals(guest_count, cones_per_guest)
#'   print(totals)
#' }
#' @export
calculate_party_totals <- function(guest_count, cones_per_guest, sd = 0.1) {
  total_cones <- sum(cones_per_guest)
  x_values <- seq(0, 10, length.out = 100)
  random_variations <- generate_random_variations(x_values, sd)
  metrics <- calculate_metrics(random_variations, x_values)
  
  total_volume <- metrics$volume * total_cones
  total_surface_area <- metrics$surface_area * total_cones
  avg_volume <- metrics$volume
  avg_surface_area <- metrics$surface_area
  
  return(list(total_volume = total_volume, 
              total_surface_area = total_surface_area, 
              avg_volume = avg_volume, 
              avg_surface_area = avg_surface_area,
              guest_count = guest_count,
              total_cones = total_cones))
}

#' Simulate parties
#'
#' @title Simulate Parties
#' @description This function simulates multiple parties based on weather forecast data and calculates the total and average metrics for each party.
#' @param num_parties An integer representing the number of parties to simulate.
#' @param forecast_data A data frame containing weather forecast data with columns for temperature, humidity, and pressure.
#' @param sd A numeric value representing the standard deviation for random variations. Default is 0.1.
#' @return A list containing vectors of total volumes, total surface areas, average volumes, average surface areas, guest counts, and cone counts for each party.
#' @examples
#' \dontrun{
#'   forecast_data <- data.frame(
#'     temperature = c(25, 30, 28),
#'     humidity = c(60, 55, 65),
#'     pressure = c(1013, 1012, 1011)
#'   )
#'   results <- simulate_parties(5, forecast_data)
#'   print(results)
#' }
#' @export
simulate_parties <- function(num_parties, forecast_data, sd = 0.1) {
  total_volumes <- numeric(num_parties)
  total_surface_areas <- numeric(num_parties)
  avg_volumes <- numeric(num_parties)
  avg_surface_areas <- numeric(num_parties)
  guest_counts <- numeric(num_parties)
  cone_counts <- numeric(num_parties)
  
  for (i in seq_len(num_parties)) {
    weather <- forecast_data[sample(nrow(forecast_data), 1), ]
    lambda <- calculate_lambda(weather$temperature, weather$humidity, weather$pressure)
    guest_count <- simulate_guest_count(lambda)
    cones_per_guest <- simulate_cones_per_guest(guest_count)
    results <- calculate_party_totals(guest_count, cones_per_guest, sd)
    
    total_volumes[i] <- results$total_volume
    total_surface_areas[i] <- results$total_surface_area
    avg_volumes[i] <- results$avg_volume
    avg_surface_areas[i] <- results$avg_surface_area
    guest_counts[i] <- results$guest_count
    cone_counts[i] <- results$total_cones
  }
  
  return(list(total_volumes = total_volumes,
              total_surface_areas = total_surface_areas,
              avg_volumes = avg_volumes,
              avg_surface_areas = avg_surface_areas,
              guest_counts = guest_counts,
              cone_counts = cone_counts))
}
