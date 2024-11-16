# simulation_functions.R

#' Calculate lambda based on weather
#'
#' @title Calculate Lambda
#' @description This function calculates the lambda value based on temperature, humidity, and pressure.
#' @param temperature A numeric value representing the temperature.
#' @param humidity A numeric value representing the humidity percentage.
#' @param pressure A numeric value representing the pressure.
#' @return A numeric value representing the calculated lambda.
#' @examples
#' \dontrun{
#'   lambda <- calculate_lambda(25, 60, 1013)
#'   print(lambda)
#' }
#' @export
calculate_lambda <- function(temperature, humidity, pressure) {
  exp(0.5 + 0.5 * temperature - 3 * (humidity / 100) + 0.001 * pressure)
}

#' Simulate the number of guests based on lambda
#'
#' @title Simulate Guest Count
#' @description This function simulates the number of guests based on the lambda value.
#' @param lambda A numeric value representing the lambda parameter for the Poisson distribution.
#' @return An integer representing the simulated number of guests.
#' @examples
#' \dontrun{
#'   guest_count <- simulate_guest_count(5)
#'   print(guest_count)
#' }
#' @export
simulate_guest_count <- function(lambda) {
  stats::rpois(1, lambda)
}

#' Simulate cone consumption per guest
#'
#' @title Simulate Cones Per Guest
#' @description This function simulates the number of cones consumed per guest.
#' @param guest_count An integer representing the number of guests.
#' @return An integer representing the total number of cones consumed.
#' @examples
#' \dontrun{
#'   cones <- simulate_cones_per_guest(10)
#'   print(cones)
#' }
#' @export
simulate_cones_per_guest <- function(guest_count) {
  stats::rbinom(guest_count, size = 1, prob = 0.67) + 1
}
