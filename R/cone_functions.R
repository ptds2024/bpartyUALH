# cone_functions.R

#' Generate random variations for cone radius
#'
#' @title Generate Random Variations
#' @description This function generates random variations for cone radius based on the provided x-values.
#' @param x_values A numeric vector of x-values.
#' @param sd A numeric value representing the standard deviation for the random variations. Default is 0.1.
#' @return A numeric vector of random variations.
#' @examples
#' \dontrun{
#'   variations <- generate_random_variations(1:10)
#'   print(variations)
#' }
#' @export
generate_random_variations <- function(x_values, sd = 0.1) {
  stats::rnorm(length(x_values), mean = 0, sd = sd)
}

#' Cone radius calculation with variations
#'
#' @title Cone Radius with Variation
#' @description This function calculates the cone radius with variations based on the provided x-values and random variations.
#' @param x A numeric value representing the x-coordinate.
#' @param random_variations A numeric vector of random variations.
#' @param x_values A numeric vector of x-values.
#' @return A numeric value representing the cone radius with variations.
#' @examples
#' \dontrun{
#'   radius <- cone_radius_with_variation(5, generate_random_variations(1:10), 1:10)
#'   print(radius)
#' }
#' @export
cone_radius_with_variation <- function(x, random_variations, x_values) {
  base_radius <- ifelse(x < 0, 0,
                        ifelse(x < 8, x / 8,
                               ifelse(x < 8 + pi / 2, 1 + 1.5 * sin(x - 8),
                                      ifelse(x < 10, 2.5 - 2 * cos(x - 8), 0))))
  base_radius + stats::approx(x_values, random_variations, xout = x, rule = 2)$y
}

#' Calculate volume and surface area
#'
#' @title Calculate Metrics
#' @description This function calculates the volume and surface area of a cone based on random variations and x-values.
#' @param random_variations A numeric vector of random variations.
#' @param x_values A numeric vector of x-values.
#' @return A list containing the volume and surface area of the cone.
#' @examples
#' \dontrun{
#'   metrics <- calculate_metrics(generate_random_variations(1:10), 1:10)
#'   print(metrics)
#' }
#' @export
calculate_metrics <- function(random_variations, x_values) {
  radii <- cone_radius_with_variation(x_values, random_variations, x_values)
  integrand_volume <- pi * radii^2
  epsilon <- 1e-5
  h_prime <- numeric(length(x_values))
  for (i in seq_along(x_values)) {
    h_prime[i] <- (cone_radius_with_variation(x_values[i] + epsilon, random_variations, x_values) -
                     cone_radius_with_variation(x_values[i], random_variations, x_values)) / epsilon
  }
  integrand_surface <- 2 * pi * radii * sqrt(1 + h_prime^2)
  volume <- pracma::trapz(x_values, integrand_volume)
  surface_area <- pracma::trapz(x_values, integrand_surface)
  return(list(volume = volume, surface_area = surface_area))
}
