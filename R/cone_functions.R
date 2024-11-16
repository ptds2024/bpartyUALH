# cone_functions.R

library(pracma)

# Generate random variations for cone radius
generate_random_variations <- function(x_values, sd = 0.1) {
  rnorm(length(x_values), mean = 0, sd = sd)
}

# Cone radius calculation with variations
cone_radius_with_variation <- function(x, random_variations, x_values) {
  base_radius <- ifelse(x < 0, 0,
                        ifelse(x < 8, x / 8,
                               ifelse(x < 8 + pi / 2, 1 + 1.5 * sin(x - 8),
                                      ifelse(x < 10, 2.5 - 2 * cos(x - 8), 0))))
  base_radius + approx(x_values, random_variations, xout = x, rule = 2)$y
}

# Calculate volume and surface area
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
  volume <- trapz(x_values, integrand_volume)
  surface_area <- trapz(x_values, integrand_surface)
  return(list(volume = volume, surface_area = surface_area))
}
