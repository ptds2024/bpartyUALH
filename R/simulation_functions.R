# simulation_functions.R

# Calculate lambda based on weather
calculate_lambda <- function(temperature, humidity, pressure) {
  exp(0.5 + 0.5 * temperature - 3 * (humidity / 100) + 0.001 * pressure)
}

# Simulate the number of guests based on lambda
simulate_guest_count <- function(lambda) {
  rpois(1, lambda)
}

# Simulate cone consumption per guest
simulate_cones_per_guest <- function(guest_count) {
  rbinom(guest_count, size = 1, prob = 0.67) + 1
}
