library(dotenv)
library(testthat)

dotenv::load_dot_env(file = file.path(rprojroot::find_rstudio_root_file(), ".env"))
API_KEY <- Sys.getenv("API_KEY")

test_that("simulate_parties generates valid outputs", {
  forecast_data <- bpartyUALH::get_weather_forecast("lausanne", API_KEY)
  results <- bpartyUALH::simulate_parties(100, forecast_data)
  expect_true("guest_counts" %in% names(results))
  expect_true(length(results$guest_counts) == 100)
})


test_that("calculate_lambda computes correct values", {
  lambda <- calculate_lambda(temperature = 25, humidity = 60, pressure = 1013)
  expect_type(lambda, "double")
  expect_gt(lambda, 0)
})

test_that("simulate_guest_count generates valid guest count", {
  lambda <- 5
  guest_count <- simulate_guest_count(lambda)
  expect_type(guest_count, "integer")
  expect_gte(guest_count, 0)
})

test_that("simulate_cones_per_guest generates valid cone counts", {
  guest_count <- 10
  cones <- simulate_cones_per_guest(guest_count)

  expect_type(cones, "double")
  expect_length(cones, guest_count)
  expect_true(all(cones %in% c(1, 2)))  # Ensure all cone counts are either 1 or 2
})


test_that("generate_random_variations produces random values", {
  x_values <- seq(0, 10, length.out = 100)
  variations <- generate_random_variations(x_values, sd = 0.1)
  expect_type(variations, "double")
  expect_length(variations, length(x_values))
  expect_true(any(variations != 0))
})

test_that("calculate_metrics computes volume and surface area", {
  x_values <- seq(0, 10, length.out = 100)
  random_variations <- generate_random_variations(x_values, sd = 0.1)
  metrics <- calculate_metrics(random_variations, x_values)

  expect_type(metrics, "list")
  expect_named(metrics, c("volume", "surface_area"))
  expect_gt(metrics$volume, 0)
  expect_gt(metrics$surface_area, 0)
})

test_that("calculate_party_totals returns valid results", {
  guest_count <- 10
  cones_per_guest <- simulate_cones_per_guest(guest_count)
  results <- calculate_party_totals(guest_count, cones_per_guest)

  expect_type(results, "list")
  expect_named(results, c("total_volume", "total_surface_area", "avg_volume", "avg_surface_area", "guest_count", "total_cones"))
  expect_gte(results$total_volume, 0)
  expect_gte(results$total_surface_area, 0)
  expect_gte(results$avg_volume, 0)
  expect_gte(results$avg_surface_area, 0)
  expect_equal(results$guest_count, guest_count)
  expect_equal(results$total_cones, sum(cones_per_guest))
})

test_that("simulate_parties generates results for multiple parties", {
  forecast_data <- data.frame(
    temperature = c(25, 30, 28),
    humidity = c(60, 55, 65),
    pressure = c(1013, 1012, 1011)
  )
  num_parties <- 5
  results <- simulate_parties(num_parties, forecast_data, sd = 0.1)

  expect_type(results, "list")
  expect_named(results, c("total_volumes", "total_surface_areas", "avg_volumes", "avg_surface_areas", "guest_counts", "cone_counts"))

  expect_length(results$total_volumes, num_parties)
  expect_length(results$total_surface_areas, num_parties)
  expect_length(results$avg_volumes, num_parties)
  expect_length(results$avg_surface_areas, num_parties)
  expect_length(results$guest_counts, num_parties)
  expect_length(results$cone_counts, num_parties)

  expect_true(all(results$total_volumes >= 0))
  expect_true(all(results$total_surface_areas >= 0))
  expect_true(all(results$avg_volumes >= 0))
  expect_true(all(results$avg_surface_areas >= 0))
  expect_true(all(results$guest_counts >= 0))
  expect_true(all(results$cone_counts >= 0))
})
