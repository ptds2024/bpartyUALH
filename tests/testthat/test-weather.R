library(testthat)
library(bpartyUALH)
library(dotenv)

dotenv::load_dot_env(file = file.path(rprojroot::find_rstudio_root_file(), ".env"))
API_KEY <- Sys.getenv("API_KEY")

test_that("get_current_weather works for valid city", {
  weather <- bpartyUALH::get_current_weather("lausanne", API_KEY)
  expect_type(weather, "list")
  expect_true("temperature" %in% names(weather))
})

test_that("get_weather_forecast returns valid data", {
  forecast <- bpartyUALH::get_weather_forecast("lausanne", API_KEY)
  expect_s3_class(forecast, "data.frame")
  expect_true(nrow(forecast) > 0)
})

test_that("check_city_validity correctly identifies valid/invalid cities", {
  expect_true(bpartyUALH::check_city_validity("lausanne", API_KEY))
  expect_false(bpartyUALH::check_city_validity("invalid_city", API_KEY = "dummy_key"))
})
