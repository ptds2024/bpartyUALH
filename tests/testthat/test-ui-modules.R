library(dotenv)
library(testthat)
library(shinytest2)


load_dot_env(file = ".env")
API_KEY <- Sys.getenv("API_KEY")

# Test for inputModuleUI()
test_that("inputModuleUI creates the correct UI elements", {
  ui <- inputModuleUI("test_input")
  expect_true(is(ui, "shiny.tag.list"))
  expect_length(ui, 5)

  # Check if specific elements exist
  expect_true(any(grepl("City", as.character(ui))))
  expect_true(any(grepl("Parameter", as.character(ui))))
  expect_true(any(grepl("Submit", as.character(ui))))
  expect_true(any(grepl("Number of Simulations", as.character(ui))))
  expect_true(any(grepl("Run Simulation", as.character(ui))))
})
