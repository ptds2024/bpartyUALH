% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_module.R
\name{inputModule}
\alias{inputModule}
\title{Input Module Server}
\usage{
inputModule(input, output, session, API_KEY)
}
\arguments{
\item{input}{The input object created by `shiny::fluidPage()`.}

\item{output}{The output object created by `shiny::fluidPage()`.}

\item{session}{The session object created by `shiny::fluidPage()`.}

\item{API_KEY}{A string containing the API key for the weather service.}
}
\value{
Reactive values including `is_valid_city`, `selected_city`, `parameter`, `simulations`, and `run_simulation`.
}
\description{
This module provides the server logic for selecting city, weather parameters, and running simulations.
}
\examples{
# Example usage:
# In UI:
inputModuleUI("location1")
# In Server:
inputModule(input, output, session, API_KEY)
}
