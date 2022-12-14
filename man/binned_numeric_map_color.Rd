% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_color_functions.R
\name{binned_numeric_map_color}
\alias{binned_numeric_map_color}
\title{Returns a function to make colors from a numeric vector (n distinct values)}
\usage{
binned_numeric_map_color(
  vals,
  n = 20,
  palette_function = "viridis",
  colors = NULL,
  reverse = FALSE,
  method = c("bin", "quantile"),
  pretty = TRUE,
  na.color = grDevices::rgb(0, 0, 0, 0),
  ...
)
}
\arguments{
\item{vals}{A numeric vector}

\item{palette_function}{A palette function from the \code{pals} package}

\item{method}{If bin, cuts the numeric vector in equal parts. If quantile,
cuts it in equal quantile parts (so that each bin contains ca. equal number of observations)}

\item{na.color}{Color for NA values (default white)}
}
\description{
Returns a function to make colors from a numeric vector (n distinct values)
}
