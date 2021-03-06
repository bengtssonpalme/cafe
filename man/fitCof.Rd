% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitCof.R
\name{fitCof}
\alias{fitCof}
\title{Generate fitness coefficients for a pair of conditions (test vs. control)}
\usage{
fitCof(
  testCondition,
  control,
  dataTable = NULL,
  undefCountTable = NULL,
  correction = "undefined"
)
}
\arguments{
\item{testCondition}{The name of the testing condition.}

\item{control}{The name of the control condition that the test condition should be compared to.}

\item{correction}{Method for correcting for uneven count distribution between samples (can be "undefined", "median" or "none").}
}
\value{
Returns a fitness coefficient object (a matrix with a fitness coefficient for every gene and replicate).
}
\description{
This function generates a fitness coefficient object comparing a test condition and a control condition.
}
