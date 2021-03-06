% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upDown.R
\name{upDown}
\alias{upDown}
\title{Displays the number of genes with positive and negative fitness coefficients}
\usage{
upDown(
  testCondition,
  control,
  p = 0.05,
  dataTable = NULL,
  undefCountTable = NULL,
  correction = "undefined",
  drawPlot = FALSE
)
}
\arguments{
\item{testCondition}{The name of the testing condition.}

\item{control}{The name of the control condition that the test condition should be compared to.}

\item{p}{The p-value cutoff to be included in the significance count.}

\item{correction}{Method for correcting for uneven count distribution between samples (can be "undefined", "median" or "none").}

\item{drawPlot}{If TRUE, the function draws a plot showing the fitness coefficients of each gene, ranked by p-value.}
}
\value{
Returns three numbers: the number of genes with a positive fitness coefficient, the number of genes with a negative fitness coefficient, and the expected number of genes with a significant positive and negative fitness coefficient based on the p-value cutoff and the number of genes.
}
\description{
This function shows the number of genes with a significantly positive and negative fitness coefficient between a test condition and the control condition. Most of the options are the same as for the fitCof function.
By default, the significance threshold is set to 0.05. This function uses the uncorrected p-values.
The upDown function can also draw a plot showing the fitness coefficients of each gene in the dataset, ranked by p-value, by setting drawPlot to TRUE.
}
