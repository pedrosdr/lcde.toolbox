% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{utils.relative_magnitude}
\alias{utils.relative_magnitude}
\title{Relative Magnitude}
\usage{
utils.relative_magnitude(data, target = 100)
}
\arguments{
\item{data}{A numeric data frame or matrix with each row representing an observation.}

\item{target}{A numeric value or vector representing the target for comparison. Defaults to 100.
If a single numeric value is provided, it is repeated for each column; if a vector, its length
must match the number of columns in `data`.}
}
\value{
A numeric vector where each element represents the relative magnitude of an observation.
}
\description{
Calculates the relative magnitude of each observation in comparison to a target value.
}
\examples{
data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
relative_magnitudes <- utils.relative_magnitude(data, target = 50)

}
