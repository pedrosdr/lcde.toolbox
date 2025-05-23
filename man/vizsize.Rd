% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vizsize.R
\name{vizsize}
\alias{vizsize}
\title{Create a Visualization Size Object}
\usage{
vizsize(size = "normal", line = "normal", text = "normal")
}
\arguments{
\item{size}{A character string indicating the overall size category. Must be one of 'small', 'normal', or 'large'.}

\item{line}{A character string indicating the line size category. Must be one of 'small', 'normal', or 'large'.}

\item{text}{A character string indicating the text size category. Must be one of 'small', 'normal', or 'large'.}
}
\value{
An object of class 'vizsize' containing width, height, line width, point size, and text sizes.
}
\description{
This function creates a visualization size object based on specified size, line, and text categories.
}
\examples{
viz <- vizsize(size = 'normal', line = 'large', text = 'small')

}
