% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vizsize.R
\name{vizsize.parse}
\alias{vizsize.parse}
\title{Parse Visualization Size Input}
\usage{
vizsize.parse(size)
}
\arguments{
\item{size}{An input that can be of type 'vizsize', character, or numeric.}
}
\value{
A vizsize object based on the input specifications.
}
\description{
This function parses input size specifications into a vizsize object.
}
\examples{
parsed_size <- vizsize.parse('large')

}
