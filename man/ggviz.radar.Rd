% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggviz.R
\name{ggviz.radar}
\alias{ggviz.radar}
\title{ggviz Radar Chart Constructor}
\usage{
ggviz.radar(
  data,
  colors = colors.mixed(),
  labels = NULL,
  title = NULL,
  show_score = FALSE,
  axes_to_invert = NULL,
  opacity = 0.3,
  size = vizsize()
)
}
\arguments{
\item{data}{A `data.frame` containing the data for the radar chart.}

\item{colors}{A character vector defining the colors of the groups.}

\item{labels}{A vector with labels for the groups. If `NULL`, labels are generated automatically.}

\item{title}{The title of the chart. If `NULL`, no title is displayed.}

\item{show_score}{Logical. If `TRUE`, displays the relative score for the first group.}

\item{axes_to_invert}{A vector with the names of the axes to be inverted (optional).}

\item{opacity}{A numeric value between 0 and 1 defining the opacity of the group fill (default is 0.3).}

\item{size}{A `vizsize` object or numeric values to define the text and line sizes of the chart.}
}
\value{
A `ggviz` object containing the radar chart.
}
\description{
Generates a radar chart using a data frame, with options for customizing
colors, titles, labels, and score display.
}
\examples{
\dontrun{
data <- data.frame(Group1 = c(70, 80, 90), Group2 = c(50, 60, 80))
ggviz.radar(data, colors = c("#FF0000", "#00FF00"), opacity = 0.7)
}
}
