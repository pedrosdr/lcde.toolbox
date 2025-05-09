% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geogg.R
\name{geogg.pca_map}
\alias{geogg.pca_map}
\title{Add PCA Map to Geographical Plot}
\usage{
geogg.pca_map(
  pca_obj,
  latitude,
  longitude,
  labels = NULL,
  add_boundary = FALSE,
  add_surface = FALSE,
  georef_obj = NULL,
  surface_data = NULL,
  surface_latitude = NULL,
  surface_longitude = NULL,
  surface_legend_title = "Legend Title",
  surface_palette = colors.purples(),
  surface_width = 100,
  surface_height = 100,
  size = vizsize.parse("large"),
  point_size = 1,
  boundary_width = 1,
  zoom = NULL,
  groups = NULL,
  color_map = NULL,
  add_tiles = TRUE
)
}
\arguments{
\item{pca_obj}{A PCA object containing principal component scores for each observation.}

\item{latitude}{A numeric vector of latitudes for the data points.}

\item{longitude}{A numeric vector of longitudes for the data points.}

\item{labels}{An optional vector of labels for each point, default is NULL.}

\item{add_boundary}{A logical value indicating whether to add a boundary overlay, default is FALSE.}

\item{add_surface}{A logical value indicating whether to add a surface overlay, default is FALSE.}

\item{georef_obj}{A georef object for geographical reference, required if add_boundary or add_surface is TRUE.}

\item{surface_data}{A numeric vector representing surface data, required if add_surface is TRUE.}

\item{surface_latitude}{A numeric vector of latitudes for the surface data, required if add_surface is TRUE.}

\item{surface_longitude}{A numeric vector of longitudes for the surface data, required if add_surface is TRUE.}

\item{surface_legend_title}{A character string for the legend title of the surface overlay, default is 'Legend Title'.}

\item{surface_palette}{A character vector of colors for the surface overlay, default is a purple color palette.}

\item{surface_width}{A numeric value specifying the width of the surface grid, default is 100.}

\item{surface_height}{A numeric value specifying the height of the surface grid, default is 100.}

\item{size}{An object defining size specifications, default is 'large'.}

\item{point_size}{A numeric describing the size of the points to be added into the plot.}

\item{boundary_width}{A numeric describing the width of the boundaries in the plot, if any.}

\item{zoom}{An integer indicating the zoom level for the map. Higher values indicate closer zoom, while lower values show a broader area.}

\item{groups}{An optional vector for grouping the points, default is NULL.}

\item{color_map}{A named character vector for color mapping groups, default is NULL.}

\item{add_tiles}{A logical value to include the map tiles, default is TRUE.}
}
\value{
A geogg object with the PCA map added.
}
\description{
This function creates a geographical plot that visualizes relative performance using principal component analysis (PCA) scores, with optional boundaries, surfaces, and custom color scales.
}
\examples{
geogg_obj <- geogg.pca_map(pca_obj = my_pca, latitude = c(-15, -20), longitude = c(-47, -48), labels = c("School A", "School B"))

}
