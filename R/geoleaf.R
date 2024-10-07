library(leaflet)

# class geoleaf
# constructors

library(leaflet)

# class geoleaf
# constructors

#' geoleaf
#'
#' Creates a new \code{geoleaf} object that extends the Leaflet map.
#'
#' @return A \code{geoleaf} object.
#'
#' @export
geoleaf = function() {
  this = leaflet(
    options = leafletOptions(
      zoomSnap=0.3,
      zoomDelta=0.1
    )
  )
  class(this) = c(class(this), 'geoleaf')
  return(this)
}

# methods

#' .geoleaf.check_class
#'
#' Checks if an object is of class \code{geoleaf}.
#'
#' @param obj An object to check.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
.geoleaf.check_class = function(
    obj
) {
  if(!('geoleaf' %in% class(obj))) {
    stop("'obj' must be of type 'geoleaf'")
  }
}

#' geoleaf.add_tiles
#'
#' Adds base tiles to the Leaflet map.
#'
#' @param this A \code{geoleaf} object.
#'
#' @return The updated \code{geoleaf} object with added tiles.
#'
#' @export
geoleaf.add_tiles = function(
    this #: geoviz, geoleaf
) {
  .geoleaf.check_class(this)

  this = this %>%
    leaflet::addTiles() %>%
    leaflet::addProviderTiles("CartoDB.VoyagerLabelsUnder")

  return(this)
}

#' geoleaf.add_pca_points
#'
#' Adds PCA points to the Leaflet map based on provided latitude and longitude.
#'
#' This function filters out any points with missing location data before adding them to the map.
#'
#' @param this A \code{geoleaf} object.
#' @param pca_obj A 'pca' object.
#' @param latitude A numeric vector of latitude coordinates for the PCA points.
#' @param longitude A numeric vector of longitude coordinates for the PCA points.
#'
#' @return The updated \code{geoleaf} object with added PCA points.
#'
#' warning Points with missing location data will be omitted from the map.
#
#' @export
geoleaf.add_pca_points = function(
    this, #: geoleaf
    pca_obj, #: pca
    latitude, #: numeric vector
    longitude, #: numeric vector,
    labels=NULL
) {
  .geoleaf.check_class(this)
  .pca.check_class(pca_obj)

  this = this %>%
    geoleaf.add_points(
      latitude = latitude,
      longitude = longitude,
      colors = pca_obj %>% pca.get_category_colors(),
      labels = labels
    )

  return(this)
}

#' Add Points to Map
#'
#' This function adds circular markers at specified geographic coordinates (latitude and longitude) to a `geoleaf` map. Markers can be customized with colors and labels.
#'
#' @param this A `geoleaf` map object.
#' @param latitude A numeric vector specifying the latitude coordinates for the points.
#' @param longitude A numeric vector specifying the longitude coordinates for the points.
#' @param colors An optional character vector specifying the colors for each marker. If not provided, a default color is used.
#' @param labels An optional vector of labels corresponding to each marker. If not provided, no labels are displayed.
#'
#' @return The modified `geoleaf` map object with the added points.
#'
#' @details
#' The function checks that `latitude` and `longitude` are numeric vectors of the same length. If `labels` are not provided, they default to empty strings, and the marker radius is set to a smaller size. If `colors` are not provided, a default color is used for all markers. The function omits any points with missing coordinates and issues a warning indicating how many points were omitted.
#'
#' @seealso \code{\link{addCircleMarkers}}, \code{\link{geoleaf}}, \code{\link{leaflet}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_points(
#'     latitude = c(-23.5505, -22.9068),
#'     longitude = c(-46.6333, -43.1729),
#'     colors = c('#FF0000', '#00FF00'),
#'     labels = c('São Paulo', 'Rio de Janeiro')
#'   )
#'
#' @export
geoleaf.add_points = function(
  this, #: geoleaf
  latitude, #: numeric vector
  longitude, #: numeric vector
  colors=NULL, #: character vector
  labels=NULL #: vector
) {
  .geoleaf.check_class(this)
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(longitude, 'longitude')

  radius = 9
  if(is.null(labels)) {
    radius = 7
    labels = rep('', length(longitude))
  }

  if(length(longitude) != length(latitude)) {
    stop("'longitude' and 'latitude' must be vectors of the same length")
  }
  if(length(longitude) != length(labels)) {
    stop("'longitude' and 'labels' must be vectors of the same length")
  }

  if(is.null(colors)) {
    colors = rep(colors.mixed()[1], length(longitude))
  }
  type.check_character(colors, 'colors')
  if(length(longitude) != length(colors)) {
    stop("'longitude' and 'colors' must be vectors of the same length")
  }

  mask = !(is.na(df$longitude) | is.na(df$latitude))
  df_filter = data.frame(
    latitude = latitude,
    longitude = longitude,
    colors = colors,
    labels = labels
  )

  df_filter = df_filter[mask,]
  latitude = df_filter$latitude
  longitude = df_filter$longitude
  colors = df_filter$colors
  labels = df_filter$labels

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  this = this %>%
    leaflet::addCircleMarkers(
      lng = longitude,
      lat = latitude,
      color = colors.grayscale()[5],
      fillColor = colors,
      weight = 0.7,
      opacity = 1,
      fillOpacity = 1,
      radius = radius,
      label = labels,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list("font-size" = "11px", "font-weight" = "750")
      )
    )

  return(this)
}

#' Add Boundary to Map
#'
#' This function adds a boundary layer to a `geoleaf` map using a spatial object provided in the `georef` format. The boundary is rendered as polygons with customizable color and opacity settings.
#'
#' @param this A `geoleaf` map object.
#' @param georef_obj A `georef` object containing the spatial data to be added as a boundary.
#'
#' @return The modified `geoleaf` map object with the added boundary layer.
#'
#' @details
#' The function checks that the `this` parameter is a valid `geoleaf` map object and that `georef_obj` is a valid `georef` object. It then adds the boundary polygons to the map, using grayscale colors for the border and filling. The line weight and opacity can also be adjusted.
#'
#' @seealso \code{\link{addPolygons}}, \code{\link{georef}}, \code{\link{geoleaf}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_boundary(georef_obj)
#'
#' @export
geoleaf.add_boundary = function(
  this, #: geoleaf
  georef_obj #: georef
) {
  .geoleaf.check_class(this)
  .georef.check_class(georef_obj)

  this = this %>%
    addPolygons(
      data=georef_obj$sf,
      color = colors.grayscale()[4],
      fillColor = colors.grayscale()[1],
      weight = 2,
      opacity = 1,
      fillOpacity = 0
    )

  return(this)
}

#' Add Continuous Legend to Map
#'
#' This function adds a continuous color gradient legend to a `geoleaf` map based on a numeric vector of data. The legend dynamically reflects the range of the data, with a title and customizable position.
#'
#' @param this A `geoleaf` map object.
#' @param data A numeric vector representing the data used to generate the color gradient for the legend.
#' @param title A character string specifying the title of the legend.
#' @param position A character string specifying the position of the legend on the map. Options are 'bottomleft', 'bottomright', 'topleft', or 'topright'.
#' @param palette A character string specifying the color palette to use for the gradient. Defaults to \code{colors.nighty()}.
#'
#' @return The modified `geoleaf` map object with the added continuous legend.
#'
#' @details
#' The function creates a continuous color scale based on the input data, using the specified color palette. The legend is divided into five breaks, which represent the range of the data, and the color gradient is applied accordingly. The position and title of the legend are customizable.
#'
#' @seealso \code{\link{addLegend}}, \code{\link{colorNumeric}}, \code{\link{geoleaf}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_legend_continuous(
#'     data = c(1, 2, 3, 4, 5),
#'     title = 'Continuous Legend',
#'     position = 'bottomright'
#'   )
#'
#' @export
geoleaf.add_surface = function(
  this, #: geoleaf
  georef_obj, #: georef
  data, #: numeric vector
  latitude, #: numeric vector
  longitude, #: numeric vector
  palette=colors.purples(), #: character vector
  width=100, #: numeric
  height=100, #: numeric
  add_legend=FALSE, #: logic
  legend_title='Legend Title', #: character
  legend_position='bottomleft'
) {
  .geoleaf.check_class(this)
  .georef.check_class(georef_obj)
  type.check_numeric(data, 'data')
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(longitude, 'longitude')
  type.check_character(palette, 'palette')
  type.check_logical(add_legend, 'add_legend')
  type.check_character(legend_title, 'legend_title')

  if(length(data) != length(latitude)) {
    stop("'data' and 'latitude' must be vectors of the same length")
  }
  if(length(data) != length(longitude)) {
    stop("'data' and 'longitude' must be vectors of the same length")
  }
  if(length(palette) < 2) {
    stop("'palette' must have at least 2 colors")
  }

  surface = georef_obj %>% georef.get_raster(
    data = data,
    latitude = latitude,
    longitude = longitude,
    width = width,
    height = height
  )

  this = this %>%
    addRasterImage(
      surface,
      colors=colorNumeric(
        palette=palette,
        domain=values(surface),
        na.color='transparent'
        ),
      opacity = 0.7,
      project = FALSE
    )

  if(add_legend) {
    this = this %>%
      geoleaf.add_legend_continuous(
        data = data,
        title = legend_title,
        position=legend_position,
        palette = palette
      )
  }

  return(this)
}

geoleaf.add_legend_continuous = function(
  this, #: geoleaf
  data, #: numeric vector
  title, #: character
  position, #: character ('bottomleft', 'bottomright', 'topleft', 'topright')
  palette=colors.nighty() #: character
) {
  .geoleaf.check_class(this)
  type.check_character(palette, 'palette')
  type.check_numeric(data, 'data')
  type.check_character(title, 'title')
  type.check_character(position, 'position')

  breaks <- seq(min(data)-0.01, max(data), length.out = 5)
  breaks = as.numeric(sprintf('%.2f', breaks))
  this = this %>%
    addLegend(
      pal = colorNumeric(
        palette=palette,
        domain=data,
        na.color='transparent'
      ),
      position = position,
      title = title,
      values = breaks
    )

  return(this)
}

#' Add Discrete Legend to Map
#'
#' This function adds a discrete legend to a `geoleaf` map, specifying a title, a set of colors, labels, and a position for the legend.
#'
#' @param this A `geoleaf` map object.
#' @param title A character string specifying the title of the legend.
#' @param colors A character vector representing the colors for each discrete category in the legend.
#' @param labels A vector of labels corresponding to each color, describing the categories.
#' @param position A character string specifying the position of the legend on the map. Options are 'bottomleft', 'bottomright', 'topleft', or 'topright'.
#'
#' @return The modified `geoleaf` map object with the added legend.
#'
#' @details
#' This function checks if the `title`, `colors`, and `position` parameters are character strings, and ensures that the length of `labels` matches the length of `colors`. If the lengths don't match, an error is raised. The legend is added to the map with the specified title, color set, labels, and position.
#'
#' @seealso \code{\link{addLegend}}, \code{\link{geoleaf}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_legend_discrete(
#'     title = 'Category Legend',
#'     colors = c('#FF0000', '#00FF00', '#0000FF'),
#'     labels = c('Low', 'Medium', 'High'),
#'     position = 'bottomright'
#'   )
#'
#' @export
geoleaf.add_legend_discrete = function(
  this, #: geoleaf
  title, #: character
  colors, #: character vector
  labels, #: vector
  position #: character ('bottomleft', 'bottomright', 'topleft', 'topright')
) {
  .geoleaf.check_class(this)
  type.check_character(title, 'title')
  type.check_character(colors, 'colors')
  type.check_character(position, 'position')
  if(length(labels) != length(colors)) {
    stop("'labels' and 'colors' must be vectors of the same length")
  }

  this = this %>% addLegend(
    title = title,
    colors = colors,
    labels = labels,
    opacity = 1,
    position = position
  )

  return(this)
}

#' Add PCA Performance Legend to Map
#'
#' This function adds a discrete legend to a `geoleaf` map representing the relative performance based on the first principal component (PC1) of a PCA analysis. The legend divides the values into quartiles.
#'
#' @param this A `geoleaf` map object.
#'
#' @return The modified `geoleaf` map object with the added legend.
#'
#' @details
#' The function adds a legend to the map with four discrete ranges representing relative performance:
#' \itemize{
#'   \item 'Menor CP1 |- 1º Quartil'
#'   \item '1º Quartil |- 2º Quartil'
#'   \item '2º Quartil |- 3º Quartil'
#'   \item '3º Quartil |-| Maior CP1'
#' }
#' The color gradient goes from red (lower values of PC1) to green (higher values of PC1), indicating the performance relative to the first principal component.
#'
#' @seealso \code{\link{geoleaf.add_legend_discrete}}, \code{\link{colors.red_to_green}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_legend_pca()
#'
#' @export
geoleaf.add_legend_pca = function(
  this #: geoleaf
) {
  .geoleaf.check_class(this)

  this = this %>% geoleaf.add_legend_discrete(
    title = 'Desempenho Relativo',
    colors = colors.red_to_green(),
    labels = c(
      'Menor CP1  |-  1º Quartil',
      '1º Quartil  |-  2º Quartil',
      '2º Quartil  |-  3º Quartil',
      '3º Quartil  |-|  Maior CP1'
    ),
    position = 'bottomright'
  )

  return(this)
}

#' Add Percentage of Proficiency Legend to Map
#'
#' This function adds a discrete legend to a `geoleaf` map, representing the percentage of adequate learning in either Mathematics or Portuguese Language.
#' The legend ranges from 0% to 100% proficiency with corresponding color gradients.
#'
#' @param this A `geoleaf` map object.
#' @param subject A character string indicating the subject for which the legend should be displayed. Must be either 'mathematics' or 'portuguese language'. Defaults to 'mathematics'.
#'
#' @return The modified `geoleaf` map object with the added legend.
#'
#' @details
#' This function checks if the `subject` is either 'mathematics' or 'portuguese language'.
#' It then adds a legend at the bottom-right corner of the map with four discrete ranges:
#' \itemize{
#'   \item '0% |- 25%'
#'   \item '25% |- 50%'
#'   \item '50% |- 70%'
#'   \item '70% |-| 100%'
#' }
#' The legend title will be either 'Aprendizado Adequado em Matemática' or 'Aprendizado Adequado em Língua Portuguesa', based on the selected subject.
#'
#' @seealso \code{\link{geoleaf.add_legend_discrete}}, \code{\link{colors.red_to_green}}
#'
#' @examples
#' geoleaf.map <- geoleaf() %>%
#'   geoleaf.add_legend_percentage_of_proficiency(subject = 'mathematics')
#'
#' @export
geoleaf.add_legend_percentage_of_proficiency = function(
  this, #: geoleaf
  subject = c('mathematics', 'portuguese language') #: character
) {
  .geoleaf.check_class(this)

  subject = subject[1]
  if(!(subject %in% c('mathematics', 'portuguese language'))) {
    stop("'subject' must be one of ('mathematics', 'portuguese language')")
  }

  title = if(subject == 'mathematics') {
    'Aprendizado Adequado<br>em Matemática'
  } else {
    'Aprendizado Adequado<br>em Língua Portuguesa'
  }

  this = this %>% geoleaf.add_legend_discrete(
    title = title,
    colors = colors.red_to_green(),
    labels = c(
      '0% |- 25%',
      '25% |- 50%',
      '50% |- 70%',
      '70% |-| 100%'
    ),
    position = 'bottomright'
  )

  return(this)
}
