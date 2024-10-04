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
    longitude #: numeric vector
) {
  .geoleaf.check_class(this)
  .pca.check_class(pca_obj)

  this = this %>%
    geoleaf.add_points(
      data = pca_obj$principal_components$CP1,
      latitude = latitude,
      longitude = longitude,
      colors = pca_obj %>% pca.get_category_colors()
    )

  return(this)
}

geoleaf.add_points = function(
  this, #: geoleaf
  data, #: numeric vector
  latitude, #: numeric vector
  longitude, #: numeric vector
  colors=NULL #: character vector
) {
  .geoleaf.check_class(this)
  type.check_numeric(data, 'data')
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(longitude, 'longitude')

  if(length(data) != length(latitude)) {
    stop("'data' and 'latitude' must be vectors of the same length")
  }
  if(length(data) != length(longitude)) {
    stop("'data' and 'longitude' must be vectors of the same length")
  }

  if(is.null(colors)) {
    colors = rep(colors.mixed()[1], length(data))
  }
  type.check_character(colors, 'colors')
  if(length(data) != length(colors)) {
    stop("'data' and 'colors' must be vectors of the same length")
  }

  mask = !(is.na(df$longitude) | is.na(df$latitude))
  df_filter = data.frame(
    data = data,
    latitude = latitude,
    longitude = longitude,
    colors = colors
  )

  df_filter = df_filter[mask,]
  data = df_filter$data
  latitude = df_filter$latitude
  longitude = df_filter$longitude
  colors = df_filter$colors

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
      radius = 7
    )

  return(this)
}

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
  legend_position='bottomright'
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

  breaks <- seq(min(data)-0.01, max(data), length.out = 5)
  breaks = as.numeric(sprintf('%.2f', breaks))
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
  position, #: character
  palette=colors.nighty() #: character
) {
  .geoleaf.check_class(this)
  type.check_character(palette, 'palette')
  type.check_numeric(data, 'data')
  type.check_character(title, 'title')
  type.check_character(position, 'position')

  this = this %>%
    addLegend(
      pal = colorNumeric(
        palette=palette,
        domain=data,
        na.color='transparent'
      ),
      title = title,
      values = data
    )

  return(this)
}
