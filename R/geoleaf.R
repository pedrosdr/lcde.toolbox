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
  this = leaflet()
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
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(longitude, 'longitude')

  pca_categories = pca_obj %>% pca.get_categories()
  colors = pca_obj %>% pca.get_category_colors()

  mask = !(is.na(df$longitude) | is.na(df$latitude))
  latitude = latitude[mask]
  longitude = longitude[mask]
  pca_obj = pca_obj %>% pca.filter(mask)
  pca_categories = pca_categories[mask]
  colors = colors[mask]

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
      color = colors
    )
  return(this)
}
