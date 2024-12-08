# class georef

# constructors

#' georef.from_geojson
#'
#' Creates a \code{georef} object from a GeoJSON string.
#'
#' @param geojson A character string representing the GeoJSON.
#'
#' @return A \code{georef} object containing the Simple Features (SF) representation of the GeoJSON.
#'
#' @export
georef.from_geojson = function(
  geojson #: character
) {
  type.check_character(geojson)
  sf_obj = sf::st_read(geojson)
  sf_obj = sf::st_transform(sf_obj, crs = 4326)

  this = list()
  class(this) = 'georef'

  this = this %>% georef.set_sf(sf_obj)
  return(this)
}

#' georef.from_sf
#'
#' Creates a \code{georef} object from an SF object.
#'
#' @param sf_obj An SF object representing the geographical data.
#'
#' @return A \code{georef} object containing the transformed SF object with CRS set to 4326.
#'
#' @export
georef.from_sf = function(
    sf_obj #: sf
) {
  type.check_sf(sf_obj)
  sf_obj = sf::st_transform(sf_obj, crs = 4326)

  this = list()
  class(this) = 'georef'

  this = this %>% georef.set_sf(sf_obj)
  return(this)
}

#' georef.from_points
#'
#' Creates a \code{georef} object from latitude and longitude points.
#'
#' @param latitude A numeric vector of latitude coordinates.
#' @param longitude A numeric vector of longitude coordinates.
#'
#' @return A \code{georef} object containing the Simple Features (SF) representation of the points.
#'
#' @export
georef.from_points = function(
  latitude, #: numeric
  longitude #: numeric
) {
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(longitude, 'longitude')

  if(length(longitude) != length(latitude)) {
    stop("'latitude' and 'longitude' must be vectors of the same length")
  }

  data = data.frame(
    latitude = latitude,
    longitude = longitude
  )

  sf_obj = sf::st_as_sf(
    data,
    coords = c('longitude', 'latitude'),
    crs = 4326
  )

  this = list()
  class(this) = 'georef'
  this$sf_obj = sf_obj

  return(this)
}

# properties

#' georef.set_sf
#'
#' Sets the Simple Features (SF) object for the \code{georef} instance.
#'
#' @param this A \code{georef} object.
#' @param sf_obj An SF object to be associated with the \code{georef}.
#'
#' @return The updated \code{georef} object with the SF data set.
#'
#' @export
georef.set_sf = function(
  this, #: georef
  sf_obj #: sf
) {
  .georef.check_class(this)
  type.check_sf(sf_obj)

  this$sf = sf_obj
  return(this)
}

# methods

#' .georef.check_class
#'
#' Checks if the provided object is of class \code{georef}.
#'
#' @param obj An object to check.
#'
#' @return NULL if the class is valid; otherwise, an error is raised.
.georef.check_class = function(
    obj
) {
  if(!('georef' %in% class(obj))) {
    stop("'obj' must be of type 'georef'")
  }
}

#' georef.get_raster
#'
#' Generates a raster based on the provided data points and their geographical coordinates.
#'
#' @param this A \code{georef} object.
#' @param data A numeric vector containing values to be interpolated.
#' @param latitude A numeric vector of latitude coordinates corresponding to the data points.
#' @param longitude A numeric vector of longitude coordinates corresponding to the data points.
#' @param width An integer specifying the width of the output raster.
#' @param height An integer specifying the height of the output raster.
#'
#' @return A terra SpatRaster object representing the interpolated values over the specified grid.
#'
#' @export
georef.get_raster = function(
  this, #: georef
  data, #: numeric vector
  latitude, #: numeric vector
  longitude, #: numeric vector
  width, #: integer
  height #: integer
) {
  .georef.check_class(this)
  type.check_numeric(data)
  type.check_integer(width)
  type.check_integer(height)
  type.check_numeric(latitude)
  type.check_numeric(longitude)
  if(length(latitude) != length(data)) {
    stop("'data' and 'latitude' must be vectors of the same length")
  }
  if(length(longitude) != length(data)) {
    stop("'data' and 'longitude' must be vectors of the same length")
  }

  mask = !(is.na(longitude) | is.na(latitude))
  df_filter = data.frame(
    data = data,
    latitude = latitude,
    longitude = longitude
  )

  df_filter = df_filter[mask,]
  data = df_filter$data
  latitude = df_filter$latitude
  longitude = df_filter$longitude

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  boundary = sf::st_union(this$sf) %>% sf::st_as_sf()

  data_sf = data.frame(
    data = data,
    latitude = latitude,
    longitude = longitude
  )
  data_sf =  sf::st_as_sf(
    data_sf,
    coords = c('longitude', 'latitude'),
    crs = sf::st_crs(boundary)
  )

  grid = terra::rast(boundary, nrows=height, ncols=width)
  xy = terra::xyFromCell(grid, 1:terra::ncell(grid)) %>% as.data.frame()
  coop = sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(boundary))
  coop = sf::st_filter(coop, boundary)

  model <- gstat::gstat(formula = data ~ 1, locations = data_sf, nmax = 3,
                 set = list(idp = 1))
  resp <- predict(model, coop)
  pred <- terra::rasterize(resp, grid, field = "var1.pred", fun = "mean")
  return(pred)
}

