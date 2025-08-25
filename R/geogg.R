# class geogg

# constructors

#' Initialize Geographical ggplot (geogg)
#'
#' Creates a ggplot object for geographical visualizations with a specified size and theme.
#'
#' @param size A vizsize object or character to define the plot size, defaults to "normal".
#' @return An object of class 'geogg' initialized with a clean theme for geographical data.
#' @export
geogg = function(
    size = vizsize.parse('normal')
) {
  size = vizsize.parse(size)

  obj = ggplot2::ggplot() +
    ggplot2::theme_light()

  class(obj) = c(class(obj), 'geogg')

  obj = obj %>% geogg.theme_clean()

  obj$size = size

  return(obj)
}

#' Add PCA Map to Geographical Plot
#'
#' This function creates a geographical plot that visualizes relative performance using principal component analysis (PCA) scores, with optional boundaries, surfaces, and custom color scales.
#'
#' @param pca_obj A PCA object containing principal component scores for each observation.
#' @param latitude A numeric vector of latitudes for the data points.
#' @param longitude A numeric vector of longitudes for the data points.
#' @param labels An optional vector of labels for each point, default is NULL.
#' @param add_boundary A logical value indicating whether to add a boundary overlay, default is FALSE.
#' @param add_surface A logical value indicating whether to add a surface overlay, default is FALSE.
#' @param georef_obj A georef object for geographical reference, required if add_boundary or add_surface is TRUE.
#' @param surface_data A numeric vector representing surface data, required if add_surface is TRUE.
#' @param surface_latitude A numeric vector of latitudes for the surface data, required if add_surface is TRUE.
#' @param surface_longitude A numeric vector of longitudes for the surface data, required if add_surface is TRUE.
#' @param surface_legend_title A character string for the legend title of the surface overlay, default is 'Legend Title'.
#' @param surface_palette A character vector of colors for the surface overlay, default is a purple color palette.
#' @param surface_width A numeric value specifying the width of the surface grid, default is 100.
#' @param surface_height A numeric value specifying the height of the surface grid, default is 100.
#' @param size An object defining size specifications, default is 'large'.
#' @param point_size A numeric describing the size of the points to be added into the plot.
#' @param boundary_width A numeric describing the width of the boundaries in the plot, if any.
#' @param zoom An integer indicating the zoom level for the map. Higher values indicate closer zoom, while lower values show a broader area.
#' @param groups An optional vector for grouping the points, default is NULL.
#' @param color_map A named character vector for color mapping groups, default is NULL.
#' @param add_tiles A logical value to include the map tiles, default is TRUE.
#'
#' @return A geogg object with the PCA map added.
#'
#' @export
geogg.pca_map = function(
    pca_obj, #: pca
    latitude, #: numeric vector
    longitude, #: numeric vector,
    labels=NULL,
    add_boundary=FALSE,
    add_surface=FALSE,
    georef_obj=NULL, #: georef
    surface_data=NULL, #: numeric vector
    surface_latitude=NULL, #: numeric vector
    surface_longitude=NULL, #: numeric vector
    surface_legend_title='Legend Title', #: character
    surface_palette=colors.purples(), #: character vector
    surface_width=100, #: numeric
    surface_height=100, #: numeric
    size = vizsize.parse('large'),
    point_size = 1, #: numeric
    boundary_width = 1, #: numeric
    zoom = NULL, #: integer
    groups = NULL, #: vector
    color_map = NULL,
    add_tiles = TRUE
) {
  .pca.check_class(pca_obj)

  if(!is.null(groups)) {
    if(length(groups) != nrow(pca_obj$data)) {
      stop("'groups' must be a vector of the same length as data")
    }
  }


  if(
    (is.null(groups) && !is.null(color_map)) ||
    (!is.null(groups) && is.null(color_map))
  ) {
    stop("'color_map' and 'groups' must be both specified or null")
  }

  if(add_boundary & is.null(georef_obj)) {
    stop("'add_boundary' is set to TRUE but no 'georef_obj' was given")
  }
  if(add_surface & is.null(georef_obj)) {
    stop("'add_surface' is set to TRUE but no 'georef_obj' was given")
  }
  if(add_surface & is.null(surface_data)) {
    stop("'add_surface' is set to TRUE but no 'surface_data' was given")
  }
  if(add_surface & is.null(surface_latitude)) {
    stop("'add_surface' is set to TRUE but no 'surface_latitude' was given")
  }
  if(add_surface & is.null(surface_longitude)) {
    stop("'add_surface' is set to TRUE but no 'surface_latitude' was given")
  }

  obj = geogg(size = size)

  if(add_tiles) {
    obj = obj %>% geogg.add_tiles(zoom = zoom)
  }

  if(add_surface) {
    obj = obj %>%
      geogg.add_surface(
        georef_obj = georef_obj,
        data = surface_data,
        latitude = surface_latitude,
        longitude = surface_longitude,
        width = surface_width,
        height = surface_height,
        legend_title = surface_legend_title,
        palette = surface_palette
      )
  }

  if(add_boundary) {
    obj = obj %>% geogg.add_boundary(georef_obj, boundary_width = boundary_width)
  }

  data = pca_obj$principal_components$CP1

  groups = if(!is.null(groups)) {
    groups
  } else {
    ifelse(
      data < stats::quantile(data, 0.25), '0% |- 25%', ifelse(
        data < stats::quantile(data, 0.50), '25% |- 50%', ifelse(
          data < stats::quantile(data, 0.75), '50% |- 75%', '75% |-| 100%'
        )
      )
    )
  }

  color_map = if(!is.null(color_map)) {
    color_map
  } else {
    c(
      '0% |- 25%' = colors.red_to_green()[1],
      '25% |- 50%' = colors.red_to_green()[2],
      '50% |- 75%' = colors.red_to_green()[3],
      '75% |-| 100%' = colors.red_to_green()[4]
    )
  }

  obj = obj %>% geogg.add_points(
    latitude=latitude,
    longitude=longitude,
    groups=groups,
    color_map = color_map,
    legend_title = 'Desempenho Relativo',
    add_new_scale = if(add_surface) TRUE else FALSE,
    point_size = point_size
  )

  if(!is.null(labels)) {
    obj = obj %>% geogg.add_labels(
      labels,
      latitude,
      longitude
    )
  }

  return(obj)
}

#' Add Percentage of Proficiency Map to Geographical Plot
#'
#' This function creates a geographical plot that visualizes proficiency levels in a specified subject by adding points for each location, optionally with boundary and surface data.
#'
#' @param data A numeric vector representing the percentage of proficiency values.
#' @param subject A character string indicating the subject, either 'mathematics' or 'portuguese language'. Default is 'mathematics'.
#' @param latitude A numeric vector of latitudes for the proficiency points.
#' @param longitude A numeric vector of longitudes for the proficiency points.
#' @param labels An optional vector of labels for each point, default is NULL.
#' @param add_boundary A logical value to include a boundary overlay, default is FALSE.
#' @param add_surface A logical value to include a surface overlay, default is FALSE.
#' @param georef_obj A georef object for geographic reference, required if add_boundary or add_surface is TRUE.
#' @param surface_data A numeric vector representing surface data, required if add_surface is TRUE.
#' @param surface_latitude A numeric vector of latitudes for the surface data, required if add_surface is TRUE.
#' @param surface_longitude A numeric vector of longitudes for the surface data, required if add_surface is TRUE.
#' @param surface_legend_title A character string for the legend title of the surface overlay, default is 'Legend Title'.
#' @param surface_palette A character vector of colors for the surface overlay, default is a purple color palette.
#' @param surface_width A numeric value specifying the width of the surface grid, default is 100.
#' @param surface_height A numeric value specifying the height of the surface grid, default is 100.
#' @param size An object defining size specifications, default is 'large'.
#' @param point_size A numeric describing the size of the points to be added.
#' @param zoom An integer indicating the zoom level for the map. Higher values indicate closer zoom, while lower values show a broader area.
#' @param add_tiles A logical value to include the map tiles, default is TRUE.
#' @param boundary_width A numeric describing the width of the boundaries.
#'
#' @return A geogg object with the proficiency map added.
#'
#' @export
geogg.percentage_of_proficiency_map = function(
    data, #: numeric vector
    subject = c('mathematics', 'portuguese language'), #: character
    latitude, #: numeric vector
    longitude, #: numeric vector,
    labels=NULL, #: vector
    add_boundary=FALSE, #: logical
    add_surface=FALSE, #: logical
    georef_obj=NULL, #: georef
    surface_data=NULL, #: numeric vector
    surface_latitude=NULL, #: numeric vector
    surface_longitude=NULL, #: numeric vector
    surface_legend_title='Legend Title', #: character
    surface_palette=colors.purples(), #: character vector
    surface_width=100, #: numeric
    surface_height=100, #: numeric
    size = vizsize.parse('large'),
    point_size = 1, #: numeric
    boundary_width = 1,
    zoom = NULL, #: integer
    add_tiles = TRUE
) {
  if(!(subject[1]) %in% c('mathematics', 'portuguese language')){
    stop("'subject' must be one of 'mathematics', 'portuguese language')")
  }

  if(add_boundary & is.null(georef_obj)) {
    stop("'add_boundary' is set to TRUE but no 'georef_obj' was given")
  }
  if(add_surface & is.null(georef_obj)) {
    stop("'add_surface' is set to TRUE but no 'georef_obj' was given")
  }
  if(add_surface & is.null(surface_data)) {
    stop("'add_surface' is set to TRUE but no 'surface_data' was given")
  }
  if(add_surface & is.null(surface_latitude)) {
    stop("'add_surface' is set to TRUE but no 'surface_latitude' was given")
  }
  if(add_surface & is.null(surface_longitude)) {
    stop("'add_surface' is set to TRUE but no 'surface_latitude' was given")
  }

  obj = geogg(size = size)

  if(add_tiles) {
    obj = obj %>% geogg.add_tiles(zoom = zoom)
  }

  if(add_surface) {
    obj = obj %>%
      geogg.add_surface(
        georef_obj = georef_obj,
        data = surface_data,
        latitude = surface_latitude,
        longitude = surface_longitude,
        width = surface_width,
        height = surface_height,
        legend_title = surface_legend_title,
        palette = surface_palette
      )
  }

  if(add_boundary) {
    obj = obj %>% geogg.add_boundary(georef_obj, boundary_width = boundary_width)
  }

  legend_title = if(subject[1] == 'mathematics') {
    'Apredizado Adequado\nem Matem\u00e1tica'
  } else {
    'Apredizado Adequado\nem L\u00edngua Portuguesa'
  }

  obj = obj %>% geogg.add_points(
    latitude=latitude,
    longitude=longitude,
    groups=ifelse(
      data < 25, '0% |- 25%', ifelse(
        data < 50, '25% |- 50%', ifelse(
          data < 70, '50% |- 70%', '70% |-| 100%'
        )
      )
    ),
    color_map = c(
      '0% |- 25%' = colors.red_to_green()[1],
      '25% |- 50%' = colors.red_to_green()[2],
      '50% |- 70%' = colors.red_to_green()[3],
      '70% |-| 100%' = colors.red_to_green()[4]
    ),
    legend_title = legend_title,
    add_new_scale = if(add_surface) TRUE else FALSE,
    point_size = point_size
  )

  if(!is.null(labels)) {
    obj = obj %>% geogg.add_labels(
      labels,
      latitude,
      longitude
    )
  }

  return(obj)
}

# methods
#' Check Geographical Plot Class
#'
#' This internal function verifies if the given object is of class `geogg`.
#' It raises an error if the object is not of the correct class.
#'
#' @param obj An object to be checked.
#'
#' @return NULL. This function is called for its side effect of error-checking.
#'
#' @details
#' The function ensures that the input `obj` is of type `geogg`. It is typically used within other `geogg` methods to validate input objects.
#'
#' @keywords internal
#'
.geogg.check_class = function(
    obj
) {
  if(!("geogg" %in% class(obj))) {
    stop("'obj' must be of type 'geogg'")
  }
}

#' Add Tile Background to Geographical Plot
#'
#' This function adds a tile layer as a background to a `geogg` object, enhancing the map visualization with a predefined cartographic style.
#'
#' @param this A `geogg` object to which the tile layer will be added.
#' @param zoom An integer indicating the zoom level for the map. Higher values indicate closer zoom, while lower values show a broader area.
#'
#' @return A `geogg` object with an added tile background layer.
#'
#' @details
#' This function utilizes `annotation_map_tile` with a "cartolight" tile style, adding a light cartographic background to the map. The function is typically used to enhance readability and provide geographic context to overlaid data.
#'
#' @export
#'
geogg.add_tiles = function(
    this, #: geogg
    zoom = NULL #: integer
) {
  .geogg.check_class(this)
  if(!is.null(zoom)) {
    type.check_integer(zoom, 'zoom')
  }

  this = this +
    ggspatial::annotation_map_tile(
      type='cartolight',
      zoom=zoom
    )

  return(this)
}

#' Add Points to Geographical Plot
#'
#' This function adds points to a geographical ggplot object, with options for grouping and color mapping.
#'
#' @param this An object of class \code{geogg}.
#' @param latitude A numeric vector of latitudes for the points.
#' @param longitude A numeric vector of longitudes for the points.
#' @param groups An optional vector for grouping the points (fill), default is \code{NULL}.
#' @param groups_shape An optional vector for grouping the points shapes, default is \code{NULL}.
#' @param color_map A named character vector for color mapping of \code{groups}, default is \code{NULL}.
#' @param shape_map A named numeric vector for shape mapping of \code{groups_shape}, default is \code{NULL}.
#' @param labels An optional vector of labels for each fill group, default is \code{NULL}.
#' @param labels_shape An optional vector of labels for each shape group, default is \code{NULL}.
#' @param legend_title A character string for the legend title, default is \code{'Legend Title'}.
#' @param legend_title_shape A character string for the shape's legend title, default is \code{'Legend Title Shape'}.
#' @param add_new_scale A logical value indicating if a new color scale should be added (useful after a surface layer), default is \code{FALSE}.
#' @param point_size A numeric value specifying the overall size of the points, default is \code{1}.
#' @return The updated geogg object with points added.
#' @export
geogg.add_points = function(
    this, #: geogg
    latitude, #: numeric
    longitude, #: numeric
    groups=NULL, #: vector
    groups_shape=NULL, #: vector
    color_map=NULL, #: key-value character vector
    shape_map=NULL, #: key-value numeric vector
    labels=NULL, #: vector
    labels_shape=NULL, #: vector
    legend_title = 'Legend Title', #: character
    legend_title_shape = 'Legend Title Shape', #: character
    add_new_scale = FALSE, #: logical
    point_size = 1 #: numeric
) {
  .geogg.check_class(this)
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(latitude, 'longitude')
  type.check_character(legend_title, 'legend_title')
  type.check_character(legend_title_shape, 'legend_title_shape')
  type.check_logical(add_new_scale, 'add_new_scale')
  type.check_numeric(point_size, 'point_size')

  has_fill = !is.null(groups)
  has_shape = !is.null(groups_shape)

  if(length(longitude) != length(latitude)) {
    stop("'latitude' and 'longitude' must be vectors of the same length")
  }

  if(is.null(groups)) {
    groups = rep('Group 1', length(latitude))
    color_map = c('Group 1' = colors.mixed()[1])
  }

  if(is.null(groups_shape)) {
    groups_shape = rep('Group 1', length(latitude))
    shape_map = c('Group 1' = 21)
  }

  if(length(groups) != length(latitude)) {
    stop("'groups' and 'latitude' must be vectors of the same length")
  }

  if(length(groups_shape) != length(latitude)) {
    stop("'groups_shape' and 'latitude' must be vectors of the same length")
  }

  if(is.null(labels)) {
    labels = names(color_map)
  }

  if(is.null(labels_shape)) {
    labels_shape = names(shape_map)
  }

  mask = !(is.na(longitude) | is.na(latitude))
  df_filter = data.frame(
    latitude = latitude,
    longitude = longitude,
    groups = groups,
    groups_shape = groups_shape
  )

  df_filter = df_filter[mask,]

  latitude = df_filter$latitude
  longitude = df_filter$longitude
  groups = df_filter$groups
  groups_shape = df_filter$groups_shape

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  # Ensuring all groups are represented in the legend, even if absent in the data
  first_point = data.frame(
    latitude = latitude[1],
    longitude = longitude[1]
  )

  virtual_groups = names(color_map)[
    which(
      !(names(color_map) %in% unique(groups))
    )
  ]

  virtual_latitude = rep(first_point$latitude, length(virtual_groups))
  virtual_longitude = rep(first_point$longitude, length(virtual_groups))

  virtual_groups_shape = names(shape_map)[
    which(
      !(names(shape_map) %in% unique(groups_shape))
    )
  ]

  virtual_latitude_shape = rep(first_point$latitude, length(virtual_groups_shape))
  virtual_longitude_shape = rep(first_point$longitude, length(virtual_groups_shape))

  # END Ensuring all groups are represented in the legend, even if absent in the data

  georef_obj = georef.from_points(latitude, longitude)

  if(length(virtual_groups) > 0) {
    virtual_georef_obj = georef.from_points(virtual_latitude, virtual_longitude)
  }

  if(length(virtual_groups_shape) > 0) {
    virtual_georef_obj_shape = georef.from_points(
      virtual_latitude_shape, virtual_longitude_shape)
  }

  if(add_new_scale) {
    this = this +
      ggnewscale::new_scale_fill()
  }

  if(length(virtual_groups) > 0) {
    this = this +
      ggplot2::geom_sf(
        ggplot2::aes(fill=virtual_groups),
        data=virtual_georef_obj$sf_obj,
        size=this$size$point_size * 0.6 * point_size,
        shape=21
      )
  }

  if(length(virtual_groups_shape) > 0) {
    this = this +
      ggplot2::geom_sf(
        ggplot2::aes(shape=virtual_groups_shape),
        data=virtual_georef_obj_shape$sf_obj,
        size=this$size$point_size * 0.35 * point_size
      )
  }

  # injeta colunas no sf_data
  sf_data = georef.from_points(latitude, longitude)$sf_obj
  sf_data$groups = groups
  sf_data$groups_shape = groups_shape

  # monta aes dinamicamente
  aes_list = list()
  if (has_fill) aes_list$fill = rlang::sym("groups")
  if (has_shape) aes_list$shape = rlang::sym("groups_shape")
  mapping = ggplot2::aes(!!!aes_list)

  # monta lista de argumentos “fixos” fora do aes
  fixed_args = list()
  if (!has_shape) fixed_args$shape = 21
  if (!has_fill) fixed_args$fill = colors.mixed()[1]   # ou outro color default

  # junte tudo e invoque via do.call()
  geom_args = c(
    list(
      mapping = mapping,
      data = sf_data,
      size = this$size$point_size * 0.6 * point_size
    ),
    fixed_args
  )
  this = this + do.call(ggplot2::geom_sf, geom_args)

  # agora adicione as escalas **só** se mapeou aquela estética:
  if (has_fill) {
    this = this +
      ggplot2::scale_fill_manual(
        name   = legend_title,
        values = color_map,
        limits = names(color_map),
        drop   = FALSE,
        labels = labels,
        guide = ggplot2::guide_legend(
          override.aes = list(
            shape  = 21,          # mesmo shape dos pontos
            fill   = color_map    # preenchimento colorido
          )
        )
      )
  }
  if (has_shape) {
    this = this +
      ggplot2::scale_shape_manual(
        name   = legend_title_shape,
        values = shape_map,
        limits = names(shape_map),
        drop   = FALSE,
        labels = labels_shape
    )
  }

  this = this %>%
    geogg.theme_base()

  return(this)
}

#' Add Labels to Geographical Plot
#'
#' This function adds labels to specified points on a geographical ggplot.
#'
#' @param this An object of class 'geogg'.
#' @param labels A vector of labels for each point.
#' @param latitude A numeric vector for the latitudes of each point.
#' @param longitude A numeric vector for the longitudes of each point.
#' @return The updated geogg object with labels added.
#' @export
geogg.add_labels = function(
    this, #: geogg
    labels, #: vector
    latitude, #: numeric
    longitude #: numeric
) {
  .geogg.check_class(this)
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(latitude, 'longitude')

  if(length(labels) != length(latitude)) {
    stop("'labels' and 'latitude' must be vectors of the same length")
  }

  if(length(labels) != length(longitude)) {
    stop("'labels' and 'longitude' must be vectors of the same length")
  }

  mask = !(is.na(longitude) | is.na(latitude))
  df_filter = data.frame(
    latitude = latitude,
    longitude = longitude,
    labels = labels
  )

  df_filter = df_filter[mask,]

  latitude = df_filter$latitude
  longitude = df_filter$longitude
  labels = df_filter$labels

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  georef_obj = georef.from_points(latitude, longitude)

  data = georef_obj$sf_obj
  data$labels = labels

  this = this +
    ggrepel::geom_text_repel(
      data = data,
      ggplot2::aes(
        label = labels,
        geometry = geometry
      ),
      stat = "sf_coordinates",
      box.padding = this$size$point_size/4,
      point.padding = this$size$point_size/6,
      size = this$size$text / 5,
      min.segment.length = 0,
      max.overlaps = 50
    )

  return(this)
}

#' Add Boundary Layer to Geographical Plot
#'
#' Adds boundary lines to a geographical ggplot based on the input georef object.
#'
#' @param this An object of class 'geogg'.
#' @param georef_obj A georef object containing the boundary data in an sf format.
#' @param boundary_width A numeric describing the width of the boundaries.
#' @param boundary_color A string describing the color of the boundaries.
#' @return The updated geogg object with the boundary added.
#' @export
geogg.add_boundary = function(
    this, #: geogg
    georef_obj, #: georef
    boundary_width = 1, #: numeric
    boundary_color = colors.grayscale()[5] #: character
) {
  .geogg.check_class(this)
  .georef.check_class(georef_obj)
  type.check_numeric(boundary_width, 'boundary_width')
  type.check_character(boundary_color, 'boundary_color')

  this = this +
    ggplot2::geom_sf(
      data = georef_obj$sf,
      linewidth = this$size$linewidth * 1.0 * boundary_width,
      color = boundary_color,
      fill = 'transparent'
    )

  return(this)
}

#' Add Surface Layer to Geographical Plot
#'
#' Adds a surface layer (such as a heatmap or raster) to the geographical ggplot for additional data visualization.
#'
#' @param this An object of class 'geogg'.
#' @param georef_obj A georef object containing georeferenced information.
#' @param data A numeric vector representing the data values for the surface.
#' @param latitude A numeric vector for the latitudes corresponding to the data.
#' @param longitude A numeric vector for the longitudes corresponding to the data.
#' @param width An integer specifying the raster width.
#' @param height An integer specifying the raster height.
#' @param legend_title A character string for the legend title, default is 'Surface Title'.
#' @param palette A character vector defining the color palette, default is colors.purples().
#' @param opacity A character specifying the opacity in hexadecimal (00-FF), default is 'BB'.
#' @param add_new_scale A logical value indicating if a new color scale should be added, default is FALSE.
#' @return The updated geogg object with the surface layer added.
#' @export
geogg.add_surface = function(
    this,
    georef_obj, #: georef
    data, #: numeric vector
    latitude, #: numeric vector
    longitude, #: numeric vector
    width = 100, #: integer
    height = 100, #: integer
    legend_title = 'Surface Title', #: character
    palette = colors.purples(), #: character
    opacity = 'BB', #: character (00-FF)
    add_new_scale = FALSE #: logical
) {
  .geogg.check_class(this)
  .georef.check_class(georef_obj)
  type.check_character(legend_title, 'legend_title')
  type.check_character(palette, 'palette')
  type.check_character(opacity, 'opacity')
  type.check_logical(add_new_scale, 'add_new_scale')

  sf_filtered = data.frame(
    data = data,
    latitude = latitude,
    longitude = longitude
  ) %>% sf::st_as_sf(
    coords=c("longitude", "latitude"),
    crs=sf::st_crs(georef_obj$sf)
  ) %>% sf::st_filter(georef_obj$sf)

  new_palette = colors.rescale_palette(
    palette = palette,
    min_old = min(data),
    max_old = max(data),
    min_new = min(sf_filtered$data),
    max_new = max(sf_filtered$data)
  )

  new_palette = paste0(new_palette, opacity)

  surface = georef_obj %>%
    georef.get_raster(
      data = data,
      latitude = latitude,
      longitude = longitude,
      width = width,
      height = height
    )

  if(add_new_scale) {
    this = this +
      ggnewscale::new_scale_fill()
  }
  this = this +
    tidyterra::geom_spatraster(
      data = surface
    ) +
    ggplot2::scale_fill_gradientn(
      colors = new_palette,
      na.value=NA,
      name=legend_title
    )

  return(this)
}

#' Apply Clean Theme to Geographical ggplot
#'
#' Removes default ggplot elements to create a clean canvas for geographical data.
#'
#' @param this An object of class 'geogg'.
#' @return The updated geogg object with a clean theme applied.
#' @export
geogg.theme_clean = function(
    this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size=this$size$text,
        hjust = 0.5
      )
    )

  return(this)
}

#' Apply Base Theme to Geographical ggplot
#'
#' Adds a base theme with formatted legend text to the geographical ggplot.
#'
#' @param this An object of class 'geogg'.
#' @return The updated geogg object with base theme formatting applied.
#' @export
geogg.theme_base = function(
    this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size=this$size$text * 0.7),
      legend.title = ggplot2::element_text(size=this$size$text * 0.7),
      plot.title = ggplot2::element_text(
        size=this$size$text,
        hjust = 0.5
      )
    )

  return(this)
}

#' Remove Legend from geogg Plot
#'
#' This function removes the legend from a \code{geogg} plot, making it suitable for visualizations where the legend is not needed.
#'
#' @param this A \code{geogg} object.
#'
#' @return The updated \code{geogg} object with the legend removed.
#'
#' @export
geogg.without_legend = function(
    this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    ggplot2::theme(
      legend.position = 'none'
    )

  return(this)
}

#' Add Title to Geogg Plot
#'
#' This function adds a title to a `geogg` plot.
#'
#' @param this A `geogg` object.
#' @param title A character string specifying the title to be added to the plot.
#'
#' @return The modified `geogg` object with the specified title added.
#'
#' @export
geogg.add_title = function(
    this, #: geogg
    title #: character
) {
  .geogg.check_class(this)
  type.check_character(title, 'title')

  this = this +
    ggplot2::labs(
      title = title
    )

  return(this)
}
