library(ggplot2)
library(ggspatial)
library(ggrepel)
library(tidyterra)
library(ggnewscale)
library(sf)

# class geogg

# constructors

geogg = function(
  size = vizsize.parse('normal')
) {
  size = vizsize.parse(size)

  obj = ggplot() +
    theme_light()

  class(obj) = c(class(obj), 'geogg')

  obj = obj %>% geogg.theme_clean()

  obj$size = size

  return(obj)
}

# methods
.geogg.check_class = function(
  obj
) {
  if(!("geogg" %in% class(obj))) {
    stop("'obj' must be of type 'geogg'")
  }
}

geogg.add_tiles = function(
  this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    annotation_map_tile(
      type='cartolight'
    )

  return(this)
}

geogg.add_points = function(
  this, #: geogg
  latitude, #: numeric
  longitude, #: numeric
  groups=NULL, #: factor
  color_map=NULL, #: key-value character vector
  legend_title = 'Legend Title', #: character
  add_new_scale = FALSE #: logical
) {
  .geogg.check_class(this)
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(latitude, 'longitude')
  type.check_character(legend_title, 'legend_title')
  type.check_logical(add_new_scale, 'add_new_scale')

  if(length(longitude) != length(latitude)) {
    stop("'latitude' and 'longitude' must be vectors of the same length")
  }

  if(is.null(groups)) {
    groups = factor(rep('Group 1', length(latitude)), levels = 'Group 1')
    color_map = c('Group 1' = colors.mixed()[1])

    warning(paste("Since 'groups' is NULL, 'colors' was ignored.",
                  "Defaulting to a single group."))
  }

  type.check_factor(groups, 'groups')
  type.check_character(color_map, 'color_map')

  if(length(groups) != length(latitude)) {
    stop("'groups' and 'latitude' must be vectors of the same length")
  }

  mask = !(is.na(df$longitude) | is.na(df$latitude))
  df_filter = data.frame(
    latitude = latitude,
    longitude = longitude,
    groups = groups
  )

  df_filter = df_filter[mask,]

  latitude = df_filter$latitude
  longitude = df_filter$longitude
  groups = df_filter$groups

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  georef_obj = georef.from_points(latitude, longitude)

  if(add_new_scale) {
    this = this +
      new_scale_fill()
  }
  this = this +
    geom_sf(
      aes(fill=groups),
      data=georef_obj$sf_obj,
      size=this$size$point_size * 1.1,
      shape=21
    ) +
    scale_fill_manual(
      name = legend_title,
      values = color_map
    )

  this = this %>%
    geogg.theme_base()

  return(this)
}

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

  mask = !(is.na(df$longitude) | is.na(df$latitude))
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
    geom_text_repel(
      data = data,
      aes(
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
}

geogg.add_boundary = function(
  this, #: geogg
  georef_obj #: georef
) {
  .geogg.check_class(this)
  .georef.check_class(georef_obj)

  this = this +
    geom_sf(
      data = georef_obj$sf,
      linewidth = this$size$linewidth * 1.5,
      color = colors.grayscale()[5],
      fill = 'transparent'
    )

  return(this)
}

geogg.add_surface = function(
  this,
  georef_obj, #: georef
  data, #: numeric vector
  latitude, #: numeric vector
  longitude, #: numeric vector
  width = 100, #: integer
  height = 100, #: integer
  title = 'Surface Title', #: character
  palette = colors.purples(), #: character
  opacity = 'CC', #: character (00-FF)
  add_new_scale = FALSE #: logical
) {
  .geogg.check_class(this)
  .georef.check_class(georef_obj)
  type.check_character(title, 'title')
  type.check_character(palette, 'palette')
  type.check_character(opacity, 'opacity')
  type.check_logical(add_new_scale, 'add_new_scale')

  palette = paste0(palette, opacity)

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
      new_scale_fill()
  }
  this = this +
    geom_spatraster(
      data = surface
    ) +
    scale_fill_gradientn(
      colors = palette,
      na.value=NA,
      name=title
    )

  return(this)
}

geogg.theme_clean = function(
  this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

  return(this)
}

geogg.theme_base = function(
    this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    theme(
      legend.text = element_text(size=this$size$text),
      legend.title = element_text(size=this$size$text)
    )

  return(this)
}
