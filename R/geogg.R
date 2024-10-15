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
    surface_height=100 #: numeric
) {
  .pca.check_class(pca_obj)

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

  obj = geogg(size = 'normal') %>%
    geogg.add_tiles()

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
    obj = obj %>% geogg.add_boundary(georef.from_geojson(malha))
  }

  data = pca_obj$principal_components$CP1

  obj = obj %>% geogg.add_points(
    latitude=latitude,
    longitude=longitude,
    groups=factor(ifelse(
      data < quantile(data, 0.25), 'A', ifelse(
        data < quantile(data, 0.50), 'B', ifelse(
          data < quantile(data, 0.75), 'C', 'D'
        )
      )
    )
    ),
    color_map = c(
      'A' = colors.red_to_green()[1],
      'B' = colors.red_to_green()[2],
      'C' = colors.red_to_green()[3],
      'D' = colors.red_to_green()[4]
    ),
    labels = c(
      '0%   |-   25%', '25% |-   50%', '50% |-   75%', '75% |-| 100%'
    ),
    legend_title = 'Desempenho Relativo',
    add_new_scale = if(add_surface) TRUE else FALSE
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

geogg.percentage_of_proficiency_map = function(
  data, #: numeric vector
  subject = c('mathematics', 'portuguese language'), #: character
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
  surface_height=100 #: numeric
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

  obj = geogg(size = 'normal') %>%
    geogg.add_tiles()

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
    obj = obj %>% geogg.add_boundary(georef.from_geojson(malha))
  }

  legend_title = if(subject[1] == 'mathematics') {
    'Apredizado Adequado\nem Matemática'
  } else {
    'Apredizado Adequado\nem Língua Portuguesa'
  }

  obj = obj %>% geogg.add_points(
      latitude=latitude,
      longitude=longitude,
      groups=factor(ifelse(
        data < 25, 'A', ifelse(
          data < 50, 'B', ifelse(
            data < 70, 'C', 'D'
          )
        )
      )
      ),
      color_map = c(
        'A' = colors.red_to_green()[1],
        'B' = colors.red_to_green()[2],
        'C' = colors.red_to_green()[3],
        'D' = colors.red_to_green()[4]
      ),
      labels = c(
        '0%   |-   25%', '25% |-   50%', '50% |-   70%', '70% |-| 100%'
      ),
      legend_title = legend_title,
      add_new_scale = if(add_surface) TRUE else FALSE
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
  labels=NULL, #: vector
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

  if(is.null(labels)) {
    labels = levels(groups)
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
      values = color_map,
      labels = labels
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
      color = colors.grayscale()[4],
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
      name=legend_title
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
