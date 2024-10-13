library(ggplot2)
library(ggspatial)
library(ggrepel)
library(sf)

# class geogg

# constructors

geogg = function(
) {
  obj = ggplot() +
    theme_light()

  class(obj) = c(class(obj), 'geogg')

  obj = obj %>% geogg.theme_clean()

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
  labels = NULL, #: vector
  groups=NULL, #: factor
  color_map=NULL, #: key-value character vector
  legend_title = 'Legend Title', #: character
  size = vizsize.parse('normal')
) {
  .geogg.check_class(this)
  type.check_numeric(latitude, 'latitude')
  type.check_numeric(latitude, 'longitude')
  type.check_character(legend_title, 'legend_title')

  size = vizsize.parse(size)
  this$size = size

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

  if(!is.null(labels) & length(labels) != length(latitude)) {
    stop("'labels' and 'latitude' must be vectors of the same length")
  }

  mask = !(is.na(df$longitude) | is.na(df$latitude))
  df_filter = data.frame(
    latitude = latitude,
    longitude = longitude,
    groups = groups
  )

  if(!is.null(labels)) {
    df_filter$labels = labels
  }

  df_filter = df_filter[mask,]

  latitude = df_filter$latitude
  longitude = df_filter$longitude
  groups = df_filter$groups

  if(!is.null(labels)) {
    labels = df_filter$labels
  }

  ommited_length = length(mask[mask == FALSE])
  if(ommited_length != 0) {
    warning(
      paste0(
        ommited_length, ' points were omitted due to missing location data'
      )
    )
  }

  georef_obj = georef.from_points(latitude, longitude)

  this = this +
    geom_sf(
      aes(color=groups),
      data=georef_obj$sf_obj,
      size=this$size$point_size * 1.2
    ) +
    scale_color_manual(
      name = legend_title,
      values = color_map
    )

  if(!is.null(labels)) {
    data_labels = georef_obj$sf_obj
    data_labels$labels = labels

    this = this +
      geom_text_repel(
        data = data_labels,
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

  this = this %>% geogg.guides_points() %>%
    geogg.theme_base()

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

geogg.guides_points = function(
    this #: geogg
) {
  .geogg.check_class(this)

  this = this +
    guides(color = guide_legend(
      position = "right"
    ))

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

