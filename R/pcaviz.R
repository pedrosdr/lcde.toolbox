# class pcaviz

# constructors

#' Create a PCA Visualization Object from a ggplot Object
#'
#' This function converts a ggplot object into a PCA visualization object
#' by associating it with a PCA analysis object.
#'
#' @param ggplot_obj A ggplot object that represents the PCA scatter plot.
#' @param pca_obj An object of class 'pca' containing the results of the PCA analysis.
#'
#' @return A 'pcaviz' object that combines the ggplot object with the PCA analysis.
#'
#' @details This function checks if the provided `pca_obj` is of class 'pca'
#' and ensures that `ggplot_obj` is indeed a ggplot object. The resulting
#' object can be used for further customization and visualization.
#'
#' @examples
#' # Assuming 'ggplot_plot' is an existing ggplot object and 'pca_result' is a pca object
#' pca_viz <- pcaviz.from_ggplot(ggplot_plot, pca_result)
#'
#' @export
pcaviz.from_ggplot = function(
  ggplot_obj, #: ggplot
  pca_obj = NULL #: pca
) {
  if(!('ggplot' %in% class(ggplot_obj))) {
    stop("'ggplot_obj' must be of type 'ggplot'")
  }

  this = ggplot_obj
  class(this) = c(class(this), 'pcaviz')

  if(!is.null(pca_obj)) {
    this = this %>% pcaviz.set_pca_obj(pca_obj)
  }

  return(this)
}

# properties

#' Set Size Properties for PCA Visualization
#'
#' This function sets the size properties for a PCA visualization object.
#'
#' @param this A PCA visualization object of class 'pcaviz'.
#' @param size_obj An object containing size properties (default is `vizsize()`).
#'
#' @return The modified PCA visualization object with updated size properties.
#'
#' @export
pcaviz.set_size = function(
  this, #: pcaviz
  size_obj = vizsize() #: vizsize | character | numeric
) {
  .pcaviz.check_class(this)
  size_obj = vizsize.parse(size_obj)

  this$size = size_obj
  return(this)
}


pcaviz.set_pca_obj = function(
  this, #: pcaviz
  pca_obj #: pca_obj
) {
  .pcaviz.check_class(this)
  .pca.check_class(pca_obj)

  this$pca_obj = pca_obj

  return(this)
}
# methods

#' Check if an Object is of Class 'pcaviz'
#'
#' This function checks if the provided object is of class 'pcaviz'.
#'
#' @param this An object to be checked.
#'
#' @return NULL if the object is of class 'pcaviz'; otherwise, an error is raised.
#'
#' @keywords internal
.pcaviz.check_class = function(
    obj
) {
  if(!inherits(obj, "pcaviz")) {
    stop("'obj' must be of type 'pcaviz'")
  }
}

#' .pcaviz.check_groups
#'
#' Validates the grouping variable for PCA visualization.
#'
#' This function checks that the `groups` parameter is a factor, has the same length as
#' the principal components data, and contains no more than 7 unique levels.
#'
#' @param this A \code{pcaviz} object containing PCA results.
#' @param groups A factor representing the grouping variable.
#'
#' @return NULL if all checks pass; otherwise, an error is raised with a descriptive message.
#'
#' @details
#' This internal function ensures that the grouping variable is appropriate for PCA analysis,
#' helping to prevent errors from incompatible group definitions.
#'
#' @export
.pcaviz.check_groups = function(
    this, #: pcaviz
    groups #: factor
) {
  .pcaviz.check_class(this)
  if(class(groups) != 'factor') {
    stop("'groups' must be of type 'factor'")
  }

  if(length(groups) != nrow(this$pca_obj$principal_components)) {
    stop("'groups' must be a vector of the same length as the data")
  }
}

#' Scatter Plot for PCA Visualization
#'
#' Generates a scatter plot for visualizing the results of a Principal Component Analysis (PCA),
#' including options for grouping, labeling, coloring, and shaping data points.
#'
#' @param pca_obj A \code{pca} object containing the PCA results.
#' @param labels An optional character vector of labels for the data points. Defaults to \code{NULL}.
#' @param groups An optional factor indicating group membership for each data point. Defaults to \code{NULL}.
#' @param color A single hex color code (as a string) to use when \code{groups} is \code{NULL}. Defaults to \code{colors.mixed()[1]}.
#' @param color_map An optional named or unnamed character vector of hex color codes to map each level of \code{groups}. Defaults to \code{NULL}.
#' @param shape_map An optional named or unnamed numeric vector of shape codes to map each level of \code{groups}. Only used when \code{single_shape = FALSE}. Defaults to \code{NULL}.
#' @param include_ID A logical value indicating whether to include the inequality indicator in the plot caption. Defaults to \code{FALSE}.
#' @param size A \code{vizsize} object or a numeric/text size parameter for customizing plot dimensions. Defaults to \code{vizsize()}.
#' @param single_shape A logical flag; if \code{TRUE}, all points use a single shape (\code{shape = 19}) and only \code{color_map} is applied. If \code{FALSE}, both color and shape mappings are used. Defaults to \code{FALSE}.
#' @param max.overlaps An integer specifying the maximum number of overlapping labels allowed when labels are added to the plot. Defaults to \code{20}.
#'
#' @return A \code{pcaviz} object representing the fully customized scatter plot.
#'
#' @details
#' Plots the first two principal components (CP1 and CP2) with optional labels, group coloring, custom palettes, and shape mappings. Axes display explained variance and include dashed reference lines.
#'
#' @export
pcaviz.scatter = function(
  pca_obj, #: pca
  color = colors.mixed()[1],
  labels = NULL, #: vector
  groups = NULL, #: factor
  color_map = NULL,
  shape_map = NULL,
  include_ID = FALSE, #: logical
  size = vizsize(), #: vizsize | text | numeric
  single_shape = FALSE,
  max.overlaps = 20
) {
  .pca.check_class(pca_obj)
  if(class(include_ID) != 'logical') {
    stop("'include_ID' must be of type 'logical'")
  }

  this = pcaviz.from_ggplot(ggplot2::ggplot(), pca_obj)
  if(!is.null(groups)) {
    this %>% .pcaviz.check_groups(groups)
  }

  this = this %>% pcaviz.set_size(size)

  this = this + ggplot2::theme_minimal()

  if(is.null(groups)) {
    this = this %>% pcaviz.add_single_group_points(color = color)
  } else {
    this = this %>% pcaviz.add_multi_group_points(
      groups = groups,
      color_map = color_map,
      shape_map = shape_map,
      single_shape = single_shape
    )
  }

  caption = paste0(
    pca_obj$component_equations[1],
    '\n',
    pca_obj$component_equations[2]
  )

  if(include_ID) {
    caption = paste0(
      caption,
      "\nIndicador de Desigualdade: ",
      sprintf("%.2f", this$pca_obj %>% pca.get_ID())
    )
  }

  this = this + ggplot2::labs(
      x = paste0(
        'CP1 (',
        sprintf("%.2f%%", 100*pca_obj$explained_variance[1]),
        ')'
      ),

      y = paste0(
        'CP2 (',
        sprintf("%.2f%%", 100*pca_obj$explained_variance[2]),
        ')'
      ),

      caption = caption
    ) +

    ggplot2::geom_hline(yintercept = 0, linetype='dashed',
               linewidth=this$size$linewidth,
               color=colors.grayscale()[5]) +
    ggplot2::geom_vline(xintercept = 0, linetype='dashed',
               linewidth=this$size$linewidth,
               color=colors.grayscale()[5])

  if(!is.null(labels)) {
    this = this %>% pcaviz.add_labels(
      labels,
      max.overlaps=max.overlaps
    )
  }

  if(!is.null(groups) & is.null(color_map)) {
    this = this %>% pcaviz.set_scale_scatter(groups)
  }

  this = this %>% pcaviz.set_theme_scatter()

  lim = max(
    abs(
      c(
        pca_obj$principal_components$CP1,
        pca_obj$principal_components$CP2
      )
    )
  )

  this = this +
    ggplot2::coord_cartesian(
      xlim = c(-lim, lim),
      ylim = c(-lim, lim)
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = 5
    ) +
    ggplot2::scale_y_continuous(
      n.breaks = 5
    )

  return(this)
}

#' pcaviz.explained_variance
#'
#' Generates a bar plot showing the percentage of explained variance for each PCA component.
#'
#' This function visualizes the proportion of variance explained by each principal component
#' in a PCA analysis. It creates a bar plot where the height of each bar represents
#' the percentage of variance explained by the corresponding component.
#'
#' @param pca_obj An object of class `pca`, which contains PCA results including
#'                explained variance and component names. The object must have a valid
#'                structure as expected by the pcaviz package.
#' @param size (Optional) A vizsize, character or numeric value that determines the size of the plot elements.
#'              Defaults to the value returned by `vizsize()`.
#'
#' @return A ggplot2 object representing the bar plot of explained variance for each PCA component.
#'         The plot includes:
#'         - Bars indicating the percentage of variance explained.
#'         - Labels displaying the explained variance formatted to two decimal places.
#'
#' @details
#' The function checks if the provided `pca_obj` is valid. It then creates a bar plot using
#' `geom_col()` to represent the explained variance for each principal component.
#' The y-axis is formatted to show percentages.
#'
#' The function uses:
#' - `theme_light()` for a clean background theme.
#' - `scale_y_continuous()` to format y-axis labels as percentages.
#'
#' @examples
#' # Assuming pca_obj is a valid PCA object created using prcomp or similar
#' library(ggplot2)
#' my_plot <- pcaviz.explained_variance(pca_obj)
#' print(my_plot)
#'
#' @export
pcaviz.explained_variance = function(
  pca_obj, #: pca
  size = vizsize() #: vizsize
) {
  .pca.check_class(pca_obj)
  this = pcaviz.from_ggplot(ggplot2::ggplot(), pca_obj)

  this = this %>% pcaviz.set_size(size)

  this = this +
    ggplot2::theme_light() +

    ggplot2::geom_col(
      ggplot2::aes(
        x=this$pca_obj$component_names,
        y=this$pca_obj$explained_variance
      ),
      fill=colors.mixed()[1]
    ) +

    ggplot2::labs(
      x='Componente Principal',
      y='Variância Explicada (%)'
    ) +

    ggplot2::geom_label(
      ggplot2::aes(
        x=this$pca_obj$component_names,
        y=this$pca_obj$explained_variance,
        label=sprintf(
          "%.2f%%", 100*this$pca_obj$explained_variance
        )
      ),
      size = this$size$text/3
    ) +

    ggplot2::scale_y_continuous(labels = function(x) {sprintf("%.2f%%", 100*x)})

  this = this %>% pcaviz.set_theme_column()

  return(this)
}

#' pcaviz.component_loads
#'
#' Generates a bar plot of PCA component loads using ggplot2.
#'
#' This function visualizes the loadings of indicators on a specified PCA component.
#' It creates a bar plot where the height of the bars represents the loadings,
#' with colors indicating whether the load is positive or negative.
#'
#' @param pca_obj An object of class `pca`, which contains the PCA results and loadings.
#'                The object must have a valid structure as expected by the pcaviz package.
#' @param component A numeric integer specifying which PCA component to visualize.
#'                  It must be within the range of available components in `pca_obj`.
#' @param size (Optional) A vizsize, character or numeric value that determines the size of the plot elements.
#'              Defaults to the value returned by `vizsize()`.
#'
#' @return A ggplot2 object representing the bar plot of PCA component loads.
#'         The plot includes:
#'         - Bars colored based on whether the load is positive or negative.
#'         - Labels displaying the loading values formatted to two decimal places.
#'
#' @details
#' The function checks if the provided `component` is valid for the given `pca_obj`.
#' If it is not valid, an error is raised. The colors for the bars are determined
#' by whether the load is positive (green) or negative (red).
#'
#' The function uses:
#' - `geom_col()` to create bar plots.
#' - `theme_light()` for a clean background theme.
#' - `scale_fill_manual()` to customize bar colors based on loading signs.
#'
#' @examples
#' # Assuming pca_obj is a valid PCA object created using prcomp or similar
#' library(ggplot2)
#' my_plot <- pcaviz.component_loads(pca_obj, component = 1)
#' print(my_plot)
#'
#' @export
pcaviz.component_loads = function(
    pca_obj, #: pca
    component, #: numeric integer
    size = vizsize() #: vizsize
) {
  .pca.check_class(pca_obj)

  this = pcaviz.from_ggplot(ggplot2::ggplot(), pca_obj)
  this = this %>% pcaviz.set_size(size)

  type.check_integer(component)

  if(component > pca_obj$length || component < 1) {
    stop("'component' must correspond to a valid component")
  }

  loads = this$pca_obj$loads[,component]
  data_names = rownames(this$pca_obj$loads) %>%
    factor(levels = rownames(this$pca_obj$loads))

  this = this +
    ggplot2::theme_light() +

    ggplot2::geom_col(
      ggplot2::aes(
        x=data_names,
        y=loads
      ),
      fill = colors.mixed()[1]
    ) +

    ggplot2::labs(
      y='Carga'
    ) +

    ggplot2::geom_label(
      ggplot2::aes(
        x=data_names,
        y=loads,
        label=sprintf(
          "%.2f", loads
        )
      ),
      size = this$size$text/3
    )

  ggplot2::scale_y_continuous(labels = function(x) {sprintf("%.2f", x)})

  this = this %>% pcaviz.set_theme_column()

  return(this)
}

#' Add Title to PCA Visualization
#'
#' This function adds a title to a PCA visualization object, enhancing interpretability.
#'
#' @param this A \code{pcaviz} object representing the PCA visualization.
#' @param title A character string containing the title text to add to the visualization.
#'
#' @return The modified \code{pcaviz} object with the title added.
#'
#' @details
#' This function appends a specified title to an existing PCA plot, allowing for better context and understanding of the visualization. The title is typically displayed at the top of the plot.
#'
#' @examples
#' pca_viz <- pcaviz(pca_obj)
#' pca_viz <- pcaviz.add_title(pca_viz, title = "Principal Component Analysis of School Performance")
#'
#' @export
pcaviz.add_title = function(
  this, #: pcaviz
  title #: character
) {
  .pcaviz.check_class(this)
  type.check_character(title)

  this = this +
    ggplot2::labs(
      title = title
    )
  return(this)
}

#' Add ID Annotation to PCA Visualization
#'
#' This function adds an annotation displaying the ID metric of a PCA object
#' to a PCA visualization object.
#'
#' @param this An object of class 'pcaviz' that contains a PCA object.
#'
#' @return The modified 'pcaviz' object with the ID metric annotated on the plot.
#'
#' @details The ID metric is calculated using the `pca.get_ID()` function.
#' The annotation is placed near the top right corner of the plot based on
#' the ranges of the first two principal components (CP1 and CP2).
#'
#' @examples
#' # Assuming 'pca_viz' is an existing pcaviz object with a PCA analysis
#' pca_viz <- pcaviz.add_ID(pca_viz)
#'
#' @export
pcaviz.add_ID = function(
  this #: pcaviz
) {
  .pcaviz.check_class(this)

  id = this$pca_obj %>% pca.get_ID()

  cp1 = this$pca_obj$principal_components$CP1
  cp2 = this$pca_obj$principal_components$CP2

  range_cp1 = max(cp1) - min(cp1)
  range_cp2 = max(cp2) - min(cp2)

  x = min(cp1) + 0.9*range_cp1
  y = min(cp2) + 0.1*range_cp2

  this = this +
    ggplot2::annotate(
      'text',
      x=x,
      y=y,
      label=sprintf("ID: %.2f", id),
      size=this$size$text/3
    )

  return(this)
}

#' Add Single Group Points to PCA Scatter Plot
#'
#' This function adds points representing a single group to the PCA scatter plot.
#'
#' @param this A PCA visualization object of class 'pcaviz'.
#'
#' @return The modified PCA visualization object with added points.
#'
#' @export
pcaviz.add_single_group_points = function(
  this, #: pcaviz
  color = colors.mixes()[1]
) {
  .pcaviz.check_class(this)

  this = this +
    ggplot2::geom_point(
      data = this$pca_obj$principal_components,
      ggplot2::aes(
        x=CP1,
        y=CP2
      ),
      color=color,
      size=this$size$point_size
    )

  return(this)
}

#' Add Multi-Group Points to PCA Scatter Plot
#'
#' Adds points representing multiple groups to the PCA scatter plot.
#'
#' @param this A \code{pcaviz} object representing the PCA visualization.
#' @param groups A factor indicating group membership for each data point; length must match the number of rows in \code{this$pca_obj$principal_components}.
#' @param color_map An optional named or unnamed character vector of hexadecimal color codes. If provided, colors will be mapped to the levels of \code{groups}. Defaults to \code{NULL}.
#' @param shape_map An optional named or unnamed numeric vector of plotting symbol codes. Used only when \code{single_shape = FALSE}. If provided, shapes will be mapped to the levels of \code{groups}. Defaults to \code{NULL}.
#' @param single_shape Logical; if \code{TRUE}, all points use a single fixed shape (\code{shape = 19}) and only \code{color_map} is applied. Defaults to \code{FALSE}.
#'
#' @return A \code{pcaviz} object with the multi-group points layer added.
#'
#' @export
pcaviz.add_multi_group_points = function(
  this, #: pcaviz
  groups, #: factor
  color_map = NULL,
  shape_map = NULL,
  single_shape = FALSE
) {
  .pcaviz.check_class(this)
  this %>% .pcaviz.check_groups(groups)

  data = this$pca_obj$principal_components
  data$groups = groups

  if(single_shape) {
    this = this +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(
          x=CP1,
          y=CP2,
          color=groups
        ),
        size=this$size$point_size
      )
  } else {
    this = this +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(
          x=CP1,
          y=CP2,
          color=groups,
          shape=groups
        ),
        size=this$size$point_size
      )
  }

  if(!is.null(color_map)) {
    this = this +
      ggplot2::scale_color_manual(
        values = color_map
      )
  }

  if(!is.null(shape_map) && !single_shape) {
    this = this +
      ggplot2::scale_shape_manual(
        values = shape_map
      )
  }

  return(this)
}

#' Add Labels to Points in PCA Scatter Plot
#'
#' This function enhances a PCA scatter plot by adding text labels to the points,
#' allowing for easier identification and interpretation of the plotted data.
#'
#' @param this A \code{pcaviz} object representing the PCA visualization.
#' @param labels A character vector of labels corresponding to each point in the plot.
#' @param x An optional numeric vector specifying the x-coordinates of the points.
#'   If \code{NULL}, defaults to the first principal component (CP1).
#' @param y An optional numeric vector specifying the y-coordinates of the points.
#'   If \code{NULL}, defaults to the second principal component (CP2).
#' @param type A character string specifying the labeling style: \code{"text"} or \code{"label"}.
#' @param max.overlaps An integer setting the maximum number of overlapping labels allowed. Defaults to \code{20}.
#'
#' @return A modified \code{pcaviz} object with added labels.
#'
#' @details
#' The function validates that the length of \code{labels} matches the number of points
#' in the PCA plot. If \code{x} or \code{y} are \code{NULL}, they default to the PCA components.
#'
#' @export
pcaviz.add_labels = function(
  this, #: pcaviz
  labels, #: vector
  x = NULL, #: vector
  y = NULL, #: vector
  type = c('text', 'label'), #: character
  max.overlaps = 20
) {
  .pcaviz.check_class(this)
  type.check_character(type, 'type')

  type = type[1]
  if(!(type %in% c('text', 'label'))) {
    stop("'type' must be one of ('text', 'label'")
  }

  if((is.null(x) || is.null(y)) &&
     length(labels) != nrow(this$pca_obj$principal_components)
  ) {
    stop("'labels' must be a vector of the same length as the data")
  }

  if(is.null(x)) {
    x = this$pca_obj$principal_components$CP1
  }

  if(is.null(y)) {
    y = this$pca_obj$principal_components$CP2
  }

  if(length(labels) != length(x) ||
     length(labels) != length(y) ||
     length(x) != length(y)) {
    stop("all vectors must have the same length")
  }

  label_function = if(type == 'text') {
    ggrepel::geom_text_repel
  } else {
    ggrepel::geom_label_repel
  }

  this = this +
    label_function(
      mapping = ggplot2::aes(
        x = x,
        y = y,
        label = labels
      ),
      min.segment.length = 0,
      max.overlaps = max.overlaps,
      box.padding = this$size$point_size/2,
      size=this$size$text/4
    )

  return(this)
}

#' Set Theme for PCA Scatter Plot
#'
#' This function sets a minimal theme for the PCA scatter plot.
#'
#' @param this A PCA visualization object of class 'pcaviz'.
#'
#' @return The modified PCA visualization object with updated theme.
#'
#' @export
pcaviz.set_theme_scatter = function(
  this #: pcaviz
) {
  .pcaviz.check_class(this)

  this = this +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size=this$size$axis_title),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10, 0, 0, 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 10, 0, 0)),
      axis.text = ggplot2::element_text(size=this$size$text),
      plot.title = ggplot2::element_text(size=this$size$title, hjust=0.5),
      plot.subtitle = ggplot2::element_text(size=this$size$subtitle, hjust=0.5),
      plot.caption = ggplot2::element_text(
        size=this$size$text,
        hjust=0.5
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size=this$size$text)
    )
  return(this)
}

#' Set Theme for PCA Column Plot
#'
#' Applies a standardized theme to a PCA column plot, enhancing visual consistency across elements like borders, grid lines, axis titles, and text sizes.
#'
#' @param this A \code{pcaviz} object representing the PCA visualization.
#'
#' @return The modified \code{pcaviz} object with the custom theme applied.
#'
#' @details
#' This function is designed to streamline the appearance of PCA column plots, setting specific sizes for axis titles, text, plot title, subtitle, and caption. It also adjusts margins for axis labels and removes unnecessary elements like minor grid lines and legends, providing a cleaner, more focused visualization.
#'
#' @examples
#' pca_viz <- pcaviz(pca_obj)
#' pca_viz <- pcaviz.set_theme_column(pca_viz)
#'
#' @export
pcaviz.set_theme_column = function(
  this #: pcaviz
) {
  .pcaviz.check_class(this)

  this = this + ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size=this$size$axis_title),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 10, 0, 0)),
    axis.text = ggplot2::element_text(size=this$size$text),
    axis.ticks = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size=this$size$title, hjust=0.5),
    plot.subtitle = ggplot2::element_text(size=this$size$subtitle, hjust=0.5),
    plot.caption = ggplot2::element_text(
      size=this$size$text,
      hjust=0.5
    ),
    legend.position = 'none',
    legend.text = ggplot2::element_text(size=this$size$text)
  )

  return(this)
}

#' Set Color Scale for PCA Scatter Plot
#'
#' This function sets a manual color scale for the scatter plot in a PCA visualization,
#' allowing different groups to be visually distinguished.
#'
#' @param this An object of class 'pcaviz' that contains a PCA visualization.
#' @param groups A factor indicating the grouping of data points in the PCA scatter plot.
#'
#' @return The modified 'pcaviz' object with the specified color scale applied to the scatter plot.
#'
#' @details The function utilizes `scale_color_manual()` to assign colors to different
#' levels of the provided factor. The number of colors used corresponds to the number of
#' unique groups in the `groups` parameter.
#'
#' @examples
#' # Assuming 'pca_viz' is an existing pcaviz object and 'group_factor' is a factor
#' pca_viz <- pcaviz.set_scale_scatter(pca_viz, group_factor)
#'
#' @export
pcaviz.set_scale_scatter = function(
  this, #: pcaviz
  groups #: factor
) {
  .pcaviz.check_class(this)
  this %>% .pcaviz.check_groups(groups)

  this = this +
    ggplot2::scale_color_manual(
      values = colors.mixed()[1:length(unique(groups))]
    )

  return(this)
}

#' Add Largest Variations to PCA Scatter Plot
#'
#' This function identifies and visualizes the largest variations in a PCA scatter plot
#' by adding segments and labels for specified keys over a range of years.
#' It helps in understanding how certain variables have changed significantly
#' across the specified time frame, either positively or negatively.
#'
#' @param this A PCA visualization object of class 'pcaviz'.
#' @param number An integer specifying the number of largest variations to visualize.
#' @param keys A vector of keys (variables) for which the largest variations are to be identified.
#' @param years A vector of years corresponding to the data points being analyzed.
#' @param labels (Optional) A vector of labels for each key, used for annotation in the plot.
#'        If NULL, defaults to an empty string.
#' @param variation A character string indicating whether to visualize 'positive' or 'negative' variations.
#'
#' @return A modified PCA visualization object with segments and labels added for the largest variations.
#'
#' @details
#' The function first checks that the input object is valid. It then retrieves
#' the largest variations using the `pca.get_largest_variations` helper function.
#' The results are printed and subsequently visualized by adding segments and labels
#' to the PCA plot. The segments are colored based on whether they represent positive
#' or negative variations.
#'
#' @examples
#' # Assuming pca_viz is a valid PCA visualization object
#' pca_viz <- pcaviz.add_largest_variations(
#'   pca_viz,
#'   number = 5,
#'   keys = c("Variable1", "Variable2"),
#'   years = c(2020, 2021, 2022),
#'   variation = 'positive'
#' )
#'
#' @export
pcaviz.add_largest_variations = function(
  this, #: pcaviz
  number, #: integer
  keys, #: vector
  years, #: vector
  labels = NULL, #: vector
  variation = 'positive'
) {
  .pcaviz.check_class(this)
  if(!(variation %in% c('positive', 'negative'))) {
    stop("'variation' must be one of ('positive', 'negative')")
  }

  df = this$pca_obj %>% pca.get_largest_variations(
    number = number,
    keys = keys,
    years = years,
    labels = labels,
    variation
  )

  if(variation == 'positive'){
    color = ifelse(
      df$var > 0, colors.red_to_green()[4], colors.grayscale()[3]
    )
  } else {
    color = ifelse(
      df$var < 0, colors.red_to_green()[1], colors.grayscale()[3]
    )
  }

  unique_years = unique(years)

  this = this %>% pcaviz.add_segments(
    df$CP1.x, df$CP1.y, df$CP2.x, df$CP2.y,
    color=color
  )

  this = this %>% pcaviz.add_labels(
    labels = rep(df$labels, 2),
    x = c(df$CP1.x, df$CP1.y),
    y = c(df$CP2.x, df$CP2.y),
    type = 'label'
  )

  return(this)
}

#' Add Segments to PCA Scatter Plot
#'
#' This function adds line segments to a PCA scatter plot, connecting specified
#' points with arrows. This can be useful for visualizing relationships or
#' transitions between points in the PCA space.
#'
#' @param this A PCA visualization object of class 'pcaviz'.
#' @param from.x A numeric vector specifying the x-coordinates of the starting
#'        points of the segments.
#' @param to.x A numeric vector specifying the x-coordinates of the ending
#'        points of the segments.
#' @param from.y A numeric vector specifying the y-coordinates of the starting
#'        points of the segments.
#' @param to.y A numeric vector specifying the y-coordinates of the ending
#'        points of the segments.
#' @param color (Optional) A character string specifying the color of the segments.
#'        Defaults to the first color from a mixed color palette.
#'
#' @return A modified PCA visualization object with added segments.
#'
#' @details
#' The function checks that all input vectors are numeric and have the same length.
#' It creates segments using `geom_segment` from ggplot2, with arrows indicating
#' direction. The linewidth and arrow size are adjustable via the `this` object.
#'
#' @examples
#' # Assuming pca_viz is a valid PCA visualization object
#' pca_viz <- pcaviz.add_segments(
#'   pca_viz,
#'   from.x = c(1, 2),
#'   to.x = c(3, 4),
#'   from.y = c(1, 2),
#'   to.y = c(3, 4)
#' )
#'
#' @export
pcaviz.add_segments = function(
  this, #: pcaviz
  from.x, #: numeric vector
  to.x, #: numeric vector
  from.y, #: numeric vector
  to.y, #: numeric vector
  color = colors.mixed()[1]
) {
  .pcaviz.check_class(this)
  if(class(from.x) != 'numeric') {
    stop("'from' must be of type 'numeric'")
  }
  if(class(to.x) != 'numeric') {
    stop("'to' must be of type 'numeric'")
  }
  if(length(from.x) != length(to.x) ||
     length(from.y) != length(to.y) ||
     length(from.x) != length(from.y) ||
     length(to.x) != length(to.y)) {
    stop("all vectors must have the same length")
  }

  if(length(color) == 1) {
    color = rep(color, length(from.x))
  }

  segments_list <- lapply(1:length(from.x), function(i) {
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = from.x[i],
        xend = to.x[i],
        y = from.y[i],
        yend = to.y[i]
      ),
      arrow = ggplot2::arrow(
        type = "closed",
        length = ggplot2::unit(this$size$linewidth/10, "inches")
      ),
      linewidth=this$size$linewidth,
      color=color[i]
    )
  })

  for (segment in segments_list) {
    this <- this + segment
  }

  return(this)
}
