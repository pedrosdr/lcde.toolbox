library(dplyr)
library(ggplot2)
library(ggrepel)

# class ggviz

# constructors

#' Create a PCA Visualization Object from a ggplot Object
#'
#' This function converts a ggplot object into a PCA visualization object
#' by associating it with a PCA analysis object.
#'
#' @param ggplot_obj A ggplot object that represents the PCA scatter plot.
#' @param pca_obj An object of class 'pca' containing the results of the PCA analysis.
#'
#' @return A 'ggviz' object that combines the ggplot object with the PCA analysis.
#'
#' @details This function checks if the provided `pca_obj` is of class 'pca'
#' and ensures that `ggplot_obj` is indeed a ggplot object. The resulting
#' object can be used for further customization and visualization.
#'
#' @examples
#' # Assuming 'ggplot_plot' is an existing ggplot object and 'pca_result' is a pca object
#' pca_viz <- ggviz.from_ggplot(ggplot_plot, pca_result)
#'
#' @export
ggviz.from_ggplot = function(
  ggplot_obj, #: ggplot
  pca_obj = NULL #: pca
) {
  if(!('ggplot' %in% class(ggplot_obj))) {
    stop("'ggplot_obj' must be of type 'ggplot'")
  }

  this = ggplot_obj
  class(this) = c(class(this), 'ggviz')

  if(!is.null(pca_obj)) {
    this = this %>% ggviz.set_pca_obj(pca_obj)
  }

  return(this)
}

# properties

#' Set Size Properties for PCA Visualization
#'
#' This function sets the size properties for a PCA visualization object.
#'
#' @param this A PCA visualization object of class 'ggviz'.
#' @param size_obj An object containing size properties (default is `vizsize()`).
#'
#' @return The modified PCA visualization object with updated size properties.
#'
#' @export
ggviz.set_size = function(
  this, #: ggviz
  size_obj = vizsize() #: vizsize | character | numeric
) {
  .ggviz.check_class(this)
  size_obj = vizsize.parse(size_obj)

  this$size = size_obj
  return(this)
}


ggviz.set_pca_obj = function(
  this, #: ggviz
  pca_obj #: pca_obj
) {
  .ggviz.check_class(this)
  .pca.check_class(pca_obj)

  this$pca_obj = pca_obj

  return(this)
}
# methods

#' Check if an Object is of Class 'ggviz'
#'
#' This function checks if the provided object is of class 'ggviz'.
#'
#' @param this An object to be checked.
#'
#' @return NULL if the object is of class 'ggviz'; otherwise, an error is raised.
#'
#' @keywords internal
.ggviz.check_class = function(
    obj
) {
  if(!('ggviz' %in% class(obj))) {
    stop("'obj' must be of type 'ggviz'")
  }
}

#' .ggviz.check_groups
#'
#' Validates the grouping variable for PCA visualization.
#'
#' This function checks that the `groups` parameter is a factor, has the same length as
#' the principal components data, and contains no more than 7 unique levels.
#'
#' @param this A \code{ggviz} object containing PCA results.
#' @param groups A factor representing the grouping variable.
#'
#' @return NULL if all checks pass; otherwise, an error is raised with a descriptive message.
#'
#' @details
#' This internal function ensures that the grouping variable is appropriate for PCA analysis,
#' helping to prevent errors from incompatible group definitions.
#'
#' @export
.ggviz.check_groups = function(
    this, #: ggviz
    groups #: factor
) {
  .ggviz.check_class(this)
  if(class(groups) != 'factor') {
    stop("'groups' must be of type 'factor'")
  }

  if(length(groups) != nrow(this$pca_obj$principal_components)) {
    stop("'groups' must be a vector of the same length as the data")
  }

  if(length(unique(groups)) > 7) {
    stop("'groups' can have a maximum of 7 levels.")
  }
}

#' Create a Scatter Plot for PCA Results
#'
#' This function generates a scatter plot of the principal components from a PCA object.
#'
#' @param pca_obj A PCA object containing principal components.
#' @param labels Optional labels for the points (default is NULL).
#' @param groups Optional grouping factor for coloring points (default is NULL).
#' @param size Size properties for the plot (default is `vizsize()`).
#'
#' @return A ggplot object representing the PCA scatter plot.
#'
#' @export
ggviz.scatter = function(
  pca_obj, #: pca
  labels = NULL, #: vector
  groups = NULL, #: factor
  include_ID = TRUE, #: logical
  size = vizsize() #: vizsize | text | numeric
) {
  .pca.check_class(pca_obj)
  if(class(include_ID) != 'logical') {
    stop("'include_ID' must be of type 'logical'")
  }

  this = ggviz.from_ggplot(ggplot(), pca_obj)
  if(!is.null(groups)) {
    this %>% .ggviz.check_groups(groups)
  }

  this = this %>% ggviz.set_size(size)

  this = this + theme_minimal()

  if(is.null(groups)) {
    this = this %>% ggviz.add_single_group_points()
  } else {
    this = this %>% ggviz.add_multi_group_points(groups)
  }

  this = this + labs(
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

      caption = paste0(pca_obj$component_equations[1],
                       '\n',
                       pca_obj$component_equations[2])
    ) +

    geom_hline(yintercept = 0, linetype='dashed',
               linewidth=this$size$linewidth,
               color=colors.grayscale()[5]) +
    geom_vline(xintercept = 0, linetype='dashed',
               linewidth=this$size$linewidth,
               color=colors.grayscale()[5])

  if(include_ID) {
    this = this %>% ggviz.add_ID()
  }

  if(!is.null(labels)) {
    this = this %>% ggviz.add_labels(labels)
  }

  if(!is.null(groups)) {
    this = this %>% ggviz.set_scale_scatter(groups)
  }

  this = this %>% ggviz.set_theme_scatter()

  return(this)
}

ggviz.radar = function(
    data, #:data.frame
    colors = colors.mixed(), #: character
    labels = NULL, #: vector
    title = NULL, #: character
    size = vizsize() #: vizsize | text | numeric
) {
  type.check_dataframe(data, 'data')
  type.check_character(colors, 'colors')
  .vizsize.check_class(size)

  if(is.null(labels)) {
    labels = paste0('Group ', 1:nrow(data))
  }

  if(length(labels) != nrow(data)) {
    stop("'labels' must be a vector of the same length as data")
  }

  data$labels = labels
  data = data[,c(ncol(data), 1:(ncol(data)-1))]

  obj = ggradar(
    data,
    label.gridlines.show = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    grid.max = 100,
    grid.n5 = 80,
    grid.n4 = 60,
    grid.n3 = 40,
    grid.n2 = 20,
    grid.min = 0,
    group.colours = colors,
    fill = TRUE,
    grid.label.size = size$text/3,
    axis.label.size = size$text/3,
    group.line.width = size$linewidth,
    group.point.size = size$point_size
  )

  if(!is.null(title)) {
    obj = obj +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = size$title))
  }

  obj = ggviz.from_ggplot(obj)

  return(obj)
}

#' ggviz.explained_variance
#'
#' Generates a bar plot showing the percentage of explained variance for each PCA component.
#'
#' This function visualizes the proportion of variance explained by each principal component
#' in a PCA analysis. It creates a bar plot where the height of each bar represents
#' the percentage of variance explained by the corresponding component.
#'
#' @param pca_obj An object of class `pca`, which contains PCA results including
#'                explained variance and component names. The object must have a valid
#'                structure as expected by the ggviz package.
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
#' my_plot <- ggviz.explained_variance(pca_obj)
#' print(my_plot)
#'
#' @export
ggviz.explained_variance = function(
  pca_obj, #: pca
  size = vizsize() #: vizsize
) {
  .pca.check_class(pca_obj)
  this = ggviz.from_ggplot(ggplot(), pca_obj)

  this = this %>% ggviz.set_size(size)

  this = this +
    theme_light() +

    geom_col(
      aes(
        x=this$pca_obj$component_names,
        y=this$pca_obj$explained_variance
      ),
      fill=colors.mixed()[2]
    ) +

    labs(
      x='Componente Principal',
      y='% de Variância Explicada'
    ) +

    geom_label(
      aes(
        x=this$pca_obj$component_names,
        y=this$pca_obj$explained_variance,
        label=sprintf(
          "%.2f%%", 100*this$pca_obj$explained_variance
        )
      ),
      size = this$size$text/3
    ) +

    scale_y_continuous(labels = function(x) {sprintf("%.2f%%", 100*x)})

  this = this %>% ggviz.set_theme_column()

  return(this)
}

#' ggviz.component_loads
#'
#' Generates a bar plot of PCA component loads using ggplot2.
#'
#' This function visualizes the loadings of indicators on a specified PCA component.
#' It creates a bar plot where the height of the bars represents the loadings,
#' with colors indicating whether the load is positive or negative.
#'
#' @param pca_obj An object of class `pca`, which contains the PCA results and loadings.
#'                The object must have a valid structure as expected by the ggviz package.
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
#' my_plot <- ggviz.component_loads(pca_obj, component = 1)
#' print(my_plot)
#'
#' @export
ggviz.component_loads = function(
    pca_obj, #: pca
    component, #: numeric integer
    size = vizsize() #: vizsize
) {
  .pca.check_class(pca_obj)

  this = ggviz.from_ggplot(ggplot(), pca_obj)
  this = this %>% ggviz.set_size(size)

  type.check_integer(component)

  if(component > pca_obj$length || component < 1) {
    stop("'component' must correspond to a valid component")
  }

  loads = this$pca_obj$loads[,component]
  data_names = rownames(this$pca_obj$loads)
  colors = ifelse(
    loads < 0, 'neg', 'pos'
  )
  colors = as.factor(colors)

  this = this +
    theme_light() +

    geom_col(
      aes(
        x=data_names,
        y=loads,
        fill = colors
      )
    ) +

    labs(
      y='Carga'
    ) +

    geom_label(
      aes(
        x=data_names,
        y=loads,
        label=sprintf(
          "%.2f", loads
        )
      ),
      size = this$size$text/3
    ) +

    scale_fill_manual(
      values=c(
        'neg' = colors.red_to_green()[1],
        'pos' = colors.red_to_green()[4]
      )
    ) +

    scale_y_continuous(labels = function(x) {sprintf("%.2f", x)})

  this = this %>% ggviz.set_theme_column()

  return(this)
}

ggviz.add_title = function(
  this, #: ggviz
  title #: character
) {
  .ggviz.check_class(this)
  type.check_character(title)

  this = this +
    labs(
      title = title
    )
  return(this)
}

#' Add ID Annotation to PCA Visualization
#'
#' This function adds an annotation displaying the ID metric of a PCA object
#' to a PCA visualization object.
#'
#' @param this An object of class 'ggviz' that contains a PCA object.
#'
#' @return The modified 'ggviz' object with the ID metric annotated on the plot.
#'
#' @details The ID metric is calculated using the `pca.get_ID()` function.
#' The annotation is placed near the top right corner of the plot based on
#' the ranges of the first two principal components (CP1 and CP2).
#'
#' @examples
#' # Assuming 'pca_viz' is an existing ggviz object with a PCA analysis
#' pca_viz <- ggviz.add_ID(pca_viz)
#'
#' @export
ggviz.add_ID = function(
  this #: ggviz
) {
  .ggviz.check_class(this)

  id = this$pca_obj %>% pca.get_ID()

  cp1 = this$pca_obj$principal_components$CP1
  cp2 = this$pca_obj$principal_components$CP2

  range_cp1 = max(cp1) - min(cp1)
  range_cp2 = max(cp2) - min(cp2)

  x = min(cp1) + 0.9*range_cp1
  y = min(cp2) + 0.1*range_cp2

  this = this +
    annotate(
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
#' @param this A PCA visualization object of class 'ggviz'.
#'
#' @return The modified PCA visualization object with added points.
#'
#' @export
ggviz.add_single_group_points = function(
  this #: ggviz
) {
  .ggviz.check_class(this)

  this = this +
    geom_point(
      data = this$pca_obj$principal_components,
      aes(
        x=CP1,
        y=CP2
      ),
      color=colors.mixed()[2],
      size=this$size$point_size
    )

  return(this)
}

#' Add Multi-Group Points to PCA Scatter Plot
#'
#' This function adds points representing multiple groups to the PCA scatter plot.
#'
#' @param this A PCA visualization object of class 'ggviz'.
#' @param groups A factor indicating group membership for each point.
#'
#' @return The modified PCA visualization object with added group points.
#'
#' @export
ggviz.add_multi_group_points = function(
  this, #: ggviz
  groups #: factor
) {
  .ggviz.check_class(this)
  this %>% .ggviz.check_groups(groups)

  data = this$pca_obj$principal_components
  data$groups = groups

  this = this +
    geom_point(
      data = data,
      aes(
        x=CP1,
        y=CP2,
        color=groups,
        shape=groups
      ),
      size=this$size$point_size
    )

  return(this)
}

#' Add Labels to Points in PCA Scatter Plot
#'
#' This function enhances a PCA scatter plot by adding text labels to the points,
#' allowing for easier identification and interpretation of the plotted data.
#'
#' @param this A PCA visualization object of class 'ggviz'.
#' @param labels A character vector of labels corresponding to each point in the plot.
#' @param x (Optional) A numeric vector specifying the x-coordinates of the points.
#'           If NULL, defaults to the first principal component (CP1).
#' @param y (Optional) A numeric vector specifying the y-coordinates of the points.
#'           If NULL, defaults to the second principal component (CP2).
#'
#' @return A modified PCA visualization object with added labels.
#'
#' @details
#' The function checks that the length of `labels` matches the number of points
#' in the PCA plot. If `x` and `y` are not provided, it will automatically use
#' the first two principal components from the PCA object.
#'
#' @examples
#' # Assuming pca_obj is a valid PCA object
#' pca_viz <- ggviz.scatter(pca_obj)
#' pca_viz <- ggviz.add_labels(pca_viz, labels = c("Label1", "Label2", "Label3"))
#'
#' @export
ggviz.add_labels = function(
  this, #: ggviz
  labels, #: vector
  x = NULL, #: vector
  y = NULL #: vector
) {
  .ggviz.check_class(this)

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

  this = this +
    geom_text_repel(
      mapping = aes(
        x = x,
        y = y,
        label = labels
      ),
      size=this$size$text/3
    )

  return(this)
}

#' Set Theme for PCA Scatter Plot
#'
#' This function sets a minimal theme for the PCA scatter plot.
#'
#' @param this A PCA visualization object of class 'ggviz'.
#'
#' @return The modified PCA visualization object with updated theme.
#'
#' @export
ggviz.set_theme_scatter = function(
  this #: ggviz
) {
  .ggviz.check_class(this)

  this = this +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size=this$size$axis_title),
      axis.title.x = element_text(margin = ggplot2::margin(10, 0, 0, 0)),
      axis.title.y = element_text(margin = ggplot2::margin(0, 10, 0, 0)),
      axis.text = element_text(size=this$size$text),
      plot.title = element_text(size=this$size$title, hjust=0.5),
      plot.subtitle = element_text(size=this$size$subtitle, hjust=0.5),
      plot.caption = element_text(
        size=this$size$text,
        hjust=0.5
      ),
      legend.title = element_blank(),
      legend.text = element_text(size=this$size$text)
    )
  return(this)
}

ggviz.set_theme_column = function(
  this #: ggviz
) {
  .ggviz.check_class(this)

  this = this + theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size=this$size$axis_title),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = ggplot2::margin(0, 10, 0, 0)),
    axis.text = element_text(size=this$size$text),
    axis.ticks = element_blank(),
    plot.title = element_text(size=this$size$title, hjust=0.5),
    plot.subtitle = element_text(size=this$size$subtitle, hjust=0.5),
    plot.caption = element_text(
      size=this$size$text,
      hjust=0.5
    ),
    legend.position = 'none',
    legend.text = element_text(size=this$size$text)
  )

  return(this)
}

#' Set Color Scale for PCA Scatter Plot
#'
#' This function sets a manual color scale for the scatter plot in a PCA visualization,
#' allowing different groups to be visually distinguished.
#'
#' @param this An object of class 'ggviz' that contains a PCA visualization.
#' @param groups A factor indicating the grouping of data points in the PCA scatter plot.
#'
#' @return The modified 'ggviz' object with the specified color scale applied to the scatter plot.
#'
#' @details The function utilizes `scale_color_manual()` to assign colors to different
#' levels of the provided factor. The number of colors used corresponds to the number of
#' unique groups in the `groups` parameter.
#'
#' @examples
#' # Assuming 'pca_viz' is an existing ggviz object and 'group_factor' is a factor
#' pca_viz <- ggviz.set_scale_scatter(pca_viz, group_factor)
#'
#' @export
ggviz.set_scale_scatter = function(
  this, #: ggviz
  groups #: factor
) {
  .ggviz.check_class(this)
  this %>% .ggviz.check_groups(groups)

  this = this +
    scale_color_manual(
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
#' @param this A PCA visualization object of class 'ggviz'.
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
#' pca_viz <- ggviz.add_largest_variations(
#'   pca_viz,
#'   number = 5,
#'   keys = c("Variable1", "Variable2"),
#'   years = c(2020, 2021, 2022),
#'   variation = 'positive'
#' )
#'
#' @export
ggviz.add_largest_variations = function(
  this, #: ggviz
  number, #: integer
  keys, #: vector
  years, #: vector
  labels = NULL, #: vector
  variation = 'positive'
) {
  .ggviz.check_class(this)
  if(!(variation %in% c('positive', 'negative'))) {
    stop("'variation' must be one of ('positive', 'negative')")
  }

  color = if(variation == 'positive'){
    colors.red_to_green()[4]
  } else {
    colors.red_to_green()[1]
  }

  df = this$pca_obj %>% pca.get_largest_variations(
    number = number,
    keys = keys,
    years = years,
    labels = labels,
    variation
  )

  unique_years = unique(years)

  this = this %>% ggviz.add_segments(
    df$CP1.x, df$CP1.y, df$CP2.x, df$CP2.y,
    color=color
  )

  this = this %>% ggviz.add_labels(
    labels = rep(df$labels, 2),
    x = c(df$CP1.x, df$CP1.y),
    y = c(df$CP2.x, df$CP2.y)
  )

  return(this)
}

#' Add Segments to PCA Scatter Plot
#'
#' This function adds line segments to a PCA scatter plot, connecting specified
#' points with arrows. This can be useful for visualizing relationships or
#' transitions between points in the PCA space.
#'
#' @param this A PCA visualization object of class 'ggviz'.
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
#' pca_viz <- ggviz.add_segments(
#'   pca_viz,
#'   from.x = c(1, 2),
#'   to.x = c(3, 4),
#'   from.y = c(1, 2),
#'   to.y = c(3, 4)
#' )
#'
#' @export
ggviz.add_segments = function(
  this, #: ggviz
  from.x, #: numeric vector
  to.x, #: numeric vector
  from.y, #: numeric vector
  to.y, #: numeric vector
  color = colors.mixed()[1]
) {
  .ggviz.check_class(this)
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

  segments_list <- lapply(1:length(from.x), function(i) {
    geom_segment(
      mapping = aes(
        x = from.x[i],
        xend = to.x[i],
        y = from.y[i],
        yend = to.y[i]
      ),
      arrow = arrow(
        type = "closed",
        length = unit(this$size$linewidth/10, "inches")
      ),
      linewidth=this$size$linewidth,
      color=color
    )
  })

  for (segment in segments_list) {
    this <- this + segment
  }

  return(this)
}
