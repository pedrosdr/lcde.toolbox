library(dplyr)
library(ggplot2)
library(ggrepel)

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
  pca_obj #: pca
) {
  .pca.check_class(pca_obj)
  if(!('ggplot' %in% class(ggplot_obj))) {
    stop("'ggplot_obj' must be of type 'ggplot'")
  }

  this = ggplot_obj
  this$pca_obj = pca_obj
  class(this) = c(class(this), 'pcaviz')
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
  size_obj = vizsize() #: vizsize
) {
  .pcaviz.check_class(this)

  this$size = size_obj
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
    this #: pcaviz
) {
  if(!('pcaviz' %in% class(this))) {
    stop("'this' must be of type 'pcaviz'")
  }
}

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
pcaviz.scatter = function(
  pca_obj, #: pca
  labels = NULL, #: vector
  groups = NULL, #: factor
  include_ID = FALSE, #: logical
  size = vizsize() #: vizsize | text | numeric
) {
  .pca.check_class(pca_obj)
  if(class(include_ID) != 'logical') {
    stop("'include_ID' must be of type 'logical'")
  }

  this = pcaviz.from_ggplot(ggplot(), pca_obj)
  this %>% .pcaviz.check_groups(groups)

  size = vizsize.parse(size)
  this = this %>% pcaviz.set_size(size)

  this = this + theme_minimal()

  if(is.null(groups)) {
    this = this %>% pcaviz.add_sigle_group_points()
  } else {
    this = this %>% pcaviz.add_multi_group_points(groups)
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
    this = this %>% pcaviz.add_ID()
  }

  if(!is.null(labels)) {
    this = this %>% pcaviz.add_labels(labels)
  }

  if(!is.null(groups)) {
    this = this %>% pcaviz.set_scale_scatter(groups)
  }

  this = this %>% pcaviz.set_theme_scatter()

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
    annotate(
      'text',
      x=x,
      y=y,
      label=sprintf("ID: %.2f%%", 100*id),
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
  this #: pcaviz
) {
  .pcaviz.check_class(this)

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
#' @param this A PCA visualization object of class 'pcaviz'.
#' @param groups A factor indicating group membership for each point.
#'
#' @return The modified PCA visualization object with added group points.
#'
#' @export
pcaviz.add_multi_group_points = function(
  this, #: pcaviz
  groups #: factor
) {
  .pcaviz.check_class(this)
  this %>% .pcaviz.check_groups(groups)

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
#' This function adds text labels to points in the PCA scatter plot.
#
#' @param this A PCA visualization object of class 'pcaviz'.
#' @param labels A vector of labels corresponding to each point in the plot.
#
#' @return The modified PCA visualization object with added labels.
#
#' @examples
#' pca_viz <- pcaviz.scatter(pca_obj, labels = c("Label1", "Label2", ...))
#
#' @export
pcaviz.add_labels = function(
  this, #: pcaviz
  labels #: vector
) {
  .pcaviz.check_class(this)

  if(length(labels) != nrow(this$pca_obj$principal_components)) {
    stop("'labels' must be a vector of the same length as the data")
  }

  this = this +
    geom_text_repel(
      data = this$pca_obj$principal_components,
      mapping = aes(
        x = CP1,
        y = CP2,
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
    scale_color_manual(
      values = colors.mixed()[1:length(unique(groups))]
    )

  return(this)
}
