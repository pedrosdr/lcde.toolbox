library(dplyr)
library(ggplot2)
library(ggrepel)

# class pcaviz

# constructors

#' Create a PCA Visualization Object from a ggplot Object
#'
#' This function converts a ggplot object into a PCA visualization object.
#'
#' @param ggplot_obj A ggplot object to be converted.
#'
#' @return An object of class 'pcaviz'.
#'
#' @examples
#' pca_viz <- .pcaviz.from_ggplot(ggplot())
#'
#' @export
.pcaviz.from_ggplot = function(
  ggplot_obj #: ggplot
) {
  if(!('ggplot' %in% class(ggplot_obj))) {
    stop("'ggplot_obj' must be of type 'ggplot'")
  }

  obj = ggplot_obj
  class(obj) = c(class(obj), 'pcaviz')
  return(obj)
}

# properties

#' Set Size Properties for PCA Visualization
#'
#' This function sets the size properties for a PCA visualization object.
#'
#' @param obj A PCA visualization object of class 'pcaviz'.
#' @param size_obj An object containing size properties (default is `vizsize()`).
#'
#' @return The modified PCA visualization object with updated size properties.
#'
#' @export
pcaviz.set_size = function(
  obj, #: pcaviz
  size_obj = vizsize() #: vizsize
) {
  .pcaviz.check_class(obj)

  obj$size = size_obj
  return(obj)
}

# methods

#' Check if an Object is of Class 'pcaviz'
#'
#' This function checks if the provided object is of class 'pcaviz'.
#'
#' @param obj An object to be checked.
#'
#' @return NULL if the object is of class 'pcaviz'; otherwise, an error is raised.
#'
#' @keywords internal
.pcaviz.check_class = function(
    obj #; pcaviz
) {
  if(!('pcaviz' %in% class(obj))) {
    stop("'obj' must be of type 'pcaviz'")
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
  size = vizsize() #: vizsize | text | numeric
) {
  .pca.check_class(pca_obj)
  size = vizsize.parse(size)

  obj = ggplot() %>% .pcaviz.from_ggplot()
  obj = obj %>% pcaviz.set_size(size)
  obj$pca_obj = pca_obj

  obj = obj + theme_minimal()

  if(is.null(groups)) {
    obj = obj %>% pcaviz.add_sigle_group_points()
  } else {
    obj = obj %>% pcaviz.add_multi_group_points(groups)
  }

  obj = obj + labs(
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
               linewidth=obj$size$linewidth,
               color=colors.grayscale()[5]) +
    geom_vline(xintercept = 0, linetype='dashed',
               linewidth=obj$size$linewidth,
               color=colors.grayscale()[5])

  if(!is.null(labels)) {
    obj = obj %>% pcaviz.add_labels(labels)
  }

  obj = obj %>% pcaviz.set_theme_scatter()

  return(obj)
}

#' Add Single Group Points to PCA Scatter Plot
#'
#' This function adds points representing a single group to the PCA scatter plot.
#'
#' @param obj A PCA visualization object of class 'pcaviz'.
#'
#' @return The modified PCA visualization object with added points.
#'
#' @export
pcaviz.add_single_group_points = function(
  obj #: pcaviz
) {
  .pcaviz.check_class(obj)

  obj = obj +
    geom_point(
      data = obj$pca_obj$principal_components,
      aes(
        x=CP1,
        y=CP2
      ),
      color=colors.mixed()[2],
      size=obj$size$point_size
    )

  return(obj)
}

#' Add Multi-Group Points to PCA Scatter Plot
#'
#' This function adds points representing multiple groups to the PCA scatter plot.
#'
#' @param obj A PCA visualization object of class 'pcaviz'.
#' @param groups A factor indicating group membership for each point.
#'
#' @return The modified PCA visualization object with added group points.
#'
#' @export
pcaviz.add_multi_group_points = function(
  obj, #: pcaviz
  groups #: factor
) {
  .pcaviz.check_class(obj)
  if(class(groups) != 'factor') {
    stop("'groups' must be of type 'factor'")
  }

  if(length(groups) != nrow(obj$pca_obj$principal_components)) {
    stop("'groups' must be a vector of the same length as the data")
  }

  data = obj$pca_obj$principal_components
  data$groups = groups

  obj = obj +
    geom_point(
      data = data,
      aes(
        x=CP1,
        y=CP2,
        color=groups
      ),
      size=obj$size$point_size
    )

  return(obj)
}

#' Add Labels to Points in PCA Scatter Plot
#'
#' This function adds text labels to points in the PCA scatter plot.
#
#' @param obj A PCA visualization object of class 'pcaviz'.
#' @param labels A vector of labels corresponding to each point in the plot.
#
#' @return The modified PCA visualization object with added labels.
#
#' @examples
#' pca_viz <- pcaviz.scatter(pca_obj, labels = c("Label1", "Label2", ...))
#
#' @export
pcaviz.add_labels = function(
  obj, #: pcaviz
  labels #: vector
) {
  .pcaviz.check_class(obj)

  if(length(labels) != nrow(obj$pca_obj$principal_components)) {
    stop("'labels' must be a vector of the same length as the data")
  }

  obj = obj +
    geom_text_repel(
      data = obj$pca_obj$principal_components,
      mapping = aes(
        x = CP1,
        y = CP2,
        label = labels
      ),
      size=obj$size$text/3
    )

  return(obj)
}

#' Set Theme for PCA Scatter Plot
#'
#' This function sets a minimal theme for the PCA scatter plot.
#'
#' @param obj A PCA visualization object of class 'pcaviz'.
#'
#' @return The modified PCA visualization object with updated theme.
#'
#' @export
pcaviz.set_theme_scatter = function(
  obj #: pcaviz
) {
  .pcaviz.check_class(obj)

  obj = obj +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size=obj$size$axis_title),
      axis.text = element_text(size=obj$size$text),
      plot.title = element_text(size=obj$size$title, hjust=0.5),
      plot.subtitle = element_text(size=obj$size$subtitle, hjust=0.5),
      plot.caption = element_text(
        size=obj$size$text,
        hjust=0.5
      )
    )
  return(obj)
}
