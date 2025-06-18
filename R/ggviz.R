# class ggviz

# constructors

#' ggviz Constructor from a ggplot Object
#'
#' Creates an object of class `ggviz` from a `ggplot` chart.
#'
#' @param ggplot_obj A `ggplot` object to be converted.
#'
#' @return An object of class `ggviz`.
#' @export
ggviz.from_ggplot = function(
  ggplot_obj #: ggplot
) {
  type.check_ggplot(ggplot_obj, 'ggplot_obj')

  obj = ggplot_obj
  class(obj) = c(class(obj), 'ggviz')

  return(obj)
}

#' ggviz Radar Chart Constructor
#'
#' Generates a radar chart using a data frame, with options for customizing
#' colors, titles, labels, and score display.
#'
#' @param data A `data.frame` containing the data for the radar chart.
#' @param colors A character vector defining the colors of the groups.
#' @param labels A vector with labels for the groups. If `NULL`, labels are generated automatically.
#' @param title The title of the chart. If `NULL`, no title is displayed.
#' @param show_score Logical. If `TRUE`, displays the relative score for the first group.
#' @param axes_to_invert A vector with the names of the axes to be inverted (optional).
#' @param opacity A numeric value between 0 and 1 defining the opacity of the group fill (default is 0.3).
#' @param size A `vizsize` object or numeric values to define the text and line sizes of the chart.
#'
#' @return A `ggviz` object containing the radar chart.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(Group1 = c(70, 80, 90), Group2 = c(50, 60, 80))
#' ggviz.radar(data, colors = c("#FF0000", "#00FF00"), opacity = 0.7)
#' }
ggviz.radar = function(
    data, #:data.frame
    colors = colors.mixed(), #: character
    labels = NULL, #: vector
    title = NULL, #: character
    show_score = FALSE, #: logical
    axes_to_invert = NULL,
    opacity = 0.3, # numeric
    size = vizsize() #: vizsize | text | numeric
) {
  size = vizsize.parse(size)

  type.check_dataframe(data, 'data')
  type.check_character(colors, 'colors')
  type.check_logical(show_score, 'show_score')
  type.check_numeric(opacity, 'opacity')
  .vizsize.check_class(size)

  if(show_score && nrow(data) > 1) {
    warning("'show_score' is only supported for single-group data; the score
            will be displayed only for the first row.")
  }

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
    fill.alpha = opacity,
    grid.label.size = size$text/3,
    axis.label.size = size$text/3,
    group.line.width = size$linewidth * 1.2,
    group.point.size = size$point_size * 1.2
  )

  if(!is.null(title)) {
    obj = obj +
      ggplot2::ggtitle(title)
  }

  if(show_score) {
    data_score = data[1,2:ncol(data)]
    if(!is.null(axes_to_invert)) {
        for(axis in axes_to_invert) {
          data_score[,axis] = 100 - data_score[,axis]
        }
    }

    score = utils.relative_magnitude(data_score)

    obj = obj +
      ggplot2::labs(
        caption = sprintf("Score: %.2f%%", 100*score)
      )
  }

  obj = obj +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = size$text),
      plot.caption = ggplot2::element_text(
        hjust = 0.5, size = size$text,
        color = colors[1]
      )
    )

  obj = ggviz.from_ggplot(obj)

  return(obj)
}
