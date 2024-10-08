library(dplyr)
library(ggplot2)

# class ggviz

# constructors
ggviz.from_ggplot = function(
  ggplot_obj #: ggplot
) {
  type.check_ggplot(ggplot_obj, 'ggplot_obj')

  obj = ggplot_obj
  class(obj) = c(class(obj), 'ggviz')

  return(obj)
}

ggviz.radar = function(
    data, #:data.frame
    colors = colors.mixed(), #: character
    labels = NULL, #: vector
    title = NULL, #: character
    show_score = FALSE, #: logical
    axes_to_invert = NULL,
    size = vizsize() #: vizsize | text | numeric
) {
  size = vizsize.parse(size)

  type.check_dataframe(data, 'data')
  type.check_character(colors, 'colors')
  type.check_logical(show_score, 'show_score')
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
    grid.label.size = size$text/3,
    axis.label.size = size$text/3,
    group.line.width = size$linewidth * 1.2,
    group.point.size = size$point_size * 1.2
  )

  if(!is.null(title)) {
    obj = obj +
      ggtitle(title)
  }

  if(show_score) {
    data_score = data[1,2:ncol(data)]
    if(!is.null(axes_to_invert)) {
        for(axis in axes_to_invert) {
          data_score[,axis] = 100 - data_score[,axis]
        }
    }

    score = stats.relative_magnitude(data_score)

    obj = obj +
      labs(
        caption = sprintf("Score: %.2f%%", 100*score)
      )
  }

  obj = obj +
    theme(
      plot.title = element_text(hjust = 0.5, size = size$title),
      plot.caption = element_text(
        hjust = 0.5, size = size$title,
        color = colors[1]
      )
    )

  obj = ggviz.from_ggplot(obj)

  return(obj)
}
