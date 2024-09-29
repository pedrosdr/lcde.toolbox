library(dplyr)

# class vizsize

# constructors

#' Create a Visualization Size Object
#'
#' This function creates a visualization size object based on specified size, line, and text categories.
#'
#' @param size A character string indicating the overall size category. Must be one of 'small', 'normal', or 'large'.
#' @param line A character string indicating the line size category. Must be one of 'small', 'normal', or 'large'.
#' @param text A character string indicating the text size category. Must be one of 'small', 'normal', or 'large'.
#'
#' @return An object of class 'vizsize' containing width, height, line width, point size, and text sizes.
#'
#' @examples
#' viz <- vizsize(size = 'normal', line = 'large', text = 'small')
#'
#' @export
vizsize = function(
  size = 'normal', #: character
  line = 'normal', #: character
  text = 'normal' #: character
) {
  obj = list()
  class(obj) = 'vizsize'

  if(size == 'small') {
    obj = obj %>% .vizsize.set_size_small()
  } else if(size == 'normal') {
    obj = obj %>% .vizsize.set_size_normal()
  } else if(size == 'large') {
    obj = obj %>% .vizsize.set_size_large()
  } else {
    stop("'size' must be one of ('small', 'normal', 'large')")
  }

  if(line == 'small') {
    obj = obj %>% .vizsize.set_line_small()
  } else if(size == 'normal') {
    obj = obj %>% .vizsize.set_line_normal()
  } else if(size == 'large') {
    obj = obj %>% .vizsize.set_line_large()
  } else {
    stop("'line' must be one of ('small', 'normal', 'large')")
  }

  if(text == 'small') {
    obj = obj %>% .vizsize.set_text_small()
  } else if(size == 'normal') {
    obj = obj %>% .vizsize.set_text_normal()
  } else if(size == 'large') {
    obj = obj %>% .vizsize.set_text_large()
  } else {
    stop("'text' must be one of ('small', 'normal', 'large')")
  }

  return(obj)
}

#' Parse Visualization Size Input
#'
#' This function parses input size specifications into a vizsize object.
#'
#' @param size An input that can be of type 'vizsize', character, or numeric.
#'
#' @return A vizsize object based on the input specifications.
#'
#' @examples
#' parsed_size <- vizsize.parse('large')
#'
#' @export
vizsize.parse = function(
    size #: vizsize | character | numeric
) {
  obj = NULL

  if(class(size) == 'vizsize') {
    obj = size
  } else if(size == 'small' || size == 1) {
    obj = vizsize('small', 'small', 'small')
  } else if(size == 'normal' || size == 2) {
    obj = vizsize('normal', 'normal', 'normal')
  } else if(size == 'large' || size == 3) {
    obj = vizsize('large', 'large', 'large')
  } else {
    stop("'size' must be of type 'vizsize' or one of
         (1, 2, 3, 'small', 'normal', 'large')")
  }

  return(obj)
}

# methods

#' Check Class of Visualization Size Object
#'
#' This function checks if the provided object is of class 'vizsize'.
#'
#' @param obj An object to be checked.
#'
#' @return NULL if the object is of class 'vizsize'; otherwise, an error is raised.
#'
#' @keywords internal
.vizsize.check_class = function(
    obj #; vizsize
) {
  if(!('vizsize' %in% class(obj))) {
    stop("'obj' must be of type 'vizsize'")
  }
}

#' Set Small Size Properties
#'
#' This function sets small dimensions for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small dimensions.
#'
#' @export
.vizsize.set_size_small = function(
  obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 1000
  obj$height = 621

  return(obj)
}

#' Set Normal Size Properties
#'
#' This function sets normal dimensions for the visualization size object.
#
#' @param obj A visualization size object of class 'vizsize'.
#
#' @return The modified visualization size object with normal dimensions.
#
#' @export
.vizsize.set_size_normal = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 1610
  obj$height = 1000

  return(obj)
}

#' Set Large Size Properties
#'
#' This function sets large dimensions for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with large dimensions.
#'
#' @export
.vizsize.set_size_large = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 2592
  obj$height = 1610

  return(obj)
}

#' Set Small Line Properties
#'
#' This function sets small line properties for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small line properties.
#'
#' @export
.vizsize.set_line_small = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 0.5
  obj$point_size = 0.5

  return(obj)
}

#' Set Normal Line Properties
#'
#' This function sets normal line properties for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with normal line properties.
#'
#' @export
.vizsize.set_line_normal = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 1
  obj$point_size = 1

  return(obj)
}

#' Set Large Line Properties
#'
#' This function sets large line properties for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with large line properties.
#'
#' @export
.vizsize.set_line_large = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 2
  obj$point_size = 2

  return(obj)
}

#' Set Small Text Properties
#'
#' This function sets small text properties for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small text properties.
#'
#' @export
.vizsize.set_text_small = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$text = 5
  obj$title = 10
  obj$subtitle = 8
  obj$axis_title = 8

  return(obj)
}

#' Set Normal Text Properties
#'
#' This function sets normal text properties for the visualization size object.
#'
#' @param obj A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with normal text properties.
#'
#' @export
.vizsize.set_text_normal = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$text = 10
  obj$title = 18
  obj$subtitle = 15
  obj$axis_title = 15

  return(obj)
}

#' Set Large Text Properties
#'
#' This function sets large text properties for the visualization size object.
#
#' @param obj A visualization size object of class 'vizsize'.
#
## @return The modified visualization size object with large text properties.
#
## @export
.vizsize.set_text_large = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$text = 20
  obj$title = 30
  obj$subtitle = 25
  obj$axis_title = 25

  return(obj)
}
