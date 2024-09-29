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
  this = list()
  class(this) = 'vizsize'

  if(size == 'small') {
    this = this %>% .vizsize.set_size_small()
  } else if(size == 'normal') {
    this = this %>% .vizsize.set_size_normal()
  } else if(size == 'large') {
    this = this %>% .vizsize.set_size_large()
  } else {
    stop("'size' must be one of ('small', 'normal', 'large')")
  }

  if(line == 'small') {
    this = this %>% .vizsize.set_line_small()
  } else if(size == 'normal') {
    this = this %>% .vizsize.set_line_normal()
  } else if(size == 'large') {
    this = this %>% .vizsize.set_line_large()
  } else {
    stop("'line' must be one of ('small', 'normal', 'large')")
  }

  if(text == 'small') {
    this = this %>% .vizsize.set_text_small()
  } else if(size == 'normal') {
    this = this %>% .vizsize.set_text_normal()
  } else if(size == 'large') {
    this = this %>% .vizsize.set_text_large()
  } else {
    stop("'text' must be one of ('small', 'normal', 'large')")
  }

  return(this)
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
  this = NULL

  if(class(size) == 'vizsize') {
    this = size
  } else if(size == 'small' || size == 1) {
    this = vizsize('small', 'small', 'small')
  } else if(size == 'normal' || size == 2) {
    this = vizsize('normal', 'normal', 'normal')
  } else if(size == 'large' || size == 3) {
    this = vizsize('large', 'large', 'large')
  } else {
    stop("'size' must be of type 'vizsize' or one of
         (1, 2, 3, 'small', 'normal', 'large')")
  }

  return(this)
}

# methods

#' Check Class of Visualization Size Object
#'
#' This function checks if the provided object is of class 'vizsize'.
#'
#' @param this An object to be checked.
#'
#' @return NULL if the object is of class 'vizsize'; otherwise, an error is raised.
#'
#' @keywords internal
.vizsize.check_class = function(
    this #; vizsize
) {
  if(!('vizsize' %in% class(this))) {
    stop("'this' must be of type 'vizsize'")
  }
}

#' Set Small Size Properties
#'
#' This function sets small dimensions for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small dimensions.
#'
#' @export
.vizsize.set_size_small = function(
  this #: vizsize
) {
  .vizsize.check_class(this)

  this$width = 1000
  this$height = 621

  return(this)
}

#' Set Normal Size Properties
#'
#' This function sets normal dimensions for the visualization size object.
#
#' @param this A visualization size object of class 'vizsize'.
#
#' @return The modified visualization size object with normal dimensions.
#
#' @export
.vizsize.set_size_normal = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$width = 1610
  this$height = 1000

  return(this)
}

#' Set Large Size Properties
#'
#' This function sets large dimensions for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with large dimensions.
#'
#' @export
.vizsize.set_size_large = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$width = 2592
  this$height = 1610

  return(this)
}

#' Set Small Line Properties
#'
#' This function sets small line properties for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small line properties.
#'
#' @export
.vizsize.set_line_small = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$linewidth = 0.5
  this$point_size = 1

  return(this)
}

#' Set Normal Line Properties
#'
#' This function sets normal line properties for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with normal line properties.
#'
#' @export
.vizsize.set_line_normal = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$linewidth = 1
  this$point_size = 3

  return(this)
}

#' Set Large Line Properties
#'
#' This function sets large line properties for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with large line properties.
#'
#' @export
.vizsize.set_line_large = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$linewidth = 2
  this$point_size = 5

  return(this)
}

#' Set Small Text Properties
#'
#' This function sets small text properties for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with small text properties.
#'
#' @export
.vizsize.set_text_small = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$text = 5
  this$title = 10
  this$subtitle = 8
  this$axis_title = 8

  return(this)
}

#' Set Normal Text Properties
#'
#' This function sets normal text properties for the visualization size object.
#'
#' @param this A visualization size object of class 'vizsize'.
#'
#' @return The modified visualization size object with normal text properties.
#'
#' @export
.vizsize.set_text_normal = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$text = 15
  this$title = 20
  this$subtitle = 17
  this$axis_title = 17

  return(this)
}

#' Set Large Text Properties
#'
#' This function sets large text properties for the visualization size object.
#
#' @param this A visualization size object of class 'vizsize'.
#
## @return The modified visualization size object with large text properties.
#
## @export
.vizsize.set_text_large = function(
    this #: vizsize
) {
  .vizsize.check_class(this)

  this$text = 20
  this$title = 30
  this$subtitle = 25
  this$axis_title = 25

  return(this)
}
