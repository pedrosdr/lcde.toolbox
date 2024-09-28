library(dplyr)

# class vizsize

# constructors

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

.vizsize.check_class = function(
    obj #; vizsize
) {
  if(!('vizsize' %in% class(obj))) {
    stop("'obj' must be of type 'vizsize'")
  }
}

.vizsize.set_size_small = function(
  obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 1000
  obj$height = 621

  return(obj)
}

.vizsize.set_size_normal = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 1610
  obj$height = 1000

  return(obj)
}

.vizsize.set_size_large = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$width = 2592
  obj$height = 1610

  return(obj)
}

.vizsize.set_line_small = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 0.5
  obj$point_size = 0.5

  return(obj)
}

.vizsize.set_line_normal = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 1
  obj$point_size = 1

  return(obj)
}

.vizsize.set_line_large = function(
    obj #: vizsize
) {
  .vizsize.check_class(obj)

  obj$linewidth = 2
  obj$point_size = 2

  return(obj)
}

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
