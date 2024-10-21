# class pptpos

# constructors
pptpos = function(
  n_rows, #: integer
  n_columns, #: integer
  row, #: integer
  column, #: integer
  width = 1, #: integer
  height = 1, #: integer
  margin = 0.1, #: numeric
  offset_top = 0.13, #: numeric
  offset_right = 0, #: numeric
  offset_bottom = 0, #: numeric
  offset_left = 0 #: numeric
) {
  type.check_integer(n_rows, 'n_rows')
  type.check_integer(n_columns, 'n_columns')
  type.check_integer(row, 'row')
  type.check_integer(column, 'column')
  type.check_integer(width, 'width')
  type.check_integer(height, 'height')
  type.check_numeric(margin, 'margin')
  type.check_numeric(offset_top, 'offset_top')
  type.check_numeric(offset_right, 'offset_right')
  type.check_numeric(offset_bottom, 'offset_bottom')
  type.check_numeric(offset_left, 'offset_left')

  this = list()
  class(this) = 'pptpos'

  row_height = (1 - offset_top - offset_bottom) / n_rows
  column_width = (1 - offset_left - offset_right) / n_columns

  this$top = offset_top + (row - 1 + margin) * row_height
  this$left = offset_left + (column - 1 + margin) * column_width
  this$width = (1 - 2 * margin) * column_width
  this$height = (1 - 2 * margin) * row_height

  return(this)
}

pptpos.title = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 1,
    column = 1,
    row = 1,
    offset_top = 0.03,
    offset_bottom = 0.87,
    margin = 0
  )

  return(this)
}

pptpos.center = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 1,
    column = 1,
    row = 1,
    offset_top = 0.13,
    margin = 0.05
  )

  return(this)
}

pptpos.left_half = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 2,
    column = 1,
    row = 1,
    offset_top = 0.13,
    offset_bottom = 0.15,
    margin = 0.03
  )

  return(this)
}

pptpos.right_half = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 2,
    column = 2,
    row = 1,
    offset_top = 0.13,
    offset_bottom = 0.15,
    margin = 0.05
  )

  return(this)
}

# methods
pptpos.parse = function(
  obj
) {
  this = NULL
  if('pptpos' %in% class(obj)) {
    this = obj
  } else if(obj == 'center') {
    this = pptpos.center()
  } else if(obj == 'left-half') {
    this = pptpos.left_half()
  } else if(obj == 'right-half') {
    this = pptpos.right_half()
  } else {
    stop(paste("Failed to parse 'obj'. Ensure it is of type 'pptpos' or one",
               "of the following character options: ('center', 'left-half',",
               " 'right-half')"))
  }

  return(this)
}

.pptpos.check_class = function(
  obj
) {
  if(!('pptpos' %in% class(obj))) {
    stop("'obj' must be of type 'pptpos'")
  }
}
