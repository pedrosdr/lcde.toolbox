# class pptpos

# constructors

#' Create pptpos Object
#'
#' Initializes a `pptpos` object that defines the position and size of elements in a slide grid layout.
#'
#' @param n_rows An integer specifying the total number of rows in the slide layout.
#' @param n_columns An integer specifying the total number of columns in the slide layout.
#' @param row An integer indicating the row position of the element.
#' @param column An integer indicating the column position of the element.
#' @param width An integer specifying the number of columns spanned by the element. Defaults to 1.
#' @param height An integer specifying the number of rows spanned by the element. Defaults to 1.
#' @param margin A numeric value for the margin around the element, as a proportion of the slide size. Defaults to 0.1.
#' @param offset_top A numeric value for the offset from the top, as a proportion of the slide height. Defaults to 0.1.
#' @param offset_right A numeric value for the offset from the right. Defaults to 0.
#' @param offset_bottom A numeric value for the offset from the bottom. Defaults to 0.
#' @param offset_left A numeric value for the offset from the left. Defaults to 0.
#'
#' @return A `pptpos` object with computed top, left, width, and height properties.
#'
#' @export
pptpos = function(
  n_rows, #: integer
  n_columns, #: integer
  row, #: integer
  column, #: integer
  width = 1, #: integer
  height = 1, #: integer
  margin = 0.1, #: numeric
  offset_top = 0.1, #: numeric
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
  this$width = (1 - 2 * margin) * column_width * width
  this$height = (1 - 2 * margin) * row_height * height

  return(this)
}

#' Title Position
#'
#' Defines the position for a title element in a slide, taking up the top section.
#'
#' @return A `pptpos` object with coordinates for positioning a title.
#'
#' @export
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

#' Document Title Position
#'
#' Defines the position for a document title in the slide, occupying the top section with increased padding.
#'
#' @return A `pptpos` object with coordinates for positioning a document title.
#'
#' @export
pptpos.document_title = function() {
  this = pptpos(
    n_rows = 3,
    n_columns = 1,
    row = 1,
    column = 1,
    margin = 0,
    offset_top = 0.1,
    offset_right = 0.05,
    offset_bottom = 0.1,
    offset_left = 0.05
  )

  return(this)
}

#' Center Large Position
#'
#' Defines the position for a large element in the center of the slide.
#'
#' @return A `pptpos` object with coordinates for a centered large element.
#'
#' @export
pptpos.center_large = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 1,
    column = 1,
    row = 1,
    offset_top = 0.08,
    margin = 0.05
  )

  return(this)
}

#' Center Position
#'
#' Defines the position for a centered element in the slide with additional side margins.
#'
#' @return A `pptpos` object with coordinates for a centered element.
#'
#' @export
pptpos.center = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 1,
    column = 1,
    row = 1,
    offset_top = 0.1,
    offset_right = 0.07,
    offset_left = 0.07,
    margin = 0.05
  )

  return(this)
}

#' Left Half Position
#'
#' Defines the position for an element occupying the left half of the slide.
#'
#' @return A `pptpos` object with coordinates for positioning an element on the left half.
#'
#' @export
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

#' Wide Left Position
#'
#' Defines the position for a wide element occupying the left side of the slide.
#'
#' @return A `pptpos` object with coordinates for a wide left-aligned element.
#'
#' @export
pptpos.right_half = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 2,
    row = 1,
    column = 2,
    offset_top = 0.13,
    offset_bottom = 0.15,
    margin = 0.05
  )

  return(this)
}

#' Wide Left Position
#'
#' Defines the position for a wide element occupying the left side of the slide.
#'
#' @return A `pptpos` object with coordinates for a wide left-aligned element.
#'
#' @export
pptpos.wide_left = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 4,
    row = 1,
    column = 1,
    width = 3,
    offset_top = 0.13,
    offset_bottom = 0.05,
    offset_right = 0.01,
    offset_left = 0.01,
    margin = 0.02
  )

  return(this)
}

#' Wide Right Position
#'
#' Defines the position for a wide element occupying the right side of the slide.
#'
#' @return A `pptpos` object with coordinates for a wide right-aligned element.
#'
#' @export
pptpos.wide_right = function() {
  this = pptpos(
    n_rows = 1,
    n_columns = 4,
    column = 2,
    row = 1,
    width = 3,
    offset_top = 0.13,
    offset_bottom = 0.05,
    offset_right = 0.01,
    offset_left = 0.01,
    margin = 0.02
  )

  return(this)
}

#' Grid Position
#'
#' Defines a custom grid-based position for an element within the slide layout.
#'
#' @param n_rows An integer specifying the total number of rows in the layout grid.
#' @param n_columns An integer specifying the total number of columns in the layout grid.
#' @param row An integer indicating the row in which the element should be positioned.
#' @param column An integer indicating the column in which the element should be positioned.
#' @param margin A numeric value for the margin around the element. Defaults to 0.1.
#'
#' @return A `pptpos` object with calculated positions for the specified grid cell.
#'
#' @export
pptpos.grid = function(
  n_rows, #: integer
  n_columns, #: integer
  row, #: integer
  column, #: integer
  margin = 0.1
) {

  this = pptpos(
    n_rows = n_rows,
    n_columns = n_columns,
    row = row,
    column = column,
    width = 1,
    offset_top = 0.13,
    offset_bottom = 0.05,
    offset_right = 0.01,
    offset_left = 0.01,
    margin = margin
  )

  return(this)
}

# methods

#' Parse pptpos Object or Character Identifier
#'
#' Converts a `pptpos` object or character identifier into a `pptpos` position. Supported identifiers include
#' 'center', 'center-large', 'left-half', 'right-half', 'wide-left', 'wide-right', 'title', and 'document-title'.
#'
#' @param obj A `pptpos` object or character identifier specifying a position preset.
#'
#' @return A `pptpos` object corresponding to the specified position.
#'
#' @export
pptpos.parse = function(
  obj
) {
  this = NULL
  if('pptpos' %in% class(obj)) {
    this = obj
  } else if(obj == 'center') {
    this = pptpos.center()
  } else if(obj == 'center-large') {
    this = pptpos.center_large()
  } else if(obj == 'left-half') {
    this = pptpos.left_half()
  } else if(obj == 'right-half') {
    this = pptpos.right_half()
  } else if(obj == 'wide-left') {
    this = pptpos.wide_left()
  } else if(obj == 'wide-right') {
    this = pptpos.wide_right()
  } else if(obj == 'title') {
    this = pptpos.title()
  } else if(obj == 'document-title') {
    this = pptpos.document_title()
  } else {
    stop(paste("Failed to parse 'obj'. Ensure it is of type 'pptpos' or one",
               "of the following character options: ('center', 'center-large',",
               " 'left-half', 'right-half', 'wide-left', 'wide-right',",
               " 'title', 'document-title)"))
  }

  return(this)
}

#' Check pptpos Class
#'
#' Verifies if the provided object is of class `pptpos`.
#'
#' @param obj The object to check.
#'
#' @return NULL if the object is valid; otherwise, an error is raised.
.pptpos.check_class = function(
  obj
) {
  if(!('pptpos' %in% class(obj))) {
    stop("'obj' must be of type 'pptpos'")
  }
}
