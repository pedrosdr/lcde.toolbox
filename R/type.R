# class type

#' type.check_character
#'
#' Checks if an object is of type character.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_character = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!inherits(obj, "character")) {
    stop(paste0("'",property,"' must be of type 'character'"))
  }
}

#' type.check_numeric
#'
#' Checks if an object is of type numeric.
#'
#' This function verifies that the provided object is classified as numeric.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_numeric = function(
  obj,
  property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!is.numeric(obj)) {
    stop(paste0("'",property,"' must be of type 'numeric'"))
  }
}

#' type.check_integer
#'
#' Checks if an object is of type integer.
#'
#' This function first checks if the object is numeric and then verifies
#' that it contains only integer values.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_integer = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  type.check_numeric(obj)

  if(round(obj, 0) != obj) {
    stop(paste0("'",property,"' must be an integer'"))
  }
}

#' type.check_sf
#'
#' Checks if an object is of class sf (Simple Features).
#'
#' This function verifies that the provided object is classified as an sf object.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_sf = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!inherits(obj, "sf")) {
    stop(paste0("'",property,"' must be of type 'sf'"))
  }
}

#' type.check_logical
#'
#' Checks if an object is of type logical.
#'
#' This function verifies that the provided object is classified as logical.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_logical = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!is.logical(obj)) {
    stop(paste0("'",property,"' must be of type 'logical'"))
  }
}

#' type.check_factor
#'
#' Checks if an object is of type factor.
#'
#' This function verifies that the provided object is classified as a factor.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_factor = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!is.factor(obj)) {
    stop(paste0("'",property,"' must be of type 'factor'"))
  }
}

#' type.check_dataframe
#'
#' Checks if an object is of type data frame.
#'
#' This function verifies that the provided object is classified as a data frame.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_dataframe = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!is.data.frame(obj)) {
    stop(paste0("'",property,"' must be of type 'data.frame'"))
  }
}

#' type.check_ggplot
#'
#' Checks if an object is of class ggplot.
#'
#' This function verifies that the provided object is a ggplot object.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_ggplot = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!inherits(obj, "ggplot")) {
    stop(paste0("'",property,"' must be of type 'ggplot'"))
  }
}

#' type.check_table
#'
#' Checks if an object is of type table.
#'
#' This function verifies that the provided object is classified as a table object.
#'
#' @param obj The object to check.
#' @param property Optional name of the property for error messages.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
#' @export
type.check_table = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  if(!inherits(obj, "table")) {
    stop(paste0("'",property,"' must be of type 'table'"))
  }
}


