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

  if(!('character' %in% class(obj))) {
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

  if(!('numeric' %in% class(obj))) {
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

  if(!('sf' %in% class(obj))) {
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

  if(!('logical' %in% class(obj))) {
    stop(paste0("'",property,"' must be of type 'logical'"))
  }
}

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


