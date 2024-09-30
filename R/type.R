# class type

# methods
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

type.check_integer = function(
    obj,
    property=NULL #: character
) {
  if(is.null(property)) {
    property = 'property'
  }

  type.check_numeric(obj)

  if(round(obj, 0) != obj) {
    stop(paste0("'",property,"' must be an integer"))
  }
}
