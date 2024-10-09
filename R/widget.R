library(htmltools)

# class widget

# constructors
widget = function() {
  obj = list()
  class(obj) = 'widget'
  obj$html = NULL

  return(obj)
}

# methods
.widget.check_class = function(
  obj
) {
  if(!('widget' %in% class(obj))) {
    stop("'obj' must be of type 'widget'")
  }
}

widget.geoleaf_legend = function(
  this, #: widget
  geoleaf_obj, #: geoleaf
  build_path = "./" #: character
) {
  .widget.check_class(this)
  .geoleaf.check_class(geoleaf_obj)
  type.check_character(build_path)

  return(geoleaf_obj)
}
