library(htmltools)

# class widget

# constructors
widget = function(
  html = NULL, #: character
  build_path = 'widget.html' #:character
) {
  if(!is.null(html)){
    type.check_character(html, 'html')
  }
  type.check_character(build_path, 'build_path')

  obj = list()
  class(obj) = c('widget')

  obj$html = html
  obj$build_path = build_path

  return(obj)
}

widget.geoleaf_legend = function(
    this, #: widget
    geoleaf_obj #: geoleaf
) {
  .widget.check_class(this)
  .geoleaf.check_class(geoleaf_obj)

  htmlwidgets::saveWidget(geoleaf_obj, this$build_path)
  this$html = readLines(this$build_path)

  return(this)
}

# methods
.widget.check_class = function(
  obj
) {
  if(!('widget' %in% class(obj))) {
    stop("'obj' must be of type 'widget'")
  }
}

widget.save = function(
  this, #: widget
  path, #: character
  verbose = TRUE #: logical
) {
  .widget.check_class(this)
  type.check_character(path)
  type.check_logical(verbose)

  if(is.null(this$html)) {
    warning("No content was saved because the 'html' field is NULL or empty.
            Please ensure that valid HTML content is provided.")
    return()
  }

  writeLines(this$html, path)
  if(verbose) {
    print(paste0("The HTML content has been successfully saved to '", path, "'."))
  }
}
