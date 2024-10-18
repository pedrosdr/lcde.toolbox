library(officer)

# class ppt

# constructors
ppt.from_template = function(
  template_path #: character
) {
  type.check_character(template_path, 'template_path')

  obj = officer::read_pptx(template_path)
  class(obj) = c(class(obj), 'ppt')

  obj = obj %>% ppt.set_slide(1)

  return(obj)
}

# methods
.ppt.check_class = function(
  obj
) {
  if(!('ppt' %in% class(obj))) {
    stop("'obj' must be of type 'ppt")
  }
}

ppt.set_slide = function(
  this, #: ppt
  page_number #: integer
) {
  type.check_integer(page_number, 'page_number')

  this = this %>%
    officer::on_slide(page_number)

  return(this)
}

ppt.new_slide = function(
  this #: ppt
) {
  .ppt.check_class(this)

  # this = this %>% officer::add_slide(
  #
  # )
}


ppt.set_layout = function(
  this, #: ppt
  layout #: character
) {
  .ppt.check_class(this)
  type.check_character(layout, 'layout')

  this$layout = layout

  return(this)
}

ppt.get_layout = function(
  this #: ppt
) {
  .ppt.check_class(this)

  if(!is.null(this$layout)) {
    return(this$layout)
  }

  layouts = officer::layout_summary(this)$layout
  layout = NULL

  if('Em Branco' %in% layouts) {
    layout = 'Em Branco'

  } else if ('Em branco' %in% layouts) {
    layout = 'Em branco'

  } else if ('Blank' %in% layouts) {
    layout = 'Blank'
  }

  if(is.null(layout)) {
    stop(paste("Unable to automatically set a layout. Please specify",
               "the layout manually using ppt.set_layout."))
  }

  return(layout)
}

ppt.save = function(
  this, #: ppt
  path #: character
) {
  .ppt.check_class(this)
  type.check_character(path, 'path')

  print(this$document, path)
}
