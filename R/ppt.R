library(officer)

# class ppt

# constructors
ppt.from_template = function(
  template_path #: character
) {
  type.check_character(template_path, 'template_path')

  obj = officer::read_pptx(template_path)
  class(obj) = c(class(obj), 'ppt')

  obj = obj %>% ppt.on_slide(1)

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

ppt.on_slide = function(
  this, #: ppt
  page_number #: integer
) {
  type.check_integer(page_number, 'page_number')

  this = this %>% officer::on_slide(page_number)

  return(this)
}

ppt.new_slide = function(
  this #: ppt
) {
  .ppt.check_class(this)

  this = this %>% officer::add_slide(
    layout = this %>% ppt.get_layout,
    master = this %>% ppt.get_master
  )

  return(this)
}
ppt.get_layout = function(
  this #: ppt
) {
  .ppt.check_class(this)

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

ppt.get_master = function(
  this #: ppt
) {
  .ppt.check_class(this)

  masters = officer::layout_summary(this)$master
  master = NULL

  if('Tema do Office' %in% masters) {
    master = 'Tema do Office'

  } else if ('Office Theme' %in% masters) {
    master = 'Office Theme'
  }

  if(is.null(master)) {
    stop(paste("Unable to automatically set a layout. Please specify",
               "the layout manually using ppt.set_layout."))
  }

  return(master)
}

ppt.save = function(
  this, #: ppt
  path #: character
) {
  .ppt.check_class(this)
  type.check_character(path, 'path')

  print(this, path)
}
