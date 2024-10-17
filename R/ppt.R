library(officer)

# class ppt

# constructors
ppt.from_template = function(
  template_path #: character
) {
  type.check_character(template_path, 'template_path')

  obj = officer::read_pptx(template_path)
  class(obj) = c(class(obj), 'ppt')

  obj = obj %>% ppt.set_page(1)

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

ppt.set_page = function(
  this, #: ppt
  page_number #: integer
) {
  type.check_integer(page_number, 'page_number')

  this = this %>%
    officer::on_slide(page_number)

  return(this)
}

ppt.save = function(
  this, #: ppt
  path #: character
) {
  .ppt.check_class(this)
  type.check_character(path, 'path')

  print(this$document, path)
}
