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
  this, #: ppt
  title = NULL #: character
) {
  .ppt.check_class(this)
  if(!is.null(title)) {
    type.check_character(title, 'title')
  }

  this = this %>% officer::add_slide(
    layout = this %>% ppt.get_layout,
    master = this %>% ppt.get_master
  )

  if(!is.null(title)){
    this = this %>%
      ppt.add_title(title)
  }

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

ppt.get_width = function(
  this #: ppt
) {
  .ppt.check_class(this)

  width = (this %>% officer::slide_size())$width

  return(width)
}

ppt.get_height = function(
    this #: ppt
) {
  .ppt.check_class(this)

  height = (this %>% officer::slide_size())$height

  return(height)
}

ppt.get_vertical_dimension = function(
    this, #: ppt
    relative_dimension #: numeric (0 - 1)
) {
  .ppt.check_class(this)
  type.check_numeric(relative_dimension, 'relative_dimension')

  height = this %>% ppt.get_height()

  return(relative_dimension * height)
}

ppt.get_horizontal_dimension = function(
    this, #: ppt
    relative_dimension #: numeric (0 - 1)
) {
  .ppt.check_class(this)
  type.check_numeric(relative_dimension, 'relative_dimension')

  width = this %>% ppt.get_width()

  return(relative_dimension * width)
}

ppt.add_title = function(
  this, #: ppt
  title #: character
) {
  .ppt.check_class(this)
  type.check_character(title, 'title')

  this = this %>%
    ppt.add_text(
      text = title,
      left = 0,
      top = 0.05,
      width = 1,
      height = 0.08,
      size = 28,
      bold = TRUE
    )

  return(this)
}

ppt.add_text = function(
  this, #: ppt
  text, #: character
  left, #: numeric (0 - 1)
  top, #: numeric (0 - 1)
  width, #: numeric (0 - 1)
  height, #: numeric (0 - 1)
  font = 'Calibri', #: character
  size = 18, #: numeric
  bold = FALSE #: logical
) {
  .ppt.check_class(this)
  type.check_character(text, 'text')
  type.check_character(font, 'font')
  type.check_numeric(size, 'size')
  type.check_logical(bold, 'bold')
  type.check_numeric(left, 'left')
  type.check_numeric(top, 'top')
  type.check_numeric(width, 'width')
  type.check_numeric(height, 'height')

  this = this %>%
    officer::ph_with(
      fpar(
        ftext(
          text,
          officer::fp_text(
            color = '#333333', font.family = 'Calibri', font.size = size, bold = TRUE
          )
        ),
        fp_p = officer::fp_par(text.align = 'center')
      ),
      officer::ph_location(
        left = this%>%ppt.get_horizontal_dimension(left),
        top = this%>%ppt.get_vertical_dimension(top),
        width = this%>%ppt.get_horizontal_dimension(width),
        height = this%>%ppt.get_vertical_dimension(height)
      )
    )

  return(this)
}

ppt.save = function(
  this, #: ppt
  path #: character
) {
  .ppt.check_class(this)
  type.check_character(path, 'path')

  print(this, path)
}
