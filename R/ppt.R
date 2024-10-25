library(officer)

# class ppt

# constructors
ppt.from_template = function(
  template_path #: character
) {
  type.check_character(template_path, 'template_path')

  obj = officer::read_pptx(template_path)
  class(obj) = c(class(obj), 'ppt')

  obj$current_index = obj %>% ppt.get_length()

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
  index #: integer
) {
  .ppt.check_class(this)
  type.check_integer(index, 'index')

  this = this %>% officer::on_slide(index)
  this$current_index = index

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

  this$current_index = this$current_index + 1

  this = this %>% ppt.move_slide(to=this$current_index)
  this = this %>% ppt.on_slide(this$current_index)

  if(!is.null(title)){
    this = this %>%
      ppt.add_title(title)
  }

  return(this)
}

ppt.add_transition = function(
  this, #: ppt
  text #: character
) {
  .ppt.check_class(this)
  type.check_character(text, 'text')

  this = this %>%
    ppt.new_slide() %>%
    ppt.add_text(
      text = text,
      size = 44,
      position = pptpos(
        n_columns = 1,
        n_rows = 3,
        column = 1,
        row = 2,
        width = 1,
        height = 1,
        offset_top = 0,
        offset_left = 0.1,
        offset_right = 0.1,
        margin = 0
      )
    )

  return(this)
}

ppt.move_slide = function(
  this, #: ppt
  index = NULL, #: integer
  to #: integer
) {
  .ppt.check_class(this)
  if(!is.null(index)) {
    type.check_integer(index, 'index')
  }
  type.check_integer(to, 'to')

  this = this %>%
    officer::move_slide(
      index = index,
      to = to
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

ppt.get_length = function(
  this #: ppt
) {
  .ppt.check_class(this)

  return(length(this))
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
      position = pptpos.title(),
      size = 28,
      bold = TRUE
    )

  return(this)
}

ppt.add_text = function(
  this, #: ppt
  text, #: character
  position, #: pptpos
  font = 'Calibri', #: character
  size = 18, #: numeric
  bold = FALSE #: logical
) {
  .ppt.check_class(this)
  type.check_character(text, 'text')
  type.check_character(font, 'font')
  type.check_numeric(size, 'size')
  type.check_logical(bold, 'bold')
  position = pptpos.parse(position)

  this = this %>%
    officer::ph_with(
      fpar(
        ftext(
          text,
          officer::fp_text(
            color = '#333333', font.family = font, font.size = size, bold = bold
          )
        ),
        fp_p = officer::fp_par(text.align = 'center')
      ),
      officer::ph_location(
        left = this%>%ppt.get_horizontal_dimension(position$left),
        top = this%>%ppt.get_vertical_dimension(position$top),
        width = this%>%ppt.get_horizontal_dimension(position$width),
        height = this%>%ppt.get_vertical_dimension(position$height)
      )
    )

  return(this)
}

ppt.add_object = function(
  this, #: ppt
  obj,
  position #: pptpos
) {
  .ppt.check_class(this)
  position = pptpos.parse(position)

  this = this %>%
    officer::ph_with(
      obj,
      location = officer::ph_location(
        left = this %>% ppt.get_horizontal_dimension(position$left),
        top = this %>% ppt.get_vertical_dimension(position$top),
        width = this %>% ppt.get_horizontal_dimension(position$width),
        height = this %>% ppt.get_vertical_dimension(position$height)
      )
    )

  return(this)
}

ppt.add_ggplot = function(
    this, #: ppt
    ggplot_obj, #: ggplot
    position #: pptpos
) {
  .ppt.check_class(this)
  type.check_ggplot(ggplot_obj, 'ggplot_obj')

  position = pptpos.parse(position)

  this = this %>%
    ppt.add_object(
      ggplot_obj,
      position
    )

  return(this)
}

ppt.add_table = function(
    this, #: ppt
    table_obj, #: table
    position, #: pptpos
    fit_height = TRUE
) {
  .ppt.check_class(this)
  type.check_table(table_obj, 'table_obj')

  position = pptpos.parse(position)

  width = this %>% ppt.get_horizontal_dimension(position$width)
  height = if(fit_height) {
    height = this %>% ppt.get_vertical_dimension(position$height)
  } else {
    NULL
  }

  table_obj = table_obj %>%
    table.fit_to_page(
      page_width = width,
      page_height = height
    )

  this = this %>%
    ppt.add_object(
      table_obj,
      position
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
