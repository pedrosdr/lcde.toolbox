# class ppt

# constructors

#' Create a PPT Object from Template
#'
#' Initializes a `ppt` object from a specified PowerPoint template file.
#'
#' @param template_path A character string specifying the path to the template PowerPoint file.
#'
#' @return A `ppt` object with the loaded template.
#'
#' @export
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

#' Check PPT Class
#'
#' Verifies if the provided object is of class `ppt`.
#'
#' @param obj The object to be checked.
#'
#' @return NULL if the object is valid; otherwise, an error is raised.
.ppt.check_class = function(
  obj
) {
  if(!inherits(obj, "ppt")) {
    stop("'obj' must be of type 'ppt")
  }
}

#' Go to Slide
#'
#' Sets the specified slide as the current slide in the `ppt` object.
#'
#' @param this A `ppt` object.
#' @param index An integer specifying the slide index to set as current.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Add a New Slide
#'
#' Adds a new slide to the `ppt` object and sets it as the current slide.
#'
#' @param this A `ppt` object.
#' @param title An optional character string specifying the title of the new slide.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Add Transition Slide
#'
#' Adds a transition slide with specified text in a predefined position.
#'
#' @param this A `ppt` object.
#' @param text A character string for the transition text.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Move Slide
#'
#' Moves the current slide to a specified position within the slide order.
#'
#' @param this A `ppt` object.
#' @param index An optional integer specifying the current slide index.
#' @param to An integer specifying the target slide index.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Get Slide Layout
#'
#' Retrieves the default layout name for blank slides in the template.
#'
#' @param this A `ppt` object.
#'
#' @return A character string representing the layout name.
#'
#' @export
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

#' Get Slide Master
#'
#' Retrieves the default master name for slides in the template.
#'
#' @param this A `ppt` object.
#'
#' @return A character string representing the master name.
#'
#' @export
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

#' Get Slide Width
#'
#' Retrieves the width of the slides in the `ppt` object.
#'
#' @param this A `ppt` object.
#'
#' @return A numeric value representing the slide width.
#'
#' @export
ppt.get_width = function(
  this #: ppt
) {
  .ppt.check_class(this)

  width = (this %>% officer::slide_size())$width

  return(width)
}

#' Get Slide Height
#'
#' Retrieves the height of the slides in the `ppt` object.
#'
#' @param this A `ppt` object.
#'
#' @return A numeric value representing the slide height.
#'
#' @export
ppt.get_height = function(
    this #: ppt
) {
  .ppt.check_class(this)

  height = (this %>% officer::slide_size())$height

  return(height)
}

#' Get Number of Slides
#'
#' Retrieves the total number of slides in the `ppt` object.
#'
#' @param this A `ppt` object.
#'
#' @return An integer representing the number of slides.
#'
#' @export
ppt.get_length = function(
  this #: ppt
) {
  .ppt.check_class(this)

  return(length(this))
}

#' Get Vertical Dimension
#'
#' Calculates the vertical dimension relative to the slide height.
#'
#' @param this A `ppt` object.
#' @param relative_dimension A numeric value (0 - 1) specifying the relative dimension.
#'
#' @return A numeric value for the absolute vertical dimension.
#'
#' @export
ppt.get_vertical_dimension = function(
    this, #: ppt
    relative_dimension #: numeric (0 - 1)
) {
  .ppt.check_class(this)
  type.check_numeric(relative_dimension, 'relative_dimension')

  height = this %>% ppt.get_height()

  return(relative_dimension * height)
}

#' Get Horizontal Dimension
#'
#' Calculates the horizontal dimension relative to the slide width.
#'
#' @param this A `ppt` object.
#' @param relative_dimension A numeric value (0 - 1) specifying the relative dimension.
#'
#' @return A numeric value for the absolute horizontal dimension.
#'
#' @export
ppt.get_horizontal_dimension = function(
    this, #: ppt
    relative_dimension #: numeric (0 - 1)
) {
  .ppt.check_class(this)
  type.check_numeric(relative_dimension, 'relative_dimension')

  width = this %>% ppt.get_width()

  return(relative_dimension * width)
}

#' Add Document Title
#'
#' Adds a large title to the current slide.
#'
#' @param this A `ppt` object.
#' @param title A character string for the document title.
#'
#' @return The modified `ppt` object.
#'
#' @export
ppt.add_document_title = function(
    this, #: ppt
    title #: character
) {
  .ppt.check_class(this)
  type.check_character(title, 'title')

  this = this %>%
    ppt.add_text(
      text = title,
      position = pptpos.document_title(),
      size = 44,
      bold = TRUE
    )

  return(this)
}

#' Add Title to Slide
#'
#' Adds a title to the current slide.
#'
#' @param this A `ppt` object.
#' @param title A character string for the slide title.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Add Text to Slide
#'
#' Adds text to the current slide at a specified position.
#'
#' @param this A `ppt` object.
#' @param text A character string for the text.
#' @param position A `pptpos` object specifying the text position.
#' @param font A character string specifying the font family.
#' @param size A numeric value for the font size.
#' @param bold A logical value indicating whether the text is bold.
#'
#' @return The modified `ppt` object.
#'
#' @export
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
      officer::fpar(
        officer::ftext(
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

#' Add Object to Slide
#'
#' Adds an object (e.g., table or ggplot) to the slide at a specified position.
#'
#' @param this A `ppt` object.
#' @param obj The object to add to the slide.
#' @param position A `pptpos` object specifying the position.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Add ggplot to Slide
#'
#' Adds a ggplot object to the current slide at a specified position.
#'
#' @param this A `ppt` object.
#' @param ggplot_obj A ggplot object to add to the slide.
#' @param position A `pptpos` object specifying the position.
#'
#' @return The modified `ppt` object.
#'
#' @export
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

#' Add Table to Slide
#'
#' Adds a table to the slide at a specified position.
#'
#' @param this A `ppt` object.
#' @param table_obj A table object to add to the slide.
#' @param position A `pptpos` object specifying the position.
#' @param fit_height A logical indicating whether to fit the table height.
#'
#' @return The modified `ppt` object.
#'
#' @export
ppt.add_table = function(
    this, #: ppt
    table_obj, #: table
    position, #: pptpos
    fit_height = FALSE
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

#' Save PowerPoint
#'
#' Saves the `ppt` object as a PowerPoint file at the specified path.
#'
#' @param this A `ppt` object.
#' @param path A character string specifying the file path.
#'
#' @export
ppt.save = function(
  this, #: ppt
  path #: character
) {
  .ppt.check_class(this)
  type.check_character(path, 'path')

  print(this, path)
}
