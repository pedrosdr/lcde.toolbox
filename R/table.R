library(flextable)

# class table

# constructors
table = function(
  dataframe, #: data.frame
  column_names = colnames(dataframe) #: vector
) {
  type.check_dataframe(dataframe, 'dataframe')

  if(length(column_names) != ncol(dataframe)) {
    stop(paste0(
      "'column_names' must be a vector of the same length as the number of",
      " columns in the dataframe"
    ))
  }

  colnames(dataframe) = column_names

  this = flextable::flextable(dataframe)
  class(this) = c(class(this), 'table')

  this = this %>%
    table.set_theme_dark() %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::autofit() %>%
    flextable::padding(padding = 1, part = 'all')

  return(this)
}

# methods
.table.check_class = function(
  obj
) {
  if(!("table" %in% class(obj))) {
    stop("'obj' must be of type 'table'")
  }
}

table.add_header_row = function(
  this, #: table
  column_names, #: vector
  column_widths #: vector
) {
  .table.check_class(this)
  if(length(column_names) != length(column_widths)) {
    stop("'column_names' and 'column_widths' must be vectors of the same size")
  }

  this = this %>%
    flextable::add_header_row(
      values = column_names,
      colwidths = column_widths
    )

  return(this)
}

table.set_theme_dark = function(
    this #: theme
) {
  .table.check_class(this)

  this = this %>%
    flextable::bg(bg = colors.grayscale()[5], part = "header") %>%
    flextable::color(color = "#ffffff", part = "header") %>%
    flextable::valign(i = 1, valign = "bottom") %>%
    flextable::align(align = 'center', part='all') %>%
    flextable::fontsize(size = 12, part='header') %>%
    flextable::fontsize(size = 10, part='body')

  return(this)
}

table.fit_to_page = function(
  this, #: table
  page_width, #: numeric
  page_height = NULL #: numeric
) {
  .table.check_class(this)
  type.check_numeric(page_width)

  this <- this %>% flextable::width(
    width = dim(this)$widths*page_width/(flextable::flextable_dim(this)$widths)
  )

  if(!is.null(page_height)) {
    type.check_numeric(page_height)

    calculated_height = mean(
      dim(this)$heights*page_height/(flextable::flextable_dim(this)$heights)
    )

    this <- this %>% flextable::height(
      height = calculated_height,
      part = 'body'
    )
  }

  return(this)
}
