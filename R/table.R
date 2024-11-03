library(flextable)

# class table

# constructors

#' Table Constructor
#'
#' Creates a `table` object, extending the `flextable` class for formatted tables.
#'
#' @param dataframe A data frame containing the data for the table.
#' @param column_names A vector of column names for the table headers. Defaults to the column names of the `dataframe`.
#'
#' @return A `table` object styled with a dark theme.
#'
#' @examples
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data, column_names = c("Col A", "Col B"))
#'
#' @export
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

  this = flextable::flextable(dataframe)
  class(this) = c(class(this), 'table')

  this = this %>%
    flextable::set_header_labels(values = column_names)

  this = this %>%
    table.set_theme_dark() %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::autofit() %>%
    flextable::padding(padding = 1, part = 'all') %>%
    flextable::height(height = 0.25, part = 'header') %>%
    flextable::height(height = 0.25, part = 'body')

  return(this)
}

#' PCA Variation Table
#'
#' Generates a table showing the largest variations in PCA components for a specific rank.
#'
#' @param pca_obj A `pca` object containing PCA results.
#' @param rank A positive integer indicating the variation rank to display.
#' @param keys A vector of keys identifying each observation.
#' @param years An integer vector specifying the years for each observation.
#' @param labels A vector of labels for each observation.
#' @param variation A character string specifying the variation type. Must be either "positive" or "negative".
#'
#' @return A `table` object displaying the PCA variation values for a specified rank.
#'
#' @examples
#' pca_result <- pca(data) # Assuming `data` is a prepared dataset
#' tbl_variation <- table.pca_variation(pca_result, rank = 1, keys, years, labels, "positive")
#'
#' @export
table.pca_variation = function(
  pca_obj, #: pca
  rank, #: positive integer
  keys, #: vector
  years, #: integer vector
  labels, #: vector
  variation = c('positive', 'negative') #: character
) {
  .pca.check_class(pca_obj)

  variations = pca_obj %>% pca.get_largest_variations(
    number = rank,
    keys = keys,
    years = years,
    labels = labels,
    variation = variation[1]
  )

  variation = variations[rank,]

  unique_years = unique(years)
  unique_years = unique_years[order(unique_years)]

  year.x = unique_years[1]
  year.y = unique_years[length(unique(years))]

  df_table = data.frame()
  for(column_name in pca_obj$data_names) {
    dfi = data.frame(
      'A' = c(column_name),
      'B' = c(variation[1,paste0(column_name, '.x')]),
      'C' = c(variation[1,paste0(column_name, '.y')])
    )

    df_table = if(nrow(df_table) == 0) dfi else rbind(df_table, dfi)
  }

  tbl = table(
      df_table,
      column_names = c('', year.x, year.y)
    ) %>%
    table.add_header_row(
      column_names = c(variation$labels),
      column_widths = c(3)
    )

  return(tbl)
}

# methods

#' Check Table Class
#'
#' Verifies if an object is of class `table`.
#'
#' @param obj An object to check.
#'
#' @return NULL if the check passes; otherwise, an error is raised.
.table.check_class = function(
  obj
) {
  if(!("table" %in% class(obj))) {
    stop("'obj' must be of type 'table'")
  }
}

#' Add Header Row
#'
#' Adds a header row with specified column names and widths to a `table` object.
#'
#' @param this A `table` object.
#' @param column_names A vector of column names for the header row.
#' @param column_widths A vector of column widths, corresponding to each column name.
#'
#' @return The modified `table` object with the new header row.
#'
#' @examples
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data, column_names = c("Col A", "Col B"))
#' tbl <- table.add_header_row(tbl, column_names = c("Header 1"), column_widths = c(2))
#'
#' @export
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
    ) %>%
    flextable::height(height = 0.25, part = 'header') %>%

  return(this)
}

#' Dark Theme for Table
#'
#' Applies a dark theme to a `table` object.
#'
#' @param this A `table` object.
#'
#' @return The styled `table` object with a dark theme applied.
#'
#' @examples
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data, column_names = c("Col A", "Col B"))
#' tbl <- table.set_theme_dark(tbl)
#'
#' @export
table.set_theme_dark = function(
    this #: theme
) {
  .table.check_class(this)

  std_border <- fp_border(width = 1, color = "#000000")

  this = this %>%
    flextable::bg(bg = colors.grayscale()[5], part = "header") %>%
    flextable::color(color = "#ffffff", part = "header") %>%
    flextable::valign(valign = "center", part = 'all') %>%
    flextable::align(align = 'center', part='all') %>%
    flextable::fontsize(size = 10, part='header') %>%
    flextable::fontsize(size = 10, part='body') %>%
    border_outer(part="all", border = std_border) %>%
    border_inner_h(border = std_border, part="all") %>%
    border_inner_v(border = std_border, part="all")

  return(this)
}

#' Fit Table to Page
#'
#' Adjusts the table size to fit within specified page dimensions.
#'
#' @param this A `table` object.
#' @param page_width A numeric value specifying the page width to fit the table within.
#' @param page_height An optional numeric value specifying the page height to fit the table within.
#'
#' @return The resized `table` object.
#'
#' @examples
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data, column_names = c("Col A", "Col B"))
#' tbl <- table.fit_to_page(tbl, page_width = 6, page_height = 8)
#'
#' @export
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
      part = 'header'
    ) %>% flextable::height(
      height = calculated_height,
      part = 'body'
    )
  }

  return(this)
}
