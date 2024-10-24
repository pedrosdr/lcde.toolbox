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
