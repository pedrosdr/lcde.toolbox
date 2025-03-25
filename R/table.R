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

#' @name table.linear_model
#' @title Create a Flextable Summary of a Linear Regression Model
#' @description
#' This function takes a fitted linear model object (`model`) as input and returns a formatted Flextable summarizing the regression results.
#'
#' @param model A fitted linear model object created using `lm`.
#'
#' @return A Flextable object containing a formatted summary of the regression model.
#'
#' @examples
#' ## Example usage
#' library(lm)
#' library(flextable)
#'
#' # Simulate some data
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = 2*x1 + 3*x2 + rnorm(100))
#'
#' # Fit a linear model
#' model <- lm(y ~ x1 + x2, data = data)
#'
#' # Generate the summary table
#' summary_table <- table.linear_model(model)
#'
#' print(summary_table)
#'
#' # You can further customize the Flextable using its functionalities.
#'
#' @import flextable
#'
#' @export
table.linear_model = function(
  model #: lm
) {
  smodel = summary(model)

  ci = confint(model)

  smodel$coefficients[2, 2]
  dfreg = as.data.frame(smodel$coefficients)
  colnames(dfreg) = c("Estimativa", "Erro Padrão", "Valor de t", "P(>|t|)")
  dfreg[,"Estimativa"] = round(dfreg[,"Estimativa"], 5)
  dfreg[,"Erro Padrão"] = round(dfreg[,"Erro Padrão"], 5)
  dfreg[,"Valor de t"] = round(dfreg[,"Valor de t"], 3)
  dfreg[,"P(>|t|)"] = ifelse(
    dfreg[,"P(>|t|)"] < 0.001,
    "<0.001",
    paste0(round(dfreg[,"P(>|t|)"], 5))
  )
  dfreg[,"CI (95%)"] = sprintf("%.4f — %.4f", ci[,1], ci[,2])
  dfreg[,"Significância"] = ifelse(
    dfreg[,"P(>|t|)"] < 0.001, "***", ifelse(
      dfreg[,"P(>|t|)"] < 0.01, "**", ifelse(
        dfreg[,"P(>|t|)"] < 0.05, "*", ifelse(
          dfreg[,"P(>|t|)"] < 0.1, ".", " "
        )
      )
    )
  )
  dfreg[,"Variável"] = ifelse(
    rownames(smodel$coefficients) == "(Intercept)",
    "(Intercepto)", rownames(smodel$coefficients)
  )
  dfreg = dfreg[,c(ncol(dfreg), (1:ncol(dfreg)-1))]

  pf_value = pf(
    smodel$fstatistic[1],
    smodel$fstatistic[2],
    smodel$fstatistic[3],
    lower.tail = FALSE
  )

  pf_value = if(pf_value < 0.001) "<0.001" else sprintf("%.4f", pf_value)

  obj = table(dfreg) %>%
    flextable::add_footer_row(
      values = sprintf(
        "Estatística F: %.1f com %.0f e %.0f GL\t—\tValor p: %s",
        smodel$fstatistic[1],
        smodel$fstatistic[2],
        smodel$fstatistic[3],
        pf_value
      ),
      colwidths = c(7)
    ) %>%
    flextable::add_footer_row(
      values = c(
        sprintf(
          "Erro padrão residual: %.4f com %.0f graus de liberdade",
          smodel$sigma, smodel$df[2]
        )
      ),
      colwidths = c(7)
    ) %>%
    flextable::add_footer_row(
      values = sprintf(
        "R²: %.4f\t\tR² Ajustado: %.4f",
        smodel$r.squared, smodel$adj.r.squared
      ),
      colwidths = c(7)
    ) %>%
    flextable::add_footer_row(
      values = paste0(
        "Códigos de Significância: 0 [***] 0.001 [**] 0.01 [*] 0.05 [.] 0.1 [ ] 1"
      ),
      colwidths = c(7)
    ) %>%
    table.set_theme_dark()
  obj = obj %>% table.text_size(9.5, part="all")
  obj = obj %>% table.padding(3, part="all")

  return(obj)
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

  std_border <- officer::fp_border(width = 1, color = "#000000")

  this = this %>%
    flextable::bg(bg = colors.grayscale()[5], part = "header") %>%
    flextable::bg(bg = "#dfdfdf", part = "footer") %>%
    flextable::color(color = "#ffffff", part = "header") %>%
    flextable::valign(valign = "center", part = 'all') %>%
    flextable::align(align = 'center', part='all') %>%
    flextable::fontsize(size = 10, part='header') %>%
    flextable::fontsize(size = 10, part='body') %>%
    flextable::border_outer(part="all", border = std_border) %>%
    flextable::border_inner_h(border = std_border, part="all") %>%
    flextable::border_inner_v(border = std_border, part="all")

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

#' Set Font Size for a Table
#'
#' Adjusts the font size of various parts of a `table` object.
#'
#' @param this A `table` object.
#' @param size The desired font size in points.
#' @param part The part of the table to modify the font size. Options include:
#'   - `"all"`: Apply the font size to the entire table.
#'   - `"body"`: Apply the font size to the table body.
#'   - `"title"`: Apply the font size to the table title.
#'   - `"footer"`: Apply the font size to the table footer.
#'
#' @return The modified `table` object.
#'
#' @examples
#' library(flextable)
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data)
#' tbl <- table.text_size(tbl, size = 14, part = "body")
#'
#' @export
table.text_size = function(
  this, #: table
  size, #: numeric
  part = c("all", "body", "header", "footer")
) {
  .table.check_class(this)
  type.check_numeric(size)

  this = this %>% flextable::fontsize(
    size = size,
    part = part[1]
  )

  return(this)
}

#' Set Padding for a Table
#'
#' Adjusts the padding of various parts of a `table` object.
#'
#' @param this A `table` object.
#' @param padding The desired padding in points.
#' @param part The part of the table to modify the padding. Options include:
#'   - `"all"`: Apply the padding to the entire table.
#'   - `"body"`: Apply the padding to the table body.
#'   - `"header"`: Apply the padding to the table header.
#'   - `"footer"`: Apply the padding to the table footer.
#'
#' @return The modified `table` object.
#'
#' @examples
#' library(flextable)
#' data <- data.frame(A = 1:3, B = 4:6)
#' tbl <- table(data)
#' tbl <- table.padding(tbl, padding = 5, part = "all")
#'
#' @export
table.padding = function(
    this, #: table
    padding, #: numeric
    part = c("all", "body", "header", "footer")
) {
  .table.check_class(this)
  type.check_numeric(padding)

  this = this %>% flextable::padding(
    padding = padding,
    part = part[1]
  )

  return(this)
}
