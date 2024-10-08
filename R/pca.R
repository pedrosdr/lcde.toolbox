library(stats)
library(dplyr)

# class pca

# constructors

#' Create a PCA object from a data frame
#'
#' This function computes the principal components of a given data frame,
#' returning an object of class 'pca'.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param center A logical value indicating whether to center the variables.
#' @param scale A logical value indicating whether to scale the variables.
#'
#' @return An object of class 'pca' containing:
#' \item{standard_deviation}{Standard deviations of the principal components.}
#' \item{explained_variance}{Proportion of variance explained by each principal component.}
#' \item{loads}{Loadings of the variables on the principal components.}
#' \item{principal_components}{The principal component scores.}
#' \item{component_equations}{Equations representing each principal component.}
#'
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' pca_result <- pca.from_data_frame(df)
#'
#' @export
pca.from_data_frame = function(
    data, #: data.frame
    center = TRUE, #: logical
    scale = FALSE #: logical
) {
  if(class(data) != 'data.frame') {
    stop("'data' must be of type 'data.frame'")
  }
  if(class(center) != 'logical') {
    stop("'center' must be of type 'logical'")
  }
  if(class(scale) != 'logical') {
    stop("'scale' must be of type 'logical'")
  }

  data = na.omit(data)
  this = list()
  class(this) = 'pca'

  temp = prcomp(data, center = center, scale. = scale)

  this$data = data
  this$standard_deviation = temp$sdev
  this$explained_variance = temp$sdev^2 / sum(temp$sde^2)
  this$loads = as.data.frame(temp$rotation)
  this$principal_components = as.data.frame(temp$x)

  this = this %>% .pca.invert_loads()
  this = this %>% .pca.translate_component_names()

  this$component_equations = this %>% .pca.get_component_equations()
  this$length = nrow(data)
  this$component_names = as.character(colnames(this$principal_components))
  this$data_names = as.character(colnames(this$data))

  return(this)
}

# properties

#' Get the ID 'Indicador de Desigualdade' of a PCA object
#'
#' This function calculates an ID metric for the PCA object based on the centroid distance.
#'
#' @param this An object of class 'pca'.
#'
#' @return A numeric value representing the ID metric.
#'
#' @export
pca.get_ID = function(
  this #: pca
) {
  .pca.check_class(this)

  id = stats.inequality_indicator(this$data)

  return(id)
}

# methods

#' Check if an object is of class 'pca'
#'
#' This function checks if the provided object is of class 'pca'.
#'
#' @param this An object to be checked.
#'
#' @return NULL if the object is of class 'pca'; otherwise, an error is raised.
#'
#' @keywords internal
.pca.check_class = function(
  obj
) {
  if(class(obj) != 'pca') {
    stop("'obj' must be of type 'pca'")
  }
}

#' Translate component names in PCA results
#'
#' This function modifies the column names of principal components and loadings,
#' replacing 'PC' with 'CP'.
#'
#' @param this An object of class 'pca'.
#'
#' @return The modified PCA object with updated component names.
#'
#' @export
.pca.translate_component_names = function(
  this #: pca
) {
  .pca.check_class(this)

  colnames(this$principal_components) = gsub(
    'PC', 'CP', colnames(this$principal_components)
  )

  colnames(this$loads) = gsub(
    'PC', 'CP', colnames(this$loads)
  )

  return(this)
}

#' Invert loadings in PCA results
#'
#' This function ensures that the first principal component has positive loadings.
#'
#' @param this An object of class 'pca'.
#'
#' @return The modified PCA object with inverted loadings if necessary.
#'
#' @export
.pca.invert_loads = function(
  this #: pca
) {
  .pca.check_class(this)

  if(this$loads$PC1[1] < 0) {
    this$loads$PC1 = this$loads$PC1*-1
    this$principal_components$PC1 = this$principal_components$PC1*-1
  }

  return(this)
}

#' Get equations for each principal component
#'
#' This function generates equations representing each principal component based on
#' their loadings.
#'
#' @param this An object of class 'pca'.
#'
#' @return A character vector containing equations for each principal component.
#'
#' @export
.pca.get_component_equations = function(
  this #: pca
) {
  .pca.check_class(this)

  equations = c()
  for(j in 1:ncol(this$loads)) {
    col = colnames(this$loads)[j]
    equation = paste(col, '=')
    for(i in 1:nrow(this$loads)) {
      row = rownames(this$loads)[i]
      signal = if(this$loads[i,j] < 0) '-' else '+'
      signal = if(i == 1) '' else signal
      equation = paste(
        equation, signal, sprintf(
          "%.2f", abs(this$loads[i,j])
        ), row
      )
      equation = gsub('  ', ' ', equation)
    }
    equations = c(equations, equation)
  }
  return(equations)
}

#' Get Largest Variations in PCA Data
#'
#' This function calculates the largest variations in principal component scores
#' between two specified years for given keys (variables). It helps in identifying
#' which variables have changed the most over time in PCA analysis, either positively
#' or negatively.
#'
#' @param this A PCA object of class 'pca'.
#' @param number An integer specifying the number of largest variations to return.
#' @param keys A vector of keys (variables) for which the largest variations are to be calculated.
#' @param years A numeric vector indicating the years corresponding to the data points.
#' @param labels (Optional) A vector of labels for each key, used for annotation. Defaults to empty strings.
#' @param variation A character string indicating whether to calculate 'positive' or 'negative' variations.
#' @param errors A character string indicating how to handle errors:
#'        'raise' to stop execution, 'warn' to issue a warning, or 'ignore' to suppress errors.
#'
#' @return A data frame containing the largest variations, including keys, labels,
#'         and the first two principal component coordinates for the specified number of largest variations.
#'
#' @details
#' The function performs several checks on the input parameters to ensure they meet
#' the expected criteria. It calculates the difference in principal component scores
#' between the minimum and maximum years provided and returns a data frame with the
#' specified number of largest variations. If there are fewer variations than requested,
#' it will handle errors based on the specified `errors` parameter.
#'
#' @examples
#' # Assuming pca_obj is a valid PCA object
#' largest_variations <- pca.get_largest_variations(
#'   this = pca_obj,
#'   number = 5,
#'   keys = c("Variable1", "Variable2", "Variable3"),
#'   years = c(2020, 2021, 2022),
#'   labels = c("Label1", "Label2", "Label3"),
#'   variation = 'positive'
#' )
#'
#' @export
pca.get_largest_variations = function(
    this, #: pca
    number, #: integer
    keys, #: vector
    years, #: vector
    labels = NULL, #: vector
    variation = 'positive', #: character
    errors='warn' #: character
) {
  .pca.check_class(this)
  if(!(variation %in% c('positive', 'negative'))) {
    stop("'variation' must be one of ('positive', 'negative')")
  }
  if(number != round(number)) {
    stop("'number' must be an integer")
  }
  if(length(years) != nrow(this$principal_components)) {
    stop("'keys' must be a vector of the same length as the data")
  }
  if(!(class(years) %in% c('numeric', 'integer'))) {
    stop("'years' must be of type 'numeric'")
  }
  if(length(years) != nrow(this$principal_components)) {
    stop("'years' must be a vector of the same length as the data")
  }
  if(!(errors %in% c('raise', 'warn', 'ignore'))) {
    stop("'errors' must be one of ('raise', 'warn', 'ignore')")
  }
  if(is.null(labels)) {
    labels = rep('', length(keys))
  }
  if(length(labels) != nrow(this$principal_components)) {
    stop("'labels' must be a vector of the same length as the data")
  }

  unique_years = unique(years)

  if(length(unique_years) <= 1) {
    stop("the number of years must be superior to 1")
  }

  df = data.frame(
    keys = keys,
    years = years,
    labels = labels
  )

  df = cbind(df, this$data, this$principal_components)

  df_min_year = df %>% filter(years == min(unique_years))
  df_max_year = df %>% filter(years == max(unique_years))

  df_intersection = merge(
    df_min_year, df_max_year,
    by=c('keys'),
    all.x=FALSE,
    all.y=FALSE
  )
  df_intersection$var = df_intersection$CP1.y - df_intersection$CP1.x

  df_largest_increases = df_intersection %>% rename(labels = labels.x)

  if(variation == 'positive') {
    df_largest_increases = df_largest_increases[
      order(-df_largest_increases$var),
    ]
  } else {
    df_largest_increases = df_largest_increases[
      order(df_largest_increases$var),
    ]
  }

  df_largest_increases = df_largest_increases[1:number,]

  if(nrow(df_largest_increases) < number) {
    if(errors == 'raise') {
      stop("The length of the output vector is shorter than the specified number.
           Please check your input data. To ignore this error set 'errors' to
           'warn' or 'ignore'")
    } else if(errors == 'warn') {
      warning("The length of the output vector is shorter than the specified number.
           Please check your input data. To ignore this warning set 'errors' to
           'ignore'")
    }
  }

  return(df_largest_increases)
}

#' pca.get_categories
#'
#' Categorizes principal components into quartiles.
#'
#' This function assigns categories (A, B, C, D) to the first principal component (CP1)
#' based on its quartile. Values are categorized as follows:
#' - D: below the 25th percentile
#' - C: between the 25th and 50th percentiles
#' - B: between the 50th and 75th percentiles
#' - A: above the 75th percentile
#'
#' @param this A \code{pca} object containing principal components.
#'
#' @return A factor indicating the category for each observation based on CP1.
#'
#' @export
pca.get_categories = function(
  this #: pca
) {
  .pca.check_class(this)
  cp1 = this$principal_components$CP1

  categories = ifelse(
    cp1 < quantile(cp1, 0.25), 'D', ifelse(
      cp1 < quantile(cp1, 0.5), 'C', ifelse(
        cp1 < quantile(cp1, 0.75), 'B', 'A'
      )
    )
  )

  categories = as.factor(categories)

  return(categories)
}

#' pca.get_category_colors
#'
#' Retrieves colors for PCA categories based on a specified color palette.
#'
#' This function assigns colors to PCA categories (A, B, C, D) using the provided
#' color palette. The palette must contain at least four colors.
#'
#' @param this A \code{pca} object containing PCA results.
#' @param palette A character vector representing a color palette.
#'                Must have at least four colors.
#'
#' @return A character vector of colors corresponding to the PCA categories.
#'
#' throws Error if the palette has less than four colors or is not of type character.
#'
#' @export
pca.get_category_colors = function(
  this, #: pca
  palette = colors.red_to_green() #: character vector
) {
  .pca.check_class(this)
  type.check_character(palette, 'palette')
  if(length(palette) < 4) {
    stop("'palette' must have at least 4 colors")
  }

  categories = this %>% pca.get_categories()
  colors = ifelse(
    categories == 'D', palette[1], ifelse(
      categories == 'C', palette[2], ifelse(
        categories == 'B', palette[3], palette[4]
      )
    )
  )

  return(colors)
}

#' pca.filter
#'
#' Filters PCA data based on a logical mask.
#'
#' This function filters the principal components and associated data in a
#' PCA object based on a logical vector (mask). Only rows corresponding to
#' TRUE values in the mask are retained.
#'
#' @param this A \code{pca} object containing PCA results.
#' @param mask A logical vector indicating which rows to keep.
#'
#' return A filtered \code{pca} object with only the selected rows.
#'
#' throws Error if the mask is not of type logical or does not match the number of rows in the data.
#'
#' @export
pca.filter = function(
  this, #: pca
  mask #: logical vector
) {
  .pca.check_class(this)
  type.check_logical(mask, 'mask')
  if(length(mask) != nrow(this$principal_components)) {
    stop("'mask' must be a vector of the same length as the data")
  }

  obj = this
  obj$principal_components = obj$principal_components[mask,]
  obj$data = obj$data[mask,]

  return(obj)
}


