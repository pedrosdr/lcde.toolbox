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

  centroid = as.numeric(lapply(this$data, mean))
  id = mean(apply(this$data, 1, function(x) sqrt(sum((x-centroid)^2))))

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
  this #; pca
) {
  if(class(this) != 'pca') {
    stop("'this' must be of type 'pca'")
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

#' Get Largest Increases in PCA Data
#'
#' This function calculates the largest increases in principal component scores
#' between two specified years for given keys (variables). It helps in identifying
#' which variables have changed the most over time in PCA analysis.
#'
#' @param this A PCA object of class 'pca'.
#' @param number An integer specifying the number of largest increases to return.
#' @param keys A vector of keys (variables) for which the largest increases are to be calculated.
#' @param years A numeric vector indicating the years corresponding to the data points.
#' @param labels (Optional) A vector of labels for each key, used for annotation. Defaults to empty strings.
#' @param errors A character string indicating how to handle errors:
#'        'raise' to stop execution, 'warn' to issue a warning, or 'ignore' to suppress errors.
#'
#' @return A data frame containing the largest increases, including keys, labels,
#'         and the first two principal component coordinates for the specified number of largest increases.
#'
#' @details
#' The function performs several checks on the input parameters to ensure they meet
#' the expected criteria. It calculates the difference in principal component scores
#' between the minimum and maximum years provided and returns a data frame with the
#' specified number of largest increases. If there are fewer increases than requested,
#' it will handle errors based on the specified `errors` parameter.
#'
#' @examples
#' # Assuming pca_obj is a valid PCA object
#' largest_increases <- pca.get_largest_increases(
#'   this = pca_obj,
#'   number = 5,
#'   keys = c("Variable1", "Variable2", "Variable3"),
#'   years = c(2020, 2021, 2022),
#'   labels = c("Label1", "Label2", "Label3")
#' )
#'
#' @export
pca.get_largest_increases = function(
    this, #: pca
    number, #: integer
    keys, #: vector
    years, #: vector
    labels = NULL, #: vector
    errors='warn' #: character
) {
  .pca.check_class(this)
  if(number != round(number)) {
    stop("'number' must be an integer")
  }
  if(length(years) != nrow(this$principal_components)) {
    stop("'keys' must be a vector of the same length as the data")
  }
  if(class(years) != 'numeric') {
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

  df_largest_increases = df_intersection[
    ,c('keys', 'labels.x', 'var', 'CP1.x', 'CP1.y', 'CP2.x', 'CP2.y')
  ]
  df_largest_increases = df_largest_increases %>% rename(labels = labels.x)
  df_largest_increases = df_largest_increases[
    order(-df_largest_increases$var),
  ]

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
