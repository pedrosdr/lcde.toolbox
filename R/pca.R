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
  obj = list()
  class(obj) = 'pca'

  temp = prcomp(data, center = center, scale. = scale)

  obj$standard_deviation = temp$sdev
  obj$explained_variance = temp$sdev^2 / sum(temp$sde^2)
  obj$loads = as.data.frame(temp$rotation)
  obj$principal_components = as.data.frame(temp$x)

  obj = obj %>% .pca.invert_loads()
  obj = obj %>% .pca.translate_component_names()

  obj$component_equations = obj %>% .pca.get_component_equations()

  return(obj)
}

# methods

#' Check if an object is of class 'pca'
#'
#' This function checks if the provided object is of class 'pca'.
#'
#' @param obj An object to be checked.
#'
#' @return NULL if the object is of class 'pca'; otherwise, an error is raised.
#'
#' @keywords internal
.pca.check_class = function(
  obj #; pca
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
#' @param obj An object of class 'pca'.
#'
#' @return The modified PCA object with updated component names.
#'
#' @export
.pca.translate_component_names = function(
  obj #: pca
) {
  .pca.check_class(obj)

  colnames(obj$principal_components) = gsub(
    'PC', 'CP', colnames(obj$principal_components)
  )

  colnames(obj$loads) = gsub(
    'PC', 'CP', colnames(obj$loads)
  )

  return(obj)
}

#' Invert loadings in PCA results
#'
#' This function ensures that the first principal component has positive loadings.
#'
#' @param obj An object of class 'pca'.
#'
#' @return The modified PCA object with inverted loadings if necessary.
#'
#' @export
.pca.invert_loads = function(
  obj #: pca
) {
  .pca.check_class(obj)

  if(obj$loads$PC1[1] < 0) {
    obj$loads$PC1 = obj$loads$PC1*-1
    obj$principal_components$PC1 = obj$principal_components$PC1*-1
  }

  return(obj)
}

#' Get equations for each principal component
#'
#' This function generates equations representing each principal component based on
#' their loadings.
#'
#' @param obj An object of class 'pca'.
#'
#' @return A character vector containing equations for each principal component.
#'
#' @export
.pca.get_component_equations = function(
  obj #: pca
) {
  .pca.check_class(obj)

  equations = c()
  for(j in 1:ncol(obj$loads)) {
    col = colnames(obj$loads)[j]
    equation = paste(col, '=')
    for(i in 1:nrow(obj$loads)) {
      row = rownames(obj$loads)[i]
      signal = if(obj$loads[i,j] < 0) '-' else '+'
      signal = if(i == 1) '' else signal
      equation = paste(
        equation, signal, sprintf(
          "%.2f", abs(obj$loads[i,j])
        ), row
      )
      equation = gsub('  ', ' ', equation)
    }
    equations = c(equations, equation)
  }
  return(equations)
}
