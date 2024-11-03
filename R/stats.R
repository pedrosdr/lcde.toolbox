# class stats

# methods

# methods

#' Inequality Indicator
#'
#' Calculates the inequality indicator for a given dataset, measuring the average distance
#' of each observation from the centroid in the data.
#'
#' @param data A numeric data frame or matrix with each row representing an observation.
#'
#' @return A numeric value representing the inequality indicator.
#'
#' @examples
#' data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' inequality <- stats.inequality_indicator(data)
#'
#' @export
stats.inequality_indicator = function(
    data #: numeric data.frame | matrix
) {
  centroid = as.numeric(apply(data, 2, mean))
  id = mean(apply(data, 1, function(x) sqrt(sum((x-centroid)^2))))

  return(id)
}

#' Magnitude of Observations
#'
#' Calculates the magnitude (Euclidean norm) of each observation in the dataset.
#'
#' @param data A numeric data frame or matrix with each row representing an observation.
#'
#' @return A numeric vector where each element corresponds to the magnitude of each observation.
#'
#' @examples
#' data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' magnitudes <- stats.magnitude(data)
#'
#' @export
stats.magnitude = function(
    data # numeric data.frame | matrix
) {
  magnitude = apply(data, 1, function(x) sqrt(sum(x^2)))

  return(magnitude)
}

#' Relative Magnitude
#'
#' Calculates the relative magnitude of each observation in comparison to a target value.
#'
#' @param data A numeric data frame or matrix with each row representing an observation.
#' @param target A numeric value or vector representing the target for comparison. Defaults to 100.
#'   If a single numeric value is provided, it is repeated for each column; if a vector, its length
#'   must match the number of columns in `data`.
#'
#' @return A numeric vector where each element represents the relative magnitude of an observation.
#'
#' @examples
#' data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' relative_magnitudes <- stats.relative_magnitude(data, target = 50)
#'
#' @export
stats.relative_magnitude = function(
    data, # numeric data.frame | matrix
    target = 100 # numeric
) {
  type.check_numeric(target, 'target')

  if(length(target) == 1) {
    target = rep(target, ncol(data))
  } else if(length(target) != ncol(data)) {
    stop("'target' must either have a length of 1 or match the number of
         columns in 'data'")
  }

  magnitude = stats.magnitude(data)
  magnitude_target = stats.magnitude(
    as.data.frame(matrix(target, nrow = 1))
  )
  relative_magnitude = magnitude / magnitude_target

  return(relative_magnitude)
}
