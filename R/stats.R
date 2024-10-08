# class stats

# methods

# methods
stats.inequality_indicator = function(
    data #: numeric data.frame | matrix
) {
  centroid = as.numeric(apply(data, 2, mean))
  id = mean(apply(data, 1, function(x) sqrt(sum((x-centroid)^2))))

  return(id)
}

stats.magnitude = function(
    data # numeric data.frame | matrix
) {
  magnitude = apply(data, 1, function(x) sqrt(sum(x^2)))

  return(magnitude)
}

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
