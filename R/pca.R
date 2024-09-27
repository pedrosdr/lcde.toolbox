library(stats)
library(dplyr)

# class pca

# constructors
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
.pca.check_class = function(
  obj #; pca
) {
  if(class(obj) != 'pca') {
    stop("'obj' must be of type 'pca'")
  }
}

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
