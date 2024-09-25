library(stats)
library(dplyr)

# class pca

# constructors
pca_from.data.frame = function(
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

  obj = list()
  class(obj) = 'pca'

  temp = prcomp(data, center = center, scale. = scale)

  obj$standard.deviation = temp$sdev
  obj$explained.variance = temp$sdev^2 / sum(temp$sde^2)
  obj$loads = as.data.frame(temp$rotation)
  obj$principal.components = as.data.frame(temp$x)

  obj = obj %>% pca_invert.loads()

  return(obj)
}

# methods
pca_invert.loads = function(
  obj #: pca
) {
  if(obj$loads$PC1[1] < 0) {
    obj$loads$PC1 = obj$loads$PC1*-1
    obj$principal.components$PC1 = obj$principal.components$PC1*-1
  }

  return(obj)
}
