library(dplyr)
library(ggplot2)

# class pcaviz

# static fields
.pcaviz.PALETTE = c(

)

# constructors
.pcaviz.from_ggplot = function(
  ggplot_obj #: ggplot
) {
  if(!('ggplot' %in% class(ggplot_obj))) {
    stop("'ggplot_obj' must be of type 'ggplot'")
  }

  obj = ggplot_obj
  class(obj) = c(class(obj), 'pcaviz')
  return(obj)
}

# methods
.pcaviz.check_class = function(
    obj #; pcaviz
) {
  if('pcaviz' %in% class(obj)) {
    stop("'obj' must be of type 'pcaviz'")
  }
}

pcaviz.scatter = function(
  pca_obj #: pca
) {
  .pca.check_class(pca_obj)

  plot = ggplot() +
    theme_minimal() +
    geom_point(
      data = pca_obj$principal_components,
      aes(
        x=CP1,
        y=CP2
      )
    ) +

    labs(
      x = paste0(
        'CP1 (',
        gsub(
          '\\.', ',',
          sprintf("%.2f%%", 100*pca_obj$explained_variance[1])
        ),
        ')'
      ),

      y = paste0(
        'CP2 (',
        gsub(
          '\\.', ',',
          sprintf("%.2f%%", 100*pca_obj$explained_variance[2])
        ),
        ')'
      )
    ) +

    geom_hline(yintercept = 0, linetype='dashed') +
    geom_vline(xintercept = 0, linetype='dashed')

  return(plot)
}

.pcaviz.set_theme_scatter = function(
  obj #: pcaviz
) {
  .pcaviz.check_class(obj)

  obj = obj +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(obj)
}

obj = pcaviz.scatter(pcs)
.pcaviz.set_theme_scatter(obj)
