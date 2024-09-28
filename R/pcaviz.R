library(dplyr)
library(ggplot2)

# class pcaviz

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

# properties
pcaviz.set_size = function(
  obj, #: pcaviz
  size_obj = vizsize() #: vizsize
) {
  .pcaviz.check_class(obj)

  obj$size = size_obj
  return(obj)
}

# methods
.pcaviz.check_class = function(
    obj #; pcaviz
) {
  if(!('pcaviz' %in% class(obj))) {
    stop("'obj' must be of type 'pcaviz'")
  }
}

pcaviz.scatter = function(
  pca_obj, #: pca
  size = vizsize() #: vizsize | text | numeric
) {
  .pca.check_class(pca_obj)
  size = vizsize.parse(size)

  obj = ggplot() +
    theme_minimal() +
    geom_point(
      data = pca_obj$principal_components,
      aes(
        x=CP1,
        y=CP2
      ),
      color=colors.mixed()[2],
      size=size$point_size
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
      ),

      caption = paste0(pca_obj$component_equations[1],
                       '\n',
                       pca_obj$component_equations[2])
    ) +

    geom_hline(yintercept = 0, linetype='dashed',
               linewidth=size$linewidth,
               color=colors.grayscale()[5]) +
    geom_vline(xintercept = 0, linetype='dashed',
               linewidth=size$linewidth,
               color=colors.grayscale()[5])


  obj = obj %>% .pcaviz.from_ggplot()
  obj = obj %>% pcaviz.set_size(size)
  obj = obj %>% pcaviz.set_theme_scatter()

  return(obj)
}

pcaviz.set_theme_scatter = function(
  obj #: pcaviz
) {
  .pcaviz.check_class(obj)

  obj = obj +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size=obj$size$axis_title),
      axis.text = element_text(size=obj$size$text),
      plot.title = element_text(size=obj$size$title, hjust=0.5),
      plot.subtitle = element_text(size=obj$size$subtitle, hjust=0.5),
      plot.caption = element_text(
        size=obj$size$text,
        hjust=0.5
      )
    )
  return(obj)
}
