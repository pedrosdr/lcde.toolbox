library(htmltools)
library(webshot)

# class widget

# constructors
widget = function(
  build_directory = '.', #:character
  name = 'widget' #: character
) {
  type.check_character(name, 'name')
  type.check_character(build_directory, 'build_directory')

  obj = list()
  class(obj) = c('widget')

  obj$build_directory = paste0(build_directory,'/',name)
  obj$build_path = paste0(build_directory,'/',name,'/',name,'.html')
  obj$name = name

  dir.create(obj$build_directory)

  return(obj)
}

# methods
.widget.check_class = function(
  obj
) {
  if(!('widget' %in% class(obj))) {
    stop("'obj' must be of type 'widget'")
  }
}

widget.close = function(
  this #: widget
) {
  .widget.check_class(this)

  unlink(this$build_directory, recursive = TRUE)
}

widget.geoleaf = function(
    this, #: widget
    geoleaf_obj #: geoleaf
) {
  .widget.check_class(this)
  .geoleaf.check_class(geoleaf_obj)

  htmlwidgets::saveWidget(geoleaf_obj, this$build_path, selfcontained = TRUE)

  return(this)
}

widget.print_screen = function(
  this, #: widget
  path=NULL, #: character
  width=800, #: integer
  height=600, #: integer
  quality=1 #: integer
) {
  .widget.check_class(this)
  if(is.null(path)) {
    path = paste0(this$build_directory,'/',this$name, '.png')
  }
  type.check_character(path, 'path')
  type.check_integer(width, 'width')
  type.check_integer(height, 'height')
  type.check_integer(quality, 'quality')

  webshot(
    url = this$build_path,
    file = path,
    vwidth = width,
    vheight = height,
    zoom = quality)
}

widget.geoleaf_legend = function(
    this, #: widget
    geoleaf_obj, #: geoleaf
    labels, #: vector
    values, # vector
    map_width=2976, #: integer
    row_width=450, #: integer
    row_limit=45 #: integer
) {
  .widget.check_class(this)
  .geoleaf.check_class(geoleaf_obj)
  if(length(labels) != length(values)) {
    stop("'labels' and 'values' must be vectors of the same length")
  }

  htmlwidgets::saveWidget(geoleaf_obj, this$build_path)

  q_per_row = ceiling(length(labels)/row_limit)

  box_width = map_width + q_per_row * row_width
  legend_width = q_per_row * row_width

  rows = c()
  for(i in 1:q_per_row) {
    row = ""
    for(j in (row_limit * i - row_limit + 1):(row_limit * i)) {
      if(j > length(labels)) break
      row = paste0(row,
                   '<div class="legend-item"><strong>',labels[j],'</strong> - ', values[j],'</div>'
      )
    }
    rows = c(rows, row)
  }

  css <- paste0(
    '<style>
      *, html, body {
          font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      }

      .box {
          width: ',box_width,'px;
          height: 2232px;
      }

      .map{
          width: ',map_width,'px;
          height: 100%;
          float: left;
      }

      .map img {
          width: ',map_width,'px;
          height: 100%;
      }

      .legend {
          float: left;
          width: ',legend_width,'px;
          height: 2132px;
      }

      .legend-row {
          width: ',row_width,'px;
          height: 2132px;
          float: left;
      }

      .legend-item {
          font-size: 2.5em;
          padding: 0px 40px 0px 40px;
      }
    </style>'
  )

  html <- paste0(
    '<!DOCTYPE html>
    <html lang="pt-br">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Document</title>
        ',css,'
    </head>
    <body>
        <div class="box">
            <div class="map">
                <img src="../',map_image_file,'" alt="">
            </div>
            <div class="legend">'
  )

  for(i in 1:q_per_row) {
    html <- paste0(html,
                   '<div class="legend-row">
                ',rows[i],'
               </div>'
    )
  }

  html <- paste0(html,
                 '</div>
        </div>
    </body>
    </html>'
  )

  write_lines(html, file = new_html_file)
  webshot(new_html_file, map_image_file, cliprect = c(0, 0, box_width, 2232))


  return(this)
}


