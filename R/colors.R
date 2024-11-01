# class colors

# methods

#' Mixed Color Palette
#'
#' This function returns a mixed color palette suitable for various visualizations.
#'
#' @return A character vector of hexadecimal color codes representing the mixed color palette.
#'
#' @examples
#' mixed_colors <- colors.mixed()
#' plot(1:7, col = mixed_colors, pch = 19, cex = 3)
#'
#' @export
colors.mixed = function() {
  palette = c(
    '#369acc',
    '#9656a2',
    '#f8e16f',
    '#f4895f',
    '#95cf92',
    '#de324c',
    '#6c584c'
  )
  return(palette)
}

#' Nighty Color Palette
#'
#' This function returns a nighty color palette with deep, vibrant colors.
#'
#' @return A character vector of hexadecimal color codes representing the nighty color palette.
#'
#' @examples
#' nighty_colors <- colors.nighty()
#' plot(1:7, col = nighty_colors, pch = 19, cex = 3)
#'
#' @export
colors.nighty = function() {
  palette = c(
    '#c82370',
    '#b82078',
    '#a71d80',
    '#961a88',
    '#85178f',
    '#751497',
    '#64109f'
  )
  return(palette)
}

#' Terra Color Palette
#'
#' This function returns a terra color palette inspired by earthy tones.
#'
#' @return A character vector of hexadecimal color codes representing the terra color palette.
#'
#' @examples
#' terra_colors <- colors.terra()
#' plot(1:7, col = terra_colors, pch = 19, cex = 3)
#'
#' @export
colors.terra = function() {
  palette = c(
    '#e7e2e0',
    '#d9c1b2',
    '#bfb4b0',
    '#c4ae9f',
    '#b3a098',
    '#948b86',
    '#827d7a'
  )
  return(palette)
}

#' Grayscale Color Palette
#'
#' This function returns a grayscale color palette ranging from white to black.
#'
#' @return A character vector of hexadecimal color codes representing the grayscale color palette.
#'
#' @examples
#' grayscale_colors <- colors.grayscale()
#' plot(1:7, col = grayscale_colors, pch = 19, cex = 3)
#'
#' @export
colors.grayscale = function() {
  palette = c(
    '#fffffe',
    '#d9d9d9',
    '#adadad',
    '#7a7a7a',
    '#404040',
    '#292929',
    '#000000'
  )
  return(palette)
}

#' Red to Green Color Palette
#'
#' This function returns a red-to-green color palette suitable for visualizing changes
#' or gradients between two extremes.
#'
#' @return A character vector of hexadecimal color codes representing the red-to-green color palette.
#'
#' @examples
#' red_to_green_colors <- colors.red_to_green()
#' plot(1:4, col = red_to_green_colors, pch = 19, cex = 3)
#'
#' @export
colors.red_to_green = function() {
  palette = c(
    '#f96161',
    '#ffb763',
    '#ffec00',
    '#56d433'
  )
  return(palette)
}

#' Purples Color Palette
#'
#' This function returns a purple color palette with various shades for use in visualizations.
#'
#' @return A character vector of hexadecimal color codes representing the purple color palette.
#'
#' @examples
#' purple_colors <- colors.purples()
#' plot(1:7, col = purple_colors, pch = 19, cex = 3)
#'
#' @export
colors.purples = function() {
  palette = c(
    '#f1eff6',
    '#d5d5e8',
    '#9995c6',
    '#796fb2',
    '#694ea2',
    '#52258e',
    '#3f007d'
  )
  return(palette)
}
