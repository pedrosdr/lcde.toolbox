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
    '#9656a2',
    '#369acc',
    '#95cf92',
    '#f8e16f',
    '#f4895f',
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
    '#c4ae9f',
    '#b3a098',
    '#948b86',
    '#827d7a',
    '#bfb4b0'
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
