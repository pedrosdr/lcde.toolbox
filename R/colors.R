# class colors

# methods

#' Rescale Color Palette
#'
#' Takes an existing color palette and remaps its colors from one numeric interval to another,
#' returning a vector of the same length with interpolated hex codes.
#'
#' @param palette A character vector of hex color codes.
#' @param min_old Numeric scalar. Lower bound of the original value range.
#' @param max_old Numeric scalar. Upper bound of the original value range.
#' @param min_new Numeric scalar. Lower bound of the target value range.
#' @param max_new Numeric scalar. Upper bound of the target value range.
#'
#' @return A character vector of hex color codes, same length as `palette`,
#'   with colors interpolated according to the new numeric range.
#'
#' @examples
#' # stretch a 5-color palette from [0,1] to [10,20]
#' pal <- c("#000000", "#555555", "#AAAAAA", "#FFFFFF", "#FF0000")
#' colors.rescale_palette(pal, min_old = 0, max_old = 1, min_new = 10, max_new = 20)
#'
#' @export
colors.rescale_palette <- function(
    palette, # character vector
    min_old, # number
    max_old, # number
    min_new, # number
    max_new  # number
) {
  type.check_character(palette, "palette")
  type.check_numeric(min_old, "min_old")
  type.check_numeric(max_old, "max_old")
  type.check_numeric(min_new, "min_new")
  type.check_numeric(max_new, "max_new")

  # number of output colors = length of the original palette
  n_out <- length(palette)

  # create the color interpolation function
  ramp_fn <- grDevices::colorRamp(palette)

  # generate n_out equally spaced points in the new range
  xs_new <- seq(min_new, max_new, length.out = n_out)

  # normalize these points to [0, 1] relative to the old range
  t <- (xs_new - min_old) / (max_old - min_old)
  t <- pmin(pmax(t, 0), 1)   # clamp values to stay within [0,1]

  # apply the ramp (returns an n_out×3 matrix with values from 0 to 255)
  rgb_mat <- ramp_fn(t)

  # convert matrix to hex colors and return
  res <- grDevices::rgb(
    red   = rgb_mat[, 1],
    green = rgb_mat[, 2],
    blue  = rgb_mat[, 3],
    maxColorValue = 255
  )

  return(res)
}

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
    '#6c584c',
    "#2b5f7f",
    "#7a3c74",
    "#d1c05a",
    "#d14f47",
    "#7bb086",
    "#b72030"
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
    '#e7e8e3',
    '#d7cac1',
    '#bfab9e',
    '#a78d7c',
    '#8f6f5a',
    '#775037',
    '#5f3215'
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

#' Sunset Glow Palette
#'
#' This function returns a "Sunset Glow" palette with warm, smooth gradients from light peach to deep burgundy.
#'
#' @return A character vector of hexadecimal color codes representing the Sunset Glow palette.
#'
#' @examples
#' sunset_glow_colors <- colors.sunset_glow()
#' plot(1:7, col = sunset_glow_colors, pch = 19, cex = 3)
#'
#' @export
colors.sunset_glow <- function() {
  palette <- c(
    '#FFDFC4',
    '#FFB69E',
    '#FF8A75',
    '#FF5E4D',
    '#E73C3E',
    '#B82743',
    '#7C1D4A'
  )
  return(palette)
}

#' Ocean Depths Palette
#'
#' This function returns an "Ocean Depths" palette with cool blue tones inspired by sea layers.
#'
#' @return A character vector of hexadecimal color codes representing the Ocean Depths palette.
#'
#' @examples
#' ocean_depths_colors <- colors.ocean_depths()
#' plot(1:7, col = ocean_depths_colors, pch = 19, cex = 3)
#'
#' @export
colors.ocean_depths <- function() {
  palette <- c(
    '#E3F2FD',
    '#B3E5FC',
    '#81D4FA',
    '#4FC3F7',
    '#29B6F6',
    '#03A9F4',
    '#0288D1'
  )
  return(palette)
}

#' Earth & Forest Palette
#'
#' This function returns an "Earth & Forest" palette with natural, muted green and brown tones.
#'
#' @return A character vector of hexadecimal color codes representing the Earth & Forest palette.
#'
#' @examples
#' earth_forest_colors <- colors.earth_forest()
#' plot(1:7, col = earth_forest_colors, pch = 19, cex = 3)
#'
#' @export
colors.earth_forest <- function() {
  palette <- c(
    '#F1E0B0',
    '#DCC48E',
    '#BFA56D',
    '#8A8C5A',
    '#5F7F4F',
    '#31734C',
    '#1A5A3F'
  )
  return(palette)
}

#' Pastel Bloom Palette
#'
#' This function returns a "Pastel Bloom" palette with soft, delicate spring colors.
#'
#' @return A character vector of hexadecimal color codes representing the Pastel Bloom palette.
#'
#' @examples
#' pastel_bloom_colors <- colors.pastel_bloom()
#' plot(1:7, col = pastel_bloom_colors, pch = 19, cex = 3)
#'
#' @export
colors.pastel_bloom <- function() {
  palette <- c(
    '#FFEAF6',
    '#FFDDEC',
    '#F5E1D3',
    '#DCE5D0',
    '#D0E7F2',
    '#E4D9F5',
    '#F7D4E9'
  )
  return(palette)
}

#' Vibrant Spectrum Palette
#'
#' This function returns a "Vibrant Spectrum" palette with bright, high-contrast hues.
#'
#' @return A character vector of hexadecimal color codes representing the Vibrant Spectrum palette.
#'
#' @examples
#' vibrant_spectrum_colors <- colors.vibrant_spectrum()
#' plot(1:7, col = vibrant_spectrum_colors, pch = 19, cex = 3)
#'
#' @export
colors.vibrant_spectrum <- function() {
  palette <- c(
    '#FF2E63',
    '#FF9F1C',
    '#FFCB47',
    '#2EC4B6',
    '#009B77',
    '#B5179E',
    '#7209B7'
  )
  return(palette)
}

#' Cool–Warm Diverging Palette
#'
#' This function returns a "Cool–Warm Diverging" palette for indicating deviation around a midpoint.
#'
#' @return A character vector of hexadecimal color codes representing the Cool–Warm Diverging palette.
#'
#' @examples
#' cool_warm_diverging_colors <- colors.cool_warm_diverging()
#' plot(1:7, col = cool_warm_diverging_colors, pch = 19, cex = 3)
#'
#' @export
colors.cool_warm_diverging <- function() {
  palette <- c(
    '#2166AC',
    '#4393C3',
    '#92C5DE',
    '#F7F7F7',
    '#F4A582',
    '#D6604D',
    '#B2182B'
  )
  return(palette)
}
