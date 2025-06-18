# class utils

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
#' inequality <- utils.inequality_indicator(data)
#'
#' @export
utils.inequality_indicator = function(
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
#' magnitudes <- utils.magnitude(data)
#'
#' @export
utils.magnitude = function(
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
#' relative_magnitudes <- utils.relative_magnitude(data, target = 50)
#'
#' @export
utils.relative_magnitude = function(
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

  magnitude = utils.magnitude(data)
  magnitude_target = utils.magnitude(
    as.data.frame(matrix(target, nrow = 1))
  )
  relative_magnitude = magnitude / magnitude_target

  return(relative_magnitude)
}

#' Abbreviate School Names
#'
#' This function abbreviates school names by retaining only the most meaningful words, based on a specified size limit. Commonly ignored words are filtered out to produce a concise representation of each school name.
#'
#' @param school_names A character vector containing the names of the schools to be abbreviated.
#' @param size An integer specifying the maximum number of words to retain in the abbreviated name. Defaults to 2.
#'
#' @return A character vector of abbreviated school names with the first letter of each word capitalized.
#'
#' @details
#' The function converts all input school names to lowercase, splits them into individual words, and filters out predefined ignored words and numeric values. The remaining words are then concatenated based on the specified size limit.
#'
#' @examples
#' school_names <- c("Escola Municipal de Ensino Fundamental", "Colégio Dr. João da Silva")
#' abbreviated_names <- utils.abbreviate_school_names(school_names, size = 2)
#'
#' @export
utils.abbreviate_school_names = function(
  school_names, #: character vector
  size = 2 #: integer
) {
  type.check_character(school_names, 'school_names')
  type.check_integer(size, 'size')

  ignored_words = c(
    "escola", "municipal", "educacao", "basica", "fundamental", "ensino", "emef",
    "em", "da", "de", "do", "dos", "a", "o", "dr", "frei", 'vereador', "professora",
    "e", "m", "mul", "educ", "emeif", "profÂº", "profÂª", "profa", "emefi", 'emeb', 'di',
    '-', 'colegio', 'doutor', "1Âº", "grau", "dep", "deputado", "unidade",
    "desembargador", "creche", "prefeito", "fund", "governador", "senador",
    "b", "sra", "alm", "emefm", "professor", "estadual", "federal", "privada",
    "prof", "r", "profª", "esc", "indigena", "indígena")

  ignored_regex = c(
    "\\d+"
  )

  abbreviated = c()
  for(school_name in school_names) {
    school_name_full <- stringr::str_split(tolower(school_name), " ")[[1]]

    meaningful_words <- school_name_full[!(
      school_name_full %in% ignored_words |
        sapply(
          school_name_full,
          function(p) any(sapply(ignored_regex, function(r) grepl(r, p)))
        )
    )]

    abbreviated_school_name = paste(head(meaningful_words, size), collapse = " ")
    abbreviated = c(abbreviated, abbreviated_school_name)
  }

  return(tools::toTitleCase(abbreviated))
}

#' Get Proficiency Categories
#'
#' This function categorizes numeric proficiency values into discrete categories based on predefined thresholds.
#'
#' @param values A numeric vector containing proficiency values to be categorized.
#'
#' @return A factor vector representing the proficiency categories: 'D', 'C', 'B', or 'A'.
#'
#' @details
#' The function categorizes values as follows:
#' - 'D' for values less than 25
#' - 'C' for values between 25 and 49
#' - 'B' for values between 50 and 69
#' - 'A' for values 70 and above
#'
#' @examples
#' proficiency_values <- c(10, 30, 55, 80)
#' categories <- utils.get_percentage_of_proficiency_categories(proficiency_values)
#'
#' @export
utils.get_percentage_of_proficiency_categories = function(
  values #: numeric vector
) {
  type.check_numeric(values, 'values')

  categories = ifelse(
    values < 25, 'D', ifelse(
      values < 50, 'C', ifelse(
        values < 70, 'B', 'A'
      )
    )
  )

  categories = as.factor(categories)

  return(categories)
}

#' Get Proficiency Category Colors
#'
#' This function assigns colors to proficiency categories based on input numeric values and a specified color palette.
#'
#' @param values A numeric vector containing proficiency values for which colors will be assigned.
#' @param palette A character vector specifying the color palette to use. Defaults to a red-to-green gradient.
#'
#' @return A character vector of colors corresponding to the proficiency categories.
#'
#' @details
#' The function first categorizes the input values using `utils.get_percentage_of_proficiency_categories`. It then assigns a color to each category based on the provided palette, where:
#' - 'D' corresponds to the first color in the palette
#' - 'C' corresponds to the second color
#' - 'B' corresponds to the third color
#' - 'A' corresponds to the fourth color
#'
#' @examples
#' proficiency_values <- c(10, 30, 55, 80)
#' colors <- utils.get_percentage_of_proficiency_category_colors(proficiency_values)
#'
#' @export
utils.get_percentage_of_proficiency_category_colors = function(
    values, #: numeric vector
    palette = colors.red_to_green() #: character vector
) {
  type.check_character(palette, 'palette')
  type.check_numeric(values, 'values')

  categories = utils.get_percentage_of_proficiency_categories(values)
  colors = ifelse(
    categories == 'D', palette[1], ifelse(
      categories == 'C', palette[2], ifelse(
        categories == 'B', palette[3], palette[4]
      )
    )
  )

  return(colors)
}
