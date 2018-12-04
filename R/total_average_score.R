#' Calculate the total (sum) or the average (mean) score of a numeric vector.
#'
#' @param vec A numeric vector.
#' @param threshold A number between 0 and 1 that indicates the minimum acceptable proportion of non-missing values in \code{vec}.
#' @param digits A non-negative integer specifying the number of digits to round the score to.
#' @return The sum or mean of the numbers contained in \code{vec} rounded to \code{round} digits. If the proportion of non-missing values in \code{vec} is less than \code{threshhold}, the functions will return NA.
#' @family scoring functions

total_score <- function(vec, threshold = 0.85) {
  # calc tot score only if >= <threshold> of items are non-missing
  if (proportion_non_missing(vec) < threshold) {
    return(NA)
  } else {
    vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
    return(sum(vec))
  }
}

#' @rdname total_score
#' @examples
#' average_score(c(1, 2, NA, 4))
average_score <- function(vec, threshold = 0.85, digits = 1) {
  # calc avg score only if >= <threshold> of items are non-missing
  if (proportion_non_missing(vec) < threshold) {
    return(NA)
  } else {
    return(mean(vec, na.rm = TRUE))
  }
}
