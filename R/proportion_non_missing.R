#' Calculate the proportion of non-missing values in a vector.
#'
#' @param vec A vector.
#' @return A number between 0 and 1 indicating the proportion of non-missing values in \code{vec}.
#' @family scoring functions
#' @export

proportion_non_missing <- function(vec) round(mean(!is.na(vec)), 2)
