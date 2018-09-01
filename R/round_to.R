#' Round the elements of a numeric vector to a specified decimal place.
#'
#' @param x A numeric vector.
#' @param k An integer indicating the number of decimals to show.
#' @return A character vector of length as \code{x} containing the rounded numbers.
#' @export
#'
#' @examples
#' round_to(pi, 5)
#' round_to(c(exp(1), 0, 1.3), 2)
#'
#' \dontrun{
#' round_to("pi", 5)
#' }

round_to <- function(x, k) trimws(format(round(x, k), nsmall = k))
