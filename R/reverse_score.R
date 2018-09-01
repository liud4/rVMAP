#' Reverse the scores in a vector by subtraction.
#'
#' @param vec A numeric vector.
#' @return \code{vec} with values reversed, based on which function was used.
#' @family scoring functions
#' @name reverse
NULL

#' @describeIn reverse Reverse the scores in a vector that normally range from 0 to 1.
reverse0to1 <- function(vec) ifelse(is.na(vec), NA, 1 - vec)

#' @describeIn reverse Reverse the scores in a vector that normally range from 1 to 3.
reverse1to3 <- function(vec) ifelse(is.na(vec), NA, 4 - vec)

#' @describeIn reverse Reverse the scores in a vector that normally range from 1 to 7.
reverse1to7 <- function(vec) ifelse(is.na(vec), NA, 8 - vec)
