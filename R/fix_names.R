#' This function replaces underscores with periods in a data frame's variable names.
#'
#' @param data A data frame.
#' @return \code{data} with underscores replaced with periods in the column names.
#' @export

fix_names <- function(data) {
  names(data) <- gsub("\\_", "\\.", names(data))

  return(data)
}
