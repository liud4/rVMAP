#' Replace underscores with periods in a data frame's variable names.
#'
#' @param data A data frame.
#' @return \code{data} with underscores replaced with periods in the variable names.
#' @export

format_names <- function(data) {
  names(data) <- gsub("\\_", "\\.", names(data))

  return(data)
}
