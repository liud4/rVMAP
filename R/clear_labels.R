#' A function that removes Hmisc labels from a labelled data frame
#'
#' @param data A labelled data frame or list
#' @return The input data frame without labels and a named character vector containing the labels with object name {data}_labels.
#' @export
#'
#' @examples
#' clear_labels(mydat)

clear_labels <- function(data) {
  labels.list.name <- paste0(deparse(substitute(data)), "_labels")
  labels.list <- sapply(data, function(x) attr(x, "label"))

  if (is.list(data)) {
    for (i in 1:length(data)) class(data[[i]]) <- setdiff(class(data[[i]]), 'labelled')

    for (i in 1:length(data)) attr(data[[i]], "label") <- NULL
  }
  else {
    class(data) <- setdiff(class(data), "labelled")
    labels.list <- attr(data, "label")
    attr(data, "label") <- NULL
  }
  assign(labels.list.name, labels.list, envir = .GlobalEnv)
  return(data)
}
