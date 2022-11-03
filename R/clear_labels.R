#' Remove Hmisc labels from a labelled data frame and return the unlabelled data frame along with a list of the labels.
#'
#' @param data A labelled data frame or list
#' @return \code{data} without labels and a named character vector containing the labels with object name \code{{data}_labels}.
#' @export
#'
#' @examples
#' \dontrun{
#' mydat.nolabels.df <- clear_labels(mydat)
#' }

clear_labels <- function(data) {
  if (is.list(data)) {
    for (i in 1:length(data)) {
      class(data[[i]]) <- setdiff(class(data[[i]]), 'labelled')
    }
    for (i in 1:length(data)) {
      attr(data[[i]], "label") <- NULL
    }
  } else {
    class(data) <- setdiff(class(data), "labelled")
    attr(data, "label") <- NULL
  }

  return(data)
}

clear_units <- function(data) {
  if (is.list(data)) {
    for (i in 1:length(data)) {
      attr(data[[i]], "units") <- NULL
    }
  } else {
    attr(data, "units") <- NULL
  }

  return(data)
}

clear_labels_and_units <- function(data) {
  if (is.list(data)) {
    for (i in 1:length(data)) {
      class(data[[i]]) <- setdiff(class(data[[i]]), 'labelled')
    }
    for (i in 1:length(data)) {
      attr(data[[i]], "label") <- NULL
      attr(data[[i]], "units") <- NULL
    }
  } else {
    class(data) <- setdiff(class(data), "labelled")
    attr(data, "label") <- NULL
    attr(data, "units") <- NULL
  }

  return(data)
}
