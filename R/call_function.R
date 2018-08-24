#' Calls a function on a data set and prints a table of added variables.
#'
#' @param function_name The name of a sourced function.
#' @param data A data frame containing VMAC variables.
#' @return \code{data} modified by \code{function_name} along with a data frame listing added variables.
#' @export

call_function <- function(function_name, data, ...) {
  oldnames <- names(data)
  otherArgs <- list(...)

  # cat("Calling function ", functionName, "().",
  #     "\n    Purpose: ", purpose, ".\n", sep= "")

  data <- do.call(function_name, c(list(data = data), otherArgs))

  newnames <- names(data)
  addedvars <- setdiff(newnames, oldnames)

  # cat("New variables:\n")

  if (length(addedvars) == 0) {
    cat("This function did not create any new variables.\n")
  } else {
    addedvars <- data.frame(addedvars)
    names(addedvars) <- "New Variables"
    print(addedvars)
  }

  # cat("\nCurrent dimensions:", dim(dat), "\n")
  # cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  return(data)
}
