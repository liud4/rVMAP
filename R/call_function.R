#' Call a function on a data set and print a table of added variables.
#'
#' @param function_name The name of a sourced function.
#' @param data A data frame containing VMAC variables.
#' @param ... Additional arguments to be passed on to the function in \code{function_name}.
#' @return \code{data} modified by \code{function_name} along with a data frame listing added variables.
#' @export

call_function <- function(function_name, data, ...) {
  oldnames <- names(data)
  otherArgs <- list(...)

  data <- do.call(function_name, c(list(data = data), otherArgs))

  newnames <- names(data)
  addedvars <- setdiff(newnames, oldnames)

  if (length(addedvars) == 0) {
    cat(paste0("The ", function_name, "() function did not create any new variables.\n"))
  } else {
    cat(paste0("The ", function_name, "() function created the following new variables:\n"))
    addedvars <- data.frame(addedvars)
    names(addedvars) <- "New Variables"
    print(addedvars)
  }

  return(data)
}
