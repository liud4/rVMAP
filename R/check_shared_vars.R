#' A preprocessing function to check if two data frames both contain variables with the same name. If a warning is printed, merging the two data frames will result in variables with suffixes ".x" and ".y".
#'
#' @param data1 A data frame.
#' @param data2 A second data frame.
#' @param merge.by A variable name or a list of variable names to exclude in the check.
#' @return If any shared variables are found, a warning with the list of shared variables.
#' @export

check_shared_vars <- function(data1, data2, merge.by) {
  sharedNames <- intersect(names(data1), setdiff(names(data2), merge.by))
  if (length(sharedNames)) {
    warning(
      paste0(
        "[warning] The datasets (", deparse(substitute(data1)), " and ", deparse(substitute(data2)), ") share the following variable names: ",
        paste(sharedNames, collapse = ", "), ". \n"
      ),
      immediate. = TRUE
    )
  } else {
    message(
      paste0("The datasets (", deparse(substitute(data1)), " and ", deparse(substitute(data2)), ") do not share any variable names except: ", deparse(substitute(merge.by)), ". \n")
    )
  }
}

