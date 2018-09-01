#' A variety of data comparison functions to use as checks prior to a destructive merge.
#'
#' @param new_data A data frame.
#' @param old_data Another data frame.
#' @return A tabulated list of differences between \code{new_data} and \code{old_data}.
#' @family comparison functions
#' @name comparison
NULL

#' @describeIn comparison Outputs a tabulated list of differences in variable names between \code{new_data} and \code{old_data}.
compare_variables <- function(new_data, old_data) {
  new_vars <- names(new_data)
  old_vars <- names(old_data)

  added_vars <- setdiff(new_vars, old_vars)
  num_added_vars <- length(added_vars)

  if (num_added_vars == 0) {
    message("The new data set does not contain any variables that are not in the old data set.\n")
  } else {
    cat("Variables in new data set but not in old:\n")
    added_vars <- data.frame(added_vars)
    names(added_vars) <- "Added Variables"
    print(added_vars, row.names = FALSE)
  }

  removed_vars <- setdiff(old_vars, new_vars)
  num_removed_vars <- length(removed_vars)

  if (num_removed_vars == 0) {
    message("The old data set does not contain any variables that are not in the new data set.\n")
  } else {
    cat("Variables in old data set but not in new:\n")
    removed_vars <- data.frame(removed_vars)
    names(removed_vars) <- "Removed Variables"
    print(removed_vars, row.names = FALSE)
  }
}

#' @describeIn comparison Outputs a tabulated list of differences in MAP IDs between \code{new_data} and \code{old_data}.
compare_ids <- function(new_data, old_data) {
  new_ids <- unique(new_data$map.id)
  old_ids <- unique(old_data$map.id)

  added_ids <- setdiff(new_ids, old_ids)
  num_added_ids <- length(added_ids)

  if (num_added_ids == 0) {
    message("The new data set does not contain any MAP IDs that are not in the old data set.\n")
  } else {
    cat("MAP IDs in new data set but not in old:\n")
    added_ids <- data.frame(added_ids)
    names(added_ids) <- "Added MAP IDs"
    print(added_ids, row.names = FALSE)
  }

  removed_ids <- setdiff(old_ids, new_ids)
  num_removed_ids <- length(removed_ids)

  if (num_removed_ids == 0) {
    message("The old data set does not contain any MAP IDs that are not in the new data set.\n")
  } else {
    cat("MAP IDs in old data set but not in new:\n")
    removed_ids <- data.frame(removed_ids)
    names(removed_ids) <- "Removed MAP IDs"
    print(removed_ids, row.names = FALSE)
  }
}

#' @describeIn comparison Outputs a tabulated list of differences in factor levels between \code{new_data} and \code{old_data}.
compare_factors <- function(new_data, old_data) {
  shared.names <- intersect(names(new_data), names(old_data))
  different_levels.df <- rbind(c(
    `Variable` = "delete",
    `Levels in New Data Set` = "delete",
    `Levels in Old Data Set` = "delete"
  ))
  for (vname in shared.names) {
    new_var <- new_data[[vname]]
    old_var <- old_data[[vname]]
    if (!identical(levels(new_var), levels(old_var))) {
      new_row <- c(
        Variable = vname,
        `Levels in New Data Set` = levels(new_var),
        `Levels in Old Data Set` = levels(old_var)
      )
      different_levels.df <- rbind.data.frame(different_levels.df, new_row, make.row.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (nrow(different_levels.df) > 1) {
    cat("There are differences in levels in the following categorical variables:\n")
    print(different_levels.df[-1, ], row.names = FALSE)
  } else {
    message("Categorical variables in the new and old data sets share the same levels.\n")
  }
}
