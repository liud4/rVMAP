#' Check whether common variables in two data frames have different storage modes. If so, the numeric vector is coerced to character. The rows of the new data frame are added to the old data frame.
#'
#' @param new_data A data frame to be bound to another data frame by rows.
#' @param old_data A data frame to which \code{new_data} will be bound by rows.
#' @return A data frame with \code{new_data} bound by rows to \code{old_data}.
#' @export

coerce_and_stack <- function(new_data, old_data) {
  shared.names <- intersect(names(new_data), names(old_data))
  if (length(shared.names) > 0) {
    for (vname in shared.names) {
      new_vec <- new_data[[vname]]
      old_vec <- old_data[[vname]]
      modes <- c(mode(new_vec), mode(old_vec))
      # If all the values are NA, R thinks the mode is 'logical'.
      #   In that case we don't care whether the modes are different.
      if ((!identical(modes[1], modes[2])) & !(all(is.na(new_vec)) | all(is.na(old_vec)))) {
        cat("\nThere are differences in the R storage modes of the following variables:\n")
        cat("\n-----------------------------\n-->", vname, "\n")
        cat("New:", mode(new_vec), "\n")
        cat("Old:", mode(old_vec), "\n")
        if (any(grepl("character", modes, fixed = TRUE)) & any(grepl("numeric", modes, fixed = TRUE))) {
          cat("\nCoercing the numeric variable to character before stacking.\n")
          if (modes[1] == "numeric") {
            new_data[[vname]] <- as.character(new_data[[vname]])
          } else {
            old_data[[vname]] <- as.character(old_data[[vname]])
          }
        }
      }
    }
  }

  return(dplyr::bind_rows(old_data, new_data))
}
