#' Check whether common variables in two data frames have different storage modes. If so, the numeric vector is coerced to character. The rows of the new data frame are added to the old data frame.
#'
#' @param newdat A data frame to be bound to another data frame by rows.
#' @param olddat A data frame to which `newdat` will be bound by rows.
#' @return A data frame with `newdat` bound by rows to `olddat`.
#' @export

coerce_and_stack <- function(newdat, olddat) {
  shared.names <- intersect(names(newdat), names(olddat))
  if (length(shared.names) > 0) {
    for (vname in shared.names) {
      newcol <- newdat[[vname]]
      oldcol <- olddat[[vname]]
      modes <- c(mode(newcol), mode(oldcol))
      # If all the values are NA, R thinks the mode is 'logical'.
      #   In that case we don't care whether the modes are different.
      if ((!identical(modes[1], modes[2])) & !(all(is.na(newcol)) | all(is.na(oldcol)))) {
        cat("\nThere are differences in the R storage modes of the following variables:\n")
        cat("\n-----------------------------\n-->", vname, "\n")
        cat("New:", mode(newcol), "\n")
        cat("Old:", mode(oldcol), "\n")
        if (any(grepl("character", modes, fixed = TRUE)) & any(grepl("numeric", modes, fixed = TRUE))) {
          cat("\nCoercing the numeric variable to character before stacking.\n")
          if (modes[1] == "numeric") {
            newdat[[vname]] <- as.character(newdat[[vname]])
          } else {
            olddat[[vname]] <- as.character(olddat[[vname]])
          }
        }
      }
    }
  }
  dplyr::bind_rows(olddat, newdat)
}
