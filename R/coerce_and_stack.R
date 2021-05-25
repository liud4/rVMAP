#' Check whether variables with the same name in two data frames have different storage modes. If so, the numeric vector is coerced to character. The rows of the new data frame are then added to the old data frame.
#'
#' @param new_data A data frame to be bound to another data frame by rows.
#' @param old_data A data frame to which \code{new_data} will be bound by rows.
#' @return A data frame with \code{new_data} bound by rows to \code{old_data}.
#' @export

coerce_and_stack <- function(new_data, old_data) {
  shared.names <- intersect(names(new_data), names(old_data))
  if (length(shared.names) > 0) {
    different_modes.df <-  rbind(c(
      `Variable` = "delete",
      `Mode in New Data Set` = "delete",
      `Mode in Old Data Set` = "delete"
    ))
    for (vname in shared.names) {
      new_vec <- new_data[[vname]]
      old_vec <- old_data[[vname]]
      modes <- c(mode(new_vec), mode(old_vec))
      # If all the values are NA, R thinks the mode is 'logical'.
      #   In that case we don't care whether the modes are different.
      # if ((!identical(modes[1], modes[2])) & !(all(is.na(new_vec)) | all(is.na(old_vec)))) {
      if ((!identical(modes[1], modes[2]))) {
        new_row <- c(
          `Variable` = vname,
          `Mode in New Data Set` = mode(new_vec),
          `Mode in Old Data Set` = mode(old_vec)
        )
        different_modes.df <- rbind.data.frame(different_modes.df, new_row, make.row.names = FALSE, stringsAsFactors = FALSE)

        if (any(grepl("character", modes, fixed = TRUE)) & any(grepl("numeric", modes, fixed = TRUE))) {
          if (modes[1] == "numeric") {
            new_data[[vname]] <- as.character(new_data[[vname]])
          } else {
            old_data[[vname]] <- as.character(old_data[[vname]])
          }
        }
      }
    }
    different_modes.df <- different_modes.df[-1, ]

    if (all(!is.null(nrow(different_modes.df)) & nrow(different_modes.df) > 0)) {
      cat("There are differences in storage modes in the following variables:\n")
      print(different_modes.df, row.names = FALSE)
      cat("Numeric variables were coerced to character.\n")
    } else {
      cat("Variables in the new and old data sets share the same storage modes.\n")
    }

  }
  return(dplyr::bind_rows(old_data, new_data))
}
