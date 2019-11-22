#' Invalidation of REDCap calculated fields.
#'
#' Numeric observations in VMAP data are coded as invalid with negative multiples of 1111 (e.g. -8888, -9999, etc.).
#' While these observations are converted to NA using \code{missing_to_na} in the \code{process_raw_data} script,
#' \code{missing_to_na} is unable to always detect invalid values in calculated fields from REDCap.
#' \code{process_calculated_fields} uses regular expressions to extract which variables were utilized in the
#' REDCap calculation of a given variable. If any of the component variables were invalidated by \code{missing_to_na},
#' \code{process_calculated_fields} will convert the corresponding calculated values to NA.
#'
#' @param data A dataframe containing data from REDCap.
#' @param data_label The label corresponding to the data as stored in MAPfreeze.list (used to locate metadata).
#' @param epoch An integer corresponding to the time period of data (used to locate metadata).
#' @return The input dataframe with properly invalidated observations in REDCap calculated variables.
#' @examples
#' apoe_processed.df <- process_calculated_fields(data = apoe.df, data_label = "APOE")
#' @export

process_calculated_fields <- function(data, data_label, epoch = current_epoch) {
  current_data.df <- data

  if (!grepl("\\.static$", data_label)) {
    metadata.df <- MAPfreeze.list[[epoch]][["metadata"]][[data_label]] %>%
      filter(
        field_type == "calc"
      ) %>%
      select(
        c("field_name", "select_choices_or_calculations")
      ) %>%
      as_tibble()

    if (dim(metadata.df)[1] == 0) {
      metadata.df <- NULL
    }

    if (!is.null(metadata.df)) {
      metadata.df$element_variables <- purrr::map(  # extract variables used in calculation of this variable
        metadata.df$field_name,
        function(x)
          gsub(
            "\\[|\\]",
            "",
            stringr::str_extract_all(
              metadata.df[metadata.df$field_name == x, "select_choices_or_calculations"],
              "\\[([^\\[\\]]+)\\]")[[1]]
          )
      )

      for (redcap_calculated_variables.i in 1:dim(metadata.df)[1]) {
        current.var <- pull(metadata.df[redcap_calculated_variables.i, "field_name"])
        current_components.var <- pull(metadata.df[redcap_calculated_variables.i, "element_variables"])[[1]]
        for (row.i in 1:dim(current_data.df)[1]) {
          if (any(is.na(current_data.df[row.i, current_components.var]))) {
            current_data.df[row.i, current.var] <- NA
          }
        }
      }
    }
  }

  return(current_data.df)
}
