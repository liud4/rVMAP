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

      # current_data.df <- MAPfreeze.list[[epoch]][["data"]][[data_label]]
      # current_data.df <- data

      for (redcap_calculated_variables.i in 1:dim(metadata.df)[1]) {
        current.var <- pull(metadata.df[redcap_calculated_variables.i, "field_name"])
        current_components.var <- pull(metadata.df[redcap_calculated_variables.i, "element_variables"])[[1]]
        for (row.i in dim(current_data.df)[1]) {
          if (any(is.na(current_data.df[row.i, current_components.var]))) {
            current_data.df[row.i, current.var] <- NA
          }
        }
      }
    }
  }

  return(current_data.df)
}
