process_factor_variables <- function(data, data_label, epoch = current_epoch) {
  current_data.df <- data

  if (!grepl("\\.static$", data_label)) {
    metadata.df <- MAPfreeze.list[[epoch]][["metadata"]][[data_label]] %>%
      filter(
        field_type == "radio"
      ) %>%
      select(
        c("field_name", "field_label", "select_choices_or_calculations")
      ) %>%
      as_tibble()

    if (!dim(metadata.df)[1] > 0) {
      metadata.df <- NULL
    }

    if (!is.null(metadata.df)) {
      metadata.df <- metadata.df %>%
        filter(
          field_name %in% names(current_data.df)
        )

      for (factor_var.i in metadata.df$field_name) {
        # current_var <- metadata.df[factor_var.i, "field_name"]
        current_var <- factor_var.i
        levels.list <- as.list(REDCapR::checkbox_choices(metadata.df[metadata.df$field_name == factor_var.i, "select_choices_or_calculations"]))
        # levels.list$id <- as.character(1 + as.numeric(levels.list$id))

        # current_data.df[, current_var] <- factor(current_data.df[, current_var], levels = levels.list$id, labels = levels.list$label)
        current_data.df[[paste0(current_var, "_factor")]] <- factor(current_data.df[[current_var]], levels = levels.list$id, labels = levels.list$label)
      }
    }
  }

  return(current_data.df)
}
