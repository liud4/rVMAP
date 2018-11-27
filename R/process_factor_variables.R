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

    metadata.df <- ifelse(dim(metadata.df)[1] > 0, metadata.df, NULL)

    if (!is.null(metadata.df)) {
      for (factor_var.i in 1:dim(metadata.df)[1]) {
        current_var <- metadata.df[factor_var.i, "field_name"]
        levels.list <- as.list(REDCapR::checkbox_choices(metadata.df[factor_var.i, "select_choices_or_calculations"]))

        current_data.df[, current_var] <- factor(current_data.df[, current_var], levels = levels.list$id, labels = levels.list$label)
        # current_data.df[, paste0(current_var, "_factor")] <- factor(current_data.df[, current_var], levels = levels.list$id, labels = levels.list$label)
      }
    }
  }

  return(current_data.df)
}
