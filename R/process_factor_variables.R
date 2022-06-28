process_factor_variables <- function(data, metadata) {
  
  metadata.df <- metadata %>%
    filter(
      field_type == "radio"
    ) %>%
    select(
      c("field_name", "field_label", "select_choices_or_calculations")
    ) %>%
    as_tibble()

    if (!is.null(metadata.df)) {
      for (factor_var.i in metadata.df$field_name) {
        levels.list <- as.list(REDCapR::checkbox_choices(pull(metadata.df[metadata.df$field_name == factor_var.i, "select_choices_or_calculations"])))
        
        data[[paste0(factor_var.i, "_factor")]] <- factor(data[[factor_var.i]], levels = levels.list$id, labels = levels.list$label)
      }
    }

  return(data)
}
