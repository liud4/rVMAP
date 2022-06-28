process_calculated_fields <- function(data, metadata) {
  
  metadata.df <- metadata %>%
    filter(
      field_type == "calc"
    ) %>%
    select(
      c("field_name", "select_choices_or_calculations")
    ) %>%
    as_tibble()
  
  if (!is.null(metadata.df)) {
    metadata.df$element_variables <- purrr::map( # extract variables used in calculation of this variable
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
      
      for (row.i in 1:dim(data)[1]) {
        if (any(is.na(data[row.i, current_components.var]))) {
          data[row.i, current.var] <- NA
        }
      }
      
    }
  }
  
  return(data)
}
