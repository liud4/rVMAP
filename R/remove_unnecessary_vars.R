remove_unnecesary_vars <- function(data) {
  to_remove.var <- which(names(data) %in% c("vmac_id", "entry_primary", "entry_secondary", "data_entry_complete",
                                            "vmac.id", "entry.primary", "entry.secondary", "data.entry.complete"))

  if (length(to_remove.var) > 0) {
    data <- data[, -to_remove.var]
  }

  return(data)
}
