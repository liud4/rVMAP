consolidate_variable_names_and_labels <- function(MAPfreeze.list = MAPfreeze.list) {
  within_epoch.df <- NULL
  all_variables_labels.df <- NULL

  for (epoch.i in names(MAPfreeze.list)[-1]) {
    for (dataset.i in names(MAPfreeze.list[[epoch.i]][["data"]])) {
      my.df <- NULL
      if (grepl("\\.static", dataset.i)) {
        my.df <- dplyr::tibble(
          epoch = epoch.i,
          dataset = dataset.i,
          field_name = names(MAPfreeze.list[[epoch.i]][["data"]][[dataset.i]]),
          field_label = NA_character_
        )
      } else {
        my.df <- dplyr::tibble(
          epoch = epoch.i,
          dataset = dataset.i,
          field_name = MAPfreeze.list[[epoch.i]][["metadata"]][[dataset.i]] %>% pull(field_name),
          field_label = MAPfreeze.list[[epoch.i]][["metadata"]][[dataset.i]] %>% pull(field_label)
        )
      }
      within_epoch.df <- rbind(within_epoch.df, my.df)
    }
    all_variables_labels.df <- rbind(all_variables_labels.df, within_epoch.df)
  }

  return(all_variables_labels.df)
}
