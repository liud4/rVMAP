process_neuroimaging_link <- function(scanner_data = MAPfreeze.list$epoch_0$data$scanner, data = merged.df) {
  scanner.df <- scanner_data %>%
    process_factor_variables(., data_label = "scanner", epoch = "epoch_0") %>%
    format_id() %>%
    select(
      session_id, scanner_factor, head_coil_factor, scanner_software_factor
    ) %>%
    format_names()

  session.var <- grep("session.id", names(data), v = T)

  sub_routine <- function(current_session.var) {
    current_session.prefix <- gsub("session.id", "", current_session.var)
    current_session_ids <- data[[paste0(current_session.prefix, "session.id")]]
    current_session_ids <- current_session_ids[!is.na(current_session_ids)]

    current_scanner.df <- scanner.df %>%
      filter(
        session.id %in% current_session_ids
      ) %>%
      setNames(
        paste0(current_session.prefix, names(scanner.df))
      )

    data <- merge(x = data, y = current_scanner.df, by = current_session.var, all.x = TRUE)

    return(data)
  }

  for (var.i in session.var) {
    data <- sub_routine(var.i)
  }

  return(data)
}
