process_neuroimaging_link <- function(scanner_data = MAPfreeze.list$epoch_0$data$scanner, data = merged.df) {
  scanner.df <- scanner_data %>%
    process_factor_variables(., data_label = "scanner", epoch = "epoch_0") %>%
    format_id() %>%
    select(
      session_id, scanner_factor, head_coil_factor, scanner_software_factor
    ) %>%
    mutate(
      scanner_factor = forcats::fct_recode(
        scanner_factor,
        "3Tb" = "3Tb Philips Acheiva system",
        "3Ta" = "3Ta Philips Acheiva system",
        "3TbUDHW" = "3Tb Philips Acheiva system updated digital hardware"
      ),
      head_coil_factor = forcats::fct_recode(
        head_coil_factor,
        "8chSENSE" = "8 channel SENSE head coil",
        "8chdStream" = "dStream 8 channel head coil",
        "32chdStream" = "dStream 32 channel head coil"
      ),
      scanner_software_factor = forcats::fct_recode(
        scanner_software_factor,
        "3.2.2.0" = "3.2.2/3.2.2.0",
        "5.1.7.1" = "5.1.7/5.1.7.1",
        "5.3.0.2" = "5.3.0/5.3.0.2",
        "5.3.0.3" = "5.3.0/5.3.0.3",
        "5.6.1.0" = "5.6.1/5.6.1.0"
      ),
    ) %>%
    mutate(
      scanner_coil_factor = paste0(scanner_factor, "; ", head_coil_factor),
      scanner_coil_software_factor = paste0(scanner_factor, "; ", head_coil_factor, "; ", scanner_software_factor)
    ) %>%
    format_names()

  scanner.df[scanner.df$scanner.coil.factor == "NA; NA", "scanner.coil.factor"] <- NA
  scanner.df[scanner.df$scanner.coil.software.factor == "NA; NA; NA", "scanner.coil.software.factor"] <- NA

  scanner.df <- scanner.df %>%
    mutate(
      scanner.coil.factor = forcats::as_factor(scanner.coil.factor),
      scanner.coil.software.factor = forcats::as_factor(scanner.coil.software.factor)
    )

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
