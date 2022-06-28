process_neuroimaging_link <- function(merged_data = merged.df, scanner_data = MAPfreeze.list$scanner$data) {

  scanner.df <- scanner_data %>%
    process_factor_variables(data = ., metadata = MAPfreeze.list$scanner$metadata) %>%
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
  
  session.var <- grep("session.id", names(merged_data), v = T)
  
  sub_routine <- function(current_session.var) {
    # browser()
    current_session.prefix <- gsub("session.id", "", current_session.var)
    current_session_ids <- merged_data[[paste0(current_session.prefix, "session.id")]]
    current_session_ids <- current_session_ids[!is.na(current_session_ids)]
    
    current_scanner.df <- scanner.df %>%
      filter(
        session.id %in% current_session_ids
      ) %>%
      setNames(
        paste0(current_session.prefix, names(scanner.df))
      )
    
    temp.label <- Hmisc::label(merged_data[[current_session.var]]) 
    
    attr(merged_data[[current_session.var]], "units") <- NULL
    attr(merged_data[[current_session.var]], "label") <- NULL
    # label(current_scanner.df[[current_session.var]])
    
    # Hmisc::label(merged_data[[current_session.var]]) <- NULL
    
    merged_data <- left_join(x = merged_data, y = current_scanner.df, by = current_session.var)
    
    Hmisc::label(merged_data[[current_session.var]]) <- temp.label
    
    return(merged_data)
  }
  
  # merged_data <- sub_routine(var.i)
  
  # merged_data.bkup <- merged_data
  for (var.i in session.var) {
    merged_data <- sub_routine(var.i)
  }
  
  return(merged_data)
}
