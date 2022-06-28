process_raw_data <- function(data) {
  data <- data %>%
    mutate(
      epoch = case_when(
         redcap_event_name == "eligibility_arm_1" ~ 0L,
         redcap_event_name == "enrollmentbaseline_arm_1" ~ 1L,
         redcap_event_name == "18month_followup_arm_1" ~ 2L,
         redcap_event_name == "3year_followup_arm_1" ~ 3L,
         redcap_event_name == "5year_followup_arm_1" ~ 4L,
         redcap_event_name == "7year_followup_arm_1" ~ 5L,
         redcap_event_name == "9year_followup_arm_1" ~ 6L,
         redcap_event_name == "11year_followup_arm_1" ~ 7L,
         redcap_event_name == "13year_followup_arm_1" ~ 8L
      ),
      .after = "map_id"
    ) %>%
    mutate(
      across(
        where(is.numeric),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), restrict.sign = TRUE)
      ),
      across(
        where(is.character),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      ),
      across(
        starts_with("ecogself"),
        ~ missing_to_na(., equal.val = 0, restrict.sign = TRUE)
      )
    )

  var_w_comparison_operators <- NULL

  var_w_comparison_operators <- grep(
    "bld|np|csf|biomarkers",
    grep(
      "notes|flow|occup",
      names(mydat)[grepl("(>|<)([[:space:]]*)(\\d+)", mydat)],
      invert = T,
      v = T),
    v = T)

  var_w_comparison_operators <- setdiff(var_w_comparison_operators, "bld_c_coag")

  if (length(var_w_comparison_operators) > 0) {
    for (var in var_w_comparison_operators) {
      data[, var] <- sapply(data[, var], process_comparison_operators, USE.NAMES = FALSE)
    }
  }

  data <- invalidate_raw_neuropsych_items(data)

  data <- process_calculated_fields(data = data, data_label = "main", epoch = current_epoch)

  mydat <- process_factor_variables(data = mydat, data_label = "main", epoch = current_epoch)

  mydat <- convert_dates(mydat)
}

