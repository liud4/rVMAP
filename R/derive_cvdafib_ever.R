derive_cvdafib_ever <- function(data = merged.df) {

  data <- data %>%
    group_by(
      map.id
    ) %>%
    mutate(
      cvd.ever.factor = case_when(
        all(is.na(cvd.factor)) ~ NA_character_,
        all(cvd.factor == "No", na.rm = TRUE) ~ "No",
        TRUE ~ "Yes"
      ),
      afib.ever.factor = case_when(
        all(is.na(afib.factor)) ~ NA_character_,
        all(afib.factor == "No", na.rm = TRUE) ~ "No",
        TRUE ~ "Yes"
      ),
      cvdafib.ever.factor = case_when(
        all(is.na(cvdafib.factor)) ~ NA_character_,
        all(cvdafib.factor == "No", na.rm = TRUE) ~ "No",
        TRUE ~ "Yes"
      )
    ) %>%
    ungroup() %>%
    mutate(
      cvd.ever.factor = factor(cvd.ever.factor, levels = c("No", "Yes")),
      afib.ever.factor = factor(afib.ever.factor, levels = c("No", "Yes")),
      cvdafib.ever.factor = factor(cvdafib.ever.factor, levels = c("No", "Yes"))
    ) %>%
    as.data.frame()

  label(data$cvd.ever.factor) <- "CVD, determined from variables in med hx, at any time point"
  label(data$afib.ever.factor) <- "A-fib, determined by med hx and/or echo, at any time point"
  label(data$cvd.ever.factor) <- "Prevalent CVD or AFib, at any time point"

  return(data)
}
