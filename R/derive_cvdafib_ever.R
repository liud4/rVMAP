derive_cvdafib_ever <- function(data = merged.df) {

  data <- data %>%
    arrange(
      map.id, epoch
    ) %>%
    mutate(
      cvd.ever.factor = case_when(
        epoch %in% 0 ~ as.character(cvd.factor),
        cvd.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      ),
      afib.ever.factor = case_when(
        epoch %in% 0 ~ as.character(afib.factor),
        afib.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      ),
      cvdafib.ever.factor = case_when(
        epoch %in% 0 ~ as.character(cvdafib.factor),
        cvdafib.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(map.id) %>%
    fill(
      cvd.ever.factor, .direction = "downup"
    ) %>%
    fill(
      afib.ever.factor, .direction = "downup"
    ) %>%
    fill(
      cvdafib.ever.factor, .direction = "downup"
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
  label(data$cvd.ever.factor) <- "Prevalent CVD or AFib at any time point"

  return(data)
}
