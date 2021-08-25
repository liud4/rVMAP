derive_cvdafib_ever <- function(data = merged.df) {

  data <- data %>%
    arrange(
      map.id, epoch
    ) %>%
    mutate(
      cvd.ever.factor = case_when(
        epoch %in% 1 ~ as.character(cvd.factor),
        cvd.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      ),
      afib.ever.factor = case_when(
        epoch %in% 1 ~ as.character(afib.factor),
        afib.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      ),
      cvdafib.ever.factor = case_when(
        epoch %in% 1 ~ as.character(cvdafib.factor),
        cvdafib.factor %in% "Yes" ~ "Yes",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(map.id) %>%
    fill(
      cvd.ever.factor, .direction = "down"
    ) %>%
    fill(
      afib.ever.factor, .direction = "down"
    ) %>%
    fill(
      cvdafib.ever.factor, .direction = "down"
    ) %>%
    ungroup() %>%
    mutate(
      cvd.ever.factor = factor(cvd.ever.factor, levels = c("No", "Yes")),
      afib.ever.factor = factor(afib.ever.factor, levels = c("No", "Yes")),
      cvdafib.ever.factor = factor(cvdafib.ever.factor, levels = c("No", "Yes"))
    ) %>%
    as.data.frame()

  return(data)
}
