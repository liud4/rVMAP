format_id <- function(data) {
  data %<>%
    mutate(
      map_id = as.numeric(map_id)
    ) %>%
    filter(
      !is.na(map_id) & map_id > 0
    ) %>%
    mutate(
      map_id = formatC(map_id, width = 3, format = "d", flag = "0")
    )

  cat("Variable `map_id` has been formatted appropriately.")
}
