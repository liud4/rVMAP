format_id <- function(data) {
  data %<>%
    arrange(map_id) %>%
    mutate(
      map_id = gsub("\\-\\-[0-9]", "", map_id),
      map_id = as.numeric(map_id)
    ) %>%
    filter(
      !is.na(map_id) & map_id > 0
    ) %>%
    mutate(
      map_id = formatC(map_id, width = 3, format = "d", flag = "0")
    ) %>%
    distinct(map_id, .keep_all = TRUE)
}
