#' Function to format MAP IDs to strings with three digits.
#'
#' @param data A data frame that contains the variable \code{map_id} or \code{map.id}.
#' @return A data frame that contains the formatted variable \code{map_id} or \code{map.id}.
#' @export

format_id <- function(data) {
  if ("map_id" %in% names(data)) {
    data %>%
      dplyr::arrange(map_id) %>%
      dplyr::mutate(
        map_id = gsub("\\-\\-[0-9]", "", map_id),
        map_id = as.numeric(map_id)
      ) %>%
      dplyr::filter(
        !is.na(map_id) & map_id > 0
      ) %>%
      dplyr::mutate(
        map_id = formatC(map_id, width = 3, format = "d", flag = "0")
      ) %>%
      dplyr::distinct(map_id, .keep_all = TRUE)
  } else if ("map.id" %in% names(data)) {
    data %>%
      dplyr::arrange(map.id) %>%
      dplyr::mutate(
        map.id = gsub("\\-\\-[0-9]", "", map.id),
        map.id = as.numeric(map.id)
      ) %>%
      dplyr::filter(
        !is.na(map.id) & map.id > 0
      ) %>%
      dplyr::mutate(
        map.id = formatC(map.id, width = 3, format = "d", flag = "0")
      ) %>%
      dplyr::distinct(map.id, .keep_all = TRUE)
  } else {
    warning("This data frame does not contain a MAP ID variable.")
    data
  }
}
