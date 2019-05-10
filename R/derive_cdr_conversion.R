#' Derive CDR conversion data.
#'
#' @param data A data frame containing VMAP data.
#' @return A processed version of \code{data}.
#' @export

derive_cdr_conversion <- function(data) {

  data <- data %>%
    mutate(
      cdr_factor = as.numeric(as.character(cdr_factor))
    )
    group_by(
      map_id
    ) %>%
    mutate(
      cdr_conversion = case_when(
        dplyr::first(diagnosis) %in% "Normal" ~ case_when(
          cdr_factor == dplyr::first(cdr_factor) ~ "Stable",
          cdr_factor > dplyr::first(cdr_factor) ~ "Conversion",
          cdr_factor < dplyr::first(cdr_factor) ~ "Reversion"
        ),
        dplyr::first(diagnosis) %in% c("MCI", "Ambiguous At Risk") ~ case_when(
          cdr_factor == dplyr::first(cdr_factor) ~ "Stable",
          cdr_factor > dplyr::first(cdr_factor) ~ case_when(
            dplyr::first(cdr_factor) == 0 & cdr_factor == 0.5 ~ "Stable",
            TRUE ~ "Conversion"
          ),
          cdr_factor < dplyr::first(cdr_factor) ~ case_when(
            dplyr::first(cdr_factor) == 0.5 & cdr_factor == 0 ~ "Stable",
            TRUE ~ "Reversion"
          )
        ),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()

    label(data$cdr_conversion) <- "CDR Conversion"

  return(data)
}
