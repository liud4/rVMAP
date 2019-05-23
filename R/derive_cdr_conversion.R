#' Derive CDR conversion data.
#'
#' @param data A data frame containing VMAP data.
#' @return A processed version of \code{data}.
#' @export

derive_cdr_conversion <- function(data) {

  data <- data %>%
    mutate(
      cdr = as.numeric(as.character(cdr))
    ) %>%
    group_by(
      map.id
    ) %>%
    mutate(
      cdr_conversion = case_when(
        dplyr::first(diagnosis) %in% "Normal" ~ case_when(
          cdr == dplyr::first(cdr) ~ "Stable",
          cdr > dplyr::first(cdr) ~ "Conversion",
          cdr < dplyr::first(cdr) ~ "Reversion"
        ),
        dplyr::first(diagnosis) %in% c("MCI", "Ambiguous At Risk") ~ case_when(
          cdr == dplyr::first(cdr) ~ "Stable",
          cdr > dplyr::first(cdr) ~ case_when(
            dplyr::first(cdr) == 0 & cdr == 0.5 ~ "Stable",
            TRUE ~ "Conversion"
          ),
          cdr < dplyr::first(cdr) ~ case_when(
            dplyr::first(cdr) == 0.5 & cdr == 0 ~ "Stable",
            TRUE ~ "Reversion"
          )
        ),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()

    data$cdr_conversion <- factor(data$cdr_conversion, levels = c("Stable", "Conversion", "Reversion"))

    label(data$cdr_conversion) <- "CDR Conversion"

  return(data)
}
