#' Derive CDR conversion data.
#'
#' @param data A data frame containing VMAP data.
#' @return A processed version of \code{data}.
#' @export

derive_cdr_conversion <- function(data) {

  data <- data %>%
    mutate(
      cdr.factor.redcap = cdr.factor,
      cdr.factor = as.numeric(as.character(cdr.factor))
    ) %>%
    group_by(
      map.id
    ) %>%
    mutate(
      cdr.conversion.factor = case_when(
        dplyr::first(diagnosis.factor) %in% "Normal" ~ case_when(
          cdr.factor == dplyr::first(cdr.factor) ~ "Stable",
          cdr.factor > dplyr::first(cdr.factor) ~ "Conversion",
          cdr.factor < dplyr::first(cdr.factor) ~ "Reversion"
        ),
        dplyr::first(diagnosis.factor) %in% c("MCI", "Ambiguous At Risk") ~ case_when(
          cdr.factor == dplyr::first(cdr.factor) ~ "Stable",
          cdr.factor > dplyr::first(cdr.factor) ~ case_when(
            dplyr::first(cdr.factor) == 0 & cdr.factor == 0.5 ~ "Stable",
            TRUE ~ "Conversion"
          ),
          cdr.factor < dplyr::first(cdr.factor) ~ case_when(
            dplyr::first(cdr.factor) == 0.5 & cdr.factor == 0 ~ "Stable",
            TRUE ~ "Reversion"
          )
        ),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()

  data$cdr.factor <- factor(data$cdr.factor, levels = c(0, 0.5, 1, 2, 3))

  data$cdr.conversion.factor <- factor(data$cdr.conversion.factor, levels = c("Stable", "Conversion", "Reversion"))

  label(data$cdr.conversion.factor) <- "CDR Conversion"

  return(data)

}
