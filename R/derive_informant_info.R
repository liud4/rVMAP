#' Derive, label, and add informant information variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added informant information variables.
#' @export

derive_informant_info <- function(data) {
  data <- within(data, {
    # convenience var for use in calculation of inform.length.totyrs
    # AJ confirmed in email 23 Feb 2015 that she wants to impute 0 for mos
    # in cases where yrs is nonmissing but mos is missing
    inform.length.mos <- as.numeric(inform.length.mos) ## "inform.length.mos" becomes a labeled character
    label(inform.length.mos) <- "Length of Time Informant Has Known Participant - months"
    inform.length.imputedmosInYrs <- ifelse(
      is.na(inform.length.yrs) & is.na(inform.length.mos), NA,
      ifelse(
        !is.na(inform.length.yrs) & !is.na(inform.length.mos),
        inform.length.mos,
        0)) / 12
  })

  data$inform.length.totyrs <-
    round(rowSums(data[, c("inform.length.yrs", "inform.length.imputedmosInYrs")]), 2)

  data <- within(data, {
    label(inform.length.imputedmosInYrs) <- "Convenience var for use in calculation of inform.length.totyrs"
    label(inform.length.totyrs) <- "Total yrs informant has known participant (calculated)"
  })

  return(data)
}
