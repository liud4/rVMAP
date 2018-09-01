#' Derive, label, and add date variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added date variables.
#' @export

derive_dates <- function(data) {
  data <- within(data, {
    days.np.date.minus.bld.date <- as.numeric(difftime(np.date, bld.date, units = "days"))
    label(days.np.date.minus.bld.date) <- "Days between blood draw and np test (np-bld)"

    days.np.date.minus.abp.date <- as.numeric(difftime(np.date, abp.date, units = "days"))
    label(days.np.date.minus.abp.date) <- "Days between ABP and np test (np-abp)"
  })

  return(data)
}
