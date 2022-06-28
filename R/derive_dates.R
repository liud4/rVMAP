#' Derive, label, and add date variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added date variables.
#' @export

derive_dates <- function(data) {
  data <- within(data, {
    days.np.date.minus.bld.date.diff <- as.numeric(difftime(np.date.time, bld.date.time, units = "days"))
    label(days.np.date.minus.bld.date.diff) <- "Days between blood draw and np test (np-bld)"

    days.np.date.minus.abp.date.diff <- as.numeric(difftime(np.date.time, abp.date, units = "days"))
    label(days.np.date.minus.abp.date.diff) <- "Days between ABP and np test (np-abp)"
  })

  return(data)
}
