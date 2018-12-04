#' Derive and add ABP QC derived variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added ABP QC derived variables.
#' @export

derive_abp_qc <- function(data) {
  data <- within(data, {
    # ABP QC:
    abp.within.90d.of.np <- ifelse(
      is.na(days.np.date.minus.abp.date.diff),
      NA,
      ifelse(
        abs(days.np.date.minus.abp.date.diff) <= 90,
        1,
        0
      )
    )
    abp.within.90d.of.np.factor <- factor(
      abp.within.90d.of.np,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(abp.within.90d.of.np.factor) <- label(abp.within.90d.of.np) <- "ABP monitoring within 90 days of neuropsych testing"

    # the other abp qc indicator:  time.reading.indicator %in% c("Yes")
    abp.passed.QC <- ifelse(
      is.na(abp.within.90d.of.np.factor) & is.na(time.reading.indicator),
      NA,
      ifelse(
        abp.within.90d.of.np.factor %in% c("Yes") & time.reading.indicator %in% c("Yes"),
        1,
        0
      )
    )
    abp.passed.QC.factor <- factor(
      abp.passed.QC,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(abp.passed.QC.factor) <- label(abp.passed.QC) <- "ABP: passed QC"
  })

  return(data)
}
