#' Derive, label, and add cardiac MRI variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added cardiac MRI variables.
#' @export

derive_cardiac_mri <- function(data) {
  data <- within(data, {
    qmass.usable.factor <- factor(qmass.usable, levels = c(0, 1), labels = c("No", "Yes"))
    qstrain.usable.factor <- factor(qstrain.usable, levels = c(0, 1), labels = c("No", "Yes"))
    pwv.usable.factor <- factor(pwv.usable, levels = c(0, 1), labels = c("No", "Yes"))
  })

  return(data)
}
