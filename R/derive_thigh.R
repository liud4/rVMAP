#' Derive, label, and add thigh variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added thigh variables.
#' @export

derive_thigh <- function(data) {
  data <- within(data, {
    thigh.skin.total.vol <- (thigh.right.skin.total.vol + thigh.left.skin.total.vol) / 2
    label(thigh.skin.total.vol) <- "Thigh Skin Total Volume"

    thigh.fascia.total.vol <- (thigh.right.fascia.total.vol + thigh.left.fascia.total.vol) / 2
    label(thigh.fascia.total.vol) <- "Thigh Fascia Total Volume"

    thigh.subcu.total.vol <- (thigh.right.subcu.total.vol + thigh.left.subcu.total.vol) / 2
    label(thigh.subcu.total.vol) <- "Thigh Subcutaneous Total Volume"

    thigh.fascia.skin.ratio <- (thigh.right.fascia.skin.ratio + thigh.left.fascia.skin.ratio) / 2
    label(thigh.fascia.skin.ratio) <- "Thigh Fascia/Skin Ratio"

    thigh.subcu.skin.ratio <- (thigh.right.subcu.skin.ratio + thigh.left.subcu.skin.ratio) / 2
    label(thigh.subcu.skin.ratio) <- "Thigh Subcutaneous/Skin Ratio"
  })

  return(data)
}
