#' Invalidate certain neuropsychological variables for certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological variables.
#' @export

invalidate_derived_neuropsych_items <- function(data) {
  x <- c(
    "np.tower"
  )

  # Variables to set to NA for MAP 007 1:14 of x
  data[data$map.id == "007" & data$epoch == 1, x] <- NA

  # MAP 035 and MAP 242 - All BFLT Items invalid
  x1 <- c(
    "np.biber.t1to5"
  )

  data[data$map.id %in% c("035", "242") & data$epoch == 1, x1] <- NA

  # MAP 135, 220, 234
  x3 <- c(
    "np.color.cumperc.err"
  )

  data[data$map.id %in% c("135","220","234") & data$epoch == 1, x3] <- NA

  # OAK 20181126: should be taken care of due to missingness from invalidate_raw_neuropsych_items.R
  #
  # 20171024 OAK: MAP 201 – All neuropsych variables from epoch 1 invalid
  # x8 <- grep("^np(?!.*elig$).*$", names(data), value = TRUE, perl = TRUE) # 20180109 OAK: get names starting with np but not ending in elig
  # data[data$map.id == "201" & data$epoch == 1, x8] <- NA

  return(data)
}
