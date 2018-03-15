#' A preprocessing function to replace large negative values with NA
#'
#' @param dat A dataframe containing VMAC variables
#' @param missing.values A vector containing the VMAC values to be recoded as NA.
#' @return The input dataframe with the specified values replaced with NA.
#' @export

missingtoNA <- function(dat, missing.values = rep(-1111, 9)*1:9) {
  # add option to select variables or exclude them
  missing.values <- enquo(missing.values)

  dat %<>%
    mutate_all(
      replace(., which(. %in% (!!missing.values)), NA)
    )
}


missingtoNAv2.fun <- function(dat, missing.values = rep(-1111, 9)*1:9, ...) {
  missing.values <- enquo(missing.values)
  vars <- quos(...)

  if (length(vars) == 0) {
    dat %<>%
      mutate_all(
        funs(replace(., which(. %in% (!!missing.values)), NA))
      )
  } else {
    dat %<>%
      select(
        (!!!vars)
      ) %>%
      mutate_all(
        funs(replace(., which(. %in% (!!missing.values)), NA))
      )
  }
}

#' A preprocessing function to replace 0 with NA in ecogself variables
#'
#' @param dat A dataframe containing VMAC variables
#' @return The input dataframe with zeroes in the ecogself variables replaced with NA.
#' @export

zerotoNA.ecogself <- function(dat) {
  dat %<>%
    select(
      matches("^ecogself\\_(mem|lang|vis|plan|org|attn)\\d{2}$")
    ) %>%
    mutate_all(
      replace(., which(0L), NA)
    )
}
