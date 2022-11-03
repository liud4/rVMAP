#' Invalidate certain variables due to color blindness in certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated color blindness-related variables.
#' @export

invalidate_color_blind <- function(data) {
  data <- merged.df

  color_blind_id.list <- c("028", "033", "131", "169", "176", "201")

  color_blind.var <- Hmisc::Cs(
    np_color,
    np_color_sscore,
    np_word,
    np_word_sscore,
    np_inhibit,
    np_inhibit_sscore,
    np_colorword_sum,
    np_colorword_comp,
    np_inhibitcolor_diff,
    np_inhibitcolor_contrast,
    np_color_scerr,
    np_color_ucerr,
    np_color_err,
    np_color_cumperc_err,
    np_word_scerr,
    np_word_ucerr,
    np_word_err,
    np_word_cumperc_err,
    np_inhibit_scerr,
    np_inhibit_cumperc_scerr,
    np_inhibit_ucerr,
    np_inhibit_cumperc_ucerr,
    np_inhibit_err,
    np_inhibit_err_sscore
  )

  data[data$map_id %in% color_blind_id.list, color_blind.var] <- NA

  return(data)
}
