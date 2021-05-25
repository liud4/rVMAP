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
    np_color_ss,
    np_word,
    np_word_ss,
    np_inhibit,
    np_inhibit_ss,
    np_colorword_sum,
    np_colorword_comp,
    np_inhibitcolor_diff,
    np_inhibitcolor_contrast,
    np_color_scerr,
    np_color_ucerr,
    np_color_err,
    np_color_cumpercerr,
    np_word_scerr,
    np_word_ucerr,
    np_word_err,
    np_word_cumpercerr,
    np_inhibit_scerr,
    np_inhibit_cumpercscerr,
    np_inhibit_ucerr,
    np_inhibit_cumpercucerr,
    np_inhibit_err,
    np_inhibit_err_ss,
    np_strp_word_elig,
    np_strp_word_ss_elig,
    np_strp_word_ucerr_elig,
    np_strp_word_scerr_elig,
    np_strp_color_elig,
    np_strp_color_ss_elig,
    np_strp_color_ucerr_elig,
    np_strp_color_scerr_elig,
    np_strp_colorword_elig,
    np_strp_colorword_ss_elig,
    np_strp_colorword_ucerr_elig,
    np_strp_colorword_scerr_elig,
    np_strp_ucerr_elig,
    np_strp_scerr_elig
  )

  data[data$map_id %in% color_blind_id.list, color_blind.var] <- NA

  return(data)
}
