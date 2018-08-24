#' Invalidate certain variables due to color blindness in certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated color blindness-related variables.
#' @export

invalidate_color_blind <- function(data) {
  color_blind_id.list <- c("028", "033", "131", "169", "176", "201")

  color_blind.var <- Hmisc::Cs(
    np.color,
    np.color.ss,
    np.word,
    np.word.ss,
    np.inhibit,
    np.inhibit.ss,
    np.colorword.sum,
    np.colorword.comp,
    np.inhibitcolor.diff,
    np.inhibitcolor.contrast,
    np.color.scerr,
    np.color.ucerr,
    np.color.err,
    np.color.cumpercerr,
    np.word.scerr,
    np.word.ucerr,
    np.word.err,
    np.word.cumpercerr,
    np.inhibit.scerr,
    np.inhibit.cumpercscerr,
    np.inhibit.ucerr,
    np.inhibit.cumpercucerr,
    np.inhibit.err,
    np.inhibit.err.ss,
    np.strp.word.elig,
    np.strp.word.ss.elig,
    np.strp.word.ucerr.elig,
    np.strp.word.scerr.elig,
    np.strp.color.elig,
    np.strp.color.ss.elig,
    np.strp.color.ucerr.elig,
    np.strp.color.scerr.elig,
    np.strp.colorword.elig,
    np.strp.colorword.ss.elig,
    np.strp.colorword.ucerr.elig,
    np.strp.colorword.scerr.elig,
    np.strp.ucerr.elig,
    np.strp.scerr.elig
  )

  data[data$map.id %in% color_blind_id.list, color_blind.var] <- NA

  return(data)
}
