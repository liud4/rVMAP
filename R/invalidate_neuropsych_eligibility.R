#' Invalidate certain neuropsychological eligibility variables for certain participants in the post-fixed eligibility data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological eligibility variables.
#' @export

invalidate_neuropsych_eligibility <- function(data) {
  x <- c(
    "np_srt6_elig",
    "np_srt_immed_elig",
    "np_srt_immed_zscore_elig",
    "np_srt_sdcr_elig",
    "np_srt_ldfr_elig",
    "np_srt_ldfr_zscore_elig",
    "np_srt_recog_elig",
    "np_srt_recog_zscore_elig",
    "np_srt_intrus_elig",
    "np_srt_intrus_zscore_elig",
    "np_srt_reps_elig"
  )

  # Variables to set to NA for MAP 007
  data[data$map_id == "007", x] <- NA

  x1 <- c(
    "np_blocks_elig",
    "np_blocks_sscore_elig"
  )

  # Variables to set to NA for MAP 016 & MAP 213
  data[data$map_id %in% c("016", "213"), x1] <- NA

  # MAP 025
  x2 <- c(
    "np_srt1_elig",
    "np_srt2_elig",
    "np_srt3_elig",
    "np_srt4_elig",
    "np_srt5_elig",
    "np_srt6_elig",
    "np_srt_immed_elig",
    "np_srt_immed_zscore_elig",
    "np_srt_sdcr_elig",
    "np_srt_ldfr_elig",
    "np_srt_ldfr_zscore_elig",
    "np_srt_recog_elig",
    "np_srt_recog_zscore_elig",
    "np_srt_intrus_elig",
    "np_srt_intrus_zscore_elig",
    "np_srt_reps_elig"
  )

  # Variables to set to NA for MAP 025
  data[data$map_id == "025", x2] <- NA

  # Variables for MAP 039
  x3 <- c(
    "np_tmtb_elig",
    "np_tmtb_sscore_elig",
    "np_tmtb_seqerr_elig",
    "np_tmtb_seterr_elig"
  )

  # Set variables to missing for MAP 039
  data[data$map_id == "039", x3] <- NA

  # MAP 137
  x4 <- c(
    "np_srt_ldfr_elig",
    "np_srt_ldfr_zscore_elig",
    "np_srt_recog_elig",
    "np_srt_recog_zscore_elig",
    "np_srt_intrus_elig",
    "np_srt_intrus_zscore_elig"
  )

  # Set variables to missing for MAP 137
  data[data$map_id == "137", x4] <- NA

  # MAP 201
  x5 <- c(
    "np_strp_color_elig",
    "np_strp_color_sscore_elig",
    "np_strp_color_ucerr_elig",
    "np_strp_color_scerr_elig",
    "np_strp_colorword_elig",
    "np_strp_colorword_sscore_elig",
    "np_strp_colorword_ucerr_elig",
    "np_strp_colorword_scerr_elig",
    "np_strp_ucerr_elig",
    "np_strp_scerr_elig")

  # Set variables to missing for MAP 201
  data[data$map_id == "201", x5] <- NA

  # MAP 212 & MAP 236
  x6 <- c(
    "np_vegq1_elig",
    "np_vegq2_elig",
    "np_vegq3_elig",
    "np_vegq4_elig",
    "np_veg_elig",
    "np_veg_zscore_elig",
    "np_veg_reps_elig",
    "np_veg_intrus_elig"
  )

  # Set variables to missing for MAP 212 & 236
  data[data$map_id %in% c("212", "236"), x6] <- NA

  # MAP 299
  x7 <- c(
    "np_wais_digitsf_elig",
    "np_wais_digits_elig",
    "np_wais_digits_sscore_elig",
    "np_wais_digitsf_span_elig",
    "np_wais_digitsf_span_zscore_elig"
  )

  # Set variables to missing for MAP 299
  data[data$map_id == "299", x7] <- NA

  # Output
  return(data)
}
