#' Invalidate certain neuropsychological variables for certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological variables.
#' @export

invalidate_raw_neuropsych_items <- function(data) {
  x <- c(
    "np_tower01",
    "np_tower02",
    "np_tower03",
    "np_tower04",
    "np_tower05",
    "np_tower06",
    "np_tower07",
    "np_tower08",
    "np_tower09",
    "np_tower_items",
    "np_tower_ruleviol",
    "np_tower_ruleviol_cumperc",
    #"np.tower",
    "np_tower_ss"
  )

  # Variables to set to NA for MAP 007 1:14 of x
  data[data$map_id == "007" & data$epoch == 1, x] <- NA

  # MAP 035 and MAP 242 - All BFLT Items invalid
  x1 <- c(
    "np_biber1",
    "np_biber1_z",
    "np_biber2",
    "np_biber2_z",
    "np_biber3",
    "np_biber3_z",
    "np_biber4",
    "np_biber4_z",
    "np_biber5",
    "np_biber5_z",
    #"np.biber.t1to5",
    "np_biber_t1to5_z",
    "np_biber_t1to5_persev",
    "np_biber_t1to5_extra",
    "np_biberb",
    "np_biberb_z",
    "np_biber_sd",
    "np_biber_sd_z",
    "np_biber_ld",
    "np_biber_ld_z",
    "np_biber1_figures",
    "np_biber2_figures",
    "np_biber3_figures",
    "np_biber4_figures",
    "np_biber5_figures",
    "np_biberb_figures",
    "np_bibersd_figures",
    "np_biberld_figures",
    "np_biber_hits",
    "np_biber_related_falsealarms",
    "np_biber_unrelated_falsealarms",
    "np_biber_falsealarms",
    "np_biber_recoghitrate",
    "np_biber_falsealarm_relatedrate",
    "np_biber_falsealarm_unrelatedrate",
    "np_biber_falsealarm_totalrate",
    "np_biber_discrim",
    "np_biber_discrim_related",
    "np_biber_discrim_unrelated",
    "np_biber_bias",
    "np_biber_bias_related",
    "np_biber_bias_unrelated"
  )

  data[data$map_id %in% c("035", "242") & data$epoch == 1, x1] <- NA

  # MAP 065 - some BFLT recognition items are invalid (np_biber_hits is okay)
  x2 <- c(
    "np_biber_related_falsealarms",
    "np_biber_unrelated_falsealarms",
    "np_biber_falsealarms",
    "np_biber_recoghitrate",
    "np_biber_falsealarm_relatedrate",
    "np_biber_falsealarm_unrelatedrate",
    "np_biber_falsealarm_totalrate",
    "np_biber_discrim",
    "np_biber_discrim_related",
    "np_biber_discrim_unrelated",
    "np_biber_bias",
    "np_biber_bias_related",
    "np_biber_bias_unrelated"
  )

  data[data$map_id == "065" & data$epoch == 1, x2] <- NA

  # MAP 135, 220, 234
  x3 <- c(
    "np_color"                  , "np_color_ss"               ,
    "np_colorword_sum"          , "np_colorword_comp"         ,
    "np_inhibitcolor_diff"      , "np_inhibitcolor_contrast"  ,
    "np_color_scerr"            , "np_color_ucerr"            ,
    "np_color_err"              , "np_color_cumpercerr"       #,
    #"np_color_cumpercerr_factor"
  )
  data[data$map_id %in% c("135","220","234") & data$epoch==1, x3] <- NA

  x4 <- c(
    "np_cvltrec_hits",
    "np_cvltrec_hits_z",
    "np_cvltrec_falsepos",
    "np_cvltrecog_falsepos_z",
    "np_cvltrecog_discrim",
    "np_cvltrecog_discrim_z",
    "np_cvltrecog_sourcediscrim",
    "np_cvltrecog_sourcediscrim_z",
    "np_cvltrecog_semanticdiscrim",
    "np_cvltrecog_semanticdiscrim_z",
    "np_cvltrecog_noveldiscrim",
    "np_cvltrecog_noveldiscrim_z",
    "np_cvltrecog_responbias",
    "np_cvltrecog_responbias_z"
  )

  data[data$map_id == 209 & data$epoch == 1, x4] <- NA

  ##
  ## below added by OAK 08/25/2017
  ##

  # MAP 016 – BNT invalid

  x5 <- Hmisc::Cs(
    np_bnt,
    np_bnt_z
  )

  data[data$map_id == '016' & data$epoch == 2, x5] <- NA

  # MAP 033, 176 - Color–word invalid

  x6 <- Hmisc::Cs(
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
    np_inhibit_err_ss
  )

  data[data$map_id %in% c("033", "176") & data$epoch == 2, x6] <- NA

  # MAP 111 – BNT VALID – originally marked invalid

  # MAP 265, 266 – Biber learning trials valid; Biber distractor, immediate recall, delayed recall, and recognition invalid

  x7 <- Hmisc::Cs(
    np_biberb,
    np_biberb_z,
    np_biber_sd,
    np_biber_sd_z,
    np_biber_ld,
    np_biber_ld_z,
    np_biberb_figures,
    np_bibersd_figures,
    np_biberld_figures,
    np_biber_hits,
    np_biber_related_falsealarms,
    np_biber_unrelated_falsealarms,
    np_biber_falsealarms,
    np_biber_recoghitrate,
    np_biber_falsealarm_relatedrate,
    np_biber_falsealarm_unrelatedrate,
    np_biber_falsealarm_totalrate,
    np_biber_discrim,
    np_biber_discrim_related,
    np_biber_discrim_unrelated,
    np_biber_bias,
    np_biber_bias_related,
    np_biber_bias_unrelated
  )

  data[data$map_id %in% c("265", "266") & data$epoch == 2, x7] <- NA

  # 20171024 OAK: MAP 201 – All neuropsych variables from epoch 1 invalid

  x8 <- grep("^np(?!.*elig$).*$", names(data), value = TRUE, perl = TRUE) # 20180109 OAK: get names starting with np but not ending in elig
  data[data$map_id == "201" & data$epoch == 1, x8] <- NA

  # 20190103 OAK: Invalidating 60 month variables as per Kim's post on Asana from 20190103

  x9 <- Hmisc::Cs(
    np_biber1,
    np_biber1_z,
    np_biber2,
    np_biber2_z,
    np_biber3,
    np_biber3_z,
    np_biber4,
    np_biber4_z,
    np_biber5,
    np_biber5_z,
    np_biber_t1to5_z,
    np_biber_t1to5_persev,
    np_biber_t1to5_extra,
    np_biberb,
    np_biberb_z,
    np_biber_sd,
    np_biber_sd_z,
    np_biber_ld,
    np_biber_ld_z,
    np_biber1_figures,
    np_biber2_figures,
    np_biber3_figures,
    np_biber4_figures,
    np_biber5_figures,
    np_biberb_figures,
    np_bibersd_figures,
    np_biberld_figures,
    np_biber_hits,
    np_biber_related_falsealarms,
    np_biber_unrelated_falsealarms,
    np_biber_falsealarms,
    np_biber_recoghitrate,
    np_biber_falsealarm_relatedrate,
    np_biber_falsealarm_unrelatedrate,
    np_biber_falsealarm_totalrate,
    np_biber_discrim,
    np_biber_discrim_related,
    np_biber_discrim_unrelated,
    np_biber_bias,
    np_biber_bias_related,
    np_biber_bias_unrelated
  )

  data[data$map_id == "001" & data$epoch == 4, x9] <- NA # 20190103 OAK

  ##

  x10 <- Hmisc::Cs(
    np_cvlt1,
    np_cvlt1z,
    np_cvlt2,
    np_cvlt2z,
    np_cvlt3,
    np_cvlt3z,
    np_cvlt4,
    np_cvlt4z,
    np_cvlt5,
    np_cvlt5z,
    np_cvlt1to5,
    np_cvlt1to5_tscore,
    np_cvltb,
    np_cvltbz,
    np_cvlt_sdfr,
    np_cvlt_sdfr_z,
    np_cvlt_sdcr,
    np_cvlt_sdcr_z,
    np_cvlt_ldfr,
    np_cvlt_ldfr_z,
    np_cvlt_ldcr,
    np_cvlt_ldcr_z,
    np_cvlt1to5_semclust,
    np_cvlt1to5_semclust_z,
    np_cvlt1to5_serialclustfwd,
    np_cvlt1to5_serialclustfwd_z,
    np_cvlt1to5_serialclustbirect,
    np_cvlt1to5_serialclustbidirect_z,
    np_cvlt1to5_primacy,
    np_cvlt1to5_primacy_z,
    np_cvlt1to5_middle,
    np_cvlt1to5_middle_z,
    np_cvlt1to5_recency,
    np_cvlt1to5_recency_z,
    np_cvltslope,
    np_cvltslope_z,
    np_cvltslope_t1to2,
    np_cvltslope_t1to2_z,
    np_cvltslope_t2to5,
    np_cvltslope_t2to5_z,
    np_cvlt_learnconsist,
    np_cvlt_learnconsist_z,
    np_cvltcontrast_bvs1,
    np_cvltcontrast_bvs1_z,
    np_cvltcontrast_sdvs5,
    np_cvltcontrast_sdvs5_z,
    np_cvltcontrast_ldvs5,
    np_cvltcontrast_ldvs5_z,
    np_cvltcontrast_ldvssd,
    np_cvltcontrast_ldvssd_z,
    np_cvlt_reps,
    np_cvlt_reps_z,
    np_cvlt_intrus,
    np_cvlt_intrus_z,
    np_cvltrec_hits,
    np_cvltrec_hits_z,
    np_cvltrec_falsepos,
    np_cvltrecog_falsepos_z,
    np_cvltrecog_discrim,
    np_cvltrecog_discrim_z,
    np_cvltrecog_sourcediscrim,
    np_cvltrecog_sourcediscrim_z,
    np_cvltrecog_semanticdiscrim,
    np_cvltrecog_semanticdiscrim_z,
    np_cvltrecog_noveldiscrim,
    np_cvltrecog_noveldiscrim_z,
    np_cvltrecog_responbias,
    np_cvltrecog_responbias_z
  )

  data[data$map_id == "009" & data$epoch == 4, x10] <- NA # 20190103 OAK

  ##

  x11 <- Hmisc::Cs(
    np_digsymb,
    np_digsymb_ss,
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
    np_tmta,
    np_tmta_ss,
    np_tmtb,
    np_tmtb_ss,
    np_tmt_contrastdiff_ss,
    np_tmta_seqerr,
    np_tmta_cumperc_seqerr,
    np_tmta_seterr,
    np_tmta_cumperc_seterr,
    np_tmtb_seqerr,
    np_tmtb_cumperc_seqerr,
    np_tmtb_seterr,
    np_tmtb_cumperc_seterr
  )

  data[data$map_id %in% c("092", "123") & data$epoch == 4, x11] <- NA # 20190103 OAK

  return(data)
}
