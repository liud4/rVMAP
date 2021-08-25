#' Invalidate certain neuropsychological variables for certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological variables.
#' @export

invalidate_raw_neuropsych_items <- function(data) {
  if (data$epoch[1] == 1) {
    #* Epoch 1 *#

    x <- c(
      "np_tower1",
      "np_tower2",
      "np_tower3",
      "np_tower4",
      "np_tower5",
      "np_tower6",
      "np_tower7",
      "np_tower8",
      "np_tower9",
      "np_tower_items",
      "np_tower_ruleviol",
      "np_tower_ruleviol_cumperc",
      # "np.tower",
      "np_tower_sscore"
    )

    # Variables to set to NA for MAP 007 1:14 of x
    data[data$map_id == "007" & data$epoch == 1, x] <- NA

    # MAP 035 and MAP 242 - All BFLT Items invalid
    x1 <- c(
      "np_biber1",
      "np_biber1_zscore",
      "np_biber2",
      "np_biber2_zscore",
      "np_biber3",
      "np_biber3_zscore",
      "np_biber4",
      "np_biber4_zscore",
      "np_biber5",
      "np_biber5_zscore",
      # "np.biber.t1to5",
      "np_biber_t1to5_zscore",
      "np_biber_t1to5_persev",
      "np_biber_t1to5_extra",
      "np_biberb",
      "np_biberb_zscore",
      "np_biber_sd",
      "np_biber_sd_zscore",
      "np_biber_ld",
      "np_biber_ld_zscore",
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

    # Epoch 1: MAP 135, 220, 234
    x3 <- c(
      "np_color"                  , "np_color_sscore"               ,
      "np_colorword_sum"          , "np_colorword_comp"         ,
      "np_inhibitcolor_diff"      , "np_inhibitcolor_contrast"  ,
      "np_color_scerr"            , "np_color_ucerr"            ,
      "np_color_err"              , "np_color_cumpercerr"       #,
      #"np_color_cumpercerr_factor"
    )
    data[data$map_id %in% c("135","220","234") & data$epoch==1, x3] <- NA

    x4 <- c(
      "np_cvltrec_hits",
      "np_cvltrec_hits_zscore",
      "np_cvltrec_falsepos",
      "np_cvltrecog_falsepos_zscore",
      "np_cvltrecog_discrim",
      "np_cvltrecog_discrim_zscore",
      "np_cvltrecog_sourcediscrim",
      "np_cvltrecog_sourcediscrim_zscore",
      "np_cvltrecog_semanticdiscrim",
      "np_cvltrecog_semanticdiscrim_zscore",
      "np_cvltrecog_noveldiscrim",
      "np_cvltrecog_noveldiscrim_zscore",
      "np_cvltrecog_responbias",
      "np_cvltrecog_responbias_zscore"
    )

    data[data$map_id == "209" & data$epoch == 1, x4] <- NA

    # Epoch 1: MAP 201 – All neuropsych variables from epoch 1 invalid

    x5 <- grep("^np(?!.*elig$).*$", names(data), value = TRUE, perl = TRUE)  # 20180109 OAK: get names starting with np but not ending in elig

    data[data$map_id == "201" & data$epoch == 1, x5] <- NA

  }

  if (data$epoch[1] == 2) {

    #* Epoch 2 *#

    # Epoch 2: MAP 016 – BNT invalid

    x6 <- Hmisc::Cs(
      np_bnt,
      np_bnt_zscore
    )

    data[data$map_id == '016' & data$epoch == 2, x6] <- NA

    # Epoch 2: MAP 033, 176 - Color–word invalid

    x7 <- Hmisc::Cs(
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
      np_inhibit_err_sscore
    )

    data[data$map_id %in% c("033", "176") & data$epoch == 2, x7] <- NA

    # Epoch 2: MAP 265, 266 – Biber learning trials valid; Biber distractor, immediate recall, delayed recall, and recognition invalid

    x8 <- Hmisc::Cs(
      np_biberb,
      np_biberb_zscore,
      np_biber_sd,
      np_biber_sd_zscore,
      np_biber_ld,
      np_biber_ld_zscore,
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

    data[data$map_id %in% c("265", "266") & data$epoch == 2, x8] <- NA
  }

  if (data$epoch[1] == 3) {

    #* Epoch 3 *#

    # Epoch 3: MAP 001 Color-word interference, TMTA, & TMT B invalid
    x9 <- Cs(
      np_inhibit,
      np_inhibit_sscore,
      np_inhibit_scerr,
      np_inhibit_cumpercscerr,
      np_inhibit_ucerr,
      np_inhibit_cumpercucerr,
      np_inhibit_err,
      np_inhibit_err_sscore,
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr
    )

    data[data$map_id %in% c("001") & data$epoch == 3, x9] <- NA

    # Epoch 3: MAP 011 Color-word interference, TMTA, & TMT B invalid
    x10 <- Cs(
      np_inhibit,
      np_inhibit_sscore,
      np_inhibit_scerr,
      np_inhibit_cumpercscerr,
      np_inhibit_ucerr,
      np_inhibit_cumpercucerr,
      np_inhibit_err,
      np_inhibit_err_sscore,
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr
    )
    data[data$map_id %in% c("011") & data$epoch == 3, x10] <- NA

    #Epoch 3: MAP 059 everything except BNT, Animals, & HVOT invalid
    x11 <- Cs(
      np_moca,
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
      np_cvlt_sdfr_zscore,
      np_cvlt_sdcr,
      np_cvlt_sdcr_zscore,
      np_cvlt_ldfr,
      np_cvlt_ldfr_zscore,
      np_cvlt_ldcr,
      np_cvlt_ldcr_zscore,
      np_cvlt1to5_semclust,
      np_cvlt1to5_semclust_zscore,
      np_cvlt1to5_serialclustfwd,
      np_cvlt1to5_serialclustfwd_zscore,
      np_cvlt1to5_serialclustbirect,
      np_cvlt1to5_serialclustbidirect_zscore,
      np_cvlt1to5_primacy,
      np_cvlt1to5_primacy_zscore,
      np_cvlt1to5_middle,
      np_cvlt1to5_middle_zscore,
      np_cvlt1to5_recency,
      np_cvlt1to5_recency_zscore,
      np_cvltslope,
      np_cvltslope_zscore,
      np_cvltslope_t1to2,
      np_cvltslope_t1to2_zscore,
      np_cvltslope_t2to5,
      np_cvltslope_t2to5_zscore,
      np_cvlt_learnconsist,
      np_cvlt_learnconsist_zscore,
      np_cvltcontrast_bvs1,
      np_cvltcontrast_bvs1_zscore,
      np_cvltcontrast_sdvs5,
      np_cvltcontrast_sdvs5_zscore,
      np_cvltcontrast_ldvs5,
      np_cvltcontrast_ldvs5_zscore,
      np_cvltcontrast_ldvssd,
      np_cvltcontrast_ldvssd_zscore,
      np_cvlt_reps,
      np_cvlt_reps_zscore,
      np_cvlt_intrus,
      np_cvlt_intrus_zscore,
      np_cvltrec_hits,
      np_cvltrec_hits_zscore,
      np_cvltrec_falsepos,
      np_cvltrecog_falsepos_zscore,
      np_cvltrecog_discrim,
      np_cvltrecog_discrim_zscore,
      np_cvltrecog_sourcediscrim,
      np_cvltrecog_sourcediscrim_zscore,
      np_cvltrecog_semanticdiscrim,
      np_cvltrecog_semanticdiscrim_zscore,
      np_cvltrecog_noveldiscrim,
      np_cvltrecog_noveldiscrim_zscore,
      np_cvltrecog_responbias,
      np_cvltrecog_responbias_zscore,
      np_tower1,
      np_tower2,
      np_tower3,
      np_tower4,
      np_tower5,
      np_tower6,
      np_tower7,
      np_tower8,
      np_tower9,
      np_tower_items,
      np_tower_ruleviol,
      np_tower_ruleviol_cumperc,
      np_tower_sscore,
      np_digsymb,
      np_digsymb_sscore,
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
      np_inhibit_err_sscore,
      np_fas_fq1,
      np_fas_fq2,
      np_fas_fq3,
      np_fas_fq4,
      np_fas_f,
      np_fas_f_intrus,
      np_fas_f_reps,
      np_fas_aq1,
      np_fas_aq2,
      np_fas_aq3,
      np_fas_aq4,
      np_fas_a,
      np_fas_a_intrus,
      np_fas_a_reps,
      np_fas_sq1,
      np_fas_sq2,
      np_fas_sq3,
      np_fas_sq4,
      np_fas_s,
      np_fas_s_intrus,
      np_fas_s_reps,
      np_fas_q1,
      np_fas_q2,
      np_fas_q3,
      np_fas_q4,
      np_fas,
      np_fas_tscore,
      np_fas_intrus,
      np_fas_rep,
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr,
      np_biber1,
      np_biber1_zscore,
      np_biber2,
      np_biber2_zscore,
      np_biber3,
      np_biber3_zscore,
      np_biber4,
      np_biber4_zscore,
      np_biber5,
      np_biber5_zscore,
      np_biber_t1to5_zscore,
      np_biber_t1to5_persev,
      np_biber_t1to5_extra,
      np_biberb,
      np_biberb_zscore,
      np_biber_sd,
      np_biber_sd_zscore,
      np_biber_ld,
      np_biber_ld_zscore,
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

    data[data$map_id %in% c("059") & data$epoch == 3, x11] <- NA

    # Epoch 3: MAP 134 Biber recognition invalid

    x12 <- Cs(
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
    data[data$map_id %in% c("134") & data$epoch == 3, x12] <- NA
  }

  if (data$epoch[1] == 4) {
    #* Epoch 4 *#
    # 20190103 OAK: Invalidating 60 month variables as per Kim's post on Asana from 20190103

    x13 <- Hmisc::Cs(
      np_biber1,
      np_biber1_zscore,
      np_biber2,
      np_biber2_zscore,
      np_biber3,
      np_biber3_zscore,
      np_biber4,
      np_biber4_zscore,
      np_biber5,
      np_biber5_zscore,
      np_biber_t1to5_zscore,
      np_biber_t1to5_persev,
      np_biber_t1to5_extra,
      np_biberb,
      np_biberb_zscore,
      np_biber_sd,
      np_biber_sd_zscore,
      np_biber_ld,
      np_biber_ld_zscore,
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

    data[data$map_id == "001" & data$epoch == 4, x13] <- NA # 20190103 OAK

    ##

    x14 <- Hmisc::Cs(
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
      np_cvlt_sdfr_zscore,
      np_cvlt_sdcr,
      np_cvlt_sdcr_zscore,
      np_cvlt_ldfr,
      np_cvlt_ldfr_zscore,
      np_cvlt_ldcr,
      np_cvlt_ldcr_zscore,
      np_cvlt1to5_semclust,
      np_cvlt1to5_semclust_zscore,
      np_cvlt1to5_serialclustfwd,
      np_cvlt1to5_serialclustfwd_zscore,
      np_cvlt1to5_serialclustbirect,
      np_cvlt1to5_serialclustbidirect_zscore,
      np_cvlt1to5_primacy,
      np_cvlt1to5_primacy_zscore,
      np_cvlt1to5_middle,
      np_cvlt1to5_middle_zscore,
      np_cvlt1to5_recency,
      np_cvlt1to5_recency_zscore,
      np_cvltslope,
      np_cvltslope_zscore,
      np_cvltslope_t1to2,
      np_cvltslope_t1to2_zscore,
      np_cvltslope_t2to5,
      np_cvltslope_t2to5_zscore,
      np_cvlt_learnconsist,
      np_cvlt_learnconsist_zscore,
      np_cvltcontrast_bvs1,
      np_cvltcontrast_bvs1_zscore,
      np_cvltcontrast_sdvs5,
      np_cvltcontrast_sdvs5_zscore,
      np_cvltcontrast_ldvs5,
      np_cvltcontrast_ldvs5_zscore,
      np_cvltcontrast_ldvssd,
      np_cvltcontrast_ldvssd_zscore,
      np_cvlt_reps,
      np_cvlt_reps_zscore,
      np_cvlt_intrus,
      np_cvlt_intrus_zscore,
      np_cvltrec_hits,
      np_cvltrec_hits_zscore,
      np_cvltrec_falsepos,
      np_cvltrecog_falsepos_zscore,
      np_cvltrecog_discrim,
      np_cvltrecog_discrim_zscore,
      np_cvltrecog_sourcediscrim,
      np_cvltrecog_sourcediscrim_zscore,
      np_cvltrecog_semanticdiscrim,
      np_cvltrecog_semanticdiscrim_zscore,
      np_cvltrecog_noveldiscrim,
      np_cvltrecog_noveldiscrim_zscore,
      np_cvltrecog_responbias,
      np_cvltrecog_responbias_zscore
    )

    data[data$map_id == "009" & data$epoch == 4, x14] <- NA # 20190103 OAK

    ##

    x15 <- Hmisc::Cs(
      np_digsymb,
      np_digsymb_sscore,
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
      np_inhibit_err_sscore,
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr
    )

    data[data$map_id %in% c("092", "123") & data$epoch == 4, x15] <- NA # 20190103 OAK
  }

  if (data$epoch[1] == 5) {
    #=========================================================================================
    # Epoch 5 Invalidation - Added 20210316

    x16 <- Hmisc::Cs(
      np_biberb,
      np_biberb_zscore,
      np_biber_sd,
      np_biber_sd_zscore,
      np_biber_ld,
      np_biber_ld_zscore,
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

    data[data$map_id == "001" & data$epoch == 5, x16] <- NA # 20210316 OAK

    ##

    x17 <- Hmisc::Cs(
      np_pvltrecog_m,
      np_pvltrecog_ts,
      np_pvltrecog_tu1,
      np_pvltrecog_ts1,
      np_pvltrecog_sem,
      np_pvltrecog_ur,
      np_pvltrecog_foil,
      np_pvltrecog_falsepos,
      np_pvltrecog_discrim
    )

    data[data$map_id == "002" & data$epoch == 5, x17] <- NA # 20210316 OAK

    ##

    x18 <- Hmisc::Cs(
      np_biberb,
      np_biberb_zscore,
      np_biber_sd,
      np_biber_sd_zscore,
      np_biber_ld,
      np_biber_ld_zscore,
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
      np_biber_bias_unrelated,
      np_moca,
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
      np_cvlt_sdfr_zscore,
      np_cvlt_sdcr,
      np_cvlt_sdcr_zscore,
      np_cvlt_ldfr,
      np_cvlt_ldfr_zscore,
      np_cvlt_ldcr,
      np_cvlt_ldcr_zscore,
      np_cvlt1to5_semclust,
      np_cvlt1to5_semclust_zscore,
      np_cvlt1to5_serialclustfwd,
      np_cvlt1to5_serialclustfwd_zscore,
      np_cvlt1to5_serialclustbirect,
      np_cvlt1to5_serialclustbidirect_zscore,
      np_cvlt1to5_primacy,
      np_cvlt1to5_primacy_zscore,
      np_cvlt1to5_middle,
      np_cvlt1to5_middle_zscore,
      np_cvlt1to5_recency,
      np_cvlt1to5_recency_zscore,
      np_cvltslope,
      np_cvltslope_zscore,
      np_cvltslope_t1to2,
      np_cvltslope_t1to2_zscore,
      np_cvltslope_t2to5,
      np_cvltslope_t2to5_zscore,
      np_cvlt_learnconsist,
      np_cvlt_learnconsist_zscore,
      np_cvltcontrast_bvs1,
      np_cvltcontrast_bvs1_zscore,
      np_cvltcontrast_sdvs5,
      np_cvltcontrast_sdvs5_zscore,
      np_cvltcontrast_ldvs5,
      np_cvltcontrast_ldvs5_zscore,
      np_cvltcontrast_ldvssd,
      np_cvltcontrast_ldvssd_zscore,
      np_cvlt_reps,
      np_cvlt_reps_zscore,
      np_cvlt_intrus,
      np_cvlt_intrus_zscore,
      np_cvltrec_hits,
      np_cvltrec_hits_zscore,
      np_cvltrec_falsepos,
      np_cvltrecog_falsepos_zscore,
      np_cvltrecog_discrim,
      np_cvltrecog_discrim_zscore,
      np_cvltrecog_sourcediscrim,
      np_cvltrecog_sourcediscrim_zscore,
      np_cvltrecog_semanticdiscrim,
      np_cvltrecog_semanticdiscrim_zscore,
      np_cvltrecog_noveldiscrim,
      np_cvltrecog_noveldiscrim_zscore,
      np_cvltrecog_responbias,
      np_cvltrecog_responbias_zscore,
      np_tower1,
      np_tower2,
      np_tower3,
      np_tower4,
      np_tower5,
      np_tower6,
      np_tower7,
      np_tower8,
      np_tower9,
      np_tower_items,
      np_tower_ruleviol,
      np_tower_ruleviol_cumperc,
      np_tower_sscore,
      np_digsymb,
      np_digsymb_sscore,
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr
    )

    data[data$map_id == "009" & data$epoch == 5, x18] <- NA # 20210316 OAK

    ##

    x19 <- Hmisc::Cs(
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
      np_inhibit_err_sscore
    )

    data[data$map_id == "033" & data$epoch == 5, x19] <- NA # 20210316 OAK

    ##

    x20 <- Hmisc::Cs(
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr,
      np_digsymb,
      np_digsymb_sscore,
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
      np_inhibit_err_sscore
    )

    data[data$map_id == "092" & data$epoch == 5, x20] <- NA # 20210316 OAK

    ##

    x21 <- Hmisc::Cs(
      np_tmta,
      np_tmta_sscore,
      np_tmtb,
      np_tmtb_sscore,
      np_tmt_contrastdiff_sscore,
      np_tmta_seqerr,
      np_tmta_cumperc_seqerr,
      np_tmta_seterr,
      np_tmta_cumperc_seterr,
      np_tmtb_seqerr,
      np_tmtb_cumperc_seqerr,
      np_tmtb_seterr,
      np_tmtb_cumperc_seterr,
      np_digsymb,
      np_digsymb_sscore,
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
      np_inhibit_err_sscore
    )

    data[data$map_id == "123" & data$epoch == 5, x21] <- NA # 20210316 OAK

    ##

    x22 <- Hmisc::Cs(
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
      np_inhibit_err_sscore
    )

    data[data$map_id == "176" & data$epoch == 5, x22] <- NA # 20210316 OAK

    ##

    x23 <- Hmisc::Cs(
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
      np_inhibit_err_sscore
    )

    data[data$map_id == "185" & data$epoch == 5, x23] <- NA # 20210316 OAK
  }

  ##

  return(data)

}
