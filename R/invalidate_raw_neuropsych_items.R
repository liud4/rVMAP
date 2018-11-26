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


  # OAK 20181126: this section is taken care of with other functions.
  #
  # # 20180201 OAK: after speaking to Kim, set epoch 3 np.* %in% c(-7777, -8888, -9999) to NA
  # x10 <- grep("^np(?!.*notes$).*$", names(data[sapply(data, is.numeric)]), value = TRUE, perl = TRUE)
  # data[data$epoch == 3, x10] <- lapply(data[data$epoch == 3, x10],
  #                                    function(x) ifelse(x %in% c(-7777, -8888, -9999), NA, x))
  #
  # # 20180202 OAK: set epoch 3 calculated np vars to NA if components %in% c(-7777, -8888, -9999, NA)
  # meta.epoch3 <- dplyr::bind_rows(MAPfreeze.list[["epoch_3"]][["metadata"]], NULL)
  # meta.epoch3$field_name <- gsub("\\_", "\\.", meta.epoch3$field_name)
  # calc.e3.vars <- data.frame(
  #   meta.epoch3[meta.epoch3$field_type == "calc", c('field_name', 'select_choices_or_calculations')],
  #   stringsAsFactors = FALSE
  # )
  # calc.e3.vars.np <- calc.e3.vars[grep("np\\.", calc.e3.vars$field_name), ]
  #
  # x12 <- calc.e3.vars.np$field_name
  #
  # np.components <- regmatches(
  #   calc.e3.vars.np[, 2],
  #   gregexpr("(?<=\\[).*?(?=\\])",
  #            calc.e3.vars.np[, 2],
  #            perl = TRUE
  #   )
  # )
  #
  # names(np.components) <- calc.e3.vars.np$field_name
  # np.components <- rapply(np.components, function(x) gsub("\\_", "\\.", x), how = "list")
  #
  # for (i in 1:length(x12)) {
  #   np.calc.var <- x12[i]
  #   np.calc.var.comp <- np.components[[i]]
  #   data[data$epoch == 3, np.calc.var] <- ifelse(
  #     apply(data[data$epoch == 3, np.calc.var.comp, drop = F], 1, function(x) any(x %in% c(-7777, -8888, -9999, NA))),
  #     NA,
  #     data[data$epoch == 3, np.calc.var]
  #   )
  # }

  #

  return(data)
}
