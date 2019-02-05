#' Major function to process raw, non-derived data within each epoch.
#'
#' @param epoch Epoch number
#' @param main MAP Main data
#' @param abp MAP ABP Consent data
#' @param biomarkers MAP Biomarkers data
#' @param auto3T MAP Automated 3T Brain MRI data
#' @param auto3TBH MAP Automated 3T Brain MRI Breath Hold data
#' @param man3T MAP Manual 3T Brain MRI data
#' @param man3TBH MAP Manual 3T Brain MRI Breath Hold data
#' @param qmass MAP Cardiac MRI Analysis data
#' @param addendum MAP Addendum data
#' @param csf MAP Clinical CSF data
#' @param srt MAP SRT Error Analysis data
#' @param breathhold.prefix A string containing the temporary prefix to be added to the breath hold variables.
#' @return MAPfreeze.list is updated with the processed data.
#' @export

process_raw_data <- function(
  epoch, main, abp = NULL, biomarkers = NULL,
  auto3T = NULL, auto3TBH = NULL, man3T = NULL, man3TBH = NULL,
  qmass = NULL, addendum = NULL, csf = NULL, srt = NULL,
  breathhold.prefix = "bHold_") {

  current_epoch <- paste0("epoch_", epoch)

  #################################################################
  # Start with the main file for this epoch.

  mydat <- main %>%
    clear_labels() %>%
    format_id() %>%
    remove_unnecesary_vars()

  mydat[, setdiff(names(mydat), "mhx_tobac_quit_year")] <- mydat[, setdiff(names(mydat), "mhx_tobac_quit_year")] %>%
    mutate_if(
      ~ any(class(.) %in% c("numeric", "integer")),
      ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
    ) %>%
    mutate_if(
      ~ any(class(.) %in% c("character")),
      ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
    )

  # mydat <- mydat %>%
  #   mutate_at(
  #     mydat %>% select_if(~ (class(.) %in% c("numeric", "integer", "character"))) %>% names() %>% setdiff(., "mhx_tobac_quit"),
  #     ~ missing_to_na(., equal.val = -8888, restrict.sign = TRUE)
  #   )

  # mydat <- mydat %>%
  #   mutate_at(
  #     grep("ccqself[0-9]+|ccqinform[0-9]+", names(.)),
  #     ~ missing_to_na(., equal.val = -2222, restrict.sign = TRUE)
  #   )

  mydat <- mydat %>%
    mutate_at(
      grep("notes|date", grep("^ecogself\\_", names(.), value = T), value = T, invert = T),
      ~ missing_to_na(., equal.val = 0, restrict.sign = TRUE)
    )

  notes.var <- grep("\\_notes", names(mydat), value = T)
  mydat[, notes.var] <- apply(mydat[, notes.var], 2, function(vec) {
    ifelse(stringr::str_trim(vec) %in% c("-8888", "-9999", ""), NA, vec)
  })

  pvlt.var <- NULL

  pvlt.var <- Cs(
    np_pvlt1, np_pvlt2, np_pvlt3, np_pvlt4, np_pvlt5, np_pvlta_tot,
    np_pvlta_intrus, np_pvlta_pers, np_pvlta_clust, np_pvlt_init_intrus, np_pvlt_tt_intrus,
    np_pvlt_wt_pers, np_pvlta_prim, np_pvlta_mid, np_pvlta_rec, np_pvlta_primperc, np_pvlta_midperc, np_pvlta_recperc,
    np_pvlt6, np_pvlt7, np_pvlt8_fruit, np_pvlt8_office, np_pvlt8_clothing, np_pvlt8, np_pvlt9,
    np_pvlt10_fruit, np_pvlt10_office, np_pvlt10_clothing, np_pvlt10,
    np_pvltrecog_m, np_pvltrecog_foil, np_pvltrecog_falsepos, np_pvltrecog_discrim
  )[Cs(
    np_pvlt1, np_pvlt2, np_pvlt3, np_pvlt4, np_pvlt5, np_pvlta_tot,
    np_pvlta_intrus, np_pvlta_pers, np_pvlta_clust, np_pvlt_init_intrus, np_pvlt_tt_intrus,
    np_pvlt_wt_pers, np_pvlta_prim, np_pvlta_mid, np_pvlta_rec, np_pvlta_primperc, np_pvlta_midperc, np_pvlta_recperc,
    np_pvlt6, np_pvlt7, np_pvlt8_fruit, np_pvlt8_office, np_pvlt8_clothing, np_pvlt8, np_pvlt9,
    np_pvlt10_fruit, np_pvlt10_office, np_pvlt10_clothing, np_pvlt10,
    np_pvltrecog_m, np_pvltrecog_foil, np_pvltrecog_falsepos, np_pvltrecog_discrim
  ) %in% names(mydat)]

  if (length(pvlt.var) > 0) {
    mydat <- mydat %>%
      mutate_at(
        vars(pvlt.var),
        funs(rVMAP::missing_to_na(., equal.val = -99, restrict.sign = TRUE))
      )
  }

  var_w_comparison_operators <- NULL

  var_w_comparison_operators <- grep(
    "bld|np|csf|biomarkers",
    grep(
      "notes|flow|occup",
      names(mydat)[grepl("(>|<)([[:space:]]*)(\\d+)", mydat)],
      invert = T,
      v = T),
    v = T)

  var_w_comparison_operators <- setdiff(var_w_comparison_operators, "bld_c_coag")

  if (length(var_w_comparison_operators) > 0) {
    for (var in var_w_comparison_operators) {
      mydat[, var] <- sapply(mydat[, var], process_comparison_operators, USE.NAMES = FALSE)
    }
  }

  var_w_comparison_operators <- NULL

  mydat <- invalidate_raw_neuropsych_items(mydat)
  mydat <- invalidate_color_blind(mydat)

  mydat <- process_calculated_fields(data = mydat, data_label = "main", epoch = current_epoch)

  mydat <- process_factor_variables(data = mydat, data_label = "main", epoch = current_epoch)

  mydat <- mydat %>%
    mutate_at(
      vars(matches("session_id$")),
      as.character
    )

  mydat <- mydat %>%
    mutate_at(
      vars(matches("_dose$")),
      as.character
    )

  mydat <- convert_dates(mydat)

  MAPfreeze.list[[current_epoch]][["data"]][["main"]] <<- mydat

  #################################################################
  # Process ABP data if available.

  if (!is.null(abp)) {
    abp.df <- abp
    names(abp.df) <- gsub("\\.", "\\_", names(abp.df))

    abp.df <- abp.df %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    abp.df <- convert_dates(abp.df)

    MAPfreeze.list[[current_epoch]][["data"]][["abp.static"]] <<- abp.df
  }

  #################################################################
  # Process biomarker data if available.

  if (!is.null(biomarkers)) {
    biomarkers.df <- biomarkers %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    var_w_comparison_operators <- grep(
      "bld|np|csf|biomarkers",
      grep(
        "notes|flow|occup",
        names(biomarkers.df)[grepl("(>|<)([[:space:]]*)(\\d+)", biomarkers.df)],
        invert = T,
        v = T),
      v = T)

    if (length(var_w_comparison_operators) > 0) {
      for (var in var_w_comparison_operators) {
        biomarkers.df[, var] <- sapply(biomarkers.df[, var], process_comparison_operators, USE.NAMES = FALSE)
      }
    }

    var_w_comparison_operators <- NULL

    biomarkers.df <- process_calculated_fields(data = biomarkers.df, data_label = "biomarkers", epoch = current_epoch)

    biomarkers.df <- process_factor_variables(data = biomarkers.df, data_label = "biomarkers", epoch = current_epoch)

    biomarkers.df <- convert_dates(biomarkers.df)

    MAPfreeze.list[[current_epoch]][["data"]][["biomarkers"]] <<- biomarkers.df
  }

  #################################################################
  # Process automated 3T imaging data if available.

  if (!is.null(auto3T)) {
    auto3T.df <- auto3T %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    auto3T.df <- auto3T.df %>%
      mutate_at(
        vars(matches("session_id$")),
        as.character
      )

    auto3T.df <- process_calculated_fields(data = auto3T.df, data_label = "auto3T", epoch = current_epoch)

    auto3T.df <- process_factor_variables(data = auto3T.df, data_label = "auto3T", epoch = current_epoch)

    auto3T.df <- convert_dates(auto3T.df)

    MAPfreeze.list[[current_epoch]][["data"]][["auto3T"]] <<- auto3T.df
  }

  #################################################################
  # Process Breath Hold automated 3T imaging data if available for Epoch 1.

  if (!is.null(auto3TBH)) {
    auto3TBH.df <- auto3TBH %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    auto3TBH.df <- auto3TBH.df %>%
      mutate_at(
        vars(matches("session_id$")),
        as.character
      )

    auto3TBH.df <- process_calculated_fields(data = auto3TBH.df, data_label = "auto3T.bh", epoch = current_epoch)

    auto3TBH.df <- process_factor_variables(data = auto3TBH.df, data_label = "auto3T.bh", epoch = current_epoch)

    auto3TBH.df <- convert_dates(auto3TBH.df)

    auto3TBH.df <- auto3TBH.df %>%
      rename_at(
        vars(-map_id),
        function(x) paste0(breathhold.prefix, x)
      )

    MAPfreeze.list[[current_epoch]][["data"]][["auto3T.bh"]] <<- auto3TBH.df
  }

  #################################################################
  # Process manual 3T imaging data if available.

  if (!is.null(man3T)) {
    man3T.df <- man3T %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    man3T.df <- man3T.df %>%
      mutate_at(
        vars(matches("session_id$")),
        as.character
      )

    man3T.df <- process_calculated_fields(data = man3T.df, data_label = "man3T", epoch = current_epoch)

    man3T.df <- process_factor_variables(data = man3T.df, data_label = "man3T", epoch = current_epoch)

    man3T.df <- convert_dates(man3T.df)

    MAPfreeze.list[[current_epoch]][["data"]][["man3T"]] <<- man3T.df
  }

  #################################################################
  # Process Breath Hold manual 3T imaging data if available for Epoch 1.

  if (!is.null(man3TBH)) {
    man3TBH.df <- man3TBH %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    man3TBH.df <- man3TBH.df %>%
      mutate_at(
        vars(matches("session_id$")),
        as.character
      )

    man3TBH.df <- process_calculated_fields(data = man3TBH.df, data_label = "man3T.bh", epoch = current_epoch)

    man3TBH.df <- process_factor_variables(data = man3TBH.df, data_label = "man3T.bh", epoch = current_epoch)

    man3TBH.df <- convert_dates(man3TBH.df)

    man3TBH.df <- man3TBH.df %>%
      rename_at(
        vars(-map_id),
        function(x) paste0(breathhold.prefix, x)
      )

    MAPfreeze.list[[current_epoch]][["data"]][["man3T.bh"]] <<- man3TBH.df
  }

  #################################################################
  # Process QMASS data if available.

  if (!is.null(qmass)) {
    qmass.df <- qmass %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    qmass.df <- process_calculated_fields(data = qmass.df, data_label = "cardiac.mri", epoch = current_epoch)

    qmass.df <- process_factor_variables(data = qmass.df, data_label = "cardiac.mri", epoch = current_epoch)

    qmass.df <- convert_dates(qmass.df)

    MAPfreeze.list[[current_epoch]][["data"]][["cardiac.mri"]] <<- qmass.df
  }

  #################################################################
  # Process addendum data if available.

  if (!is.null(addendum)) {
    addendum.df <- addendum %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    addendum.df %<>%
      select(
        -one_of(
          Hmisc::Cs(
            enrollment, diagnosis, diagnosis_date, nc_type, mci_amnestic,
            mci_domain, mci_stage, mci_notes, diagnosis_complete, np_examiner
          )
        )
      ) %>%
      rename_at(
        vars(Hmisc::Cs(np_date, np_notes, neuropsychological_assessment_complete)),
        function(x) paste0(x, "_addend")
      )

    addendum.df %<>% mutate_if(
      names(.) %in% Cs(
        np_pvlt1, np_pvlt2, np_pvlt3, np_pvlt4, np_pvlt5, np_pvlta_tot,
        np_pvlta_intrus, np_pvlta_pers, np_pvlta_clust, np_pvlt_init_intrus, np_pvlt_tt_intrus,
        np_pvlt_wt_pers, np_pvlta_prim, np_pvlta_mid, np_pvlta_rec, np_pvlta_primperc, np_pvlta_midperc, np_pvlta_recperc,
        np_pvlt6, np_pvlt7, np_pvlt8_fruit, np_pvlt8_office, np_pvlt8_clothing, np_pvlt8, np_pvlt9,
        np_pvlt10_fruit, np_pvlt10_office, np_pvlt10_clothing, np_pvlt10,
        np_pvltrecog_m, np_pvltrecog_foil, np_pvltrecog_falsepos, np_pvltrecog_discrim
      ),
      funs(missing_to_na(., equal.val = -99))
    )

    var_w_comparison_operators <- NULL

    var_w_comparison_operators <- grep(
      "bld|np|csf|biomarkers",
      grep(
        "notes|flow|occup",
        names(addendum.df)[grepl("(>|<)([[:space:]]*)(\\d+)", addendum.df)],
        invert = T,
        v = T),
      v = T)

    if (length(var_w_comparison_operators) > 0) {
      for (var in var_w_comparison_operators) {
        addendum.df[, var] <- sapply(addendum.df[, var], process_comparison_operators, USE.NAMES = FALSE)
      }
    }

    var_w_comparison_operators <- NULL

    addendum.df <- invalidate_neuropsych_addendum(addendum.df)

    addendum.df <- process_calculated_fields(data = addendum.df, data_label = "addendum", epoch = current_epoch)

    addendum.df <- process_factor_variables(data = addendum.df, data_label = "addendum", epoch = current_epoch)

    addendum.df <- convert_dates(addendum.df)

    MAPfreeze.list[[current_epoch]][["data"]][["addendum"]] <<- addendum.df
  }


  ###############################################################
  # Process CSF data if available.

  if (!is.null(csf)) {
    csf.df <- csf %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    var_w_comparison_operators <- grep(
      "bld|np|csf|biomarkers",
      grep(
        "notes|flow|occup",
        names(csf.df)[grepl("(>|<)([[:space:]]*)(\\d+)", csf.df)],
        invert = T,
        v = T),
      v = T)

    if (length(var_w_comparison_operators) > 0) {
      for (var in var_w_comparison_operators) {
        csf.df[, var] <- sapply(csf.df[, var], process_comparison_operators, USE.NAMES = FALSE)
      }
    }

    var_w_comparison_operators <- NULL

    csf.df <- process_calculated_fields(data = csf.df, data_label = "csf", epoch = current_epoch)

    csf.df <- process_factor_variables(data = csf.df, data_label = "csf", epoch = current_epoch)

    csf.df <- convert_dates(csf.df)

    MAPfreeze.list[[current_epoch]][["data"]][["csf"]] <<- csf.df
  }

  #################################################################
  # Process SRT error data if available.

  if (!is.null(srt)) {
    srt.df <- srt %>%
      clear_labels() %>%
      format_id() %>%
      remove_unnecesary_vars() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer")),
        ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
      ) %>%
      mutate_if(
        ~ any(class(.) %in% c("character")),
        ~ missing_to_na(., equal.val = c("-6666", "-7777", "-8888", "-9999"), restrict.sign = TRUE)
      )

    srt.df <- convert_dates(srt.df)

    MAPfreeze.list[[current_epoch]][["data"]][["srt.static"]] <<- srt.df
  }
}
