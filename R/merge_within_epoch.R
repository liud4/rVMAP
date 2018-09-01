#' Major function to merge raw, non-derived data within each epoch.
#'
#' @param epoch Epoch number
#' @param save.file An optional file path to an RDS object where the merged data will be saved.
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
#' @return A data frame with the above files merged together.
#' @export

merge_within_epoch <- function(
  epoch, save.file = NULL, main, abp = NULL, biomarkers = NULL,
  auto3T = NULL, auto3TBH = NULL, man3T = NULL, man3TBH = NULL,
  qmass = NULL, addendum = NULL, csf = NULL, srt = NULL,
  breathhold.prefix = "bHold.") {

  #################################################################
  # Start with the main file for this epoch.

  mydat <- main %>%
    clear_labels() %>%
    format_id()

  mydat <- mydat %>%
    mutate_if(
      ~ any(class(.) %in% c("numeric", "integer", "character")),
      ~ missing_to_na(., equal.val = c(-6666, -7777, -9999))
    )

  mydat <- mydat %>%
    mutate_at(
      mydat %>% select_if(~ (class(.) %in% c("numeric", "integer", "character"))) %>% names() %>% setdiff(., "mhx_tobac_quit"),
      ~ missing_to_na(., equal.val = -8888)
    )

  mydat <- mydat %>%
    mutate_at(
      grep("ccqself[0-9]+|ccqinform[0-9]+", names(.)),
      ~ missing_to_na(., equal.val = -2222)
    )

  mydat <- mydat %>%
    mutate_at(
      grep("notes|date", grep("^ecogself\\_", names(.), value = T), value = T, invert = T),
      ~ missing_to_na(., equal.val = 0)
    )

  notes.var <- grep("\\_notes$", names(mydat), value = T)
  mydat[, notes.var] <- apply(mydat[, notes.var], 2, function(vec) {
    ifelse(stringr::str_trim(vec) %in% c("-8888", "-9999", ""), NA, vec)
  })

  # mydat <- format_names(mydat)

  mydat <- format_id(mydat)

  print(paste0("The 'main' data has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))

  #################################################################
  # Merge in ABP data if available.

  if (!is.null(abp)) {

    abp.df <- abp
    names(abp.df) <- gsub("\\.", "\\_", names(abp.df))

    abp.df <- abp.df %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    check_shared_vars(mydat, abp.df, 'map_id')

    mydat %<>% left_join(
      abp.df[, c("map_id", setdiff(names(abp.df), names(mydat)))],
      by = "map_id"
    )

    print(paste0("After adding in ABP data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in biomarker data if available.

  if (!is.null(biomarkers)) {

    biomarkers.df <- biomarkers %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    check_shared_vars(mydat, biomarkers.df, 'map_id')

    mydat %<>% left_join(
      biomarkers.df,
      by = "map_id"
    )

    print(paste0("After adding in biomarker data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in automated 3T imaging data if available.

  if (!is.null(auto3T)) {

    auto3T.df <- auto3T %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    to_rename_auto3T <- grep("scan\\_date|scan\\_acquired|session_id", names(auto3T.df), value = T)

    if (length(to_rename_auto3T) > 0) {
      auto3T.df <- auto3T.df %>%
        rename_at(
          vars(to_rename_auto3T),
          function(x) paste0(x, "_auto3T")
        )
    }

    to_remove_auto3T <- which(names(auto3T.df) %in% c("vmac_id", "entry_primary", "entry_secondary", "data_entry_complete"))

    if (length(to_remove_auto3T) > 0) {
      auto3T.df <- auto3T.df[, -to_remove_auto3T]
    }

    check_shared_vars(mydat, auto3T.df, 'map_id')

    mydat %<>% left_join(
      auto3T.df,
      by = "map_id"
    )

    print(paste0("After adding in automated 3T data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in Breath Hold automated 3T imaging data if available for Epoch 1.

  if (!is.null(auto3TBH)) {

    auto3TBH.df <- auto3TBH %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    auto3TBH.df <- auto3TBH.df %>%
      rename_at(
        vars(-map_id),
        function(x) paste0(breathhold.prefix, x)
      )

    check_shared_vars(mydat, auto3TBH.df, 'map_id')

    mydat %<>% left_join(
      auto3TBH.df,
      by = "map_id"
    )

    print(paste0("After adding in automatic 3T (breath hold) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in manual 3T imaging data if available.

  if (!is.null(man3T)) {
    man3T.df <- man3T %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    to_rename_man3T <- grep("scan\\_date|scan\\_acquired|session_id", names(man3T.df), value = T)

    if (length(to_rename_man3T) > 0) {
      man3T.df <- man3T.df %>%
        rename_at(
          vars(to_rename_man3T),
          function(x) paste0(x, "_man3T")
        )
    }

    to_remove_man3T <- which(names(man3T.df) %in% c("vmac_id", "entry_primary", "entry_secondary", "data_entry_complete"))

    if (length(to_remove_man3T) > 0) {
      man3T.df <- man3T.df[, -to_remove_man3T]
    }

    check_shared_vars(mydat, man3T.df, 'map_id')

    mydat %<>% left_join(
      man3T.df,
      by = "map_id"
    )

    print(paste0("After adding in manual 3T data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in Breath Hold manual 3T imaging data if available for Epoch 1.

  if (!is.null(man3TBH)) {

    man3TBH.df <- man3TBH %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    man3TBH.df <- man3TBH.df %>%
      rename_at(
        vars(-map_id),
        function(x) paste0(breathhold.prefix, x)
      )

    check_shared_vars(mydat, man3TBH.df, 'map_id')

    mydat %<>% left_join(
      man3TBH.df,
      by = "map_id"
    )

    print(paste0("After adding in manual 3T (breath hold) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in QMASS data if available.

  if (!is.null(qmass)) {

    qmass.df <- qmass %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    qmass.df %<>%
      select(-vmac_id)

    check_shared_vars(mydat, qmass.df, 'map_id')

    mydat %<>% left_join(
      qmass.df,
      by = "map_id"
    )

    print(paste0("After adding in cardiac MRI (qmass) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in addendum data if available.

  if (!is.null(addendum)) {

    addendum.df <- addendum %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    addendum.df %<>%
      select(
        -one_of(
          Hmisc::Cs(
            vmac_id, entry_primary, entry_secondary, data_entry_complete,
            enrollment, diagnosis, diagnosis_date, nc_type, mci_amnestic,
            mci_domain, mci_stage, mci_notes, diagnosis_complete, np_examiner
          )
        )
      ) %>%
      rename_at(
        vars(Hmisc::Cs(np_date, np_notes, neuropsychological_assessment_complete)),
        function(x) paste0(x, ".addend")
      )

    check_shared_vars(mydat, addendum.df, 'map_id')

    mydat %<>% left_join(
      addendum.df,
      by = "map_id"
    )

    print(paste0("After adding in addendum data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  # Dec 13 2016: Dandan updated PVLT variable lists below to set -99 as missing, based on the list Katie provided for PVLT EFA projects.
  # 15 Dec 2016, LS: we will keep this code here because it applies to variables in the addendum for Epoch 1, but
  # to variables in the main dataset for Epoch 2.

  mydat %<>% mutate_if(
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

  ###############################################################
  # Merge in CSF data if available.

  if (!is.null(csf)) {

    csf.df <- csf %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    csf.df %<>% select(
      -one_of(Cs(vmac_id, entry_primary, entry_secondary, data_entry_complete))
    )

    check_shared_vars(mydat, csf.df, 'map_id')

    mydat %<>% left_join(
      csf.df,
      by = "map_id"
    )

    print(paste0("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Merge in SRT error data if available.

  if (!is.null(srt)) {

    srt.df <- srt %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    check_shared_vars(mydat, srt.df, 'map_id')

    mydat %<>% left_join(
      srt.df,
      by = "map_id"
    )

    print(paste0("After adding in SRT data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))
  }

  #################################################################
  # Add in the epoch number, save, and return the merged data frame.

  mydat <- within(mydat, {
    epoch <- epoch
  })

  if (!is.null(save.file)) {
    saveRDS(mydat, file = save.file)
  }

  return(mydat)
}
