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
  breathhold.prefix = "bHold.") {

  current_epoch <- paste0("epoch_", epoch)

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

  # mydat <- format_id(mydat) # OAK 20181009: is this even needed? This is alraedy being done at the beginning.

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

  MAPfreeze.list[[current_epoch]][["data"]][["main"]] <<- mydat
  # assign(x = deparse(substitute(main)), value = mydat, envir = .GlobalEnv)

  #################################################################
  # Process ABP data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["abp.static"]] <<- abp.df
    # assign(x = deparse(substitute(abp)), value = abp.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process biomarker data if available.

  if (!is.null(biomarkers)) {
    biomarkers.df <- biomarkers %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    MAPfreeze.list[[current_epoch]][["data"]][["biomarkers"]] <<- biomarkers.df
    # assign(x = deparse(substitute(biomarkers)), value = biomarkers.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process automated 3T imaging data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["auto3T"]] <<- auto3T.df
    # assign(x = deparse(substitute(auto3T)), value = auto3T.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process Breath Hold automated 3T imaging data if available for Epoch 1.

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

    MAPfreeze.list[[current_epoch]][["data"]][["auto3T.bh"]] <<- auto3TBH.df
    # assign(x = deparse(substitute(auto3TBH)), value = auto3TBH.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process manual 3T imaging data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["man3T"]] <<- man3T.df
    # assign(x = deparse(substitute(man3T)), value = man3T.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process Breath Hold manual 3T imaging data if available for Epoch 1.

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

    MAPfreeze.list[[current_epoch]][["data"]][["man3T.bh"]] <<- man3TBH.df
    # assign(x = deparse(substitute(man3TBH)), value = man3TBH.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process QMASS data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["cardiac.mri"]] <<- qmass.df
    # assign(x = deparse(substitute(qmass)), value = qmass.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process addendum data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["addendum"]] <<- addendum.df
    # assign(x = deparse(substitute(addendum)), value = addendum.df, envir = .GlobalEnv)
  }


  ###############################################################
  # Process CSF data if available.

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

    MAPfreeze.list[[current_epoch]][["data"]][["csf"]] <<- csf.df
    # assign(x = deparse(substitute(csf)), value = csf.df, envir = .GlobalEnv)
  }

  #################################################################
  # Process SRT error data if available.

  if (!is.null(srt)) {
    srt.df <- srt %>%
      clear_labels() %>%
      format_id() %>%
      mutate_if(
        ~ any(class(.) %in% c("numeric", "integer", "character")),
        ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
      )

    MAPfreeze.list[[current_epoch]][["data"]][["srt.static"]] <<- srt.df
    # assign(x = deparse(substitute(srt)), value = srt.df, envir = .GlobalEnv)
  }
}
