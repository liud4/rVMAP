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

  mydat <- main

  # OAK 20181009: Should these be print or cat? Look into this.
  cat(paste0("The 'main' data has ", nrow(mydat), " rows and ", ncol(mydat), " columns."))

  #################################################################
  # Merge in ABP data if available.

  if (!is.null(abp)) {
    abp.df <- abp

    check_shared_vars(mydat, abp.df, 'map_id')

    mydat %<>% left_join(
      abp.df[, c("map_id", setdiff(names(abp.df), names(mydat)))],
      by = "map_id"
    )

    cat(paste0("After adding in ABP data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in biomarker data if available.

  if (!is.null(biomarkers)) {
    biomarkers.df <- biomarkers

    check_shared_vars(mydat, biomarkers.df, 'map_id')

    mydat %<>% left_join(
      biomarkers.df,
      by = "map_id"
    )

    cat(paste0("After adding in biomarker data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in automated 3T imaging data if available.

  if (!is.null(auto3T)) {
    auto3T.df <- auto3T

    check_shared_vars(mydat, auto3T.df, 'map_id')

    mydat %<>% left_join(
      auto3T.df,
      by = "map_id"
    )

    cat(paste0("After adding in automated 3T data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in Breath Hold automated 3T imaging data if available for Epoch 1.

  if (!is.null(auto3TBH)) {
    auto3TBH.df <- auto3TBH

    check_shared_vars(mydat, auto3TBH.df, 'map_id')

    mydat %<>% left_join(
      auto3TBH.df,
      by = "map_id"
    )

    cat(paste0("After adding in automatic 3T (breath hold) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in manual 3T imaging data if available.

  if (!is.null(man3T)) {
    man3T.df <- man3T

    check_shared_vars(mydat, man3T.df, 'map_id')

    mydat %<>% left_join(
      man3T.df,
      by = "map_id"
    )

    cat(paste0("After adding in manual 3T data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in Breath Hold manual 3T imaging data if available for Epoch 1.

  if (!is.null(man3TBH)) {
    man3TBH.df <- man3TBH

    check_shared_vars(mydat, man3TBH.df, 'map_id')

    mydat %<>% left_join(
      man3TBH.df,
      by = "map_id"
    )

    cat(paste0("After adding in manual 3T (breath hold) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in QMASS data if available.

  if (!is.null(qmass)) {
    qmass.df <- qmass

    check_shared_vars(mydat, qmass.df, 'map_id')

    mydat %<>% left_join(
      qmass.df,
      by = "map_id"
    )

    cat(paste0("After adding in cardiac MRI (qmass) data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in addendum data if available.

  if (!is.null(addendum)) {
    addendum.df <- addendum

    check_shared_vars(mydat, addendum.df, 'map_id')

    mydat %<>% left_join(
      addendum.df,
      by = "map_id"
    )

    cat(paste0("After adding in addendum data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  ###############################################################
  # Merge in CSF data if available.

  if (!is.null(csf)) {
    csf.df <- csf

    check_shared_vars(mydat, csf.df, 'map_id')

    mydat %<>% left_join(
      csf.df,
      by = "map_id"
    )

    cat(paste0("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
  }

  #################################################################
  # Merge in SRT error data if available.

  if (!is.null(srt)) {
    srt.df <- srt

    check_shared_vars(mydat, srt.df, 'map_id')

    mydat %<>% left_join(
      srt.df,
      by = "map_id"
    )

    cat(paste0("After adding in SRT data, the merged data now has ", nrow(mydat), " rows and ", ncol(mydat), " columns.\n\n"))
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
