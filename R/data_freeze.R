#' Function to freeze data by downloading it from REDCap. The API tokens must be unlocked from a secure, encrypted vault.
#'
#' @param box.dir User path to Box home directory (parent directory of "VMAC BIOSTAT").
#' @param quarterly.download A logical value indicating whether this is a regularly scheduled download or if it is off-cycle (\code{scheduled download = TRUE}; \code{interim download = FALSE}). This variable determines the save location.
#' @param tokens.list The decrypted list of databases to download from REDCap.
#' @param redcap.api.uri The URI for the REDCap API
#' @param save A logical value indicating whether to save the output as an RDS file in the default directory ("rawData" or "rawData/temp", depending on the value of \code{quarterly.download})
#' @param return A logical value indicating whether to return the output.
#' @return A comprehensive list object containing all the raw data necessary to perform a data merge.
#' @export
#'
#' @examples
#' MAPfreeze_interim.list <- data_freeze(quarterly.download = FALSE, save = TRUE, return = TRUE)

data_freeze <- function(box.dir = file.path("~", "box"),
                        quarterly.download,
                        tokens.list = secret::get_secret(name = "MAP_redcap_tokens.secret"),
                        redcap.api.uri = "https://redcap.vanderbilt.edu/api/",
                        save = TRUE,
                        return = TRUE) {

  # define important directories
  vmac.dir <- file.path(box.dir, "VMAC BIOSTAT")
  data.dir <- file.path(vmac.dir, "DATA", "MAP")
  med.dir <- file.path(data.dir, "Medication")
  surg.dir <- file.path(data.dir, "Surgeries")
  data.raw.dir <- ifelse(
    quarterly.download,
    file.path(data.dir, "rawData"),
    file.path(data.dir, "rawData", "temp")
  )

  # set timezone
  Sys.setenv(TZ = "US/Central")

  # initialize list
  MAPfreeze.list <- list(
    general = list(
      timestamp = format(Sys.time(), "%Y%m%d %H:%M", tz = "US/Central", usetz = FALSE),
      versions = c(
        R = R.version$version.string,
        REDCapR = paste0(packageVersion('REDCapR')),
        Hmisc = paste0(packageVersion('Hmisc')),
        REDCap = NA
      ),
      sysinfo = Sys.info()[c("sysname", "release", "nodename", "login", "user", "effective_user")]
    ),
    epoch_0 = list(
      project = tokens.list[tokens.list$epoch == 0, "project"],
      shortname = tokens.list[tokens.list$epoch == 0, "shortname"],
      token = tokens.list[tokens.list$epoch == 0, "token"]
    ),
    epoch_1 = list(
      project = tokens.list[tokens.list$epoch == 1, "project"],
      shortname = tokens.list[tokens.list$epoch == 1, "shortname"],
      token = tokens.list[tokens.list$epoch == 1, "token"]
    ),
    epoch_2 = list(
      project = tokens.list[tokens.list$epoch == 2, "project"],
      shortname = tokens.list[tokens.list$epoch == 2, "shortname"],
      token = tokens.list[tokens.list$epoch == 2, "token"]
    ),
    epoch_3 = list(
      project = tokens.list[tokens.list$epoch == 3, "project"],
      shortname = tokens.list[tokens.list$epoch == 3, "shortname"],
      token = tokens.list[tokens.list$epoch == 3, "token"]
    ),
    epoch_4 = list(
      project = tokens.list[tokens.list$epoch == 4, "project"],
      shortname = tokens.list[tokens.list$epoch == 4, "shortname"],
      token = tokens.list[tokens.list$epoch == 4, "token"]
    )
  )

  ###

  # use REDCap API to download data
  for (index.epoch in unique(tokens.list$epoch)){
    epoch <- paste0("epoch_", index.epoch)

    list.names <- MAPfreeze.list[[epoch]][["shortname"]]

    MAPfreeze.list[[epoch]][["metadata"]] <-
      MAPfreeze.list[[epoch]][["data"]] <-
      vector("list", length(list.names))

    names(MAPfreeze.list[[epoch]][["metadata"]]) <-
      names(MAPfreeze.list[[epoch]][["data"]]) <-
      names(MAPfreeze.list[[epoch]][["token"]]) <-
      list.names

    for (index.token in 1:length(MAPfreeze.list[[epoch]][["token"]])){
      current.shortname <- MAPfreeze.list[[epoch]][["shortname"]][index.token]
      current.token <- MAPfreeze.list[[epoch]][["token"]][index.token]
      df <- meta.df <- NULL

      # grab data
      df <- REDCapR::redcap_read_oneshot(
        redcap_uri = redcap.api.uri,
        token = current.token,
        raw_or_label = "raw",
        verbose = FALSE
      )$data

      meta.df <- REDCapR::redcap_metadata_read(
        redcap_uri = redcap.api.uri,
        token = current.token,
        verbose = FALSE
      )$data

      # save data
      MAPfreeze.list[[epoch]][["data"]][[current.shortname]] <- df
      MAPfreeze.list[[epoch]][["metadata"]][[current.shortname]] <- meta.df

      # clean up
      remove(df, meta.df)
    }
  }

  MAPfreeze.list$general$versions[["REDCap"]] <- as.character(
    REDCapR::redcap_version(
      redcap_uri = redcap.api.uri,
      token = MAPfreeze.list$epoch_1$token[1],
      verbose = FALSE
    )
  )

  ###

  med.file.date <- 20150817 # date for main medication files
  antihypSubtype.file.date <- 20160928 # date for antihyp subtype files
  surg.file.date <- 20150817 # date for afibsurgery

  # ABP consent info
  track.file <- file.path(
    data.dir, "rawData",
    "MAPParticipantTracki_DATA_2015-08-26_1412.csv"
  )

  # The APOE data
  apoe.file <- file.path(
    data.dir, "rawData",
    "MAPAPOE_DATA_2015-08-26_1415.csv"
  )

  # Selected data from eligibility.
  elig.file <- file.path(
    data.dir, "rawData",
    "MAPEligibility_DATA_2016-11-03_1532.csv"
  )

  chol.file <- file.path(
    med.dir,
    paste0("CholesterolMedNumbers_", med.file.date, ".csv")
  )

  afib.file <- file.path(
    med.dir,
    paste0("AFibMedNumbers_", med.file.date, ".csv")
  )

  antihyp.file <- file.path(
    med.dir, "old (save for reference)",
    paste0("AntihypertensiveMedNumbers_", med.file.date, ".csv")
  )

  diab.file <- file.path(
    med.dir,
    paste0("DiabetesMedNumbers_", med.file.date, ".csv")
  )

  # antihypertensive med subtypes
  antihypBetaBlocker.file <- file.path(
    med.dir,
    paste0("antiHyp_BetaBlockers_", antihypSubtype.file.date, ".csv")
  )

  antihypBetaBlockerIfNotDrop.file <- file.path(
    med.dir,
    paste0("antiHyp_BetaBlockersIfNotDrops_", antihypSubtype.file.date, ".csv")
  )

  antihypACEInhib.file <- file.path(
    med.dir,
    paste0("antiHyp_ACEInhibitors_", antihypSubtype.file.date, ".csv")
  )

  antihypARB.file <- file.path(
    med.dir,
    paste0("antiHyp_ARBs_", antihypSubtype.file.date, ".csv")
  )

  antihypCCB.file <- file.path(
    med.dir,
    paste0("antiHyp_CaChannelBlockers_", antihypSubtype.file.date, ".csv")
  )

  antihypKSD.file <- file.path(
    med.dir,
    paste0("antiHyp_PotassiumSparingDiuretics_", antihypSubtype.file.date, ".csv")
  )

  antihypOther.file <- file.path(
    med.dir,
    paste0("antiHyp_Other_", antihypSubtype.file.date, ".csv")
  )

  # surgery lookup files
  afibsurg.file <- file.path(
    surg.dir,
    paste0("AFibSurgNumbers_", surg.file.date, ".csv")
  )

  # epoch_1
  abp.raw.derived.file <- file.path(
    data.dir,
    "ABP_RawAndDerived",
    "MapAbpData_Epoch1_20150625.rds"
  )

  srt.file <- file.path(
    data.raw.dir,
    "MAPSRTErrorAnalysisEpoch1_DATA_2017-09-01_0936.csv"
  )

  ###

  # epoch_0

  static.list.0 = list(
    project = paste0(
      "MAP ",
      c("ABP Consent",
        "APOE",
        "Selected Eligibility",
        "Cholesterol",
        "Atrial Fibrillation",
        "Antihypertensive",
        "Diabetes",
        "Antihypertensive - Beta Blockers",
        "Antihypertensive - Beta Blockers if not Drop",
        "Antihypertensive - ACE Inhibitors",
        "Antihypertensive - Angiotensin II Receptor Blockers",
        "Antihypertensive - Calcium Channel Blockers",
        "Antihypertensive - Potasium Sparing Diuretics",
        "Antihypertensive - Other",
        "Atrial Fibrillation Surgery"
      ),
      " (Static)"
    ),
    shortname = paste0(
      c("tracking",
        "apoe",
        "eligibility",
        "cholesterol",
        "afib",
        "antihyp",
        "diabetes",
        "antihypBetaBlocker",
        "antihypBetaBlockerIfNotDrop",
        "antihypACEInhib",
        "antihypARB",
        "antihypCCB",
        "antihypKSD",
        "antihypOther",
        "afibsurg"
      ),
      ".static"
    ),
    token = rep("static.file", 15),
    data = list(
      read.csv(track.file, stringsAsFactors = FALSE),
      read.csv(apoe.file, stringsAsFactors = FALSE),
      read.csv(elig.file, stringsAsFactors = FALSE),
      read.csv(chol.file, stringsAsFactors = FALSE),
      read.csv(afib.file, stringsAsFactors = FALSE),
      read.csv(antihyp.file, stringsAsFactors = FALSE),
      read.csv(diab.file, stringsAsFactors = FALSE),
      read.csv(antihypBetaBlocker.file, stringsAsFactors = FALSE),
      read.csv(antihypBetaBlockerIfNotDrop.file, stringsAsFactors = FALSE),
      read.csv(antihypACEInhib.file, stringsAsFactors = FALSE),
      read.csv(antihypARB.file, stringsAsFactors = FALSE),
      read.csv(antihypCCB.file, stringsAsFactors = FALSE),
      read.csv(antihypKSD.file, stringsAsFactors = FALSE),
      read.csv(antihypOther.file, stringsAsFactors = FALSE),
      read.csv(afibsurg.file, stringsAsFactors = FALSE)
    ),
    metadata = rep("NA", 15)
  )

  names(static.list.0[["token"]]) <- static.list.0$shortname
  names(static.list.0[["data"]]) <- static.list.0$shortname
  names(static.list.0[["metadata"]]) <- static.list.0$shortname

  for (list.names in names(static.list.0)) {
    MAPfreeze.list[["epoch_0"]][[list.names]] <- c(MAPfreeze.list[["epoch_0"]][[list.names]], static.list.0[[list.names]])
  }

  # epoch_1

  static.list.1 = list(
    project = paste0(
      "MAP ",
      c("ABP Raw and Derived Summary Data",
        "SRT Error Variables"
      ),
      " Epoch 1 (Static)"
    ),
    shortname = paste0(
      c("abp",
        "srt"
      ),
      ".static"
    ),
    token = rep("static.file", 2),
    data = list(
      readRDS(abp.raw.derived.file),
      read.csv(apoe.file, stringsAsFactors = FALSE)
    ),
    metadata = rep("NA", 2)
  )

  names(static.list.1[["token"]]) <- static.list.1$shortname
  names(static.list.1[["data"]]) <- static.list.1$shortname
  names(static.list.1[["metadata"]]) <- static.list.1$shortname

  for (list.names in names(static.list.1)) {
    MAPfreeze.list[["epoch_1"]][[list.names]] <- c(MAPfreeze.list[["epoch_1"]][[list.names]], static.list.1[[list.names]])
  }

  if (save == TRUE) {
    saveRDS(
      object = MAPfreeze.list,
      file = file.path(
        data.raw.dir,
        paste0("MAPfreeze", "_", format(Sys.time(), "%Y%m%d"), ".rds")
      )
    )
  }

  if (return == TRUE) {
    return(MAPfreeze.list)
  }
}