data_freeze <- function(box.dir = file.path("~", "Box Sync"),
                        output.dir = NULL,
                        redcap.api.uri = "https://redcap.vanderbilt.edu/api/",
                        save = TRUE,
                        return = TRUE) {

  # define important directories
  vmac.dir <- file.path(box.dir, "VMAC BIOSTAT")
  data.dir <- file.path(vmac.dir, "DATA", "MAP")
  med.dir <- file.path(data.dir, "Medication")
  surg.dir <- file.path(data.dir, "Surgeries")
  data.raw.dir <- file.path(data.dir, "rawData")

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
    vmap_edc = list(
      project = "VMAP Electronic Data Capture Epochs 1-6",
      token = "E727B91E222517FC041A20C728B04A1C"
    ),
    questionnaires = list(
      project = "VMAP Questionnaires",
      token = "EA09F0B49D7670C3D0F69887B89B9C8D"
    ),
    biomarkers = list(
      project = "VMAP CSF, Plasma, and Serum Biomarkers Epoch 1-6",
      token = "7E1BFA5259BD5E59FECD095F0324B62A"
    ),
    apoe = list(
      project = "MAP APOE",
      token = "87BC5364FCBBE6B6942410E415EDE1D2"
    ),
    scanner = list(
      project = "MAP Neuroimaging Link",
      token = "6C1EE1161B2BF40BA37159F3077FD99E"
    )
  )

  ###

  # use REDCap API to download data
  for (redcap.i in setdiff(names(MAPfreeze.list), "general")) {

    message(paste0("Currently downloading: ", MAPfreeze.list[[redcap.i]][["project"]], "\n\n"))
    
    # grab data
    MAPfreeze.list[[redcap.i]][["data"]] <- REDCapR::redcap_read(
      batch_size = 50L,
      redcap_uri = redcap.api.uri,
      token = MAPfreeze.list[[redcap.i]][["token"]],
      raw_or_label = "raw",
      verbose = TRUE
    )$data
    
    MAPfreeze.list[[redcap.i]][["metadata"]] <- REDCapR::redcap_metadata_read(
      redcap_uri = redcap.api.uri,
      token = MAPfreeze.list[[redcap.i]][["token"]],
      verbose = TRUE
    )$data
  }
  
  MAPfreeze.list$general$versions[["REDCap"]] <- as.character(
    REDCapR::redcap_version(
      redcap_uri = redcap.api.uri,
      token = MAPfreeze.list[[redcap.i]][["token"]],
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

  # # The APOE data
  # apoe.file <- file.path(
  #   data.dir, "rawData",
  #   "MAPAPOE_DATA_2015-08-26_1415.csv"
  # )

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

  np.memory.composite.file <- file.path(
    data.raw.dir,
    "Memory_Scores_20210324.rds"
  )

  np.executive.composite.file <- file.path(
    data.raw.dir,
    "EF_Scores_20210329.rds"
  )

  polygenetic.file <- file.path(
    data.raw.dir,
    "VMAP_PRS.rds"
  )

  ###

  # epoch_0

  static.list.0 = list(
    project = paste0(
      "MAP ",
      c("ABP Consent",
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
        "Atrial Fibrillation Surgery",
        "Neuropsych - Memory Composite",
        "Neuropsych - EF Composite",
        "Polygenetic Risk Scores"
      ),
      " (Static)"
    ),
    shortname = paste0(
      c("tracking",
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
        "afibsurg",
        "np.memory.composite",
        "np.executive.composite",
        "polygenetic"
      ),
      ".static"
    ),
    token = rep("static.file", 17),
    data = list(
      read.csv(track.file, stringsAsFactors = FALSE),
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
      read.csv(afibsurg.file, stringsAsFactors = FALSE),
      readRDS(np.memory.composite.file),
      readRDS(np.executive.composite.file),
      readRDS(polygenetic.file)
    ),
    metadata = rep("NA", 17)
  )

  names(static.list.0[["token"]]) <- static.list.0$shortname
  names(static.list.0[["data"]]) <- static.list.0$shortname
  names(static.list.0[["metadata"]]) <- static.list.0$shortname

  for (list.names in names(static.list.0)) {
    MAPfreeze.list[["static"]][[list.names]] <- c(MAPfreeze.list[["static"]][[list.names]], static.list.0[[list.names]])
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
      read.csv(srt.file, stringsAsFactors = FALSE)
    ),
    metadata = rep("NA", 2)
  )

  names(static.list.1[["token"]]) <- static.list.1$shortname
  names(static.list.1[["data"]]) <- static.list.1$shortname
  names(static.list.1[["metadata"]]) <- static.list.1$shortname

  for (list.names in names(static.list.1)) {
    MAPfreeze.list[["static"]][[list.names]] <- c(MAPfreeze.list[["static"]][[list.names]], static.list.1[[list.names]])
  }

  if (save == TRUE) {
    if (is.null(output.dir)) {
      saveRDS(
        object = MAPfreeze.list,
        file = file.path(
          data.raw.dir,
          paste0("MAPfreeze", "_", format(Sys.time(), "%Y%m%d"), ".rds")
        )
      )
    } else {
      saveRDS(
        object = MAPfreeze.list,
        file = file.path(
          output.dir,
          paste0("MAPfreeze", "_", format(Sys.time(), "%Y%m%d"), ".rds")
        )
      )
    }
  }

  if (return == TRUE) {
    return(MAPfreeze.list)
  }
}
