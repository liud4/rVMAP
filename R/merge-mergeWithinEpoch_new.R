#' Function to merge data within each epoch.
#'
#' @param epoch Epoch number
#' @param save.file A file path to an RDS object where the merged data will be saved.
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

mergeWithinEpoch <- function(
  epoch, save.file, main, abp = NULL, biomarkers = NULL,
  auto3T = NULL, auto3TBH = NULL, man3T = NULL, man3TBH = NULL,
  qmass = NULL, addendum = NULL, csf = NULL, srt = NULL,
  breathhold.prefix = "bHold.") {

  # Make sure to have sourced MiscUtilityFunctions.R first

  # cat("Processing and merging within Epoch ", epoch, ".\n")

  #################################################################
  # Start with the main file for this epoch

  mydat <- main

  source(file.path(function.dir, "preprocessingMain.R"))

  # Set -9999's to missing, etc.
  mydat <- preprocessingMain(mydat)

  # change underscores to periods
  # names(mydat) <- gsub("\\_", "\\.", names(mydat))

  # mydat <- mydat[order(mydat$map_id), ]
  #
  # if (is.numeric(mydat$map_id)) {
  #   mydat <- within(mydat, {
  #     # map_id.orig <- as.character(map_id)
  #     map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
  #   })
  #   mydat1 <- mydat
  # } else {
  #   mydat <- within(mydat, {
  #     # map_id.orig <- map_id
  #     map_id <- gsub("\\-\\-[0-9]", "", map_id)
  #   })
  #   mydat1 <- mydat[!duplicated(mydat$map_id), ]
  # }

  mydat <- format_id(mydat)

  print(paste0("The 'main' data has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  #################################################################

  #################################################################
  # Merge with abp data if available.
  # Keep ALL rows from abp file
  if (!is.null(abp)) {
    PrintPreMergeMessage("ABP data", epoch)

    abp.df <- abp
    # names(abpdat) <- gsub("\\_", "\\.", names(abpdat))
    # this file also has numeric map_id... but no duplicates

    format_id(abp.df)

    mydat %<>% left_join(
      abp.df[, c("map_id", setdiff(names(abp.df), names(mydat)))],
      by = "map_id"
    )

    # mydat1 <- merge(
    #   mydat1,
    #   abpdat[, c("map_id", setdiff(names(abp.dat), names(mydat1)))],
    #   by = "map_id",
    #   all = TRUE
    # )

    print(paste0("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Merge in biomarker data if available
  if (!is.null(biomarkers)) {
    PrintPreMergeMessage("biomarker data", epoch)

    #biomarkdat <- read.csv(biomarkfile, stringsAsFactors =  FALSE)
    biomarkers.df <- biomarkers

    # names(biomarkdat) <- gsub("\\_", "\\.", names(biomarkdat))

    # this file has numeric map_id, but no duplicates
    # biomarkers.dat <- within(biomarkers.dat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    # })

    format_id(biomarkers.df)

    # numericCols <- names(biomarkdat1)[sapply(biomarkdat1, is.numeric)]
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus9999)
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus8888)
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus7777)

    biomarkers.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888),
        missingtoNA(., equal.val = -7777)
      )
    )

    CheckNamesBeforeMerge(mydat, biomarkers.df, c('map_id'))

    mydat %<>% left_join(
      biomarkers.df,
      by = "map_id"
    )

    # mydat1 <- merge(
    #   mydat1,
    #   biomarkers.dat1,
    #   by = "map_id",
    #   all.x = TRUE
    # )

    print(paste0("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Merge with automated 3T imaging data if available.
  if (!is.null(auto3T)) {
    PrintPreMergeMessage("automated 3T imaging data", epoch)

    #auto3Tdat <- read.csv(auto3Tfile, stringsAsFactors = FALSE)
    auto3T.df <- auto3T
    # names(auto3Tdat) <- gsub("\\_", "\\.", names(auto3Tdat))
    # this file also has numeric map_id... but no duplicates
    # auto3Tdat <- within(auto3Tdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    # })
    format_id(auto3T.df)

    # auto3Tdat1 <- auto3Tdat

    # numericCols <- names(auto3Tdat1)[sapply(auto3Tdat1, is.numeric)]
    # auto3Tdat1[, numericCols] <-
    #   apply(auto3Tdat1[, numericCols], 2, minus9999)
    # auto3Tdat1[, numericCols] <-
    #   apply(auto3Tdat1[, numericCols], 2, minus8888)

    auto3T.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    CheckNamesBeforeMerge(mydat, auto3T.df, c('map_id'))

    mydat %<>% left_join(
      auto3T.df,
      by = "map_id"
    )

    # mydat1 <- merge(
    #   mydat1,
    #   auto3Tdat1,
    #   by = "map_id",
    #   all = TRUE
    # )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Next: merge in the Auto 3T Breath Hold data if available
  # (applies to Epoch 1 only)
  if (!is.null(auto3TBH)) {
    PrintPreMergeMessage("Breath Hold Auto3T imaging data", epoch)

    auto3TBH.df <- auto3TBH

    #names(auto3TBHdat) <- gsub("\\_", "\\.", names(auto3TBHdat))

    # auto3TBHdat <- within(auto3TBHdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    # })

    format_id(auto3T.df)

    # 20180205 OAK: Removed the code below as it is fixed.
    # if("ma.left.vaudate.vol" %in% names(auto3TBHdat)){
    #     names(auto3TBHdat)[names(auto3TBHdat) == "ma.left.vaudate.vol"] <-
    #     "ma.left.caudate.vol"
    # }

    # numericCols <- names(auto3TBHdat)[sapply(auto3TBHdat, is.numeric)]
    # auto3TBHdat[, numericCols] <-
    #   apply(auto3TBHdat[, numericCols], 2, minus9999)
    # auto3TBHdat[, numericCols] <-
    #   apply(auto3TBHdat[, numericCols], 2, minus8888)

    auto3TBH.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    #  add prefix for everything except map_id
    # names(auto3TBHdat)[!(names(auto3TBHdat) == "map_id")] <-
    #   paste0(breathHoldPrefix, names(auto3TBHdat)[!(names(auto3TBHdat) == "map_id")])

    auto3TBH.df <- auto3TBH.df %>%
      rename_at(
        .vars = matches("^map\\_id"),
        .funs = paste0(breathhold.prefix, .)
      )

    CheckNamesBeforeMerge(mydat, auto3TBH.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   auto3TBHdat,
    #   by = "map_id",
    #   all = TRUE
    # )

    mydat %<>% left_join(
      auto3TBH.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  # Merge with Manual 3T imaging data if available.
  if (!is.null(man3T)) {
    PrintPreMergeMessage("manual 3T imaging data", epoch)

    man3T.df <- man3T
    # names(man3Tdat) <- gsub("\\_", "\\.", names(man3Tdat))

    # # 20180126 OAK: added if clauses due to errors in Epoch 3 merge
    # man3Tdat <- within(man3Tdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    #   if ("scan.date" %in% names(man3Tdat)){
    #     scan.date.man3T <- scan.date
    #   }
    #   if ("scan.date.2" %in% names(man3Tdat)){
    #     scan.date.2.man3T <- scan.date.2
    #   }
    #   if ("scan.date.3" %in% names(man3Tdat)){
    #     scan.date.3.man3T <- scan.date.3
    #   }
    #   if ("scan.date.4" %in% names(man3Tdat)){
    #     scan.date.4.man3T <- scan.date.4
    #   }
    #   if ("session.id" %in% names(man3Tdat)){
    #     session.id.man3T <- session.id
    #   }
    #   if ("session.id.2" %in% names(man3Tdat)){
    #     session.id.2.man3T <- session.id.2
    #   }
    #   if ("session.id.3" %in% names(man3Tdat)){
    #     session.id.3.man3T <- session.id.3
    #   }
    #   if ("session.id.4" %in% names(man3Tdat)){
    #     session.id.4.man3T <- session.id.4
    #   }
    # })
    # man3Tdat1 <- man3Tdat

    # indicestoremoveMan3T <- which(
    #   # names(man3Tdat1) %in% c("vmac.id",
    #   #                         "entry.primary", "entry.secondary", "data.entry.complete",
    #   #                         "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
    #   #                         "session.id", "session.id.2", "session.id.3", "session.id.4"
    #   # )
    #   names(man3Tdat1) %in% c("vmac.id",
    #                           "entry.primary", "entry.secondary", "data.entry.complete"
    #   )
    #
    # )
    #
    # man3Tdat1 <- man3Tdat1[, -indicestoremoveMan3T]

    man3T.df %<>%
      select(
        -one_of(
          vmac_id, entry_primary, entry_secondary, data_entry_complete,
          asl_manual_session_id, asl_manual_scan_date,
          cow_session_id, cow_scan_date,
          swi_session_id, swi_scan_date,
          vwi_session_id, vwi_scan_date,
          lacunar_infarcts_session_id, lacunar_infarcts_scan_date
        )
      )

    # 19 Oct 2016: this var is in the BH dataset, but not Facemask:
    if (!("swi.microbleeds.distribution...2" %in% names(man3T.df))) {
      man3Tdat1$swi.microbleeds.distribution...2 <- NA
    }

    # numericCols <-
    #   names(man3Tdat1)[sapply(man3Tdat1, is.numeric)]
    # man3Tdat1[, numericCols] <-
    #   apply(man3Tdat1[, numericCols], 2, minus9999)
    # man3Tdat1[, numericCols] <-
    #   apply(man3Tdat1[, numericCols], 2, minus8888)

    man3T.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    CheckNamesBeforeMerge(mydat, man3T.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   man3Tdat1,
    #   by = "map_id",
    #   all = TRUE
    # )

    mydat %<>% left_join(
      man3T.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Merge in the Manual 3T Breath Hold data if available
  # (applies to Epoch 1 only)
  if (!is.null(man3TBH)) {
    PrintPreMergeMessage("Breath Hold Manual 3T imaging data", epoch)

    man3TBH.df <- man3TBH
    # names(man3TBHdat) <- gsub("\\_", "\\.", names(man3TBHdat))

    # If there is a record w/ map_id xxx, keep that;
    # otherwise keep xxx--1
    # at some point, this may become moot
    # man3TBHdat <- man3TBHdat[order(man3TBHdat$map_id), ]
    #
    # if (is.numeric(man3TBHdat$map_id)) {
    #   man3TBHdat <- within(man3TBHdat, {
    #     map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    #   })
    # } else {
    #   man3TBHdat <- within(man3TBHdat, {
    #     map_id <- gsub("\\-\\-[0-9]", "", map_id)
    #   })
    #   man3TBHdat <- man3TBHdat[!duplicated(man3TBHdat$map_id), ]
    # }

    format_id(man3TBH.df)

    # man3TBHdat <- within(man3TBHdat, {
    #   #scan.date.man3T <- scan.date
    #   scan.date.2.man3T <- scan.date.2
    #   scan.date.3.man3T <- scan.date.3
    #   #scan.date.4.man3T <- scan.date.4
    #   #session.id.man3T <- session.id
    #   session.id.2.man3T <- session.id.2
    #   session.id.3.man3T <- session.id.3
    #   #session.id.4.man3T <- session.id.4
    #   # The raw vars are labelled later,
    #   # in the manual3T() function
    # })

    # indicestoremoveMan3TBH <- which(
    #   names(man3TBHdat) %in% c("vmac.id", "entry.primary", "entry.secondary",
    #                            "data.entry.complete",
    #                            "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
    #                            "session.id", "session.id.2", "session.id.3", "session.id.4"
    #   )
    # )

    # man3TBHdat <- man3TBHdat[, -indicestoremoveMan3TBH]

    man3TBH.df %<>%
      select(
        -one_of(
          vmac_id, entry_primary, entry_secondary, data_entry_complete,
          asl_manual_session_id, asl_manual_scan_date,
          cow_session_id, cow_scan_date,
          swi_session_id, swi_scan_date,
          lacunar_infarcts_session_id, lacunar_infarcts_scan_date
        )
      )

    # numericCols <-
    #   names(man3TBHdat)[sapply(man3TBHdat, is.numeric)]
    # man3TBHdat[, numericCols] <-
    #   apply(man3TBHdat[, numericCols], 2, minus9999)
    # man3TBHdat[, numericCols] <-
    #   apply(man3TBHdat[, numericCols], 2, minus8888)

    man3TBH.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    #  add prefix for everything except map_id
    # names(man3TBHdat)[!(names(man3TBHdat) == "map_id")] <-
    #   paste0(breathHoldPrefix, names(man3TBHdat)[!(names(man3TBHdat) == "map_id")])

    man3TBH.df <- man3TBH.df %>%
      rename_at(
        .vars = matches("^map\\_id"),
        .funs = paste0(breathhold.prefix, .)
      )

    CheckNamesBeforeMerge(mydat, man3TBH.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   auto3TBHdat,
    #   by = "map_id",
    #   all = TRUE
    # )

    mydat %<>% left_join(
      man3TBH.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Merge in QMASS data if available
  if (!is.null(qmass)) {
    PrintPreMergeMessage("qmass data", epoch)

    qmass.df <- qmass
    # names(qmassdat) <- gsub("\\_", "\\.", names(qmassdat))

    # this file has numeric map_id, but no duplicates
    # qmassdat <- within(qmassdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    #   rm(vmac_id)
    # })

    format_id(qmass.df)

    qmass.df %<>%
      select(-vmac_id)

    # qmassdat1 <- qmassdat

    # numericCols <- names(qmassdat1)[sapply(qmassdat1, is.numeric)]
    # qmassdat1[, numericCols] <-
    #   apply(qmassdat1[, numericCols], 2, minus9999)
    # qmassdat1[, numericCols] <-
    #   apply(qmassdat1[, numericCols], 2, minus8888)
    # qmassdat1[, numericCols] <-
    #   apply(qmassdat1[, numericCols], 2, minus7777)

    qmassdat1 %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888),
        missingtoNA(., equal.val = -7777)
      )
    )

    CheckNamesBeforeMerge(mydat, qmass.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   qmassdat1,
    #   by = "map_id",
    #   all.x = TRUE
    # )

    mydat %<>% left_join(
      qmass.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  ###############################################################

  #################################################################
  # Merge in addendum data if available
  #  after first appending .addend to varnames
  if (!is.null(addendum)) {
    PrintPreMergeMessage("addendum data", epoch)

    addendum.df <- addendum
    # names(addendumdat) <- gsub("\\_", "\\.", names(addendumdat))

    # addendumdat <- within(addendumdat, {
    #   map_id <- as.numeric(map_id)
    # })
    # addendumdat <- addendumdat[!is.na(addendumdat$map_id) & addendumdat$map_id > 0, ]
    # addendumdat <- within(addendumdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    # })

    # keep only one record per vmac_id
    # keeping --1 at request of AJ as we do w/ eligibility (email 13 Feb 2015)
    # at some point, this may become moot

    # addendum.df <- addendum.df[order(addendum.df$vmac_id), ]

    addendum.df %<>%
      arrange(vmac_id) %>%
      mutate(
        vmac_id_short <- gsub("\\-\\-[0-9]", "", vmac_id)
      )

    # correspondence btw vmac id & map id should be 1-1
    idcount <- with(addendum.df, tapply(map_id, vmac_id_short, function(vec) length(unique(vec)) > 1))
    print("Possible problems with addendum database:\n")
    print("The following VMAC IDs are associated with more than one MAP ID:\n")
    print(addendum.df[addendum.df$vmac_id_short %in% names(idcount)[idcount], c("vmac_id", "map_id")], row.names = FALSE)

    # addendumdat <- addendumdat[order(addendumdat$map_id), ]
    addendum.df %<>%
      arrange(map_id)

    idcount2 <- with(addendum.df, tapply(vmac_id_short, map_id, function(vec) length(unique(vec)) > 1))
    print("The following MAP IDs are associated with more than one VMAC ID:\n")
    print(addendum.df[addendum.df$map_id %in% names(idcount2)[idcount2], c("map_id", "vmac_id")], row.names = FALSE)
    # cat("~~~~~~~~~~~~~~~~~~~\n")
    # This puts reconciled first, then --1, then --2
    # addendumdat <- addendumdat[order(addendumdat$vmac.id), ]

    addendum.df %<>%
      arrange(vmac_id)

    # Take the first row for each vmac id.
    # This will be the reconciled one if avail, or else the "--1"
    # addendumdat1 <- addendumdat[!duplicated(addendumdat$vmac.id.short), ]

    addendum.df %<>% distinct(
      vmac_id_short, .keep_all = TRUE
    )

    # numericCols <- names(addendumdat1)[sapply(addendumdat1, is.numeric)]
    # addendumdat1[, numericCols] <-
    #   apply(addendumdat1[, numericCols], 2, minus9999)
    # addendumdat1[, numericCols] <-
    #   apply(addendumdat1[, numericCols], 2, minus8888)

    addendum.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    # indicestoremoveAddendum <- which(names(addendumdat1) %in%
    #                                    c("vmac.id", "entry.primary", "entry.secondary",
    #                                      "data.entry.complete", "enrollment", "diagnosis",
    #                                      "diagnosis.date", "nc.type", "mci.amnestic",
    #                                      "mci.domain", "mci.stage", "mci.notes",
    #                                      "diagnosis.complete", "np.examiner"))
    # addendumdat1 <- addendumdat1[, -indicestoremoveAddendum]
    # names(addendumdat1)[names(addendumdat1) == "np.date"] <-
    #   "np.date.addend"
    # names(addendumdat1)[names(addendumdat1) == "np.notes"] <-
    #   "np.notes.addend"
    # names(addendumdat1)[names(addendumdat1) == "neuropsychological.assessment.complete"] <-
    #   "neuropsychological.assessment.complete.addend"

    addendum.df %<>%
      select(
        -one_of(
          vmac_id, entry_primary, entry_secondary, data_entry_complete,
          enrollment, diagnosis, diagnosis_date, nc_type, mci_amnestic,
          mci_domain, mci_stage, mci_notes, diagnosis_complete, np_examiner
        )
      ) %>%
      rename_at(
        .vars = one_of(np_date, np_notes, neuropsychological_assessment_complete),
        .funs = paste0(., ".addend")
      )

    CheckNamesBeforeMerge(mydat, addendum.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   addendumdat1,
    #   by = "map_id",
    #   all.x = TRUE)

    mydat %<>% left_join(
      addendum.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  # Dec 13 2016: Dandan updated PVLT variable lists below to set -99 as missing, based on the list Katie provided for PVLT EFA projects.
  # 15 Dec 2016, LS: we will keep this code here because it applies to variables in the addendum for Epoch 1, but
  # to variables in the main dataset for Epoch 2.
  # pvltFor99 <-Cs(np.pvlt1, np.pvlt2, np.pvlt3, np.pvlt4, np.pvlt5, np.pvlta.tot,
  #                np.pvlta.intrus, np.pvlta.pers, np.pvlta.clust, np.pvlt.init.intrus, np.pvlt.tt.intrus,
  #                np.pvlt.wt.pers, np.pvlta.prim, np.pvlta.mid, np.pvlta.rec, np.pvlta.primperc, np.pvlta.midperc, np.pvlta.recperc,
  #                np.pvlt6, np.pvlt7, np.pvlt8.fruit, np.pvlt8.office, np.pvlt8.clothing, np.pvlt8, np.pvlt9,
  #                np.pvlt10.fruit, np.pvlt10.office, np.pvlt10.clothing, np.pvlt10,
  #                np.pvltrecog.m, np.pvltrecog.foil, np.pvltrecog.falsepos, np.pvltrecog.discrim)
  # mydat1[, pvltFor99] <- apply(mydat1[, pvltFor99], 2,
  #                              minus99)

  mydat %<>% mutate_at(
    .vars = one_of(
      np_pvlt1, np_pvlt2, np_pvlt3, np_pvlt4, np_pvlt5, np_pvlta_tot,
      np_pvlta_intrus, np_pvlta_pers, np_pvlta_clust, np_pvlt_init_intrus, np_pvlt_tt_intrus,
      np_pvlt_wt_pers, np_pvlta_prim, np_pvlta_mid, np_pvlta_rec, np_pvlta_primperc, np_pvlta_midperc, np_pvlta_recperc,
      np_pvlt6, np_pvlt7, np_pvlt8_fruit, np_pvlt8_office, np_pvlt8_clothing, np_pvlt8, np_pvlt9,
      np_pvlt10_fruit, np_pvlt10_office, np_pvlt10_clothing, np_pvlt10,
      np_pvltrecog_m, np_pvltrecog_foil, np_pvltrecog_falsepos, np_pvltrecog_discrim
    ),
    .funs = missingtoNA(., equal.val = -99)
  )

  ###############################################################

  ###############################################################
  # Merge in csf data if available
  if (!is.null(csf)) {
    PrintPreMergeMessage("csf data", epoch)

    #csfdat <- read.csv(csffile, stringsAsFactors =  FALSE)
    csf.df <- csf
    # names(csfdat) <- gsub("\\_", "\\.", names(csfdat))
    # keep only one record per map_id

    # If there is a record w/ map_id xxx, keep that;
    # otherwise keep xxx--1
    # at some point, this may become moot
    # csfdat <- csfdat[order(csfdat$map_id), ]
    #
    # if(is.numeric(csfdat$map_id)){
    #   csfdat <- within(csfdat, {
    #     map_id.orig <- as.character(map_id)
    #     map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    #   })
    #   csfdat1 <- csfdat
    # } else {
    #   csfdat <- within(csfdat, {
    #     map_id.orig <- map_id
    #     map_id <- gsub("\\-\\-[0-9]", "", map_id)
    #   })
    #   csfdat1 <- csfdat[!duplicated(csfdat$map_id), ]
    # }

    format_id(csf.df)

    # indicestoremove <- which(names(csfdat1) %in% c("vmac.id", "entry.primary", "entry.secondary", "data.entry.complete", "map_id.orig"))
    # csfdat1 <- csfdat1[, -indicestoremove]

    csf.df %<>% select(
      -one_of(vmac_id, entry_primary, entry_secondary, data_entry_complete)
    ) %>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

#     numericCols <- names(csfdat1)[sapply(csfdat1, is.numeric)]
#     csfdat1[, numericCols] <-
#       apply(csfdat1[, numericCols], 2, minus9999)
#     csfdat1[, numericCols] <-
#       apply(csfdat1[, numericCols], 2, minus8888)

    # The raw vars are labelled later,
    # in the csflabel() function

    CheckNamesBeforeMerge(mydat, csf.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   csfdat1,
    #   by = "map_id",
    #   all.x = TRUE)

    mydat %<>% left_join(
      csf.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Merge in SRT error data if available
  if (!is.null(srt)) {
    PrintPreMergeMessage("SRT error data", epoch)

    srt.df <- srt

    # srtdat <- read.csv(srtfile, stringsAsFactors =  FALSE)
    # names(srtdat) <- gsub("\\_", "\\.", names(srtdat))

    # this file has numeric map_id, but no duplicates
    # srtdat <- within(srtdat, {
    #   map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    #   rm(vmac.id)
    # })

    format_id(srt.df)

    srt.df %<>%
      select(-vmac_id)

    # srtdat1 <- srtdat
    # numericCols <- names(srtdat1)[sapply(srtdat1, is.numeric)]
    # srtdat1[, numericCols] <-
    #   apply(srtdat1[, numericCols], 2, minus9999)
    # srtdat1[, numericCols] <-
    #   apply(srtdat1[, numericCols], 2, minus8888)
    # srtdat1[, numericCols] <-
    #   apply(srtdat1[, numericCols], 2, minus7777)

    srt.df %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888),
        missingtoNA(., equal.val = -7777)
      )
    )

    CheckNamesBeforeMerge(mydat, srt.df, c('map_id'))

    # mydat1 <- merge(
    #   mydat1,
    #   srtdat1,
    #   by = "map_id",
    #   all.x = TRUE)

    mydat %<>% left_join(
      srt.df,
      by = "map_id"
    )

    print(paste("The merged data now has ", nrow(mydat), " rows and ", ncol(mydat), "columns."))
  }

  #################################################################

  #################################################################
  # Add in the epoch number
  mydat <- within(mydat, {
    epoch <- epoch
    label(epoch) <- "Epoch"
    label(map_id.orig) <- "Original map_id from REDCap, for checking"
  })

  # save this as a first step
  saveRDS(mydat, file = save.file)

  # Do not return the file!
  return(NULL)
}
