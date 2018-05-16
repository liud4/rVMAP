mergeWithinEpoch <- function(epochnum, main.df, function.dir, firstRds,
                             abp.df = NULL, biomark.df = NULL,
                             auto3T.df = NULL, auto3TBH.df = NULL,
                             man3T.df = NULL, man3TBHfile = NULL,
                             qmass.df = NULL,
                             addendum.df = NULL, csf.df = NULL,
                             srt.df = NULL,
                             breathHoldPrefix = "bHold.") {
  # Returns a dataset with the above files merged
  # into a single dataset using mainfile as the "backbone":
  # unless those values are set to NULL.
  # Also: add in the epoch number

  # Make sure to have sourced MiscUtilityFunctions.R first

  cat("Processing and merging within Epoch ", epochnum, ".\n")

  #################################################################
  # Start with the main file for this epoch

  mydat <- main.df

  source(file.path(function.dir, "preprocessingMain.R"))

  # Set -9999's to missing, etc.
  mydat <- preprocessingMain(mydat)

  # change underscores to periods
  # names(mydat) <- gsub("\\_", "\\.", names(mydat))

  mydat <- mydat[order(mydat$map_id), ]

  if (is.numeric(mydat$map_id)) {
    mydat <- within(mydat, {
      # map_id.orig <- as.character(map_id)
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })
    mydat1 <- mydat
  } else {
    mydat <- within(mydat, {
      # map_id.orig <- map_id
      map_id <- gsub("\\-\\-[0-9]", "", map_id)
    })
    mydat1 <- mydat[!duplicated(mydat$map_id), ]
  }

  print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  #################################################################

  #################################################################
  # Merge with abp data if available.
  # Keep ALL rows from abp file
  if (!is.null(abp.df)) {
    PrintPreMergeMessage("ABP data", epochnum)

    abp.dat <- abp.df
    # names(abpdat) <- gsub("\\_", "\\.", names(abpdat))
    # this file also has numeric map_id... but no duplicates

    abp.dat <- within(abp.dat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })

    mydat1 <- merge(
      mydat1,
      abpdat[, c("map_id", setdiff(names(abp.dat), names(mydat1)))],
      by = "map_id",
      all = TRUE
    )
    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Merge in biomarker data if available
  if (!is.null(biomarkfile)) {
    PrintPreMergeMessage("biomarker data", epochnum)

    #biomarkdat <- read.csv(biomarkfile, stringsAsFactors =  FALSE)
    biomark.dat <- biomark.df

    # names(biomarkdat) <- gsub("\\_", "\\.", names(biomarkdat))

    # this file has numeric map_id, but no duplicates
    biomark.dat <- within(biomark.dat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })

    biomark.dat1 <- biomark.dat

    # numericCols <- names(biomarkdat1)[sapply(biomarkdat1, is.numeric)]
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus9999)
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus8888)
    # biomarkdat1[, numericCols] <-
    #   apply(biomarkdat1[, numericCols], 2, minus7777)

    biomark.dat1 %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888),
        missingtoNA(., equal.val = -7777)
        )
    )

    CheckNamesBeforeMerge(mydat1, biomarkdat1, c('map_id'))

    mydat1 <- merge(
      mydat1,
      biomark.dat1,
      by = "map_id",
      all.x = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Merge with automated 3T imaging data if available.
  if (!is.null(auto3Tfile)) {
    PrintPreMergeMessage("automated 3T imaging data", epochnum)

    #auto3Tdat <- read.csv(auto3Tfile, stringsAsFactors = FALSE)
    auto3Tdat <- auto3T.df
    # names(auto3Tdat) <- gsub("\\_", "\\.", names(auto3Tdat))
    # this file also has numeric map_id... but no duplicates
    auto3Tdat <- within(auto3Tdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })

    auto3Tdat1 <- auto3Tdat

    # numericCols <- names(auto3Tdat1)[sapply(auto3Tdat1, is.numeric)]
    # auto3Tdat1[, numericCols] <-
    #   apply(auto3Tdat1[, numericCols], 2, minus9999)
    # auto3Tdat1[, numericCols] <-
    #   apply(auto3Tdat1[, numericCols], 2, minus8888)

    auto3Tdat1 %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    CheckNamesBeforeMerge(mydat1, auto3Tdat1, c('map_id'))

    mydat1 <- merge(
      mydat1,
      auto3Tdat1,
      by = "map_id",
      all = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Next: merge in the Auto 3T Breath Hold data if available
  # (applies to Epoch 1 only)
  if (!is.null(auto3TBHfile)) {
    PrintPreMergeMessage("Breath Hold Auto3T imaging data", epochnum)

    auto3TBHdat <- auto3TBHfile

    #names(auto3TBHdat) <- gsub("\\_", "\\.", names(auto3TBHdat))

    auto3TBHdat <- within(auto3TBHdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })

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

    auto3TBHdat %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    #  add prefix for everything except map_id
    names(auto3TBHdat)[!(names(auto3TBHdat) == "map_id")] <-
      paste0(breathHoldPrefix, names(auto3TBHdat)[!(names(auto3TBHdat) == "map_id")])

    CheckNamesBeforeMerge(mydat1, auto3TBHdat, c('map_id'))

    mydat1 <- merge(
      mydat1,
      auto3TBHdat,
      by = "map_id",
      all = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  # Merge with Manual 3T imaging data if available.
  if (!is.null(man3Tfile)) {
    PrintPreMergeMessage("manual 3T imaging data", epochnum)

    man3Tdat <- man3Tfile
    # names(man3Tdat) <- gsub("\\_", "\\.", names(man3Tdat))

    # 20180126 OAK: added if clauses due to errors in Epoch 3 merge
    man3Tdat <- within(man3Tdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
      if ("scan.date" %in% names(man3Tdat)){
        scan.date.man3T <- scan.date
      }
      if ("scan.date.2" %in% names(man3Tdat)){
        scan.date.2.man3T <- scan.date.2
      }
      if ("scan.date.3" %in% names(man3Tdat)){
        scan.date.3.man3T <- scan.date.3
      }
      if ("scan.date.4" %in% names(man3Tdat)){
        scan.date.4.man3T <- scan.date.4
      }
      if ("session.id" %in% names(man3Tdat)){
        session.id.man3T <- session.id
      }
      if ("session.id.2" %in% names(man3Tdat)){
        session.id.2.man3T <- session.id.2
      }
      if ("session.id.3" %in% names(man3Tdat)){
        session.id.3.man3T <- session.id.3
      }
      if ("session.id.4" %in% names(man3Tdat)){
        session.id.4.man3T <- session.id.4
      }
    })
    man3Tdat1 <- man3Tdat

    indicestoremoveMan3T <- which(
      names(man3Tdat1) %in% c("vmac.id",
                              "entry.primary", "entry.secondary", "data.entry.complete",
                              "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
                              "session.id", "session.id.2", "session.id.3", "session.id.4"
      )
    )

    man3Tdat1 <- man3Tdat1[, -indicestoremoveMan3T]

    # 19 Oct 2016: this var is in the BH dataset, but not Facemask:
    if (!("swi.microbleeds.distribution...2" %in% names(man3Tdat1))) {
      man3Tdat1$swi.microbleeds.distribution...2 <- NA
    }

    # numericCols <-
    #   names(man3Tdat1)[sapply(man3Tdat1, is.numeric)]
    # man3Tdat1[, numericCols] <-
    #   apply(man3Tdat1[, numericCols], 2, minus9999)
    # man3Tdat1[, numericCols] <-
    #   apply(man3Tdat1[, numericCols], 2, minus8888)

    man3Tdat1 %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    CheckNamesBeforeMerge(mydat1, man3Tdat1, c('map_id'))

    mydat1 <- merge(
      mydat1,
      man3Tdat1,
      by = "map_id",
      all = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Merge in the Manual 3T Breath Hold data if available
  # (applies to Epoch 1 only)
  if (!is.null(man3TBHfile)) {
    PrintPreMergeMessage("Breath Hold Manual 3T imaging data", epochnum)

    man3TBHdat <- man3TBHfile
    # names(man3TBHdat) <- gsub("\\_", "\\.", names(man3TBHdat))

    # If there is a record w/ map_id xxx, keep that;
    # otherwise keep xxx--1
    # at some point, this may become moot
    man3TBHdat <- man3TBHdat[order(man3TBHdat$map_id), ]

    if (is.numeric(man3TBHdat$map_id)) {
      man3TBHdat <- within(man3TBHdat, {
        map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
      })
    } else {
      man3TBHdat <- within(man3TBHdat, {
        map_id <- gsub("\\-\\-[0-9]", "", map_id)
      })
      man3TBHdat <- man3TBHdat[!duplicated(man3TBHdat$map_id), ]
    }

    man3TBHdat <- within(man3TBHdat, {
      #scan.date.man3T <- scan.date
      scan.date.2.man3T <- scan.date.2
      scan.date.3.man3T <- scan.date.3
      #scan.date.4.man3T <- scan.date.4
      #session.id.man3T <- session.id
      session.id.2.man3T <- session.id.2
      session.id.3.man3T <- session.id.3
      #session.id.4.man3T <- session.id.4
      # The raw vars are labelled later,
      # in the manual3T() function
    })

    indicestoremoveMan3TBH <- which(
      names(man3TBHdat) %in% c("vmac.id", "entry.primary", "entry.secondary",
                               "data.entry.complete",
                               "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
                               "session.id", "session.id.2", "session.id.3", "session.id.4"
      )
    )

    man3TBHdat <- man3TBHdat[, -indicestoremoveMan3TBH]

    # numericCols <-
    #   names(man3TBHdat)[sapply(man3TBHdat, is.numeric)]
    # man3TBHdat[, numericCols] <-
    #   apply(man3TBHdat[, numericCols], 2, minus9999)
    # man3TBHdat[, numericCols] <-
    #   apply(man3TBHdat[, numericCols], 2, minus8888)

    man3TBHdat %<>% mutate_if(
      is.numeric,
      funs(
        missingtoNA(., equal.val = -9999),
        missingtoNA(., equal.val = -8888)
      )
    )

    #  add prefix for everything except map_id
    names(man3TBHdat)[!(names(man3TBHdat) == "map_id")] <-
      paste0(breathHoldPrefix, names(man3TBHdat)[!(names(man3TBHdat) == "map_id")])

    CheckNamesBeforeMerge(mydat1, man3TBHdat, c('map_id'))

    mydat1 <- merge(
      mydat1,
      man3TBHdat,
      by = "map_id",
      all = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Merge in QMASS data if available
  if (!is.null(qmassfile)) {
    PrintPreMergeMessage("qmass data", epochnum)

    qmassdat <- qmassfile
    # names(qmassdat) <- gsub("\\_", "\\.", names(qmassdat))

    # this file has numeric map_id, but no duplicates
    qmassdat <- within(qmassdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
      rm(vmac_id)
    })

    qmassdat1 <- qmassdat

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

    CheckNamesBeforeMerge(mydat1, qmassdat1, c('map_id'))

    mydat1 <- merge(
      mydat1,
      qmassdat1,
      by = "map_id",
      all.x = TRUE
    )

    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  ###############################################################

  #################################################################
  # Merge in addendum data if available
  #  after first appending .addend to varnames
  if (!is.null(addendumfile)){
    PrintPreMergeMessage("addendum data", epochnum)

    addendumdat <- addendumfile
    # names(addendumdat) <- gsub("\\_", "\\.", names(addendumdat))

    addendumdat <- within(addendumdat, {
      map_id <- as.numeric(map_id)
    })
    addendumdat <- addendumdat[!is.na(addendumdat$map_id) & addendumdat$map_id > 0, ]
    addendumdat <- within(addendumdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
    })

    # keep only one record per vmac_id
    # keeping --1 at request of AJ as we do w/ eligibility (email 13 Feb 2015)
    # at some point, this may become moot
    addendumdat <- addendumdat[order(addendumdat$vmac.id), ]
    addendumdat <- within(addendumdat, {
      vmac.id.short <- gsub("\\-\\-[0-9]", "", vmac.id)
    })
    # correspondence btw vmac id & map id should be 1-1
    idcount <- with(addendumdat, tapply(map_id, vmac.id.short,
                                        function(vec) length(unique(vec)) > 1))
    cat("~~~~~~~~~~~~~~~~~~~\nPossible problems w/ addendum db:\n")
    cat("The following vmac id's are associated with more than one map id:\n")
    print(addendumdat[addendumdat$vmac.id.short %in% names(idcount)[idcount], Cs(vmac.id, map_id)], row.names = FALSE)

    addendumdat <- addendumdat[order(addendumdat$map_id), ]
    idcount2 <- with(addendumdat, tapply(vmac.id.short, map_id,
                                         function(vec) length(unique(vec)) > 1))
    cat("The following map id's are associated with more than one vmac id:\n")
    print(addendumdat[addendumdat$map_id %in% names(idcount2)[idcount2], Cs(map_id, vmac.id)], row.names = FALSE)
    cat("~~~~~~~~~~~~~~~~~~~\n")
    # This puts reconciled first, then --1, then --2
    addendumdat <- addendumdat[order(addendumdat$vmac.id), ]

    # Take the first row for each vmac id.
    # This will be the reconciled one if avail, or else the "--1"
    addendumdat1 <- addendumdat[!duplicated(addendumdat$vmac.id.short), ]


    numericCols <- names(addendumdat1)[sapply(addendumdat1, is.numeric)]
    addendumdat1[, numericCols] <-
      apply(addendumdat1[, numericCols], 2, minus9999)
    addendumdat1[, numericCols] <-
      apply(addendumdat1[, numericCols], 2, minus8888)

    indicestoremoveAddendum <- which(names(addendumdat1) %in%
                                       c("vmac.id", "entry.primary", "entry.secondary",
                                         "data.entry.complete", "enrollment", "diagnosis",
                                         "diagnosis.date", "nc.type", "mci.amnestic",
                                         "mci.domain", "mci.stage", "mci.notes",
                                         "diagnosis.complete", "np.examiner"))
    addendumdat1 <- addendumdat1[, -indicestoremoveAddendum]
    names(addendumdat1)[names(addendumdat1) == "np.date"] <-
      "np.date.addend"
    names(addendumdat1)[names(addendumdat1) == "np.notes"] <-
      "np.notes.addend"
    names(addendumdat1)[names(addendumdat1) == "neuropsychological.assessment.complete"] <-
      "neuropsychological.assessment.complete.addend"

    CheckNamesBeforeMerge(mydat1, addendumdat1, c('map_id'))
    mydat1 <- merge(
      mydat1,
      addendumdat1,
      by = "map_id",
      all.x = TRUE)
    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  # Dec 13 2016: Dandan updated PVLT variable lists below to set -99 as missing, based on the list Katie provided for PVLT EFA projects.
  # 15 Dec 2016, LS: we will keep this code here because it applies to variables in the addendum for Epoch 1, but
  # to variables in the main dataset for Epoch 2.
  pvltFor99 <-Cs(np.pvlt1, np.pvlt2, np.pvlt3, np.pvlt4, np.pvlt5, np.pvlta.tot,
                 np.pvlta.intrus, np.pvlta.pers, np.pvlta.clust, np.pvlt.init.intrus, np.pvlt.tt.intrus,
                 np.pvlt.wt.pers, np.pvlta.prim, np.pvlta.mid, np.pvlta.rec, np.pvlta.primperc, np.pvlta.midperc, np.pvlta.recperc,
                 np.pvlt6, np.pvlt7, np.pvlt8.fruit, np.pvlt8.office, np.pvlt8.clothing, np.pvlt8, np.pvlt9,
                 np.pvlt10.fruit, np.pvlt10.office, np.pvlt10.clothing, np.pvlt10,
                 np.pvltrecog.m, np.pvltrecog.foil, np.pvltrecog.falsepos, np.pvltrecog.discrim)
  mydat1[, pvltFor99] <- apply(mydat1[, pvltFor99], 2,
                               minus99)



  ###############################################################

  ###############################################################
  # Merge in csf data if available
  if(!is.null(csffile)){
    PrintPreMergeMessage("csf data", epochnum)

    #csfdat <- read.csv(csffile, stringsAsFactors =  FALSE)
    csfdat <- csffile
    names(csfdat) <- gsub("\\_", "\\.", names(csfdat))
    # keep only one record per map_id

    # If there is a record w/ map_id xxx, keep that;
    # otherwise keep xxx--1
    # at some point, this may become moot
    csfdat <- csfdat[order(csfdat$map_id), ]

    if(is.numeric(csfdat$map_id)){
      csfdat <- within(csfdat, {
        map_id.orig <- as.character(map_id)
        map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
      })
      csfdat1 <- csfdat
    } else {
      csfdat <- within(csfdat, {
        map_id.orig <- map_id
        map_id <- gsub("\\-\\-[0-9]", "", map_id)
      })
      csfdat1 <- csfdat[!duplicated(csfdat$map_id), ]
    }

    indicestoremove <- which(names(csfdat1) %in% c("vmac.id", "entry.primary", "entry.secondary", "data.entry.complete", "map_id.orig"))
    csfdat1 <- csfdat1[, -indicestoremove]

    numericCols <- names(csfdat1)[sapply(csfdat1, is.numeric)]
    csfdat1[, numericCols] <-
      apply(csfdat1[, numericCols], 2, minus9999)
    csfdat1[, numericCols] <-
      apply(csfdat1[, numericCols], 2, minus8888)

    # The raw vars are labelled later,
    # in the csflabel() function

    CheckNamesBeforeMerge(mydat1, csfdat1, c('map_id'))
    mydat1 <- merge(
      mydat1,
      csfdat1,
      by = "map_id",
      all.x = TRUE)
    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Merge in SRT error data if available
  if(!is.null(srtfile)){
    PrintPreMergeMessage("SRT error data", epochnum)

    srtdat <- read.csv(srtfile, stringsAsFactors =  FALSE)
    names(srtdat) <- gsub("\\_", "\\.", names(srtdat))

    # this file has numeric map_id, but no duplicates
    srtdat <- within(srtdat, {
      map_id <- formatC(map_id, width = 3, format = "d", flag = "0")
      rm(vmac.id)
    })

    srtdat1 <- srtdat
    numericCols <- names(srtdat1)[sapply(srtdat1, is.numeric)]
    srtdat1[, numericCols] <-
      apply(srtdat1[, numericCols], 2, minus9999)
    srtdat1[, numericCols] <-
      apply(srtdat1[, numericCols], 2, minus8888)
    srtdat1[, numericCols] <-
      apply(srtdat1[, numericCols], 2, minus7777)

    CheckNamesBeforeMerge(mydat1, srtdat, c('map_id'))
    mydat1 <- merge(
      mydat1,
      srtdat1,
      by = "map_id",
      all.x = TRUE)
    print(paste("Data has ", nrow(mydat1), " rows and ", ncol(mydat1), "columns"))
  }

  #################################################################

  #################################################################
  # Add in the epoch number
  mydat1 <- within(mydat1, {
    epoch <- epochnum
    label(epoch) <- "Epoch"
    label(map_id.orig) <- "Original map_id from REDCap, for checking"
  })

  # save this as a first step
  saveRDS(mydat1, file = firstRds)

  # Do not return the file!
  return(NULL)
}
