# Changes:

## 06 Mar 2015, LS:
#   added code to print warning about overlapping varnames

## 06 Apr 2015, LS:
#  Added code to deal with map.ids from main file when
#  double-entry is finished & the ids are numeric

## 14 Apr 2015 & 21 Apr 2015, LS:
#  Dealing with alt 3T imaging file for Epoch 1

## 11 May 2015, LS:
## In the main 3T dset, they changed ma.left.vaudate.vol to ma.left.caudate.vol. I added code here to deal with that in the Breath Hold data

# 13 May 2015, LS & JN:
# Adding processing for the QMASS file

# 13 July 2015, JN:
# Adding processing for the CSF file
# TODO: Fix MAP ID processing for future. 

# 20 July 2015, JN:
# Fixed MAP ID processing for CSF data
# Added processing for numeric columns with -9999 and -8888 as values in CSF data

# 27 July 2015, JN:
# Added print statements so we know the numbers of rows and columns after each file is merged into the dataset

# 26 August 2015, JN:
## Commented out code for sourcing labels for enrollment db as a test

# 11 November 2015 JN:
## Adding code to merge the manual 3T database

# 25 January 2016, JN
## Added code to merge the SRT error database

# 05 April 2016, JN
## Added preprocessing code for biomarker file

# 19-20 Oct 2016, LS:
## Starting to process Manual 3T Breath Hold data
## Changing some read.csv statements below to have stringsAsFactors= FALSE
## Adding processing of missing values

# 05 Dec 2016, LS
## Moving the labeling & factor creation for main dataset to another function
## Removing 'maincode' arg

# 12--15 Dec 2016, LS
##  Tightening up for Epoch 2
##  For main file: changing stringsAsFactors to FALSE
##  Changing processing of addendum vars

mergeWithinEpoch <- function(epochnum, mainfile, 
    functiondir, firstRds, abpfile= NULL, biomarkfile= NULL,
    auto3Tfile= NULL, auto3TBHfile= NULL,
    man3Tfile= NULL, man3TBHfile= NULL,
    qmassfile= NULL,
    addendumfile= NULL, csffile= NULL,
    srtfile= NULL,
    breathHoldPrefix= "bHold."){
    # Returns a dataset with the above files merged
    # into a single dataset using mainfile as the "backbone":
    # unless those values are set to NULL.
    # Also: add in the epoch number 

    # Make sure to have sourced MiscUtilityFunctions.R first


    cat("Processing and merging within Epoch ", epochnum, ".\n")

    #################################################################
    # Start with the main file for this epoch

    mydat <- read.csv(mainfile, stringsAsFactors= FALSE)

    source(file.path(functiondir, "preprocessingMain.R"))
    # Set -9999's to missing, etc.
    mydat <- preprocessingMain(mydat)

    # change underscores to periods
    names(mydat) <- gsub("\\_", "\\.", names(mydat))

    # keep only one record per map_id
    # 20 Feb 2015: email from AJ:
    # If there is a record w/ map_id xxx, keep that;
    # otherwise keep xxx--1
    # at some point, this may become moot
    mydat <- mydat[order(mydat$map.id), ]

    if(is.numeric(mydat$map.id)){
        mydat <- within(mydat, {
           map.id.orig <- as.character(map.id)
           map.id <- formatC(map.id, width= 3, format= "d", flag= "0")        
        })
        mydat1 <- mydat
    } else {
        mydat <- within(mydat, {
            map.id.orig <- map.id
            map.id <- gsub("\\-\\-[0-9]", "", map.id)
        })
        mydat1 <- mydat[!duplicated(mydat$map.id), ]
    }


    print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    #################################################################

    #################################################################
    # Merge with abp data if available. 
    # Keep ALL rows from abp file
    if(!is.null(abpfile)){
        PrintPreMergeMessage("ABP data", epochnum)

        abpdat <- readRDS(abpfile)
        names(abpdat) <- gsub("\\_", "\\.", names(abpdat))
        # this file also has numeric map_id... but no duplicates
        abpdat <- within(abpdat, {
            map.id <- 
                formatC(map.id, width= 3, format= "d", flag= "0")        
        })
        mydat1 <- merge(
            mydat1, 
            abpdat[, c("map.id", setdiff(names(abpdat), names(mydat1)))],
            by= "map.id", 
            all= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    #################################################################

    #################################################################
    # Merge in biomarker data if available
    if(!is.null(biomarkfile)){
        PrintPreMergeMessage("biomarker data", epochnum)

        biomarkdat <- read.csv(biomarkfile, stringsAsFactors=  FALSE)
        names(biomarkdat) <- gsub("\\_", "\\.", names(biomarkdat))

        # this file has numeric map_id, but no duplicates
        biomarkdat <- within(biomarkdat, {
            map.id <- formatC(map.id, width= 3, format= "d", flag= "0")
            
            
            # 19 May 2015: removing this line bec. they have removed the variable
            #  (without telling us)
            #rm(allelles) 
            # We have the same varname (w/ same misspelling)
             # in the APOE db.  
             # The one in the biomarker db had no data as of
             # 13 Feb 2015

            # The raw vars are labelled later, 
            # in the biomarkers() function
        })
        
        biomarkdat1 <- biomarkdat
        numericCols <- names(biomarkdat1)[sapply(biomarkdat1, is.numeric)]
        biomarkdat1[, numericCols] <- 
          apply(biomarkdat1[, numericCols], 2, minus9999)
        biomarkdat1[, numericCols] <- 
          apply(biomarkdat1[, numericCols], 2, minus8888)
        biomarkdat1[, numericCols] <- 
          apply(biomarkdat1[, numericCols], 2, minus7777)

        CheckNamesBeforeMerge(mydat1, biomarkdat1, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            biomarkdat1,
            by= "map.id", 
            all.x= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    #################################################################

    #################################################################
    # Merge with automated 3T imaging data if available. 
    if(!is.null(auto3Tfile)){
        PrintPreMergeMessage("automated 3T imaging data", epochnum)

        auto3Tdat <- read.csv(auto3Tfile, stringsAsFactors= FALSE)
        names(auto3Tdat) <- gsub("\\_", "\\.", names(auto3Tdat))
        # this file also has numeric map_id... but no duplicates
        auto3Tdat <- within(auto3Tdat, {
            map.id <- 
                formatC(map.id, width= 3, format= "d", flag= "0")        
            # The raw vars are labelled later, 
            # in the automated3T() function
        })
        auto3Tdat1 <- auto3Tdat
        numericCols <- names(auto3Tdat1)[sapply(auto3Tdat1, is.numeric)]
        auto3Tdat1[, numericCols] <- 
          apply(auto3Tdat1[, numericCols], 2, minus9999)
        auto3Tdat1[, numericCols] <- 
          apply(auto3Tdat1[, numericCols], 2, minus8888)

        CheckNamesBeforeMerge(mydat1, auto3Tdat1, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            auto3Tdat1,
            by= "map.id", 
            all= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    #################################################################

    #################################################################
    # Next: merge in the Auto 3T Breath Hold data if available
    # (applies to Epoch 1 only)
    if(!is.null(auto3TBHfile)){
        PrintPreMergeMessage("Breath Hold Auto3T imaging data", epochnum)

        auto3TBHdat <- read.csv(auto3TBHfile, stringsAsFactors= FALSE)
        names(auto3TBHdat) <- gsub("\\_", "\\.", names(auto3TBHdat))
        # this file also has numeric map_id... but no duplicates
        auto3TBHdat <- within(auto3TBHdat, {
            map.id <- 
                formatC(map.id, width= 3, format= "d", flag= "0")        
            # The raw vars are labelled later, 
            # in the automated3T() function
        })

        # 11 May 2015: see note at top of file
        # 20180205 OAK: Removed the code below as it is fixed.
        # if("ma.left.vaudate.vol" %in% names(auto3TBHdat)){
        #     names(auto3TBHdat)[names(auto3TBHdat) == "ma.left.vaudate.vol"] <-
        #     "ma.left.caudate.vol"
        # }

        numericCols <- names(auto3TBHdat)[sapply(auto3TBHdat, is.numeric)]
        auto3TBHdat[, numericCols] <- 
          apply(auto3TBHdat[, numericCols], 2, minus9999)
        auto3TBHdat[, numericCols] <- 
          apply(auto3TBHdat[, numericCols], 2, minus8888)

        #  add prefix for everything except map.id 
        names(auto3TBHdat)[!(names(auto3TBHdat) == "map.id")] <-
            paste0(breathHoldPrefix, 
            names(auto3TBHdat)[!(names(auto3TBHdat) == "map.id")])

        CheckNamesBeforeMerge(mydat1, auto3TBHdat, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            auto3TBHdat,
            by= "map.id", 
            all= TRUE) 
        print(paste("Data has ", nrow(mydat1)," rows and ", 
            ncol(mydat1),"columns"))
    }
    
    #################################################################
    
    # Merge with Manual 3T imaging data if available. 
    if(!is.null(man3Tfile)){
        PrintPreMergeMessage("manual 3T imaging data", epochnum)

        man3Tdat <- read.csv(man3Tfile, stringsAsFactors= FALSE)
        names(man3Tdat) <- gsub("\\_", "\\.", names(man3Tdat))
        
        # 20180126 OAK: added if clauses due to errors in Epoch 3 merge

        man3Tdat <- within(man3Tdat, {
            map.id <- formatC(map.id, width= 3, format= "d", flag= "0")
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
            # The raw vars are labelled later, 
            # in the manual3T() function
        })
        man3Tdat1 <- man3Tdat

        indicestoremoveMan3T <- which(names(man3Tdat1) %in% c("vmac.id",
            "entry.primary", "entry.secondary", "data.entry.complete",
            "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
            "session.id", "session.id.2", "session.id.3", "session.id.4"))
        man3Tdat1 <- man3Tdat1[, -indicestoremoveMan3T]

        # 19 Oct 2016: this var is in the BH dataset, but not Facemask:
        if (!("swi.microbleeds.distribution...2" %in% 
            names(man3Tdat1))){
            man3Tdat1$swi.microbleeds.distribution...2 <- NA
        }

        numericCols <- 
            names(man3Tdat1)[sapply(man3Tdat1, is.numeric)]
        man3Tdat1[, numericCols] <- 
            apply(man3Tdat1[, numericCols], 2, minus9999)
        man3Tdat1[, numericCols] <- 
            apply(man3Tdat1[, numericCols], 2, minus8888)

        CheckNamesBeforeMerge(mydat1, man3Tdat1, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            man3Tdat1,
            by= "map.id", 
            all= TRUE
        ) 
        print(paste("Data has ", nrow(mydat1)," rows and ", ncol(mydat1),"columns"))
    }
    
    #################################################################

    #################################################################
    # Merge in the Manual 3T Breath Hold data if available
    # (applies to Epoch 1 only)
    if(!is.null(man3TBHfile)){
        PrintPreMergeMessage("Breath Hold Manual 3T imaging data", 
            epochnum)

        man3TBHdat <- read.csv(man3TBHfile, stringsAsFactors= FALSE)
        names(man3TBHdat) <- gsub("\\_", "\\.", names(man3TBHdat))

        # If there is a record w/ map_id xxx, keep that;
        # otherwise keep xxx--1
        # at some point, this may become moot
        man3TBHdat <- man3TBHdat[order(man3TBHdat$map.id), ]

        if(is.numeric(man3TBHdat$map.id)){
            man3TBHdat <- within(man3TBHdat, {
               map.id <- formatC(map.id, width= 3, format= "d", 
                    flag= "0")        
            })
        } else {
            man3TBHdat <- within(man3TBHdat, {
                map.id <- gsub("\\-\\-[0-9]", "", map.id)
            })
            man3TBHdat <- 
                man3TBHdat[!duplicated(man3TBHdat$map.id), ]
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
        indicestoremoveMan3TBH <- which(names(man3TBHdat) %in% c(
            "vmac.id", "entry.primary", "entry.secondary", 
            "data.entry.complete",
            "scan.date", "scan.date.2", "scan.date.3", "scan.date.4",
            "session.id", "session.id.2", "session.id.3", "session.id.4"))
        man3TBHdat <- man3TBHdat[, -indicestoremoveMan3TBH]

        numericCols <- 
            names(man3TBHdat)[sapply(man3TBHdat, is.numeric)]
        man3TBHdat[, numericCols] <- 
            apply(man3TBHdat[, numericCols], 2, minus9999)
        man3TBHdat[, numericCols] <- 
            apply(man3TBHdat[, numericCols], 2, minus8888)

        #  add prefix for everything except map.id 
        names(man3TBHdat)[!(names(man3TBHdat) == "map.id")] <-
            paste0(breathHoldPrefix, 
            names(man3TBHdat)[!(names(man3TBHdat) == "map.id")])

        CheckNamesBeforeMerge(mydat1, man3TBHdat, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            man3TBHdat,
            by= "map.id", 
            all= TRUE) 
        print(paste("Data has ", nrow(mydat1)," rows and ", 
            ncol(mydat1),"columns"))
    }
    
    #################################################################
    
    #################################################################
    # Merge in QMASS data if available
    if(!is.null(qmassfile)){
        PrintPreMergeMessage("qmass data", epochnum)

        qmassdat <- read.csv(qmassfile, stringsAsFactors=  FALSE)
        names(qmassdat) <- gsub("\\_", "\\.", names(qmassdat))

        # this file has numeric map_id, but no duplicates
        qmassdat <- within(qmassdat, {
            map.id <- 
                formatC(map.id, width= 3, format= "d", flag= "0")
            rm(vmac.id)
            # The raw vars are labelled later, 
            # in the qmass() function
        })
        
        qmassdat1 <- qmassdat
        numericCols <- names(qmassdat1)[sapply(qmassdat1, is.numeric)]
        qmassdat1[, numericCols] <- 
          apply(qmassdat1[, numericCols], 2, minus9999)
        qmassdat1[, numericCols] <- 
          apply(qmassdat1[, numericCols], 2, minus8888)
        qmassdat1[, numericCols] <- 
          apply(qmassdat1[, numericCols], 2, minus7777)

        CheckNamesBeforeMerge(mydat1, qmassdat1, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            qmassdat1,
            by= "map.id", 
            all.x= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    ###############################################################
    
    #################################################################
    # Merge in addendum data if available
    #  after first appending .addend to varnames    
    if(!is.null(addendumfile)){
        PrintPreMergeMessage("addendum data", epochnum)

        addendumdat <- 
            read.csv(addendumfile, 
                stringsAsFactors=  FALSE)
        names(addendumdat) <- gsub("\\_", "\\.", names(addendumdat))
        # There are a ton of problems with their map.id
        addendumdat <- within(addendumdat, {
            map.id <- as.numeric(map.id)
        })
        addendumdat <- addendumdat[!is.na(addendumdat$map.id) & 
            addendumdat$map.id > 0, ]
        addendumdat <- within(addendumdat, {
            map.id <- formatC(map.id, width= 3, format= "d", flag= "0")
        })

        # keep only one record per vmac_id
        # keeping --1 at request of AJ as we do w/ eligibility (email 13 Feb 2015)
        # at some point, this may become moot
        addendumdat <- addendumdat[order(addendumdat$vmac.id), ]
        addendumdat <- within(addendumdat, {
            vmac.id.short <- gsub("\\-\\-[0-9]", "", vmac.id)
        })
        # correspondence btw vmac id & map id should be 1-1
        idcount <- with(addendumdat, tapply(map.id, vmac.id.short,   
            function(vec) length(unique(vec)) > 1))
        cat("~~~~~~~~~~~~~~~~~~~\nPossible problems w/ addendum db:\n")
        cat("The following vmac id's are associated with more than one map id:\n")
        print(addendumdat[addendumdat$vmac.id.short %in% names(idcount)[idcount], Cs(vmac.id, map.id)], row.names= FALSE)

        addendumdat <- addendumdat[order(addendumdat$map.id), ]
        idcount2 <- with(addendumdat, tapply(vmac.id.short, map.id, 
            function(vec) length(unique(vec)) > 1))
        cat("The following map id's are associated with more than one vmac id:\n")
        print(addendumdat[addendumdat$map.id %in% names(idcount2)[idcount2], Cs(map.id, vmac.id)], row.names= FALSE)
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
            c("vmac.id","entry.primary","entry.secondary",
            "data.entry.complete","enrollment", "diagnosis", 
            "diagnosis.date", "nc.type", "mci.amnestic",
            "mci.domain", "mci.stage", "mci.notes",
            "diagnosis.complete", "np.examiner"))
        addendumdat1 <- addendumdat1[,-indicestoremoveAddendum]
        names(addendumdat1)[names(addendumdat1) == "np.date"] <-
            "np.date.addend"
        names(addendumdat1)[names(addendumdat1) == "np.notes"] <-
            "np.notes.addend"
        names(addendumdat1)[names(addendumdat1) == "neuropsychological.assessment.complete"] <-
            "neuropsychological.assessment.complete.addend"

        CheckNamesBeforeMerge(mydat1, addendumdat1, c('map.id'))
        mydat1 <- merge(
            mydat1, 
            addendumdat1,
            by= "map.id", 
            all.x= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
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
      
      csfdat <- 
        read.csv(csffile, 
                 # 22 May 2015: I think we can add this 
                 # to get rid of warning msg in future 
                 # if VMAC doesn't fix the problem
                 #
                 #skipNul= TRUE, 
                 stringsAsFactors=  FALSE)
      names(csfdat) <- gsub("\\_", "\\.", names(csfdat))
      # keep only one record per map_id

      # If there is a record w/ map_id xxx, keep that;
      # otherwise keep xxx--1
      # at some point, this may become moot
      csfdat <- csfdat[order(csfdat$map.id), ]
      
      if(is.numeric(csfdat$map.id)){
        csfdat <- within(csfdat, {
          map.id.orig <- as.character(map.id)
          map.id <- formatC(map.id, width= 3, format= "d", flag= "0")        
        })
        csfdat1 <- csfdat
      } else {
        csfdat <- within(csfdat, {
          map.id.orig <- map.id
          map.id <- gsub("\\-\\-[0-9]", "", map.id)
        })
        csfdat1 <- csfdat[!duplicated(csfdat$map.id), ]
      }
      
      indicestoremove <- which(names(csfdat1) %in% c("vmac.id","entry.primary","entry.secondary","data.entry.complete","map.id.orig"))
      csfdat1 <- csfdat1[,-indicestoremove]
      
      numericCols <- names(csfdat1)[sapply(csfdat1, is.numeric)]
      csfdat1[, numericCols] <- 
        apply(csfdat1[, numericCols], 2, minus9999)
      csfdat1[, numericCols] <- 
        apply(csfdat1[, numericCols], 2, minus8888)
      
      # The raw vars are labelled later, 
      # in the csflabel() function
      
      CheckNamesBeforeMerge(mydat1, csfdat1, c('map.id'))
      mydat1 <- merge(
        mydat1, 
        csfdat1,
        by= "map.id", 
        all.x= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    #################################################################
    
    #################################################################
    # Merge in SRT error data if available
    if(!is.null(srtfile)){
      PrintPreMergeMessage("SRT error data", epochnum)
      
      srtdat <- read.csv(srtfile, stringsAsFactors=  FALSE)
      names(srtdat) <- gsub("\\_", "\\.", names(srtdat))
      
      # this file has numeric map_id, but no duplicates
      srtdat <- within(srtdat, {
        map.id <- formatC(map.id, width= 3, format= "d", flag= "0")
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
      
      CheckNamesBeforeMerge(mydat1, srtdat, c('map.id'))
      mydat1 <- merge(
        mydat1, 
        srtdat1,
        by= "map.id", 
        all.x= TRUE) 
        print(paste("Data has ",nrow(mydat1)," rows and ",ncol(mydat1),"columns"))
    }
    
    #################################################################
    
    #################################################################
    # Add in the epoch number
    mydat1 <- within(mydat1, {
        epoch <- epochnum
        label(epoch) <- "Epoch"
        label(map.id.orig) <- "Original map.id from REDCap, for checking"
    })

    # save this as a first step
    saveRDS(mydat1, file= firstRds)
    
    # Do not return the file!
    return(NULL)
}
