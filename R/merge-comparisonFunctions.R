# Several functions used in comparing datasets

compareVarnames <- function(new_data, olddat, maxprint= 100) {
  newnames <- names(new_data)
  oldnames <- names(olddat)

  cat("\nVariables in new dataset but not in old:\n")
  addedvars <- setdiff(newnames, oldnames)
  numAddedvars <- length(addedvars)
  if (numAddedvars == 0) {
    cat("None.\n")
  } else if (numAddedvars <= maxprint) {
    print(data.frame(addedvars))
  } else {
    cat("Added ", numAddedvars, " new variables.\n", sep= "")
  }

  cat("\nVariables in old dataset but not in new:\n")
  removedvars <- setdiff(oldnames, newnames)
  numRemovedvars <- length(removedvars)
  if (numRemovedvars == 0) {
    cat("None.\n")
  } else if (numRemovedvars <= 100) {
    print(data.frame(removedvars))
  } else {
    cat(numRemovedvars, " variables from old file are not in new file.\n", sep= "")
  }
}

compareIDs <- function(new_data, olddat) {
  newids <- unique(new_data$map.id)
  oldids <- unique(olddat$map.id)

  cat("\nIDs in new dataset but not in old:\n")
  addedIDs <- setdiff(newids, oldids)
  numAddedIDs <- length(addedIDs)
  if (numAddedIDs == 0) {
    cat("None.\n")
  } else {
    print(data.frame(addedIDs))
  }

  cat("\nIDs in old dataset but not in new:\n")
  removedIDs <- setdiff(oldids, newids)
  numRemovedIDs <- length(removedIDs)
  if (numRemovedIDs == 0) {
    cat("None.\n")
  } else {
    print(data.frame(removedIDs))
  }
}
compareFactors <- function(new_data, olddat) {
  shared.names <- intersect(names(new_data), names(olddat))
  cat("\nThere are differences in the factor levels for the following variables:\n")
  for (vname in shared.names) {
    newcol <- new_data[[vname]]
    oldcol <- olddat[[vname]]
    if (!identical(levels(newcol), levels(oldcol))) {
      cat("\n-----------------------------\n-->", vname, "\n")
      cat("New:", levels(newcol), "\n")
      cat("Old:", levels(oldcol), "\n")
    }
  }
}

# this next one is not currently in use (13 Dec 2016)
compareModes <- function(new_data, olddat) {
  shared.names <- intersect(names(new_data), names(olddat))
  cat("\nThere are differences in the following variables:\n")
  for (vname in shared.names) {
    newcol <- new_data[[vname]]
    oldcol <- olddat[[vname]]
    if (!identical(mode(newcol), mode(oldcol))) {
      cat("\n-----------------------------\n-->", vname, "\n")
      cat("\nThe R mode of the variable is not the same in the two datasets.\n")
      cat("New:", mode(newcol), "\n")
      cat("Old:", mode(oldcol), "\n")
    }
  }
}


compareToLast <- function(newfile, oldfile, useSinkfile, shared.names= NULL,
                          namesToSkip= NULL) {
  # for the args use e.g. mergedfile and lastMergedfile
  if (newfile == oldfile) stop("The two files have exactly the same name.")

  new_data <- readRDS(paste0(newfile, ".rds"))
  olddat <- readRDS(paste0(oldfile, ".rds"))
  if (useSinkfile) {
    cat("Output will be saved in separate file.\n")

    # The file we'll send the output to
    sinkfile <- paste0(newfile, "_compare.txt")
    sink(sinkfile)
  }

  cat("\nCurrent dimensions:", dim(new_data), "\n")
  cat("\nOld dimensions:", dim(olddat), "\n")

  compareVarnames(new_data, olddat)
  compareIDs(new_data, olddat)

  cat("\nNow checking matching rows and columns for changes.\n")
  new_data <- within(new_data, {
    tmp.id <- paste0(epoch, map.id)
  })
  olddat <- within(olddat, {
    tmp.id <- paste0(epoch, map.id)
  })
  new_data.sorted <- new_data[order(new_data$tmp.id), ]
  olddat.sorted <- olddat[order(olddat$tmp.id), ]

  if (is.null(shared.names)) shared.names <- intersect(names(new_data), names(olddat))
  #shared.names <- c('fsrp','fsrp.minus.age.points','cvd','cvd.factor','enrolled.dx.factor',cognames[1:100])
  #shared.names <- shared.names[!(shared.names %in% c(famhxnames,mednames,surgnames,notenames))]
  shared.names <- setdiff(shared.names, namesToSkip)

  shared.ids   <- intersect(new_data.sorted$tmp.id, olddat.sorted$tmp.id)

  new_data.sorted.sub <- new_data.sorted[new_data.sorted$tmp.id %in% shared.ids,
                                     shared.names]
  olddat.sorted.sub <- olddat.sorted[olddat.sorted$tmp.id %in% shared.ids,
                                     shared.names]
  cat("\nThere are changes in the following columns:\n")
  for (vname in shared.names) {
    newcol <- new_data.sorted.sub[[vname]]
    oldcol <- olddat.sorted.sub[[vname]]
    if (!identical(levels(newcol), levels(oldcol))) {
      cat("\n-----------------------------\n-->", vname, "\n")
      cat("\nFactor levels are not the same in the two datasets.\n")
      cat("New:", levels(newcol), "\n")
      cat("Old:", levels(oldcol), "\n")
    } else if (!identical(newcol, oldcol)) {
      cat("\n-----------------------------\n-->", vname, "\n")
      numdiscrep <- 0
      for(id in shared.ids){
        newval <- new_data.sorted[new_data.sorted$tmp.id == id, vname]
        oldval <- olddat.sorted[olddat.sorted$tmp.id == id, vname]
        numNAs <- sum(is.na(c(newval, oldval)))
        if (numNAs == 1 | (numNAs == 0 & (newval != oldval))) {
          numdiscrep <- numdiscrep + 1
          dat <-rbind(
            new_data.sorted[new_data.sorted$tmp.id == id, c('epoch', 'map.id', vname)],
            olddat.sorted[olddat.sorted$tmp.id == id, c('epoch', 'map.id', vname)]
          )
          rownames(dat) <- c("new", "old")
          print(dat)
        }
      }
      if (numdiscrep == 0) cat ("\nMight need to look at this one by hand (labels or other attributes might be different).\n")
    }
  }
  if (useSinkfile) {
    sink()
  }
}

