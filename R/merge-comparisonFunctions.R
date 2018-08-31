# Several functions used in comparing datasets

compare_variables <- function(new_data, old_data) {
  new_vars <- names(new_data)
  old_vars <- names(old_data)

  added_vars <- setdiff(new_vars, old_vars)
  num_added_vars <- length(added_vars)

  if (num_added_vars == 0) {
    cat("The new data set does not contain any variables that are not in the old data set.\n")
  } else {
    cat("Variables in new data set but not in old:\n")
    added_vars <- data.frame(added_vars)
    names(added_vars) <- "Added Variables"
    print(added_vars)
  }

  cat("\nVariables in old dataset but not in new:\n")
  removed_vars <- setdiff(old_vars, new_vars)
  num_removed_vars <- length(removed_vars)

  if (num_removed_vars == 0) {
    cat("The old data set does not contain any variables that are not in the new data set.\n")
  } else {
    cat("Variables in old data set but not in new:\n")
    removed_vars <- data.frame(removed_vars)
    names(removed_vars) <- "Removed Variables"
    print(removed_vars)
  }
}

compare_ids <- function(new_data, old_data) {
  new_ids <- unique(new_data$map.id)
  old_ids <- unique(old_data$map.id)

  added_ids <- setdiff(new_ids, old_ids)
  num_added_ids <- length(added_ids)

  if (num_added_ids == 0) {
    cat("The new data set does not contain any MAP IDs that are not in the old data set.\n")
  } else {
    cat("MAP IDs in new data set but not in old:\n")
    added_ids <- data.frame(added_ids)
    names(added_ids) <- "Added MAP IDs"
    print(added_ids)
  }

  removed_ids <- setdiff(old_ids, new_ids)
  num_removed_ids <- length(removed_ids)

  if (num_removed_ids == 0) {
    cat("The old data set does not contain any MAP IDs that are not in the new data set.\n")
  } else {
    cat("MAP IDs in old data set but not in new:\n")
    removed_ids <- data.frame(removed_ids)
    names(removed_ids) <- "Removed MAP IDs"
    print(removed_ids)
  }
}

compare_factors <- function(new_data, old_data) {
  shared.names <- intersect(names(new_data), names(old_data))
  different_levels.df <- rbind(c(
    `Variable` = "delete",
    `Levels in New Data Set` = "delete",
    `Levels in Old Data Set` = "delete"
  ))
  for (vname in shared.names) {
    new_var <- new_data[[vname]]
    old_var <- old_data[[vname]]
    if (!identical(levels(new_var), levels(old_var))) {
      new_row <- c(
        Variable = vname,
        `Levels in New Data Set` = levels(new_var),
        `Levels in Old Data Set` = levels(old_var)
      )
      different_levels.df <- rbind.data.frame(different_levels.df, new_row, make.row.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (!is.null(nrow(different_levels.df))) {
    cat("There are differences in levels in the following categorical variables:\n")
    print(different_levels.df[-1, ])
  } else {
    cat("Categorical variables in the new and old data sets share the same levels.\n")
  }
}


# compareToLast <- function(newfile, oldfile, useSinkfile, shared.names= NULL,
#                           namesToSkip= NULL) {
#   # for the args use e.g. mergedfile and lastMergedfile
#   if (newfile == oldfile) stop("The two files have exactly the same name.")
#
#   new_data <- readRDS(paste0(newfile, ".rds"))
#   old_data <- readRDS(paste0(oldfile, ".rds"))
#   if (useSinkfile) {
#     cat("Output will be saved in separate file.\n")
#
#     # The file we'll send the output to
#     sinkfile <- paste0(newfile, "_compare.txt")
#     sink(sinkfile)
#   }
#
#   cat("\nCurrent dimensions:", dim(new_data), "\n")
#   cat("\nOld dimensions:", dim(old_data), "\n")
#
#   compareVarnames(new_data, old_data)
#   compareIDs(new_data, old_data)
#
#   cat("\nNow checking matching rows and columns for changes.\n")
#   new_data <- within(new_data, {
#     tmp.id <- paste0(epoch, map.id)
#   })
#   old_data <- within(old_data, {
#     tmp.id <- paste0(epoch, map.id)
#   })
#   new_data.sorted <- new_data[order(new_data$tmp.id), ]
#   old_data.sorted <- old_data[order(old_data$tmp.id), ]
#
#   if (is.null(shared.names)) shared.names <- intersect(names(new_data), names(old_data))
#   #shared.names <- c('fsrp','fsrp.minus.age.points','cvd','cvd.factor','enrolled.dx.factor',cognames[1:100])
#   #shared.names <- shared.names[!(shared.names %in% c(famhxnames,mednames,surgnames,notenames))]
#   shared.names <- setdiff(shared.names, namesToSkip)
#
#   shared.ids   <- intersect(new_data.sorted$tmp.id, old_data.sorted$tmp.id)
#
#   new_data.sorted.sub <- new_data.sorted[new_data.sorted$tmp.id %in% shared.ids,
#                                      shared.names]
#   old_data.sorted.sub <- old_data.sorted[old_data.sorted$tmp.id %in% shared.ids,
#                                      shared.names]
#   cat("\nThere are changes in the following columns:\n")
#   for (vname in shared.names) {
#     newcol <- new_data.sorted.sub[[vname]]
#     oldcol <- old_data.sorted.sub[[vname]]
#     if (!identical(levels(newcol), levels(oldcol))) {
#       cat("\n-----------------------------\n-->", vname, "\n")
#       cat("\nFactor levels are not the same in the two datasets.\n")
#       cat("New:", levels(newcol), "\n")
#       cat("Old:", levels(oldcol), "\n")
#     } else if (!identical(newcol, oldcol)) {
#       cat("\n-----------------------------\n-->", vname, "\n")
#       numdiscrep <- 0
#       for(id in shared.ids){
#         newval <- new_data.sorted[new_data.sorted$tmp.id == id, vname]
#         oldval <- old_data.sorted[old_data.sorted$tmp.id == id, vname]
#         numNAs <- sum(is.na(c(newval, oldval)))
#         if (numNAs == 1 | (numNAs == 0 & (newval != oldval))) {
#           numdiscrep <- numdiscrep + 1
#           dat <-rbind(
#             new_data.sorted[new_data.sorted$tmp.id == id, c('epoch', 'map.id', vname)],
#             old_data.sorted[old_data.sorted$tmp.id == id, c('epoch', 'map.id', vname)]
#           )
#           rownames(dat) <- c("new", "old")
#           print(dat)
#         }
#       }
#       if (numdiscrep == 0) cat ("\nMight need to look at this one by hand (labels or other attributes might be different).\n")
#     }
#   }
#   if (useSinkfile) {
#     sink()
#   }
# }

