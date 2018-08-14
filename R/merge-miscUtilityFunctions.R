# Miscellaneous utility functions


# Not needed.
# PrintPreMergeMessage <- function(dset2name, epochNum){
#     cat("Now merging", dset2name, "in Epoch", epochNum, ".\n")
# }

# CheckNamesBeforeMerge <- function(dset1, dset2, mergingBy){
#     sharedNames <- intersect(names(dset1),
#         setdiff(names(dset2), mergingBy))
#     if(length(sharedNames)) {
#         warning(paste0("The datasets share the following variable names: ",
#             paste(sharedNames, collapse= " ")), immediate.= TRUE)
#     }
# }

check_shared_vars <- function(data1, data2, merge.by){
  sharedNames <- intersect(names(data1), setdiff(names(data2), merge.by))
  if (length(sharedNames)) {
    warning(
      paste0(
        "[warning] The datasets share the following variable names: ",
        paste(sharedNames, collapse = ", ")
      ),
      immediate. = TRUE
    )
    cat(
      paste0(
        "[warning] The datasets share the following variable names: ",
        paste(sharedNames, collapse = ", "),
        " \n"
      )
    )
  } else {
    cat(
      paste0("The merge occurred with no errors.", " \n")
    )
  }
}

