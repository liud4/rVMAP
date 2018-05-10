# Miscellaneous utility functions
PrintPreMergeMessage <- function(dset2name, epochNum){
    cat("Now merging", dset2name, "in Epoch", epochNum, ".\n")
}

CheckNamesBeforeMerge <- function(dset1, dset2, mergingBy){
    sharedNames <- intersect(names(dset1), 
        setdiff(names(dset2), mergingBy))
    if(length(sharedNames)) {
        warning(paste0("The datasets share the following variable names: ", 
            paste(sharedNames, collapse= " ")), immediate.= TRUE)
    }
}
