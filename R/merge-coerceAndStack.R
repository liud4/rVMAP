coerceAndStack <- function(newdat, olddat) {
    shared.names <- intersect(names(newdat), names(olddat))
    cat("\nThere are differences in the R modes of the following variables:\n")
    for (vname in shared.names) {
        newcol <- newdat[[vname]] 
        oldcol <- olddat[[vname]] 
        modes <- c(mode(newcol), mode(oldcol))
        # If all the values are NA, R thinks the mode is 'logical'.
        #   In that case we don't care whether the modes are different.
        if ((!identical(modes[1], modes[2])) &
                !(all(is.na(newcol)) | all(is.na(oldcol)))) {
            cat("\n-----------------------------\n-->", vname, "\n")
            cat("New:", mode(newcol), "\n")
            cat("Old:", mode(oldcol), "\n")
            if (any(grepl("character", modes, fixed= TRUE)) &
                any(grepl("numeric", modes, fixed= TRUE))) {
            cat("\nCoercing the numeric variable to character before stacking.\n")
                if (modes[1] == "numeric") {
                    newdat[[vname]] <- as.character(newdat[[vname]])
                } else {
                    olddat[[vname]] <- as.character(olddat[[vname]])
                }
            } 
        } 
    }
    # We use bind_rows() rather than rbind() bec. different epochs will
    #   have different columns
    bind_rows(olddat, newdat)
}
