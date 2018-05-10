# A collection of functions for calculating total scores, etc.


propNonMissing <- function(vec) round(mean(!is.na(vec)), 2)

totscore <- function(vec, threshold= 0.85){
    # calc tot score only if >= <threshold> of items are nonmissing
    if(propNonMissing(vec) < threshold) return(NA) else {
        vec[is.na(vec)] <- mean(vec, na.rm= TRUE)
        return(round(sum(vec), 1))
    }
}

avgscore <- function(vec, threshold= 0.85){
    # calc avg score only if >= <threshold> of items are nonmissing
    if(propNonMissing(vec) < threshold) return(NA) else {
        return(round(mean(vec, na.rm= TRUE), 1))
    }
}

reverse01   <- function(vec) ifelse(is.na(vec), NA, 1 - vec)
reverse1to3 <- function(vec) ifelse(is.na(vec), NA, 4 - vec)
reverse1to7 <- function(vec) ifelse(is.na(vec), NA, 8 - vec)

