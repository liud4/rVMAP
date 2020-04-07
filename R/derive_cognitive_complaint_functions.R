#' Functions to be used in the derivation of cognitive complaint variables: derive_cognitive_complaint()

totscore <- function(vec, threshold = 0.85) {
  # calc tot score only if >= <threshold> of items are nonmissing
  if(proportion_non_missing(vec) < threshold) return(NA) else {
    vec[is.na(vec)] <- mean(vec, na.rm= TRUE)
    return(round(sum(vec), 1))
  }
}

totscore.impute <- function(vec, threshold = 0.85) {
  # calc tot score only if >= <threshold> of items are nonmissing
  if(proportion_non_missing(vec) < threshold) return(vec) else {
    vec[is.na(vec)] <- mean(vec, na.rm= TRUE)
    return(vec)
  }
}

VTM <- function(vc, dm) {
  matrix(vc, ncol = length(vc), nrow = dm, byrow = T)
}

#acquire ranges for each SCD items
rescale.range<-function(v) {
  out<-matrix(0, ncol=length(v), nrow=2)
  out[, grepl('cogdif', v)]=c(0,4)
  out[, grepl('mfq', v)]=c(1,7)
  out[, grepl('ecogself', v)]=c(1,4)
  out[, grepl('ccqself', v)]=c(0,1)
  out
}

#scale each SCD item to 0-1 range
rescale<-function(v, data) {
  r<-rescale.range(v)
  n<-dim(data)[1]
  (data[, v]-VTM(r[1,], n))/VTM(r[2,]-r[1,], n)
}

#rescale the scaled 0-1 SCD item back to its original scale
rescale2 <- function(v, data) {
  r<-rescale.range(v)
  n<-dim(data)[1]
  data[, v]*VTM(r[2,]-r[1,], n)+VTM(r[1,], n)
}
