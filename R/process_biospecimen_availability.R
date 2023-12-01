#' Derive, label, and add demographic variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added demographic variables.
#' @export


process_biospecimen_availability <- function(data) {
  data <- within(data, {
    plasma.availability <-
      serum.availability <-
      dna.availability <-
      paxgene.availability <- ifelse(is.na(blood.complete),
                                     NA,
                                     ifelse(blood.complete == 1, "Yes", "No"))
    
    label(plasma.availability) <- "Plasma Availability"
    
    label(serum.availability) <- "Serum Availability"
    
    label(dna.availability) <- "DNA Availability"
    
    label(paxgene.availability) <- "PAXGene Availability"
    
    csf.availability <- ifelse(is.na(csf.fluid),
                               NA,
                               ifelse(csf.fluid == 1, "Yes", "No"))
    
    label(csf.availability) <- "CSF availability"
    
  })
  
  return(data)
}
