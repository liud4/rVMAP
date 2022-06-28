#' Derive, label, and add biomarker variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added biomarker variables.
#' @export

derive_biomarkers <- function(data) {
  vegf_label <- attr(data$biomarkers.milliplex.vegf.plasma.2015, "label")
  vegf_units <- attr(data$biomarkers.milliplex.vegf.plasma.2015, "units")
  
  il6_label <- attr(data$biomarkers.milliplex.il6.plasma.2015, "label")
  il6_units <- attr(data$biomarkers.milliplex.il6.plasma.2015, "units")
  
  data <- within(data, {
    amyloidPos <- ifelse(
      is.na(csf.innotest.ab1.42.2014),
      NA,
      ifelse(
        csf.innotest.ab1.42.2014 < 530,
        1,
        0
      )
    )
    amyloidPos.factor <- factor(
      amyloidPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(amyloidPos) <- label(amyloidPos.factor) <- "Amyloid positive (csf.innotest.ab1.42.2014 < 530 ng/L)"
    
    tauPos <- ifelse(
      is.na(csf.innotest.tau.2014),
      NA,
      ifelse(
        csf.innotest.tau.2014 > 400,
        1,
        0
      )
    )
    tauPos.factor <- factor(
      tauPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(tauPos) <- label(tauPos.factor) <- "Tau positive (csf.innotest.tau.2014 > 400 ng/L)"
    
    ptauPos <- ifelse(
      is.na(csf.innotest.ptau.2014),
      NA,
      ifelse(
        csf.innotest.ptau.2014 > 80,
        1,
        0
      )
    )
    ptauPos.factor <- factor(
      ptauPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(ptauPos) <- label(ptauPos.factor) <- "P-Tau positive (csf.innotest.ptau.2014 > 80 ng/L)"
    
    # 20220603 OAK: removed because process_comparison_markers() was updated to remove "_" and "?"
    
    # biomarkers.milliplex.vegf.plasma.2015 <- ifelse(
    #   biomarkers.milliplex.vegf.plasma.2015 == "<11.32_", 11.32,
    #   ifelse(biomarkers.milliplex.vegf.plasma.2015 == "<10.61_", 10.61,
    #          ifelse(biomarkers.milliplex.vegf.plasma.2015 == "<9.56_", 9.56,
    #                 ifelse(biomarkers.milliplex.vegf.plasma.2015 == "<13.51_", 13.51,
    #                        ifelse(biomarkers.milliplex.vegf.plasma.2015 == "<13.51?", 13.51,
    #                               biomarkers.milliplex.vegf.plasma.2015)))))
    # 
    # biomarkers.milliplex.il6.plasma.2015 <- ifelse(
    #   biomarkers.milliplex.il6.plasma.2015 == "<0.33_", 0.33,
    #   ifelse(biomarkers.milliplex.il6.plasma.2015 == "<0.66_", 0.66,
    #          biomarkers.milliplex.il6.plasma.2015))
    # biomarkers.milliplex.vegf.plasma.2015 <- as.numeric(biomarkers.milliplex.vegf.plasma.2015)
    # biomarkers.milliplex.il6.plasma.2015 <- as.numeric(biomarkers.milliplex.il6.plasma.2015)
    
    # A Beta 42/40 ratio
    csf.abeta42.40.ratio <- csf.msdabtriplex.abx42.2014 / csf.msdabtriplex.abx40.2014
    label(csf.abeta42.40.ratio) <- 'Ratio of AbX-42:AbX-40'
    
    # csf.snap groups
    csf.snap <- ifelse(
      is.na(amyloidPos.factor) | is.na(tauPos.factor), NA,
      ifelse(amyloidPos.factor == "No" &  tauPos.factor== "No", 1,
             ifelse(amyloidPos.factor == "Yes" & tauPos.factor== "No", 2,
                    ifelse(amyloidPos.factor == "No" &  tauPos.factor== "Yes", 3,
                           ifelse(amyloidPos.factor == "Yes" & tauPos.factor== "Yes", 4,
                                  NA)))))
    
    csf.snap.factor <- factor(
      csf.snap,
      levels = 1:4,
      labels = c("AB-/tau-",
                 "AB+/tau-",
                 "AB-/tau+",
                 "AB+/tau+"),
      ordered = TRUE
    )
    label(csf.snap) <- label(csf.snap.factor) <- "CSF-defined SNAP based on AB and tau cutpoints"
  })
  
  attr(data$biomarkers.milliplex.vegf.plasma.2015, "label") <- vegf_label
  attr(data$biomarkers.milliplex.vegf.plasma.2015, "units") <- vegf_units
  
  attr(data$biomarkers.milliplex.il6.plasma.2015, "label") <- il6_label
  attr(data$biomarkers.milliplex.il6.plasma.2015, "units") <- il6_units
  
  return(data)
}
