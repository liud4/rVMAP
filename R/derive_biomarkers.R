#' Derive, label, and add biomarker variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added biomarker variables.
#' @export

derive_biomarkers <- function(data) {

  vegf_label <- label(data$biomarkers.vegf)
  vegf_units <- units(data$biomarkers.vegf)
  il6_label <- label(data$biomarkers.il6)
  il6_units <- units(data$biomarkers.il6)

  data <- within(data, {
    # label(biomarkers.leptin)         <- "Leptin pg/ml"
    # label(biomarkers.il6)            <- "IL-6 pg/ml"
    # label(biomarkers.tnfalpha)       <- "TNFalpha pg/ml"
    # # As of 16 Feb 2015, we are not using <allelles> from this file.
    # #label(allelles)                  <- "Genotype- Alleles"
    # label(biomarkers.vegf)           <- "VEGF pg/ml"
    # label(biomarkers.notes)          <- "Biomarkers notes"
    # label(blood.biomarkers.complete) <- "Complete?"
    # label(csf.labid)                 <- "Lab ID"
    # label(csf.abx42)                 <- "Ab X-42 (ng/L)"
    # label(csf.abx40)                 <- "Ab X-40 (ng/L)"
    # label(csf.abx42.abx40.ratio)     <- "Ab X- 42/Ab X-40"
    # label(csf.ab1.42)                <- "Ab 1-42 (ng/L)"
    # label(csf.tau)                   <- "Tau (ng/L)"
    # label(csf.ptau)                  <- "P-Tau (ng/L)"
    # label(csf.notes)                 <- "CSF notes"
    # label(csf.biomarkers.complete)   <- "Complete?"
    # label(csf.nfl)                   <- "Sweden Neurofilament Light (pg/mL)"

    # Variables requested by AJ in email, 13 Feb 2015:
    amyloidPos <- ifelse(
      is.na(csf.ab1.42),
      NA,
      ifelse(
        csf.ab1.42 < 530,
        1,
        0
      )
    )
    amyloidPos.factor <- factor(
      amyloidPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(amyloidPos) <- label(amyloidPos.factor) <- "Amyloid positive (csf.ab1.42 < 530 ng/L)"

    tauPos <- ifelse(
      is.na(csf.tau),
      NA,
      ifelse(
        csf.tau > 400,
        1,
        0
      )
    )
    tauPos.factor <- factor(
      tauPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(tauPos) <- label(tauPos.factor) <- "Tau positive (csf.tau > 400 ng/L)"

    ptauPos <- ifelse(
      is.na(csf.ptau),
      NA,
      ifelse(
        csf.ptau > 80,
        1,
        0
      )
    )
    ptauPos.factor <- factor(
      ptauPos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(ptauPos) <- label(ptauPos.factor) <- "P-Tau positive (csf.ptau > 80 ng/L)"

    biomarkers.vegf <- ifelse(
      biomarkers.vegf == "<11.32_", 11.32,
      ifelse(biomarkers.vegf == "<10.61_", 10.61,
             ifelse(biomarkers.vegf == "<9.56_", 9.56,
                    ifelse(biomarkers.vegf == "<13.51_", 13.51,
                           ifelse(biomarkers.vegf == "<13.51?", 13.51,
                                  biomarkers.vegf)))))

    biomarkers.il6 <- ifelse(
      biomarkers.il6 == "<0.33_", 0.33,
      ifelse(biomarkers.il6 == "<0.66_", 0.66,
             biomarkers.il6))
    biomarkers.vegf <- as.numeric(biomarkers.vegf)
    biomarkers.il6 <- as.numeric(biomarkers.il6)

    # A Beta 42/40 ratio
    csf.abeta42.40.ratio <- csf.abx42 / csf.abx40
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

  label(data$biomarkers.vegf) <- vegf_label
  units(data$biomarkers.vegf) <- vegf_units
  label(data$biomarkers.il6) <- il6_label
  units(data$biomarkers.il6) <- il6_units

  return(data)
}
