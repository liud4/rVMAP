#' Derive, label, and add biomarker variables to the merged data set.
#'
#' Clinical cut-points for AD for LUMIPULSE assay CSF biomarkers are reported in the following paper:
#'
#' Gobom, J., Parnetti, L., Rosa-Neto, P., Vyhnalek, M., Gauthier, S., Cataldi, S., Lerch, O., Laczo, J., Cechova, K.,
#' Clarin, M., Benet, A. I., Pascoal, T. A., Rahmouni, N., Vandijck, M., Huyck, E., Le Bastard, N., Stevenson, J.,
#' Chamoun, M., Alcolea, D., ... Blennow, K. (2021).
#' Validation of the LUMIPULSE automated immunoassay for the measurement of core ad biomarkers in cerebrospinal fluid.
#' Clinical Chemistry and Laboratory Medicine (CCLM).
#' https://doi.org/10.1515/cclm-2021-0651
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

    csf.innotest.ab1.42.2014.positive <- ifelse(
      is.na(csf.innotest.ab1.42.2014),
      NA,
      ifelse(
        csf.innotest.ab1.42.2014 < 530,
        1,
        0
      )
    )
    csf.innotest.ab1.42.2014.positive.factor <- factor(
      csf.innotest.ab1.42.2014.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.innotest.ab1.42.2014.positive) <- label(csf.innotest.ab1.42.2014.positive.factor) <- "Amyloid Positive (csf.innotest.ab1.42.2014 < 530 pg/mL)"

    ##

    csf.innotest.tau.2014.positive <- ifelse(
      is.na(csf.innotest.tau.2014),
      NA,
      ifelse(
        csf.innotest.tau.2014 > 400,
        1,
        0
      )
    )
    csf.innotest.tau.2014.positive.factor <- factor(
      csf.innotest.tau.2014.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.innotest.tau.2014.positive) <- label(csf.innotest.tau.2014.positive.factor) <- "Tau Positive (csf.innotest.tau.2014 > 400 pg/mL)"

    ###

    csf.innotest.ptau.2014.positive <- ifelse(
      is.na(csf.innotest.ptau.2014),
      NA,
      ifelse(
        csf.innotest.ptau.2014 > 80,
        1,
        0
      )
    )
    csf.innotest.ptau.2014.positive.factor <- factor(
      csf.innotest.ptau.2014.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.innotest.ptau.2014.positive) <- label(csf.innotest.ptau.2014.positive.factor) <- "pTau Positive (csf.innotest.ptau.2014 > 80 pg/mL)"

    ###

    csf.msdabtriplex.abx42.40.2014.ratio <- csf.msdabtriplex.abx42.2014 / csf.msdabtriplex.abx40.2014
    # Thu Sep 22 16:22:25 2022 ------------------------------
    # label name was derived from information in this paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7867115/
    label(csf.msdabtriplex.abx42.40.2014.ratio) <- 'Ratio of AbX-42/AbX-40 (Meso Scale Discovery AB Triplex, 2014)'

    ###

    csf.innotest.snap.2014 <- ifelse(
      is.na(csf.innotest.ab1.42.2014.positive.factor) | is.na(csf.innotest.tau.2014.positive.factor), NA,
      ifelse(csf.innotest.ab1.42.2014.positive.factor == "No" &  csf.innotest.tau.2014.positive.factor == "No", 1,
             ifelse(csf.innotest.ab1.42.2014.positive.factor == "Yes" & csf.innotest.tau.2014.positive.factor == "No", 2,
                    ifelse(csf.innotest.ab1.42.2014.positive.factor == "No" &  csf.innotest.tau.2014.positive.factor == "Yes", 3,
                           ifelse(csf.innotest.ab1.42.2014.positive.factor == "Yes" & csf.innotest.tau.2014.positive.factor == "Yes", 4,
                                  NA)))))

    csf.innotest.snap.2014.factor <- factor(
      csf.innotest.snap.2014,
      levels = 1:4,
      labels = c("AB-/tau-",
                 "AB+/tau-",
                 "AB-/tau+",
                 "AB+/tau+"),
      ordered = TRUE
    )
    label(csf.innotest.snap.2014) <- label(csf.innotest.snap.2014.factor) <- "CSF-defined SNAP based on AB and tau cutpoints (INNOTEST, 2014)"

    ##############################

    csf.lumipulse.ab42.2022.positive <- ifelse(
      is.na(csf.fujirebio.ab42.2022),
      NA,
      ifelse(
        csf.fujirebio.ab42.2022 <= 526,
        1,
        0
      )
    )
    csf.lumipulse.ab42.2022.positive.factor <- factor(
      csf.lumipulse.ab42.2022.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.lumipulse.ab42.2022.positive) <- label(csf.lumipulse.ab42.2022.positive.factor) <- "Amyloid Positive (csf.fujirebio.ab42.2022 <= 526 ng/L)"

    ##

    csf.lumipulse.tau.2022.positive <- ifelse(
      is.na(csf.fujirebio.tau.2022),
      NA,
      ifelse(
        csf.fujirebio.tau.2022 >= 409,
        1,
        0
      )
    )
    csf.lumipulse.tau.2022.positive.factor <- factor(
      csf.lumipulse.tau.2022.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.lumipulse.tau.2022.positive) <- label(csf.lumipulse.tau.2022.positive.factor) <- "Tau Positive (csf.fujirebio.tau.2022 >= 409 ng/L)"

    ###

    csf.lumipulse.ptau.2022.positive <- ifelse(
      is.na(csf.fujirebio.ptau.2022),
      NA,
      ifelse(
        csf.fujirebio.ptau.2022 >= 50.2,
        1,
        0
      )
    )
    csf.lumipulse.ptau.2022.positive.factor <- factor(
      csf.lumipulse.ptau.2022.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.lumipulse.ptau.2022.positive) <- label(csf.lumipulse.ptau.2022.positive.factor) <- "pTau positive (csf.fujirebio.ptau.2022 >= 50.2 ng/L)"

    ###

    csf.lumipulse.ab42.40.2022.ratio <- csf.fujirebio.ab42.2022 / csf.fujirebio.ab40.2022
    label(csf.lumipulse.ab42.40.2022.ratio) <- 'Ratio of AB42/AB40 (LUMIPULSE, 2022)'

    csf.lumipulse.ab42.40.2022.ratio.positive <- ifelse(
      is.na(csf.lumipulse.ab42.40.2022.ratio),
      NA,
      ifelse(
        csf.lumipulse.ab42.40.2022.ratio <= 0.072,
        1,
        0
      )
    )
    csf.lumipulse.ab42.40.2022.ratio.positive.factor <- factor(
      csf.lumipulse.ab42.40.2022.ratio.positive,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(csf.lumipulse.ab42.40.2022.ratio.positive) <- label(csf.lumipulse.ab42.40.2022.ratio.positive.factor) <- "AB42/AB40 Ratio Positive (csf.lumipulse.ab42.40.2022.ratio <= 0.072)"

    ###

    csf.lumipulse.snap.2022 <- ifelse(
      is.na(csf.lumipulse.ab42.2022.positive.factor) | is.na(csf.lumipulse.tau.2022.positive.factor), NA,
      ifelse(csf.lumipulse.ab42.2022.positive.factor == "No" &  csf.lumipulse.tau.2022.positive.factor == "No", 1,
             ifelse(csf.lumipulse.ab42.2022.positive.factor == "Yes" & csf.lumipulse.tau.2022.positive.factor == "No", 2,
                    ifelse(csf.lumipulse.ab42.2022.positive.factor == "No" &  csf.lumipulse.tau.2022.positive.factor == "Yes", 3,
                           ifelse(csf.lumipulse.ab42.2022.positive.factor == "Yes" & csf.lumipulse.tau.2022.positive.factor == "Yes", 4,
                                  NA)))))

    csf.lumipulse.snap.2022.factor <- factor(
      csf.lumipulse.snap.2022,
      levels = 1:4,
      labels = c("AB-/tau-",
                 "AB+/tau-",
                 "AB-/tau+",
                 "AB+/tau+"),
      ordered = TRUE
    )
    label(csf.lumipulse.snap.2022) <- label(csf.lumipulse.snap.2022.factor) <- "CSF-defined SNAP based on AB and tau cutpoints (LUMIPULSE, 2022)"
  })

  attr(data$biomarkers.milliplex.vegf.plasma.2015, "label") <- vegf_label
  attr(data$biomarkers.milliplex.vegf.plasma.2015, "units") <- vegf_units

  attr(data$biomarkers.milliplex.il6.plasma.2015, "label") <- il6_label
  attr(data$biomarkers.milliplex.il6.plasma.2015, "units") <- il6_units

  attr(data$csf.fujirebio.ab40.2022, "units") <- "ng/L"
  attr(data$csf.fujirebio.ab42.2022, "units") <- "ng/L"
  attr(data$csf.fujirebio.tau.2022, "units") <- "ng/L"
  attr(data$csf.fujirebio.ptau.2022, "units") <- "ng/L"

  return(data)
}
