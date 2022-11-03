#' Derive, label, and add diagnosis and staging variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added diagnosis and staging variables.
#' @export

derive_diagnosis_staging <- function(data) {
  data <- within(data, {

    ### INNOTEST ###

    # From Albert et al. 2011 Table 3
    innotest.albert.mci.stage <- ifelse(
      is.na(diagnosis.factor) | diagnosis.factor != "MCI", NA,
      ifelse((is.na(csf.innotest.ab1.42.2014.positive) & is.na(csf.innotest.tau.2014.positive)) | (!is.na(csf.innotest.ab1.42.2014.positive) & !is.na(csf.innotest.tau.2014.positive) & csf.innotest.ab1.42.2014.positive + csf.innotest.tau.2014.positive == 1), 1,
             ifelse((csf.innotest.ab1.42.2014.positive == 1 & is.na(csf.innotest.tau.2014.positive)) | (is.na(csf.innotest.ab1.42.2014.positive) & csf.innotest.tau.2014.positive == 1), 2,
                    ifelse(csf.innotest.ab1.42.2014.positive == 1 & csf.innotest.tau.2014.positive == 1, 3,
                           ifelse(csf.innotest.ab1.42.2014.positive == 0 & csf.innotest.tau.2014.positive == 0, 0,
                                  NA)))))

    innotest.albert.mci.stage.factor <- factor(
      innotest.albert.mci.stage,
      levels = 0:3,
      labels = c("Unlikely due to AD", "Core clinical criteria", "Due to AD - Intermediate Likelihood", "Due to AD - High Likelihood"),
      ordered = TRUE
    )

    label(innotest.albert.mci.stage) <- label(innotest.albert.mci.stage.factor) <- "Albert et al. 2011 MCI categorization (INNOTEST, 2014)"

    ###

    # modified version requested in 13 Apr 2015 meeting:
    mod.innotest.albert.mci.stage <- ifelse(
      is.na(diagnosis.factor) | diagnosis.factor != "MCI", NA,
      ifelse(!is.na(csf.innotest.ab1.42.2014.positive) & !is.na(csf.innotest.tau.2014.positive) & csf.innotest.ab1.42.2014.positive + csf.innotest.tau.2014.positive == 1, 1,
             ifelse((csf.innotest.ab1.42.2014.positive == 1 & is.na(csf.innotest.tau.2014.positive)) | (is.na(csf.innotest.ab1.42.2014.positive) & csf.innotest.tau.2014.positive == 1), 2,
                    ifelse(csf.innotest.ab1.42.2014.positive == 1 & csf.innotest.tau.2014.positive == 1, 3,
                           ifelse(csf.innotest.ab1.42.2014.positive == 0 & csf.innotest.tau.2014.positive == 0, 0,
                                  NA)))))

    mod.innotest.albert.mci.stage.factor <- factor(
      mod.innotest.albert.mci.stage,
      levels = 0:3,
      labels = c("Unlikely due to AD", "Core clinical criteria", "Due to AD - Intermediate Likelihood", "Due to AD - High Likelihood"),
      ordered = TRUE
    )

    label(mod.innotest.albert.mci.stage) <- label(mod.innotest.albert.mci.stage.factor) <- "Modified Albert et al. 2011 MCI categorization (INNOTEST, 2014)"

    ### LUMIPULSE ###

    # From Albert et al. 2011 Table 3
    lumipulse.albert.mci.stage <- ifelse(
      is.na(diagnosis.factor) | diagnosis.factor != "MCI", NA,
      ifelse((is.na(csf.lumipulse.ab42.2022.positive) & is.na(csf.lumipulse.tau.2022.positive)) | (!is.na(csf.lumipulse.ab42.2022.positive) & !is.na(csf.lumipulse.tau.2022.positive) & csf.lumipulse.ab42.2022.positive + csf.lumipulse.tau.2022.positive == 1), 1,
             ifelse((csf.lumipulse.ab42.2022.positive == 1 & is.na(csf.lumipulse.tau.2022.positive)) | (is.na(csf.lumipulse.ab42.2022.positive) & csf.lumipulse.tau.2022.positive == 1), 2,
                    ifelse(csf.lumipulse.ab42.2022.positive == 1 & csf.lumipulse.tau.2022.positive == 1, 3,
                           ifelse(csf.lumipulse.ab42.2022.positive == 0 & csf.lumipulse.tau.2022.positive == 0, 0,
                                  NA)))))

    lumipulse.albert.mci.stage.factor <- factor(
      lumipulse.albert.mci.stage,
      levels = 0:3,
      labels = c("Unlikely due to AD", "Core clinical criteria", "Due to AD - Intermediate Likelihood", "Due to AD - High Likelihood"),
      ordered = TRUE
    )

    label(lumipulse.albert.mci.stage) <- label(lumipulse.albert.mci.stage.factor) <- "Albert et al. 2011 MCI categorization (LUMIPULSE, 2022)"

    ###

    # modified version requested in 13 Apr 2015 meeting:
    mod.lumipulse.albert.mci.stage <- ifelse(
      is.na(diagnosis.factor) | diagnosis.factor != "MCI", NA,
      ifelse(!is.na(csf.lumipulse.ab42.2022.positive) & !is.na(csf.lumipulse.tau.2022.positive) & csf.lumipulse.ab42.2022.positive + csf.lumipulse.tau.2022.positive == 1, 1,
             ifelse((csf.lumipulse.ab42.2022.positive == 1 & is.na(csf.lumipulse.tau.2022.positive)) | (is.na(csf.lumipulse.ab42.2022.positive) & csf.lumipulse.tau.2022.positive == 1), 2,
                    ifelse(csf.lumipulse.ab42.2022.positive == 1 & csf.lumipulse.tau.2022.positive == 1, 3,
                           ifelse(csf.lumipulse.ab42.2022.positive == 0 & csf.lumipulse.tau.2022.positive == 0, 0,
                                  NA)))))

    mod.lumipulse.albert.mci.stage.factor <- factor(
      mod.lumipulse.albert.mci.stage,
      levels = 0:3,
      labels = c("Unlikely due to AD", "Core clinical criteria", "Due to AD - Intermediate Likelihood", "Due to AD - High Likelihood"),
      ordered = TRUE
    )

    label(mod.lumipulse.albert.mci.stage) <- label(mod.lumipulse.albert.mci.stage.factor) <- "Modified Albert et al. 2011 MCI categorization (LUMIPULSE, 2022)"

    ###

    mci.stage.factor <- ordered(mci.stage.factor)

  })

  return(data)
}
