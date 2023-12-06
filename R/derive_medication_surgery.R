#' Derive, label, and add medication and surgery variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @param diabetes.file,cholesterol.file,afib.file,afib_surgery.file,AH_beta_blocker.file,AH_beta_blocker_if_not_drop.file,AH_ace_inhibitor.file,AH_arb.file,AH_ccb.file,AH_ksd.file,AH_other.file File paths to the respective data sets.
#' @return \code{data} with added medication and surgery variables.
#' @export

derive_medication_surgery <- function(data, diabetes.file, cholesterol.file,
                                      afib.file, afib_surgery.file,
                                      AH_beta_blocker.file,
                                      AH_beta_blocker_if_not_drop.file,
                                      AH_ace_inhibitor.file,
                                      AH_arb.file,
                                      AH_ccb.file,
                                      AH_ksd.file,
                                      AH_other.file) {

  diabetes.redcap.num <- diabetes.file[, 1]
  cholesterol.redcap.num <- cholesterol.file[, 1]
  afib.redcap.num <- afib.file[, 1]
  afib_surgery.redcap.num <- afib_surgery.file[, 1]

  # anti-hyp. subtype files
  AH_beta_blocker.redcap.num <- AH_beta_blocker.file[, 1]
  AH_beta_blocker_if_not_drop.redcap.num <- AH_beta_blocker_if_not_drop.file[, 1]
  AH_ace.redcap.num <- AH_ace_inhibitor.file[, 1]
  AH_arb.redcap.num <- AH_arb.file[, 1]
  AH_ccb.redcap.num <- AH_ccb.file[, 1]
  AH_ksd.redcap.num <- AH_ksd.file[, 1]
  AH_other.redcap.num <- AH_other.file[, 1]

  # dset consisting of all the medication columns
  medFrame <- data[, grepl("med[0-9][0-9]\\.name$", names(data))]

  # new code 20230612
  takingMeds <- apply(medFrame, 1, function(x) as.numeric(any(!is.na(x))))

  diabetesrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% diabetes.redcap.num))
  )
  cholesterolrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% cholesterol.redcap.num))
  )
  #htnrxPrep <- as.numeric(
  #    apply(medFrame, 1, function(x) any(x %in% antihypNums)))
  afibrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% afib.redcap.num))
  )

  # Antihypertensive med subtypes
  # note different name here--- we have further processing below
  ahBetaBlockerPrep0 <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_beta_blocker.redcap.num))
  )
  ahACEInhibPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_ace.redcap.num))
  )
  ahARBPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_arb.redcap.num))
  )
  ahCCBPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_ccb.redcap.num))
  )
  ahKSDPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_ksd.redcap.num))
  )
  ahOtherPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% AH_other.redcap.num))
  )


  ahBetaBlockerIfNotDropPrep <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    possibleEyedropCols <- which(medFrame[i, ] %in% AH_beta_blocker_if_not_drop.redcap.num)
    if (length(possibleEyedropCols) >= 1) {
      mednames <- names(medFrame)[possibleEyedropCols]
      unitnames <- gsub("name", "units", mednames, fixed = TRUE)
      vec <- data[i, unitnames]
      # Note that it's important to use != rather than !%in% here
      notDrops <- vec != 8
      if (any(notDrops)) {
        ahBetaBlockerIfNotDropPrep[i] <- 1
      } else if (anyNA(notDrops)) {
        ahBetaBlockerIfNotDropPrep[i] <- NA
      }
    }
  }

  # Here we want NA if 0 and NA, but 1 if 1 and NA
  ahBetaBlockerPrep <- ifelse(
    ahBetaBlockerPrep0 %in% 1,
    1,
    ifelse(
      ahBetaBlockerIfNotDropPrep %in% 1,
      1,
      ifelse(
        is.na(ahBetaBlockerIfNotDropPrep),
        NA,
        0
      )
    )
  )

  # dset consisting of all the surgery columns
  # This pulls the factors too (we do not create those for the meds,
  # so this step was not necessary above)
  surgVars <- names(data)[grep("surg[0-9][0-9]\\.name", names(data))]
  surgVarsNumericOnly <- surgVars[!grepl("factor", surgVars)]
  surgFrame <- data[, surgVarsNumericOnly]

  afibsurgPrep <- as.numeric(
    apply(surgFrame, 1, function(x) any(x %in% afib_surgery.redcap.num))
  )

  data <- within(data, {
    meds <- takingMeds
    diabetesrx <- ifelse(is.na(meds), NA, diabetesrxPrep)
    label(diabetesrx) <- 'Taking at least 1 diabetes med'

    diabetesrx.factor <- factor(
      diabetesrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(diabetesrx.factor) <- 'Taking at least 1 diabetes med'

    cholesterolrx <- ifelse(is.na(meds), NA, cholesterolrxPrep)
    label(cholesterolrx) <- 'Taking at least 1 cholesterol med'

    cholesterolrx.factor <- factor(
      cholesterolrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(cholesterolrx.factor) <- 'Taking at least 1 cholesterol med'

    afibrx <- ifelse(is.na(meds), NA, afibrxPrep)
    label(afibrx) <- 'Taking at least 1 afib med'

    afibrx.factor <- factor(
      afibrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(afibrx.factor) <- 'Taking at least 1 afib med'

    afibsurg <- ifelse(is.na(surg01), NA, afibsurgPrep)
    label(afibsurg) <- 'At least 1 afib surgery'

    afibsurg.factor <- factor(
      afibsurg,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(afibsurg.factor) <- 'At least 1 afib surgery'

    rx.htn.beta.blocker <- ifelse(is.na(meds), NA, ahBetaBlockerPrep)
    label(rx.htn.beta.blocker) <- 'Taking at least 1 beta-blocker'

    rx.htn.beta.blocker.factor <- factor(
      rx.htn.beta.blocker,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.beta.blocker.factor) <- 'Taking at least 1 beta-blocker'

    rx.htn.ace.inhibit <- ifelse(is.na(meds), NA, ahACEInhibPrep)
    label(rx.htn.ace.inhibit) <- 'Taking at least 1 ACE inhibitor'

    rx.htn.ace.inhibit.factor <- factor(
      rx.htn.ace.inhibit,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ace.inhibit.factor) <- 'Taking at least 1 ACE inhibitor'

    rx.htn.arb <- ifelse(is.na(meds), NA, ahARBPrep)
    label(rx.htn.arb) <- 'Taking at least 1 ARB'

    rx.htn.arb.factor <- factor(
      rx.htn.arb,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.arb.factor) <- 'Taking at least 1 ARB'

    rx.htn.ccb <- ifelse(is.na(meds), NA, ahCCBPrep)
    label(rx.htn.ccb) <- 'Taking at least 1 Ca channel blocker'

    rx.htn.ccb.factor <- factor(
      rx.htn.ccb,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ccb.factor) <- 'Taking at least 1 Ca channel blocker'

    rx.htn.ksd <- ifelse(is.na(meds), NA, ahKSDPrep)
    label(rx.htn.ksd) <- 'Taking at least 1 K-sparing diuretic'

    rx.htn.ksd.factor <- factor(
      rx.htn.ksd,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ksd.factor) <- 'Taking at least 1 K-sparing diuretic'

    rx.htn.other <- ifelse(is.na(meds), NA, ahOtherPrep)
    label(rx.htn.other) <- "Taking at least 1 antihyp. med of type 'Other'"

    rx.htn.other.factor <- factor(
      rx.htn.other,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.other.factor) <- "Taking at least 1 antihyp. med of type 'Other'"
  })

  ahSubtypeDat <- data[, Hmisc::Cs(rx.htn.beta.blocker, rx.htn.ace.inhibit, rx.htn.arb, rx.htn.ccb, rx.htn.ksd, rx.htn.other)]
  GEOneAHSubtype <- as.numeric(
    apply(ahSubtypeDat, 1, function(vec) any(vec %in% 1))
  )

  data <- within(data, {
    htnrx <- ifelse(is.na(meds), NA, GEOneAHSubtype)
    label(htnrx) <- 'Taking at least 1 anti-hypertensive med'

    htnrx.factor <- factor(
      htnrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(htnrx.factor) <- 'Taking at least 1 anti-hypertensive med'
  })

  return(data)
}
