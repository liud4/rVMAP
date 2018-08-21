medicationAndSurgery <- function(
  dat, diabFile, cholFile,
  #antihypFile,
  afibFile, afibsurgFile,
  antihypBetaBlockerFile, antihypBetaBlockerIfNotDropFile,
  antihypACEInhibFile,
  antihypARBFile, antihypCCBFile, antihypKSDFile,
  antihypOtherFile) {
  # Returns dat with medication and surgery derived variables added

  # Read the csv files to get the vectors of med/surg numbers
  diabNums <- diabFile[, 1]
  cholNums <- cholFile[, 1]
  afibNums <- afibFile[, 1]
  afibsurgNums <- afibsurgFile[, 1]

  # anti-hyp. subtype files
  ahBetaBlockerNums <- antihypBetaBlockerFile[, 1]
  ahBetaBlockerIfNotDropNums <- antihypBetaBlockerIfNotDropFile[, 1]
  ahACEInhibNums <- antihypACEInhibFile[, 1]
  ahARBNums <- antihypARBFile[, 1]
  ahCCBNums <- antihypCCBFile[, 1]
  ahKSDNums <- antihypKSDFile[, 1]
  ahOtherNums <- antihypOtherFile[, 1]

  # 07 Oct 2016: now a med is antihyp only if it's in one of the subtypes
  #antihypNums <- antihypFile[, 1]

  # dset consisting of all the medication columns
  medFrame <- dat[, grepl("med[0-9][0-9]\\.name$", names(dat))]

  diabetesrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% diabNums))
  )
  cholesterolrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% cholNums))
  )
  #htnrxPrep <- as.numeric(
  #    apply(medFrame, 1, function(x) any(x %in% antihypNums)))
  afibrxPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% afibNums))
  )

  # Antihypertensive med subtypes
  # note different name here--- we have further processing below
  ahBetaBlockerPrep0 <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahBetaBlockerNums))
  )
  ahACEInhibPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahACEInhibNums))
  )
  ahARBPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahARBNums))
  )
  ahCCBPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahCCBNums))
  )
  ahKSDPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahKSDNums))
  )
  ahOtherPrep <- as.numeric(
    apply(medFrame, 1, function(x) any(x %in% ahOtherNums))
  )


  ahBetaBlockerIfNotDropPrep <- rep(0, nrow(dat))
  for (i in 1:nrow(dat)) {
    possibleEyedropCols <- which(medFrame[i, ] %in% ahBetaBlockerIfNotDropNums)
    if (length(possibleEyedropCols) >= 1) {
      mednames <- names(medFrame)[possibleEyedropCols]
      unitnames <- gsub("name", "units.factor", mednames, fixed = TRUE)
      vec <- dat[i, unitnames]
      # Note that it's important to use != rather than !%in% here
      notDrops <- vec != "drop(s)"
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
  surgVars <- names(dat)[grep("surg[0-9][0-9]\\.name", names(dat))]
  surgVarsNumericOnly <- surgVars[!grepl("factor", surgVars)]
  surgFrame <- dat[, surgVarsNumericOnly]

  afibsurgPrep <- as.numeric(
    apply(surgFrame, 1, function(x) any(x %in% afibsurgNums))
  )

  dat <- within(dat, {
    diabetesrx <- ifelse(is.na(meds), NA, diabetesrxPrep)
    diabetesrx.factor <- factor(
      diabetesrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(diabetesrx) <- label(diabetesrx.factor) <- 'Taking at least 1 diabetes med'

    cholesterolrx <- ifelse(is.na(meds), NA, cholesterolrxPrep)
    cholesterolrx.factor <- factor(
      cholesterolrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(cholesterolrx) <- label(cholesterolrx.factor) <-'Taking at least 1 cholesterol med'

    afibrx <- ifelse(is.na(meds), NA, afibrxPrep)
    afibrx.factor <- factor(
      afibrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(afibrx) <- label(afibrx.factor) <- 'Taking at least 1 afib med'

    afibsurg <- ifelse(is.na(surg01), NA, afibsurgPrep)
    afibsurg.factor <- factor(
      afibsurg,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(afibsurg) <- label(afibsurg.factor) <- 'At least 1 afib surgery'

    rx.htn.beta.blocker <- ifelse(is.na(meds), NA, ahBetaBlockerPrep)
    rx.htn.beta.blocker.factor <- factor(
      rx.htn.beta.blocker,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.beta.blocker) <- label(rx.htn.beta.blocker.factor) <- 'Taking at least 1 beta-blocker'

    rx.htn.ace.inhibit <- ifelse(is.na(meds), NA, ahACEInhibPrep)
    rx.htn.ace.inhibit.factor <- factor(
      rx.htn.ace.inhibit,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ace.inhibit) <- label(rx.htn.ace.inhibit.factor) <- 'Taking at least 1 ACE inhibitor'

    rx.htn.arb <- ifelse(is.na(meds), NA, ahARBPrep)
    rx.htn.arb.factor <- factor(
      rx.htn.arb,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.arb) <- label(rx.htn.arb.factor) <- 'Taking at least 1 ARB'

    rx.htn.ccb <- ifelse(is.na(meds), NA, ahCCBPrep)
    rx.htn.ccb.factor <- factor(
      rx.htn.ccb,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ccb) <- label(rx.htn.ccb.factor) <- 'Taking at least 1 Ca channel blocker'

    rx.htn.ksd <- ifelse(is.na(meds), NA, ahKSDPrep)
    rx.htn.ksd.factor <- factor(
      rx.htn.ksd,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.ksd) <- label(rx.htn.ksd.factor) <- 'Taking at least 1 K-sparing diuretic'

    rx.htn.other <- ifelse(is.na(meds), NA, ahOtherPrep)
    rx.htn.other.factor <- factor(
      rx.htn.other,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(rx.htn.other) <- label(rx.htn.other.factor) <- "Taking at least 1 antihyp. med of type 'Other'"
  })

  ahSubtypeDat <- dat[, Hmisc::Cs(rx.htn.beta.blocker, rx.htn.ace.inhibit, rx.htn.arb, rx.htn.ccb, rx.htn.ksd, rx.htn.other)]
  GEOneAHSubtype <- as.numeric(
    apply(ahSubtypeDat, 1, function(vec) any(vec %in% 1))
  )

  dat <- within(dat, {
    #htnrx <- ifelse(is.na(meds), NA, htnrxPrep)
    #htnrx.factor <- factor(htnrx,
    #    levels= c(1, 0), labels= c("Yes", "No"))
    #label(htnrx) <- label(htnrx.factor) <-
    #    'Taking at least 1 anti-hypertensive med'

    htnrx <- ifelse(is.na(meds), NA, GEOneAHSubtype)
    htnrx.factor <- factor(
      htnrx,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(htnrx) <- label(htnrx.factor) <- 'Taking at least 1 anti-hypertensive med'

    # These checks are useful if we have separate lists for htnrx and the subtypes
    #rx.htn.NOS <-
    #    ifelse(is.na(htnrx), NA,
    #    ifelse(htnrx == 1, 1 - GEOneSubtype, 0))
    #rx.htn.NOS.factor <- factor(rx.htn.NOS,
    #    levels= c(1, 0), labels= c("Yes", "No"))
    #label(rx.htn.NOS) <- label(rx.htn.NOS.factor) <-
    #    "For checking purposes: Taking at least 1 antihyp. med, type NOS"

    #rx.htn.Problem <-
    #    ifelse(htnrx %in% 1, 0, GEOneSubtype)
    #rx.htn.Problem.factor <- factor(rx.htn.Problem,
    #    levels= c(1, 0), labels= c("Yes", "No"))
    #label(rx.htn.Problem) <- label(rx.htn.Problem.factor) <-
    #    "For checking purposes: Taking at least 1 antihyp. med subtype, but htnrx != 1"
  })

  dat
}
