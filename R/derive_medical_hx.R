#' Derive, label, and add medical history variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added medical history variables.
#' @export

###########
# Tue Sep 20 16:38:45 2022 ------------------------------
# This function was modified in places commented below. Units were also added.
# The original function is retained at the end, commented out.

derive_medical_hx <- function(data) {
  # Returns data with medical-history derived variables added
  # (the ones that are not derived previously elsewhere,
  #  like lab variables & medication/surgery variables)
  # and also various other medical-type vars (cmr, echo, etc.)

  data <- within(data, {
    # Current smoking
    # updated coding: use medhx.date (email from AJ, 17 Sep 2014)
    medhx.date.year <- lubridate::year(medhx.date)

    # We want to count people as current smokers if they have
    # ever smoked AND if either
    # a) they haven't quit (mhx.tobac.quit = -8888) or
    # b) mhx.tobac.quit is the current year or the last year
    # Note that this code depends on our NOT recoding -8888 to NA earlier in the program!
    currentsmoking <-
      ifelse(is.na(mhx.tobac), NA,
             ifelse(mhx.tobac == 1 & is.na(mhx.tobac.quit.year), 1,
                    # Tue Sep 20 16:18:22 2022 ------------------------------
                    # The above NA should change to 1.
                    ifelse(mhx.tobac == 1 & (mhx.tobac.quit.year >= medhx.date.year - 1), 1,
                           # Tue Sep 20 16:19:46 2022 ------------------------------
                           # The above -8888 should be removed as this value is no longer on REDCap.
                           0)))
    currentsmoking.factor <- factor(currentsmoking, levels= c(1, 0), labels= c("Yes", "No"))
    label(currentsmoking) <- label(currentsmoking.factor) <- "Current smoker (or quit in this or last calendar yr)"

    # Wed Sep 21 15:52:16 2022 ------------------------------
    # mhx.tobac.quit.formersmokers <- ifelse(is.na(mhx.tobac), NA,
    #                                        ifelse(mhx.tobac == 0, NA,
    #                                               ifelse(mhx.tobac.quit.year == -8888, NA, mhx.tobac.quit.year)))
    mhx.tobac.quit.formersmokers <- ifelse(currentsmoking.factor == "No" & !is.na(mhx.tobac.quit.year), mhx.tobac.quit.year, NA)
    label(mhx.tobac.quit.formersmokers) <- "Year stopped smoking, if former smoker; NA otherwise"

    # 20 Oct 2016: (Based on meeting, 17 Oct 2016)
    #   in epoch 1, we will count map id 293 as having missing
    #   mhx.tobac.pks (-6666 w/ note in redcap)
    mhx.tobac.pks <- ifelse(epoch == 1 & map.id == "293", NA, mhx.tobac.pks)
    # message("Note: In this function, we have manually set mhx.tobac.pks to NA for MAP ID 293 in Epoch 1 (as per discussion on 20161017).\n")

    # Pack years
    dob.year <- lubridate::year(dob)
    smoking.years <- ifelse(is.na(mhx.tobac), NA,
                            ifelse(mhx.tobac == 0, 0,
                                   ifelse(currentsmoking.factor == "No",
                                          ((mhx.tobac.quit.year - dob.year) - mhx.tobac.age),
                                          ((medhx.date.year - dob.year) - mhx.tobac.age))))
    label(smoking.years) <- "Number of years smoked"

    pack.years <- ifelse(is.na(mhx.tobac), NA,
                         ifelse(mhx.tobac == 0, 0,
                                ifelse(smoking.years == 0, 0,
                                       smoking.years * mhx.tobac.pks)))
    label(pack.years) <- "Pack years (avg ppd X number of yrs smoked)"



    cvd <-
      ifelse(mhx.angina==1 | mhx.cad==1 | mhx.bypass ==1 | mhx.mi ==1 |
               mhx.hf ==1 | mhx.angio ==1, 1,
             ifelse(mhx.angina==0 & mhx.cad==0 & mhx.bypass ==0 & mhx.mi ==0 &
                      mhx.hf ==0 & mhx.angio ==0, 0, NA))
    cvd.factor <- factor(cvd, levels= c(1, 0), labels= c("Yes", "No"))
    label(cvd.factor) <- "CVD, determined from variables in med hx"
    label(cvd) <- "CVD, determined from variables in med hx"
  })

  data$echo.sbp2 <- as.numeric(data$echo.sbp2)

  data$sbp <- rowMeans(data[, Cs(echo.sbp, echo.sbp2)], na.rm= TRUE)
  data$dbp <- rowMeans(data[, Cs(echo.dbp, echo.dbp2)], na.rm= TRUE)

  # the above procedure introduces NaN's if both components are missing
  data <- within(data, {
    sbp <- ifelse(is.nan(sbp), NA, sbp)
    dbp <- ifelse(is.nan(dbp), NA, dbp)
    pp <- sbp-dbp

    label(sbp) <- "Mean of first two echo SBP readings"
    label(dbp) <- "Mean of first two echo DBP readings"
    label(pp) <- "Pulse pressure"
  })

  data <- within(data, {
    # changed 2 Sep 2014 (email from AJ)
    dyslipidemia <-
      ifelse(bld.c.chol >= 200 | bld.c.hdlc < 40 |
               cholesterolrx == 1, 1,
             ifelse(bld.c.chol < 200 & bld.c.hdlc >= 40 &
                      cholesterolrx == 0, 0, NA))

    dyslipidemia.factor <- factor(dyslipidemia, levels= c(1, 0), labels= c("Yes", "No"))
    label(dyslipidemia) <- label(dyslipidemia.factor) <- "Dyslipidemic, determined by chol, hdlc, and/or rx"

    hypertensive <- ifelse(is.na(sbp) & is.na(dbp), NA,
                           ifelse(sbp >= 140 | dbp >= 90, 1,
                                  ifelse(sbp <  140 & dbp <  90, 0, NA)))

    hypertensive.factor <- factor(hypertensive, levels= c(1, 0), labels= c("Yes", "No"))
    label(hypertensive.factor) <- label(hypertensive) <- "Hypertensive, determined solely by echo SBP and/or DBP"

    htn <- ifelse(is.na(hypertensive) & is.na(htnrx), NA,
                  ifelse(hypertensive == 1 | htnrx == 1, 1,
                         ifelse(hypertensive == 0 & htnrx == 0, 0, NA)))

    htn.factor <- factor(htn, levels= c(1, 0), labels= c("Yes", "No"))
    label(htn.factor) <- label(htn) <- "Hypertensive, determined by echo BP and/or med use"

    # this is a convenience var for use in later calculations
    afibecho <- ifelse(is.na(echo.rhythm), NA,
                       ifelse(echo.rhythm %in% c(2, 3), 1,
                              ifelse(echo.rhythm %in% c(1, 4, 5), 0, NA))) # redundant but safe

    afibecho.factor <- factor(afibecho, levels= c(1, 0), labels= c("Yes", "No"))
    label(afibecho.factor) <- label(afibecho) <- "Echo.rhythm = 2 or 3"

    # another convenience var for use in later calculations
    afibcmr <- ifelse(is.na(cmr.findings.rhythm), NA,
                      ifelse(stringr::str_trim(tolower(cmr.findings.rhythm)) %in%
                               cmr.findings.rhythm.afib, 1,
                             ifelse(stringr::str_trim(tolower(cmr.findings.rhythm)) %in%
                                      cmr.findings.rhythm.normal, 0, NA)))

    afibcmr.factor <- factor(afibcmr, levels= c(1, 0), labels= c("Yes", "No"))
    label(afibcmr.factor) <- label(afibcmr) <- "cmr.findings.rhythm indicates afib"

    afibconfirm <-
      # 19 Jan 2015:  Hard-coding values for 5 map id's as requested by
      # Angela in 16 Jan 2015 email.
      # I am using cmr.date here as a timestamp since we don't have an
      # "epoch" variable yet, so that we do this hard-coding for
      # the first wave (enrollment) only
      # 20220602 changed cmr.date to epoch == 1
      ifelse(map.id == "070" & epoch == 1, 1,
             ifelse(map.id == "089" & epoch == 1, 1,
                    ifelse(map.id == "176" & epoch == 1, 1,
                           ifelse(map.id == "194" & epoch == 1, 1,
                                  ifelse(map.id == "196" & epoch == 1, 1,
                                         # regular coding starts here
                                         ifelse(is.na(mhx.afib), NA,
                                                ifelse(mhx.afib == 0, NA,
                                                       ifelse(mhx.afib == 1 & # redundant but safe
                                                                (afibrx   == 1 |
                                                                   afibsurg  == 1 |
                                                                   afibecho  == 1 |
                                                                   afibcmr   == 1), 1,
                                                              ifelse(mhx.afib == 1 & # redundant but safe
                                                                       (afibrx   == 0 &
                                                                          afibsurg  == 0 &
                                                                          afibecho  == 0 &
                                                                          afibcmr   == 0), 0, NA)))))))))

    afibconfirm.factor <- factor(afibconfirm, levels= c(1, 0), labels= c("Yes", "No"))
    label(afibconfirm.factor) <- label(afibconfirm) <- "Afib self-report confirmed (NA if self-rep.= no)"

    afib <-ifelse(is.na(mhx.afib) & is.na(afibecho), NA,
             ifelse(is.na(afibecho), mhx.afib, ifelse(afibecho==1|mhx.afib==1, 1, mhx.afib)))
    # in next line, note that if afibconfirm == 1, it must be true
    # that mhx.afib == 1, so we don't need mhx.afib in there.
    # But it's clearer to me to do it this way.

    afib.factor <- factor(afib, levels= c(1, 0), labels= c("Yes", "No"))
    label(afib) <- label(afib.factor) <- "A-fib, determined by med hx and/or echo "
    # afib and cvd - used for exclusion criteria

    cvdafib.factor=factor(1*(cvd.factor=='Yes'|afib.factor=='Yes'), levels=c(0, 1), labels=c("No", "Yes"))
    label(cvdafib.factor)="Prevalent CVD or AFib"

    #afib <-
    #    ifelse(is.na(mhx.afib) & is.na(afibecho) #& is.na(afibcmr)
    #        , NA,
    #    ifelse(
    # in next line, note that if afibconfirm == 1, it must be true
    # that mhx.afib == 1, so we don't need mhx.afib in there.
    # But it's clearer to me to do it this way.
    #        (mhx.afib == 1 & afibconfirm == 1) |
    #       afibecho == 1 |
    #        afibcmr == 1, 1,
    #    ifelse(
    #        mhx.afib == 0 & # implied here: is.na(afibconfirm)
    #        afibecho == 0 &
    #        afibcmr == 0, 0, NA)))
    #afib.factor <- factor(afib, levels= c(1, 0), labels= c("Yes", "No"))
    #label(afib) <- label(afib.factor) <- "A-fib, determined by med hx and/or echo and/or cmr rhythm"

    # afib and cvd - used for exclusion criteria
    cvdafib.factor=factor(1*(cvd.factor=='Yes'|afib.factor=='Yes'), levels=c(0, 1), labels=c("No", "Yes"))
    label(cvdafib.factor)="Prevalent CVD or AFib"

    #diabetesa1c
    #1, Yes bld.c.hgba1c  >= 6.5
    #0, No bld.c.hgba1c < 6.5
    # 26 May 2015: AJ confirmed in email that these
    #  cutoff values are still the ones we want
    diabetesa1c <- ifelse(is.na(bld.c.hgba1c), NA,
                          ifelse(bld.c.hgba1c >= 6.5, 1, 0))

    diabetesa1c.factor <- factor(diabetesa1c, levels= c(1, 0), labels= c("Yes", "No"))
    label(diabetesa1c) <- label(diabetesa1c.factor) <- 'bld.c.hgba1c >= 6.5'

    # diabetesglucose
    # 1, Yes bld.c.glucose >=  126 mg/dL
    # 0, No bld.c.glucose < 126 mg/dL
    # 26 May 2015: AJ confirmed in email that these
    #  cutoff values are still the ones we want
    diabetesglucose <- ifelse(is.na(bld.c.glucose), NA,
                              ifelse(bld.c.glucose >= 126, 1, 0))

    diabetesglucose.factor <- factor(diabetesglucose, levels= c(1, 0), labels= c("Yes", "No"))
    label(diabetesglucose) <- label(diabetesglucose.factor) <- 'bld.c.glucose >= 126'

    # diabetes.  Changed 04 Sep 2014 per instructions from 03 Sep 2014 meeting.
    # 1, Yes: diabetesa1c=1 or diabetesrx=1 or diabetesglucose =1
    # 0, No: diabetesa1c=0 and diabetesrx=0 and diabetesglucose =0 OR
    #  is.na(diabetesa1c) and diabetesrx=0 and diabetesglucose =0 (this last part added 21 Apr 2016, per email from Angela)
    # Reordered April 27 2016, JN due to missing value in diabetes and fsrp
    diabetes <- ifelse(is.na(diabetesa1c) & is.na(diabetesrx) &
                         is.na(diabetesglucose), NA,
                       ifelse(is.na(diabetesa1c) & diabetesrx == 0 & diabetesglucose == 0, 0,
                              ifelse(diabetesa1c == 1 | diabetesrx == 1 | diabetesglucose == 1, 1,
                                     ifelse(diabetesa1c == 0 & diabetesrx == 0 & diabetesglucose == 0, 0, NA))))
    diabetes.factor <- factor(diabetes, levels= c(1, 0), labels= c("Yes", "No"))
    label(diabetes) <- label(diabetes.factor) <- "Diabetic, determined by a1c, glucose, and/or rx"

    # QUICKI: 1/(log(fasting insulin) + log(fasting glucose))
    # where log is log base 10--- confirmed by Liz, 13 Oct 2014
    quicki <- 1 / (log(bld.c.insulin, base= 10) + log(bld.c.glucose, base= 10))
    label(quicki) <- "Quantitative insulin sens. check index (QUICKI)"

    # 04 Feb 2015: they might not want to use this cutoff
    # and in the 23 Feb 2015 meeting, Angela said she does not want to
    # dichotomize insulin resistance ever,
    # so we might just get rid of this one
    insulinresistance <- ifelse(is.na(quicki), NA,
                                ifelse(quicki < 0.35, 1, 0))

    insulinresistance.factor <- factor(insulinresistance, levels= c(1, 0), labels= c("Yes", "No"))
    label(insulinresistance.factor) <- label(insulinresistance) <- "Insulin resistant as determined by QUICKI cutoff"

    # Cardiac output var. request from Angela, 28 Oct 2014
    echo.vd.mr.ge1 <- ifelse(is.na(echo.vd.mr), NA,
                             ifelse(echo.vd.mr >= 1, 1, 0))

    echo.vd.mr.ge1.factor <- factor(echo.vd.mr.ge1, levels= c(1, 0), labels= c("Yes", "No"))
    label(echo.vd.mr.ge1) <- label(echo.vd.mr.ge1.factor) <- 'Mitral valve regurgitation: echo.vd.mr >= 1'

    echo.vd.ar.ge1 <- ifelse(is.na(echo.vd.ar), NA,
                             ifelse(echo.vd.ar >= 1, 1, 0))

    echo.vd.ar.ge1.factor <- factor(echo.vd.ar.ge1, levels= c(1, 0), labels= c("Yes", "No"))
    label(echo.vd.ar.ge1) <- label(echo.vd.ar.ge1.factor) <- 'Aortic regurgitation: echo.vd.ar >= 1'

    cmr.findings.pulse <- as.numeric(cmr.findings.pulse)

    cmr.co.flow <- cmr.transaortic.sv * cmr.findings.pulse / 1000
    label(cmr.co.flow) <- 'Cardiac Output: Aortic Flow Analysis, L/min'

    cmr.co.volumetric <- cmr.lvsv * cmr.findings.pulse / 1000
    label(cmr.co.volumetric) <- 'Cardiac Output: Volumetric Analysis, L/min'


    echo.lvmass <- round(0.8 * 1.04 * ((echo.lvidd + echo.lvpwd + echo.ivsd)^3 - echo.lvidd^3) + 0.6, 2)
    label(echo.lvmass) <- "Echo - LV mass; recalculated"

    bsa <- round(weight^0.425 * height^0.725 * 0.007184, 2)
    label(bsa) <- 'BSA - physical exam; recalculated'

    cardiac.index <- cmr.co.flow / bsa
    label(cardiac.index) <- "Cardiac index (cmr.co.flow / bsa)"

    cardiac.index.low <- ifelse(is.na(cardiac.index), NA,
                                ifelse(cardiac.index < 2.5, 1, 0))

    cardiac.index.low.factor <- factor(cardiac.index.low, levels= c(1, 0), labels= c("Yes", "No"))
    label(cardiac.index.low) <- label(cardiac.index.low.factor) <- "Low cardiac index (< 2.5)"

    # email from Angela, 11 Nov 2014: "I just wrapped up a conference call with Rick and we agree that using the LV mass variable indexed with BSA is the best approach. You can finalize the FSRP calculation using this value. "
    echo.lvmass.scaled <- round(echo.lvmass / bsa, 2)
    label(echo.lvmass.scaled) <- "Echo - LV mass, divided by BSA (g/m^2)"

    # Formula: weight (kg) / [height (m)]^2
    bmi <- weight / (height / 100)^2
    label(bmi) <- "Physical Exam - BMI; recalculated"

    # LVH
    echo.rwt <- 2 * echo.lvpwd / echo.lvidd
    label(echo.rwt) <- 'Relative wall thickness'

    # from Lang et al. 2005, Table 4
    echo.lvmass.scaled.integer <- round(echo.lvmass.scaled, 0)
    # 17 Nov 2014: Rick says in email to count scaled lv mass lower than
    # ref. limit as normal.
    # 31 Dec 2015: now renaming this to use as our main definition of LVH
    #  to align w/ methods paper
    echo.lvh <-
      ifelse(is.na(sex.factor) | is.na(echo.lvmass.scaled.integer), NA,
             ifelse(sex.factor == 'Female' & echo.lvmass.scaled.integer <= 95, 0,
                    ifelse(sex.factor == 'Female' & echo.lvmass.scaled.integer > 95, 1,
                           ifelse(sex.factor == 'Male' & echo.lvmass.scaled.integer <= 115, 0,
                                  1))))

    echo.lvh.factor <- factor(echo.lvh, levels= c(1, 0), labels= c("Yes", "No"))
    label(echo.lvh.factor) <- label(echo.lvh) <- 'LV hypertrophy, determined by sex and scaled LV mass'

    # 31 Dec 2015: This variable used to be called echo.lvh
    echo.lvh.geometry <-
      ifelse(is.na(echo.rwt) | is.na(echo.lvh), NA,
             #ifelse(echo.rwt >= 0.42 & echo.lvh == 1, 1, 0))
             # 31 Dec 2015: Changing from AND to OR at Dandan's request. LS
             ifelse(echo.rwt >= 0.42 | echo.lvh == 1, 1, 0))

    echo.lvh.geometry.factor <- factor(echo.lvh, levels= c(1, 0), labels= c("Yes", "No"))
    label(echo.lvh.geometry) <- label(echo.lvh.geometry.factor) <- 'LV hypertrophy, determined by rel. wall thickness and/or sex and scaled LV mass'

    # LVH.echo, less detailed version for methods paper
    # 31 Dec 2015: note that this is now identical to echo.lvh above
    echo.lvh.scaledlvmass <-
      ifelse(is.na(sex.factor) | is.na(echo.lvmass.scaled.integer), NA,
             ifelse(sex.factor == 'Female' & echo.lvmass.scaled.integer > 95 ,1,
                    ifelse(sex.factor=='Male' & echo.lvmass.scaled.integer>115, 1,
                           0)))

    echo.lvh.scaledlvmass.factor <- factor(echo.lvh.scaledlvmass, levels=c(0,1), labels=c("No","Yes"))
    label(echo.lvh.scaledlvmass) <- label(echo.lvh.scaledlvmass.factor) <- 'LV hypertrophy, determined by sex and scaled LV mass'

    # creatinine clearance
    creatinine.clearance <-
      ifelse(sex.factor=="Female",
             # 31 Dec 2015: Jacquelyn- I was working in this file and noticed
             #   the syntax was a little weird on these two lines.
             #   I took the assignment statements out for clarity.
             #   (the code should run the same though) -LS
             #creatinine.clearance <- 0.85*((140 - age) / (bld.c.creatinine)) *(weight/72),
             #creatinine.clearance <- ((140 - age)/(bld.c.creatinine))*(weight/72))
             0.85*((140 - age) / (bld.c.creatinine)) *(weight/72),
             ((140 - age)/(bld.c.creatinine))*(weight/72))

    # 22 Jan 2016: Jacquelyn- this label wasn't coming through, so I
    #    fixed the syntax -LS
    #label("creatinine clearance, calculated (mL/min)")
    label(creatinine.clearance) <- "Creatinine clearance, calculated (mL/min)"

    # Some additional variables created for MAP Methods
    # lp.complete <- ifelse(is.na(csf.abx42),0,1) # 20220602 OAK
    # XXX lp.complete DOES NOT MATCH csf.complete XXX
    lp.complete <- ifelse(is.na(csf.msdabtriplex.abx42.2014),0,1)
    lp.complete.factor <- factor(lp.complete,levels=c(1,0),labels=c("Yes","No"))
    csf.complete.factor <- factor(csf.complete, levels = c(1, 0), labels = c("Yes", "No"))
    abp.complete <- ifelse(is.na(time.reading.indicator),0,1)
    abp.complete.factor <- factor(abp.complete,levels=c(1,0),labels=c("Yes","No"))

    label(abp.complete.factor) <- "Did participant complete abp?"

    map.id.afibduringcmr <- c("068", "096", "102", "157", "159", "161", "185", "186", "198")
    afib.during.cmr <- ifelse(map.id %in% map.id.afibduringcmr & epoch==1,1,0)

    afib.during.cmr.factor <- factor(afib.during.cmr,levels=c(1,0),labels=c("Yes","No"))
    label(afib.during.cmr) <- label(afib.during.cmr.factor) <- "Participant had atrial fibrillation at CMR"

    rm(map.id.afibduringcmr)


    anemic <- ifelse(is.na(bld.c.hgb), NA,
                     ifelse((sex.factor == 'Female' & bld.c.hgb < 12.0) |
                              (sex.factor == 'Male' & bld.c.hgb < 13.0), 1, 0))

    anemic.factor <- factor(anemic, levels= c(1, 0), labels= c('Yes', 'No'))
    label(anemic) <- label(anemic.factor) <- "bld.c.hgb < 12.0 (F) or < 13.0 (M)"
  })

  units(data$smoking.years) <- "year"
  units(data$pack.years) <- "year"
  units(data$sbp) <- "mmHg"
  units(data$dbp) <- "mmHg"
  units(data$pp) <- "mmHg"
  units(data$cmr.co.flow) <- "L/min"
  units(data$cmr.co.volumetric) <- "L/min"
  units(data$echo.lvmass) <- "g"
  units(data$bsa) <- "m^2"
  units(data$cardiac.index) <- "L/min/m^2"
  units(data$echo.lvmass.scaled) <- "g/m^2"
  units(data$bmi) <- "kg/m^2"
  units(data$creatinine.clearance) <- "mL/min"

  return(data)
}
