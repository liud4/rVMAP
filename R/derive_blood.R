#' Derive, convert, label, and add blood variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added blood variables.
#' @export

derive_blood <- function(data) {
  idsForConversion <- formatC(1:91, width = 3, format = "d", flag = "0")

  data <- within(data, {
    m <- (99 - 70)/(110 - 70)
    b <- 99 - m*110
    bld.c.glucose.redcap <- bld.c.glucose
    bld.c.glucose <- ifelse(map.id %in% idsForConversion & epoch == 1, m*bld.c.glucose.redcap + b, bld.c.glucose.redcap)

    m <- (23 - 0)/(16 - 0)
    bld.c.insulin.redcap <- bld.c.insulin
    bld.c.insulin <-
      ifelse(is.na(bld.c.insulin) | bld.c.insulin %in% c("-9999", ""), NA,
             ifelse(bld.c.insulin %in% c("<2.0"), 1.9,
                    as.numeric(as.character(bld.c.insulin))))
    bld.c.insulin <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.insulin, bld.c.insulin)

    m <- (144-136)/(145-135)
    b <- 144 - m*145
    bld.c.sodium.redcap <- bld.c.sodium
    bld.c.sodium <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.sodium.redcap + b, bld.c.sodium.redcap)

    m <- (107-98)/(105-95)
    b <- 107 - m*105
    bld.c.chloride.redcap <- bld.c.chloride
    bld.c.chloride <- ifelse(map.id %in% idsForConversion & epoch==1,m*bld.c.chloride.redcap + b, bld.c.chloride.redcap)

    m <- (31-23)/(30-23)
    b <- 31 - m*30
    bld.c.co2.redcap <- bld.c.co2
    bld.c.co2 <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.co2.redcap + b, bld.c.co2.redcap)

    m <- (26-8)/(25-5)
    b <- 26 - m*25
    bld.c.urea.nitrogen.redcap <- bld.c.urea.nitrogen
    bld.c.urea.nitrogen <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.urea.nitrogen.redcap + b, bld.c.urea.nitrogen.redcap)

    m <- (10.5 - 8.4)/(10.5 - 8.5)
    b <- 10.5 - m*10.5
    bld.c.calcium.redcap <- bld.c.calcium
    bld.c.calcium <- ifelse(map.id %in% idsForConversion & epoch==1,m*bld.c.calcium.redcap + b, bld.c.calcium.redcap)

    m <- (8.3 - 6)/(8.4 - 6.1)
    b <- 8.3 - m*8.4
    bld.c.protein.t.redcap <- bld.c.protein.t
    bld.c.protein.t <- ifelse(map.id %in% idsForConversion & epoch==1,m*bld.c.protein.t.redcap + b, bld.c.protein.t.redcap)

    m <- (1.2 - 0.2)/(1.2 - 0.2)
    b <- 1.2 - m*1.2
    bld.c.bilirubin.t.redcap <- bld.c.bilirubin.t
    bld.c.bilirubin.t <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.bilirubin.t.redcap + b, bld.c.bilirubin.t.redcap)

    m <- (40 - 5)/(40 - 4)
    b <- 40 - m*40
    bld.c.ast.redcap <- bld.c.ast
    bld.c.ast <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.ast.redcap + b, bld.c.ast.redcap)

    m <- (55 - 0)/(40 - 4)
    b <- 55 - m*40
    bld.c.alt.redcap <- bld.c.alt
    # adding 13 Dec 2016. Coding requested by AJ in email.
    bld.c.alt <-
      ifelse(is.na(bld.c.alt) | bld.c.alt %in% c("-9999", ""), NA,
             ifelse(bld.c.alt %in% c("<6"), 6,
                    as.numeric(as.character(bld.c.alt))))
    bld.c.alt <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.alt + b, bld.c.alt)

    m <- (3.6 - 0.35)/(5 - 0.3)
    b <- 3.6 - m*5
    bld.c.tsh.redcap <- bld.c.tsh
    # adding 13 Dec 2016. Coding requested by AJ in email.
    bld.c.tsh <-
      ifelse(is.na(bld.c.tsh) | bld.c.tsh %in% c("-9999", ""), NA,
             ifelse(bld.c.tsh %in% c("<0.015"), 0.014, as.numeric(as.character(bld.c.tsh))))
    bld.c.tsh <- ifelse(map.id %in% idsForConversion & epoch==1, m*bld.c.tsh+ b, bld.c.tsh)

    mf <- (1.11 - 0.57)/(1.5 - 0.7)
    bf <- 1.11 - mf*1.5
    mm <- (1.25 - 0.72)/(1.5 - 0.7)
    bm <- 1.25 - mm*1.5

    bld.c.creatinine.redcap <- bld.c.creatinine
    bld.c.creatinine <- ifelse(map.id %in% idsForConversion & sex.factor=="Male" & epoch==1,mm*bld.c.creatinine.redcap + bm,
                               ifelse(map.id %in% idsForConversion & sex.factor=="Female" & epoch==1
                                      ,mf*bld.c.creatinine.redcap + bf, bld.c.creatinine.redcap))

    m1 <- (150 - 40)/(110 - 40)
    b1 <- 150 - m1*110
    m2 <- (150 - 40)/(190 - 40)
    b2 <- 150 - m2*190

    bld.c.alkphos.redcap <- bld.c.alkphos
    bld.c.alkphos <- ifelse(map.id %in% idsForConversion & sex.factor=="Male" & age<=64 & epoch==1, m1*bld.c.alkphos.redcap + b1,
                            ifelse(map.id %in% idsForConversion & sex.factor=="Male" & age < 64 & epoch==1, m2*bld.c.alkphos.redcap + b2,
                                   ifelse(map.id %in% idsForConversion & sex.factor=="Female" & age <= 64 & epoch==1, m1*bld.c.alkphos.redcap + b1,
                                          ifelse(map.id %in% idsForConversion & sex.factor=="Female" & age > 64 & epoch==1, m2*bld.c.alkphos.redcap + b2,
                                                 bld.c.alkphos.redcap))))

    m <- 2.9/3

    bld.c.crp.redcap <- bld.c.crp
    bld.c.crp <-
      ifelse(is.na(bld.c.crp) | bld.c.crp %in% c("-9999", ""), NA,
             ifelse(bld.c.crp %in% c("<0.2"), 0.1,
                    as.numeric(as.character(bld.c.crp))))
    bld.c.crp <- ifelse(map.id %in% idsForConversion & epoch==1,m*bld.c.crp,bld.c.crp)

    bld.c.igm.redcap <- bld.c.igm
    bld.c.igm <-
      ifelse(is.na(bld.c.igm) | bld.c.igm %in% c("-9999", ""), NA,
             ifelse(bld.c.igm %in% c("> 60.0", ">60.0"), 60.1,
                    as.numeric(as.character(bld.c.igm))))
    label(bld.c.igm) <- "Clinical Blood-APA-IgM, set to 60.1 if `> 60.0'"

    # adding 13 Dec 2016. Coding requested by AJ in email.
    bld.c.igg.redcap <- bld.c.igg
    bld.c.igg <-
      ifelse(is.na(bld.c.igg) | bld.c.igg %in% c("-9999", ""), NA,
             ifelse(bld.c.igg %in% c(">80.0"), 80.1, as.numeric(as.character(bld.c.igg))))

    m1 <- (4.6 - 3.2)/(5 - 3.5)
    b1 <- 4.6 - m1*5
    m2 <- (24.6 - 13.9)/(5 - 3.5)
    b2 <- 24.6 - m2*5

    bld.c.albumin.redcap <- bld.c.albumin
    bld.c.albumin <- ifelse(map.id %in% idsForConversion & age < 90 & epoch==1, m1*bld.c.albumin.redcap + b1,
                            ifelse(map.id %in% idsForConversion & age >= 90 & epoch==1, m2*bld.c.albumin.redcap + b2,
                                   bld.c.albumin.redcap))

    # Recalculating these variables based on the new converted inputs
    bld.c.egfr <- ifelse(sex==1 & epoch==1,175*bld.c.creatinine^(-1.154)*age^(-.203),175*bld.c.creatinine^(-1.154)*age^(-.203)*.742)

    bld.c.egfraa <- bld.c.egfr*1.212

    bld.c.homair <- bld.c.insulin/18*bld.c.glucose*100/405

    insulin.resist <- ifelse(bld.c.homair>4.65 & epoch==1,1,0)
    insulin.resist <- factor(insulin.resist,levels=c(0,1))


    label(bld.c.insulin) = "Set to 1.9 if `<2.0', Converted Insulin Values (mcU/mL)"
    label(bld.c.glucose) = "Converted Glucose Level (mg/dL)"
    label(bld.c.sodium) = "Converted Sodium Level (mmol/L)"
    label(bld.c.chloride) = "Converted Chloride Level (mmol/L)"
    label(bld.c.co2) = "Converted Carbon Dioxide Level (mmol/L)"
    label(bld.c.urea.nitrogen) = "Converted BUN (mg/dL)"
    label(bld.c.calcium) = "Converted Calcium Level Total (mg/dL)"
    label(bld.c.protein.t) = "Converted Protein Level (g/dL)"
    label(bld.c.bilirubin.t) = "Converted Bilirubin Total (mg/dL)"
    label(bld.c.ast) = "Converted AST (U/L)"
    label(bld.c.alt) = "Set to 6 if `<6', Converted ALT (U/L)"
    label(bld.c.tsh) = "Set to 0.014 if `<0.015', Converted TSH (mcU/mL)"
    label(bld.c.creatinine) = "Converted Creatinine (mg/dL)"
    label(bld.c.alkphos) = "Converted Alkaline Phosphatase (U/L)"
    label(bld.c.crp) = "Set to 0.1 if `<0.2', Converted C-Reactive Protein HS - Cardiac (mg/L)"
    # label(creatinine.clearance)
    label(bld.c.egfr) <- "Clinical Blood - eGFR, recalculated"
    label(bld.c.egfraa) <- "Clinical Blood - eGFRAA, recalculated"
    label(bld.c.homair) <- "Clinical Blood - HOMA-IR (m/dL), recalculated"
    label(insulin.resist) <- "Clinical Blood - Insulin resistance, recalculated"
    label(bld.c.igg) <- "Clinical Blood-APA-IgG, set to 80.1 if `>80.0'"

    rm(mf,mm,m,m1,m2,b,b1,b2,bf,bm)
  })

  return(data)
}
