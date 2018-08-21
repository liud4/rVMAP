depression <- function(dat) {
  # Returns dat with depression derived variables added
  # and with some totals that were calculated in REDCap recalculated
  # Code from LS & JN
  # Make sure to source "scoringFunctions.R" before calling this function

  # 16 Jan 2015: re-calculating gds total score
  # 02 Mar 2015:  AJ and KG confirmed in meeting that they
  # want to use the 85% rule for this one
  gdsvars <- paste0("gds", 1:30)
  dat$gds <- apply(dat[, gdsvars], 1, totscore)

  #QIDS
  qids.sub <- paste0("qids", formatC(1:16, width = 2, format = "d", flag = "0"))
  my.pmax <- function(x) {
    apply(x, MARGIN = 1, FUN = function(y) ifelse(sum(!is.na(y)) == 0, NA, max(y, na.rm = TRUE)))
  }
  dat$qids <- rowSums(
    cbind(
      my.pmax(dat[, qids.sub[1:4]]),
      my.pmax(dat[, qids.sub[6:9]]),
      my.pmax(dat[, qids.sub[15:16]]),
      dat[, qids.sub[c(5, 10:14)]]
    ),
    na.rm = TRUE
  )
  # TODO: The above is probably not handling missing values correctly.  JN will check with VMAC.

  dat <- within(dat, {
    # 09 Jan 2015: modified gds variable for KG
    gds.minus.gds14 <- gds - gds14
    label(gds.minus.gds14) <- "GDS tot score minus gds14 (memory prob.)"

    # 19 Oct 2015: modified gds variable for KG
    # Email from Laurie 08 Oct 2015
    gds.minus.cog <- gds - (gds14 + gds26 + gds29 + gds30)
    label(gds.minus.cog) <- "GDS tot score minus cog. items (14, 26, 29, 30)"

    # Dichotomizations
    depress.gds         <- ifelse(is.na(gds), NA,
                                  ifelse(gds >= 10, 1, 0))
    depress.gds.factor  <- factor(depress.gds,
                                  levels = c(1, 0), labels = c("Yes", "No"))
    depress.qids        <- ifelse(is.na(qids), NA,
                                  ifelse(qids >= 6, 1, 0))
    depress.qids.factor <- factor(depress.qids,
                                  levels = c(1, 0), labels = c("Yes", "No"))
  })

  dat <- Hmisc::upData(
    dat,
    labels = c(
      gds                 = "GDS total score, recalculated",
      qids                = "QIDS total score, recalculated",
      depress.gds         = "Depression - GDS Definition",
      depress.gds.factor  = "Depression - GDS Definition",
      depress.qids        = "Depression - QIDS Definition",
      depress.qids.factor = "Depression - QIDS Definition"
    ))

  dat
}
