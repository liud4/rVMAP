#' Derive, label, and add PVLT variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added PVLT variables.
#' @export

derive_pvlt <- function(data) {
  data <- within(data, {
    np.pvlta.tot.redcap <- np.pvlta.tot
    np.pvlta.intrus.redcap <- np.pvlta.intrus
    np.pvlta.pers.redcap <- np.pvlta.pers
    np.pvlta.clust.redcap <- np.pvlta.clust
    np.pvlta.primperc.redcap <- np.pvlta.primperc
    np.pvlta.midperc.redcap <- np.pvlta.midperc
    np.pvlta.recperc.redcap <- np.pvlta.recperc
    np.pvlt8.redcap <- np.pvlt8
    np.pvlt10.redcap <- np.pvlt10
    np.pvltrecog.discrim.redcap <- np.pvltrecog.discrim
    np.pvlt5.sdfr.redcap <- np.pvlt5.sdfr
    np.pvlt5.ldfr.redcap <- np.pvlt5.ldfr
    np.pvlt.sdfr.sdcr.redcap <- np.pvlt.sdfr.sdcr
    np.pvlt.ldfr.ldcr.redcap <- np.pvlt.ldfr.ldcr
    np.pvlt.sdfr.ldfr.redcap <- np.pvlt.sdfr.ldfr
    np.pvlt.ldfr.dr.redcap <- np.pvlt.ldfr.dr
    np.pvlt5.dr.redcap <- np.pvlt5.dr
    np.mc1.acc.redcap <- np.mc1.acc
    np.mc2.acc.redcap <- np.mc2.acc
    np.mc3.acc.redcap <- np.mc3.acc
    np.mc4.acc.redcap <- np.mc4.acc
    np.mc5.acc.redcap <- np.mc5.acc
    np.mc6.acc.redcap <- np.mc6.acc
    np.mc7.acc.redcap <- np.mc7.acc
    np.mc8.acc.redcap <- np.mc8.acc
    np.mc.auto.mc.redcap <- np.mc.auto.mc
    np.mc.nonauto.mc.redcap <- np.mc.nonauto.mc

    np.pvlta.tot <- np.pvlt1 + np.pvlt2 + np.pvlt3 + np.pvlt4 + np.pvlt5
    label(np.pvlta.tot) <- "Total, np.pvlt1 to 5"

    np.pvlta.intrus <- np.pvlt1.intrus + np.pvlt2.intrus + np.pvlt3.intrus + np.pvlt4.intrus + np.pvlt5.intrus
    label(np.pvlta.intrus) <- "PVLT Intrus"

    np.pvlta.pers <- np.pvlt1.pers + np.pvlt2.pers + np.pvlt3.pers + np.pvlt4.pers + np.pvlt5.pers
    label(np.pvlta.pers) <- "PVLT Pers"

    np.pvlta.clust <- np.pvlt1.clust + np.pvlt2.clust + np.pvlt3.clust + np.pvlt4.clust + np.pvlt5.clust
    label(np.pvlta.clust) <- "PVLT Cluster"

    np.pvlta.primperc <- np.pvlta.prim / np.pvlta.tot
    np.pvlta.midperc  <- np.pvlta.mid / np.pvlta.tot
    np.pvlta.recperc  <- np.pvlta.rec / np.pvlta.tot
    label(np.pvlta.primperc) <- "PVLT Primacy Percentage"
    label(np.pvlta.midperc) <- "PVLT Middle Percentage"
    label(np.pvlta.recperc) <- "PVLT Recency Percentage"

    np.pvlt8 <- np.pvlt8.fruit + np.pvlt8.office + np.pvlt8.clothing
    np.pvlt10 <- np.pvlt10.fruit + np.pvlt10.office + np.pvlt10.clothing
    label(np.pvlt8) <- "PVLT 8"
    label(np.pvlt10) <- "PVLT 10"

    # OAK 20181008: https://github.com/liud4/rVMAP/issues/4
    # np.pvltrecog.discrim <- 1 - ((12 - np.pvltrecog.m + np.pvltrecog.foil) / 48)
    np.pvltrecog.discrim <- (np.pvltrecog.m + 0.5)/13 - (np.pvltrecog.falsepos + 0.5)/37
    label(np.pvltrecog.discrim) <- label(np.pvltrecog.discrim.redcap)

    np.pvlt5.sdfr <- (np.pvlt7 - np.pvlt5) / np.pvlt5
    np.pvlt5.ldfr <- (np.pvlt9 - np.pvlt5) / np.pvlt5
    label(np.pvlt5.sdfr) <- label(np.pvlt5.sdfr.redcap)
    label(np.pvlt5.ldfr) <- label(np.pvlt5.ldfr.redcap)

    np.pvlt.sdfr.sdcr <- (np.pvlt8 - np.pvlt7) / np.pvlt7
    np.pvlt.sdfr.sdcr[np.pvlt.sdfr.sdcr == Inf | np.pvlt.sdfr.sdcr == "NaN"] <- NA
    label(np.pvlt.sdfr.sdcr) <-  label(np.pvlt.sdfr.sdcr.redcap)

    np.pvlt.ldfr.ldcr <- (np.pvlt10 - np.pvlt9) / np.pvlt9
    np.pvlt.ldfr.ldcr[np.pvlt.ldfr.ldcr == Inf | np.pvlt.ldfr.ldcr == "NaN"] <- NA
    label(np.pvlt.ldfr.ldcr) <-  label(np.pvlt.ldfr.ldcr.redcap)

    np.pvlt.sdfr.ldfr <- (np.pvlt9 - np.pvlt7) / np.pvlt7
    np.pvlt.sdfr.ldfr[np.pvlt.sdfr.ldfr == Inf | np.pvlt.sdfr.ldfr == "NaN"] <- NA
    label(np.pvlt.sdfr.ldfr) <- label(np.pvlt.sdfr.ldfr.redcap)

    np.pvlt.ldfr.dr <- (np.pvltrecog.m - np.pvlt9) / np.pvltrecog.m
    label(np.pvlt.ldfr.dr) <- label(np.pvlt.ldfr.dr.redcap)

    np.pvlt5.dr <- (np.pvltrecog.m - np.pvlt5) / np.pvltrecog.m
    label(np.pvlt5.dr) <- label(np.pvlt5.dr.redcap)

    np.mc1.acc <- 1 - (np.mc1.omissions + np.mc1.falsepos) / 20
    np.mc2.acc <- 1 - (np.mc2.omissions + np.mc2.falsepos) / 26
    np.mc3.acc <- 1 - (np.mc3.omissions + np.mc3.falsepos) / 14
    np.mc4.acc <- 1 - (np.mc4.omissions + np.mc4.falsepos) / 12
    np.mc5.acc <- 1 - (np.mc5.omissions + np.mc5.falsepos) / 12
    np.mc6.acc <- 1 - (np.mc6.omissions + np.mc6.falsepos) / 9
    np.mc7.acc <- 1 - (np.mc7.omissions + np.mc7.falsepos) / 11
    np.mc8.acc <- 1 - (np.mc8.omissions + np.mc8.falsepos) / 13

    label(np.mc1.acc) <- label(np.mc1.acc.redcap)
    label(np.mc2.acc) <- label(np.mc2.acc.redcap)
    label(np.mc3.acc) <- label(np.mc3.acc.redcap)
    label(np.mc4.acc) <- label(np.mc4.acc.redcap)
    label(np.mc5.acc) <- label(np.mc5.acc.redcap)
    label(np.mc6.acc) <- label(np.mc6.acc.redcap)
    label(np.mc7.acc) <- label(np.mc7.acc.redcap)
    label(np.mc8.acc) <- label(np.mc8.acc.redcap)

    np.mc.auto.mc <- (np.mc1.acc + np.mc2.acc + np.mc3.acc + np.mc4.acc) / 4
    label(np.mc.auto.mc) <- label(np.mc.auto.mc.redcap)

    np.mc.nonauto.mc <- (np.mc5.acc + np.mc6.acc + np.mc7.acc) / 3
    label(np.mc.nonauto.mc) <- label(np.mc.nonauto.mc.redcap)
  })
  pvlt.pers <- paste0("np.pvlt", formatC(1:10, digits = 0), ".pers")
  pvlt.intrus <- paste0("np.pvlt", formatC(1:10, digits = 0), ".intrus")
  pvlt.clust <- c(paste0("np.pvlt", formatC(1:7, digits = 0), ".clust"), "np.pvlt9.pers")

  data$np.pvlt.pers.total <- apply(data[,pvlt.pers], 1, sum)
  data$np.pvlt.intrus.total <- apply(data[,pvlt.intrus], 1, sum)
  data$np.pvlt.clust.total <- apply(data[,pvlt.clust], 1, sum)

  label(data$np.pvlt.pers.total) <- "Total, np.pvlt.pers"
  label(data$np.pvlt.intrus.total) <- "Total, np.pvlt.intrus"
  label(data$np.pvlt.clust.total) <- "Total, np.pvlt.clust"

  data$np.pvlt.proint <- data$np.pvlt6 - data$np.pvlt1
  data$np.pvlt.shortretention <- data$np.pvlt7 - data$np.pvlt5
  data$np.pvlt.longretention <- data$np.pvlt9 - data$np.pvlt5
  data$np.pvlt.freerecallint <- data$np.pvlt7.intrus + data$np.pvlt9.intrus

  label(data$np.pvlt.proint) <- "PVLT6 - PVLT1"
  label(data$np.pvlt.shortretention) <- "PVLT7 - PVLT5"
  label(data$np.pvlt.longretention) <- "PVLT9 - PVLT5"
  label(data$np.pvlt.freerecallint) <- "pvlt7.intrus + pvlt9.intrus"

  data$np.pvlt.cuedrecallint <- data$np.pvlt8.intrus + data$np.pvlt10.intrus
  label(data$np.pvlt.cuedrecallint) <- "pvlt8.intrus + pvlt10.intrus"

  for (row.i in 1:nrow(data)) {
    y <- as.numeric(data[row.i, c("np.pvlt1", "np.pvlt2", "np.pvlt3", "np.pvlt4", "np.pvlt5")])
    if (sum(is.na(y)) != 5) {
      x <- seq(1:5)
      res <- lm(y ~ x)
      regcoef <- res$coefficients[2]
    } else {
      regcoef <- NA
    }
    data[row.i, "np.pvlt.slope"] <- regcoef  
  }

  return(data)
}
