#' Derive, label, and add automated 3T variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added automated 3T variables.
#' @export

derive_automated_3T <- function(data) {
  
  asl.3t.bl.var <-
    c(
      "asl.3t.bl.etco2a",
      "asl.3t.bl.etco2b",
      "asl.3t.bl.etco2.1",
      "asl.3t.bl.etco2c",
      "asl.3t.bl.etco2d",
      "asl.3t.bl.etco2.2",
      "asl.3t.bl.etco2e",
      "asl.3t.bl.etco2.3",
      "asl.3t.bl.etco2f",
      "asl.3t.bl.etco2g"
    )
  
  asl.3t.hyper.var <-
    c(
      "asl.3t.hyper.etco2a",
      "asl.3t.hyper.etco2b",
      "asl.3t.hyper.etco2.1",
      "asl.3t.hyper.etco2c",
      "asl.3t.hyper.etco2d",
      "asl.3t.hyper.etco2.2",
      "asl.3t.hyper.etco2e",
      "asl.3t.hyper.etco2.3",
      "asl.3t.hyper.etco2f",
      "asl.3t.hyper.etco2g"
    )
  
  asl.3t.trust.var <-
    c("asl.3t.trust.spo2",
      "asl.3t.trust.spo2.1",
      "asl.3t.trust.spo2.2")

  data$icv <- rowSums(data[, Cs(vbmqa.gm.vol, vbmqa.wm.vol, vbmqa.csf.vol)])
  label(data$icv) <- "ICV (calculated)"
  
  data <- within(data, {
    wml.volume.plus.1.log <- log(wml.volume + 1)
    label(wml.volume.plus.1.log) <- "Log of wml.volume + 1"

    wml.volume.frontal.lobe.plus.1.log <- log(wml.volume.frontal.lobe + 1)
    label(wml.volume.frontal.lobe.plus.1.log) <- "Log of wml.volume.frontal.lobe + 1"

    wml.volume.occipital.lobe.plus.1.log <- log(wml.volume.occipital.lobe + 1)
    label(wml.volume.occipital.lobe.plus.1.log) <- "Log of wml.volume.occipital.lobe + 1"

    wml.volume.temporal.lobe.plus.1.log <- log(wml.volume.temporal.lobe + 1)
    label(wml.volume.temporal.lobe.plus.1.log) <- "Log of wml.volume.temporal.lobe + 1"

    wml.volume.parietal.lobe.plus.1.log <- log(wml.volume.parietal.lobe + 1)
    label(wml.volume.parietal.lobe.plus.1.log) <- "Log of wml.volume.parietal.lobe + 1"

    asl.3t.rest.etco2 <- coalesce(rowMeans(data[, asl.3t.bl.var], na.rm = TRUE), as.numeric(asl.rest.mean.etco2))
    label(asl.3t.rest.etco2) <- "Resting EtCO2"

    asl.3t.chall.etco2 <- coalesce(rowMeans(data[, asl.3t.hyper.var], na.rm = TRUE), as.numeric(asl.chall.mean.etco2))
    label(asl.3t.chall.etco2) <- "Challenge EtCO2"
    
    asl.3t.change.etco2 <- asl.3t.chall.etco2 - asl.3t.rest.etco2
    label(asl.3t.change.etco2) <- "Change in EtCO2"
  })

  # CMRO2 and OEF  (OAK 20191203)

  data$oef.ya <- coalesce(rowMeans(data[, asl.3t.trust.var], na.rm = TRUE), as.numeric(data$trust.mean.spo2))
  data$oef.ya[is.nan(data$oef.ya)] <- NA
  label(data$oef.ya) <- "Arterial Oxygenation (%)"

  data$oef.cmro2.hct <- data$asl.rest.grey.matter.hct * ((data$oef.ya - data$oef.yv.hct) * 0.01) * (0.556 * data$bld.c.hgb) # see https://github.com/liud4/rVMAP/issues/39
  label(data$oef.cmro2.hct) <- "Cerebral Metabolic Rate of Oxygen (µmol/100 g/min)"

  data$oef.oef <- 100 * ((data$oef.ya - data$oef.yv) / data$oef.ya)
  label(data$oef.oef) <- "Oxygen Extraction Fraction (%)"

  data$oef.oef.hct <- 100 * ((data$oef.ya - data$oef.yv.hct) / data$oef.ya)
  label(data$oef.oef.hct) <- "Oxygen Extraction Fraction (%) Hct corrected"

  return(data)
}
