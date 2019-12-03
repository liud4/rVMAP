#' Derive, label, and add cardiac MRI variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added cardiac MRI variables.
#' @export

derive_cardiac_mri <- function(data) {
  data <- within(data, {
    qmass.usable.factor <- factor(qmass.usable, levels = c(0, 1), labels = c("No", "Yes"))
    qstrain.usable.factor <- factor(qstrain.usable, levels = c(0, 1), labels = c("No", "Yes"))
    pwv.usable.factor <- factor(pwv.usable, levels = c(0, 1), labels = c("No", "Yes"))
  })

  #within(dat,{
  #dat <- within(dat,{
  # qmass.lv.stroke.volume=ifelse(qmass.lv.stroke.volume==-9999, NA, qmass.lv.stroke.volume)
  #pp.index=pp/qmass.lv.stroke.volume
  #label(pp.index)       = 'Echo Pulse Pressure Indexed by Stroke Volume'

  #  qmass.lv.mass <- ifelse(qmass.lv.mass==-9999,NA,qmass.lv.mass)
  # qmass.lv.mass <- as.numeric(qmass.lv.mass)

  #qmass.cardiac.index <- qmass.lv.cardiac.output/bsa
  #tomtec.cardiac.index <- (tomtec.stroke.volume*qmass.heart.rate)/bsa
  #tomtec.cardiac.index <- ifelse(dat$map.id %in% c("139","173","228"),(tomtec.stroke.volume*cmr.findings.pulse)/bsa,tomtec.cardiac.index)
  #tomtec.cardiac.index <- tomtec.cardiac.index/1000
  #qmass.lv.mass.index <- qmass.lv.mass/((weight^0.425*height^0.725)*0.007184)
  #qmass.lv.es.volume.index <- qmass.lv.es.volume/((weight^0.425)*(height^0.725)*0.007184)

  #qmass.lv.ed.volume.index <- qmass.lv.ed.volume/((weight^0.425)*(height^0.725)*0.007184)

  #qmass.lv.stroke.volume.index <- qmass.lv.stroke.volume/((weight^0.425)*(height^0.725)*0.007184)

  #qmass.lv.cardiac.index <- qmass.lv.cardiac.output/((weight^0.425)*(height^0.725)*0.007184)

  # Change to numeric, some numbers include commas
  #qflow.ascending.aorta.max.area <- as.numeric(gsub(",","", qflow.ascending.aorta.max.area))
  #qflow.ascending.aorta.max.area <- ifelse(qflow.ascending.aorta.max.area==-9999,NA,qflow.ascending.aorta.max.area)

  #qflow.ascending.aorta.min.area <- as.numeric(gsub(",","", qflow.ascending.aorta.min.area))
  #qflow.ascending.aorta.min.area <- ifelse(qflow.ascending.aorta.min.area==-9999,NA,qflow.ascending.aorta.min.area)
  #cmr.findings.pp <- as.numeric(cmr.findings.pp)

  #qmass.ascen.ao.compliance <- (qflow.ascending.aorta.max.area - qflow.ascending.aorta.min.area)/(cmr.findings.pp)

  # Change to numeric, some numbers include commas
  #qflow.descending.aorta.max.area <- as.numeric(gsub(",","", qflow.descending.aorta.max.area))
  #qflow.descending.aorta.max.area <- ifelse(qflow.descending.aorta.max.area==-9999,NA,qflow.descending.aorta.max.area)

  #qflow.descending.aorta.min.area <- as.numeric(gsub(",","", qflow.descending.aorta.min.area))
  #qflow.descending.aorta.min.area <- ifelse(qflow.descending.aorta.min.area==-9999,NA,qflow.descending.aorta.min.area)

  #qmass.descend.ao.compliance <- (qflow.descending.aorta.max.area - qflow.descending.aorta.min.area)/(cmr.findings.pp)

  #qmass.myocardial.contraction.fraction <- (qmass.lv.stroke.volume)/(qmass.lv.mass*1.05)

  # Labeling Variables
  #label(qmass.cardiac.index) <- "Cardiac Index - QMASS"
  #label(tomtec.cardiac.index) <- "Cardiac Index - TomTec"
  #label(qmass.lv.mass.index) <- "LV mass index (LVMi) in g/m2 - QMASS"
  #label(qmass.lv.es.volume.index) <- "LV end systolic volume index (LVESVi) in mL/m2 - QMASS"
  #label(qmass.lv.ed.volume.index) <- "LV end diastolic volume index (LVEDVi) in mL/m2 - QMASS"
  #label(qmass.lv.stroke.volume.index) <- "LV stroke volume index (LVSVi) in mL/m2 - QMASS"
  #label(qmass.lv.cardiac.index) <- "LV cardiac index (LVCi) in mL/m2 - QMASS"
  #label(qmass.ascen.ao.compliance) <- "Ascending aortic compliance in mm2/mmHG - QMASS"
  #label(qmass.descend.ao.compliance) <- "Descending aortic compliance in mm2/mmHG - QMASS"
  #label(qmass.myocardial.contraction.fraction) <- "Myocardial Contraction Fraction - QMASS"

  #  })

  return(data)
}
