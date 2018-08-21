# NOTE: this function uses bsa, which is recalculated in medhxEtc.
#   That function needs to be called before this one.
# NOTE: ALSO: the function invalidCmr needs to be called before this one.

cardiacMRI <- function(dat) {
  dat <- within(dat, {
    #label(scan.date)="Scan date"
    label(qmass.usable)="Is QMass data usable?"
    label(qmass.label)="XNAT label with scan date & analysis"
    label(qmass.analysis.name)="Series name"
    label(bsa.mosteller)="BSA-Mosteller"
    label(qmass.method)="Medis analysis label"
    label(qmass.normal.range.method)="reference range "
    label(qmass.lv.absolute.ed.mass)="Left Ventricular Mass"
    label(qmass.lv.absolute.edv)="Left Ventricular End Diastolic Volume"
    label(qmass.lv.absolute.esv)="Left Ventricular End Systolic Volume "
    label(qmass.lv.absolute.sv)="Left Ventricular Stroke Volume"
    label(qmass.lv.absolute.ef)="Left Ventricular Ejection Fraction"
    label(qmass.lv.absolute.co)="Left Ventricular Cardiac Output "
    label(qmass.lv.bsa.indexed.ed.mass)="Left Ventricular Mass indexed to BSA"
    label(qmass.lv.bsa.indexed.edv)="Left Ventricular End Diastolic Volume indexed to BSA "
    label(qmass.lv.bsa.indexed.esv)="Left Ventricular End Systolic Volume indexed to BSA "
    label(qmass.lv.bsa.indexed.sv)="Left Ventricular Stroke Volume indexed to BSA"
    label(qmass.lv.bsa.indexed.co)="Left Ventricular Cardiac Output indexed to BSA"
    label(qmass.rv.absolute.edv)="Right Ventricular End Diastolic Volume"
    label(qmass.rv.absolute.esv)="Right Ventricular End Systolic Volume "
    label(qmass.rv.absolute.sv)="Right Ventricular Stroke Volume"
    label(qmass.rv.absolute.ef)="Right Ventricular Ejection Fraction"
    label(qmass.rv.absolute.co)="Right Ventricular Cardiac Output "
    label(qmass.rv.bsa.indexed.edv)="Right Ventricular End Diastolic Volume indexed to BSA "
    label(qmass.rv.bsa.indexed.esv)="Right Ventricular End Systolic Volume indexed to BSA "
    label(qmass.rv.bsa.indexed.sv)="Right Ventricular Stroke Volume indexed to BSA"
    label(qmass.rv.bsa.indexed.co)="Right Ventricular Cardiac Output indexed to BSA"
    label(qmass.comments)="QMass Comments"
    label(qmass.complete)="Complete?"
    label(qstrain.usable)="Is QStrain data usable?"
    label(qstrain.label.longaxis)="XNAT label with scan date & Qstrain long axis analysis"
    label(qstrain.2ch.analysis.date)="2ch LA Strain analysis date"
    label(qstrain.2ch.segments.model)="2ch segments model"
    label(qstrain.2ch.bpm)="2ch LA heart rate "
    label(qstrain.2ch.pixel.dimension)="2ch Pixel Dimension"
    label(qstrain.2ch.edv)="2ch LA Left Ventricular End Diastolic Volume"
    label(qstrain.2ch.esv)="2ch LA Left Ventricular End Systolic Volume "
    label(qstrain.2ch.ef)="2ch LA Ejection Fraction "
    label(qstrain.2ch.grs)="2ch LA Global Radial Strain based on Endo contours"
    label(qstrain.2ch.myogls)="2ch LA Global Longitudinal Strain"
    label(qstrain.2ch.myogcs)="2ch LA Global Circumferntial Strain"
    label(qstrain.2ch.sd.ts.peak)="2ch Standard Deviation of peak Transverse (Radial) strain "
    label(qstrain.2ch.sd.ls.peak)="2ch Standard Deviation of peak Longitudinal strain "
    label(qstrain.4ch.analysis.date)="4ch LA strain analysis date"
    label(qstrain.4ch.segments.model)="4ch segments model"
    label(qstrain.4ch.bpm)="4ch LA heart rate "
    label(qstrain.4ch.pixel.dimension)="4ch Pixel Dimension"
    label(qstrain.4ch.edv)="4ch LA Left Ventricular End Diastolic Volume"
    label(qstrain.4ch.esv)="4ch LA Left Ventricular End Systolic Volume "
    label(qstrain.4ch.ef)="4ch LA Ejection Fraction "
    label(qstrain.4ch.grs)="4ch LA Global Radial Strain"
    label(qstrain.4ch.myogls)="4ch LA Global Longitudinal Strain"
    label(qstrain.4ch.myogcs)="4ch LA Global Circumferntial Strain"
    label(qstrain.4ch.sd.ts.peak)="4ch Standard Deviation of peak Transverse (Radial) strain "
    label(qstrain.4ch.sd.ls.peak)="4ch Standard Deviation of peak Longitudinal strain "
    label(qstrain.avg.grs)="2ch and 4 ch LA Average Global Radial Strain"
    label(qstrain.label.shortaxis)="XNAT label with scan date & Qstrain short axis analysis"
    label(qstrain.apex.analysis.date)="SA strain analysis date"
    label(qstrain.apex.segments.model)="Apex segments.model"
    label(qstrain.apex.bpm)="SA apex heart rate "
    label(qstrain.apex.pixel.dimension)="Apex Pixel Dimension"
    label(qstrain.apex.eda)="SA apex end diastolic area"
    label(qstrain.apex.esa)="SA apex end systolic area"
    label(qstrain.apex.fac)="Apex Fractional Area Change "
    label(qstrain.apex.myorot)="SA myocardial rotation "
    label(qstrain.apex.myogcs)="SA myocardial gobal circumferential strain "
    label(qstrain.apex.grs)="SA myocardial gobal radial strain "
    label(qstrain.apex.delta.rot)="Difference in rotation between Apex and Base "
    label(qstrain.apex.sd.rs.peak)="Apex Standard Deviation of peak Transverse (Radial) strain "
    label(qstrain.apex.sd.cs.peak)="Apex Standard Deviation of peak Longitudinal strain "
    label(qstrain.mid.analysis.date)="SA strain analysis date"
    label(qstrain.mid.segments.model)="Mid segments.model"
    label(qstrain.mid.bpm)="SA mid heart rate "
    label(qstrain.mid.pixel.dimension)="Mid Pixel Dimension"
    label(qstrain.mid.eda)="SA mid end diastolic area"
    label(qstrain.mid.esa)="SA mid end systolic area"
    label(qstrain.mid.fac)="Mid Fractional Area Change "
    label(qstrain.mid.myorot)="SA mid myocardial rotation "
    label(qstrain.mid.myogcs)="SA mid myocardial gobal circumferential strain "
    label(qstrain.mid.grs)="SA mid myocardial gobal radial strain "
    label(qstrain.mid.delta.rot)="Difference in rotation between Apex and Base "
    label(qstrain.mid.sd.rs.peak)="Mid Standard Deviation of peak Transverse (Radial) strain "
    label(qstrain.mid.sd.cs.peak)="Mid Standard Deviation of peak Longitudinal strain "
    label(qstrain.base.analysis.date)="SA strain analysis date"
    label(qstrain.base.segments.model)="Base segments.model"
    label(qstrain.base.bpm)="SA base heart rate "
    label(qstrain.base.pixel.dimension)="Base Pixel Dimension"
    label(qstrain.base.eda)="SA base end diastolic area"
    label(qstrain.base.esa)="SA base end systolic area"
    label(qstrain.base.fac)="Base Fractional Area Change "
    label(qstrain.base.myorot)="SA base myocardial rotation "
    label(qstrain.base.myogcs)="SA base myocardial gobal circumferential strain "
    label(qstrain.base.grs)="SA base myocardial gobal radial strain "
    label(qstrain.base.delta.rot)="Difference in rotation between Apex and Base "
    label(qstrain.base.sd.rs.peak)="Base Standard Deviation of peak Transverse (Radial) strain "
    label(qstrain.base.sd.cs.peak)="Base Standard Deviation of peak Longitudinal strain "
    label(qstrain.comments)="QStrain Comments"
    label(qstrain.complete)="Complete?"
    label(pwv.usable)="Is Pulse Wave Velocity Data Usable?"
    label(aorta.length.cm)="Aortic Length"
    label(aorta.length.m)="Aorta Length"
    label(tdiff.halfmax)="Time Differential at Half Maximum"
    label(pwv)="Pulse Wave Velocity"
    label(pwv.comments)="Pulse Wave Velocity Comments"
    label(pulse.wave.velocity.complete)="Complete?"

    #Setting Factors(will create new variable for factors)
    qmass.usable.factor = factor(qmass.usable, levels = c("1", "0"))
    qmass.complete.factor = factor(qmass.complete, levels = c("0", "1", "2"))
    qstrain.usable.factor = factor(qstrain.usable, levels = c("1", "0"))
    qstrain.complete.factor = factor(qstrain.complete, levels = c("0", "1", "2"))
    pwv.usable.factor = factor(pwv.usable, levels = c("1", "0"))
    pulse.wave.velocity.complete.factor = factor(pulse.wave.velocity.complete, levels = c("0", "1", "2"))

    levels(qmass.usable.factor) = c("Yes", "No")
    levels(qmass.complete.factor) = c("Incomplete", "Unverified", "Complete")
    levels(qstrain.usable.factor) = c("Yes", "No")
    levels(qstrain.complete.factor) = c("Incomplete", "Unverified", "Complete")
    levels(pwv.usable.factor) = c("Yes", "No")
    levels(pulse.wave.velocity.complete.factor) = c("Incomplete", "Unverified", "Complete")

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

    dat
}
#alldat <- cmr(alldat)
