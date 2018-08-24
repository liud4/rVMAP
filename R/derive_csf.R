#' Derive, convert, label, and add CSF variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added CSF variables.
#' @export

derive_csf <- function(data) {
  idsForConversion <- formatC(1:63, width = 3, format = "d", flag = "0")

  data <- within(data, {
    # To find cutoff date:
    # mydat$csf.date<"2013-09-23"
    # mydat$csf.date[63]
    # mydat[mydat$map.id=="063","csf.date"]

    csf.c.protein.redcap <- csf.c.protein
    m <- (40-15)/(60-30)
    b <- 40 - m*60

    csf.c.protein <- ifelse(map.id %in% idsForConversion & epoch == 1, m * csf.c.protein.redcap + b, csf.c.protein.redcap)

    csf.c.gluc <- ifelse(csf.c.gluc == "-9999" | csf.c.gluc == "-8888", NA, csf.c.gluc)

    csf.c.insulin.redcap <- csf.c.insulin
    csf.c.insulin <- ifelse(csf.c.insulin.redcap == "-9999" | csf.c.insulin.redcap == "-8888" | csf.c.insulin.redcap == "", NA, csf.c.insulin.redcap)
    csf.c.insulin <- as.factor(csf.c.insulin)

    label(csf.date)="date - CSF"
    label(csffast.last.ate.date)="date last ate - fasting compliance check - CSF"
    label(csffast.last.ate.time)="time last ate - fasting compliance check - CSF"
    label(csffast.last.ate.what)="what ate last - fasting compliance check - CSF"
    label(csffast.waking)="eaten since waking up - fasting compliance check - CSF"
    label(csf.physician)="physician - CSF"
    label(csf.nurse)="nurse - CSF"
    label(csf.lab.staff)="lab staff - CSF"
    label(csf.start.hr)="procedure start time - hour - CSF"
    label(csf.start.min)="procedure start time - minute - CSF"
    label(csf.end.hr)="procedure end time - hour - CSF"
    label(csf.end.min)="procedure end time - minute - CSF"
    label(csf.c.procnotes)="procedural notes - clinical CSF"
    label(csf.c.appear)="gross appearance- clinical CSF"
    label(csf.c.nuccells)="nucleated cells - clinical CSF"
    label(csf.c.rbc)="red blood cell count - clinical CSF"
    label(csf.c.cellcount)="cell count - clinical CSF"
    label(csf.c.lymph)="lymphocyte percentage - clinical CSF"
    label(csf.c.monomac)="monocyte/macrophage percentage - clinical CSF"
    label(csf.c.gluc)="glucose - clinical CSF"
    label(csf.c.protein)="total protein - clinical CSF"
    label(csf.c.insulin)="insulin - clinical CSF"
    label(csf.c.formnotes)="form notes - clinical CSF"
    label(clinical.cerebrospinal.fluid.complete)="Complete?"
    label(csf.c.protein) <- "total protein - clinical CSF, converted"
    label(csf.c.gluc) <- "glucose - clinical CSF, converted"
    label(csf.c.insulin) <- "insulin - clinical CSF, wrong assay used by lab"


    csffast.waking.factor = factor(csffast.waking,levels=c("1","0","-9999"))
    csf.physician.factor = factor(csf.physician,levels=c("1","2"))
    csf.nurse.factor = factor(csf.nurse,levels=c("12","22","3","5","1","13","23","4","19","6","7","20","8","9","10","14","11","2","15"))
    csf.lab.staff.factor = factor(csf.lab.staff,levels=c("4","3","2","1","6","5"))
    clinical.cerebrospinal.fluid.complete.factor = factor(clinical.cerebrospinal.fluid.complete,levels=c("0","1","2"))

    levels(csffast.waking.factor)=c("Yes","No","missing")
    levels(csf.physician.factor)=c("Susan Bell","Leah Acosta")
    levels(csf.nurse.factor)=c("Ben Small","Christa Hedstrom","Crystal Rice","Hal Bowman","Debbie King (Debbie Ragsdale)","Debra Poplar","Deloris Lee","Diane Anders","Lamar Bowman","Lana Howard","Lisa Sposa","Lori Quintana","Melissa Lehman","Robin Perkins","Samantha Saalwaechter","Sheila Beavers","Sherri Hails","Timothy Smith","Missing")
    levels(csf.lab.staff.factor)=c("Elleena Benson","Laura Fritzsche","Laura Logan","Melanie Leslie","Ray Romano","Missing")
    levels(clinical.cerebrospinal.fluid.complete.factor)=c("Incomplete","Unverified","Complete")
  })

  return(data)
}
