#' Derive, label, and add manual 3T variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added manual 3T variables.
#' @export

derive_manual_3T <- function(data) {

  data <- within(data, {

    pvs.edit.pat.basal.ganglia.factor <- ifelse(is.na(pvs.edit.pat.basal.ganglia), NA, ifelse(pvs.edit.pat.basal.ganglia <= 2, 0, 1))
    pvs.edit.pat.basal.ganglia.factor <- factor(pvs.edit.pat.basal.ganglia.factor, levels = c(0, 1), labels = c("Low", "High"))
    label(pvs.edit.pat.basal.ganglia.factor) <- paste0(label(pvs.edit.pat.basal.ganglia), ", dichotomized [0-2]vs.[3-5]")

    pvs.edit.pat.centrum.factor <- ifelse(is.na(pvs.edit.pat.centrum), NA, ifelse(pvs.edit.pat.centrum <= 1, 0, 1))
    pvs.edit.pat.centrum.factor <- factor(pvs.edit.pat.centrum.factor, levels = c(0, 1), labels = c("Low", "High"))
    label(pvs.edit.pat.centrum.factor) <- paste0(label(pvs.edit.pat.centrum), ", dichotomized [0-1]vs.[2+]")

    pvs.edit.han.basal.ganglia.factor <- ifelse(is.na(pvs.edit.han.basal.ganglia), NA, ifelse(pvs.edit.han.basal.ganglia <= 1, 0, 1))
    pvs.edit.han.basal.ganglia.factor <- factor(pvs.edit.han.basal.ganglia.factor, levels = c(0, 1), labels = c("Low", "High"))
    label(pvs.edit.han.basal.ganglia.factor) <- paste0(label(pvs.edit.han.basal.ganglia), ", dichotomized [0-1]vs.[2+]")

    pvs.edit.han.centrum.factor <- ifelse(is.na(pvs.edit.han.centrum <= 1), NA, ifelse(pvs.edit.han.centrum <= 1, 0, 1))
    pvs.edit.han.centrum.factor <- factor(pvs.edit.han.centrum.factor, levels = c(0, 1), labels = c("Low", "High"))
    label(pvs.edit.han.centrum.factor) <- paste0(label(pvs.edit.han.centrum), ", dichotomized [0-1]vs.[2+]")

    pvs.edit.han.total.factor <- ifelse(is.na(pvs.edit.han.total), NA, ifelse(pvs.edit.han.total <= 3, 0, 1))
    pvs.edit.han.total.factor <- factor(pvs.edit.han.total.factor, levels = c(0, 1), labels = c("Low", "High"))
    label(pvs.edit.han.total.factor) <- paste0(label(pvs.edit.han.total), ", dichotomized [0-3]vs.[4+]")

    swi.microbleeds.number.orig <- swi.microbleeds.number
    swi.microbleeds.number <- as.character(swi.microbleeds.number)
    swi.microbleeds.number <- ifelse(swi.microbleeds.number == "", NA, swi.microbleeds.number)
    swi.microbleeds.number <- ifelse(swi.microbleeds.number == "-9999", NA, swi.microbleeds.number)

    swi.microbleeds.number <- gsub("+", "", swi.microbleeds.number, fixed = TRUE)
    swi.microbleeds.number <- as.numeric(swi.microbleeds.number)
    label(swi.microbleeds.number) <- paste0(label(swi.microbleeds.number.orig), ", recoded")

    swi.microbleeds.number.factor <- ifelse(is.na(swi.microbleeds.number), NA, ifelse(swi.microbleeds.number < 1, 0, 1))
    swi.microbleeds.number.factor <- factor(swi.microbleeds.number.factor, levels = c(0, 1), labels = c("No", "Yes"))
    label(swi.microbleeds.number.factor) <- "Microbleeds Present"

    lacunar.infarcts.number.factor=ifelse(is.na(lacunar.infarcts.number), NA, ifelse(lacunar.infarcts.number < 1, 0, 1))
    lacunar.infarcts.number.factor<-factor(lacunar.infarcts.number.factor, levels = c(0, 1), labels = c("No", "Yes"))
    label(lacunar.infarcts.number.factor) <- 'Lacunar Infarcts Present'

    #VWI Derivation
    vwi.right.ica.thick <- (vwi.right.ica.od - vwi.right.ica.id)/2
    vwi.left.ica.thick  <- (vwi.left.ica.od  - vwi.left.ica.id)/2
    vwi.ica.thick       <- (vwi.right.ica.thick + vwi.left.ica.thick)/2
    vwi.right.aca.thick <- (vwi.right.aca.od - vwi.right.aca.id)/2
    vwi.left.aca.thick  <- (vwi.left.aca.od  - vwi.left.aca.id)/2
    vwi.aca.thick       <- (vwi.right.aca.thick + vwi.left.aca.thick)/2
    vwi.right.mca.thick <- (vwi.right.mca.od - vwi.right.mca.id)/2
    vwi.left.mca.thick  <- (vwi.left.mca.od  - vwi.left.mca.id)/2
    vwi.mca.thick       <- (vwi.right.mca.thick + vwi.left.mca.thick)/2
    vwi.vb.thick        <- (vwi.vb.od - vwi.vb.id)/2

    vwi.ica.id                <- (vwi.right.ica.id + vwi.left.ica.id) / 2
    vwi.aca.id                <- (vwi.right.aca.id + vwi.left.aca.id) / 2
    vwi.mca.id                <- (vwi.right.mca.id + vwi.left.mca.id) / 2

    label(vwi.right.ica.thick) <- 'Right ICA total wall thickness, mm'
    label(vwi.left.ica.thick)  <- 'Left ICA total wall thickness, mm'
    label(vwi.ica.thick)       <- 'Mean ICA total wall thickness, mm'
    label(vwi.right.aca.thick) <- 'Right ACA total wall thickness, mm'
    label(vwi.left.aca.thick)  <- 'Left ACA total wall thickness, mm'
    label(vwi.aca.thick)       <- 'Mean ACA total wall thickness, mm'
    label(vwi.right.mca.thick) <- 'Right MCA total wall thickness, mm'
    label(vwi.left.mca.thick)  <- 'Left MCA total wall thickness, mm'
    label(vwi.mca.thick)       <- 'Mean MCA total wall thickness, mm'
    label(vwi.vb.thick)        <- 'VB total wall thickness, mm'
    label(vwi.ica.id)                <- 'Mean ICA inner diameter, mm'
    label(vwi.aca.id)                <- 'Mean ACA inner diameter, mm'
    label(vwi.mca.id)                <- 'Mean MCA inner diameter, mm'

    ica.right.id.ratio <- vwi.right.ica.id / vwi.right.ica.od
    ica.left.id.ratio  <- vwi.left.ica.id  / vwi.left.ica.od
    aca.right.id.ratio <- vwi.right.aca.id / vwi.right.aca.od
    aca.left.id.ratio  <- vwi.left.aca.id  / vwi.left.aca.od
    mca.right.id.ratio <- vwi.right.mca.id / vwi.right.mca.od
    mca.left.id.ratio  <- vwi.left.mca.id  / vwi.left.mca.od
    vb.id.ratio        <- vwi.vb.id / vwi.vb.od

    label(ica.right.id.ratio) <- 'Right ICA ID ratio'
    label(ica.left.id.ratio)  <- 'Left ICA ID ratio'
    label(aca.right.id.ratio) <- 'Right ACA ID ratio'
    label(aca.left.id.ratio)  <- 'Left ACA ID ratio'
    label(mca.right.id.ratio) <- 'Right MCA ID ratio'
    label(mca.left.id.ratio)  <- 'Left MCA ID ratio'
    label(vb.id.ratio)        <- 'VB ID ratio'

    ica.right.wall.ratio <- (vwi.right.ica.od - vwi.right.ica.id) / vwi.right.ica.od
    ica.left.wall.ratio  <- (vwi.left.ica.od  - vwi.left.ica.id ) / vwi.left.ica.od
    aca.right.wall.ratio <- (vwi.right.aca.od - vwi.right.aca.id) / vwi.right.aca.od
    aca.left.wall.ratio  <- (vwi.left.aca.od  - vwi.left.aca.id ) / vwi.left.aca.od
    mca.right.wall.ratio <- (vwi.right.mca.od - vwi.right.mca.id) / vwi.right.mca.od
    mca.left.wall.ratio  <- (vwi.left.mca.od  - vwi.left.mca.id ) / vwi.left.mca.od
    vb.wall.ratio        <- (vwi.vb.od - vwi.vb.id) / vwi.vb.od

    label(ica.right.wall.ratio) <- 'Right ICA wall ratio'
    label(ica.left.wall.ratio)  <- 'Left ICA wall ratio'
    label(aca.right.wall.ratio) <- 'Right ACA wall ratio'
    label(aca.left.wall.ratio)  <- 'Left ACA wall ratio'
    label(mca.right.wall.ratio) <- 'Right MCA wall ratio'
    label(mca.left.wall.ratio)  <- 'Left MCA wall ratio'
    label(vb.wall.ratio)        <- 'VB wall ratio'

    # New PVS variable derivation
    pvs.edit.count.basal.ganglia.plus.1.log <- log1p(pvs.edit.count.basal.ganglia)
    pvs.edit.volume.basal.ganglia.standardized.plus.1.log <- log1p(pvs.edit.volume.basal.ganglia.standardized)
    pvs.edit.count.caudate.plus.1.log <- log1p(pvs.edit.count.caudate)
    pvs.edit.volume.caudate.standardized.plus.1.log <- log1p(pvs.edit.volume.caudate.standardized)
    pvs.edit.count.putamen.plus.1.log <- log1p(pvs.edit.count.putamen)
    pvs.edit.volume.putamen.standardized.plus.1.log <- log1p(pvs.edit.volume.putamen.standardized)
    pvs.edit.count.pallidum.plus.1.log <- log1p(pvs.edit.count.pallidum)
    pvs.edit.volume.pallidum.standardized.plus.1.log <- log1p(pvs.edit.volume.pallidum.standardized)
    pvs.edit.count.basal.ganglia.upper.plus.1.log <- log1p(pvs.edit.count.basal.ganglia.upper)
    pvs.edit.volume.basal.ganglia.upper.standardized.plus.1.log <- log1p(pvs.edit.volume.basal.ganglia.upper.standardized)
    pvs.edit.count.basal.ganglia.lower.plus.1.log <- log1p(pvs.edit.count.basal.ganglia.lower)
    pvs.edit.volume.basal.ganglia.lower.standardized.plus.1.log <- log1p(pvs.edit.volume.basal.ganglia.lower.standardized)

    label(pvs.edit.count.basal.ganglia.plus.1.log) <- "Log of pvs.edit.count.basal.ganglia + 1"
    label(pvs.edit.volume.basal.ganglia.standardized.plus.1.log) <- "Log of pvs.edit.volume.basal.ganglia.standardized + 1"
    label(pvs.edit.count.caudate.plus.1.log) <- "Log of pvs.edit.count.caudate + 1"
    label(pvs.edit.volume.caudate.standardized.plus.1.log) <- "Log of pvs.edit.volume.caudate.standardized + 1"
    label(pvs.edit.count.putamen.plus.1.log) <- "Log of pvs.edit.count.putamen + 1"
    label(pvs.edit.volume.putamen.standardized.plus.1.log) <- "Log of pvs.edit.volume.putamen.standardized + 1"
    label(pvs.edit.count.pallidum.plus.1.log) <- "Log of pvs.edit.count.pallidum + 1"
    label(pvs.edit.volume.pallidum.standardized.plus.1.log) <- "Log of pvs.edit.volume.pallidum.standardized + 1"
    label(pvs.edit.count.basal.ganglia.upper.plus.1.log) <- "Log of pvs.edit.count.basal.ganglia.upper + 1"
    label(pvs.edit.volume.basal.ganglia.upper.standardized.plus.1.log) <- "Log of pvs.edit.volume.basal.ganglia.upper.standardized + 1"
    label(pvs.edit.count.basal.ganglia.lower.plus.1.log) <- "Log of pvs.edit.count.basal.ganglia.lower + 1"
    label(pvs.edit.volume.basal.ganglia.lower.standardized.plus.1.log) <- "Log of pvs.edit.volume.basal.ganglia.lower.standardized + 1"
  })

  return(data)
}
