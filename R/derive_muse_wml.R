#' Derive, label, and add DLMUSE white mattervolume variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added DLMUSE volume variables.
#' @export

derive_muse_wml <- function(data) {

  # create muse white matter volume
  data.new <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      wml.muse.wm.fron.vol = sum(wml.muse.r.fron.wm.vol, wml.muse.l.fron.wm.vol),
      wml.muse.wm.par.vol = sum(wml.muse.r.par.wm.vol, wml.muse.l.par.wm.vol),
      wml.muse.wm.temp.vol = sum(wml.muse.r.temp.wm.vol, wml.muse.l.temp.wm.vol),
      wml.muse.wm.occ.vol = sum(wml.muse.r.occ.wm.vol, wml.muse.l.occ.wm.vol),
      wml.muse.wm.deep.vol = sum(wml.muse.r.deep.wm.vol, wml.muse.l.deep.wm.vol),
      wml.muse.wm.cc.vol = sum(wml.muse.r.cc.vol, wml.muse.l.cc.vol),
      wml.muse.juxcort.fron.vol = sum(wml.muse.r.fron.juxcort.vol, wml.muse.l.fron.juxcort.vol),
      wml.muse.juxcort.par.vol = sum(wml.muse.r.par.juxcort.vol, wml.muse.l.par.juxcort.vol),
      wml.muse.juxcort.temp.vol = sum(wml.muse.r.temp.juxcort.vol, wml.muse.l.temp.juxcort.vol),
      wml.muse.juxcort.occ.vol = sum(wml.muse.r.occ.juxcort.vol, wml.muse.l.occ.juxcort.vol),
      wml.muse.juxcort.deep.vol = sum(wml.muse.r.deep.juxcort.vol, wml.muse.l.deep.juxcort.vol),
      wml.muse.perivent.fron.vol = sum(wml.muse.r.fron.perivent.vol, wml.muse.l.fron.perivent.vol),
      wml.muse.perivent.par.vol = sum(wml.muse.r.par.perivent.vol, wml.muse.l.par.perivent.vol),
      wml.muse.perivent.temp.vol = sum(wml.muse.r.temp.perivent.vol, wml.muse.l.temp.perivent.vol),
      wml.muse.perivent.occ.vol = sum(wml.muse.r.occ.perivent.vol, wml.muse.l.occ.perivent.vol),
      wml.muse.perivent.deep.vol = sum(wml.muse.r.deep.perivent.vol, wml.muse.l.deep.perivent.vol),
      wml.muse.fron.vol = sum(wml.muse.wm.fron.vol,
                              wml.muse.juxcort.fron.vol,
                              wml.muse.perivent.fron.vol),
      wml.muse.par.vol = sum(wml.muse.wm.par.vol,
                             wml.muse.juxcort.par.vol,
                             wml.muse.perivent.par.vol),
      wml.muse.temp.vol = sum(wml.muse.wm.temp.vol,
                              wml.muse.juxcort.temp.vol,
                              wml.muse.perivent.temp.vol),
      wml.muse.occ.vol = sum(wml.muse.wm.occ.vol,
                             wml.muse.juxcort.occ.vol,
                             wml.muse.perivent.occ.vol),
      wml.muse.deep.vol = sum(wml.muse.wm.deep.vol,
                              wml.muse.juxcort.deep.vol,
                              wml.muse.perivent.deep.vol),
      wml.muse.wm.vol = sum(
        wml.muse.r.fron.wm.vol,
        wml.muse.l.fron.wm.vol,
        wml.muse.r.par.wm.vol,
        wml.muse.l.par.wm.vol,
        wml.muse.r.temp.wm.vol,
        wml.muse.l.temp.wm.vol,
        wml.muse.r.occ.wm.vol,
        wml.muse.l.occ.wm.vol,
        wml.muse.r.deep.wm.vol,
        wml.muse.l.deep.wm.vol,
        wml.muse.r.cc.vol,
        wml.muse.l.cc.vol
      ),
      wml.muse.juxcort.vol = sum(
        wml.muse.r.fron.juxcort.vol,
        wml.muse.l.fron.juxcort.vol,
        wml.muse.r.par.juxcort.vol,
        wml.muse.l.par.juxcort.vol,
        wml.muse.r.temp.juxcort.vol,
        wml.muse.l.temp.juxcort.vol,
        wml.muse.r.occ.juxcort.vol,
        wml.muse.l.occ.juxcort.vol,
        wml.muse.r.deep.juxcort.vol,
        wml.muse.l.deep.juxcort.vol
      ),
      wml.muse.perivent.vol = sum(
        wml.muse.r.fron.perivent.vol,
        wml.muse.l.fron.perivent.vol,
        wml.muse.r.par.perivent.vol,
        wml.muse.l.par.perivent.vol,
        wml.muse.r.temp.perivent.vol,
        wml.muse.l.temp.perivent.vol,
        wml.muse.r.occ.perivent.vol,
        wml.muse.l.occ.perivent.vol,
        wml.muse.r.deep.perivent.vol,
        wml.muse.l.deep.perivent.vol
      ),
      wml.muse.vol = sum(
        wml.muse.r.fron.wm.vol,
        wml.muse.l.fron.wm.vol,
        wml.muse.r.par.wm.vol,
        wml.muse.l.par.wm.vol,
        wml.muse.r.temp.wm.vol,
        wml.muse.l.temp.wm.vol,
        wml.muse.r.occ.wm.vol,
        wml.muse.l.occ.wm.vol,
        wml.muse.r.deep.wm.vol,
        wml.muse.l.deep.wm.vol,
        wml.muse.r.cc.vol,
        wml.muse.l.cc.vol,
        wml.muse.r.fron.juxcort.vol,
        wml.muse.l.fron.juxcort.vol,
        wml.muse.r.par.juxcort.vol,
        wml.muse.l.par.juxcort.vol,
        wml.muse.r.temp.juxcort.vol,
        wml.muse.l.temp.juxcort.vol,
        wml.muse.r.occ.juxcort.vol,
        wml.muse.l.occ.juxcort.vol,
        wml.muse.r.deep.juxcort.vol,
        wml.muse.l.deep.juxcort.vol,
        wml.muse.r.fron.perivent.vol,
        wml.muse.l.fron.perivent.vol,
        wml.muse.r.par.perivent.vol,
        wml.muse.l.par.perivent.vol,
        wml.muse.r.temp.perivent.vol,
        wml.muse.l.temp.perivent.vol,
        wml.muse.r.occ.perivent.vol,
        wml.muse.l.occ.perivent.vol,
        wml.muse.r.deep.perivent.vol,
        wml.muse.l.deep.perivent.vol
      ), 
      wml.muse.fron.vol.cm = wml.muse.fron.vol / 1000,
      wml.muse.par.vol.cm = wml.muse.par.vol / 1000,
      wml.muse.temp.vol.cm = wml.muse.temp.vol / 1000,
      wml.muse.occ.vol.cm = wml.muse.occ.vol / 1000,
      wml.muse.vol.cm = wml.muse.vol / 1000,
      wml.muse.fron.vol.cm.plus.1.log = log(wml.muse.fron.vol.cm + 1),
      wml.muse.par.vol.cm.plus.1.log = log(wml.muse.par.vol.cm + 1),
      wml.muse.temp.vol.cm.plus.1.log = log(wml.muse.temp.vol.cm + 1),
      wml.muse.occ.vol.cm.plus.1.log = log(wml.muse.occ.vol.cm + 1),
      wml.muse.vol.cm.plus.1.log = log(wml.muse.vol.cm + 1)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  data.new <- within(data.new, {
    Hmisc::label(wml.muse.wm.fron.vol) = "White matter lesion volume - Frontal Lobe WM (mm3)"
    Hmisc::label(wml.muse.wm.par.vol) = "White matter lesion volume - Parietal Lobe WM (mm3)"
    Hmisc::label(wml.muse.wm.temp.vol) = "White matter lesion volume - Temporal Lobe WM (mm3)"
    Hmisc::label(wml.muse.wm.occ.vol) = "White matter lesion volume - Occipital Lobe WM (mm3)"
    Hmisc::label(wml.muse.wm.deep.vol) = "White matter lesion volume - Deep White Matter WM (mm3)"
    Hmisc::label(wml.muse.wm.cc.vol) = "White matter lesion volume - Corpus Callosum WM (mm3)"
    Hmisc::label(wml.muse.juxcort.fron.vol) = "White matter lesion volume - Juxtacortical Frontal Lobe (mm3)"
    Hmisc::label(wml.muse.juxcort.par.vol) = "White matter lesion volume - Juxtacortical Parietal Lobe (mm3)"
    Hmisc::label(wml.muse.juxcort.temp.vol) = "White matter lesion volume - Juxtacortical Temporal Lobe (mm3)"
    Hmisc::label(wml.muse.juxcort.occ.vol) = "White matter lesion volume - Juxtacortical Occipital Lobe (mm3)"
    Hmisc::label(wml.muse.juxcort.deep.vol) = "White matter lesion volume - Juxtacortical Deep White Matter (mm3)"
    Hmisc::label(wml.muse.perivent.fron.vol) = "White matter lesion volume - Periventricular Frontal Lobe (mm3)"
    Hmisc::label(wml.muse.perivent.par.vol) = "White matter lesion volume - Periventricular Parietal Lobe (mm3)"
    Hmisc::label(wml.muse.perivent.temp.vol) = "White matter lesion volume - Periventricular Temporal Lobe (mm3)"
    Hmisc::label(wml.muse.perivent.occ.vol) = "White matter lesion volume - Periventricular Occipital Lobe (mm3)"
    Hmisc::label(wml.muse.perivent.deep.vol) = "White matter lesion volume - Periventricular Deep White Matter (mm3)"
    Hmisc::label(wml.muse.fron.vol) = "White matter lesion volume - Frontal Lobe (mm3)"
    Hmisc::label(wml.muse.par.vol) = "White matter lesion volume - Parietal Lobe (mm3)"
    Hmisc::label(wml.muse.temp.vol) = "White matter lesion volume - Temporal Lobe (mm3)"
    Hmisc::label(wml.muse.occ.vol) = "White matter lesion volume - Occipital Lobe (mm3)"
    Hmisc::label(wml.muse.deep.vol) = "White matter lesion volume - Deep White Matter (mm3)"
    Hmisc::label(wml.muse.wm.vol) = "White matter lesion volume - White Matter Total (mm3)"
    Hmisc::label(wml.muse.juxcort.vol) = "White matter lesion volume - Juxtacortical Total (mm3)"
    Hmisc::label(wml.muse.perivent.vol) = "White matter lesion volume - Periventricular Total (mm3)"
    Hmisc::label(wml.muse.vol) = "White matter lesion volume - Total Brainmask (mm3)"
    Hmisc::label(wml.muse.fron.vol.cm) = "White matter lesion volume - Frontal Lobe (cm3)"
    Hmisc::label(wml.muse.par.vol.cm) = "White matter lesion volume - Parietal Lobe (cm3)"
    Hmisc::label(wml.muse.temp.vol.cm) = "White matter lesion volume - Temporal Lobe (cm3)"
    Hmisc::label(wml.muse.occ.vol.cm) = "White matter lesion volume - Occipital Lobe (cm3)"
    Hmisc::label(wml.muse.vol.cm) = "White matter lesion volume - Total Brainmask (cm3)"
    Hmisc::label(wml.muse.fron.vol.cm.plus.1.log) = "White matter lesion volume - Frontal Lobe (log-transformed)"
    Hmisc::label(wml.muse.par.vol.cm.plus.1.log) = "White matter lesion volume - Parietal Lobe (log-transformed)"
    Hmisc::label(wml.muse.temp.vol.cm.plus.1.log) = "White matter lesion volume - Temporal Lobe (log-transformed)"
    Hmisc::label(wml.muse.occ.vol.cm.plus.1.log) = "White matter lesion volume - Occipital Lobe (log-transformed)"
    Hmisc::label(wml.muse.vol.cm.plus.1.log) = "White matter lesion volume - Total Brainmask (log-transformed)"
  })
  
  return(data.new)
}
