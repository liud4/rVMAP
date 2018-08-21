# Split off from main cmr function 19 Sep 2016 (LS)

# 19 Sep 2016, LS: added epoch-specificity

invalidCmr <- function(dat){
  qmass.exclude <- Hmisc::Cs(
    qmass.lv.ed.volume,
    qmass.lv.es.volume,
    qmass.lv.stroke.volume,
    qmass.lv.cardiac.output,
    qmass.lv.ejection.fraction,
    qmass.lv.mass,
    qmass.lv.ejection.peak.ejection.rate,
    qmass.lv.time.to.peak.ejection.rate,
    qmass.lv.peak.ejection.rate,
    qmass.lv.time.to.peak.filling.rate,
    qmass.rv.ed.volume,
    qmass.rv.es.volume,
    qmass.rv.stroke.volume,
    qmass.rv.cardiac.output,
    qmass.rv.ejection.fraction,
    qmass.rv.mass,
    qmass.rv.ejection.peak.ejection.rate,
    qmass.rv.time.to.peak.ejection.rate,
    qmass.rv.peak.ejection.rate,
    qmass.rv.time.to.peak.filling.rate
  )

  tomtec.exclude <- Hmisc::Cs(
    tomtec.ejection.fraction,
    tomtec.ed.volume,
    tomtec.es.volume,
    tomtec.stroke.volume,
    tomtec.lv.mass,
    tomtec.global.longitudinal.strain,
    t2p.transverse.strain.basal.septal,
    v2p.transverse.strain.basal.septal,
    t2p.transverse.strain.mid.septal,
    v2p.transverse.strain.mid.septal,
    t2p.transverse.strain.apical.septal,
    v2p.transverse.strain.apical.septal,
    t2p.transverse.strain.apical.latera,
    v2p.transverse.strain.apical.lateral,
    t2p.transverse.strain.mid.lateral,
    v2p.transverse.strain.mid.lateral,
    t2p.transverse.strain.basal.lateral,
    v2p.transverse.strain.basal.lateral,
    t2p.transverse.strain.average,
    v2p.transverse.strain.average,
    t2p.longitudinal.strain.endo.basal.septal,
    v2p.longitudinal.strain.endo.basal.septal,
    t2p.longitudinal.strain.endo.mid.septal,
    v2p.longitudinal.strain.endo.mid.septal,
    t2p.longitudinal.strain.endo.apical.septal,
    v2p.longitudinal.strain.endo.apical.septal,
    t2p.longitudinal.strain.endo.apical.lateral,
    v2p.longitudinal.strain.endo.apical.lateral,
    t2p.longitudinal.strain.endo.mid.lateral,
    v2p.longitudinal.strain.endo.mid.lateral,
    t2p.longitudinal.strain.endo.basal.lateral,
    v2p.longitudinal.strain.endo.basal.lateral,
    t2p.longitudinal.strain.endo.average,
    v2p.longitudinal.strain.endo.average,
    t2p.longitudinal.strain.epi.basal.septal,
    v2p.longitudinal.strain.epi.basal.septal,
    t2p.longitudinal.strain.epi.mid.septal,
    v2p.longitudinal.strain.epi.mid.septal,
    t2p.longitudinal.strain.epi.apical.septal,
    v2p.longitudinal.strain.epi.apical.septal,
    t2p.longitudinal.strain.epi.apical.lateral,
    v2p.longitudinal.strain.epi.apical.lateral,
    t2p.longitudinal.strain.epi.mid.lateral,
    v2p.longitudinal.strain.epi.mid.lateral,
    t2p.longitudinal.strain.epi.basal.lateral,
    v2p.longitudinal.strain.epi.basal.lateral,
    t2p.longitudinal.strain.epi.average,
    v2p.longitudinal.strain.epi.average,
    t2p.transverse.strain.rate.basal.septal,
    v2p.transverse.strain.rate.basal.septal,
    t2p.transverse.strain.rate.mid.septal,
    v2p.transverse.strain.rate.mid.septal,
    t2p.transverse.strain.rate.apical.septal,
    v2p.transverse.strain.rate.apical.septal,
    t2p.transverse.strain.rate.apical.lateral,
    v2p.transverse.strain.rate.apical.lateral,
    t2p.transverse.strain.rate.mid.lateral,
    v2p.transverse.strain.rate.mid.lateral,
    t2p.transverse.strain.rate.basal.lateral,
    v2p.transverse.strain.rate.basal.lateral,
    t2p.transverse.strain.rate.average,
    v2p.transverse.strain.rate.average,
    t2p.longitudinal.strain.endo.rate.basal.septal,
    v2p.longitudinal.strain.rate.endo.basal.septal,
    t2p.longitudinal.strain.rate.endo.mid.septal,
    v2p.longitudinal.strain.rate.endo.mid.septal,
    t2p.longitudinal.strain.rate.endo.apical.septal,
    v2p.longitudinal.strain.rate.endo.apical.septal,
    t2p.longitudinal.strain.rate.endo.apical.lateral,
    v2p.longitudinal.strain.rate.endo.apical.lateral,
    t2p.longitudinal.strain.rate.endo.mid.lateral,
    v2p.longitudinal.strain.rate.endo.mid.lateral,
    t2p.longitudinal.strain.rate.endo.basal.lateral,
    v2p.longitudinal.strain.rate.endo.basal.lateral,
    t2p.longitudinal.strain.rate.endo.average,
    v2p.longitudinal.strain.rate.endo.average,
    t2p.longitudinal.strain.rate.epi.basal.septal,
    v2p.longitudinal.strain.rate.epi.basal.septal,
    t2p.longitudinal.strain.rate.epi.mid.septal,
    v2p.longitudinal.strain.rate.epi.mid.septal,
    t2p.longitudinal.strain.rate.epi.apical.septal,
    v2p.longitudinal.strain.rate.epi.apical.septal,
    t2p.longitudinal.strain.rate.epi.apical.lateral,
    v2p.longitudinal.strain.rate.epi.apical.lateral,
    t2p.longitudinal.strain.rate.epi.mid.lateral,
    v2p.longitudinal.strain.rate.epi.mid.lateral,
    t2p.longitudinal.strain.rate.epi.basal.lateral,
    v2p.longitudinal.strain.rate.epi.basal.lateral,
    t2p.longitudinal.strain.rate.epi.average,
    v2p.longitudinal.strain.rate.epi.average
  )
  #global..circumferential.strain)

  # Epoch 1
  ids.qmass.tomtec <- c("005","037","040","068","071","157","159","161","185","186","190","198","300","316")
  dat[dat$map.id %in% ids.qmass.tomtec & dat$epoch == 1, c(qmass.exclude, tomtec.exclude)] <- NA

  ids.tomtec <- c("034","035","073","083","086","096","102","119","131","140","162","171","184","211","247","272","280","294","298","314")
  dat[dat$map.id %in% ids.tomtec & dat$epoch == 1, tomtec.exclude] <- NA

  ids.qmass <- c("316")
  dat[dat$map.id %in% ids.qmass & dat$epoch == 1, qmass.exclude] <- NA


  dat
}
