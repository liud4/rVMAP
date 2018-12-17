#' Derive, label, and add automated 3T variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added automated 3T variables.
#' @export

derive_automated_3T <- function(data) {

  data$icv <- rowSums(data[, Cs(vbmqa.gm.vol, vbmqa.wm.vol, vbmqa.csf.vol)])
  label(data$icv) <- "ICV (calculated)"

  data$bHold.icv <- rowSums(data[, Cs(bHold.vbmqa.gm.vol, bHold.vbmqa.wm.vol, bHold.vbmqa.csf.vol)])
  label(data$bHold.icv) <- "ICV (calculated)"

  data <- within(data, {
    wml.volume.plus.1.log <- log(wml.volume + 1)
    label(wml.volume.plus.1.log) <- "Log of wml.volume + 1"

    bHold.wml.volume.plus.1.log <- log(bHold.wml.volume + 1)
    label(bHold.wml.volume.plus.1.log) <- "Log of wml.volume + 1"

    asl.3t.rest.etco2 <- (asl.3t.bl.etco2.1 + asl.3t.bl.etco2.2 + asl.3t.bl.etco2.3) / 3
    label(asl.3t.rest.etco2) <- "Resting EtCO2"

    asl.3t.chall.etco2 <- (asl.3t.hyper.etco2.1 + asl.3t.hyper.etco2.2 + asl.3t.hyper.etco2.3) / 3
    label(asl.3t.chall.etco2) <- "Challenge EtCO2"

    asl.3t.change.etco2 <- asl.3t.chall.etco2 - asl.3t.rest.etco2
    label(asl.3t.change.etco2) <- "Change in EtCO2"

    # Derive ASL reactivity variables (OAK 20181217)

    asl.reac.grey.matter.hct <- 100 * (asl.chall.grey.matter.hct - asl.rest.grey.matter.hct) / asl.rest.grey.matter.hct / asl.3t.change.etco2
    asl.reac.left.hemisphere.hct <- 100 * (asl.chall.left.hemisphere.hct - asl.rest.left.hemisphere.hct) / asl.rest.left.hemisphere.hct / asl.3t.change.etco2
    asl.reac.right.hemisphere.hct <- 100 * (asl.chall.right.hemisphere.hct - asl.rest.right.hemisphere.hct) / asl.rest.right.hemisphere.hct / asl.3t.change.etco2
    asl.reac.right.frontal.lobe.hct <- 100 * (asl.chall.right.frontal.lobe.hct - asl.rest.right.frontal.lobe.hct) / asl.rest.right.frontal.lobe.hct / asl.3t.change.etco2
    asl.reac.left.frontal.lobe.hct <- 100 * (asl.chall.left.frontal.lobe.hct - asl.rest.left.frontal.lobe.hct) / asl.rest.left.frontal.lobe.hct / asl.3t.change.etco2
    asl.reac.frontal.lobe.hct <- 100 * (asl.chall.frontal.lobe.hct - asl.rest.frontal.lobe.hct) / asl.rest.frontal.lobe.hct / asl.3t.change.etco2
    asl.reac.right.occipital.lobe.hct <- 100 * (asl.chall.right.occipital.lobe.hct - asl.rest.right.occipital.lobe.hct) / asl.rest.right.occipital.lobe.hct / asl.3t.change.etco2
    asl.reac.left.occipital.lobe.hct <- 100 * (asl.chall.left.occipital.lobe.hct - asl.rest.left.occipital.lobe.hct) / asl.rest.left.occipital.lobe.hct / asl.3t.change.etco2
    asl.reac.occipital.lobe.hct <- 100 * (asl.chall.occipital.lobe.hct - asl.rest.occipital.lobe.hct) / asl.rest.occipital.lobe.hct / asl.3t.change.etco2
    asl.reac.right.temporal.lobe.hct <- 100 * (asl.chall.right.temporal.lobe.hct - asl.rest.right.temporal.lobe.hct) / asl.rest.right.temporal.lobe.hct / asl.3t.change.etco2
    asl.reac.left.temporal.lobe.hct <- 100 * (asl.chall.left.temporal.lobe.hct - asl.rest.left.temporal.lobe.hct) / asl.rest.left.temporal.lobe.hct / asl.3t.change.etco2
    asl.reac.temporal.lobe.hct <- 100 * (asl.chall.temporal.lobe.hct - asl.rest.temporal.lobe.hct) / asl.rest.temporal.lobe.hct / asl.3t.change.etco2
    asl.reac.right.parietal.lobe.hct <- 100 * (asl.chall.right.parietal.lobe.hct - asl.rest.right.parietal.lobe.hct) / asl.rest.right.parietal.lobe.hct / asl.3t.change.etco2
    asl.reac.left.parietal.lobe.hct <- 100 * (asl.chall.left.parietal.lobe.hct - asl.rest.left.parietal.lobe.hct) / asl.rest.left.parietal.lobe.hct / asl.3t.change.etco2
    asl.reac.parietal.lobe.hct <- 100 * (asl.chall.parietal.lobe.hct - asl.rest.parietal.lobe.hct) / asl.rest.parietal.lobe.hct / asl.3t.change.etco2
    asl.reac.3rd.ventricle.hct <- 100 * (asl.chall.3rd.ventricle.hct - asl.rest.3rd.ventricle.hct) / asl.rest.3rd.ventricle.hct / asl.3t.change.etco2
    asl.reac.4th.ventricle.hct <- 100 * (asl.chall.4th.ventricle.hct - asl.rest.4th.ventricle.hct) / asl.rest.4th.ventricle.hct / asl.3t.change.etco2
    asl.reac.right.accumbens.area.hct <- 100 * (asl.chall.right.accumbens.area.hct - asl.rest.right.accumbens.area.hct) / asl.rest.right.accumbens.area.hct / asl.3t.change.etco2
    asl.reac.left.accumbens.area.hct <- 100 * (asl.chall.left.accumbens.area.hct - asl.rest.left.accumbens.area.hct) / asl.rest.left.accumbens.area.hct / asl.3t.change.etco2
    asl.reac.right.amygdala.hct <- 100 * (asl.chall.right.amygdala.hct - asl.rest.right.amygdala.hct) / asl.rest.right.amygdala.hct / asl.3t.change.etco2
    asl.reac.left.amygdala.hct <- 100 * (asl.chall.left.amygdala.hct - asl.rest.left.amygdala.hct) / asl.rest.left.amygdala.hct / asl.3t.change.etco2
    asl.reac.brain.stem.hct <- 100 * (asl.chall.brain.stem.hct - asl.rest.brain.stem.hct) / asl.rest.brain.stem.hct / asl.3t.change.etco2
    asl.reac.right.caudate.hct <- 100 * (asl.chall.right.caudate.hct - asl.rest.right.caudate.hct) / asl.rest.right.caudate.hct / asl.3t.change.etco2
    asl.reac.left.caudate.hct <- 100 * (asl.chall.left.caudate.hct - asl.rest.left.caudate.hct) / asl.rest.left.caudate.hct / asl.3t.change.etco2
    asl.reac.right.cerebellum.exterior.hct <- 100 * (asl.chall.right.cerebellum.exterior.hct - asl.rest.right.cerebellum.exterior.hct) / asl.rest.right.cerebellum.exterior.hct / asl.3t.change.etco2
    asl.reac.left.cerebellum.exterior.hct <- 100 * (asl.chall.left.cerebellum.exterior.hct - asl.rest.left.cerebellum.exterior.hct) / asl.rest.left.cerebellum.exterior.hct / asl.3t.change.etco2
    asl.reac.right.cerebellum.white.matter.hct <- 100 * (asl.chall.right.cerebellum.white.matter.hct - asl.rest.right.cerebellum.white.matter.hct) / asl.rest.right.cerebellum.white.matter.hct / asl.3t.change.etco2
    asl.reac.left.cerebellum.white.matter.hct <- 100 * (asl.chall.left.cerebellum.white.matter.hct - asl.rest.left.cerebellum.white.matter.hct) / asl.rest.left.cerebellum.white.matter.hct / asl.3t.change.etco2
    asl.reac.right.cerebral.white.matter.hct <- 100 * (asl.chall.right.cerebral.white.matter.hct - asl.rest.right.cerebral.white.matter.hct) / asl.rest.right.cerebral.white.matter.hct / asl.3t.change.etco2
    asl.reac.left.cerebral.white.matter.hct <- 100 * (asl.chall.left.cerebral.white.matter.hct - asl.rest.left.cerebral.white.matter.hct) / asl.rest.left.cerebral.white.matter.hct / asl.3t.change.etco2
    asl.reac.right.hippocampus.hct <- 100 * (asl.chall.right.hippocampus.hct - asl.rest.right.hippocampus.hct) / asl.rest.right.hippocampus.hct / asl.3t.change.etco2
    asl.reac.left.hippocampus.hct <- 100 * (asl.chall.left.hippocampus.hct - asl.rest.left.hippocampus.hct) / asl.rest.left.hippocampus.hct / asl.3t.change.etco2
    asl.reac.right.inf.lat.vent.hct <- 100 * (asl.chall.right.inf.lat.vent.hct - asl.rest.right.inf.lat.vent.hct) / asl.rest.right.inf.lat.vent.hct / asl.3t.change.etco2
    asl.reac.left.inf.lat.vent.hct <- 100 * (asl.chall.left.inf.lat.vent.hct - asl.rest.left.inf.lat.vent.hct) / asl.rest.left.inf.lat.vent.hct / asl.3t.change.etco2
    asl.reac.right.lateral.ventricle.hct <- 100 * (asl.chall.right.lateral.ventricle.hct - asl.rest.right.lateral.ventricle.hct) / asl.rest.right.lateral.ventricle.hct / asl.3t.change.etco2
    asl.reac.left.lateral.ventricle.hct <- 100 * (asl.chall.left.lateral.ventricle.hct - asl.rest.left.lateral.ventricle.hct) / asl.rest.left.lateral.ventricle.hct / asl.3t.change.etco2
    asl.reac.right.pallidum.hct <- 100 * (asl.chall.right.pallidum.hct - asl.rest.right.pallidum.hct) / asl.rest.right.pallidum.hct / asl.3t.change.etco2
    asl.reac.left.pallidum.hct <- 100 * (asl.chall.left.pallidum.hct - asl.rest.left.pallidum.hct) / asl.rest.left.pallidum.hct / asl.3t.change.etco2
    asl.reac.right.putamen.hct <- 100 * (asl.chall.right.putamen.hct - asl.rest.right.putamen.hct) / asl.rest.right.putamen.hct / asl.3t.change.etco2
    asl.reac.left.putamen.hct <- 100 * (asl.chall.left.putamen.hct - asl.rest.left.putamen.hct) / asl.rest.left.putamen.hct / asl.3t.change.etco2
    asl.reac.right.thalamus.proper.hct <- 100 * (asl.chall.right.thalamus.proper.hct - asl.rest.right.thalamus.proper.hct) / asl.rest.right.thalamus.proper.hct / asl.3t.change.etco2
    asl.reac.left.thalamus.proper.hct <- 100 * (asl.chall.left.thalamus.proper.hct - asl.rest.left.thalamus.proper.hct) / asl.rest.left.thalamus.proper.hct / asl.3t.change.etco2
    asl.reac.right.ventral.dc.hct <- 100 * (asl.chall.right.ventral.dc.hct - asl.rest.right.ventral.dc.hct) / asl.rest.right.ventral.dc.hct / asl.3t.change.etco2
    asl.reac.left.ventral.dc.hct <- 100 * (asl.chall.left.ventral.dc.hct - asl.rest.left.ventral.dc.hct) / asl.rest.left.ventral.dc.hct / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.iv.hct <- 100 * (asl.chall.cerebellar.vermal.lobules.iv.hct - asl.rest.cerebellar.vermal.lobules.iv.hct) / asl.rest.cerebellar.vermal.lobules.iv.hct / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.vivii.hct <- 100 * (asl.chall.cerebellar.vermal.lobules.vivii.hct - asl.rest.cerebellar.vermal.lobules.vivii.hct) / asl.rest.cerebellar.vermal.lobules.vivii.hct / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.viiix.hct <- 100 * (asl.chall.cerebellar.vermal.lobules.viiix.hct - asl.rest.cerebellar.vermal.lobules.viiix.hct) / asl.rest.cerebellar.vermal.lobules.viiix.hct / asl.3t.change.etco2
    asl.reac.left.basal.forebrain.hct <- 100 * (asl.chall.left.basal.forebrain.hct - asl.rest.left.basal.forebrain.hct) / asl.rest.left.basal.forebrain.hct / asl.3t.change.etco2
    asl.reac.right.basal.forebrain.hct <- 100 * (asl.chall.right.basal.forebrain.hct - asl.rest.right.basal.forebrain.hct) / asl.rest.right.basal.forebrain.hct / asl.3t.change.etco2
    asl.reac.right.acgg.anterior.cingulate.gyrus.hct <- 100 * (asl.chall.right.acgg.anterior.cingulate.gyrus.hct - asl.rest.right.acgg.anterior.cingulate.gyrus.hct) / asl.rest.right.acgg.anterior.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.acgg.anterior.cingulate.gyrus.hct <- 100 * (asl.chall.left.acgg.anterior.cingulate.gyrus.hct - asl.rest.left.acgg.anterior.cingulate.gyrus.hct) / asl.rest.left.acgg.anterior.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.ains.anterior.insula.hct <- 100 * (asl.chall.right.ains.anterior.insula.hct - asl.rest.right.ains.anterior.insula.hct) / asl.rest.right.ains.anterior.insula.hct / asl.3t.change.etco2
    asl.reac.left.ains.anterior.insula.hct <- 100 * (asl.chall.left.ains.anterior.insula.hct - asl.rest.left.ains.anterior.insula.hct) / asl.rest.left.ains.anterior.insula.hct / asl.3t.change.etco2
    asl.reac.right.aorg.anterior.orbital.gyrus.hct <- 100 * (asl.chall.right.aorg.anterior.orbital.gyrus.hct - asl.rest.right.aorg.anterior.orbital.gyrus.hct) / asl.rest.right.aorg.anterior.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.aorg.anterior.orbital.gyrus.hct <- 100 * (asl.chall.left.aorg.anterior.orbital.gyrus.hct - asl.rest.left.aorg.anterior.orbital.gyrus.hct) / asl.rest.left.aorg.anterior.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.ang.angular.gyrus.hct <- 100 * (asl.chall.right.ang.angular.gyrus.hct - asl.rest.right.ang.angular.gyrus.hct) / asl.rest.right.ang.angular.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.ang.angular.gyrus.hct <- 100 * (asl.chall.left.ang.angular.gyrus.hct - asl.rest.left.ang.angular.gyrus.hct) / asl.rest.left.ang.angular.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.calc.calcarine.cortex.hct <- 100 * (asl.chall.right.calc.calcarine.cortex.hct - asl.rest.right.calc.calcarine.cortex.hct) / asl.rest.right.calc.calcarine.cortex.hct / asl.3t.change.etco2
    asl.reac.left.calc.calcarine.cortex.hct <- 100 * (asl.chall.left.calc.calcarine.cortex.hct - asl.rest.left.calc.calcarine.cortex.hct) / asl.rest.left.calc.calcarine.cortex.hct / asl.3t.change.etco2
    asl.reac.right.co.central.operculum.hct <- 100 * (asl.chall.right.co.central.operculum.hct - asl.rest.right.co.central.operculum.hct) / asl.rest.right.co.central.operculum.hct / asl.3t.change.etco2
    asl.reac.left.co.central.operculum.hct <- 100 * (asl.chall.left.co.central.operculum.hct - asl.rest.left.co.central.operculum.hct) / asl.rest.left.co.central.operculum.hct / asl.3t.change.etco2
    asl.reac.right.cun.cuneus.hct <- 100 * (asl.chall.right.cun.cuneus.hct - asl.rest.right.cun.cuneus.hct) / asl.rest.right.cun.cuneus.hct / asl.3t.change.etco2
    asl.reac.left.cun.cuneus.hct <- 100 * (asl.chall.left.cun.cuneus.hct - asl.rest.left.cun.cuneus.hct) / asl.rest.left.cun.cuneus.hct / asl.3t.change.etco2
    asl.reac.right.ent.entorhinal.area.hct <- 100 * (asl.chall.right.ent.entorhinal.area.hct - asl.rest.right.ent.entorhinal.area.hct) / asl.rest.right.ent.entorhinal.area.hct / asl.3t.change.etco2
    asl.reac.left.ent.entorhinal.area.hct <- 100 * (asl.chall.left.ent.entorhinal.area.hct - asl.rest.left.ent.entorhinal.area.hct) / asl.rest.left.ent.entorhinal.area.hct / asl.3t.change.etco2
    asl.reac.right.fo.frontal.operculum.hct <- 100 * (asl.chall.right.fo.frontal.operculum.hct - asl.rest.right.fo.frontal.operculum.hct) / asl.rest.right.fo.frontal.operculum.hct / asl.3t.change.etco2
    asl.reac.left.fo.frontal.operculum.hct <- 100 * (asl.chall.left.fo.frontal.operculum.hct - asl.rest.left.fo.frontal.operculum.hct) / asl.rest.left.fo.frontal.operculum.hct / asl.3t.change.etco2
    asl.reac.right.frp.frontal.pole.hct <- 100 * (asl.chall.right.frp.frontal.pole.hct - asl.rest.right.frp.frontal.pole.hct) / asl.rest.right.frp.frontal.pole.hct / asl.3t.change.etco2
    asl.reac.left.frp.frontal.pole.hct <- 100 * (asl.chall.left.frp.frontal.pole.hct - asl.rest.left.frp.frontal.pole.hct) / asl.rest.left.frp.frontal.pole.hct / asl.3t.change.etco2
    asl.reac.right.fug.fusiform.gyrus.hct <- 100 * (asl.chall.right.fug.fusiform.gyrus.hct - asl.rest.right.fug.fusiform.gyrus.hct) / asl.rest.right.fug.fusiform.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.fug.fusiform.gyrus.hct <- 100 * (asl.chall.left.fug.fusiform.gyrus.hct - asl.rest.left.fug.fusiform.gyrus.hct) / asl.rest.left.fug.fusiform.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.gre.gyrus.rectus.hct <- 100 * (asl.chall.right.gre.gyrus.rectus.hct - asl.rest.right.gre.gyrus.rectus.hct) / asl.rest.right.gre.gyrus.rectus.hct / asl.3t.change.etco2
    asl.reac.left.gre.gyrus.rectus.hct <- 100 * (asl.chall.left.gre.gyrus.rectus.hct - asl.rest.left.gre.gyrus.rectus.hct) / asl.rest.left.gre.gyrus.rectus.hct / asl.3t.change.etco2
    asl.reac.right.iog.inferior.occipital.gyrus.hct <- 100 * (asl.chall.right.iog.inferior.occipital.gyrus.hct - asl.rest.right.iog.inferior.occipital.gyrus.hct) / asl.rest.right.iog.inferior.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.iog.inferior.occipital.gyrus.hct <- 100 * (asl.chall.left.iog.inferior.occipital.gyrus.hct - asl.rest.left.iog.inferior.occipital.gyrus.hct) / asl.rest.left.iog.inferior.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.itg.inferior.temporal.gyrus.hct <- 100 * (asl.chall.right.itg.inferior.temporal.gyrus.hct - asl.rest.right.itg.inferior.temporal.gyrus.hct) / asl.rest.right.itg.inferior.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.itg.inferior.temporal.gyrus.hct <- 100 * (asl.chall.left.itg.inferior.temporal.gyrus.hct - asl.rest.left.itg.inferior.temporal.gyrus.hct) / asl.rest.left.itg.inferior.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.lig.lingual.gyrus.hct <- 100 * (asl.chall.right.lig.lingual.gyrus.hct - asl.rest.right.lig.lingual.gyrus.hct) / asl.rest.right.lig.lingual.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.lig.lingual.gyrus.hct <- 100 * (asl.chall.left.lig.lingual.gyrus.hct - asl.rest.left.lig.lingual.gyrus.hct) / asl.rest.left.lig.lingual.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.lorg.lateral.orbital.gyrus.hct <- 100 * (asl.chall.right.lorg.lateral.orbital.gyrus.hct - asl.rest.right.lorg.lateral.orbital.gyrus.hct) / asl.rest.right.lorg.lateral.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.lorg.lateral.orbital.gyrus.hct <- 100 * (asl.chall.left.lorg.lateral.orbital.gyrus.hct - asl.rest.left.lorg.lateral.orbital.gyrus.hct) / asl.rest.left.lorg.lateral.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.mcgg.middle.cingulate.gyrus.hct <- 100 * (asl.chall.right.mcgg.middle.cingulate.gyrus.hct - asl.rest.right.mcgg.middle.cingulate.gyrus.hct) / asl.rest.right.mcgg.middle.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.mcgg.middle.cingulate.gyrus.hct <- 100 * (asl.chall.left.mcgg.middle.cingulate.gyrus.hct - asl.rest.left.mcgg.middle.cingulate.gyrus.hct) / asl.rest.left.mcgg.middle.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.mfc.medial.frontal.cortex.hct <- 100 * (asl.chall.right.mfc.medial.frontal.cortex.hct - asl.rest.right.mfc.medial.frontal.cortex.hct) / asl.rest.right.mfc.medial.frontal.cortex.hct / asl.3t.change.etco2
    asl.reac.left.mfc.medial.frontal.cortex.hct <- 100 * (asl.chall.left.mfc.medial.frontal.cortex.hct - asl.rest.left.mfc.medial.frontal.cortex.hct) / asl.rest.left.mfc.medial.frontal.cortex.hct / asl.3t.change.etco2
    asl.reac.right.mfg.middle.frontal.gyrus.hct <- 100 * (asl.chall.right.mfg.middle.frontal.gyrus.hct - asl.rest.right.mfg.middle.frontal.gyrus.hct) / asl.rest.right.mfg.middle.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.mfg.middle.frontal.gyrus.hct <- 100 * (asl.chall.left.mfg.middle.frontal.gyrus.hct - asl.rest.left.mfg.middle.frontal.gyrus.hct) / asl.rest.left.mfg.middle.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.mog.middle.occipital.gyrus.hct <- 100 * (asl.chall.right.mog.middle.occipital.gyrus.hct - asl.rest.right.mog.middle.occipital.gyrus.hct) / asl.rest.right.mog.middle.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.mog.middle.occipital.gyrus.hct <- 100 * (asl.chall.left.mog.middle.occipital.gyrus.hct - asl.rest.left.mog.middle.occipital.gyrus.hct) / asl.rest.left.mog.middle.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.morg.medial.orbital.gyrus.hct <- 100 * (asl.chall.right.morg.medial.orbital.gyrus.hct - asl.rest.right.morg.medial.orbital.gyrus.hct) / asl.rest.right.morg.medial.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.morg.medial.orbital.gyrus.hct <- 100 * (asl.chall.left.morg.medial.orbital.gyrus.hct - asl.rest.left.morg.medial.orbital.gyrus.hct) / asl.rest.left.morg.medial.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.mpog.postcentral.gyrus.medial.segment.hct <- 100 * (asl.chall.right.mpog.postcentral.gyrus.medial.segment.hct - asl.rest.right.mpog.postcentral.gyrus.medial.segment.hct) / asl.rest.right.mpog.postcentral.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.left.mpog.postcentral.gyrus.medial.segment.hct <- 100 * (asl.chall.left.mpog.postcentral.gyrus.medial.segment.hct - asl.rest.left.mpog.postcentral.gyrus.medial.segment.hct) / asl.rest.left.mpog.postcentral.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.right.mprg.precentral.gyrus.medial.segment.hct <- 100 * (asl.chall.right.mprg.precentral.gyrus.medial.segment.hct - asl.rest.right.mprg.precentral.gyrus.medial.segment.hct) / asl.rest.right.mprg.precentral.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.left.mprg.precentral.gyrus.medial.segment.hct <- 100 * (asl.chall.left.mprg.precentral.gyrus.medial.segment.hct - asl.rest.left.mprg.precentral.gyrus.medial.segment.hct) / asl.rest.left.mprg.precentral.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.right.msfg.superior.frontal.gyrus.medial.segment.hct <- 100 * (asl.chall.right.msfg.superior.frontal.gyrus.medial.segment.hct - asl.rest.right.msfg.superior.frontal.gyrus.medial.segment.hct) / asl.rest.right.msfg.superior.frontal.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.left.msfg.superior.frontal.gyrus.medial.segment.hct <- 100 * (asl.chall.left.msfg.superior.frontal.gyrus.medial.segment.hct - asl.rest.left.msfg.superior.frontal.gyrus.medial.segment.hct) / asl.rest.left.msfg.superior.frontal.gyrus.medial.segment.hct / asl.3t.change.etco2
    asl.reac.right.mtg.middle.temporal.gyrus.hct <- 100 * (asl.chall.right.mtg.middle.temporal.gyrus.hct - asl.rest.right.mtg.middle.temporal.gyrus.hct) / asl.rest.right.mtg.middle.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.mtg.middle.temporal.gyrus.hct <- 100 * (asl.chall.left.mtg.middle.temporal.gyrus.hct - asl.rest.left.mtg.middle.temporal.gyrus.hct) / asl.rest.left.mtg.middle.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.ocp.occipital.pole.hct <- 100 * (asl.chall.right.ocp.occipital.pole.hct - asl.rest.right.ocp.occipital.pole.hct) / asl.rest.right.ocp.occipital.pole.hct / asl.3t.change.etco2
    asl.reac.left.ocp.occipital.pole.hct <- 100 * (asl.chall.left.ocp.occipital.pole.hct - asl.rest.left.ocp.occipital.pole.hct) / asl.rest.left.ocp.occipital.pole.hct / asl.3t.change.etco2
    asl.reac.right.ofug.occipital.fusiform.gyrus.hct <- 100 * (asl.chall.right.ofug.occipital.fusiform.gyrus.hct - asl.rest.right.ofug.occipital.fusiform.gyrus.hct) / asl.rest.right.ofug.occipital.fusiform.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.ofug.occipital.fusiform.gyrus.hct <- 100 * (asl.chall.left.ofug.occipital.fusiform.gyrus.hct - asl.rest.left.ofug.occipital.fusiform.gyrus.hct) / asl.rest.left.ofug.occipital.fusiform.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.right.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct - asl.rest.right.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.right.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.left.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct - asl.rest.left.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.left.opifg.opercular.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.right.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct - asl.rest.right.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.right.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.left.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct - asl.rest.left.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.left.orifg.orbital.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.pcgg.posterior.cingulate.gyrus.hct <- 100 * (asl.chall.right.pcgg.posterior.cingulate.gyrus.hct - asl.rest.right.pcgg.posterior.cingulate.gyrus.hct) / asl.rest.right.pcgg.posterior.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.pcgg.posterior.cingulate.gyrus.hct <- 100 * (asl.chall.left.pcgg.posterior.cingulate.gyrus.hct - asl.rest.left.pcgg.posterior.cingulate.gyrus.hct) / asl.rest.left.pcgg.posterior.cingulate.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.pcu.precuneus.hct <- 100 * (asl.chall.right.pcu.precuneus.hct - asl.rest.right.pcu.precuneus.hct) / asl.rest.right.pcu.precuneus.hct / asl.3t.change.etco2
    asl.reac.left.pcu.precuneus.hct <- 100 * (asl.chall.left.pcu.precuneus.hct - asl.rest.left.pcu.precuneus.hct) / asl.rest.left.pcu.precuneus.hct / asl.3t.change.etco2
    asl.reac.right.phg.parahippocampal.gyrus.hct <- 100 * (asl.chall.right.phg.parahippocampal.gyrus.hct - asl.rest.right.phg.parahippocampal.gyrus.hct) / asl.rest.right.phg.parahippocampal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.phg.parahippocampal.gyrus.hct <- 100 * (asl.chall.left.phg.parahippocampal.gyrus.hct - asl.rest.left.phg.parahippocampal.gyrus.hct) / asl.rest.left.phg.parahippocampal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.pins.posterior.insula.hct <- 100 * (asl.chall.right.pins.posterior.insula.hct - asl.rest.right.pins.posterior.insula.hct) / asl.rest.right.pins.posterior.insula.hct / asl.3t.change.etco2
    asl.reac.left.pins.posterior.insula.hct <- 100 * (asl.chall.left.pins.posterior.insula.hct - asl.rest.left.pins.posterior.insula.hct) / asl.rest.left.pins.posterior.insula.hct / asl.3t.change.etco2
    asl.reac.right.po.parietal.operculum.hct <- 100 * (asl.chall.right.po.parietal.operculum.hct - asl.rest.right.po.parietal.operculum.hct) / asl.rest.right.po.parietal.operculum.hct / asl.3t.change.etco2
    asl.reac.left.po.parietal.operculum.hct <- 100 * (asl.chall.left.po.parietal.operculum.hct - asl.rest.left.po.parietal.operculum.hct) / asl.rest.left.po.parietal.operculum.hct / asl.3t.change.etco2
    asl.reac.right.pog.postcentral.gyrus.hct <- 100 * (asl.chall.right.pog.postcentral.gyrus.hct - asl.rest.right.pog.postcentral.gyrus.hct) / asl.rest.right.pog.postcentral.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.pog.postcentral.gyrus.hct <- 100 * (asl.chall.left.pog.postcentral.gyrus.hct - asl.rest.left.pog.postcentral.gyrus.hct) / asl.rest.left.pog.postcentral.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.porg.posterior.orbital.gyrus.hct <- 100 * (asl.chall.right.porg.posterior.orbital.gyrus.hct - asl.rest.right.porg.posterior.orbital.gyrus.hct) / asl.rest.right.porg.posterior.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.porg.posterior.orbital.gyrus.hct <- 100 * (asl.chall.left.porg.posterior.orbital.gyrus.hct - asl.rest.left.porg.posterior.orbital.gyrus.hct) / asl.rest.left.porg.posterior.orbital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.pp.planum.polare.hct <- 100 * (asl.chall.right.pp.planum.polare.hct - asl.rest.right.pp.planum.polare.hct) / asl.rest.right.pp.planum.polare.hct / asl.3t.change.etco2
    asl.reac.left.pp.planum.polare.hct <- 100 * (asl.chall.left.pp.planum.polare.hct - asl.rest.left.pp.planum.polare.hct) / asl.rest.left.pp.planum.polare.hct / asl.3t.change.etco2
    asl.reac.right.prg.precentral.gyrus.hct <- 100 * (asl.chall.right.prg.precentral.gyrus.hct - asl.rest.right.prg.precentral.gyrus.hct) / asl.rest.right.prg.precentral.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.prg.precentral.gyrus.hct <- 100 * (asl.chall.left.prg.precentral.gyrus.hct - asl.rest.left.prg.precentral.gyrus.hct) / asl.rest.left.prg.precentral.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.pt.planum.temporale.hct <- 100 * (asl.chall.right.pt.planum.temporale.hct - asl.rest.right.pt.planum.temporale.hct) / asl.rest.right.pt.planum.temporale.hct / asl.3t.change.etco2
    asl.reac.left.pt.planum.temporale.hct <- 100 * (asl.chall.left.pt.planum.temporale.hct - asl.rest.left.pt.planum.temporale.hct) / asl.rest.left.pt.planum.temporale.hct / asl.3t.change.etco2
    asl.reac.right.sca.subcallosal.area.hct <- 100 * (asl.chall.right.sca.subcallosal.area.hct - asl.rest.right.sca.subcallosal.area.hct) / asl.rest.right.sca.subcallosal.area.hct / asl.3t.change.etco2
    asl.reac.left.sca.subcallosal.area.hct <- 100 * (asl.chall.left.sca.subcallosal.area.hct - asl.rest.left.sca.subcallosal.area.hct) / asl.rest.left.sca.subcallosal.area.hct / asl.3t.change.etco2
    asl.reac.right.sfg.superior.frontal.gyrus.hct <- 100 * (asl.chall.right.sfg.superior.frontal.gyrus.hct - asl.rest.right.sfg.superior.frontal.gyrus.hct) / asl.rest.right.sfg.superior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.sfg.superior.frontal.gyrus.hct <- 100 * (asl.chall.left.sfg.superior.frontal.gyrus.hct - asl.rest.left.sfg.superior.frontal.gyrus.hct) / asl.rest.left.sfg.superior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.smc.supplementary.motor.cortex.hct <- 100 * (asl.chall.right.smc.supplementary.motor.cortex.hct - asl.rest.right.smc.supplementary.motor.cortex.hct) / asl.rest.right.smc.supplementary.motor.cortex.hct / asl.3t.change.etco2
    asl.reac.left.smc.supplementary.motor.cortex.hct <- 100 * (asl.chall.left.smc.supplementary.motor.cortex.hct - asl.rest.left.smc.supplementary.motor.cortex.hct) / asl.rest.left.smc.supplementary.motor.cortex.hct / asl.3t.change.etco2
    asl.reac.right.smg.supramarginal.gyrus.hct <- 100 * (asl.chall.right.smg.supramarginal.gyrus.hct - asl.rest.right.smg.supramarginal.gyrus.hct) / asl.rest.right.smg.supramarginal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.smg.supramarginal.gyrus.hct <- 100 * (asl.chall.left.smg.supramarginal.gyrus.hct - asl.rest.left.smg.supramarginal.gyrus.hct) / asl.rest.left.smg.supramarginal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.sog.superior.occipital.gyrus.hct <- 100 * (asl.chall.right.sog.superior.occipital.gyrus.hct - asl.rest.right.sog.superior.occipital.gyrus.hct) / asl.rest.right.sog.superior.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.sog.superior.occipital.gyrus.hct <- 100 * (asl.chall.left.sog.superior.occipital.gyrus.hct - asl.rest.left.sog.superior.occipital.gyrus.hct) / asl.rest.left.sog.superior.occipital.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.spl.superior.parietal.lobule.hct <- 100 * (asl.chall.right.spl.superior.parietal.lobule.hct - asl.rest.right.spl.superior.parietal.lobule.hct) / asl.rest.right.spl.superior.parietal.lobule.hct / asl.3t.change.etco2
    asl.reac.left.spl.superior.parietal.lobule.hct <- 100 * (asl.chall.left.spl.superior.parietal.lobule.hct - asl.rest.left.spl.superior.parietal.lobule.hct) / asl.rest.left.spl.superior.parietal.lobule.hct / asl.3t.change.etco2
    asl.reac.right.stg.superior.temporal.gyrus.hct <- 100 * (asl.chall.right.stg.superior.temporal.gyrus.hct - asl.rest.right.stg.superior.temporal.gyrus.hct) / asl.rest.right.stg.superior.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.stg.superior.temporal.gyrus.hct <- 100 * (asl.chall.left.stg.superior.temporal.gyrus.hct - asl.rest.left.stg.superior.temporal.gyrus.hct) / asl.rest.left.stg.superior.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.tmp.temporal.pole.hct <- 100 * (asl.chall.right.tmp.temporal.pole.hct - asl.rest.right.tmp.temporal.pole.hct) / asl.rest.right.tmp.temporal.pole.hct / asl.3t.change.etco2
    asl.reac.left.tmp.temporal.pole.hct <- 100 * (asl.chall.left.tmp.temporal.pole.hct - asl.rest.left.tmp.temporal.pole.hct) / asl.rest.left.tmp.temporal.pole.hct / asl.3t.change.etco2
    asl.reac.right.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.right.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct - asl.rest.right.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.right.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct <- 100 * (asl.chall.left.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct - asl.rest.left.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct) / asl.rest.left.trifg.triangular.part.of.the.inferior.frontal.gyrus.hct / asl.3t.change.etco2
    asl.reac.right.ttg.transverse.temporal.gyrus.hct <- 100 * (asl.chall.right.ttg.transverse.temporal.gyrus.hct - asl.rest.right.ttg.transverse.temporal.gyrus.hct) / asl.rest.right.ttg.transverse.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.left.ttg.transverse.temporal.gyrus.hct <- 100 * (asl.chall.left.ttg.transverse.temporal.gyrus.hct - asl.rest.left.ttg.transverse.temporal.gyrus.hct) / asl.rest.left.ttg.transverse.temporal.gyrus.hct / asl.3t.change.etco2
    asl.reac.grey.matter <- 100 * (asl.chall.grey.matter - asl.rest.grey.matter) / asl.rest.grey.matter / asl.3t.change.etco2
    asl.reac.left.hemisphere <- 100 * (asl.chall.left.hemisphere - asl.rest.left.hemisphere) / asl.rest.left.hemisphere / asl.3t.change.etco2
    asl.reac.right.hemisphere <- 100 * (asl.chall.right.hemisphere - asl.rest.right.hemisphere) / asl.rest.right.hemisphere / asl.3t.change.etco2
    asl.reac.right.frontal.lobe <- 100 * (asl.chall.right.frontal.lobe - asl.rest.right.frontal.lobe) / asl.rest.right.frontal.lobe / asl.3t.change.etco2
    asl.reac.left.frontal.lobe <- 100 * (asl.chall.left.frontal.lobe - asl.rest.left.frontal.lobe) / asl.rest.left.frontal.lobe / asl.3t.change.etco2
    asl.reac.frontal.lobe <- 100 * (asl.chall.frontal.lobe - asl.rest.frontal.lobe) / asl.rest.frontal.lobe / asl.3t.change.etco2
    asl.reac.right.occipital.lobe <- 100 * (asl.chall.right.occipital.lobe - asl.rest.right.occipital.lobe) / asl.rest.right.occipital.lobe / asl.3t.change.etco2
    asl.reac.left.occipital.lobe <- 100 * (asl.chall.left.occipital.lobe - asl.rest.left.occipital.lobe) / asl.rest.left.occipital.lobe / asl.3t.change.etco2
    asl.reac.occipital.lobe <- 100 * (asl.chall.occipital.lobe - asl.rest.occipital.lobe) / asl.rest.occipital.lobe / asl.3t.change.etco2
    asl.reac.right.temporal.lobe <- 100 * (asl.chall.right.temporal.lobe - asl.rest.right.temporal.lobe) / asl.rest.right.temporal.lobe / asl.3t.change.etco2
    asl.reac.left.temporal.lobe <- 100 * (asl.chall.left.temporal.lobe - asl.rest.left.temporal.lobe) / asl.rest.left.temporal.lobe / asl.3t.change.etco2
    asl.reac.temporal.lobe <- 100 * (asl.chall.temporal.lobe - asl.rest.temporal.lobe) / asl.rest.temporal.lobe / asl.3t.change.etco2
    asl.reac.right.parietal.lobe <- 100 * (asl.chall.right.parietal.lobe - asl.rest.right.parietal.lobe) / asl.rest.right.parietal.lobe / asl.3t.change.etco2
    asl.reac.left.parietal.lobe <- 100 * (asl.chall.left.parietal.lobe - asl.rest.left.parietal.lobe) / asl.rest.left.parietal.lobe / asl.3t.change.etco2
    asl.reac.parietal.lobe <- 100 * (asl.chall.parietal.lobe - asl.rest.parietal.lobe) / asl.rest.parietal.lobe / asl.3t.change.etco2
    asl.reac.3rd.ventricle <- 100 * (asl.chall.3rd.ventricle - asl.rest.3rd.ventricle) / asl.rest.3rd.ventricle / asl.3t.change.etco2
    asl.reac.4th.ventricle <- 100 * (asl.chall.4th.ventricle - asl.rest.4th.ventricle) / asl.rest.4th.ventricle / asl.3t.change.etco2
    asl.reac.right.accumbens.area <- 100 * (asl.chall.right.accumbens.area - asl.rest.right.accumbens.area) / asl.rest.right.accumbens.area / asl.3t.change.etco2
    asl.reac.left.accumbens.area <- 100 * (asl.chall.left.accumbens.area - asl.rest.left.accumbens.area) / asl.rest.left.accumbens.area / asl.3t.change.etco2
    asl.reac.right.amygdala <- 100 * (asl.chall.right.amygdala - asl.rest.right.amygdala) / asl.rest.right.amygdala / asl.3t.change.etco2
    asl.reac.left.amygdala <- 100 * (asl.chall.left.amygdala - asl.rest.left.amygdala) / asl.rest.left.amygdala / asl.3t.change.etco2
    asl.reac.brain.stem <- 100 * (asl.chall.brain.stem - asl.rest.brain.stem) / asl.rest.brain.stem / asl.3t.change.etco2
    asl.reac.right.caudate <- 100 * (asl.chall.right.caudate - asl.rest.right.caudate) / asl.rest.right.caudate / asl.3t.change.etco2
    asl.reac.left.caudate <- 100 * (asl.chall.left.caudate - asl.rest.left.caudate) / asl.rest.left.caudate / asl.3t.change.etco2
    asl.reac.right.cerebellum.exterior <- 100 * (asl.chall.right.cerebellum.exterior - asl.rest.right.cerebellum.exterior) / asl.rest.right.cerebellum.exterior / asl.3t.change.etco2
    asl.reac.left.cerebellum.exterior <- 100 * (asl.chall.left.cerebellum.exterior - asl.rest.left.cerebellum.exterior) / asl.rest.left.cerebellum.exterior / asl.3t.change.etco2
    asl.reac.right.cerebellum.white.matter <- 100 * (asl.chall.right.cerebellum.white.matter - asl.rest.right.cerebellum.white.matter) / asl.rest.right.cerebellum.white.matter / asl.3t.change.etco2
    asl.reac.left.cerebellum.white.matter <- 100 * (asl.chall.left.cerebellum.white.matter - asl.rest.left.cerebellum.white.matter) / asl.rest.left.cerebellum.white.matter / asl.3t.change.etco2
    asl.reac.right.cerebral.white.matter <- 100 * (asl.chall.right.cerebral.white.matter - asl.rest.right.cerebral.white.matter) / asl.rest.right.cerebral.white.matter / asl.3t.change.etco2
    asl.reac.left.cerebral.white.matter <- 100 * (asl.chall.left.cerebral.white.matter - asl.rest.left.cerebral.white.matter) / asl.rest.left.cerebral.white.matter / asl.3t.change.etco2
    asl.reac.right.hippocampus <- 100 * (asl.chall.right.hippocampus - asl.rest.right.hippocampus) / asl.rest.right.hippocampus / asl.3t.change.etco2
    asl.reac.left.hippocampus <- 100 * (asl.chall.left.hippocampus - asl.rest.left.hippocampus) / asl.rest.left.hippocampus / asl.3t.change.etco2
    asl.reac.right.inf.lat.vent <- 100 * (asl.chall.right.inf.lat.vent - asl.rest.right.inf.lat.vent) / asl.rest.right.inf.lat.vent / asl.3t.change.etco2
    asl.reac.left.inf.lat.vent <- 100 * (asl.chall.left.inf.lat.vent - asl.rest.left.inf.lat.vent) / asl.rest.left.inf.lat.vent / asl.3t.change.etco2
    asl.reac.right.lateral.ventricle <- 100 * (asl.chall.right.lateral.ventricle - asl.rest.right.lateral.ventricle) / asl.rest.right.lateral.ventricle / asl.3t.change.etco2
    asl.reac.left.lateral.ventricle <- 100 * (asl.chall.left.lateral.ventricle - asl.rest.left.lateral.ventricle) / asl.rest.left.lateral.ventricle / asl.3t.change.etco2
    asl.reac.right.pallidum <- 100 * (asl.chall.right.pallidum - asl.rest.right.pallidum) / asl.rest.right.pallidum / asl.3t.change.etco2
    asl.reac.left.pallidum <- 100 * (asl.chall.left.pallidum - asl.rest.left.pallidum) / asl.rest.left.pallidum / asl.3t.change.etco2
    asl.reac.right.putamen <- 100 * (asl.chall.right.putamen - asl.rest.right.putamen) / asl.rest.right.putamen / asl.3t.change.etco2
    asl.reac.left.putamen <- 100 * (asl.chall.left.putamen - asl.rest.left.putamen) / asl.rest.left.putamen / asl.3t.change.etco2
    asl.reac.right.thalamus.proper <- 100 * (asl.chall.right.thalamus.proper - asl.rest.right.thalamus.proper) / asl.rest.right.thalamus.proper / asl.3t.change.etco2
    asl.reac.left.thalamus.proper <- 100 * (asl.chall.left.thalamus.proper - asl.rest.left.thalamus.proper) / asl.rest.left.thalamus.proper / asl.3t.change.etco2
    asl.reac.right.ventral.dc <- 100 * (asl.chall.right.ventral.dc - asl.rest.right.ventral.dc) / asl.rest.right.ventral.dc / asl.3t.change.etco2
    asl.reac.left.ventral.dc <- 100 * (asl.chall.left.ventral.dc - asl.rest.left.ventral.dc) / asl.rest.left.ventral.dc / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.iv <- 100 * (asl.chall.cerebellar.vermal.lobules.iv - asl.rest.cerebellar.vermal.lobules.iv) / asl.rest.cerebellar.vermal.lobules.iv / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.vivii <- 100 * (asl.chall.cerebellar.vermal.lobules.vivii - asl.rest.cerebellar.vermal.lobules.vivii) / asl.rest.cerebellar.vermal.lobules.vivii / asl.3t.change.etco2
    asl.reac.cerebellar.vermal.lobules.viiix <- 100 * (asl.chall.cerebellar.vermal.lobules.viiix - asl.rest.cerebellar.vermal.lobules.viiix) / asl.rest.cerebellar.vermal.lobules.viiix / asl.3t.change.etco2
    asl.reac.left.basal.forebrain <- 100 * (asl.chall.left.basal.forebrain - asl.rest.left.basal.forebrain) / asl.rest.left.basal.forebrain / asl.3t.change.etco2
    asl.reac.right.basal.forebrain <- 100 * (asl.chall.right.basal.forebrain - asl.rest.right.basal.forebrain) / asl.rest.right.basal.forebrain / asl.3t.change.etco2
    asl.reac.right.acgg.anterior.cingulate.gyrus <- 100 * (asl.chall.right.acgg.anterior.cingulate.gyrus - asl.rest.right.acgg.anterior.cingulate.gyrus) / asl.rest.right.acgg.anterior.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.left.acgg.anterior.cingulate.gyrus <- 100 * (asl.chall.left.acgg.anterior.cingulate.gyrus - asl.rest.left.acgg.anterior.cingulate.gyrus) / asl.rest.left.acgg.anterior.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.right.ains.anterior.insula <- 100 * (asl.chall.right.ains.anterior.insula - asl.rest.right.ains.anterior.insula) / asl.rest.right.ains.anterior.insula / asl.3t.change.etco2
    asl.reac.left.ains.anterior.insula <- 100 * (asl.chall.left.ains.anterior.insula - asl.rest.left.ains.anterior.insula) / asl.rest.left.ains.anterior.insula / asl.3t.change.etco2
    asl.reac.right.aorg.anterior.orbital.gyrus <- 100 * (asl.chall.right.aorg.anterior.orbital.gyrus - asl.rest.right.aorg.anterior.orbital.gyrus) / asl.rest.right.aorg.anterior.orbital.gyrus / asl.3t.change.etco2
    asl.reac.left.aorg.anterior.orbital.gyrus <- 100 * (asl.chall.left.aorg.anterior.orbital.gyrus - asl.rest.left.aorg.anterior.orbital.gyrus) / asl.rest.left.aorg.anterior.orbital.gyrus / asl.3t.change.etco2
    asl.reac.right.ang.angular.gyrus <- 100 * (asl.chall.right.ang.angular.gyrus - asl.rest.right.ang.angular.gyrus) / asl.rest.right.ang.angular.gyrus / asl.3t.change.etco2
    asl.reac.left.ang.angular.gyrus <- 100 * (asl.chall.left.ang.angular.gyrus - asl.rest.left.ang.angular.gyrus) / asl.rest.left.ang.angular.gyrus / asl.3t.change.etco2
    asl.reac.right.calc.calcarine.cortex <- 100 * (asl.chall.right.calc.calcarine.cortex - asl.rest.right.calc.calcarine.cortex) / asl.rest.right.calc.calcarine.cortex / asl.3t.change.etco2
    asl.reac.left.calc.calcarine.cortex <- 100 * (asl.chall.left.calc.calcarine.cortex - asl.rest.left.calc.calcarine.cortex) / asl.rest.left.calc.calcarine.cortex / asl.3t.change.etco2
    asl.reac.right.co.central.operculum <- 100 * (asl.chall.right.co.central.operculum - asl.rest.right.co.central.operculum) / asl.rest.right.co.central.operculum / asl.3t.change.etco2
    asl.reac.left.co.central.operculum <- 100 * (asl.chall.left.co.central.operculum - asl.rest.left.co.central.operculum) / asl.rest.left.co.central.operculum / asl.3t.change.etco2
    asl.reac.right.cun.cuneus <- 100 * (asl.chall.right.cun.cuneus - asl.rest.right.cun.cuneus) / asl.rest.right.cun.cuneus / asl.3t.change.etco2
    asl.reac.left.cun.cuneus <- 100 * (asl.chall.left.cun.cuneus - asl.rest.left.cun.cuneus) / asl.rest.left.cun.cuneus / asl.3t.change.etco2
    asl.reac.right.ent.entorhinal.area <- 100 * (asl.chall.right.ent.entorhinal.area - asl.rest.right.ent.entorhinal.area) / asl.rest.right.ent.entorhinal.area / asl.3t.change.etco2
    asl.reac.left.ent.entorhinal.area <- 100 * (asl.chall.left.ent.entorhinal.area - asl.rest.left.ent.entorhinal.area) / asl.rest.left.ent.entorhinal.area / asl.3t.change.etco2
    asl.reac.right.fo.frontal.operculum <- 100 * (asl.chall.right.fo.frontal.operculum - asl.rest.right.fo.frontal.operculum) / asl.rest.right.fo.frontal.operculum / asl.3t.change.etco2
    asl.reac.left.fo.frontal.operculum <- 100 * (asl.chall.left.fo.frontal.operculum - asl.rest.left.fo.frontal.operculum) / asl.rest.left.fo.frontal.operculum / asl.3t.change.etco2
    asl.reac.right.frp.frontal.pole <- 100 * (asl.chall.right.frp.frontal.pole - asl.rest.right.frp.frontal.pole) / asl.rest.right.frp.frontal.pole / asl.3t.change.etco2
    asl.reac.left.frp.frontal.pole <- 100 * (asl.chall.left.frp.frontal.pole - asl.rest.left.frp.frontal.pole) / asl.rest.left.frp.frontal.pole / asl.3t.change.etco2
    asl.reac.right.fug.fusiform.gyrus <- 100 * (asl.chall.right.fug.fusiform.gyrus - asl.rest.right.fug.fusiform.gyrus) / asl.rest.right.fug.fusiform.gyrus / asl.3t.change.etco2
    asl.reac.left.fug.fusiform.gyrus <- 100 * (asl.chall.left.fug.fusiform.gyrus - asl.rest.left.fug.fusiform.gyrus) / asl.rest.left.fug.fusiform.gyrus / asl.3t.change.etco2
    asl.reac.right.gre.gyrus.rectus <- 100 * (asl.chall.right.gre.gyrus.rectus - asl.rest.right.gre.gyrus.rectus) / asl.rest.right.gre.gyrus.rectus / asl.3t.change.etco2
    asl.reac.left.gre.gyrus.rectus <- 100 * (asl.chall.left.gre.gyrus.rectus - asl.rest.left.gre.gyrus.rectus) / asl.rest.left.gre.gyrus.rectus / asl.3t.change.etco2
    asl.reac.right.iog.inferior.occipital.gyrus <- 100 * (asl.chall.right.iog.inferior.occipital.gyrus - asl.rest.right.iog.inferior.occipital.gyrus) / asl.rest.right.iog.inferior.occipital.gyrus / asl.3t.change.etco2
    asl.reac.left.iog.inferior.occipital.gyrus <- 100 * (asl.chall.left.iog.inferior.occipital.gyrus - asl.rest.left.iog.inferior.occipital.gyrus) / asl.rest.left.iog.inferior.occipital.gyrus / asl.3t.change.etco2
    asl.reac.right.itg.inferior.temporal.gyrus <- 100 * (asl.chall.right.itg.inferior.temporal.gyrus - asl.rest.right.itg.inferior.temporal.gyrus) / asl.rest.right.itg.inferior.temporal.gyrus / asl.3t.change.etco2
    asl.reac.left.itg.inferior.temporal.gyrus <- 100 * (asl.chall.left.itg.inferior.temporal.gyrus - asl.rest.left.itg.inferior.temporal.gyrus) / asl.rest.left.itg.inferior.temporal.gyrus / asl.3t.change.etco2
    asl.reac.right.lig.lingual.gyrus <- 100 * (asl.chall.right.lig.lingual.gyrus - asl.rest.right.lig.lingual.gyrus) / asl.rest.right.lig.lingual.gyrus / asl.3t.change.etco2
    asl.reac.left.lig.lingual.gyrus <- 100 * (asl.chall.left.lig.lingual.gyrus - asl.rest.left.lig.lingual.gyrus) / asl.rest.left.lig.lingual.gyrus / asl.3t.change.etco2
    asl.reac.right.lorg.lateral.orbital.gyrus <- 100 * (asl.chall.right.lorg.lateral.orbital.gyrus - asl.rest.right.lorg.lateral.orbital.gyrus) / asl.rest.right.lorg.lateral.orbital.gyrus / asl.3t.change.etco2
    asl.reac.left.lorg.lateral.orbital.gyrus <- 100 * (asl.chall.left.lorg.lateral.orbital.gyrus - asl.rest.left.lorg.lateral.orbital.gyrus) / asl.rest.left.lorg.lateral.orbital.gyrus / asl.3t.change.etco2
    asl.reac.right.mcgg.middle.cingulate.gyrus <- 100 * (asl.chall.right.mcgg.middle.cingulate.gyrus - asl.rest.right.mcgg.middle.cingulate.gyrus) / asl.rest.right.mcgg.middle.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.left.mcgg.middle.cingulate.gyrus <- 100 * (asl.chall.left.mcgg.middle.cingulate.gyrus - asl.rest.left.mcgg.middle.cingulate.gyrus) / asl.rest.left.mcgg.middle.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.right.mfc.medial.frontal.cortex <- 100 * (asl.chall.right.mfc.medial.frontal.cortex - asl.rest.right.mfc.medial.frontal.cortex) / asl.rest.right.mfc.medial.frontal.cortex / asl.3t.change.etco2
    asl.reac.left.mfc.medial.frontal.cortex <- 100 * (asl.chall.left.mfc.medial.frontal.cortex - asl.rest.left.mfc.medial.frontal.cortex) / asl.rest.left.mfc.medial.frontal.cortex / asl.3t.change.etco2
    asl.reac.right.mfg.middle.frontal.gyrus <- 100 * (asl.chall.right.mfg.middle.frontal.gyrus - asl.rest.right.mfg.middle.frontal.gyrus) / asl.rest.right.mfg.middle.frontal.gyrus / asl.3t.change.etco2
    asl.reac.left.mfg.middle.frontal.gyrus <- 100 * (asl.chall.left.mfg.middle.frontal.gyrus - asl.rest.left.mfg.middle.frontal.gyrus) / asl.rest.left.mfg.middle.frontal.gyrus / asl.3t.change.etco2
    asl.reac.right.mog.middle.occipital.gyrus <- 100 * (asl.chall.right.mog.middle.occipital.gyrus - asl.rest.right.mog.middle.occipital.gyrus) / asl.rest.right.mog.middle.occipital.gyrus / asl.3t.change.etco2
    asl.reac.left.mog.middle.occipital.gyrus <- 100 * (asl.chall.left.mog.middle.occipital.gyrus - asl.rest.left.mog.middle.occipital.gyrus) / asl.rest.left.mog.middle.occipital.gyrus / asl.3t.change.etco2
    asl.reac.right.morg.medial.orbital.gyrus <- 100 * (asl.chall.right.morg.medial.orbital.gyrus - asl.rest.right.morg.medial.orbital.gyrus) / asl.rest.right.morg.medial.orbital.gyrus / asl.3t.change.etco2
    asl.reac.left.morg.medial.orbital.gyrus <- 100 * (asl.chall.left.morg.medial.orbital.gyrus - asl.rest.left.morg.medial.orbital.gyrus) / asl.rest.left.morg.medial.orbital.gyrus / asl.3t.change.etco2
    asl.reac.right.mpog.postcentral.gyrus.medial.segment <- 100 * (asl.chall.right.mpog.postcentral.gyrus.medial.segment - asl.rest.right.mpog.postcentral.gyrus.medial.segment) / asl.rest.right.mpog.postcentral.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.left.mpog.postcentral.gyrus.medial.segment <- 100 * (asl.chall.left.mpog.postcentral.gyrus.medial.segment - asl.rest.left.mpog.postcentral.gyrus.medial.segment) / asl.rest.left.mpog.postcentral.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.right.mprg.precentral.gyrus.medial.segment <- 100 * (asl.chall.right.mprg.precentral.gyrus.medial.segment - asl.rest.right.mprg.precentral.gyrus.medial.segment) / asl.rest.right.mprg.precentral.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.left.mprg.precentral.gyrus.medial.segment <- 100 * (asl.chall.left.mprg.precentral.gyrus.medial.segment - asl.rest.left.mprg.precentral.gyrus.medial.segment) / asl.rest.left.mprg.precentral.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.right.msfg.superior.frontal.gyrus.medial.segment <- 100 * (asl.chall.right.msfg.superior.frontal.gyrus.medial.segment - asl.rest.right.msfg.superior.frontal.gyrus.medial.segment) / asl.rest.right.msfg.superior.frontal.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.left.msfg.superior.frontal.gyrus.medial.segment <- 100 * (asl.chall.left.msfg.superior.frontal.gyrus.medial.segment - asl.rest.left.msfg.superior.frontal.gyrus.medial.segment) / asl.rest.left.msfg.superior.frontal.gyrus.medial.segment / asl.3t.change.etco2
    asl.reac.right.mtg.middle.temporal.gyrus <- 100 * (asl.chall.right.mtg.middle.temporal.gyrus - asl.rest.right.mtg.middle.temporal.gyrus) / asl.rest.right.mtg.middle.temporal.gyrus / asl.3t.change.etco2
    asl.reac.left.mtg.middle.temporal.gyrus <- 100 * (asl.chall.left.mtg.middle.temporal.gyrus - asl.rest.left.mtg.middle.temporal.gyrus) / asl.rest.left.mtg.middle.temporal.gyrus / asl.3t.change.etco2
    asl.reac.right.ocp.occipital.pole <- 100 * (asl.chall.right.ocp.occipital.pole - asl.rest.right.ocp.occipital.pole) / asl.rest.right.ocp.occipital.pole / asl.3t.change.etco2
    asl.reac.left.ocp.occipital.pole <- 100 * (asl.chall.left.ocp.occipital.pole - asl.rest.left.ocp.occipital.pole) / asl.rest.left.ocp.occipital.pole / asl.3t.change.etco2
    asl.reac.right.ofug.occipital.fusiform.gyrus <- 100 * (asl.chall.right.ofug.occipital.fusiform.gyrus - asl.rest.right.ofug.occipital.fusiform.gyrus) / asl.rest.right.ofug.occipital.fusiform.gyrus / asl.3t.change.etco2
    asl.reac.left.ofug.occipital.fusiform.gyrus <- 100 * (asl.chall.left.ofug.occipital.fusiform.gyrus - asl.rest.left.ofug.occipital.fusiform.gyrus) / asl.rest.left.ofug.occipital.fusiform.gyrus / asl.3t.change.etco2
    asl.reac.right.opifg.opercular.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.right.opifg.opercular.part.of.the.inferior.frontal.gyrus - asl.rest.right.opifg.opercular.part.of.the.inferior.frontal.gyrus) / asl.rest.right.opifg.opercular.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.left.opifg.opercular.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.left.opifg.opercular.part.of.the.inferior.frontal.gyrus - asl.rest.left.opifg.opercular.part.of.the.inferior.frontal.gyrus) / asl.rest.left.opifg.opercular.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.right.orifg.orbital.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.right.orifg.orbital.part.of.the.inferior.frontal.gyrus - asl.rest.right.orifg.orbital.part.of.the.inferior.frontal.gyrus) / asl.rest.right.orifg.orbital.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.left.orifg.orbital.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.left.orifg.orbital.part.of.the.inferior.frontal.gyrus - asl.rest.left.orifg.orbital.part.of.the.inferior.frontal.gyrus) / asl.rest.left.orifg.orbital.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.right.pcgg.posterior.cingulate.gyrus <- 100 * (asl.chall.right.pcgg.posterior.cingulate.gyrus - asl.rest.right.pcgg.posterior.cingulate.gyrus) / asl.rest.right.pcgg.posterior.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.left.pcgg.posterior.cingulate.gyrus <- 100 * (asl.chall.left.pcgg.posterior.cingulate.gyrus - asl.rest.left.pcgg.posterior.cingulate.gyrus) / asl.rest.left.pcgg.posterior.cingulate.gyrus / asl.3t.change.etco2
    asl.reac.right.pcu.precuneus <- 100 * (asl.chall.right.pcu.precuneus - asl.rest.right.pcu.precuneus) / asl.rest.right.pcu.precuneus / asl.3t.change.etco2
    asl.reac.left.pcu.precuneus <- 100 * (asl.chall.left.pcu.precuneus - asl.rest.left.pcu.precuneus) / asl.rest.left.pcu.precuneus / asl.3t.change.etco2
    asl.reac.right.phg.parahippocampal.gyrus <- 100 * (asl.chall.right.phg.parahippocampal.gyrus - asl.rest.right.phg.parahippocampal.gyrus) / asl.rest.right.phg.parahippocampal.gyrus / asl.3t.change.etco2
    asl.reac.left.phg.parahippocampal.gyrus <- 100 * (asl.chall.left.phg.parahippocampal.gyrus - asl.rest.left.phg.parahippocampal.gyrus) / asl.rest.left.phg.parahippocampal.gyrus / asl.3t.change.etco2
    asl.reac.right.pins.posterior.insula <- 100 * (asl.chall.right.pins.posterior.insula - asl.rest.right.pins.posterior.insula) / asl.rest.right.pins.posterior.insula / asl.3t.change.etco2
    asl.reac.left.pins.posterior.insula <- 100 * (asl.chall.left.pins.posterior.insula - asl.rest.left.pins.posterior.insula) / asl.rest.left.pins.posterior.insula / asl.3t.change.etco2
    asl.reac.right.po.parietal.operculum <- 100 * (asl.chall.right.po.parietal.operculum - asl.rest.right.po.parietal.operculum) / asl.rest.right.po.parietal.operculum / asl.3t.change.etco2
    asl.reac.left.po.parietal.operculum <- 100 * (asl.chall.left.po.parietal.operculum - asl.rest.left.po.parietal.operculum) / asl.rest.left.po.parietal.operculum / asl.3t.change.etco2
    asl.reac.right.pog.postcentral.gyrus <- 100 * (asl.chall.right.pog.postcentral.gyrus - asl.rest.right.pog.postcentral.gyrus) / asl.rest.right.pog.postcentral.gyrus / asl.3t.change.etco2
    asl.reac.left.pog.postcentral.gyrus <- 100 * (asl.chall.left.pog.postcentral.gyrus - asl.rest.left.pog.postcentral.gyrus) / asl.rest.left.pog.postcentral.gyrus / asl.3t.change.etco2
    asl.reac.right.porg.posterior.orbital.gyrus <- 100 * (asl.chall.right.porg.posterior.orbital.gyrus - asl.rest.right.porg.posterior.orbital.gyrus) / asl.rest.right.porg.posterior.orbital.gyrus / asl.3t.change.etco2
    asl.reac.left.porg.posterior.orbital.gyrus <- 100 * (asl.chall.left.porg.posterior.orbital.gyrus - asl.rest.left.porg.posterior.orbital.gyrus) / asl.rest.left.porg.posterior.orbital.gyrus / asl.3t.change.etco2
    asl.reac.right.pp.planum.polare <- 100 * (asl.chall.right.pp.planum.polare - asl.rest.right.pp.planum.polare) / asl.rest.right.pp.planum.polare / asl.3t.change.etco2
    asl.reac.left.pp.planum.polare <- 100 * (asl.chall.left.pp.planum.polare - asl.rest.left.pp.planum.polare) / asl.rest.left.pp.planum.polare / asl.3t.change.etco2
    asl.reac.right.prg.precentral.gyrus <- 100 * (asl.chall.right.prg.precentral.gyrus - asl.rest.right.prg.precentral.gyrus) / asl.rest.right.prg.precentral.gyrus / asl.3t.change.etco2
    asl.reac.left.prg.precentral.gyrus <- 100 * (asl.chall.left.prg.precentral.gyrus - asl.rest.left.prg.precentral.gyrus) / asl.rest.left.prg.precentral.gyrus / asl.3t.change.etco2
    asl.reac.right.pt.planum.temporale <- 100 * (asl.chall.right.pt.planum.temporale - asl.rest.right.pt.planum.temporale) / asl.rest.right.pt.planum.temporale / asl.3t.change.etco2
    asl.reac.left.pt.planum.temporale <- 100 * (asl.chall.left.pt.planum.temporale - asl.rest.left.pt.planum.temporale) / asl.rest.left.pt.planum.temporale / asl.3t.change.etco2
    asl.reac.right.sca.subcallosal.area <- 100 * (asl.chall.right.sca.subcallosal.area - asl.rest.right.sca.subcallosal.area) / asl.rest.right.sca.subcallosal.area / asl.3t.change.etco2
    asl.reac.left.sca.subcallosal.area <- 100 * (asl.chall.left.sca.subcallosal.area - asl.rest.left.sca.subcallosal.area) / asl.rest.left.sca.subcallosal.area / asl.3t.change.etco2
    asl.reac.right.sfg.superior.frontal.gyrus <- 100 * (asl.chall.right.sfg.superior.frontal.gyrus - asl.rest.right.sfg.superior.frontal.gyrus) / asl.rest.right.sfg.superior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.left.sfg.superior.frontal.gyrus <- 100 * (asl.chall.left.sfg.superior.frontal.gyrus - asl.rest.left.sfg.superior.frontal.gyrus) / asl.rest.left.sfg.superior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.right.smc.supplementary.motor.cortex <- 100 * (asl.chall.right.smc.supplementary.motor.cortex - asl.rest.right.smc.supplementary.motor.cortex) / asl.rest.right.smc.supplementary.motor.cortex / asl.3t.change.etco2
    asl.reac.left.smc.supplementary.motor.cortex <- 100 * (asl.chall.left.smc.supplementary.motor.cortex - asl.rest.left.smc.supplementary.motor.cortex) / asl.rest.left.smc.supplementary.motor.cortex / asl.3t.change.etco2
    asl.reac.right.smg.supramarginal.gyrus <- 100 * (asl.chall.right.smg.supramarginal.gyrus - asl.rest.right.smg.supramarginal.gyrus) / asl.rest.right.smg.supramarginal.gyrus / asl.3t.change.etco2
    asl.reac.left.smg.supramarginal.gyrus <- 100 * (asl.chall.left.smg.supramarginal.gyrus - asl.rest.left.smg.supramarginal.gyrus) / asl.rest.left.smg.supramarginal.gyrus / asl.3t.change.etco2
    asl.reac.right.sog.superior.occipital.gyrus <- 100 * (asl.chall.right.sog.superior.occipital.gyrus - asl.rest.right.sog.superior.occipital.gyrus) / asl.rest.right.sog.superior.occipital.gyrus / asl.3t.change.etco2
    asl.reac.left.sog.superior.occipital.gyrus <- 100 * (asl.chall.left.sog.superior.occipital.gyrus - asl.rest.left.sog.superior.occipital.gyrus) / asl.rest.left.sog.superior.occipital.gyrus / asl.3t.change.etco2
    asl.reac.right.spl.superior.parietal.lobule <- 100 * (asl.chall.right.spl.superior.parietal.lobule - asl.rest.right.spl.superior.parietal.lobule) / asl.rest.right.spl.superior.parietal.lobule / asl.3t.change.etco2
    asl.reac.left.spl.superior.parietal.lobule <- 100 * (asl.chall.left.spl.superior.parietal.lobule - asl.rest.left.spl.superior.parietal.lobule) / asl.rest.left.spl.superior.parietal.lobule / asl.3t.change.etco2
    asl.reac.right.stg.superior.temporal.gyrus <- 100 * (asl.chall.right.stg.superior.temporal.gyrus - asl.rest.right.stg.superior.temporal.gyrus) / asl.rest.right.stg.superior.temporal.gyrus / asl.3t.change.etco2
    asl.reac.left.stg.superior.temporal.gyrus <- 100 * (asl.chall.left.stg.superior.temporal.gyrus - asl.rest.left.stg.superior.temporal.gyrus) / asl.rest.left.stg.superior.temporal.gyrus / asl.3t.change.etco2
    asl.reac.right.tmp.temporal.pole <- 100 * (asl.chall.right.tmp.temporal.pole - asl.rest.right.tmp.temporal.pole) / asl.rest.right.tmp.temporal.pole / asl.3t.change.etco2
    asl.reac.left.tmp.temporal.pole <- 100 * (asl.chall.left.tmp.temporal.pole - asl.rest.left.tmp.temporal.pole) / asl.rest.left.tmp.temporal.pole / asl.3t.change.etco2
    asl.reac.right.trifg.triangular.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.right.trifg.triangular.part.of.the.inferior.frontal.gyrus - asl.rest.right.trifg.triangular.part.of.the.inferior.frontal.gyrus) / asl.rest.right.trifg.triangular.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.left.trifg.triangular.part.of.the.inferior.frontal.gyrus <- 100 * (asl.chall.left.trifg.triangular.part.of.the.inferior.frontal.gyrus - asl.rest.left.trifg.triangular.part.of.the.inferior.frontal.gyrus) / asl.rest.left.trifg.triangular.part.of.the.inferior.frontal.gyrus / asl.3t.change.etco2
    asl.reac.right.ttg.transverse.temporal.gyrus <- 100 * (asl.chall.right.ttg.transverse.temporal.gyrus - asl.rest.right.ttg.transverse.temporal.gyrus) / asl.rest.right.ttg.transverse.temporal.gyrus / asl.3t.change.etco2
    asl.reac.left.ttg.transverse.temporal.gyrus <- 100 * (asl.chall.left.ttg.transverse.temporal.gyrus - asl.rest.left.ttg.transverse.temporal.gyrus) / asl.rest.left.ttg.transverse.temporal.gyrus / asl.3t.change.etco2

  })

  return(data)
}
