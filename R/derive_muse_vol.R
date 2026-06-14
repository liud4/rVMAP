#' Derive, label, and add DLMUSE volume variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added AD signature variables.
#' @export

derive_AD_signature_temp <- function(data) {
  # data <- merged.df
  muse.short.name <- Hmisc:::Cs(
    cort_csf,
    third_vent,
    fourth_vent,
    r_accum,
    l_accum,
    r_amyg,
    l_amyg,
    r_bs,
    l_bs,
    r_caudate,
    l_caudate,
    r_cb_ext,
    l_cb_ext,
    r_cb_wm,
    l_cb_wm,
    csf_46,
    r_hipp,
    l_hipp,
    r_inf_lat_vent,
    l_inf_lat_vent,
    r_lat_vent,
    l_lat_vent,
    r_pallidum,
    l_pallidum,
    r_putamen,
    l_putamen,
    r_thal,
    l_thal,
    r_ventdc,
    l_ventdc,
    r_cb_verm_1_5,
    r_cb_verm_6_7,
    r_cb_verm_8_10,
    l_cb_verm_1_5,
    l_cb_verm_6_7,
    l_cb_verm_8_10,
    l_bf,
    r_bf,
    r_fron_wm,
    l_fron_wm,
    r_occ_wm,
    l_occ_wm,
    r_par_wm,
    l_par_wm,
    r_temp_wm,
    l_temp_wm,
    r_fornix,
    l_fornix,
    r_pos_intcap_ped,
    l_pos_intcap_ped,
    r_ant_intcap,
    l_ant_intcap,
    r_cc,
    l_cc,
    r_ant_cing_g,
    l_ant_cing_g,
    r_ant_insula,
    l_ant_insula,
    r_ant_orb_g,
    l_ant_orb_g,
    r_ang_g,
    l_ang_g,
    r_calc_ctx,
    l_calc_ctx,
    r_cent_op,
    l_cent_op,
    r_cuneus,
    l_cuneus,
    r_ent,
    l_ent,
    r_fron_op,
    l_fron_op,
    r_fron_pole,
    l_fron_pole,
    r_fus_g,
    l_fus_g,
    r_rectus_g,
    l_rectus_g,
    r_inf_occ_g,
    l_inf_occ_g,
    r_inf_temp_g,
    l_inf_temp_g,
    r_ling_g,
    l_ling_g,
    r_lat_orb_g,
    l_lat_orb_g,
    r_mid_cing_g,
    l_mid_cing_g,
    r_med_fron_ctx,
    l_med_fron_ctx,
    r_mid_fron_g,
    l_mid_fron_g,
    r_mid_occ_g,
    l_mid_occ_g,
    r_med_orb_g,
    l_med_orb_g,
    r_med_postcent_g,
    l_med_postcent_g,
    r_med_precent_g,
    l_med_precent_g,
    r_sup_med_fron_g,
    l_sup_med_fron_g,
    r_mid_temp_g,
    l_mid_temp_g,
    r_occ_pole,
    l_occ_pole,
    r_occ_fus_g,
    l_occ_fus_g,
    r_inf_fron_op_g,
    l_inf_fron_op_g,
    r_inf_fron_orb_g,
    l_inf_fron_orb_g,
    r_pos_cing_g,
    l_pos_cing_g,
    r_precuneus,
    l_precuneus,
    r_parahipp_g,
    l_parahipp_g,
    r_pos_insula,
    l_pos_insula,
    r_par_op,
    l_par_op,
    r_postcent_g,
    l_postcent_g,
    r_pos_orb_g,
    l_pos_orb_g,
    r_pp,
    l_pp,
    r_precent_g,
    l_precent_g,
    r_pt,
    l_pt,
    r_sca,
    l_sca,
    r_sup_fron_g,
    l_sup_fron_g,
    r_supp_motor_ctx,
    l_supp_motor_ctx,
    r_supra_g,
    l_supra_g,
    r_sup_occ_g,
    l_sup_occ_g,
    r_sup_par,
    l_sup_par,
    r_sup_temp_g,
    l_sup_temp_g,
    r_temp_pole,
    l_temp_pole,
    r_inf_fron_tri_g,
    l_inf_fron_tri_g,
    r_trans_temp_g,
    l_trans_temp_g
  )
  
  muse.var <- paste0("muse.", gsub("_", ".", muse.short.name), ".vol")
  
  # sanity check
  # sum(!(muse.var %in% names(merged.df)))
  
  # create gray matter volume
  data.new <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      muse.r.fron.inf.gm.vol = sum(
        muse.r.ant.orb.g.vol,
        muse.r.lat.orb.g.vol,
        muse.r.med.orb.g.vol,
        muse.r.pos.orb.g.vol
      ),
      muse.r.fron.insular.gm.vol = sum(muse.r.ant.insula.vol, muse.r.pos.insula.vol),
      muse.r.fron.lat.gm.vol = sum(
        muse.r.fron.pole.vol,
        muse.r.mid.fron.g.vol,
        muse.r.inf.fron.op.g.vol,
        muse.r.precent.g.vol,
        muse.r.inf.fron.orb.g.vol,
        muse.r.sup.fron.g.vol,
        muse.r.inf.fron.tri.g.vol
      ),
      muse.r.fron.med.gm.vol = sum(
        muse.r.rectus.g.vol,
        muse.r.med.fron.ctx.vol,
        muse.r.med.precent.g.vol,
        muse.r.sup.med.fron.g.vol,
        muse.r.sca.vol,
        muse.r.supp.motor.ctx.vol
      ),
      muse.r.fron.op.gm.vol = sum(muse.r.cent.op.vol, muse.r.fron.op.vol, muse.r.par.op.vol),
      muse.r.fron.gm.vol = sum(
        muse.r.fron.inf.gm.vol,
        muse.r.fron.insular.gm.vol,
        muse.r.fron.lat.gm.vol,
        muse.r.fron.med.gm.vol,
        muse.r.fron.op.gm.vol
      ),
      muse.r.fron.vol = sum(muse.r.fron.gm.vol, muse.r.fron.wm.vol),
      muse.l.fron.inf.gm.vol = sum(
        muse.l.ant.orb.g.vol,
        muse.l.lat.orb.g.vol,
        muse.l.med.orb.g.vol,
        muse.l.pos.orb.g.vol
      ),
      muse.l.fron.insular.gm.vol = sum(muse.l.ant.insula.vol, muse.l.pos.insula.vol),
      muse.l.fron.lat.gm.vol = sum(
        muse.l.fron.pole.vol,
        muse.l.mid.fron.g.vol,
        muse.l.inf.fron.op.g.vol,
        muse.l.inf.fron.orb.g.vol,
        muse.l.precent.g.vol,
        muse.l.sup.fron.g.vol,
        muse.l.inf.fron.tri.g.vol
      ),
      muse.l.fron.med.gm.vol = sum(
        muse.l.rectus.g.vol,
        muse.l.med.fron.ctx.vol,
        muse.l.med.precent.g.vol,
        muse.l.sup.med.fron.g.vol,
        muse.l.sca.vol,
        muse.l.supp.motor.ctx.vol
      ),
      muse.l.fron.op.gm.vol = sum(muse.l.cent.op.vol, muse.l.fron.op.vol, muse.l.par.op.vol),
      muse.l.fron.gm.vol  = sum(
        muse.l.fron.inf.gm.vol,
        muse.l.fron.insular.gm.vol,
        muse.l.fron.lat.gm.vol,
        muse.l.fron.med.gm.vol,
        muse.l.fron.op.gm.vol
      ),
      muse.l.fron.vol = sum(muse.l.fron.gm.vol, muse.l.fron.wm.vol),
      muse.r.occ.inf.gm.vol = muse.r.occ.fus.g.vol,
      muse.r.occ.lat.gm.vol = sum(
        muse.r.inf.occ.g.vol,
        muse.r.mid.occ.g.vol,
        muse.r.occ.pole.vol,
        muse.r.sup.occ.g.vol
      ),
      muse.r.occ.med.gm.vol = sum(muse.r.calc.ctx.vol, muse.r.cuneus.vol, muse.r.ling.g.vol),
      muse.r.occ.gm.vol = sum(
        muse.r.occ.inf.gm.vol,
        muse.r.occ.lat.gm.vol,
        muse.r.occ.med.gm.vol
      ),
      muse.r.occ.vol    = sum(muse.r.occ.gm.vol, muse.r.occ.wm.vol),
      muse.l.occ.inf.gm.vol = muse.l.occ.fus.g.vol,
      muse.l.occ.lat.gm.vol = sum(
        muse.l.inf.occ.g.vol,
        muse.l.mid.occ.g.vol,
        muse.l.occ.pole.vol,
        muse.l.sup.occ.g.vol
      ),
      muse.l.occ.med.gm.vol = sum(muse.l.calc.ctx.vol, muse.l.cuneus.vol, muse.l.ling.g.vol),
      muse.l.occ.gm.vol = sum(
        muse.l.occ.inf.gm.vol,
        muse.l.occ.lat.gm.vol,
        muse.l.occ.med.gm.vol
      ),
      muse.l.occ.vol    = sum(muse.l.occ.gm.vol, muse.l.occ.wm.vol),
      muse.r.par.lat.gm.vol = sum(
        muse.r.ang.g.vol,
        muse.r.postcent.g.vol,
        muse.r.supra.g.vol,
        muse.r.sup.par.vol
      ),
      muse.r.par.med.gm.vol = sum(muse.r.med.postcent.g.vol, muse.r.precuneus.vol),
      muse.r.par.gm.vol  = sum(muse.r.par.lat.gm.vol, muse.r.par.med.gm.vol),
      muse.r.par.vol     = sum(muse.r.par.gm.vol, muse.r.par.wm.vol),
      muse.l.par.lat.gm.vol = sum(
        muse.l.ang.g.vol,
        muse.l.postcent.g.vol,
        muse.l.supra.g.vol,
        muse.l.sup.par.vol
      ),
      muse.l.par.med.gm.vol = sum(muse.l.med.postcent.g.vol, muse.l.precuneus.vol),
      muse.l.par.gm.vol  = sum(muse.l.par.lat.gm.vol, muse.l.par.med.gm.vol),
      muse.l.par.vol     = sum(muse.l.par.gm.vol, muse.l.par.wm.vol),
      muse.r.temp.inf.gm.vol = muse.r.fus.g.vol,
      muse.r.temp.lat.gm.vol = sum(
        muse.r.inf.temp.g.vol,
        muse.r.mid.temp.g.vol,
        muse.r.sup.temp.g.vol,
        muse.r.temp.pole.vol
      ),
      muse.r.temp.supra.temp.gm.vol = sum(muse.r.pp.vol, muse.r.pt.vol, muse.r.trans.temp.g.vol),
      muse.r.temp.gm.vol  = sum(
        muse.r.temp.inf.gm.vol,
        muse.r.temp.lat.gm.vol,
        muse.r.temp.supra.temp.gm.vol
      ),
      muse.r.temp.vol     = sum(muse.r.temp.gm.vol, muse.r.temp.wm.vol),
      muse.l.temp.inf.gm.vol = muse.l.fus.g.vol,
      muse.l.temp.lat.gm.vol = sum(
        muse.l.inf.temp.g.vol,
        muse.l.mid.temp.g.vol,
        muse.l.sup.temp.g.vol,
        muse.l.temp.pole.vol
      ),
      muse.l.temp.supra.temp.gm.vol = sum(muse.l.pp.vol, muse.l.pt.vol, muse.l.trans.temp.g.vol),
      muse.l.temp.gm.vol  = sum(
        muse.l.temp.inf.gm.vol,
        muse.l.temp.lat.gm.vol,
        muse.l.temp.supra.temp.gm.vol
      ),
      muse.l.temp.vol     = sum(muse.l.temp.gm.vol, muse.l.temp.wm.vol),
      muse.r.limb.cing.gm.vol = sum(
        muse.r.ant.cing.g.vol,
        muse.r.mid.cing.g.vol,
        muse.r.pos.cing.g.vol
      ),
      muse.r.limb.med.temp.gm.vol = sum(muse.r.ent.vol, muse.r.parahipp.g.vol),
      muse.r.limb.gm.vol    = sum(muse.r.limb.cing.gm.vol, muse.r.limb.med.temp.gm.vol),
      muse.l.limb.cing.gm.vol = sum(
        muse.l.ant.cing.g.vol,
        muse.l.mid.cing.g.vol,
        muse.l.pos.cing.g.vol
      ),
      muse.l.limb.med.temp.gm.vol = sum(muse.l.ent.vol, muse.l.parahipp.g.vol),
      muse.l.limb.gm.vol    = sum(muse.l.limb.cing.gm.vol, muse.l.limb.med.temp.gm.vol),
      muse.fron.inf.gm.vol = sum(muse.r.fron.inf.gm.vol, muse.l.fron.inf.gm.vol),
      muse.fron.insular.gm.vol = sum(muse.r.fron.insular.gm.vol, muse.l.fron.insular.gm.vol),
      muse.fron.lat.gm.vol = sum(muse.r.fron.lat.gm.vol, muse.l.fron.lat.gm.vol),
      muse.fron.med.gm.vol = sum(muse.r.fron.med.gm.vol, muse.l.fron.med.gm.vol),
      muse.fron.op.gm.vol = sum(muse.r.fron.op.gm.vol, muse.l.fron.op.gm.vol),
      muse.limb.cing.gm.vol = sum(muse.r.limb.cing.gm.vol, muse.l.limb.cing.gm.vol),
      muse.limb.med.temp.gm.vol = sum(muse.r.limb.med.temp.gm.vol, muse.l.limb.med.temp.gm.vol),
      muse.occ.inf.gm.vol = sum(muse.r.occ.inf.gm.vol, muse.l.occ.inf.gm.vol),
      muse.occ.lat.gm.vol = sum(muse.r.occ.lat.gm.vol, muse.l.occ.lat.gm.vol),
      muse.occ.med.gm.vol = sum(muse.r.occ.med.gm.vol, muse.l.occ.med.gm.vol),
      muse.par.lat.gm.vol = sum(muse.r.par.lat.gm.vol, muse.l.par.lat.gm.vol),
      muse.par.med.gm.vol = sum(muse.r.par.med.gm.vol, muse.l.par.med.gm.vol),
      muse.temp.inf.gm.vol = sum(muse.r.temp.inf.gm.vol, muse.l.temp.inf.gm.vol),
      muse.temp.lat.gm.vol = sum(muse.r.temp.lat.gm.vol, muse.l.temp.lat.gm.vol),
      muse.temp.supra.temp.gm.vol = sum(
        muse.r.temp.supra.temp.gm.vol,
        muse.l.temp.supra.temp.gm.vol
      ),
      muse.fron.gm.vol   = sum(muse.r.fron.gm.vol, muse.l.fron.gm.vol),
      muse.fron.wm.vol   = sum(muse.r.fron.wm.vol, muse.l.fron.wm.vol),
      muse.fron.vol      = sum(muse.r.fron.vol, muse.l.fron.vol),
      muse.limb.gm.vol    = sum(muse.r.limb.gm.vol, muse.l.limb.gm.vol),
      muse.occ.gm.vol = sum(muse.r.occ.gm.vol, muse.l.occ.gm.vol),
      muse.occ.wm.vol = sum(muse.r.occ.wm.vol, muse.l.occ.wm.vol),
      muse.occ.vol    = sum(muse.r.occ.vol, muse.l.occ.vol),
      muse.par.gm.vol  = sum(muse.r.par.gm.vol, muse.l.par.gm.vol),
      muse.par.wm.vol  = sum(muse.r.par.wm.vol, muse.l.par.wm.vol),
      muse.par.vol     = sum(muse.r.par.vol, muse.l.par.vol),
      muse.temp.gm.vol  = sum(muse.r.temp.gm.vol, muse.l.temp.gm.vol),
      muse.temp.wm.vol  = sum(muse.r.temp.wm.vol, muse.l.temp.wm.vol),
      muse.temp.vol     = sum(muse.r.temp.vol, muse.l.temp.vol),
      muse.r.bg.vol = sum(
        muse.r.accum.vol,
        muse.r.caudate.vol,
        muse.r.pallidum.vol,
        muse.r.putamen.vol,
        muse.r.thal.vol,
        muse.r.bf.vol,
        muse.r.pos.intcap.ped.vol,
        muse.r.ant.intcap.vol
      ),
      muse.l.bg.vol = sum(
        muse.l.accum.vol,
        muse.l.caudate.vol,
        muse.l.pallidum.vol,
        muse.l.putamen.vol,
        muse.l.thal.vol,
        muse.l.bf.vol,
        muse.l.pos.intcap.ped.vol,
        muse.l.ant.intcap.vol
      ),
      muse.bg.vol = sum(muse.r.bg.vol, muse.l.bg.vol),
      muse.hipp.vol = sum(muse.r.hipp.vol, muse.l.hipp.vol),
      muse.amyg.vol = sum(muse.r.amyg.vol, muse.l.amyg.vol),
      muse.r.hipp.amyg.vol = sum(muse.r.hipp.vol, muse.r.amyg.vol),
      muse.l.hipp.amyg.vol = sum(muse.l.hipp.vol, muse.l.amyg.vol),
      muse.hipp.amyg.vol = sum(muse.r.hipp.amyg.vol, muse.l.hipp.amyg.vol),
      muse.r.deep.gm.vol  = sum(
        muse.r.hipp.amyg.vol,
        muse.r.accum.vol,
        muse.r.caudate.vol,
        muse.r.pallidum.vol,
        muse.r.putamen.vol,
        muse.r.thal.vol,
        muse.r.bf.vol
      ),
      muse.r.deep.wm.vol  = sum(
        muse.r.fornix.vol,
        muse.r.pos.intcap.ped.vol,
        muse.r.ant.intcap.vol,
        muse.r.ventdc.vol
      ),
      muse.l.deep.gm.vol  = sum(
        muse.l.hipp.amyg.vol,
        muse.l.accum.vol,
        muse.l.caudate.vol,
        muse.l.pallidum.vol,
        muse.l.putamen.vol,
        muse.l.thal.vol,
        muse.l.bf.vol
      ),
      muse.l.deep.wm.vol  = sum(
        muse.l.fornix.vol,
        muse.l.pos.intcap.ped.vol,
        muse.l.ant.intcap.vol,
        muse.l.ventdc.vol
      ),
      muse.deep.gm.vol  = sum(muse.r.deep.gm.vol, muse.l.deep.gm.vol),
      muse.deep.wm.vol  = sum(muse.r.deep.wm.vol, muse.l.deep.wm.vol),
      muse.deep.wm.gm.vol  = sum(muse.deep.gm.vol, muse.deep.wm.vol),
      muse.cc.vol = sum(muse.l.cc.vol, muse.r.cc.vol),
      muse.bs.vol = sum(muse.l.bs.vol, muse.r.bs.vol),
      muse.l.cb.ext.vol = sum(muse.l.cb.ext.vol),
      muse.r.cb.ext.vol = sum(muse.r.cb.ext.vol),
      muse.l.verm.vol = sum(
        muse.l.cb.verm.1.5.vol,
        muse.l.cb.verm.6.7.vol,
        muse.l.cb.verm.8.10.vol
      ),
      muse.r.verm.vol = sum(
        muse.r.cb.verm.1.5.vol,
        muse.r.cb.verm.6.7.vol,
        muse.r.cb.verm.8.10.vol
      ),
      muse.l.cb.gm.vol = sum(muse.l.cb.ext.vol, muse.l.verm.vol),
      muse.r.cb.gm.vol = sum(muse.r.cb.ext.vol, muse.r.verm.vol),
      muse.l.cb.vol = sum(muse.l.cb.gm.vol, muse.l.cb.wm.vol),
      muse.r.cb.vol = sum(muse.r.cb.gm.vol, muse.r.cb.wm.vol),
      muse.cb.vol = sum(muse.l.cb.vol, muse.r.cb.vol),
      muse.vent.vol = sum(
        muse.r.inf.lat.vent.vol,
        muse.r.lat.vent.vol,
        muse.l.inf.lat.vent.vol,
        muse.l.lat.vent.vol,
        muse.third.vent.vol,
        muse.fourth.vent.vol
      ),
      muse.inf.lat.vent.vol = sum(muse.r.inf.lat.vent.vol, muse.l.inf.lat.vent.vol),
      muse.r.gm.vol = sum(
        muse.r.bg.vol,
        muse.r.hipp.amyg.vol,
        muse.r.deep.gm.vol,
        muse.r.cb.gm.vol,
        muse.r.fron.gm.vol,
        muse.r.limb.gm.vol,
        muse.r.occ.gm.vol,
        muse.r.par.gm.vol,
        muse.r.temp.gm.vol
      ),
      muse.r.wm.vol = sum(
        muse.r.fron.wm.vol,
        muse.r.occ.wm.vol,
        muse.r.par.wm.vol,
        muse.r.temp.wm.vol,
        muse.r.cb.wm.vol,
        muse.r.deep.wm.vol,
        muse.r.cc.vol,
        muse.r.bs.vol
      ),
      muse.l.gm.vol = sum(
        muse.l.bg.vol,
        muse.l.hipp.amyg.vol,
        muse.l.deep.gm.vol,
        muse.l.cb.gm.vol,
        muse.l.fron.gm.vol,
        muse.l.limb.gm.vol,
        muse.l.occ.gm.vol,
        muse.l.par.gm.vol,
        muse.l.temp.gm.vol
      ),
      muse.l.wm.vol = sum(
        muse.l.fron.wm.vol,
        muse.l.occ.wm.vol,
        muse.l.par.wm.vol,
        muse.l.temp.wm.vol,
        muse.l.cb.wm.vol,
        muse.l.deep.wm.vol,
        muse.l.cc.vol,
        muse.l.bs.vol
      ),
      muse.gm.vol = sum(muse.r.gm.vol, muse.l.gm.vol),
      muse.wm.vol = sum(muse.r.wm.vol, muse.l.wm.vol),
      muse.csf.vol = sum(
        muse.third.vent.vol,
        muse.fourth.vent.vol,
        muse.r.inf.lat.vent.vol,
        muse.l.inf.lat.vent.vol,
        muse.r.lat.vent.vol,
        muse.l.lat.vent.vol,
        muse.cort.csf.vol,
        muse.csf.46.vol
      ),
      muse.icv.vol = sum(muse.gm.vol, muse.wm.vol, muse.csf.vol),
      muse.total.brain.vol = muse.icv.vol - muse.cort.csf.vol - muse.csf.46.vol,
      muse.parenchyma.vol = sum(muse.gm.vol, muse.wm.vol),
      muse.l.cerebrum.gm.vol = sum(
        muse.l.fron.gm.vol,
        muse.l.limb.gm.vol,
        muse.l.occ.gm.vol,
        muse.l.par.gm.vol,
        muse.l.temp.gm.vol
      ),
      muse.l.cerebrum.wm.vol = sum(
        muse.l.fron.wm.vol,
        muse.l.occ.wm.vol,
        muse.l.par.wm.vol,
        muse.l.temp.wm.vol
      ),
      muse.l.cerebrum.vol = sum(muse.l.cerebrum.gm.vol, muse.l.cerebrum.wm.vol),
      muse.r.cerebrum.gm.vol = sum(
        muse.r.fron.gm.vol,
        muse.r.limb.gm.vol,
        muse.r.occ.gm.vol,
        muse.r.par.gm.vol,
        muse.r.temp.gm.vol
      ),
      muse.r.cerebrum.wm.vol = sum(
        muse.r.fron.wm.vol,
        muse.r.occ.wm.vol,
        muse.r.par.wm.vol,
        muse.r.temp.wm.vol
      ),
      muse.r.cerebrum.vol = sum(muse.r.cerebrum.gm.vol, muse.r.cerebrum.wm.vol),
      muse.cerebrum.gm.vol = sum(muse.r.cerebrum.gm.vol, muse.l.cerebrum.gm.vol),
      muse.cerebrum.wm.vol = sum(muse.r.cerebrum.wm.vol, muse.l.cerebrum.wm.vol),
      muse.cerebrum.vol = sum(muse.r.cerebrum.vol, muse.l.cerebrum.vol)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  data.new <- within(data.new, {
    Hmisc::label(muse.cerebrum.vol) <- "Cerebrum [mm3s]"
    Hmisc::label(muse.cerebrum.wm.vol) <- "Cerebrum White Matter [mm3s]"
    Hmisc::label(muse.deep.gm.vol) <- "Deep Grey Matter [mm3s]"
    Hmisc::label(muse.deep.wm.gm.vol) <- "Deep Brain Structures [mm3s]"
    Hmisc::label(muse.deep.wm.vol) <- "Deep White matter [mm3s]"
    Hmisc::label(muse.fron.inf.gm.vol) <- "Frontal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.fron.insular.gm.vol) <- "Frontal Lobe Insular Grey Matter [mm3s]"
    Hmisc::label(muse.fron.med.gm.vol) <- "Frontal Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.fron.op.gm.vol) <- "Frontal Lobe Opercular Grey Matter [mm3s]"
    Hmisc::label(muse.fron.wm.vol) <- "Frontal Lobe White Matter [mm3s]"
    Hmisc::label(muse.hipp.amyg.vol) <- "Hippocampus and Amygdala [mm3s]"
    Hmisc::label(muse.hipp.vol) <- "Hippocampus [mm3s]"
    Hmisc::label(muse.amyg.vol) <- "Amygdala [mm3s]"
    Hmisc::label(muse.icv.vol) <- "Intracranial Volume [mm3s]"
    Hmisc::label(muse.l.cerebrum.vol) <- "Left Cerebrum [mm3s]"
    Hmisc::label(muse.l.deep.gm.vol) <- "Left Deep Brain Structures Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.insular.gm.vol) <- "Left Frontal Lobe Insular Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.lat.gm.vol) <- "Left Frontal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.op.gm.vol) <- "Left Frontal Lobe Opercular Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.vol) <- "Left Frontal Lobe [mm3s]"
    Hmisc::label(muse.l.occ.inf.gm.vol) <- "Left Occipital Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.l.par.lat.gm.vol) <- "Left Parietal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.l.temp.lat.gm.vol) <- "Left Temporal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.l.temp.supra.temp.gm.vol) <- "Left Temporal Lobe Supratemporal Grey Matter [mm3s]"
    Hmisc::label(muse.l.temp.vol) <- "Left Temporal Lobe [mm3s]"
    Hmisc::label(muse.occ.gm.vol) <- "Occipital Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.parenchyma.vol) <- "Parenchyma [mm3s]"
    Hmisc::label(muse.r.cb.gm.vol) <- "Right Cerebellum Grey Matter [mm3s]"
    Hmisc::label(muse.r.cerebrum.vol) <- "Right Cerebrum [mm3s]"
    Hmisc::label(muse.r.cerebrum.wm.vol) <- "Right Cerebrum White Matter [mm3s]"
    Hmisc::label(muse.r.deep.gm.vol) <- "Right Deep Brain Structures Grey Matter [mm3s]"
    Hmisc::label(muse.r.deep.wm.vol) <- "Right Deep Brain Structures White Matter [mm3s]"
    Hmisc::label(muse.r.fron.insular.gm.vol) <- "Right Deep Brain Structures Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.lat.gm.vol) <- "Right Frontal Lobe Insular Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.med.gm.vol) <- "Right Frontal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.op.gm.vol) <- "Right Frontal Lobe Opercular Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.vol) <- "Right Frontal Lobe [mm3s]"
    Hmisc::label(muse.r.par.lat.gm.vol) <- "Right Parietal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.r.temp.inf.gm.vol) <- "Right Temporal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.r.temp.lat.gm.vol) <- "Right Temporal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.r.temp.supra.temp.gm.vol) <- "Right Temporal Lobe Supratemporal Grey Matter [mm3s]"
    Hmisc::label(muse.r.temp.vol) <- "Right Temporal Lobe [mm3s]"
    Hmisc::label(muse.temp.gm.vol) <- "Temporal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.temp.lat.gm.vol) <- "Temporal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.temp.supra.temp.gm.vol) <- "Temporal Lobe Supratemporal Grey Matter [mm3s]"
    Hmisc::label(muse.temp.wm.vol) <- "Temporal Lobe White Matter [mm3s]"
    Hmisc::label(muse.total.brain.vol) <- "Total Brain Volume [mm3s]"
    Hmisc::label(muse.bg.vol) <- "Basal Ganglia [mm3s]"
    Hmisc::label(muse.bs.vol) <- "Brain Stem [mm3s]"
    Hmisc::label(muse.cb.vol) <- "Cerebellum [mm3s]"
    Hmisc::label(muse.cc.vol) <- "Corpus Callosum [mm3s]"
    Hmisc::label(muse.cerebrum.gm.vol) <- "Cerebrum Grey Matter [mm3s]"
    Hmisc::label(muse.csf.vol) <- "CSF [mm3s]"
    Hmisc::label(muse.fron.gm.vol) <- "Frontal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.fron.lat.gm.vol) <- "Frontal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.fron.vol) <- "Frontal Lobe [mm3s]"
    Hmisc::label(muse.gm.vol) <- "Grey Matter [mm3s]"
    Hmisc::label(muse.l.bg.vol) <- "Left Basal Ganglia [mm3s]"
    Hmisc::label(muse.l.cb.gm.vol) <- "Left Cerebellum Grey Matter [mm3s]"
    Hmisc::label(muse.l.cb.vol) <- "Left Cerebellum [mm3s]"
    Hmisc::label(muse.l.cerebrum.gm.vol) <- "Left Cerebrum Grey Matter [mm3s]"
    Hmisc::label(muse.l.cerebrum.wm.vol) <- "Left Cerebrum White Matter [mm3s]"
    Hmisc::label(muse.l.deep.wm.vol) <- "Left Deep Brain Structures White Matter [mm3s]"
    Hmisc::label(muse.l.fron.gm.vol) <- "Left Frontal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.inf.gm.vol) <- "Left Frontal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.l.fron.med.gm.vol) <- "Left Frontal Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.l.gm.vol) <- "Left Grey Matter [mm3s]"
    Hmisc::label(muse.l.hipp.amyg.vol) <- "Left Hippocampus and Amygdala [mm3s]"
    Hmisc::label(muse.l.limb.cing.gm.vol) <- "Left Limbic Cingulate Grey Matter [mm3s]"
    Hmisc::label(muse.l.limb.gm.vol) <- "Left Limbic Regions Grey Matter [mm3s]"
    Hmisc::label(muse.l.limb.med.temp.gm.vol) <- "Left Limbic Medialtemporal Grey Matter [mm3s]"
    Hmisc::label(muse.l.occ.gm.vol) <- "Left Occipital Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.l.occ.lat.gm.vol) <- "Left Occipital Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.l.occ.med.gm.vol) <- "Left Occipital Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.l.occ.vol) <- "Left Occipital Lobe [mm3s]"
    Hmisc::label(muse.l.par.gm.vol) <- "Left Parietal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.l.par.med.gm.vol) <- "Left Parietal Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.l.par.vol) <- "Left Parietal Lobe [mm3s]"
    Hmisc::label(muse.l.temp.gm.vol) <- "Left Temporal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.l.temp.inf.gm.vol) <- "Left Temporal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.l.verm.vol) <- "Left Cerebellar Vermal Lobules [mm3s]"
    Hmisc::label(muse.l.wm.vol) <- "Left White Matter [mm3s]"
    Hmisc::label(muse.limb.cing.gm.vol) <- "Limbic Regions Cingulate Grey Matter [mm3s]"
    Hmisc::label(muse.limb.gm.vol) <- "Limbic Regions Grey Matter [mm3s]"
    Hmisc::label(muse.limb.med.temp.gm.vol) <- "Limbic Regions Medial Temporal Grey Matter [mm3s]"
    Hmisc::label(muse.occ.inf.gm.vol) <- "Occipital Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.occ.lat.gm.vol) <- "Occipital Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.occ.med.gm.vol) <- "Occipital Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.occ.vol) <- "Occipital Lobe [mm3s]"
    Hmisc::label(muse.occ.wm.vol) <- "Occipital Lobe White Matter [mm3s]"
    Hmisc::label(muse.par.gm.vol) <- "Parietal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.par.lat.gm.vol) <- "Parietal Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.par.med.gm.vol) <- "Parietal Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.par.vol) <- "Parietal Lobe [mm3s]"
    Hmisc::label(muse.par.wm.vol) <- "Parietal Lobe White Matter [mm3s]"
    Hmisc::label(muse.r.bg.vol) <- "Right Basal Ganglia [mm3s]"
    Hmisc::label(muse.r.cb.vol) <- "Cerebellum [mm3s]"
    Hmisc::label(muse.r.cerebrum.gm.vol) <- "Cerebrum Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.gm.vol) <- "Right Frontal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.r.fron.inf.gm.vol) <- "Right Frontal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.r.gm.vol) <- "Right Grey Matter [mm3s]"
    Hmisc::label(muse.r.hipp.amyg.vol) <- "Right Hippocampus and Amygdala [mm3s]"
    Hmisc::label(muse.r.limb.cing.gm.vol) <- "Right Limbic Cingulate Grey Matter [mm3s]"
    Hmisc::label(muse.r.limb.gm.vol) <- "Right Limbic Regions Grey Matter [mm3s]"
    Hmisc::label(muse.r.limb.med.temp.gm.vol) <- "Right Limbic Medialtemporal Grey Matter [mm3s]"
    Hmisc::label(muse.r.occ.gm.vol) <- "Right Occipital Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.r.occ.inf.gm.vol) <- "Right Occipital Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.r.occ.lat.gm.vol) <- "Right Occipital Lobe Lateral Grey Matter [mm3s]"
    Hmisc::label(muse.r.occ.med.gm.vol) <- "Right Occipital Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.r.occ.vol) <- "Right Occipital Lobe [mm3s]"
    Hmisc::label(muse.r.par.gm.vol) <- "Right Parietal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.r.par.med.gm.vol) <- "Right Parietal Lobe Medial Grey Matter [mm3s]"
    Hmisc::label(muse.r.par.vol) <- "Right Parietal Lobe [mm3s]"
    Hmisc::label(muse.r.temp.gm.vol) <- "Right Temporal Lobe Grey Matter [mm3s]"
    Hmisc::label(muse.r.verm.vol) <- "Right Cerebellar Vermal Lobules [mm3s]"
    Hmisc::label(muse.r.wm.vol) <- "Right White Matter [mm3s]"
    Hmisc::label(muse.temp.inf.gm.vol) <- "Right Temporal Lobe Inferior Grey Matter [mm3s]"
    Hmisc::label(muse.temp.vol) <- "Temoral Lobe [mm3s]"
    Hmisc::label(muse.vent.vol) <- "Ventricles [mm3s]"
    Hmisc::label(muse.wm.vol) <- "White Matter [mm3s]"
    Hmisc::label(muse.inf.lat.vent.vol) <- "Inferior Lateral Ventricles [mm3s]"
  })
  
  return(data.new)
}
