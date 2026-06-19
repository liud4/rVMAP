#' Derive, label, and add AD signature McEvoy and Schwarz variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added AD signature variables.
#' @export

derive_AD_signature_temp <- function(data) {
  # data <- merged.df
  muse.short.name <- Hmisc::Cs(
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
      muse.r.gm.vol = sum(
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
      muse.total.brain.vol = muse.icv.vol - sum(muse.cort.csf.vol, muse.csf.46.vol),
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
    ) %>% as.data.frame()
  
  
  
  schwarz <- Hmisc::Cs(
    rh.entorhinal.thickness,
    rh.inferiortemporal.thickness,
    rh.middletemporal.thickness,
    rh.inferiorparietal.thickness,
    rh.fusiform.thickness,
    rh.precuneus.thickness,
    lh.entorhinal.thickness,
    lh.inferiortemporal.thickness,
    lh.middletemporal.thickness,
    lh.inferiorparietal.thickness,
    lh.fusiform.thickness,
    lh.precuneus.thickness
  )
  
  mcevoy.Y <- Hmisc::Cs(
    avg.hippocampus,
    avg.entorhinal.thickness,
    avg.middletemporal.thickness,
    avg.bankssts.thickness,
    avg.isthmuscingulate.thickness,
    avg.superiortemporal.thickness,
    avg.medialorbitofrontal.thickness,
    avg.lateralorbitofrontal.thickness
  )
  mcevoy.coef <- c(
    0.709, # avg.hippocampus
    0.597, # avg.entorhinal.thickness
    0.506, # avg.middletemporal.thickness
    0.453, # avg.bankssts.thickness
    0.395, # avg.isthmuscingulate.thickness
    0.328, # avg.superiortemporal.thickness
    0.269, # avg.medialorbitofrontal.thickness
    0.250  # avg.lateralorbitofrontal.thickness
  )
  ## create mcevoy.Y variables
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      avg.hippocampus = mean(c(right.hippocampus, left.hippocampus)),
      avg.entorhinal.thickness = mean(c(rh.entorhinal.thickness, lh.entorhinal.thickness)),
      avg.middletemporal.thickness = mean(c(rh.middletemporal.thickness, lh.middletemporal.thickness)),
      avg.bankssts.thickness = mean(c(rh.bankssts.thickness, lh.bankssts.thickness)),
      avg.isthmuscingulate.thickness = mean(c(rh.isthmuscingulate.thickness, lh.isthmuscingulate.thickness)),
      avg.superiortemporal.thickness = mean(c(rh.superiortemporal.thickness, lh.superiortemporal.thickness)),
      avg.medialorbitofrontal.thickness = mean(c(rh.medialorbitofrontal.thickness, lh.medialorbitofrontal.thickness)),
      avg.lateralorbitofrontal.thickness = mean(c(rh.lateralorbitofrontal.thickness, lh.lateralorbitofrontal.thickness))
    )
  
  mcevoy.all.temp <- NULL
  
  derive_mcevoy.df <- data %>%
    dplyr::filter(epoch == 1) %>%
    dplyr::filter(!is.na(avg.hippocampus)) %>%
    dplyr::filter(diagnosis.factor.base == "Normal") %>%
    dplyr::select(
      all_of(
        Cs(map.id, epoch, redcap.repeat.instrument, redcap.repeat.instance,
           age, sex.factor, diagnosis.factor.base, intracranialvol, avg.hippocampus, avg.entorhinal.thickness,
           avg.middletemporal.thickness, avg.bankssts.thickness, avg.isthmuscingulate.thickness,
           avg.superiortemporal.thickness, avg.medialorbitofrontal.thickness, avg.lateralorbitofrontal.thickness)
      )
    )
  
  # derive_mcevoy.df <- droplevels(derive_mcevoy.df)
  
  fit.1 <- lm(formula = as.formula(paste0(mcevoy.Y[1], ' ~ age + sex.factor + intracranialvol')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  
  fit.2 <- lm(formula = as.formula(paste0(mcevoy.Y[2], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.3 <- lm(formula = as.formula(paste0(mcevoy.Y[3], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.4 <- lm(formula = as.formula(paste0(mcevoy.Y[4], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.5 <- lm(formula = as.formula(paste0(mcevoy.Y[5], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.6 <- lm(formula = as.formula(paste0(mcevoy.Y[6], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.7 <- lm(formula = as.formula(paste0(mcevoy.Y[7], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  fit.8 <- lm(formula = as.formula(paste0(mcevoy.Y[8], ' ~ age + sex.factor')),
              data = derive_mcevoy.df,
              na.action = na.exclude)
  data_w_fit.df <- data %>%
    modelr::add_predictions(model = fit.1, var = "mcevoy.fit.1") %>%
    modelr::add_predictions(model = fit.2, var = "mcevoy.fit.2") %>%
    modelr::add_predictions(model = fit.3, var = "mcevoy.fit.3") %>%
    modelr::add_predictions(model = fit.4, var = "mcevoy.fit.4") %>%
    modelr::add_predictions(model = fit.5, var = "mcevoy.fit.5") %>%
    modelr::add_predictions(model = fit.6, var = "mcevoy.fit.6") %>%
    modelr::add_predictions(model = fit.7, var = "mcevoy.fit.7") %>%
    modelr::add_predictions(model = fit.8, var = "mcevoy.fit.8")
  mcevoy.temp <- data.frame(
    map.id = data_w_fit.df$map.id,
    epoch = data_w_fit.df$epoch,
    redcap.repeat.instrument = data_w_fit.df$redcap.repeat.instrument,
    redcap.repeat.instance = data_w_fit.df$redcap.repeat.instance,
    stringsAsFactors = FALSE
  )
  mcevoy.temp$fit.1 <-
    ((data_w_fit.df[, mcevoy.Y[1]] - data_w_fit.df$mcevoy.fit.1) / sd(resid(fit.1), na.rm = TRUE)) * mcevoy.coef[1]
  mcevoy.temp$fit.2 <-
    ((data_w_fit.df[, mcevoy.Y[2]] - data_w_fit.df$mcevoy.fit.2) / sd(resid(fit.2), na.rm = TRUE)) * mcevoy.coef[2]
  mcevoy.temp$fit.3 <-
    ((data_w_fit.df[, mcevoy.Y[3]] - data_w_fit.df$mcevoy.fit.3) / sd(resid(fit.3), na.rm = TRUE)) * mcevoy.coef[3]
  mcevoy.temp$fit.4 <-
    ((data_w_fit.df[, mcevoy.Y[4]] - data_w_fit.df$mcevoy.fit.4) / sd(resid(fit.4), na.rm = TRUE)) * mcevoy.coef[4]
  mcevoy.temp$fit.5 <-
    ((data_w_fit.df[, mcevoy.Y[5]] - data_w_fit.df$mcevoy.fit.5) / sd(resid(fit.5), na.rm = TRUE)) * mcevoy.coef[5]
  mcevoy.temp$fit.6 <-
    ((data_w_fit.df[, mcevoy.Y[6]] - data_w_fit.df$mcevoy.fit.6) / sd(resid(fit.6), na.rm = TRUE)) * mcevoy.coef[6]
  mcevoy.temp$fit.7 <-
    ((data_w_fit.df[, mcevoy.Y[7]] - data_w_fit.df$mcevoy.fit.7) / sd(resid(fit.7), na.rm = TRUE)) * mcevoy.coef[7]
  mcevoy.temp$fit.8 <-
    ((data_w_fit.df[, mcevoy.Y[8]] - data_w_fit.df$mcevoy.fit.8) / sd(resid(fit.8), na.rm = TRUE)) * mcevoy.coef[8]
  
  mcevoy.temp$AD.sig.mcevoy <- rowSums(mcevoy.temp[, c("fit.1", "fit.2", "fit.3", "fit.4", "fit.5", "fit.6", "fit.7", "fit.8")])
  
  mcevoy.temp <- mcevoy.temp %>%
    select(map.id, epoch, redcap.repeat.instrument, redcap.repeat.instance, AD.sig.mcevoy)
  
  data <- left_join(data, mcevoy.temp, by = c("map.id", "epoch", "redcap.repeat.instrument", "redcap.repeat.instance"))
  
  data$AD.sig.schwarz <- rowMeans(data[, schwarz], na.rm = FALSE)
  
  data <- within(data, {
    label(avg.hippocampus) <- "Avg hippocampus vol - T1 3T FS"
    label(avg.entorhinal.thickness) <- "Avg entorhinal cortex thickness - T1 3T FS"
    label(avg.middletemporal.thickness) <- "Avg middletemporal thickness - T1 3T FS"
    label(avg.bankssts.thickness) <- "Avg bankssts thickness - T1 3T FS"
    label(avg.isthmuscingulate.thickness) <- "Avg isthmuscingulate thickness - T1 3T FS"
    label(avg.superiortemporal.thickness) <- "Avg superiortemporal thickness - T1 3T FS"
    label(avg.medialorbitofrontal.thickness) <- "Avg mediaorbitofrontal thickness - T1 3T FS"
    label(avg.lateralorbitofrontal.thickness) <- "Avg lateralorbitofrontal thickness - T1 3T FS"
    label(AD.sig.mcevoy) <- "AD signature - McEvoy"
    label(AD.sig.schwarz) <- "AD signature - Schwarz"
  })
  
  return(data)
}
