#' Derive, label, and add harmonized DLMUSE volume variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added harmonized DLMUSE volume variables.
#' @export

derive_muse_vol_combat <- function(data) {
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
  
  muse.var <- paste0("muse.", gsub("_", ".", muse.short.name), ".vol.combat")
  
  # sanity check
  # sum(!(muse.var %in% names(data)))
  
  # create gray matter volume
  data.new <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      muse.r.fron.inf.gm.vol.combat = sum(
        muse.r.ant.orb.g.vol.combat,
        muse.r.lat.orb.g.vol.combat,
        muse.r.med.orb.g.vol.combat,
        muse.r.pos.orb.g.vol.combat
      ),
      muse.r.fron.insular.gm.vol.combat = sum(
        muse.r.ant.insula.vol.combat,
        muse.r.pos.insula.vol.combat
      ),
      muse.r.fron.lat.gm.vol.combat = sum(
        muse.r.fron.pole.vol.combat,
        muse.r.mid.fron.g.vol.combat,
        muse.r.inf.fron.op.g.vol.combat,
        muse.r.precent.g.vol.combat,
        muse.r.inf.fron.orb.g.vol.combat,
        muse.r.sup.fron.g.vol.combat,
        muse.r.inf.fron.tri.g.vol.combat
      ),
      muse.r.fron.med.gm.vol.combat = sum(
        muse.r.rectus.g.vol.combat,
        muse.r.med.fron.ctx.vol.combat,
        muse.r.med.precent.g.vol.combat,
        muse.r.sup.med.fron.g.vol.combat,
        muse.r.sca.vol.combat,
        muse.r.supp.motor.ctx.vol.combat
      ),
      muse.r.fron.op.gm.vol.combat = sum(
        muse.r.cent.op.vol.combat,
        muse.r.fron.op.vol.combat,
        muse.r.par.op.vol.combat
      ),
      muse.r.fron.gm.vol.combat = sum(
        muse.r.fron.inf.gm.vol.combat,
        muse.r.fron.insular.gm.vol.combat,
        muse.r.fron.lat.gm.vol.combat,
        muse.r.fron.med.gm.vol.combat,
        muse.r.fron.op.gm.vol.combat
      ),
      muse.r.fron.vol.combat = sum(muse.r.fron.gm.vol.combat, muse.r.fron.wm.vol.combat),
      muse.l.fron.inf.gm.vol.combat = sum(
        muse.l.ant.orb.g.vol.combat,
        muse.l.lat.orb.g.vol.combat,
        muse.l.med.orb.g.vol.combat,
        muse.l.pos.orb.g.vol.combat
      ),
      muse.l.fron.insular.gm.vol.combat = sum(
        muse.l.ant.insula.vol.combat,
        muse.l.pos.insula.vol.combat
      ),
      muse.l.fron.lat.gm.vol.combat = sum(
        muse.l.fron.pole.vol.combat,
        muse.l.mid.fron.g.vol.combat,
        muse.l.inf.fron.op.g.vol.combat,
        muse.l.inf.fron.orb.g.vol.combat,
        muse.l.precent.g.vol.combat,
        muse.l.sup.fron.g.vol.combat,
        muse.l.inf.fron.tri.g.vol.combat
      ),
      muse.l.fron.med.gm.vol.combat = sum(
        muse.l.rectus.g.vol.combat,
        muse.l.med.fron.ctx.vol.combat,
        muse.l.med.precent.g.vol.combat,
        muse.l.sup.med.fron.g.vol.combat,
        muse.l.sca.vol.combat,
        muse.l.supp.motor.ctx.vol.combat
      ),
      muse.l.fron.op.gm.vol.combat = sum(
        muse.l.cent.op.vol.combat,
        muse.l.fron.op.vol.combat,
        muse.l.par.op.vol.combat
      ),
      muse.l.fron.gm.vol.combat  = sum(
        muse.l.fron.inf.gm.vol.combat,
        muse.l.fron.insular.gm.vol.combat,
        muse.l.fron.lat.gm.vol.combat,
        muse.l.fron.med.gm.vol.combat,
        muse.l.fron.op.gm.vol.combat
      ),
      muse.l.fron.vol.combat = sum(muse.l.fron.gm.vol.combat, muse.l.fron.wm.vol.combat),
      muse.r.occ.inf.gm.vol.combat = muse.r.occ.fus.g.vol.combat,
      muse.r.occ.lat.gm.vol.combat = sum(
        muse.r.inf.occ.g.vol.combat,
        muse.r.mid.occ.g.vol.combat,
        muse.r.occ.pole.vol.combat,
        muse.r.sup.occ.g.vol.combat
      ),
      muse.r.occ.med.gm.vol.combat = sum(
        muse.r.calc.ctx.vol.combat,
        muse.r.cuneus.vol.combat,
        muse.r.ling.g.vol.combat
      ),
      muse.r.occ.gm.vol.combat = sum(
        muse.r.occ.inf.gm.vol.combat,
        muse.r.occ.lat.gm.vol.combat,
        muse.r.occ.med.gm.vol.combat
      ),
      muse.r.occ.vol.combat = sum(muse.r.occ.gm.vol.combat, muse.r.occ.wm.vol.combat),
      muse.l.occ.inf.gm.vol.combat = muse.l.occ.fus.g.vol.combat,
      muse.l.occ.lat.gm.vol.combat = sum(
        muse.l.inf.occ.g.vol.combat,
        muse.l.mid.occ.g.vol.combat,
        muse.l.occ.pole.vol.combat,
        muse.l.sup.occ.g.vol.combat
      ),
      muse.l.occ.med.gm.vol.combat = sum(
        muse.l.calc.ctx.vol.combat,
        muse.l.cuneus.vol.combat,
        muse.l.ling.g.vol.combat
      ),
      muse.l.occ.gm.vol.combat = sum(
        muse.l.occ.inf.gm.vol.combat,
        muse.l.occ.lat.gm.vol.combat,
        muse.l.occ.med.gm.vol.combat
      ),
      muse.l.occ.vol.combat = sum(muse.l.occ.gm.vol.combat, muse.l.occ.wm.vol.combat),
      muse.r.par.lat.gm.vol.combat = sum(
        muse.r.ang.g.vol.combat,
        muse.r.postcent.g.vol.combat,
        muse.r.supra.g.vol.combat,
        muse.r.sup.par.vol.combat
      ),
      muse.r.par.med.gm.vol.combat = sum(
        muse.r.med.postcent.g.vol.combat,
        muse.r.precuneus.vol.combat
      ),
      muse.r.par.gm.vol.combat = sum(
        muse.r.par.lat.gm.vol.combat,
        muse.r.par.med.gm.vol.combat
      ),
      muse.r.par.vol.combat = sum(muse.r.par.gm.vol.combat, muse.r.par.wm.vol.combat),
      muse.l.par.lat.gm.vol.combat = sum(
        muse.l.ang.g.vol.combat,
        muse.l.postcent.g.vol.combat,
        muse.l.supra.g.vol.combat,
        muse.l.sup.par.vol.combat
      ),
      muse.l.par.med.gm.vol.combat = sum(
        muse.l.med.postcent.g.vol.combat,
        muse.l.precuneus.vol.combat
      ),
      muse.l.par.gm.vol.combat = sum(
        muse.l.par.lat.gm.vol.combat,
        muse.l.par.med.gm.vol.combat
      ),
      muse.l.par.vol.combat = sum(muse.l.par.gm.vol.combat, muse.l.par.wm.vol.combat),
      muse.r.temp.inf.gm.vol.combat = muse.r.fus.g.vol.combat,
      muse.r.temp.lat.gm.vol.combat = sum(
        muse.r.inf.temp.g.vol.combat,
        muse.r.mid.temp.g.vol.combat,
        muse.r.sup.temp.g.vol.combat,
        muse.r.temp.pole.vol.combat
      ),
      muse.r.temp.supra.temp.gm.vol.combat = sum(
        muse.r.pp.vol.combat,
        muse.r.pt.vol.combat,
        muse.r.trans.temp.g.vol.combat
      ),
      muse.r.temp.gm.vol.combat = sum(
        muse.r.temp.inf.gm.vol.combat,
        muse.r.temp.lat.gm.vol.combat,
        muse.r.temp.supra.temp.gm.vol.combat
      ),
      muse.r.temp.vol.combat = sum(muse.r.temp.gm.vol.combat, muse.r.temp.wm.vol.combat),
      muse.l.temp.inf.gm.vol.combat = muse.l.fus.g.vol.combat,
      muse.l.temp.lat.gm.vol.combat = sum(
        muse.l.inf.temp.g.vol.combat,
        muse.l.mid.temp.g.vol.combat,
        muse.l.sup.temp.g.vol.combat,
        muse.l.temp.pole.vol.combat
      ),
      muse.l.temp.supra.temp.gm.vol.combat = sum(
        muse.l.pp.vol.combat,
        muse.l.pt.vol.combat,
        muse.l.trans.temp.g.vol.combat
      ),
      muse.l.temp.gm.vol.combat  = sum(
        muse.l.temp.inf.gm.vol.combat,
        muse.l.temp.lat.gm.vol.combat,
        muse.l.temp.supra.temp.gm.vol.combat
      ),
      muse.l.temp.vol.combat = sum(muse.l.temp.gm.vol.combat, muse.l.temp.wm.vol.combat),
      muse.r.limb.cing.gm.vol.combat = sum(
        muse.r.ant.cing.g.vol.combat,
        muse.r.mid.cing.g.vol.combat,
        muse.r.pos.cing.g.vol.combat
      ),
      muse.r.limb.med.temp.gm.vol.combat = sum(muse.r.ent.vol.combat, muse.r.parahipp.g.vol.combat),
      muse.r.limb.gm.vol.combat = sum(
        muse.r.limb.cing.gm.vol.combat,
        muse.r.limb.med.temp.gm.vol.combat
      ),
      muse.l.limb.cing.gm.vol.combat = sum(
        muse.l.ant.cing.g.vol.combat,
        muse.l.mid.cing.g.vol.combat,
        muse.l.pos.cing.g.vol.combat
      ),
      muse.l.limb.med.temp.gm.vol.combat = sum(muse.l.ent.vol.combat, muse.l.parahipp.g.vol.combat),
      muse.l.limb.gm.vol.combat = sum(
        muse.l.limb.cing.gm.vol.combat,
        muse.l.limb.med.temp.gm.vol.combat
      ),
      muse.fron.inf.gm.vol.combat = sum(
        muse.r.fron.inf.gm.vol.combat,
        muse.l.fron.inf.gm.vol.combat
      ),
      muse.fron.insular.gm.vol.combat = sum(
        muse.r.fron.insular.gm.vol.combat,
        muse.l.fron.insular.gm.vol.combat
      ),
      muse.fron.lat.gm.vol.combat = sum(
        muse.r.fron.lat.gm.vol.combat,
        muse.l.fron.lat.gm.vol.combat
      ),
      muse.fron.med.gm.vol.combat = sum(
        muse.r.fron.med.gm.vol.combat,
        muse.l.fron.med.gm.vol.combat
      ),
      muse.fron.op.gm.vol.combat = sum(
        muse.r.fron.op.gm.vol.combat,
        muse.l.fron.op.gm.vol.combat
      ),
      muse.limb.cing.gm.vol.combat = sum(
        muse.r.limb.cing.gm.vol.combat,
        muse.l.limb.cing.gm.vol.combat
      ),
      muse.limb.med.temp.gm.vol.combat = sum(
        muse.r.limb.med.temp.gm.vol.combat,
        muse.l.limb.med.temp.gm.vol.combat
      ),
      muse.occ.inf.gm.vol.combat = sum(
        muse.r.occ.inf.gm.vol.combat,
        muse.l.occ.inf.gm.vol.combat
      ),
      muse.occ.lat.gm.vol.combat = sum(
        muse.r.occ.lat.gm.vol.combat,
        muse.l.occ.lat.gm.vol.combat
      ),
      muse.occ.med.gm.vol.combat = sum(
        muse.r.occ.med.gm.vol.combat,
        muse.l.occ.med.gm.vol.combat
      ),
      muse.par.lat.gm.vol.combat = sum(
        muse.r.par.lat.gm.vol.combat,
        muse.l.par.lat.gm.vol.combat
      ),
      muse.par.med.gm.vol.combat = sum(
        muse.r.par.med.gm.vol.combat,
        muse.l.par.med.gm.vol.combat
      ),
      muse.temp.inf.gm.vol.combat = sum(
        muse.r.temp.inf.gm.vol.combat,
        muse.l.temp.inf.gm.vol.combat
      ),
      muse.temp.lat.gm.vol.combat = sum(
        muse.r.temp.lat.gm.vol.combat,
        muse.l.temp.lat.gm.vol.combat
      ),
      muse.temp.supra.temp.gm.vol.combat = sum(
        muse.r.temp.supra.temp.gm.vol.combat,
        muse.l.temp.supra.temp.gm.vol.combat
      ),
      muse.fron.gm.vol.combat = sum(muse.r.fron.gm.vol.combat, muse.l.fron.gm.vol.combat),
      muse.fron.wm.vol.combat = sum(muse.r.fron.wm.vol.combat, muse.l.fron.wm.vol.combat),
      muse.fron.vol.combat = sum(muse.r.fron.vol.combat, muse.l.fron.vol.combat),
      muse.limb.gm.vol.combat = sum(muse.r.limb.gm.vol.combat, muse.l.limb.gm.vol.combat),
      muse.occ.gm.vol.combat = sum(muse.r.occ.gm.vol.combat, muse.l.occ.gm.vol.combat),
      muse.occ.wm.vol.combat = sum(muse.r.occ.wm.vol.combat, muse.l.occ.wm.vol.combat),
      muse.occ.vol.combat = sum(muse.r.occ.vol.combat, muse.l.occ.vol.combat),
      muse.par.gm.vol.combat = sum(muse.r.par.gm.vol.combat, muse.l.par.gm.vol.combat),
      muse.par.wm.vol.combat = sum(muse.r.par.wm.vol.combat, muse.l.par.wm.vol.combat),
      muse.par.vol.combat = sum(muse.r.par.vol.combat, muse.l.par.vol.combat),
      muse.temp.gm.vol.combat = sum(muse.r.temp.gm.vol.combat, muse.l.temp.gm.vol.combat),
      muse.temp.wm.vol.combat = sum(muse.r.temp.wm.vol.combat, muse.l.temp.wm.vol.combat),
      muse.temp.vol.combat = sum(muse.r.temp.vol.combat, muse.l.temp.vol.combat),
      muse.r.bg.vol.combat = sum(
        muse.r.accum.vol.combat,
        muse.r.caudate.vol.combat,
        muse.r.pallidum.vol.combat,
        muse.r.putamen.vol.combat,
        muse.r.thal.vol.combat,
        muse.r.bf.vol.combat,
        muse.r.pos.intcap.ped.vol.combat,
        muse.r.ant.intcap.vol.combat
      ),
      muse.l.bg.vol.combat = sum(
        muse.l.accum.vol.combat,
        muse.l.caudate.vol.combat,
        muse.l.pallidum.vol.combat,
        muse.l.putamen.vol.combat,
        muse.l.thal.vol.combat,
        muse.l.bf.vol.combat,
        muse.l.pos.intcap.ped.vol.combat,
        muse.l.ant.intcap.vol.combat
      ),
      muse.bg.vol.combat = sum(muse.r.bg.vol.combat, muse.l.bg.vol.combat),
      muse.hipp.vol.combat = sum(muse.r.hipp.vol.combat, muse.l.hipp.vol.combat),
      muse.amyg.vol.combat = sum(muse.r.amyg.vol.combat, muse.l.amyg.vol.combat),
      muse.r.hipp.amyg.vol.combat = sum(muse.r.hipp.vol.combat, muse.r.amyg.vol.combat),
      muse.l.hipp.amyg.vol.combat = sum(muse.l.hipp.vol.combat, muse.l.amyg.vol.combat),
      muse.hipp.amyg.vol.combat = sum(muse.r.hipp.amyg.vol.combat, muse.l.hipp.amyg.vol.combat),
      muse.r.deep.gm.vol.combat = sum(
        muse.r.hipp.amyg.vol.combat,
        muse.r.accum.vol.combat,
        muse.r.caudate.vol.combat,
        muse.r.pallidum.vol.combat,
        muse.r.putamen.vol.combat,
        muse.r.thal.vol.combat,
        muse.r.bf.vol.combat
      ),
      muse.r.deep.wm.vol.combat  = sum(
        muse.r.fornix.vol.combat,
        muse.r.pos.intcap.ped.vol.combat,
        muse.r.ant.intcap.vol.combat,
        muse.r.ventdc.vol.combat
      ),
      muse.l.deep.gm.vol.combat  = sum(
        muse.l.hipp.amyg.vol.combat,
        muse.l.accum.vol.combat,
        muse.l.caudate.vol.combat,
        muse.l.pallidum.vol.combat,
        muse.l.putamen.vol.combat,
        muse.l.thal.vol.combat,
        muse.l.bf.vol.combat
      ),
      muse.l.deep.wm.vol.combat  = sum(
        muse.l.fornix.vol.combat,
        muse.l.pos.intcap.ped.vol.combat,
        muse.l.ant.intcap.vol.combat,
        muse.l.ventdc.vol.combat
      ),
      muse.deep.gm.vol.combat = sum(muse.r.deep.gm.vol.combat, muse.l.deep.gm.vol.combat),
      muse.deep.wm.vol.combat = sum(muse.r.deep.wm.vol.combat, muse.l.deep.wm.vol.combat),
      muse.deep.wm.gm.vol.combat  = sum(muse.deep.gm.vol.combat, muse.deep.wm.vol.combat),
      muse.l.cc.vol.combat = sum(muse.l.cc.vol.combat),
      muse.r.cc.vol.combat = sum(muse.r.cc.vol.combat),
      muse.cc.vol.combat = sum(muse.l.cc.vol.combat, muse.r.cc.vol.combat),
      muse.l.bs.vol.combat = sum(muse.l.bs.vol.combat),
      muse.r.bs.vol.combat = sum(muse.r.bs.vol.combat),
      muse.bs.vol.combat = sum(muse.l.bs.vol.combat, muse.r.bs.vol.combat),
      muse.l.verm.vol.combat = sum(
        muse.l.cb.verm.1.5.vol.combat,
        muse.l.cb.verm.6.7.vol.combat,
        muse.l.cb.verm.8.10.vol.combat
      ),
      muse.r.verm.vol.combat = sum(
        muse.r.cb.verm.1.5.vol.combat,
        muse.r.cb.verm.6.7.vol.combat,
        muse.r.cb.verm.8.10.vol.combat
      ),
      muse.l.cb.gm.vol.combat = sum(muse.l.cb.ext.vol.combat, muse.l.verm.vol.combat),
      muse.r.cb.gm.vol.combat = sum(muse.r.cb.ext.vol.combat, muse.r.verm.vol.combat),
      muse.l.cb.vol.combat = sum(muse.l.cb.gm.vol.combat, muse.l.cb.wm.vol.combat),
      muse.r.cb.vol.combat = sum(muse.r.cb.gm.vol.combat, muse.r.cb.wm.vol.combat),
      muse.cb.vol.combat = sum(muse.l.cb.vol.combat, muse.r.cb.vol.combat),
      muse.inf.lat.vent.vol.combat = sum(
        muse.r.inf.lat.vent.vol.combat,
        muse.l.inf.lat.vent.vol.combat
      ),
      muse.vent.vol.combat = sum(
        muse.r.inf.lat.vent.vol.combat,
        muse.r.lat.vent.vol.combat,
        muse.l.inf.lat.vent.vol.combat,
        muse.l.lat.vent.vol.combat,
        muse.third.vent.vol.combat,
        muse.fourth.vent.vol.combat
      ),
      muse.r.gm.vol.combat = sum(
        muse.r.deep.gm.vol.combat,
        muse.r.cb.gm.vol.combat,
        muse.r.fron.gm.vol.combat,
        muse.r.limb.gm.vol.combat,
        muse.r.occ.gm.vol.combat,
        muse.r.par.gm.vol.combat,
        muse.r.temp.gm.vol.combat
      ),
      muse.r.wm.vol.combat = sum(
        muse.r.fron.wm.vol.combat,
        muse.r.occ.wm.vol.combat,
        muse.r.par.wm.vol.combat,
        muse.r.temp.wm.vol.combat,
        muse.r.cb.wm.vol.combat,
        muse.r.deep.wm.vol.combat,
        muse.r.cc.vol.combat,
        muse.r.bs.vol.combat
      ),
      muse.l.gm.vol.combat = sum(
        muse.l.deep.gm.vol.combat,
        muse.l.cb.gm.vol.combat,
        muse.l.fron.gm.vol.combat,
        muse.l.limb.gm.vol.combat,
        muse.l.occ.gm.vol.combat,
        muse.l.par.gm.vol.combat,
        muse.l.temp.gm.vol.combat
      ),
      muse.l.wm.vol.combat = sum(
        muse.l.fron.wm.vol.combat,
        muse.l.occ.wm.vol.combat,
        muse.l.par.wm.vol.combat,
        muse.l.temp.wm.vol.combat,
        muse.l.cb.wm.vol.combat,
        muse.l.deep.wm.vol.combat,
        muse.l.cc.vol.combat,
        muse.l.bs.vol.combat
      ),
      muse.gm.vol.combat = sum(muse.r.gm.vol.combat, muse.l.gm.vol.combat),
      muse.wm.vol.combat = sum(muse.r.wm.vol.combat, muse.l.wm.vol.combat),
      muse.csf.vol.combat = sum(
        muse.third.vent.vol.combat,
        muse.fourth.vent.vol.combat,
        muse.r.inf.lat.vent.vol.combat,
        muse.l.inf.lat.vent.vol.combat,
        muse.r.lat.vent.vol.combat,
        muse.l.lat.vent.vol.combat,
        muse.cort.csf.vol.combat,
        muse.csf.46.vol.combat
      ),
      muse.icv.vol.combat = sum(
        muse.gm.vol.combat,
        muse.wm.vol.combat,
        muse.csf.vol.combat
      ),
      muse.total.brain.vol.combat = muse.icv.vol.combat - sum(muse.cort.csf.vol.combat, muse.csf.46.vol.combat),
      muse.parenchyma.vol.combat = sum(muse.gm.vol.combat, muse.wm.vol.combat),
      muse.l.cerebrum.gm.vol.combat = sum(
        muse.l.fron.gm.vol.combat,
        muse.l.limb.gm.vol.combat,
        muse.l.occ.gm.vol.combat,
        muse.l.par.gm.vol.combat,
        muse.l.temp.gm.vol.combat
      ),
      muse.l.cerebrum.wm.vol.combat = sum(
        muse.l.fron.wm.vol.combat,
        muse.l.occ.wm.vol.combat,
        muse.l.par.wm.vol.combat,
        muse.l.temp.wm.vol.combat
      ),
      muse.l.cerebrum.vol.combat = sum(
        muse.l.cerebrum.gm.vol.combat,
        muse.l.cerebrum.wm.vol.combat
      ),
      muse.r.cerebrum.gm.vol.combat = sum(
        muse.r.fron.gm.vol.combat,
        muse.r.limb.gm.vol.combat,
        muse.r.occ.gm.vol.combat,
        muse.r.par.gm.vol.combat,
        muse.r.temp.gm.vol.combat
      ),
      muse.r.cerebrum.wm.vol.combat = sum(
        muse.r.fron.wm.vol.combat,
        muse.r.occ.wm.vol.combat,
        muse.r.par.wm.vol.combat,
        muse.r.temp.wm.vol.combat
      ),
      muse.r.cerebrum.vol.combat = sum(
        muse.r.cerebrum.gm.vol.combat,
        muse.r.cerebrum.wm.vol.combat
      ),
      muse.cerebrum.gm.vol.combat = sum(
        muse.r.cerebrum.gm.vol.combat,
        muse.l.cerebrum.gm.vol.combat
      ),
      muse.cerebrum.wm.vol.combat = sum(
        muse.r.cerebrum.wm.vol.combat,
        muse.l.cerebrum.wm.vol.combat
      ),
      muse.cerebrum.vol.combat = sum(muse.r.cerebrum.vol.combat, muse.l.cerebrum.vol.combat)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  data.new <- within(data.new, {
    Hmisc::label(muse.cerebrum.vol.combat) <- "Cerebrum (Harmonized) [mm3s]"
    Hmisc::label(muse.cerebrum.wm.vol.combat) <- "Cerebrum White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.deep.gm.vol.combat) <- "Deep Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.deep.wm.gm.vol.combat) <- "Deep Brain Structures (Harmonized) [mm3s]"
    Hmisc::label(muse.deep.wm.vol.combat) <- "Deep White matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.inf.gm.vol.combat) <- "Frontal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.insular.gm.vol.combat) <- "Frontal Lobe Insular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.med.gm.vol.combat) <- "Frontal Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.op.gm.vol.combat) <- "Frontal Lobe Opercular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.wm.vol.combat) <- "Frontal Lobe White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.hipp.amyg.vol.combat) <- "Hippocampus and Amygdala (Harmonized) [mm3s]"
    Hmisc::label(muse.hipp.vol.combat) <- "Hippocampus (Harmonized) [mm3s]"
    Hmisc::label(muse.amyg.vol.combat) <- "Amygdala (Harmonized) [mm3s]"
    Hmisc::label(muse.icv.vol.combat) <- "Intracranial Volume (Harmonized) [mm3s]"
    Hmisc::label(muse.l.cerebrum.vol.combat) <- "Left Cerebrum (Harmonized) [mm3s]"
    Hmisc::label(muse.l.deep.gm.vol.combat) <- "Left Deep Brain Structures Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.insular.gm.vol.combat) <- "Left Frontal Lobe Insular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.lat.gm.vol.combat) <- "Left Frontal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.op.gm.vol.combat) <- "Left Frontal Lobe Opercular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.vol.combat) <- "Left Frontal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.l.occ.inf.gm.vol.combat) <- "Left Occipital Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.par.lat.gm.vol.combat) <- "Left Parietal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.temp.lat.gm.vol.combat) <- "Left Temporal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.temp.supra.temp.gm.vol.combat) <- "Left Temporal Lobe Supratemporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.temp.vol.combat) <- "Left Temporal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.gm.vol.combat) <- "Occipital Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.parenchyma.vol.combat) <- "Parenchyma (Harmonized) [mm3s]"
    Hmisc::label(muse.r.cb.gm.vol.combat) <- "Right Cerebellum Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.cerebrum.vol.combat) <- "Right Cerebrum (Harmonized) [mm3s]"
    Hmisc::label(muse.r.cerebrum.wm.vol.combat) <- "Right Cerebrum White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.deep.gm.vol.combat) <- "Right Deep Brain Structures Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.deep.wm.vol.combat) <- "Right Deep Brain Structures White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.insular.gm.vol.combat) <- "Right Deep Brain Structures Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.lat.gm.vol.combat) <- "Right Frontal Lobe Insular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.med.gm.vol.combat) <- "Right Frontal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.op.gm.vol.combat) <- "Right Frontal Lobe Opercular Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.vol.combat) <- "Right Frontal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.r.par.lat.gm.vol.combat) <- "Right Parietal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.temp.inf.gm.vol.combat) <- "Right Temporal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.temp.lat.gm.vol.combat) <- "Right Temporal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.temp.supra.temp.gm.vol.combat) <- "Right Temporal Lobe Supratemporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.temp.vol.combat) <- "Right Temporal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.gm.vol.combat) <- "Temporal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.lat.gm.vol.combat) <- "Temporal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.supra.temp.gm.vol.combat) <- "Temporal Lobe Supratemporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.wm.vol.combat) <- "Temporal Lobe White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.total.brain.vol.combat) <- "Total Brain Volume (Harmonized) [mm3s]"
    Hmisc::label(muse.bg.vol.combat) <- "Basal Ganglia (Harmonized) [mm3s]"
    Hmisc::label(muse.bs.vol.combat) <- "Brain Stem (Harmonized) [mm3s]"
    Hmisc::label(muse.cb.vol.combat) <- "Cerebellum (Harmonized) [mm3s]"
    Hmisc::label(muse.cc.vol.combat) <- "Corpus Callosum (Harmonized) [mm3s]"
    Hmisc::label(muse.cerebrum.gm.vol.combat) <- "Cerebrum Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.csf.vol.combat) <- "CSF (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.gm.vol.combat) <- "Frontal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.lat.gm.vol.combat) <- "Frontal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.fron.vol.combat) <- "Frontal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.gm.vol.combat) <- "Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.bg.vol.combat) <- "Left Basal Ganglia (Harmonized) [mm3s]"
    Hmisc::label(muse.l.cb.gm.vol.combat) <- "Left Cerebellum Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.cb.vol.combat) <- "Left Cerebellum (Harmonized) [mm3s]"
    Hmisc::label(muse.l.cerebrum.gm.vol.combat) <- "Left Cerebrum Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.cerebrum.wm.vol.combat) <- "Left Cerebrum White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.deep.wm.vol.combat) <- "Left Deep Brain Structures White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.gm.vol.combat) <- "Left Frontal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.inf.gm.vol.combat) <- "Left Frontal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.fron.med.gm.vol.combat) <- "Left Frontal Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.gm.vol.combat) <- "Left Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.hipp.amyg.vol.combat) <- "Left Hippocampus and Amygdala (Harmonized) [mm3s]"
    Hmisc::label(muse.l.limb.cing.gm.vol.combat) <- "Left Limbic Cingulate Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.limb.gm.vol.combat) <- "Left Limbic Regions Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.limb.med.temp.gm.vol.combat) <- "Left Limbic Medialtemporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.occ.gm.vol.combat) <- "Left Occipital Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.occ.lat.gm.vol.combat) <- "Left Occipital Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.occ.med.gm.vol.combat) <- "Left Occipital Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.occ.vol.combat) <- "Left Occipital Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.l.par.gm.vol.combat) <- "Left Parietal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.par.med.gm.vol.combat) <- "Left Parietal Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.par.vol.combat) <- "Left Parietal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.l.temp.gm.vol.combat) <- "Left Temporal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.temp.inf.gm.vol.combat) <- "Left Temporal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.l.verm.vol.combat) <- "Left Cerebellar Vermal Lobules (Harmonized) [mm3s]"
    Hmisc::label(muse.l.wm.vol.combat) <- "Left White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.limb.cing.gm.vol.combat) <- "Limbic Regions Cingulate Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.limb.gm.vol.combat) <- "Limbic Regions Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.limb.med.temp.gm.vol.combat) <- "Limbic Regions Medial Temporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.inf.gm.vol.combat) <- "Occipital Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.lat.gm.vol.combat) <- "Occipital Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.med.gm.vol.combat) <- "Occipital Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.vol.combat) <- "Occipital Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.occ.wm.vol.combat) <- "Occipital Lobe White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.par.gm.vol.combat) <- "Parietal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.par.lat.gm.vol.combat) <- "Parietal Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.par.med.gm.vol.combat) <- "Parietal Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.par.vol.combat) <- "Parietal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.par.wm.vol.combat) <- "Parietal Lobe White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.bg.vol.combat) <- "Right Basal Ganglia (Harmonized) [mm3s]"
    Hmisc::label(muse.r.cb.vol.combat) <- "Cerebellum (Harmonized) [mm3s]"
    Hmisc::label(muse.r.cerebrum.gm.vol.combat) <- "Cerebrum Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.gm.vol.combat) <- "Right Frontal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.fron.inf.gm.vol.combat) <- "Right Frontal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.gm.vol.combat) <- "Right Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.hipp.amyg.vol.combat) <- "Right Hippocampus and Amygdala (Harmonized) [mm3s]"
    Hmisc::label(muse.r.limb.cing.gm.vol.combat) <- "Right Limbic Cingulate Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.limb.gm.vol.combat) <- "Right Limbic Regions Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.limb.med.temp.gm.vol.combat) <- "Right Limbic Medialtemporal Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.occ.gm.vol.combat) <- "Right Occipital Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.occ.inf.gm.vol.combat) <- "Right Occipital Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.occ.lat.gm.vol.combat) <- "Right Occipital Lobe Lateral Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.occ.med.gm.vol.combat) <- "Right Occipital Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.occ.vol.combat) <- "Right Occipital Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.r.par.gm.vol.combat) <- "Right Parietal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.par.med.gm.vol.combat) <- "Right Parietal Lobe Medial Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.par.vol.combat) <- "Right Parietal Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.r.temp.gm.vol.combat) <- "Right Temporal Lobe Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.r.verm.vol.combat) <- "Right Cerebellar Vermal Lobules (Harmonized) [mm3s]"
    Hmisc::label(muse.r.wm.vol.combat) <- "Right White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.inf.gm.vol.combat) <- "Right Temporal Lobe Inferior Grey Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.temp.vol.combat) <- "Temoral Lobe (Harmonized) [mm3s]"
    Hmisc::label(muse.vent.vol.combat) <- "Ventricles (Harmonized) [mm3s]"
    Hmisc::label(muse.wm.vol.combat) <- "White Matter (Harmonized) [mm3s]"
    Hmisc::label(muse.inf.lat.vent.vol.combat) <- "Inferior Lateral Ventricles (Harmonized) [mm3s]"
  })
  
  return(data.new)
}
