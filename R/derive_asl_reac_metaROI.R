#' Derive, label, and add ASL MUSE reactivity CVR variables (metaROI) to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added ASL MUSE reactivity CVR variables (metaROI).
#' @export

derive_asl_reac_metaROI <- function(data) {
  # data <- merged.df
  meta.roi.name <- c(
    'l_fron_gm',
    'r_fron_gm',
    'fron_gm',
    'l_limb_gm',
    'r_limb_gm',
    'limb_gm',
    'l_occ_gm',
    'r_occ_gm',
    'occ_gm',
    'l_par_gm',
    'r_par_gm',
    'par_gm',
    'l_temp_gm',
    'r_temp_gm',
    'temp_gm',
    'l_deep_gm',
    'r_deep_gm',
    'deep_gm',
    'cerebrum_gm',
    'gm',
    'cb_gm'
  )
  
  ###############
  ### Frontal ###
  ###############
  
  asl.muse.reac.l.fron.gm.var <- c(
    "asl.muse.reac.l.ant.orb.g.cbf.hct",
    "asl.muse.reac.l.lat.orb.g.cbf.hct",
    "asl.muse.reac.l.med.orb.g.cbf.hct",
    "asl.muse.reac.l.pos.orb.g.cbf.hct",
    "asl.muse.reac.l.ant.insula.cbf.hct",
    "asl.muse.reac.l.pos.insula.cbf.hct",
    "asl.muse.reac.l.fron.pole.cbf.hct",
    "asl.muse.reac.l.mid.fron.g.cbf.hct",
    "asl.muse.reac.l.inf.fron.op.g.cbf.hct",
    "asl.muse.reac.l.inf.fron.orb.g.cbf.hct",
    "asl.muse.reac.l.precent.g.cbf.hct",
    "asl.muse.reac.l.sup.fron.g.cbf.hct",
    "asl.muse.reac.l.inf.fron.tri.g.cbf.hct",
    "asl.muse.reac.l.rectus.g.cbf.hct",
    "asl.muse.reac.l.med.fron.ctx.cbf.hct",
    "asl.muse.reac.l.med.precent.g.cbf.hct",
    "asl.muse.reac.l.sup.med.fron.g.cbf.hct",
    "asl.muse.reac.l.sca.cbf.hct",
    "asl.muse.reac.l.supp.motor.ctx.cbf.hct",
    "asl.muse.reac.l.cent.op.cbf.hct",
    "asl.muse.reac.l.fron.op.cbf.hct",
    "asl.muse.reac.l.par.op.cbf.hct"
  )
  
  asl.muse.reac.l.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.fron.gm.var)
  asl.muse.reac.l.fron.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.fron.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.fron.gm.var,  asl.muse.reac.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.fron.gm.var <- c(
    "asl.muse.reac.r.ant.orb.g.cbf.hct",
    "asl.muse.reac.r.lat.orb.g.cbf.hct",
    "asl.muse.reac.r.med.orb.g.cbf.hct",
    "asl.muse.reac.r.pos.orb.g.cbf.hct",
    "asl.muse.reac.r.ant.insula.cbf.hct",
    "asl.muse.reac.r.pos.insula.cbf.hct",
    "asl.muse.reac.r.fron.pole.cbf.hct",
    "asl.muse.reac.r.mid.fron.g.cbf.hct",
    "asl.muse.reac.r.inf.fron.op.g.cbf.hct",
    "asl.muse.reac.r.inf.fron.orb.g.cbf.hct",
    "asl.muse.reac.r.precent.g.cbf.hct",
    "asl.muse.reac.r.sup.fron.g.cbf.hct",
    "asl.muse.reac.r.inf.fron.tri.g.cbf.hct",
    "asl.muse.reac.r.rectus.g.cbf.hct",
    "asl.muse.reac.r.med.fron.ctx.cbf.hct",
    "asl.muse.reac.r.med.precent.g.cbf.hct",
    "asl.muse.reac.r.sup.med.fron.g.cbf.hct",
    "asl.muse.reac.r.sca.cbf.hct",
    "asl.muse.reac.r.supp.motor.ctx.cbf.hct",
    "asl.muse.reac.r.cent.op.cbf.hct",
    "asl.muse.reac.r.fron.op.cbf.hct",
    "asl.muse.reac.r.par.op.cbf.hct"
  )
  
  asl.muse.reac.r.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.fron.gm.var)
  asl.muse.reac.r.fron.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.fron.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.fron.gm.var,  asl.muse.reac.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.fron.gm.var <- c(asl.muse.reac.l.fron.gm.var, asl.muse.reac.r.fron.gm.var)
  asl.muse.reac.fron.gm.regvol <- c(asl.muse.reac.l.fron.gm.regvol,
                                    asl.muse.reac.r.fron.gm.regvol)
  
  # create `asl.muse.reac.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##############
  ### Limbic ###
  ##############
  
  asl.muse.reac.l.limb.gm.var <- c(
    "asl.muse.reac.l.ant.cing.g.cbf.hct",
    "asl.muse.reac.l.mid.cing.g.cbf.hct",
    "asl.muse.reac.l.pos.cing.g.cbf.hct",
    "asl.muse.reac.l.ent.cbf.hct",
    "asl.muse.reac.l.parahipp.g.cbf.hct"
  )
  
  asl.muse.reac.l.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.limb.gm.var)
  asl.muse.reac.l.limb.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.limb.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.limb.gm.var,  asl.muse.reac.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.limb.gm.var <- c(
    "asl.muse.reac.r.ant.cing.g.cbf.hct",
    "asl.muse.reac.r.mid.cing.g.cbf.hct",
    "asl.muse.reac.r.pos.cing.g.cbf.hct",
    "asl.muse.reac.r.ent.cbf.hct",
    "asl.muse.reac.r.parahipp.g.cbf.hct"
  )
  
  asl.muse.reac.r.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.limb.gm.var)
  asl.muse.reac.r.limb.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.limb.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.limb.gm.var,  asl.muse.reac.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.limb.gm.var <- c(asl.muse.reac.l.limb.gm.var, asl.muse.reac.r.limb.gm.var)
  asl.muse.reac.limb.gm.regvol <- c(asl.muse.reac.l.limb.gm.regvol,
                                    asl.muse.reac.r.limb.gm.regvol)
  
  # create `asl.muse.reac.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #################
  ### Occipital ###
  #################
  
  asl.muse.reac.l.occ.gm.var <- c(
    "asl.muse.reac.l.occ.fus.g.cbf.hct",
    "asl.muse.reac.l.inf.occ.g.cbf.hct",
    "asl.muse.reac.l.mid.occ.g.cbf.hct",
    "asl.muse.reac.l.occ.pole.cbf.hct",
    "asl.muse.reac.l.sup.occ.g.cbf.hct",
    "asl.muse.reac.l.calc.ctx.cbf.hct",
    "asl.muse.reac.l.cuneus.cbf.hct",
    "asl.muse.reac.l.ling.g.cbf.hct"
  )
  
  asl.muse.reac.l.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.occ.gm.var)
  asl.muse.reac.l.occ.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.occ.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.occ.gm.var,  asl.muse.reac.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.occ.gm.var <- c(
    "asl.muse.reac.r.occ.fus.g.cbf.hct",
    "asl.muse.reac.r.inf.occ.g.cbf.hct",
    "asl.muse.reac.r.mid.occ.g.cbf.hct",
    "asl.muse.reac.r.occ.pole.cbf.hct",
    "asl.muse.reac.r.sup.occ.g.cbf.hct",
    "asl.muse.reac.r.calc.ctx.cbf.hct",
    "asl.muse.reac.r.cuneus.cbf.hct",
    "asl.muse.reac.r.ling.g.cbf.hct"
  )
  
  asl.muse.reac.r.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.occ.gm.var)
  asl.muse.reac.r.occ.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.occ.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.occ.gm.var,  asl.muse.reac.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.occ.gm.var <- c(asl.muse.reac.l.occ.gm.var, asl.muse.reac.r.occ.gm.var)
  asl.muse.reac.occ.gm.regvol <- c(asl.muse.reac.l.occ.gm.regvol,
                                   asl.muse.reac.r.occ.gm.regvol)
  
  # create `asl.muse.reac.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Parietal ###
  ################
  
  asl.muse.reac.l.par.gm.var <- c(
    "asl.muse.reac.l.ang.g.cbf.hct",
    "asl.muse.reac.l.postcent.g.cbf.hct",
    "asl.muse.reac.l.supra.g.cbf.hct",
    "asl.muse.reac.l.sup.par.cbf.hct",
    "asl.muse.reac.l.med.postcent.g.cbf.hct",
    "asl.muse.reac.l.precuneus.cbf.hct"
  )
  
  asl.muse.reac.l.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.par.gm.var)
  asl.muse.reac.l.par.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.par.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.par.gm.var,  asl.muse.reac.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.par.gm.var <- c(
    "asl.muse.reac.r.ang.g.cbf.hct",
    "asl.muse.reac.r.postcent.g.cbf.hct",
    "asl.muse.reac.r.supra.g.cbf.hct",
    "asl.muse.reac.r.sup.par.cbf.hct",
    "asl.muse.reac.r.med.postcent.g.cbf.hct",
    "asl.muse.reac.r.precuneus.cbf.hct"
  )
  
  asl.muse.reac.r.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.par.gm.var)
  asl.muse.reac.r.par.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.par.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.par.gm.var,  asl.muse.reac.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.par.gm.var <- c(asl.muse.reac.l.par.gm.var, asl.muse.reac.r.par.gm.var)
  asl.muse.reac.par.gm.regvol <- c(asl.muse.reac.l.par.gm.regvol,
                                   asl.muse.reac.r.par.gm.regvol)
  
  # create `asl.muse.reac.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Temporal ###
  ################
  
  asl.muse.reac.l.temp.gm.var <- c(
    "asl.muse.reac.l.fus.g.cbf.hct",
    "asl.muse.reac.l.inf.temp.g.cbf.hct",
    "asl.muse.reac.l.mid.temp.g.cbf.hct",
    "asl.muse.reac.l.sup.temp.g.cbf.hct",
    "asl.muse.reac.l.temp.pole.cbf.hct",
    "asl.muse.reac.l.pp.cbf.hct",
    "asl.muse.reac.l.pt.cbf.hct",
    "asl.muse.reac.l.trans.temp.g.cbf.hct"
  )
  
  asl.muse.reac.l.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.temp.gm.var)
  asl.muse.reac.l.temp.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.temp.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.temp.gm.var,  asl.muse.reac.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.temp.gm.var <- c(
    "asl.muse.reac.r.fus.g.cbf.hct",
    "asl.muse.reac.r.inf.temp.g.cbf.hct",
    "asl.muse.reac.r.mid.temp.g.cbf.hct",
    "asl.muse.reac.r.sup.temp.g.cbf.hct",
    "asl.muse.reac.r.temp.pole.cbf.hct",
    "asl.muse.reac.r.pp.cbf.hct",
    "asl.muse.reac.r.pt.cbf.hct",
    "asl.muse.reac.r.trans.temp.g.cbf.hct"
  )
  
  asl.muse.reac.r.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.temp.gm.var)
  asl.muse.reac.r.temp.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.temp.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.temp.gm.var,  asl.muse.reac.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.temp.gm.var <- c(asl.muse.reac.l.temp.gm.var, asl.muse.reac.r.temp.gm.var)
  asl.muse.reac.temp.gm.regvol <- c(asl.muse.reac.l.temp.gm.regvol,
                                    asl.muse.reac.r.temp.gm.regvol)
  
  # create `asl.muse.reac.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ############
  ### Deep ###
  ############
  
  asl.muse.reac.l.deep.gm.var <- c(
    "asl.muse.reac.l.hipp.cbf.hct",
    "asl.muse.reac.l.amyg.cbf.hct",
    "asl.muse.reac.l.accum.cbf.hct",
    "asl.muse.reac.l.caudate.cbf.hct",
    "asl.muse.reac.l.pallidum.cbf.hct",
    "asl.muse.reac.l.putamen.cbf.hct",
    "asl.muse.reac.l.thal.cbf.hct",
    "asl.muse.reac.l.bf.cbf.hct"
  )
  
  asl.muse.reac.l.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.l.deep.gm.var)
  asl.muse.reac.l.deep.gm.regvol <- gsub("reac", "rest", asl.muse.reac.l.deep.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.deep.gm.var,  asl.muse.reac.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.deep.gm.var <- c(
    "asl.muse.reac.r.hipp.cbf.hct",
    "asl.muse.reac.r.amyg.cbf.hct",
    "asl.muse.reac.r.accum.cbf.hct",
    "asl.muse.reac.r.caudate.cbf.hct",
    "asl.muse.reac.r.pallidum.cbf.hct",
    "asl.muse.reac.r.putamen.cbf.hct",
    "asl.muse.reac.r.thal.cbf.hct",
    "asl.muse.reac.r.bf.cbf.hct"
  )
  
  asl.muse.reac.r.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.r.deep.gm.var)
  asl.muse.reac.r.deep.gm.regvol <- gsub("reac", "rest", asl.muse.reac.r.deep.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.deep.gm.var,  asl.muse.reac.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.deep.gm.var <- c(asl.muse.reac.l.deep.gm.var, asl.muse.reac.r.deep.gm.var)
  asl.muse.reac.deep.gm.regvol <- c(asl.muse.reac.l.deep.gm.regvol,
                                    asl.muse.reac.r.deep.gm.regvol)
  
  # create `asl.muse.reac.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Cerebrum ###
  ################
  
  asl.muse.reac.cerebrum.gm.var <- c(
    asl.muse.reac.fron.gm.var,
    asl.muse.reac.limb.gm.var,
    asl.muse.reac.occ.gm.var,
    asl.muse.reac.par.gm.var,
    asl.muse.reac.temp.gm.var
  )
  
  asl.muse.reac.cerebrum.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.cerebrum.gm.var)
  asl.muse.reac.cerebrum.gm.regvol <- gsub("reac", "rest", asl.muse.reac.cerebrum.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.cerebrum.gm.var,  asl.muse.reac.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.cerebrum.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.cerebrum.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cerebrum.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Cerebellum ###
  ##################
  
  asl.muse.reac.cb.gm.var <- c(
    "asl.muse.reac.l.cb.ext.cbf.hct",
    "asl.muse.reac.l.cb.verm.1.5.cbf.hct",
    "asl.muse.reac.l.cb.verm.6.7.cbf.hct",
    "asl.muse.reac.l.cb.verm.8.10.cbf.hct",
    "asl.muse.reac.r.cb.ext.cbf.hct",
    "asl.muse.reac.r.cb.verm.1.5.cbf.hct",
    "asl.muse.reac.r.cb.verm.6.7.cbf.hct",
    "asl.muse.reac.r.cb.verm.8.10.cbf.hct"
  )
  
  asl.muse.reac.cb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.cb.gm.var)
  asl.muse.reac.cb.gm.regvol <- gsub("reac", "rest", asl.muse.reac.cb.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.cb.gm.var,  asl.muse.reac.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.cb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.cb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.cb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Total GM ###
  ################
  
  asl.muse.reac.gm.var <- c(
    asl.muse.reac.l.deep.gm.var,
    asl.muse.reac.l.fron.gm.var,
    asl.muse.reac.l.limb.gm.var,
    asl.muse.reac.l.occ.gm.var,
    asl.muse.reac.l.par.gm.var,
    asl.muse.reac.l.temp.gm.var,
    asl.muse.reac.r.deep.gm.var,
    asl.muse.reac.r.fron.gm.var,
    asl.muse.reac.r.limb.gm.var,
    asl.muse.reac.r.occ.gm.var,
    asl.muse.reac.r.par.gm.var,
    asl.muse.reac.r.temp.gm.var,
    asl.muse.reac.cb.gm.var
  )
  
  asl.muse.reac.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.reac.gm.var)
  asl.muse.reac.gm.regvol <- gsub("reac", "rest", asl.muse.reac.gm.regvol)
  
  # sanity check
  # sum(!(c(asl.muse.reac.gm.var,  asl.muse.reac.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.reac.gm.cbf.hct =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.reac.gm.var
                    )) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.reac.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.reac.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.reac.var <- paste0("asl.muse.reac.", gsub("_", ".", meta.roi.name), ".cbf.hct")
  # sum(!asl.muse.reac.var %in% names(data))
  
  ##----------------------------------------------------------------------------
  ##----------------------------------------------------------------------------
  
  ####################
  ### Frontal (PVC)###
  ####################
  
  asl.muse.reac.l.fron.gm.var.pvc <- c(
    "asl.muse.reac.l.ant.orb.g.cbf.hct.pvc",
    "asl.muse.reac.l.lat.orb.g.cbf.hct.pvc",
    "asl.muse.reac.l.med.orb.g.cbf.hct.pvc",
    "asl.muse.reac.l.pos.orb.g.cbf.hct.pvc",
    "asl.muse.reac.l.ant.insula.cbf.hct.pvc",
    "asl.muse.reac.l.pos.insula.cbf.hct.pvc",
    "asl.muse.reac.l.fron.pole.cbf.hct.pvc",
    "asl.muse.reac.l.mid.fron.g.cbf.hct.pvc",
    "asl.muse.reac.l.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.reac.l.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.reac.l.precent.g.cbf.hct.pvc",
    "asl.muse.reac.l.sup.fron.g.cbf.hct.pvc",
    "asl.muse.reac.l.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.reac.l.rectus.g.cbf.hct.pvc",
    "asl.muse.reac.l.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.reac.l.med.precent.g.cbf.hct.pvc",
    "asl.muse.reac.l.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.reac.l.sca.cbf.hct.pvc",
    "asl.muse.reac.l.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.reac.l.cent.op.cbf.hct.pvc",
    "asl.muse.reac.l.fron.op.cbf.hct.pvc",
    "asl.muse.reac.l.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.fron.gm.var.pvc,  asl.muse.reac.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.fron.gm.var.pvc <- c(
    "asl.muse.reac.r.ant.orb.g.cbf.hct.pvc",
    "asl.muse.reac.r.lat.orb.g.cbf.hct.pvc",
    "asl.muse.reac.r.med.orb.g.cbf.hct.pvc",
    "asl.muse.reac.r.pos.orb.g.cbf.hct.pvc",
    "asl.muse.reac.r.ant.insula.cbf.hct.pvc",
    "asl.muse.reac.r.pos.insula.cbf.hct.pvc",
    "asl.muse.reac.r.fron.pole.cbf.hct.pvc",
    "asl.muse.reac.r.mid.fron.g.cbf.hct.pvc",
    "asl.muse.reac.r.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.reac.r.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.reac.r.precent.g.cbf.hct.pvc",
    "asl.muse.reac.r.sup.fron.g.cbf.hct.pvc",
    "asl.muse.reac.r.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.reac.r.rectus.g.cbf.hct.pvc",
    "asl.muse.reac.r.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.reac.r.med.precent.g.cbf.hct.pvc",
    "asl.muse.reac.r.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.reac.r.sca.cbf.hct.pvc",
    "asl.muse.reac.r.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.reac.r.cent.op.cbf.hct.pvc",
    "asl.muse.reac.r.fron.op.cbf.hct.pvc",
    "asl.muse.reac.r.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.fron.gm.var,  asl.muse.reac.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.fron.gm.var.pvc <- c(asl.muse.reac.l.fron.gm.var.pvc,
                                     asl.muse.reac.r.fron.gm.var.pvc)
  
  # create `asl.muse.reac.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ####################
  ### Limbic (PVC) ###
  ####################
  
  asl.muse.reac.l.limb.gm.var.pvc <- c(
    "asl.muse.reac.l.ant.cing.g.cbf.hct.pvc",
    "asl.muse.reac.l.mid.cing.g.cbf.hct.pvc",
    "asl.muse.reac.l.pos.cing.g.cbf.hct.pvc",
    "asl.muse.reac.l.ent.cbf.hct.pvc",
    "asl.muse.reac.l.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.limb.gm.var.pvc,  asl.muse.reac.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.limb.gm.var.pvc <- c(
    "asl.muse.reac.r.ant.cing.g.cbf.hct.pvc",
    "asl.muse.reac.r.mid.cing.g.cbf.hct.pvc",
    "asl.muse.reac.r.pos.cing.g.cbf.hct.pvc",
    "asl.muse.reac.r.ent.cbf.hct.pvc",
    "asl.muse.reac.r.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.limb.gm.var.pvc,  asl.muse.reac.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.limb.gm.var.pvc <- c(asl.muse.reac.l.limb.gm.var.pvc,
                                     asl.muse.reac.r.limb.gm.var.pvc)
  
  # create `asl.muse.reac.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #######################
  ### Occipital (PVC) ###
  #######################
  
  asl.muse.reac.l.occ.gm.var.pvc <- c(
    "asl.muse.reac.l.occ.fus.g.cbf.hct.pvc",
    "asl.muse.reac.l.inf.occ.g.cbf.hct.pvc",
    "asl.muse.reac.l.mid.occ.g.cbf.hct.pvc",
    "asl.muse.reac.l.occ.pole.cbf.hct.pvc",
    "asl.muse.reac.l.sup.occ.g.cbf.hct.pvc",
    "asl.muse.reac.l.calc.ctx.cbf.hct.pvc",
    "asl.muse.reac.l.cuneus.cbf.hct.pvc",
    "asl.muse.reac.l.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.occ.gm.var.pvc,  asl.muse.reac.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.occ.gm.var.pvc <- c(
    "asl.muse.reac.r.occ.fus.g.cbf.hct.pvc",
    "asl.muse.reac.r.inf.occ.g.cbf.hct.pvc",
    "asl.muse.reac.r.mid.occ.g.cbf.hct.pvc",
    "asl.muse.reac.r.occ.pole.cbf.hct.pvc",
    "asl.muse.reac.r.sup.occ.g.cbf.hct.pvc",
    "asl.muse.reac.r.calc.ctx.cbf.hct.pvc",
    "asl.muse.reac.r.cuneus.cbf.hct.pvc",
    "asl.muse.reac.r.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.occ.gm.var.pvc,  asl.muse.reac.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.occ.gm.var.pvc <- c(asl.muse.reac.l.occ.gm.var.pvc,
                                    asl.muse.reac.r.occ.gm.var.pvc)
  
  # create `asl.muse.reac.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Parietal (PVC) ###
  ######################
  
  asl.muse.reac.l.par.gm.var.pvc <- c(
    "asl.muse.reac.l.ang.g.cbf.hct.pvc",
    "asl.muse.reac.l.postcent.g.cbf.hct.pvc",
    "asl.muse.reac.l.supra.g.cbf.hct.pvc",
    "asl.muse.reac.l.sup.par.cbf.hct.pvc",
    "asl.muse.reac.l.med.postcent.g.cbf.hct.pvc",
    "asl.muse.reac.l.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.par.gm.var.pvc,  asl.muse.reac.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.par.gm.var.pvc <- c(
    "asl.muse.reac.r.ang.g.cbf.hct.pvc",
    "asl.muse.reac.r.postcent.g.cbf.hct.pvc",
    "asl.muse.reac.r.supra.g.cbf.hct.pvc",
    "asl.muse.reac.r.sup.par.cbf.hct.pvc",
    "asl.muse.reac.r.med.postcent.g.cbf.hct.pvc",
    "asl.muse.reac.r.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.par.gm.var.pvc,  asl.muse.reac.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.par.gm.var.pvc <- c(asl.muse.reac.l.par.gm.var.pvc,
                                    asl.muse.reac.r.par.gm.var.pvc)
  
  # create `asl.muse.reac.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Temporal (PVC) ###
  ######################
  
  asl.muse.reac.l.temp.gm.var.pvc <- c(
    "asl.muse.reac.l.fus.g.cbf.hct.pvc",
    "asl.muse.reac.l.inf.temp.g.cbf.hct.pvc",
    "asl.muse.reac.l.mid.temp.g.cbf.hct.pvc",
    "asl.muse.reac.l.sup.temp.g.cbf.hct.pvc",
    "asl.muse.reac.l.temp.pole.cbf.hct.pvc",
    "asl.muse.reac.l.pp.cbf.hct.pvc",
    "asl.muse.reac.l.pt.cbf.hct.pvc",
    "asl.muse.reac.l.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.temp.gm.var.pvc,  asl.muse.reac.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.temp.gm.var.pvc <- c(
    "asl.muse.reac.r.fus.g.cbf.hct.pvc",
    "asl.muse.reac.r.inf.temp.g.cbf.hct.pvc",
    "asl.muse.reac.r.mid.temp.g.cbf.hct.pvc",
    "asl.muse.reac.r.sup.temp.g.cbf.hct.pvc",
    "asl.muse.reac.r.temp.pole.cbf.hct.pvc",
    "asl.muse.reac.r.pp.cbf.hct.pvc",
    "asl.muse.reac.r.pt.cbf.hct.pvc",
    "asl.muse.reac.r.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.temp.gm.var.pvc,  asl.muse.reac.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.temp.gm.var.pvc <- c(asl.muse.reac.l.temp.gm.var.pvc,
                                     asl.muse.reac.r.temp.gm.var.pvc)
  
  # create `asl.muse.reac.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Deep (PVC) ###
  ##################
  
  asl.muse.reac.l.deep.gm.var.pvc <- c(
    "asl.muse.reac.l.hipp.cbf.hct.pvc",
    "asl.muse.reac.l.amyg.cbf.hct.pvc",
    "asl.muse.reac.l.accum.cbf.hct.pvc",
    "asl.muse.reac.l.caudate.cbf.hct.pvc",
    "asl.muse.reac.l.pallidum.cbf.hct.pvc",
    "asl.muse.reac.l.putamen.cbf.hct.pvc",
    "asl.muse.reac.l.thal.cbf.hct.pvc",
    "asl.muse.reac.l.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.l.deep.gm.var.pvc,  asl.muse.reac.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.l.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.l.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.r.deep.gm.var.pvc <- c(
    "asl.muse.reac.r.hipp.cbf.hct.pvc",
    "asl.muse.reac.r.amyg.cbf.hct.pvc",
    "asl.muse.reac.r.accum.cbf.hct.pvc",
    "asl.muse.reac.r.caudate.cbf.hct.pvc",
    "asl.muse.reac.r.pallidum.cbf.hct.pvc",
    "asl.muse.reac.r.putamen.cbf.hct.pvc",
    "asl.muse.reac.r.thal.cbf.hct.pvc",
    "asl.muse.reac.r.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.r.deep.gm.var.pvc,  asl.muse.reac.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.r.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.r.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.reac.deep.gm.var.pvc <- c(asl.muse.reac.l.deep.gm.var.pvc,
                                     asl.muse.reac.r.deep.gm.var.pvc)
  
  # create `asl.muse.reac.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Cerebrum (PVC) ###
  ######################
  
  asl.muse.reac.cerebrum.gm.var.pvc <- c(
    asl.muse.reac.fron.gm.var.pvc,
    asl.muse.reac.limb.gm.var.pvc,
    asl.muse.reac.occ.gm.var.pvc,
    asl.muse.reac.par.gm.var.pvc,
    asl.muse.reac.temp.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.cerebrum.gm.var.pvc,  asl.muse.reac.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.cerebrum.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.cerebrum.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cerebrum.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ########################
  ### Cerebellum (PVC) ###
  ########################
  
  asl.muse.reac.cb.gm.var.pvc <- c(
    "asl.muse.reac.l.cb.ext.cbf.hct.pvc",
    "asl.muse.reac.l.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.reac.l.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.reac.l.cb.verm.8.10.cbf.hct.pvc",
    "asl.muse.reac.r.cb.ext.cbf.hct.pvc",
    "asl.muse.reac.r.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.reac.r.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.reac.r.cb.verm.8.10.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.cb.gm.var.pvc,  asl.muse.reac.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.cb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.cb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.cb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.cb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Total GM (PVC) ###
  ######################
  
  asl.muse.reac.gm.var.pvc <- c(
    asl.muse.reac.l.deep.gm.var.pvc,
    asl.muse.reac.l.fron.gm.var.pvc,
    asl.muse.reac.l.limb.gm.var.pvc,
    asl.muse.reac.l.occ.gm.var.pvc,
    asl.muse.reac.l.par.gm.var.pvc,
    asl.muse.reac.l.temp.gm.var.pvc,
    asl.muse.reac.r.deep.gm.var.pvc,
    asl.muse.reac.r.fron.gm.var.pvc,
    asl.muse.reac.r.limb.gm.var.pvc,
    asl.muse.reac.r.occ.gm.var.pvc,
    asl.muse.reac.r.par.gm.var.pvc,
    asl.muse.reac.r.temp.gm.var.pvc,
    asl.muse.reac.cb.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.reac.gm.var.pvc,  asl.muse.reac.gm.regvol) %in% names(data)))
  
  # create `asl.muse.reac.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.reac.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.reac.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.reac.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.reac.var.pvc <- paste0("asl.muse.reac.", gsub("_", ".", meta.roi.name), ".cbf.hct.pvc")
  # sum(!asl.muse.reac.var.pvc %in% names(data))
  
  return(data)
}



