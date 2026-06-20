#' Derive, label, and add ASL MUSE challenging CBF variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added ASL MUSE challenging CBF variables.
#' @export

derive_asl_chall <- function(data) {
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
  
  asl.muse.chall.l.fron.gm.var <- c(
    "asl.muse.chall.l.ant.orb.g.cbf.hct",
    "asl.muse.chall.l.lat.orb.g.cbf.hct",
    "asl.muse.chall.l.med.orb.g.cbf.hct",
    "asl.muse.chall.l.pos.orb.g.cbf.hct",
    "asl.muse.chall.l.ant.insula.cbf.hct",
    "asl.muse.chall.l.pos.insula.cbf.hct",
    "asl.muse.chall.l.fron.pole.cbf.hct",
    "asl.muse.chall.l.mid.fron.g.cbf.hct",
    "asl.muse.chall.l.inf.fron.op.g.cbf.hct",
    "asl.muse.chall.l.inf.fron.orb.g.cbf.hct",
    "asl.muse.chall.l.precent.g.cbf.hct",
    "asl.muse.chall.l.sup.fron.g.cbf.hct",
    "asl.muse.chall.l.inf.fron.tri.g.cbf.hct",
    "asl.muse.chall.l.rectus.g.cbf.hct",
    "asl.muse.chall.l.med.fron.ctx.cbf.hct",
    "asl.muse.chall.l.med.precent.g.cbf.hct",
    "asl.muse.chall.l.sup.med.fron.g.cbf.hct",
    "asl.muse.chall.l.sca.cbf.hct",
    "asl.muse.chall.l.supp.motor.ctx.cbf.hct",
    "asl.muse.chall.l.cent.op.cbf.hct",
    "asl.muse.chall.l.fron.op.cbf.hct",
    "asl.muse.chall.l.par.op.cbf.hct"
  )
  
  asl.muse.chall.l.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.fron.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.fron.gm.var,  asl.muse.chall.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.fron.gm.var <- c(
    "asl.muse.chall.r.ant.orb.g.cbf.hct",
    "asl.muse.chall.r.lat.orb.g.cbf.hct",
    "asl.muse.chall.r.med.orb.g.cbf.hct",
    "asl.muse.chall.r.pos.orb.g.cbf.hct",
    "asl.muse.chall.r.ant.insula.cbf.hct",
    "asl.muse.chall.r.pos.insula.cbf.hct",
    "asl.muse.chall.r.fron.pole.cbf.hct",
    "asl.muse.chall.r.mid.fron.g.cbf.hct",
    "asl.muse.chall.r.inf.fron.op.g.cbf.hct",
    "asl.muse.chall.r.inf.fron.orb.g.cbf.hct",
    "asl.muse.chall.r.precent.g.cbf.hct",
    "asl.muse.chall.r.sup.fron.g.cbf.hct",
    "asl.muse.chall.r.inf.fron.tri.g.cbf.hct",
    "asl.muse.chall.r.rectus.g.cbf.hct",
    "asl.muse.chall.r.med.fron.ctx.cbf.hct",
    "asl.muse.chall.r.med.precent.g.cbf.hct",
    "asl.muse.chall.r.sup.med.fron.g.cbf.hct",
    "asl.muse.chall.r.sca.cbf.hct",
    "asl.muse.chall.r.supp.motor.ctx.cbf.hct",
    "asl.muse.chall.r.cent.op.cbf.hct",
    "asl.muse.chall.r.fron.op.cbf.hct",
    "asl.muse.chall.r.par.op.cbf.hct"
  )
  
  asl.muse.chall.r.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.fron.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.fron.gm.var,  asl.muse.chall.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.fron.gm.var <- c(asl.muse.chall.l.fron.gm.var, asl.muse.chall.r.fron.gm.var)
  asl.muse.chall.fron.gm.regvol <- c(asl.muse.chall.l.fron.gm.regvol,
                                    asl.muse.chall.r.fron.gm.regvol)
  
  # create `asl.muse.chall.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##############
  ### Limbic ###
  ##############
  
  asl.muse.chall.l.limb.gm.var <- c(
    "asl.muse.chall.l.ant.cing.g.cbf.hct",
    "asl.muse.chall.l.mid.cing.g.cbf.hct",
    "asl.muse.chall.l.pos.cing.g.cbf.hct",
    "asl.muse.chall.l.ent.cbf.hct",
    "asl.muse.chall.l.parahipp.g.cbf.hct"
  )
  
  asl.muse.chall.l.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.limb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.limb.gm.var,  asl.muse.chall.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.limb.gm.var <- c(
    "asl.muse.chall.r.ant.cing.g.cbf.hct",
    "asl.muse.chall.r.mid.cing.g.cbf.hct",
    "asl.muse.chall.r.pos.cing.g.cbf.hct",
    "asl.muse.chall.r.ent.cbf.hct",
    "asl.muse.chall.r.parahipp.g.cbf.hct"
  )
  
  asl.muse.chall.r.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.limb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.limb.gm.var,  asl.muse.chall.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.limb.gm.var <- c(asl.muse.chall.l.limb.gm.var, asl.muse.chall.r.limb.gm.var)
  asl.muse.chall.limb.gm.regvol <- c(asl.muse.chall.l.limb.gm.regvol,
                                    asl.muse.chall.r.limb.gm.regvol)
  
  # create `asl.muse.chall.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #################
  ### Occipital ###
  #################
  
  asl.muse.chall.l.occ.gm.var <- c(
    "asl.muse.chall.l.occ.fus.g.cbf.hct",
    "asl.muse.chall.l.inf.occ.g.cbf.hct",
    "asl.muse.chall.l.mid.occ.g.cbf.hct",
    "asl.muse.chall.l.occ.pole.cbf.hct",
    "asl.muse.chall.l.sup.occ.g.cbf.hct",
    "asl.muse.chall.l.calc.ctx.cbf.hct",
    "asl.muse.chall.l.cuneus.cbf.hct",
    "asl.muse.chall.l.ling.g.cbf.hct"
  )
  
  asl.muse.chall.l.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.occ.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.occ.gm.var,  asl.muse.chall.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.occ.gm.var <- c(
    "asl.muse.chall.r.occ.fus.g.cbf.hct",
    "asl.muse.chall.r.inf.occ.g.cbf.hct",
    "asl.muse.chall.r.mid.occ.g.cbf.hct",
    "asl.muse.chall.r.occ.pole.cbf.hct",
    "asl.muse.chall.r.sup.occ.g.cbf.hct",
    "asl.muse.chall.r.calc.ctx.cbf.hct",
    "asl.muse.chall.r.cuneus.cbf.hct",
    "asl.muse.chall.r.ling.g.cbf.hct"
  )
  
  asl.muse.chall.r.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.occ.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.occ.gm.var,  asl.muse.chall.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.occ.gm.var <- c(asl.muse.chall.l.occ.gm.var, asl.muse.chall.r.occ.gm.var)
  asl.muse.chall.occ.gm.regvol <- c(asl.muse.chall.l.occ.gm.regvol,
                                   asl.muse.chall.r.occ.gm.regvol)
  
  # create `asl.muse.chall.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.occ.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.occ.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.occ.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Parietal ###
  ################
  
  asl.muse.chall.l.par.gm.var <- c(
    "asl.muse.chall.l.ang.g.cbf.hct",
    "asl.muse.chall.l.postcent.g.cbf.hct",
    "asl.muse.chall.l.supra.g.cbf.hct",
    "asl.muse.chall.l.sup.par.cbf.hct",
    "asl.muse.chall.l.med.postcent.g.cbf.hct",
    "asl.muse.chall.l.precuneus.cbf.hct"
  )
  
  asl.muse.chall.l.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.par.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.par.gm.var,  asl.muse.chall.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.par.gm.var <- c(
    "asl.muse.chall.r.ang.g.cbf.hct",
    "asl.muse.chall.r.postcent.g.cbf.hct",
    "asl.muse.chall.r.supra.g.cbf.hct",
    "asl.muse.chall.r.sup.par.cbf.hct",
    "asl.muse.chall.r.med.postcent.g.cbf.hct",
    "asl.muse.chall.r.precuneus.cbf.hct"
  )
  
  asl.muse.chall.r.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.par.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.par.gm.var,  asl.muse.chall.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.par.gm.var <- c(asl.muse.chall.l.par.gm.var, asl.muse.chall.r.par.gm.var)
  asl.muse.chall.par.gm.regvol <- c(asl.muse.chall.l.par.gm.regvol,
                                   asl.muse.chall.r.par.gm.regvol)
  
  # create `asl.muse.chall.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.par.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.par.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.par.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.par.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Temporal ###
  ################
  
  asl.muse.chall.l.temp.gm.var <- c(
    "asl.muse.chall.l.fus.g.cbf.hct",
    "asl.muse.chall.l.inf.temp.g.cbf.hct",
    "asl.muse.chall.l.mid.temp.g.cbf.hct",
    "asl.muse.chall.l.sup.temp.g.cbf.hct",
    "asl.muse.chall.l.temp.pole.cbf.hct",
    "asl.muse.chall.l.pp.cbf.hct",
    "asl.muse.chall.l.pt.cbf.hct",
    "asl.muse.chall.l.trans.temp.g.cbf.hct"
  )
  
  asl.muse.chall.l.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.temp.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.temp.gm.var,  asl.muse.chall.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.temp.gm.var <- c(
    "asl.muse.chall.r.fus.g.cbf.hct",
    "asl.muse.chall.r.inf.temp.g.cbf.hct",
    "asl.muse.chall.r.mid.temp.g.cbf.hct",
    "asl.muse.chall.r.sup.temp.g.cbf.hct",
    "asl.muse.chall.r.temp.pole.cbf.hct",
    "asl.muse.chall.r.pp.cbf.hct",
    "asl.muse.chall.r.pt.cbf.hct",
    "asl.muse.chall.r.trans.temp.g.cbf.hct"
  )
  
  asl.muse.chall.r.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.temp.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.temp.gm.var,  asl.muse.chall.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.temp.gm.var <- c(asl.muse.chall.l.temp.gm.var, asl.muse.chall.r.temp.gm.var)
  asl.muse.chall.temp.gm.regvol <- c(asl.muse.chall.l.temp.gm.regvol,
                                    asl.muse.chall.r.temp.gm.regvol)
  
  # create `asl.muse.chall.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ############
  ### Deep ###
  ############
  
  asl.muse.chall.l.deep.gm.var <- c(
    "asl.muse.chall.l.hipp.cbf.hct",
    "asl.muse.chall.l.amyg.cbf.hct",
    "asl.muse.chall.l.accum.cbf.hct",
    "asl.muse.chall.l.caudate.cbf.hct",
    "asl.muse.chall.l.pallidum.cbf.hct",
    "asl.muse.chall.l.putamen.cbf.hct",
    "asl.muse.chall.l.thal.cbf.hct",
    "asl.muse.chall.l.bf.cbf.hct"
  )
  
  asl.muse.chall.l.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.l.deep.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.deep.gm.var,  asl.muse.chall.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.deep.gm.var <- c(
    "asl.muse.chall.r.hipp.cbf.hct",
    "asl.muse.chall.r.amyg.cbf.hct",
    "asl.muse.chall.r.accum.cbf.hct",
    "asl.muse.chall.r.caudate.cbf.hct",
    "asl.muse.chall.r.pallidum.cbf.hct",
    "asl.muse.chall.r.putamen.cbf.hct",
    "asl.muse.chall.r.thal.cbf.hct",
    "asl.muse.chall.r.bf.cbf.hct"
  )
  
  asl.muse.chall.r.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.r.deep.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.deep.gm.var,  asl.muse.chall.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.deep.gm.var <- c(asl.muse.chall.l.deep.gm.var, asl.muse.chall.r.deep.gm.var)
  asl.muse.chall.deep.gm.regvol <- c(asl.muse.chall.l.deep.gm.regvol,
                                    asl.muse.chall.r.deep.gm.regvol)
  
  # create `asl.muse.chall.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Cerebrum ###
  ################
  
  asl.muse.chall.cerebrum.gm.var <- c(
    asl.muse.chall.fron.gm.var,
    asl.muse.chall.limb.gm.var,
    asl.muse.chall.occ.gm.var,
    asl.muse.chall.par.gm.var,
    asl.muse.chall.temp.gm.var
  )
  
  asl.muse.chall.cerebrum.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.cerebrum.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.cerebrum.gm.var,  asl.muse.chall.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.cerebrum.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.cerebrum.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.cerebrum.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Cerebellum ###
  ##################
  
  asl.muse.chall.cb.gm.var <- c(
    "asl.muse.chall.l.cb.ext.cbf.hct",
    "asl.muse.chall.l.cb.verm.1.5.cbf.hct",
    "asl.muse.chall.l.cb.verm.6.7.cbf.hct",
    "asl.muse.chall.l.cb.verm.8.10.cbf.hct",
    "asl.muse.chall.r.cb.ext.cbf.hct",
    "asl.muse.chall.r.cb.verm.1.5.cbf.hct",
    "asl.muse.chall.r.cb.verm.6.7.cbf.hct",
    "asl.muse.chall.r.cb.verm.8.10.cbf.hct"
  )
  
  asl.muse.chall.cb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.cb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.cb.gm.var,  asl.muse.chall.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.cb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.cb.gm.cbf.hct =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.cb.gm.var
                    )) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.cb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.cb.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Total GM ###
  ################
  
  asl.muse.chall.gm.var <- c(
    asl.muse.chall.l.deep.gm.var,
    asl.muse.chall.l.fron.gm.var,
    asl.muse.chall.l.limb.gm.var,
    asl.muse.chall.l.occ.gm.var,
    asl.muse.chall.l.par.gm.var,
    asl.muse.chall.l.temp.gm.var,
    asl.muse.chall.r.deep.gm.var,
    asl.muse.chall.r.fron.gm.var,
    asl.muse.chall.r.limb.gm.var,
    asl.muse.chall.r.occ.gm.var,
    asl.muse.chall.r.par.gm.var,
    asl.muse.chall.r.temp.gm.var,
    asl.muse.chall.cb.gm.var
  )
  
  asl.muse.chall.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.chall.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.chall.gm.var,  asl.muse.chall.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.gm.cbf.hct =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.gm.var
                    )) *
                      dplyr::pick(dplyr::all_of(
                        asl.muse.chall.gm.regvol
                      ))) /
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.gm.regvol
                    )))) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.chall.var <- paste0("asl.muse.chall.", gsub("_", ".", meta.roi.name), ".cbf.hct")
  # sum(!asl.muse.chall.var %in% names(data))
  
  ##----------------------------------------------------------------------------
  ##----------------------------------------------------------------------------
  
  ####################
  ### Frontal (PVC)###
  ####################
  
  asl.muse.chall.l.fron.gm.var.pvc <- c(
    "asl.muse.chall.l.ant.orb.g.cbf.hct.pvc",
    "asl.muse.chall.l.lat.orb.g.cbf.hct.pvc",
    "asl.muse.chall.l.med.orb.g.cbf.hct.pvc",
    "asl.muse.chall.l.pos.orb.g.cbf.hct.pvc",
    "asl.muse.chall.l.ant.insula.cbf.hct.pvc",
    "asl.muse.chall.l.pos.insula.cbf.hct.pvc",
    "asl.muse.chall.l.fron.pole.cbf.hct.pvc",
    "asl.muse.chall.l.mid.fron.g.cbf.hct.pvc",
    "asl.muse.chall.l.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.chall.l.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.chall.l.precent.g.cbf.hct.pvc",
    "asl.muse.chall.l.sup.fron.g.cbf.hct.pvc",
    "asl.muse.chall.l.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.chall.l.rectus.g.cbf.hct.pvc",
    "asl.muse.chall.l.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.chall.l.med.precent.g.cbf.hct.pvc",
    "asl.muse.chall.l.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.chall.l.sca.cbf.hct.pvc",
    "asl.muse.chall.l.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.chall.l.cent.op.cbf.hct.pvc",
    "asl.muse.chall.l.fron.op.cbf.hct.pvc",
    "asl.muse.chall.l.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.fron.gm.var.pvc,  asl.muse.chall.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.fron.gm.var.pvc <- c(
    "asl.muse.chall.r.ant.orb.g.cbf.hct.pvc",
    "asl.muse.chall.r.lat.orb.g.cbf.hct.pvc",
    "asl.muse.chall.r.med.orb.g.cbf.hct.pvc",
    "asl.muse.chall.r.pos.orb.g.cbf.hct.pvc",
    "asl.muse.chall.r.ant.insula.cbf.hct.pvc",
    "asl.muse.chall.r.pos.insula.cbf.hct.pvc",
    "asl.muse.chall.r.fron.pole.cbf.hct.pvc",
    "asl.muse.chall.r.mid.fron.g.cbf.hct.pvc",
    "asl.muse.chall.r.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.chall.r.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.chall.r.precent.g.cbf.hct.pvc",
    "asl.muse.chall.r.sup.fron.g.cbf.hct.pvc",
    "asl.muse.chall.r.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.chall.r.rectus.g.cbf.hct.pvc",
    "asl.muse.chall.r.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.chall.r.med.precent.g.cbf.hct.pvc",
    "asl.muse.chall.r.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.chall.r.sca.cbf.hct.pvc",
    "asl.muse.chall.r.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.chall.r.cent.op.cbf.hct.pvc",
    "asl.muse.chall.r.fron.op.cbf.hct.pvc",
    "asl.muse.chall.r.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.fron.gm.var,  asl.muse.chall.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.fron.gm.var.pvc <- c(asl.muse.chall.l.fron.gm.var.pvc,
                                     asl.muse.chall.r.fron.gm.var.pvc)
  
  # create `asl.muse.chall.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ####################
  ### Limbic (PVC) ###
  ####################
  
  asl.muse.chall.l.limb.gm.var.pvc <- c(
    "asl.muse.chall.l.ant.cing.g.cbf.hct.pvc",
    "asl.muse.chall.l.mid.cing.g.cbf.hct.pvc",
    "asl.muse.chall.l.pos.cing.g.cbf.hct.pvc",
    "asl.muse.chall.l.ent.cbf.hct.pvc",
    "asl.muse.chall.l.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.limb.gm.var.pvc,  asl.muse.chall.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.limb.gm.var.pvc <- c(
    "asl.muse.chall.r.ant.cing.g.cbf.hct.pvc",
    "asl.muse.chall.r.mid.cing.g.cbf.hct.pvc",
    "asl.muse.chall.r.pos.cing.g.cbf.hct.pvc",
    "asl.muse.chall.r.ent.cbf.hct.pvc",
    "asl.muse.chall.r.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.limb.gm.var.pvc,  asl.muse.chall.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.limb.gm.var.pvc <- c(asl.muse.chall.l.limb.gm.var.pvc, asl.muse.chall.r.limb.gm.var.pvc)
  
  # create `asl.muse.chall.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #######################
  ### Occipital (PVC) ###
  #######################
  
  asl.muse.chall.l.occ.gm.var.pvc <- c(
    "asl.muse.chall.l.occ.fus.g.cbf.hct.pvc",
    "asl.muse.chall.l.inf.occ.g.cbf.hct.pvc",
    "asl.muse.chall.l.mid.occ.g.cbf.hct.pvc",
    "asl.muse.chall.l.occ.pole.cbf.hct.pvc",
    "asl.muse.chall.l.sup.occ.g.cbf.hct.pvc",
    "asl.muse.chall.l.calc.ctx.cbf.hct.pvc",
    "asl.muse.chall.l.cuneus.cbf.hct.pvc",
    "asl.muse.chall.l.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.occ.gm.var.pvc,  asl.muse.chall.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.occ.gm.var.pvc <- c(
    "asl.muse.chall.r.occ.fus.g.cbf.hct.pvc",
    "asl.muse.chall.r.inf.occ.g.cbf.hct.pvc",
    "asl.muse.chall.r.mid.occ.g.cbf.hct.pvc",
    "asl.muse.chall.r.occ.pole.cbf.hct.pvc",
    "asl.muse.chall.r.sup.occ.g.cbf.hct.pvc",
    "asl.muse.chall.r.calc.ctx.cbf.hct.pvc",
    "asl.muse.chall.r.cuneus.cbf.hct.pvc",
    "asl.muse.chall.r.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.occ.gm.var.pvc,  asl.muse.chall.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.occ.gm.var.pvc <- c(asl.muse.chall.l.occ.gm.var.pvc, asl.muse.chall.r.occ.gm.var.pvc)
  
  # create `asl.muse.chall.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.occ.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.occ.gm.var.pvc)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.occ.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Parietal (PVC) ###
  ######################
  
  asl.muse.chall.l.par.gm.var.pvc <- c(
    "asl.muse.chall.l.ang.g.cbf.hct.pvc",
    "asl.muse.chall.l.postcent.g.cbf.hct.pvc",
    "asl.muse.chall.l.supra.g.cbf.hct.pvc",
    "asl.muse.chall.l.sup.par.cbf.hct.pvc",
    "asl.muse.chall.l.med.postcent.g.cbf.hct.pvc",
    "asl.muse.chall.l.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.par.gm.var.pvc,  asl.muse.chall.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.par.gm.var.pvc <- c(
    "asl.muse.chall.r.ang.g.cbf.hct.pvc",
    "asl.muse.chall.r.postcent.g.cbf.hct.pvc",
    "asl.muse.chall.r.supra.g.cbf.hct.pvc",
    "asl.muse.chall.r.sup.par.cbf.hct.pvc",
    "asl.muse.chall.r.med.postcent.g.cbf.hct.pvc",
    "asl.muse.chall.r.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.par.gm.var.pvc,  asl.muse.chall.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.par.gm.var.pvc <- c(asl.muse.chall.l.par.gm.var.pvc, asl.muse.chall.r.par.gm.var.pvc)
  
  # create `asl.muse.chall.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.par.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.par.gm.var.pvc)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.par.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.par.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Temporal (PVC) ###
  ######################
  
  asl.muse.chall.l.temp.gm.var.pvc <- c(
    "asl.muse.chall.l.fus.g.cbf.hct.pvc",
    "asl.muse.chall.l.inf.temp.g.cbf.hct.pvc",
    "asl.muse.chall.l.mid.temp.g.cbf.hct.pvc",
    "asl.muse.chall.l.sup.temp.g.cbf.hct.pvc",
    "asl.muse.chall.l.temp.pole.cbf.hct.pvc",
    "asl.muse.chall.l.pp.cbf.hct.pvc",
    "asl.muse.chall.l.pt.cbf.hct.pvc",
    "asl.muse.chall.l.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.temp.gm.var.pvc,  asl.muse.chall.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.temp.gm.var.pvc <- c(
    "asl.muse.chall.r.fus.g.cbf.hct.pvc",
    "asl.muse.chall.r.inf.temp.g.cbf.hct.pvc",
    "asl.muse.chall.r.mid.temp.g.cbf.hct.pvc",
    "asl.muse.chall.r.sup.temp.g.cbf.hct.pvc",
    "asl.muse.chall.r.temp.pole.cbf.hct.pvc",
    "asl.muse.chall.r.pp.cbf.hct.pvc",
    "asl.muse.chall.r.pt.cbf.hct.pvc",
    "asl.muse.chall.r.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.temp.gm.var.pvc,  asl.muse.chall.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.temp.gm.var.pvc <- c(asl.muse.chall.l.temp.gm.var.pvc, asl.muse.chall.r.temp.gm.var.pvc)
  
  # create `asl.muse.chall.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Deep (PVC) ###
  ##################
  
  asl.muse.chall.l.deep.gm.var.pvc <- c(
    "asl.muse.chall.l.hipp.cbf.hct.pvc",
    "asl.muse.chall.l.amyg.cbf.hct.pvc",
    "asl.muse.chall.l.accum.cbf.hct.pvc",
    "asl.muse.chall.l.caudate.cbf.hct.pvc",
    "asl.muse.chall.l.pallidum.cbf.hct.pvc",
    "asl.muse.chall.l.putamen.cbf.hct.pvc",
    "asl.muse.chall.l.thal.cbf.hct.pvc",
    "asl.muse.chall.l.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.l.deep.gm.var.pvc,  asl.muse.chall.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.l.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.l.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.r.deep.gm.var.pvc <- c(
    "asl.muse.chall.r.hipp.cbf.hct.pvc",
    "asl.muse.chall.r.amyg.cbf.hct.pvc",
    "asl.muse.chall.r.accum.cbf.hct.pvc",
    "asl.muse.chall.r.caudate.cbf.hct.pvc",
    "asl.muse.chall.r.pallidum.cbf.hct.pvc",
    "asl.muse.chall.r.putamen.cbf.hct.pvc",
    "asl.muse.chall.r.thal.cbf.hct.pvc",
    "asl.muse.chall.r.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.r.deep.gm.var.pvc,  asl.muse.chall.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.r.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.r.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.chall.deep.gm.var.pvc <- c(asl.muse.chall.l.deep.gm.var.pvc, asl.muse.chall.r.deep.gm.var.pvc)
  
  # create `asl.muse.chall.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Cerebrum (PVC) ###
  ######################
  
  asl.muse.chall.cerebrum.gm.var.pvc <- c(
    asl.muse.chall.fron.gm.var.pvc,
    asl.muse.chall.limb.gm.var.pvc,
    asl.muse.chall.occ.gm.var.pvc,
    asl.muse.chall.par.gm.var.pvc,
    asl.muse.chall.temp.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.cerebrum.gm.var.pvc,  asl.muse.chall.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.cerebrum.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.chall.cerebrum.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.cerebrum.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.chall.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.chall.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ########################
  ### Cerebellum (PVC) ###
  ########################
  
  asl.muse.chall.cb.gm.var.pvc <- c(
    "asl.muse.chall.l.cb.ext.cbf.hct.pvc",
    "asl.muse.chall.l.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.chall.l.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.chall.l.cb.verm.8.10.cbf.hct.pvc",
    "asl.muse.chall.r.cb.ext.cbf.hct.pvc",
    "asl.muse.chall.r.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.chall.r.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.chall.r.cb.verm.8.10.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.cb.gm.var.pvc,  asl.muse.chall.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.cb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.cb.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.cb.gm.var.pvc
                    )) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.chall.cb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.chall.cb.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Total GM (PVC) ###
  ######################
  
  asl.muse.chall.gm.var.pvc <- c(
    asl.muse.chall.l.deep.gm.var.pvc,
    asl.muse.chall.l.fron.gm.var.pvc,
    asl.muse.chall.l.limb.gm.var.pvc,
    asl.muse.chall.l.occ.gm.var.pvc,
    asl.muse.chall.l.par.gm.var.pvc,
    asl.muse.chall.l.temp.gm.var.pvc,
    asl.muse.chall.r.deep.gm.var.pvc,
    asl.muse.chall.r.fron.gm.var.pvc,
    asl.muse.chall.r.limb.gm.var.pvc,
    asl.muse.chall.r.occ.gm.var.pvc,
    asl.muse.chall.r.par.gm.var.pvc,
    asl.muse.chall.r.temp.gm.var.pvc,
    asl.muse.chall.cb.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.chall.gm.var.pvc,  asl.muse.chall.gm.regvol) %in% names(data)))
  
  # create `asl.muse.chall.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.chall.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.gm.var.pvc
                    )) *
                      dplyr::pick(dplyr::all_of(
                        asl.muse.chall.gm.regvol
                      ))) /
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.chall.gm.regvol
                    )))) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.chall.var.pvc <- paste0("asl.muse.chall.", gsub("_", ".", meta.roi.name), ".cbf.hct.pvc")
  # sum(!asl.muse.chall.var.pvc %in% names(data))
  
  return(data)
}

