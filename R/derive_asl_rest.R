#' Derive, label, and add ASL MUSE resting CBF variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added ASL MUSE resting CBF variables.
#' @export

derive_asl_rest <- function(data) {
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
  
  asl.muse.rest.l.fron.gm.var <- c(
    "asl.muse.rest.l.ant.orb.g.cbf.hct",
    "asl.muse.rest.l.lat.orb.g.cbf.hct",
    "asl.muse.rest.l.med.orb.g.cbf.hct",
    "asl.muse.rest.l.pos.orb.g.cbf.hct",
    "asl.muse.rest.l.ant.insula.cbf.hct",
    "asl.muse.rest.l.pos.insula.cbf.hct",
    "asl.muse.rest.l.fron.pole.cbf.hct",
    "asl.muse.rest.l.mid.fron.g.cbf.hct",
    "asl.muse.rest.l.inf.fron.op.g.cbf.hct",
    "asl.muse.rest.l.inf.fron.orb.g.cbf.hct",
    "asl.muse.rest.l.precent.g.cbf.hct",
    "asl.muse.rest.l.sup.fron.g.cbf.hct",
    "asl.muse.rest.l.inf.fron.tri.g.cbf.hct",
    "asl.muse.rest.l.rectus.g.cbf.hct",
    "asl.muse.rest.l.med.fron.ctx.cbf.hct",
    "asl.muse.rest.l.med.precent.g.cbf.hct",
    "asl.muse.rest.l.sup.med.fron.g.cbf.hct",
    "asl.muse.rest.l.sca.cbf.hct",
    "asl.muse.rest.l.supp.motor.ctx.cbf.hct",
    "asl.muse.rest.l.cent.op.cbf.hct",
    "asl.muse.rest.l.fron.op.cbf.hct",
    "asl.muse.rest.l.par.op.cbf.hct"
  )
  
  asl.muse.rest.l.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.fron.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.fron.gm.var,  asl.muse.rest.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.fron.gm.var <- c(
    "asl.muse.rest.r.ant.orb.g.cbf.hct",
    "asl.muse.rest.r.lat.orb.g.cbf.hct",
    "asl.muse.rest.r.med.orb.g.cbf.hct",
    "asl.muse.rest.r.pos.orb.g.cbf.hct",
    "asl.muse.rest.r.ant.insula.cbf.hct",
    "asl.muse.rest.r.pos.insula.cbf.hct",
    "asl.muse.rest.r.fron.pole.cbf.hct",
    "asl.muse.rest.r.mid.fron.g.cbf.hct",
    "asl.muse.rest.r.inf.fron.op.g.cbf.hct",
    "asl.muse.rest.r.inf.fron.orb.g.cbf.hct",
    "asl.muse.rest.r.precent.g.cbf.hct",
    "asl.muse.rest.r.sup.fron.g.cbf.hct",
    "asl.muse.rest.r.inf.fron.tri.g.cbf.hct",
    "asl.muse.rest.r.rectus.g.cbf.hct",
    "asl.muse.rest.r.med.fron.ctx.cbf.hct",
    "asl.muse.rest.r.med.precent.g.cbf.hct",
    "asl.muse.rest.r.sup.med.fron.g.cbf.hct",
    "asl.muse.rest.r.sca.cbf.hct",
    "asl.muse.rest.r.supp.motor.ctx.cbf.hct",
    "asl.muse.rest.r.cent.op.cbf.hct",
    "asl.muse.rest.r.fron.op.cbf.hct",
    "asl.muse.rest.r.par.op.cbf.hct"
  )
  
  asl.muse.rest.r.fron.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.fron.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.fron.gm.var,  asl.muse.rest.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.fron.gm.var <- c(asl.muse.rest.l.fron.gm.var, asl.muse.rest.r.fron.gm.var)
  asl.muse.rest.fron.gm.regvol <- c(asl.muse.rest.l.fron.gm.regvol,
                                    asl.muse.rest.r.fron.gm.regvol)
  
  # create `asl.muse.rest.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.fron.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.fron.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##############
  ### Limbic ###
  ##############
  
  asl.muse.rest.l.limb.gm.var <- c(
    "asl.muse.rest.l.ant.cing.g.cbf.hct",
    "asl.muse.rest.l.mid.cing.g.cbf.hct",
    "asl.muse.rest.l.pos.cing.g.cbf.hct",
    "asl.muse.rest.l.ent.cbf.hct",
    "asl.muse.rest.l.parahipp.g.cbf.hct"
  )
  
  asl.muse.rest.l.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.limb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.limb.gm.var,  asl.muse.rest.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.limb.gm.var <- c(
    "asl.muse.rest.r.ant.cing.g.cbf.hct",
    "asl.muse.rest.r.mid.cing.g.cbf.hct",
    "asl.muse.rest.r.pos.cing.g.cbf.hct",
    "asl.muse.rest.r.ent.cbf.hct",
    "asl.muse.rest.r.parahipp.g.cbf.hct"
  )
  
  asl.muse.rest.r.limb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.limb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.limb.gm.var,  asl.muse.rest.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.limb.gm.var <- c(asl.muse.rest.l.limb.gm.var, asl.muse.rest.r.limb.gm.var)
  asl.muse.rest.limb.gm.regvol <- c(asl.muse.rest.l.limb.gm.regvol,
                                    asl.muse.rest.r.limb.gm.regvol)
  
  # create `asl.muse.rest.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.limb.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.limb.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #################
  ### Occipital ###
  #################
  
  asl.muse.rest.l.occ.gm.var <- c(
    "asl.muse.rest.l.occ.fus.g.cbf.hct",
    "asl.muse.rest.l.inf.occ.g.cbf.hct",
    "asl.muse.rest.l.mid.occ.g.cbf.hct",
    "asl.muse.rest.l.occ.pole.cbf.hct",
    "asl.muse.rest.l.sup.occ.g.cbf.hct",
    "asl.muse.rest.l.calc.ctx.cbf.hct",
    "asl.muse.rest.l.cuneus.cbf.hct",
    "asl.muse.rest.l.ling.g.cbf.hct"
  )
  
  asl.muse.rest.l.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.occ.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.occ.gm.var,  asl.muse.rest.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.occ.gm.var <- c(
    "asl.muse.rest.r.occ.fus.g.cbf.hct",
    "asl.muse.rest.r.inf.occ.g.cbf.hct",
    "asl.muse.rest.r.mid.occ.g.cbf.hct",
    "asl.muse.rest.r.occ.pole.cbf.hct",
    "asl.muse.rest.r.sup.occ.g.cbf.hct",
    "asl.muse.rest.r.calc.ctx.cbf.hct",
    "asl.muse.rest.r.cuneus.cbf.hct",
    "asl.muse.rest.r.ling.g.cbf.hct"
  )
  
  asl.muse.rest.r.occ.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.occ.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.occ.gm.var,  asl.muse.rest.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.occ.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.occ.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.occ.gm.var <- c(asl.muse.rest.l.occ.gm.var, asl.muse.rest.r.occ.gm.var)
  asl.muse.rest.occ.gm.regvol <- c(asl.muse.rest.l.occ.gm.regvol,
                                   asl.muse.rest.r.occ.gm.regvol)
  
  # create `asl.muse.rest.occ.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.occ.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.occ.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.occ.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Parietal ###
  ################
  
  asl.muse.rest.l.par.gm.var <- c(
    "asl.muse.rest.l.ang.g.cbf.hct",
    "asl.muse.rest.l.postcent.g.cbf.hct",
    "asl.muse.rest.l.supra.g.cbf.hct",
    "asl.muse.rest.l.sup.par.cbf.hct",
    "asl.muse.rest.l.med.postcent.g.cbf.hct",
    "asl.muse.rest.l.precuneus.cbf.hct"
  )
  
  asl.muse.rest.l.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.par.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.par.gm.var,  asl.muse.rest.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.par.gm.var <- c(
    "asl.muse.rest.r.ang.g.cbf.hct",
    "asl.muse.rest.r.postcent.g.cbf.hct",
    "asl.muse.rest.r.supra.g.cbf.hct",
    "asl.muse.rest.r.sup.par.cbf.hct",
    "asl.muse.rest.r.med.postcent.g.cbf.hct",
    "asl.muse.rest.r.precuneus.cbf.hct"
  )
  
  asl.muse.rest.r.par.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.par.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.par.gm.var,  asl.muse.rest.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.par.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.par.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.par.gm.var <- c(asl.muse.rest.l.par.gm.var, asl.muse.rest.r.par.gm.var)
  asl.muse.rest.par.gm.regvol <- c(asl.muse.rest.l.par.gm.regvol,
                                   asl.muse.rest.r.par.gm.regvol)
  
  # create `asl.muse.rest.par.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.par.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.par.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.par.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.par.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Temporal ###
  ################
  
  asl.muse.rest.l.temp.gm.var <- c(
    "asl.muse.rest.l.fus.g.cbf.hct",
    "asl.muse.rest.l.inf.temp.g.cbf.hct",
    "asl.muse.rest.l.mid.temp.g.cbf.hct",
    "asl.muse.rest.l.sup.temp.g.cbf.hct",
    "asl.muse.rest.l.temp.pole.cbf.hct",
    "asl.muse.rest.l.pp.cbf.hct",
    "asl.muse.rest.l.pt.cbf.hct",
    "asl.muse.rest.l.trans.temp.g.cbf.hct"
  )
  
  asl.muse.rest.l.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.temp.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.temp.gm.var,  asl.muse.rest.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.temp.gm.var <- c(
    "asl.muse.rest.r.fus.g.cbf.hct",
    "asl.muse.rest.r.inf.temp.g.cbf.hct",
    "asl.muse.rest.r.mid.temp.g.cbf.hct",
    "asl.muse.rest.r.sup.temp.g.cbf.hct",
    "asl.muse.rest.r.temp.pole.cbf.hct",
    "asl.muse.rest.r.pp.cbf.hct",
    "asl.muse.rest.r.pt.cbf.hct",
    "asl.muse.rest.r.trans.temp.g.cbf.hct"
  )
  
  asl.muse.rest.r.temp.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.temp.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.temp.gm.var,  asl.muse.rest.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.temp.gm.var <- c(asl.muse.rest.l.temp.gm.var, asl.muse.rest.r.temp.gm.var)
  asl.muse.rest.temp.gm.regvol <- c(asl.muse.rest.l.temp.gm.regvol,
                                    asl.muse.rest.r.temp.gm.regvol)
  
  # create `asl.muse.rest.temp.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.temp.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.temp.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ############
  ### Deep ###
  ############
  
  asl.muse.rest.l.deep.gm.var <- c(
    "asl.muse.rest.l.hipp.cbf.hct",
    "asl.muse.rest.l.amyg.cbf.hct",
    "asl.muse.rest.l.accum.cbf.hct",
    "asl.muse.rest.l.caudate.cbf.hct",
    "asl.muse.rest.l.pallidum.cbf.hct",
    "asl.muse.rest.l.putamen.cbf.hct",
    "asl.muse.rest.l.thal.cbf.hct",
    "asl.muse.rest.l.bf.cbf.hct"
  )
  
  asl.muse.rest.l.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.l.deep.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.deep.gm.var,  asl.muse.rest.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.deep.gm.var <- c(
    "asl.muse.rest.r.hipp.cbf.hct",
    "asl.muse.rest.r.amyg.cbf.hct",
    "asl.muse.rest.r.accum.cbf.hct",
    "asl.muse.rest.r.caudate.cbf.hct",
    "asl.muse.rest.r.pallidum.cbf.hct",
    "asl.muse.rest.r.putamen.cbf.hct",
    "asl.muse.rest.r.thal.cbf.hct",
    "asl.muse.rest.r.bf.cbf.hct"
  )
  
  asl.muse.rest.r.deep.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.r.deep.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.deep.gm.var,  asl.muse.rest.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.deep.gm.var <- c(asl.muse.rest.l.deep.gm.var, asl.muse.rest.r.deep.gm.var)
  asl.muse.rest.deep.gm.regvol <- c(asl.muse.rest.l.deep.gm.regvol,
                                    asl.muse.rest.r.deep.gm.regvol)
  
  # create `asl.muse.rest.deep.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.deep.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.deep.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ################
  ### Cerebrum ###
  ################
  
  asl.muse.rest.cerebrum.gm.var <- c(
    asl.muse.rest.fron.gm.var,
    asl.muse.rest.limb.gm.var,
    asl.muse.rest.occ.gm.var,
    asl.muse.rest.par.gm.var,
    asl.muse.rest.temp.gm.var
  )
  
  asl.muse.rest.cerebrum.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.cerebrum.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.cerebrum.gm.var,  asl.muse.rest.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.cerebrum.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.cerebrum.gm.cbf.hct =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.cerebrum.gm.var)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Cerebellum ###
  ##################
  
  asl.muse.rest.cb.gm.var <- c(
    "asl.muse.rest.l.cb.ext.cbf.hct",
    "asl.muse.rest.l.cb.verm.1.5.cbf.hct",
    "asl.muse.rest.l.cb.verm.6.7.cbf.hct",
    "asl.muse.rest.l.cb.verm.8.10.cbf.hct",
    "asl.muse.rest.r.cb.ext.cbf.hct",
    "asl.muse.rest.r.cb.verm.1.5.cbf.hct",
    "asl.muse.rest.r.cb.verm.6.7.cbf.hct",
    "asl.muse.rest.r.cb.verm.8.10.cbf.hct"
  )
  
  asl.muse.rest.cb.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.cb.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.cb.gm.var,  asl.muse.rest.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.cb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.cb.gm.cbf.hct =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.cb.gm.var
                    )) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.cb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.cb.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ################
  ### Total GM ###
  ################
  
  asl.muse.rest.gm.var <- c(
    asl.muse.rest.l.deep.gm.var,
    asl.muse.rest.l.fron.gm.var,
    asl.muse.rest.l.limb.gm.var,
    asl.muse.rest.l.occ.gm.var,
    asl.muse.rest.l.par.gm.var,
    asl.muse.rest.l.temp.gm.var,
    asl.muse.rest.r.deep.gm.var,
    asl.muse.rest.r.fron.gm.var,
    asl.muse.rest.r.limb.gm.var,
    asl.muse.rest.r.occ.gm.var,
    asl.muse.rest.r.par.gm.var,
    asl.muse.rest.r.temp.gm.var,
    asl.muse.rest.cb.gm.var
  )
  
  asl.muse.rest.gm.regvol <- gsub("cbf.hct", "regvol", asl.muse.rest.gm.var)
  
  # sanity check
  # sum(!(c(asl.muse.rest.gm.var,  asl.muse.rest.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.gm.cbf.hct =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.gm.var
                    )) *
                      dplyr::pick(dplyr::all_of(
                        asl.muse.rest.gm.regvol
                      ))) /
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.gm.regvol
                    )))) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.rest.var <- paste0("asl.muse.rest.", gsub("_", ".", meta.roi.name), ".cbf.hct")
  # sum(!asl.muse.rest.var %in% names(data))
  
  ##----------------------------------------------------------------------------
  ##----------------------------------------------------------------------------
  
  ####################
  ### Frontal (PVC)###
  ####################
  
  asl.muse.rest.l.fron.gm.var.pvc <- c(
    "asl.muse.rest.l.ant.orb.g.cbf.hct.pvc",
    "asl.muse.rest.l.lat.orb.g.cbf.hct.pvc",
    "asl.muse.rest.l.med.orb.g.cbf.hct.pvc",
    "asl.muse.rest.l.pos.orb.g.cbf.hct.pvc",
    "asl.muse.rest.l.ant.insula.cbf.hct.pvc",
    "asl.muse.rest.l.pos.insula.cbf.hct.pvc",
    "asl.muse.rest.l.fron.pole.cbf.hct.pvc",
    "asl.muse.rest.l.mid.fron.g.cbf.hct.pvc",
    "asl.muse.rest.l.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.rest.l.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.rest.l.precent.g.cbf.hct.pvc",
    "asl.muse.rest.l.sup.fron.g.cbf.hct.pvc",
    "asl.muse.rest.l.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.rest.l.rectus.g.cbf.hct.pvc",
    "asl.muse.rest.l.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.rest.l.med.precent.g.cbf.hct.pvc",
    "asl.muse.rest.l.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.rest.l.sca.cbf.hct.pvc",
    "asl.muse.rest.l.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.rest.l.cent.op.cbf.hct.pvc",
    "asl.muse.rest.l.fron.op.cbf.hct.pvc",
    "asl.muse.rest.l.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.fron.gm.var.pvc,  asl.muse.rest.l.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.fron.gm.var.pvc <- c(
    "asl.muse.rest.r.ant.orb.g.cbf.hct.pvc",
    "asl.muse.rest.r.lat.orb.g.cbf.hct.pvc",
    "asl.muse.rest.r.med.orb.g.cbf.hct.pvc",
    "asl.muse.rest.r.pos.orb.g.cbf.hct.pvc",
    "asl.muse.rest.r.ant.insula.cbf.hct.pvc",
    "asl.muse.rest.r.pos.insula.cbf.hct.pvc",
    "asl.muse.rest.r.fron.pole.cbf.hct.pvc",
    "asl.muse.rest.r.mid.fron.g.cbf.hct.pvc",
    "asl.muse.rest.r.inf.fron.op.g.cbf.hct.pvc",
    "asl.muse.rest.r.inf.fron.orb.g.cbf.hct.pvc",
    "asl.muse.rest.r.precent.g.cbf.hct.pvc",
    "asl.muse.rest.r.sup.fron.g.cbf.hct.pvc",
    "asl.muse.rest.r.inf.fron.tri.g.cbf.hct.pvc",
    "asl.muse.rest.r.rectus.g.cbf.hct.pvc",
    "asl.muse.rest.r.med.fron.ctx.cbf.hct.pvc",
    "asl.muse.rest.r.med.precent.g.cbf.hct.pvc",
    "asl.muse.rest.r.sup.med.fron.g.cbf.hct.pvc",
    "asl.muse.rest.r.sca.cbf.hct.pvc",
    "asl.muse.rest.r.supp.motor.ctx.cbf.hct.pvc",
    "asl.muse.rest.r.cent.op.cbf.hct.pvc",
    "asl.muse.rest.r.fron.op.cbf.hct.pvc",
    "asl.muse.rest.r.par.op.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.fron.gm.var,  asl.muse.rest.r.fron.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.fron.gm.var.pvc <- c(asl.muse.rest.l.fron.gm.var.pvc,
                                     asl.muse.rest.r.fron.gm.var.pvc)
  
  # create `asl.muse.rest.fron.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.fron.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.fron.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.fron.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.fron.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ####################
  ### Limbic (PVC) ###
  ####################
  
  asl.muse.rest.l.limb.gm.var.pvc <- c(
    "asl.muse.rest.l.ant.cing.g.cbf.hct.pvc",
    "asl.muse.rest.l.mid.cing.g.cbf.hct.pvc",
    "asl.muse.rest.l.pos.cing.g.cbf.hct.pvc",
    "asl.muse.rest.l.ent.cbf.hct.pvc",
    "asl.muse.rest.l.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.limb.gm.var.pvc,  asl.muse.rest.l.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.limb.gm.var.pvc <- c(
    "asl.muse.rest.r.ant.cing.g.cbf.hct.pvc",
    "asl.muse.rest.r.mid.cing.g.cbf.hct.pvc",
    "asl.muse.rest.r.pos.cing.g.cbf.hct.pvc",
    "asl.muse.rest.r.ent.cbf.hct.pvc",
    "asl.muse.rest.r.parahipp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.limb.gm.var.pvc,  asl.muse.rest.r.limb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.limb.gm.var.pvc <- c(asl.muse.rest.l.limb.gm.var.pvc, asl.muse.rest.r.limb.gm.var.pvc)
  
  # create `asl.muse.rest.limb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.limb.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.limb.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.limb.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.limb.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  #######################
  ### Occipital (PVC) ###
  #######################
  
  asl.muse.rest.l.occ.gm.var.pvc <- c(
    "asl.muse.rest.l.occ.fus.g.cbf.hct.pvc",
    "asl.muse.rest.l.inf.occ.g.cbf.hct.pvc",
    "asl.muse.rest.l.mid.occ.g.cbf.hct.pvc",
    "asl.muse.rest.l.occ.pole.cbf.hct.pvc",
    "asl.muse.rest.l.sup.occ.g.cbf.hct.pvc",
    "asl.muse.rest.l.calc.ctx.cbf.hct.pvc",
    "asl.muse.rest.l.cuneus.cbf.hct.pvc",
    "asl.muse.rest.l.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.occ.gm.var.pvc,  asl.muse.rest.l.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.occ.gm.var.pvc <- c(
    "asl.muse.rest.r.occ.fus.g.cbf.hct.pvc",
    "asl.muse.rest.r.inf.occ.g.cbf.hct.pvc",
    "asl.muse.rest.r.mid.occ.g.cbf.hct.pvc",
    "asl.muse.rest.r.occ.pole.cbf.hct.pvc",
    "asl.muse.rest.r.sup.occ.g.cbf.hct.pvc",
    "asl.muse.rest.r.calc.ctx.cbf.hct.pvc",
    "asl.muse.rest.r.cuneus.cbf.hct.pvc",
    "asl.muse.rest.r.ling.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.occ.gm.var.pvc,  asl.muse.rest.r.occ.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.occ.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.occ.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.occ.gm.var.pvc <- c(asl.muse.rest.l.occ.gm.var.pvc, asl.muse.rest.r.occ.gm.var.pvc)
  
  # create `asl.muse.rest.occ.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.occ.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.occ.gm.var.pvc)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.occ.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Parietal (PVC) ###
  ######################
  
  asl.muse.rest.l.par.gm.var.pvc <- c(
    "asl.muse.rest.l.ang.g.cbf.hct.pvc",
    "asl.muse.rest.l.postcent.g.cbf.hct.pvc",
    "asl.muse.rest.l.supra.g.cbf.hct.pvc",
    "asl.muse.rest.l.sup.par.cbf.hct.pvc",
    "asl.muse.rest.l.med.postcent.g.cbf.hct.pvc",
    "asl.muse.rest.l.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.par.gm.var.pvc,  asl.muse.rest.l.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.par.gm.var.pvc <- c(
    "asl.muse.rest.r.ang.g.cbf.hct.pvc",
    "asl.muse.rest.r.postcent.g.cbf.hct.pvc",
    "asl.muse.rest.r.supra.g.cbf.hct.pvc",
    "asl.muse.rest.r.sup.par.cbf.hct.pvc",
    "asl.muse.rest.r.med.postcent.g.cbf.hct.pvc",
    "asl.muse.rest.r.precuneus.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.par.gm.var.pvc,  asl.muse.rest.r.par.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.par.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.par.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.par.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.par.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.par.gm.var.pvc <- c(asl.muse.rest.l.par.gm.var.pvc, asl.muse.rest.r.par.gm.var.pvc)
  
  # create `asl.muse.rest.par.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.par.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.par.gm.var.pvc)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.par.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.par.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Temporal (PVC) ###
  ######################
  
  asl.muse.rest.l.temp.gm.var.pvc <- c(
    "asl.muse.rest.l.fus.g.cbf.hct.pvc",
    "asl.muse.rest.l.inf.temp.g.cbf.hct.pvc",
    "asl.muse.rest.l.mid.temp.g.cbf.hct.pvc",
    "asl.muse.rest.l.sup.temp.g.cbf.hct.pvc",
    "asl.muse.rest.l.temp.pole.cbf.hct.pvc",
    "asl.muse.rest.l.pp.cbf.hct.pvc",
    "asl.muse.rest.l.pt.cbf.hct.pvc",
    "asl.muse.rest.l.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.temp.gm.var.pvc,  asl.muse.rest.l.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.temp.gm.var.pvc <- c(
    "asl.muse.rest.r.fus.g.cbf.hct.pvc",
    "asl.muse.rest.r.inf.temp.g.cbf.hct.pvc",
    "asl.muse.rest.r.mid.temp.g.cbf.hct.pvc",
    "asl.muse.rest.r.sup.temp.g.cbf.hct.pvc",
    "asl.muse.rest.r.temp.pole.cbf.hct.pvc",
    "asl.muse.rest.r.pp.cbf.hct.pvc",
    "asl.muse.rest.r.pt.cbf.hct.pvc",
    "asl.muse.rest.r.trans.temp.g.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.temp.gm.var.pvc,  asl.muse.rest.r.temp.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.temp.gm.var.pvc <- c(asl.muse.rest.l.temp.gm.var.pvc, asl.muse.rest.r.temp.gm.var.pvc)
  
  # create `asl.muse.rest.temp.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.temp.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.temp.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.temp.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.temp.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ##################
  ### Deep (PVC) ###
  ##################
  
  asl.muse.rest.l.deep.gm.var.pvc <- c(
    "asl.muse.rest.l.hipp.cbf.hct.pvc",
    "asl.muse.rest.l.amyg.cbf.hct.pvc",
    "asl.muse.rest.l.accum.cbf.hct.pvc",
    "asl.muse.rest.l.caudate.cbf.hct.pvc",
    "asl.muse.rest.l.pallidum.cbf.hct.pvc",
    "asl.muse.rest.l.putamen.cbf.hct.pvc",
    "asl.muse.rest.l.thal.cbf.hct.pvc",
    "asl.muse.rest.l.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.l.deep.gm.var.pvc,  asl.muse.rest.l.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.l.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.l.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.l.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.l.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.r.deep.gm.var.pvc <- c(
    "asl.muse.rest.r.hipp.cbf.hct.pvc",
    "asl.muse.rest.r.amyg.cbf.hct.pvc",
    "asl.muse.rest.r.accum.cbf.hct.pvc",
    "asl.muse.rest.r.caudate.cbf.hct.pvc",
    "asl.muse.rest.r.pallidum.cbf.hct.pvc",
    "asl.muse.rest.r.putamen.cbf.hct.pvc",
    "asl.muse.rest.r.thal.cbf.hct.pvc",
    "asl.muse.rest.r.bf.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.r.deep.gm.var.pvc,  asl.muse.rest.r.deep.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.r.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.r.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.r.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.r.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.deep.gm.var.pvc <- c(asl.muse.rest.l.deep.gm.var.pvc, asl.muse.rest.r.deep.gm.var.pvc)
  
  # create `asl.muse.rest.deep.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.deep.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.deep.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.deep.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.deep.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ######################
  ### Cerebrum (PVC) ###
  ######################
  
  asl.muse.rest.cerebrum.gm.var.pvc <- c(
    asl.muse.rest.fron.gm.var.pvc,
    asl.muse.rest.limb.gm.var.pvc,
    asl.muse.rest.occ.gm.var.pvc,
    asl.muse.rest.par.gm.var.pvc,
    asl.muse.rest.temp.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.cerebrum.gm.var.pvc,  asl.muse.rest.cerebrum.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.cerebrum.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(
      asl.muse.rest.cerebrum.gm.cbf.hct.pvc =
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.cerebrum.gm.var.pvc)
        ) *
          dplyr::pick(
            dplyr::all_of(asl.muse.rest.cerebrum.gm.regvol)
          )) /
        rowSums(dplyr::pick(
          dplyr::all_of(asl.muse.rest.cerebrum.gm.regvol)
        ))
    ) %>%
    as.data.frame()
  
  ########################
  ### Cerebellum (PVC) ###
  ########################
  
  asl.muse.rest.cb.gm.var.pvc <- c(
    "asl.muse.rest.l.cb.ext.cbf.hct.pvc",
    "asl.muse.rest.l.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.rest.l.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.rest.l.cb.verm.8.10.cbf.hct.pvc",
    "asl.muse.rest.r.cb.ext.cbf.hct.pvc",
    "asl.muse.rest.r.cb.verm.1.5.cbf.hct.pvc",
    "asl.muse.rest.r.cb.verm.6.7.cbf.hct.pvc",
    "asl.muse.rest.r.cb.verm.8.10.cbf.hct.pvc"
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.cb.gm.var.pvc,  asl.muse.rest.cb.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.cb.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.cb.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.cb.gm.var.pvc
                    )) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.cb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.cb.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ######################
  ### Total GM (PVC) ###
  ######################
  
  asl.muse.rest.gm.var.pvc <- c(
    asl.muse.rest.l.deep.gm.var.pvc,
    asl.muse.rest.l.fron.gm.var.pvc,
    asl.muse.rest.l.limb.gm.var.pvc,
    asl.muse.rest.l.occ.gm.var.pvc,
    asl.muse.rest.l.par.gm.var.pvc,
    asl.muse.rest.l.temp.gm.var.pvc,
    asl.muse.rest.r.deep.gm.var.pvc,
    asl.muse.rest.r.fron.gm.var.pvc,
    asl.muse.rest.r.limb.gm.var.pvc,
    asl.muse.rest.r.occ.gm.var.pvc,
    asl.muse.rest.r.par.gm.var.pvc,
    asl.muse.rest.r.temp.gm.var.pvc,
    asl.muse.rest.cb.gm.var.pvc
  )
  
  # sanity check
  # sum(!(c(asl.muse.rest.gm.var.pvc,  asl.muse.rest.gm.regvol) %in% names(data)))
  
  # create `asl.muse.rest.gm.cbf.hct.pvc`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.gm.cbf.hct.pvc =
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.gm.var.pvc
                    )) *
                      dplyr::pick(dplyr::all_of(
                        asl.muse.rest.gm.regvol
                      ))) /
                    rowSums(dplyr::pick(dplyr::all_of(
                      asl.muse.rest.gm.regvol
                    )))) %>%
    as.data.frame()
  
  # sanity check
  # asl.muse.rest.var.pvc <- paste0("asl.muse.rest.", gsub("_", ".", meta.roi.name), ".cbf.hct.pvc")
  # sum(!asl.muse.rest.var.pvc %in% names(data))
  
  return(data)
}



