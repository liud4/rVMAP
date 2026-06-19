#' Derive, label, and add DLMUSE volume variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added DLMUSE volume variables.
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
  
  asl.muse.rest.var <- paste0("asl.muse.rest.", gsub("_", ".", meta.roi.name), ".cbf.hct")
  
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
    dplyr::mutate(asl.muse.rest.l.fron.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.fron.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.fron.gm.regvol)
                    ))) %>%
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
    dplyr::mutate(asl.muse.rest.r.fron.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.fron.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.fron.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.fron.gm.var <- c(asl.muse.rest.l.fron.gm.var, asl.muse.rest.r.fron.gm.var)
  asl.muse.rest.fron.gm.regvol <- c(asl.muse.rest.l.fron.gm.regvol, asl.muse.rest.r.fron.gm.regvol)
  
  # create `asl.muse.rest.fron.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.fron.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.fron.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.fron.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.fron.gm.regvol)
                    ))) %>%
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
    dplyr::mutate(asl.muse.rest.l.limb.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.limb.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.limb.gm.regvol)
                    ))) %>%
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
    dplyr::mutate(asl.muse.rest.r.limb.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.limb.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.limb.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.limb.gm.var <- c(asl.muse.rest.l.limb.gm.var, asl.muse.rest.r.limb.gm.var)
  asl.muse.rest.limb.gm.regvol <- c(asl.muse.rest.l.limb.gm.regvol, asl.muse.rest.r.limb.gm.regvol)
  
  # create `asl.muse.rest.limb.gm.cbf.hct`
  data <- data %>%
    dplyr::mutate(asl.muse.rest.limb.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.limb.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.limb.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.limb.gm.regvol)
                    ))) %>%
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
    dplyr::mutate(asl.muse.rest.l.occ.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.occ.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.l.occ.gm.regvol)
                    ))) %>%
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
    dplyr::mutate(asl.muse.rest.r.occ.gm.cbf.hct =
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.occ.gm.var)
                    ) *
                      dplyr::pick(
                        dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
                      )) /
                    rowSums(dplyr::pick(
                      dplyr::all_of(asl.muse.rest.r.occ.gm.regvol)
                    ))) %>%
    as.data.frame()
  
  ###
  
  asl.muse.rest.occ.gm.var <- c(asl.muse.rest.l.occ.gm.var, asl.muse.rest.r.occ.gm.var)
  asl.muse.rest.occ.gm.regvol <- c(asl.muse.rest.l.occ.gm.regvol, asl.muse.rest.r.occ.gm.regvol)
  
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
  
  return(data)
}
