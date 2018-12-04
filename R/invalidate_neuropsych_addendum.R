#' Invalidate certain neuropsychological addendum variables for certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological addendum variables.
#' @export

invalidate_neuropsych_addendum <- function(data) {

  # OAK 20181120: Since this function will be called before format_names(), the variables should have underscores
  # nosuffix <- Hmisc::Cs(
  #   np.pvltrecog.m,
  #   np.pvltrecog.ts,
  #   np.pvltrecog.tu1,
  #   np.pvltrecog.ts1,
  #   np.pvltrecog.sem,
  #   np.pvltrecog.ur,
  #   np.pvltrecog.foil,
  #   np.pvltrecog.falsepos,
  #   np.pvltrecog.discrim
  # )
  nosuffix <- Hmisc::Cs(
    np_pvltrecog_m,
    np_pvltrecog_ts,
    np_pvltrecog_tu1,
    np_pvltrecog_ts1,
    np_pvltrecog_sem,
    np_pvltrecog_ur,
    np_pvltrecog_foil,
    np_pvltrecog_falsepos,
    np_pvltrecog_discrim
  )

  # withsuffix <- paste0(nosuffix, ".addend")
  withsuffix <- paste0(nosuffix, "_addend")

  x <- intersect(names(data), c(nosuffix, withsuffix))

  #data[data$vmac.id.add == 1137 & data$epoch == 1, x] <- NA
  # 22 May 2015: Kim confirmed in email
  #   that this is the right map.id
  # OAK 20181120: Removed epoch argument in subset so that I can call this function in process_raw_data.R
  # data[data$map.id == 194 & data$epoch == 1, x] <- NA
  data[data$map.id == 194, x] <- NA

  # 22 May 2015: Kim confirmed in email
  #   that this person is not a MAP participant
  #x1 <- paste0(Cs(np.pvlt1,
  #       np.pvlt1.intrus,
  #       np.pvlt1.pers,
  #       np.pvlt1.clust,
  #       np.pvlt2,
  #       np.pvlt2.intrus,
  #       np.pvlt2.pers,
  #       np.pvlt2.clust,
  #       np.pvlt3,
  #       np.pvlt3.intrus,
  #       np.pvlt3.pers,
  #       np.pvlt3.clust,
  #       np.pvlt4,
  #       np.pvlt4.intrus,
  #       np.pvlt4.pers,
  #       np.pvlt4.clust,
  #       np.pvlt5,
  #       np.pvlt5.intrus,
  #       np.pvlt5.pers,
  #       np.pvlt5.clust
  #), ".addend")
  #data[data$vmac.id.addend == 2963 & data$epoch == 1, x1] <- NA

  return(data)
}
