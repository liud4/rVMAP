#' A postprocessing function to derive and add AD signature - Schwarz
#'
#' @param dat A dataframe containing VMAC variables
#' @return The input dataframe and a new variable (AD.sig.schwarz)
#' @export

AD.sig.schwarz.fun <- function(dat){
  ## define: AD.sig.schwarz components
  schwarz.components <- Cs(
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

  ## derive: AD.sig.schwarz
  dat <- dat %>%
    mutate(
      AD.sig.schwarz = select(., one_of(schwarz.components)) %>% rowSums()
    )

  # add Hmisc::label
  dat <- within(dat, {
    label(AD.sig.schwarz) <- "AD signature - Schwarz"
  })
}
