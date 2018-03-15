#' A postprocessing function to derive and add AD signature - McEvoy
#'
#' @param dat A dataframe containing VMAC variables
#' @return The input dataframe and a new variable (AD.sig.mcevoy)
#' @export

AD.sig.mcevoy.fun <- function(dat){
  # define: AD.sig.mcevoy components
  mcevoy.components <- Cs(
    avg.hippocampus,
    avg.entorhinal.thickness,
    avg.middletemporal.thickness,
    avg.bankssts.thickness,
    avg.isthmuscingulate.thickness,
    avg.superiortemporal.thickness,
    avg.medialorbitofrontal.thickness,
    avg.lateralorbitofrontal.thickness
  )

  # define: AD.sig.mcevoy component weights
  mcevoy.coef <- c(0.709, 0.597, 0.506, 0.453, 0.395, 0.328, 0.269, 0.250)

  # derive: mcevoy.components
  dat <- dat %>%
    rowwise() %>%
    mutate(
      avg.hippocampus = mean(c(right.hippocampus, left.hippocampus)),
      avg.entorhinal.thickness = mean(c(rh.entorhinal.thickness, lh.entorhinal.thickness)),
      avg.middletemporal.thickness = mean(c(rh.middletemporal.thickness, lh.middletemporal.thickness)),
      avg.bankssts.thickness = mean(c(rh.isthmuscingulate.thickness, lh.isthmuscingulate.thickness)),
      avg.isthmuscingulate.thickness = mean(c(rh.superiortemporal.thickness, lh.superiortemporal.thickness)),
      avg.superiortemporal.thickness = mean(c(right.hippocampus, left.hippocampus)),
      avg.medialorbitofrontal.thickness = mean(c(rh.medialorbitofrontal.thickness, lh.medialorbitofrontal.thickness)),
      avg.lateralorbitofrontal.thickness = mean(c(rh.lateralorbitofrontal.thickness, lh.lateralorbitofrontal.thickness))
    )

  # initialize: temp variables
  AD.fit.list <- paste0("fit.", 1:length(unique(mcevoy.components)))
  mcevoy.all.temp <- NULL

  # derive: AD.sig.mcevoy
  ## by epoch
  for (i in 1:length(unique(dat$epoch))){
    epoch.df <- dat %>%
      filter(
        epoch == i
      )

    mcevoy.temp <- data.frame(
      map.id = epoch.df$map.id,
      epoch = epoch.df$epoch,
      AD.sig.mcevoy = 0,
      stringsAsFactors = FALSE
    )

    fit.1 <- lm(formula = avg.hippocampus ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.2 <- lm(formula = avg.entorhinal.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.3 <- lm(formula = avg.middletemporal.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.4 <- lm(formula = avg.bankssts.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.5 <- lm(formula = avg.isthmuscingulate.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.6 <- lm(formula = avg.superiortemporal.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.7 <- lm(formula = avg.medialorbitofrontal.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)
    fit.8 <- lm(formula = avg.lateralorbitofrontal.thickness ~ age + sex.factor + intracranialvol,
                data = epoch.df,
                na.action = na.exclude)

    for (i in 1:length(mcevoy.coef)){
      mcevoy.temp[, "AD.sig.mcevoy"] <- mcevoy.temp[, "AD.sig.mcevoy"] + (rstandard(get(AD.fit.list[i])) * mcevoy.coef[i])
    }
    mcevoy.all.temp <- rbind(mcevoy.all.temp, mcevoy.temp)
  }

  # merge AD.sig.mcevoy with input dat
  dat <- merge(dat, mcevoy.all.temp, by = c("map.id", "epoch"))

  # add Hmisc::label
  dat <- within(dat, {
    label(avg.hippocampus) <- "Avg hippocampus vol - T1 3T FreeSurfer"
    label(avg.entorhinal.thickness) <- "Avg entorhinal cortex thickness - T1 3T FS"
    label(avg.middletemporal.thickness) <- "Avg middletemporal thickness - T1 3T FS"
    label(avg.bankssts.thickness) <- "Avg bankssts thickness - T1 3T FS"
    label(avg.isthmuscingulate.thickness) <- "Avg isthmuscingulate thickness - T1 3T FS"
    label(avg.superiortemporal.thickness) <- "Avg superiortemporal thickness - T1 3T FS"
    label(avg.medialorbitofrontal.thickness) <- "Avg mediaorbitofrontal thickness - T1 3T FS"
    label(avg.lateralorbitofrontal.thickness) <- "Avg lateralorbitofrontal thickness - T1 3T FS"
    label(AD.sig.mcevoy) <- "AD signature - McEvoy"
  })
}
