#' A postprocessing function to derive and add AD signature McEvoy and Schwarz
#'
#' @param dat A dataframe containing VMAC variables
#' @return The input dataframe and two new variables (AD.sig.mcevoy and AD.sig.schwarz)
#' @export

addADsig <- function(dat) {

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

  mcevoy.coef <- c(0.709, 0.597, 0.506, 0.453, 0.395, 0.328, 0.269, 0.250)

  ## create mcevoy.Y variables
  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      avg.hippocampus = mean(c(right.hippocampus, left.hippocampus)),
      avg.entorhinal.thickness = mean(c(rh.entorhinal.thickness, lh.entorhinal.thickness)),
      avg.middletemporal.thickness = mean(c(rh.middletemporal.thickness, lh.middletemporal.thickness)),
      avg.bankssts.thickness = mean(c(rh.isthmuscingulate.thickness, lh.isthmuscingulate.thickness)),
      avg.isthmuscingulate.thickness = mean(c(rh.superiortemporal.thickness, lh.superiortemporal.thickness)),
      avg.superiortemporal.thickness = mean(c(right.hippocampus, left.hippocampus)),
      avg.medialorbitofrontal.thickness = mean(c(rh.medialorbitofrontal.thickness, lh.medialorbitofrontal.thickness)),
      avg.lateralorbitofrontal.thickness = mean(c(rh.lateralorbitofrontal.thickness, lh.lateralorbitofrontal.thickness))
    )

  myfit <- paste0("fit.", 1:length(unique(mcevoy.Y)))
  mcevoy.all.temp <- NULL

  #for (i in seq_along(unique(dat$epoch))) {
  for (i in 1:3) {

    epoch.df <- dat %>%
      dplyr::filter(epoch == i) %>%
      select(
        one_of(
          Cs(map.id, epoch, age, sex.factor, intracranialvol, avg.hippocampus, avg.entorhinal.thickness,
             avg.middletemporal.thickness, avg.bankssts.thickness, avg.isthmuscingulate.thickness,
             avg.superiortemporal.thickness, avg.medialorbitofrontal.thickness, avg.lateralorbitofrontal.thickness)
        )
      )

    epoch.df <- droplevels(epoch.df)

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

    for (i in 1:length(mcevoy.coef)) {
      mcevoy.temp[, "AD.sig.mcevoy"] <- mcevoy.temp[, "AD.sig.mcevoy"] + (rstandard(get(myfit[i])) * mcevoy.coef[i])
    }

    mcevoy.all.temp <- rbind(mcevoy.all.temp, mcevoy.temp)
  }

  dat <- merge(dat, mcevoy.all.temp, by = c("map.id", "epoch"), all.x = TRUE)

  dat$AD.sig.schwarz <- rowSums(dat[, schwarz], na.rm = FALSE)

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
    label(AD.sig.schwarz) <- "AD signature - Schwarz"
  })

}
