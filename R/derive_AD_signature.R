#' Derive, label, and add AD signature McEvoy and Schwarz variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added AD signature variables.
#' @export

derive_AD_signature <- function(data) {
  # Per the VMAC Biostatistics meeting on 2024/02/27, we will use T1 hippocampus for AD.sig.McEvoy, and will
  # only provide harmonized AD signature variables in the merged data.

  
  # data <- merged.df

  schwarz <- Hmisc::Cs(
    rh.entorhinal.thickness.combat,
    rh.inferiortemporal.thickness.combat,
    rh.middletemporal.thickness.combat,
    rh.inferiorparietal.thickness.combat,
    rh.fusiform.thickness.combat,
    rh.precuneus.thickness.combat,
    lh.entorhinal.thickness.combat,
    lh.inferiortemporal.thickness.combat,
    lh.middletemporal.thickness.combat,
    lh.inferiorparietal.thickness.combat,
    lh.fusiform.thickness.combat,
    lh.precuneus.thickness.combat
  )

  mcevoy.Y <- Hmisc::Cs(
    avg.hippocampus.combat,
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
      avg.hippocampus.combat = mean(c(ma.lh.hippocampus.vol.combat, ma.rh.hippocampus.vol.combat)),
      avg.entorhinal.thickness.combat = mean(c(rh.entorhinal.thickness.combat, lh.entorhinal.thickness.combat)),
      avg.middletemporal.thickness.combat = mean(c(rh.middletemporal.thickness.combat, lh.middletemporal.thickness.combat)),
      avg.bankssts.thickness.combat = mean(c(rh.bankssts.thickness.combat, lh.bankssts.thickness.combat)),
      avg.isthmuscingulate.thickness.combat = mean(c(rh.isthmuscingulate.thickness.combat, lh.isthmuscingulate.thickness.combat)),
      avg.superiortemporal.thickness.combat = mean(c(rh.superiortemporal.thickness.combat, lh.superiortemporal.thickness.combat)),
      avg.medialorbitofrontal.thickness.combat = mean(c(rh.medialorbitofrontal.thickness.combat, lh.medialorbitofrontal.thickness.combat)),
      avg.lateralorbitofrontal.thickness.combat = mean(c(rh.lateralorbitofrontal.thickness.combat, lh.lateralorbitofrontal.thickness.combat))
    )

  mcevoy.all.temp <- NULL

  derive_mcevoy.df <- data %>%
    dplyr::filter(epoch == 1) %>%
    dplyr::filter(!is.na(avg.hippocampus.combat)) %>%
    dplyr::filter(diagnosis.factor.base == "Normal") %>%
    dplyr::select(
      all_of(
        Cs(map.id, epoch, redcap.repeat.instrument, redcap.repeat.instance,
           age, sex.factor, diagnosis.factor.base, ma.total.intracranial.vol.combat, avg.hippocampus.combat, avg.entorhinal.thickness.combat,
           avg.middletemporal.thickness.combat, avg.bankssts.thickness.combat, avg.isthmuscingulate.thickness.combat,
           avg.superiortemporal.thickness.combat, avg.medialorbitofrontal.thickness.combat, avg.lateralorbitofrontal.thickness.combat)
      )
    )

  # derive_mcevoy.df <- droplevels(derive_mcevoy.df)

  fit.1 <- lm(formula = as.formula(paste0(mcevoy.Y[1], ' ~ age + sex.factor + ma.total.intracranial.vol.combat')),
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

  mcevoy.temp$AD.sig.mcevoy.combat <- rowSums(mcevoy.temp[, c("fit.1", "fit.2", "fit.3", "fit.4", "fit.5", "fit.6", "fit.7", "fit.8")])

  mcevoy.temp <- mcevoy.temp %>%
    select(map.id, epoch, redcap.repeat.instrument, redcap.repeat.instance, AD.sig.mcevoy.combat)

  data <- left_join(data, mcevoy.temp, by = c("map.id", "epoch", "redcap.repeat.instrument", "redcap.repeat.instance"))

  data$AD.sig.schwarz.combat <- rowMeans(data[, schwarz], na.rm = FALSE)

  data <- within(data, {
    label(avg.hippocampus.combat) <- "Avg hippocampus vol - T1 3T FS (Harmonized)"
    label(avg.entorhinal.thickness.combat) <- "Avg entorhinal cortex thickness - T1 3T FS (Harmonized)"
    label(avg.middletemporal.thickness.combat) <- "Avg middletemporal thickness - T1 3T FS (Harmonized)"
    label(avg.bankssts.thickness.combat.combat) <- "Avg bankssts thickness - T1 3T FS (Harmonized)"
    label(avg.isthmuscingulate.thickness.combat) <- "Avg isthmuscingulate thickness - T1 3T FS (Harmonized)"
    label(avg.superiortemporal.thickness.combat) <- "Avg superiortemporal thickness - T1 3T FS (Harmonized)"
    label(avg.medialorbitofrontal.thickness.combat) <- "Avg mediaorbitofrontal thickness - T1 3T FS (Harmonized)"
    label(avg.lateralorbitofrontal.thickness.combat) <- "Avg lateralorbitofrontal thickness - T1 3T FS (Harmonized)"
    label(AD.sig.mcevoy.combat) <- "AD signature - McEvoy"
    label(AD.sig.schwarz.combat) <- "AD signature - Schwarz"
  })

  return(data)
}

