schwarz <- Cs(
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

mcevoy.Y <- Cs(
  avg.hippocampus,
  avg.entorhinal.thickness,
  avg.middletemporal.thickness,
  avg.bankssts.thickness,
  avg.isthmuscingulate.thickness,
  avg.superiortemporal.thickness,
  avg.medialorbitofrontal.thickness,
  avg.lateralorbitofrontal.thickness
)

## mcevoy.fun == function to calculate AD.sig.mcevoy
# mcevoy.fun <- function(myY, coef, data){
#   df = matrix(nrow = dim(data)[1], ncol = length(myY))
#   for (i in 1:length(myY)) {
#     fit.lm <- lm(formula = as.formula(paste0(myY[i], " ~ age + sex.factor + intracranialvol")), 
#                  data = data,
#                  na.action = na.exclude)
#     df[, i] <- rstandard(fit.lm)
#     df[, i] <- df[, i] * coef[i]
#   }
#   return(data.frame(AD.sig.mcevoy = rowSums(df)))
# }

mcevoy.coef <- c(0.709, 0.597, 0.506, 0.453, 0.395, 0.328, 0.269, 0.250)

addADsig <- function(dat){
  ## create mcevoy.Y variables
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
  
  myfit <- paste0("fit.", 1:length(unique(mcevoy.Y)))
  mcevoy.all.temp <- NULL
  
  for (i in 1:length(unique(dat$epoch))){
    
    epoch.df <- dat %>%
      filter(epoch == i)
    
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
    
  dat <- merge(dat, mcevoy.all.temp, by = c("map.id", "epoch"))
    
  # temp.df <- subset(dat, epoch == 1, select = c("map.id", "epoch", mcevoy.Y, "age", "sex.factor", "intracranialvol"))
  # temp.df.e1 <- tempdat %>% 
  #   filter(epoch == 1) %>% 
  #   select(., c("map.id", "epoch", mcevoy.Y, "age", "sex.factor", "intracranialvol"))
  # 
  # mcevoy.e1 <<- data.frame(
  #   map.id = temp.df$map.id,
  #   epoch = temp.df$epoch,
  #   AD.sig.mcevoy = mcevoy.fun(myY = mcevoy.Y, coef = mcevoy.coef, data = temp.df),
  #   stringsAsFactors = FALSE)
  # rm(temp.df)
  # 
  # temp.df <- subset(dat, epoch == 2, select = c("map.id", "epoch", mcevoy.Y, "age", "sex.factor", "intracranialvol"))
  # mcevoy.e2 <<- data.frame(
  #   map.id = temp.df$map.id,
  #   epoch = temp.df$epoch,
  #   AD.sig.mcevoy = mcevoy.fun(myY = mcevoy.Y, coef = mcevoy.coef, data = temp.df),
  #   stringsAsFactors = FALSE)
  # rm(temp.df)
  # 
  # temp.df <- subset(dat, epoch == 3, select = c("map.id", "epoch", mcevoy.Y, "age", "sex.factor", "intracranialvol"))
  # mcevoy.e3 <<- data.frame(
  #   map.id = temp.df$map.id,
  #   epoch = temp.df$epoch,
  #   AD.sig.mcevoy = mcevoy.fun(myY = mcevoy.Y, coef = mcevoy.coef, data = temp.df),
  #   stringsAsFactors = FALSE)
  # rm(temp.df)
  # 
  # mcevoy.all <- rbind(
  #   mcevoy.e1, mcevoy.e2,
  #   make.row.names = FALSE,
  #   stringsAsFactors = FALSE
  # )
  # 
  # mcevoy.all <- rbind(
  #   mcevoy.all, mcevoy.e3,
  #   make.row.names = FALSE,
  #   stringsAsFactors = FALSE
  # )
  
  # dat$AD.sig.mcevoy <- mcevoy.all$AD.sig.mcevoy
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
