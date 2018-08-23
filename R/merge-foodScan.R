#' Derive, convert, label, and add food scan variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added food scan variables.
#' @export

derive_food_scan <- function(data) {
  foodvar <- paste0(
    "food01",
    formatC(c(letters[1:15]), width = 1, format = "d", flag = "0")
  )

  foodvar.freq <- paste0(
    "food01",
    formatC(c(letters[1:15]), width = 1, format = "d", flag = "0"),
    ".freq"
  )

  # Pro-rate for missing
  propNonMissing <- function(vec) round(mean(!is.na(vec)), 2)

  avgscore <- function(vec, threshold = 0.85) {
    # calc avg score only if >= <threshold> of items are nonmissing
    if (propNonMissing(vec) < threshold) {
      return(NA)
    } else {
      return(round(mean(vec, na.rm= TRUE), 1))
    }
  }

  pro.rate <- round(apply(data[, foodvar], MARGIN = 1, FUN = avgscore))

  for (j in 1:length(foodvar)) {
    for (i in 1:nrow(data)) {
      data[i, foodvar[j]] <- ifelse(is.na(data[i, foodvar[j]]), pro.rate, data[i, foodvar[j]])
    }
  }

  # Create variables for frequency for each food
  for (i in 1:length(foodvar)) {
    data[, foodvar.freq[i]] = ifelse(data[, foodvar[i]] == 0, 0,
                                     ifelse(data[, foodvar[i]] == 1, 0.018,
                                            ifelse(data[, foodvar[i]] == 2, 0.066,
                                                   ifelse(data[, foodvar[i]] == 3, 0.214,
                                                          ifelse(data[, foodvar[i]] == 4, 0.499,
                                                                 ifelse(data[, foodvar[i]] == 5, 0.784,
                                                                        ifelse(data[, foodvar[i]] == 6, 1,
                                                                               2)))))))
  }

  # Total Fat
  data$totfat <- data$food01e.freq + data$food01k.freq + data$food01o.freq

  # Reduced Fat
  data$regfat.freq <- dplyr::case_when(
    is.na(data$food02) ~ NA_real_,
    data$food02 == 0 | data$food02 == 1 ~ 1,
    data$food02 == 2 ~ 0.75,
    data$food02 == 3 ~ 0.5,
    data$food02 == 4 ~ 0.25,
    TRUE ~ 0
  )

  data$reg.fat <- data$regfat.freq * data$totfat

  # Create Age Categories (minimum age is 60 for inputted data)
  data$age.cat <- factor(
    ifelse(data$age > 58 & data$age <= 67, 1,
           ifelse(data$age > 68 & data$age <= 77, 2,
                  3)))

  # Create coefficient matrix for food and age categories
  food.matrix.sex1 <- data.frame(matrix(c(1,2,3,214.375,198.9375,160.725,27,26,24,46,39,33,122,118,114.25,9.15,13.75,4.58,29.4,29.4,29.38,92,80,80,248,186.75,186.75,57,57,57,28.35,24,22.88,85.5,85.5,97,7.883333,7.1,7,165,158,158),nrow=3))
  food.matrix.sex2 <- data.frame(matrix(c(1,2,3,196,183.75,183.75,18,19.5,16,33,33,33.5,118,112.427143,109,6.11,10.31,4.58,29.38,29.38,22.03,68,56,46,186.75,186.6,186.75,57,57,57,24,21,25.8,66,70,64,5.296667,5.31,4.865,122.25,158,83),nrow=3))
  colnames(food.matrix.sex1) <- colnames(food.matrix.sex2) <- c("Age","Skim","Bacon","ColdCereal","Fruit","Mayo","SaladDressing","Eggs","CitrusJuice","HotDogs","Cheese","FrenchFries","RegFat","Rice")

  coef.sex1 <- c(-0.009666,0.10569,-0.022086,-0.009346,0.145026,0.114649,0.026997,-0.004946,0.040118,0.069945,0.024262,-0.167937,0.017017) #male
  coef.sex2 <- c(-0.010393,0.198808,-0.045171,-0.012103,0.287044,0.182758,0.036787,-0.010141,0.106686,0.103239,0.040374,0.326702,-0.014224) #female

  x <- cbind(data$food01b.freq,data$food01d.freq,data$food01a.freq,data$food01g.freq,data$food01l.freq,data$food01m.freq,data$food01c.freq,data$food01f.freq,data$food01h.freq,data$food01i.freq,data$food01j.freq,data$reg.fat,data$food01n.freq)

  data$food.percent.ener.fat <- rep(0, nrow(data))

  for (i in 1:nrow(data)){
    if(is.na(data$sex[i]) | is.na(data$age.cat[i])){
      data$food.percent.ener.fat[i] <- NA
    } else if(data$sex[i]==1 & data$age.cat[i]==1){
      data$food.percent.ener.fat[i] <- 30.79765 + sum(x[i,]*coef.sex1*food.matrix.sex1[1,2:14])
    } else if(data$sex[i]==1 & data$age.cat[i]==2){
      data$food.percent.ener.fat[i] <- 30.79765 + sum(x[i,]*coef.sex1*food.matrix.sex1[2,2:14])
    } else if(data$sex[i]==1 & data$age.cat[i]==3){
      data$food.percent.ener.fat[i] <- 30.79765 + sum(x[i,]*coef.sex1*food.matrix.sex1[3,2:14])
    } else if(data$sex[i]==2 & data$age.cat[i]==1){
      data$food.percent.ener.fat[i] <- 29.86587 + sum(x[i,]*coef.sex2*food.matrix.sex2[1,2:14])
    } else if(data$sex[i]==2 & data$age.cat[i]==2){
      data$food.percent.ener.fat[i] <- 29.86587 + sum(x[i,]*coef.sex2*food.matrix.sex2[2,2:14])
    } else if(data$sex[i]==2 & data$age.cat[i]==3){
      data$food.percent.ener.fat[i] <- 29.86587 + sum(x[i,]*coef.sex2*food.matrix.sex2[3,2:14])
    } else {
      data$food.percent.ener.fat[i] <- NA
    }
  }

  label(data$food.percent.ener.fat) <- "Food-Percent Energy from Fat"

  return(data)
}
