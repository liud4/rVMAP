#' Derive, convert, label, and add food scan variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added food scan variables.
#' @export

derive_food_scan <- function (data)
{
  foodvar <- paste0("food01", formatC(
    c(letters[1:15]),
    width = 1,
    format = "d",
    flag = "0"
  ))
  
  foodvar.freq <- paste0("food01", formatC(
    c(letters[1:15]),
    width = 1,
    format = "d",
    flag = "0"
  ), ".freq")
  
  propNonMissing <- function(vec) {
    round(mean(!is.na(vec)), 2)
  }
  
  avgscore <- function(vec, threshold = 0.85) {
    if (propNonMissing(vec) < threshold) {
      return(NA)
    }
    else {
      return(round(mean(vec, na.rm = TRUE), 1))
    }
  }
  
  pro.rate <- round(apply(data[, foodvar], MARGIN = 1, FUN = avgscore))
  
  for (j in 1:length(foodvar)) {
    for (i in 1:nrow(data)) {
      data[i, foodvar[j]] <- ifelse(is.na(data[i, foodvar[j]]), pro.rate, data[i, foodvar[j]])
    }
  }
  
  for (i in 1:length(foodvar)) {
    data[, foodvar.freq[i]] = ifelse(data[, foodvar[i]] ==
                                       0, 0, ifelse(data[, foodvar[i]] == 1, 0.018, 
                                                    ifelse(data[, foodvar[i]] == 2, 0.066, ifelse(data[, foodvar[i]] ==
                                                                                                    3, 0.214, ifelse(data[, foodvar[i]] == 4, 0.499, 
                                                                                                                     ifelse(data[, foodvar[i]] == 5, 0.784, ifelse(data[, foodvar[i]] == 6, 1, 2))
                                                                                                                     )
                                                                                                  )
                                                           )
                                                    )
                                     )
  }
  
  data$totfat.freq <- data$food01e.freq + data$food01k.freq + data$food01o.freq
  
  data$regfat.prop <- dplyr::case_when(
    is.na(data$food02) ~ NA_real_,
    data$food02 == 0 | data$food02 == 1 ~ 1,
    data$food02 == 2 ~ 0.75,
    data$food02 == 3 ~ 0.5,
    data$food02 == 4 ~ 0.25,
    TRUE ~ 0
  )
  
  data$regfat.freq <- data$regfat.prop * data$totfat.freq
  
  age.cat <- factor(if_else(data$age > 47 & data$age <= 57, 1,
    if_else(data$age > 57 & data$age <= 67, 2,
      if_else(data$age > 67 & data$age <= 77, 3, 4, missing = NULL)
    )
  ))
  
  ## Step 1: Age-adjusted rates
  ### male 
  food.matrix.sex1 <- rbind(
    c(
      1,
      245,
      32,
      56,
      127.5,
      13.75,
      31.25,
      92,
      249,
      114,
      28.35,
      100,
      9.2,
      165
    ),
    c(
      2,
      214.38,
      27,
      46,
      122,
      9.15,
      29.4,
      92,
      248,
      57,
      28.35,
      85.5,
      7.88,
      165
    ),
    c(
      3,
      198.94,
      26,
      39,
      118,
      13.75,
      29.4,
      80,
      186.75,
      57,
      24,
      85.5,
      7.1,
      158
    ),
    c(
      4,
      160.73,
      24,
      33,
      114.25,
      4.58,
      29.38,
      80,
      186.75,
      57,
      22.88,
      97,
      7,
      158
    )
  ) %>% as_tibble()
  
  ## female
  food.matrix.sex2 <- rbind(
    c(
      1,
      229.69,
      24,
      43.5,
      118,
      9.18,
      29.4,
      80,
      217.88,
      114,
      22.1,
      70,
      7.10,
      155
    ),
    c(2, 196, 18, 33, 118, 6.11, 29.38, 68, 186.75, 57, 24, 66, 5.30, 122),
    c(
      3,
      183.75,
      19.5,
      33,
      112.43,
      10.31,
      29.38,
      56,
      186.6,
      57,
      21,
      70,
      5.32,
      158
    ),
    c(
      4,
      183.75,
      16,
      33.5,
      109,
      4.58,
      22.03,
      46,
      186.75,
      57,
      25.8,
      64,
      4.87,
      83
    )
  ) %>% as_tibble()
  
  colnames(food.matrix.sex1) <- colnames(food.matrix.sex2) <- Cs(
    Age,
    `Skim Milk`,
    Bacon,
    `Cold Cereal`,
    Fruit,
    Mayo,
    Dressing,
    Eggs,
    `Fruit Juice`,
    `Hot Dogs`,
    Cheese,
    `French Fries`,
    Margarine,
    Rice
  )
  
  ## Step 2: Estimated regression coefficients for energy percentage from fat on food
  ## male
  intercept1 <- 30.795765
  
  coef.sex1 <- c(
    -0.009666,
    0.109569,
    -0.022086,
    -0.009346,
    0.145026,
    0.114649,
    0.026997,
    -0.004946,
    0.040118,
    0.069945,
    0.024262,
    0.167937,-0.017017
  )
  
  ## female
  intercept2 <- 29.865870
  
  coef.sex2 <- c(
    -0.010393,
    0.198808,
    -0.045171,
    -0.012103,
    0.287044,
    0.182758,
    0.036787,
    -0.010141,
    0.106686,
    0.103239,
    0.040374,
    0.326702,-0.014224
  )
  
  x <- cbind(
    data$food01b.freq,
    data$food01d.freq,
    data$food01a.freq,
    data$food01g.freq,
    data$food01l.freq,
    data$food01m.freq,
    data$food01c.freq,
    data$food01f.freq,
    data$food01h.freq,
    data$food01i.freq,
    data$food01j.freq,
    data$regfat.freq,
    data$food01n.freq
  )
  
  data$food.percent.ener.fat <- rep(0, nrow(data))
  
  for (i in 1:nrow(data)) {
    # i=1
    if (is.na(data$sex[i]) | is.na(age.cat[i])) {
      data$food.percent.ener.fat[i] <- NA
    } else if (data$sex[i] == 1) {
      data$food.percent.ener.fat[i] <- intercept1 +  sum(x[i, ] * coef.sex1 *
                                                           food.matrix.sex1[as.numeric(age.cat[i]), 2:14])
    } else if (data$sex[i] == 2) {
      data$food.percent.ener.fat[i] <- intercept2 + sum(x[i, ] * coef.sex2 * food.matrix.sex2[as.numeric(age.cat[i]), 2:14])
    } else {
      data$food.percent.ener.fat[i] <- NA
    }
  }
  
  label(data$food.percent.ener.fat) <- "Food-Percent Energy from Fat"
  
  return(data)
}
