incremental_R2_diff <- function(data, inds, predictor, model, outcomeA, outcomeB, covariatesA, covariatesB) {

  if (grepl('main', model)) {
    data.df <- data
    predictor_phrase.str <- predictor
  } else if (grepl('inter.dx', model)) {
    covariatesA <- unique(c(covariatesA, "diagnosis.factor.base"))
    covariatesB <- unique(c(covariatesB, "diagnosis.factor.base"))
    data.df <- subset(data, diagnosis.factor.base %in% c("Normal", "MCI"))
    predictor_phrase.str <- paste0(predictor, "*diagnosis.factor.base")
  } else if (grepl("sub.dx.nc", model)) {
    covariatesA <- setdiff(covariatesA, "diagnosis.factor.base")
    covariatesB <- setdiff(covariatesB, "diagnosis.factor.base")
    data.df <- subset(data, diagnosis.factor.base == "Normal")
    predictor_phrase.str <- predictor
  } else if (grepl("sub.dx.mci", model)) {
    covariatesA <- setdiff(covariatesA, "diagnosis.factor.base")
    covariatesB <- setdiff(covariatesB, "diagnosis.factor.base")
    data.df <- subset(data, diagnosis.factor.base == "MCI")
    predictor_phrase.str <- predictor
  } else if (grepl("inter.apoe", model)) {
    covariatesA <- unique(c(covariatesA, "apoe4pos.factor"))
    covariatesB <- unique(c(covariatesB, "apoe4pos.factor"))
    data.df <- data
    predictor_phrase.str <- paste0(predictor, "*apoe4pos.factor")
  } else if (grepl("sub.apoe.no", model)) {
    covariatesA <- setdiff(covariatesA, "apoe4pos.factor")
    covariatesB <- setdiff(covariatesB, "apoe4pos.factor")
    data.df <- subset(data, apoe4pos.factor == "No")
    predictor_phrase.str <- predictor
  } else if (grepl("sub.apoe.yes", model)) {
    covariatesA <- setdiff(covariatesA, "apoe4pos.factor")
    covariatesB <- setdiff(covariatesB, "apoe4pos.factor")
    data.df <- subset(data, apoe4pos.factor == "Yes")
    predictor_phrase.str <- predictor
  } else if (grepl("inter.sex", model)) {
    covariatesA <- unique(c(covariatesA, "sex.factor"))
    covariatesB <- unique(c(covariatesB, "sex.factor"))
    data.df <- data
    predictor_phrase.str <- paste0(predictor, "*sex.factor")
  } else if (grepl("sub.sex.male", model)) {
    covariatesA <- setdiff(covariatesA, "sex.factor")
    covariatesB <- setdiff(covariatesB, "sex.factor")
    data.df <- subset(data, sex.factor == "Male")
    predictor_phrase.str <- predictor
  } else if (grepl("sub.sex.female", model)) {
    covariatesA <- setdiff(covariatesA, "sex.factor")
    covariatesB <- setdiff(covariatesB, "sex.factor")
    data.df <- subset(data, sex.factor == "Female")
    predictor_phrase.str <- predictor
  } else if (grepl('inter.age', model)) {
    covariatesA <- unique(c(covariatesA, "age"))
    covariatesB <- unique(c(covariatesB, "age"))
    data.df <- data
    predictor_phrase.str <- paste0(predictor, "*age")
  } else if (grepl("sub.age.low", model)) {
    covariatesA <- setdiff(covariatesA, "age")
    covariatesB <- setdiff(covariatesB, "age")
    data.df <- subset(data, age.median.factor == levels(data$age.median.factor)[1])
    predictor_phrase.str <- predictor
  } else if (grepl("sub.age.high", model)) {
    covariatesA <- setdiff(covariatesA, "age")
    covariatesB <- setdiff(covariatesB, "age")
    data.df <- subset(data, age.median.factor == levels(data$age.median.factor)[2])
    predictor_phrase.str <- predictor
  } else {
    stop("Please specify a valid model name.\n")
  }

  # Subset data

  subset.df <- clear_labels(data.df)
  subset.df <- na.omit(subset.df[, unique(c(outcomeA, outcomeB, predictor, covariatesA, covariatesB))])
  subset.df <- droplevels(subset.df)

  # Define regression models

  A.formula <- as.formula(
    paste0(outcomeA, ' ~ ', paste0(c(covariatesA, predictor_phrase.str), collapse = ' + '))
  )
  B.formula <- as.formula(
    paste0(outcomeB, ' ~ ', paste0(c(covariatesB, predictor_phrase.str), collapse = ' + '))
  )

  if (grepl("inter", model)) {
    A_covar.formula <- as.formula(
      paste0(outcomeA, ' ~ ', paste0(c(covariatesA, predictor), collapse = ' + '))
    )
    B_covar.formula <- as.formula(
      paste0(outcomeB, ' ~ ', paste0(c(covariatesB, predictor), collapse = ' + '))
    )
  } else {
    A_covar.formula <- as.formula(
      paste0(outcomeA, ' ~ ', paste0(c(covariatesA), collapse = ' + '))
    )
    B_covar.formula <- as.formula(
      paste0(outcomeB, ' ~ ', paste0(c(covariatesB), collapse = ' + '))
    )
  }

  ###

  tryCatch({
    A.fit <- ols(A.formula, data = subset.df)
    B.fit <- ols(B.formula, data = subset.df)
    A_covar.fit <- ols(A_covar.formula, data = subset.df)
    B_covar.fit <- ols(B_covar.formula, data = subset.df)

    N <- as.integer(nrow(subset.df))
    A.R2 <- as.numeric(A.fit$stats["R2"])
    B.R2 <- as.numeric(B.fit$stats["R2"])
    A_covar.R2 <- as.numeric(A_covar.fit$stats["R2"])
    B_covar.R2 <- as.numeric(B_covar.fit$stats["R2"])
    A.incr.R2 <- A.R2 - A_covar.R2
    B.incr.R2 <- B.R2 - B_covar.R2
    A.B.incr.R2.diff <- A.incr.R2 - B.incr.R2

    return(
      tibble(
        model = as.character(model),
        predictor = as.character(predictor),
        outcomeA = as.character(outcomeA),
        outcomeB = as.character(outcomeB),
        N = N,
        A.R2 = A.R2,
        B.R2 = B.R2,
        A_covar.R2 = A_covar.R2,
        B_covar.R2 = B_covar.R2,
        A.incr.R2 = A.incr.R2,
        B.incr.R2 = B.incr.R2,
        A.B.incr.R2.diff = A.B.incr.R2.diff
      )
    )
  },

  error = function(err) {
    return(
      tibble(
        model = as.character(model),
        predictor = as.character(predictor),
        outcomeA = as.character(outcomeA),
        outcomeB = as.character(outcomeB),
        N = NA_integer_,
        A.R2 = NA_real_,
        B.R2 = NA_real_,
        A_covar.R2 = NA_real_,
        B_covar.R2 = NA_real_,
        A.incr.R2 = NA_real_,
        B.incr.R2 = NA_real_,
        A.B.incr.R2.diff = NA_real_,
      )
    )
  }
  )
}

incremental_R2_diff_boot <- function(data, replicates = 250, predictor, model, outcomeA, outcomeB, covariatesA, covariatesB) {
  incremental_R2_diff.out <- incremental_R2_diff(
    predictor = predictor,
    model = model,
    outcomeA = outcomeA,
    outcomeB = outcomeB,
    covariatesA = covariatesA,
    covariatesB = covariatesB
  )

  boot.out <- boot::boot(
    data = data,
    statistic = incremental_diff_R2,
    R = replicates,
    predictor = predictor,
    model = model,
    outcomeA = outcomeA,
    outcomeB = outcomeB,
    covariatesA = covariatesA,
    covariatesB = covariatesB
  )

  A.boot.ci <- boot::boot.ci(
    boot.out,
    type = "perc",
    t0 = as.numeric(boot.out$t0["A.incr.R2"]),
    t = as.numeric(boot.out$t[, 6]) # need to check that this is correct column
  )
  B.boot.ci <- boot::boot.ci(
    boot.out,
    type = "perc",
    t0 = as.numeric(boot.out$t0["B.incr.R2"]),
    t = as.numeric(boot.out$t[, 7]) # need to check that this is correct column
  )
  A.B.boot.ci <- boot::boot.ci(
    boot.out,
    type = "perc",
    t0 = as.numeric(boot.out$t0["A.B.incr.R2.diff"]),
    t = as.numeric(boot.out$t[, 8]) # need to check that this is correct column
  )

  output.df <- tibble(
    model = model,
    predictor = predictor,
    outcomeA = outcomeA,
    outcomeB = outcomeB,
    N = incremental_R2_diff.out$N,
    replicates = replicates,
    A.incr.R2 = boot.out$t0["A.incr.R2"],
    A.incr.R2.LCI = A.boot.ci$percent[4],
    A.incr.R2.UCI = A.boot.ci$percent[5],
    B.incr.R2 = boot.out$t0["B.incr.R2"],
    B.incr.R2.LCI = B.boot.ci$percent[4],
    B.incr.R2.UCI = B.boot.ci$percent[5],
    A.B.incr.R2.diff = boot.out$t0["A.B.incr.R2.diff"],
    A.B.incr.R2.diff.LCI = A.B.boot.ci$percent[4],
    A.B.incr.R2.diff.UCI = A.B.boot.ci$percent[5]
  )

  return(output.df)
}

