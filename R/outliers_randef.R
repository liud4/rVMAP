outliers_randef <- function(data, sd, outcome.var, predictor.var, time.var) {
  # inititalize tibble if running function for first time
  if (!exists("outliers_randef.df")) {
    outliers_randef.df <- tibble(
      label = character(),
      variable = character(),
      map.id = character(),
      diagnosis.factor.base = character(),
      outlier_of = character(),
      intercept = numeric(),
      sd_from_intercept = numeric(),
      slope = numeric(),
      sd_from_slope = numeric()
    )
  }

  form <- as.formula(paste0(outcome.var, " ~ ", predictor.var, "*", time.var))

  ctrl <- lmeControl(opt='optim')

  rand <- paste0("~1 + ", time.var, "|map.id")

  corr <- paste0("~", time.var, "|map.id")

  fit <- lme(form,
             data = mydat.long,
             na.action = na.omit,
             #random = ~1 + np.date.diff.yr|map.id,
             #correlation = corCompSymm(form = ~time.var|map.id),
             random = as.formula(rand),
             correlation = corCompSymm(form = as.formula(corr)),
             control = ctrl)

  rand_ef.df <- tibble::rownames_to_column(ranef(fit), var = "map.id")

  mean_intercept <- mean(rand_ef.df[, 2], na.rm = TRUE)
  mean_slope <- mean(rand_ef.df[, 3], na.rm = TRUE)

  sd_intercept <- sd(rand_ef.df[, 2], na.rm = TRUE)
  sd_slope <- sd(rand_ef.df[, 3], na.rm = TRUE)

  map.id_intercept <- rand_ef.df[abs(rand_ef.df[, 2]) > sd*sd_intercept, "map.id"]
  map.id_slope <- rand_ef.df[abs(rand_ef.df[, 3]) > sd*sd_slope, "map.id"]
  map.id_all <- sort(unique(c(map.id_intercept, map.id_slope)))

  if (length(map.id_all) > 0) {
    new_rows <- tibble(
      variable = outcome.var,
      map.id = map.id_all,
      intercept = rand_ef.df[rand_ef.df$map.id %in% map.id_all, 2],
      slope = rand_ef.df[rand_ef.df$map.id %in% map.id_all, 3]
    )

    new_rows <- new_rows %>%
      mutate(
        label = as.character(label(mydat[, variable])),
        diagnosis.factor.base = as.character(mydat[mydat$map.id %in% map.id_all, 'diagnosis.factor.base']),
        sd_from_intercept = (intercept - mean_intercept) / sd_intercept,
        sd_from_slope = (slope - mean_slope) / sd_slope
      ) %>%
      mutate(
        outlier_of = dplyr::case_when(
          ((abs(sd_from_intercept) > sd) & (abs(sd_from_slope) > sd)) ~ "intercept & slope",
          (abs(sd_from_intercept) > sd) ~ "intercept",
          (abs(sd_from_slope) > sd) ~ "slope",
          TRUE ~ NA_character_
        )
      ) %>%
      select(
        label, variable, map.id, diagnosis.factor.base, outlier_of, intercept, sd_from_intercept, slope, sd_from_slope
      )

    outliers_randef.df <<- bind_rows(
      outliers_randef.df,
      new_rows
    )
  }
}
