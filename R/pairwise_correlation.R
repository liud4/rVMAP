calc_corr <- function(data, varA, varB, type) {
  if (varA == varB) {
    output.df <- data.frame(
      varA = varA,
      varB = varB,
      r = NA_real_,
      p = NA_real_,
      n = NA_integer_,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  } else {
    temp.df <- data[, c(varA, varB)]
    temp.df <- sapply(temp.df, as.numeric)
    temp.df <- na.omit(temp.df)

    x <- Hmisc::rcorr(temp.df, type = type)

    output.df <- data.frame(
      varA = varA,
      varB = varB,
      r = as.numeric(x[["r"]][1, 2]),
      p = as.numeric(x[["P"]][1, 2]),
      n = as.numeric(x[["n"]][1, 2]),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }

  return(output.df)
}

pairwise_correlation <- function(data = mydat, varA = predictors.var, varB = descriptive.var, group = "diagnosis.factor.base", type = "spearman", df = FALSE) {
  varA <- setdiff(varA, group)
  varB <- setdiff(varB, group)
  varB <- unique(c(varA, varB))

  data.df <- clear_labels(data[, c(varB, group)])

  pairwise_vars.df <- expand.grid(varA = varA, varB = varB, stringsAsFactors = FALSE)
  pairwise_vars.df <- pairwise_vars.df[order(match(pairwise_vars.df$varA, varA)), ]

  data.list <- lapply(group, function (x) split(data.df, data.df[[x]]))

  names(data.list) <- group

  overall.list <- list(
    Overall = list(
      Overall = purrr::map2_df(
        .x = pairwise_vars.df$varA,
        .y = pairwise_vars.df$varB,
        ~ calc_corr(data = data.df, varA = .x, varB = .y, type = type)
      )
    )
  )

  cor.list <- purrr::map(
    data.list,
    function (sublist) {
      purrr::map(
        sublist,
        function (subset) {
          purrr::map2_df(
            .x = pairwise_vars.df$varA,
            .y = pairwise_vars.df$varB,
            ~ calc_corr(data = subset, varA = .x, varB = .y, type = type)
          )
        }
      )
    }
  )

  cor.list <- c(
    overall.list,
    cor.list
  )

  cor.df <- bind_rows(
    purrr::map(
      cor.list,
      function(sublist) {
        bind_rows(sublist, .id = "level")
      }
    ),
    .id = "group"
  ) %>%
    mutate(
      label = paste0(group, "_", level)
    )

  names.df <- expand.grid(c("r_", "p_", "n_"), unique(cor.df[["label"]]), stringsAsFactors = FALSE) %>%
    mutate(
      colname = paste0(Var1, Var2)
    )

  output.df <- tidyr::pivot_wider(
    cor.df[, names(cor.df) %nin% c("group", "level")],
    names_from = label,
    values_from = c(r, p, n)
  )

  if (isTRUE(df)) {
    output.df <- as.data.frame(output.df)
  }

  return(
    output.df[, c("varA", "varB", names.df$colname)]
  )

}
