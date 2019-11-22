# merged.df <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/DATA/MAP/mergedData/MAP_bh_d20190903_m20190903.rds")
#
# data.df <- merged.df[merged.df$epoch == 1, ]
# data.df <- droplevels(data.df[data.df$diagnosis.factor.base %nin% "Dementia", ])
# group <- c("diagnosis.factor.base", "apoe4pos.factor")
#
# varA <- c("np.moca", "np.bnt")
# varB <- c("np.moca", "np.bnt", "np.biber.discrim", "np.memory.composite")

calc_corr <- function(data, varA, varB, type) {
  if (varA == varB) {
    output <- data.frame(
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

    output <- data.frame(
      varA = varA,
      varB = varB,
      r = as.numeric(x[["r"]][1, 2]),
      p = as.numeric(x[["P"]][1, 2]),
      n = as.numeric(x[["n"]][1, 2]),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }

  return(output)
}

pairwise_correlation <- function(data = mydat, varA = predictors.var, varB = descriptive.var, group = "diagnosis.factor.base", type = "spearman", output = "DT") {
  varA <- setdiff(varA, group)
  varB <- setdiff(varB, group)
  varB <- unique(c(varA, varB))

  pairwise_vars.df <- expand.grid(varA = varA, varB = varB, stringsAsFactors = FALSE)
  pairwise_vars.df <- pairwise_vars.df[order(match(pairwise_vars.df$varA, varA)), ]

  data.df <- clear_labels(data[, c(varB, group)])

  data.list <- sapply(group, function (x) split(data.df, data.df[[x]]))

  output_names.str <- c("Overall", unname(unlist(data.list)))

  cor.list <- map(
    levels.list,
    function (sublist) {
      map(
        sublist,
        function (df) {
          map2_df(
            .x = pairwise_vars.df$varA,
            .y = pairwise_vars.df$varB,
            ~ calc_corr(data = df, varA = .x, varB = .y, type = "spearman")
          )
        }
      )
    }
  )

  cor.df <- bind_rows(map(cor.list, function(sublist) { bind_rows(sublist, .id = "level") }), .id = "group")

  return(cor.df)
}

format_correlation_table <- function(cor.list = cor.list) {
  # sketch = htmltools::withTags(table(
  #   class = 'display',
  #   thead(
  #     tr(
  #       th(rowspan = 2, 'Independent Variable'),
  #       th(rowspan = 2, 'Dependent Variable'),
  #       th(colspan = 3, 'Overall'),
  #       th(colspan = 3, 'Normal'),
  #       th(colspan = 3, 'Ambiguous At Risk'),
  #       th(colspan = 3, 'MCI')
  #     ),
  #     tr(
  #       lapply(rep(c('r', 'p', 'N'), 4), th)
  #     )
  #   )
  # ))

  tidyr::pivot_wider(cor.df[, names(cor.df) %nin% "group"], names_from = level, values_from = c(r, p, n))

}
