cor_label <- function(data, xvar, yvar, method = "spearman") {
  data <- na.omit(data[, c(xvar, yvar)])
  cor.list <- suppressWarnings(Hmisc::rcorr(data[[xvar]], data[[yvar]], type = method))
  r.val = cor.list$r[1, 2]
  p.val = cor.list$P[1, 2]
  label = paste0(
    "r=",
    format(round(r.val, 2), nsmall = 2),
    "; ",
    ifelse(
      p.val >= 0.001,
      paste0(
        'p=',
        format(round(p.val, 3), nsmall = 3)
      ),
      'p<0.001'
    )
  )

  return(label)
}

###

facet_baseline_scatter <- function(data, x_var, y_var, ...) {
  y_var <- setdiff(y_var, x_var)

  labels.vec <- paste0("Baseline ", y_var)
  names(labels.vec) = y_var
  y_labels <- as_labeller(labels.vec)

  label.df <- data.frame(
    label = purrr::map_chr(y_var, ~ cor_label(data = data, xvar = x_var, yvar = ., ...)),
    variable = y_var
  )

  p0 <- data %>%
    clear_labels() %>%
    select(
      one_of(
        x_var, c(y_var)
      )
    ) %>%
    tidyr::gather(key = variable, value = value, -.data[[x_var]]) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = .data[[x_var]], y = value)) +
    geom_point() +
    geom_smooth(method = lm) +
    facet_wrap(. ~ variable, ncol = 3, scales = "free", labeller = y_labels) +
    xlab(paste0("Baseline ", x_var))


  p <- p0 +
    geom_label(
      data    = label.df,
      mapping = aes(x = Inf, y = Inf, label = label),
      hjust   = 1.05,
      vjust   = 1.5
    )

  return(p)
}

###

facet_ac_scatter <- function(data, baseline_var, ac_var, ...) {
  labels.vec <- paste0("Annual Change in ", ac_var)
  names(labels.vec) = ac_var

  ac_labels <- as_labeller(labels.vec)

  label.df <- data.frame(
    label = purrr::map_chr(ac_var, ~ cor_label(data = data, xvar = baseline_var, yvar = ., ...)),
    variable = ac_var
  )

  p0 <- data %>%
    clear_labels() %>%
    select(
      one_of(
        baseline_var, c(ac_var)
      )
    ) %>%
    tidyr::gather(key = variable, value = value, -.data[[baseline_var]]) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = .data[[baseline_var]], y = value)) +
    geom_point() +
    geom_smooth(method = lm) +
    facet_wrap(. ~ variable, ncol = 3, scales = "free", labeller = ac_labels) +
    xlab(paste0("Baseline ", baseline_var))

  p <- p0 +
    geom_label(
      data    = label.df,
      mapping = aes(x = Inf, y = Inf, label = label),
      hjust   = 1.05,
      vjust   = 1.5
    )

  return(p)
}
