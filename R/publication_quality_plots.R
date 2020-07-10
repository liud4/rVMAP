color_scale_scatter <- c("#000000", "#CC3311", "#004488", "#BBBBBB")
color_scale_fitted <- color_scale_scatter
color_scale_boxplot <- c("#FFFFFF", "#D76351", "#4570A1", "#C9C9C9")

main_scatter <- function(data = mydat, x, y, xlab = x, ylab = y, label = NULL, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {

  filename.str <- paste0("scatter_", "main", "_", x, "_", y)

  plot.df <- droplevels(na.omit(data[, c(x, y)]))

  p <- ggplot(data = plot.df, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = lm, color = "black") +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  if (!is.null(label)) {
    p <- p +
      geom_label(
        mapping = aes(x = Inf, y = Inf, label = label),
        hjust   = 1.0,
        vjust   = 1.0
      )
  }

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }
}

inter_scatter <- function(data = mydat, x, y, group, model = group, xlab = x, ylab = y, grouplab = group, label = NULL, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {

  data_name.str <- deparse(substitute(data))

  if (!is.factor(data[[group]])) {
    warning(paste0(data_name.str, "$", group, " must be a factor variable. It will be coerced to one for the puproses of this plot."))
    data[[group]] <- factor(data[[group]])
  }

  filename.str <- paste0("scatter_", model, "_", x, "_", y)

  if (group == "diagnosis.factor.base") {
    plot.df <- droplevels(na.omit(data[data[["diagnosis.factor.base"]] %in% c("Normal", "MCI"), c(x, y, group)]))
  } else {
    plot.df <- droplevels(na.omit(data[, c(x, y, group)]))
  }

  p <- ggplot(data = plot.df, aes_string(x = x, y = y)) +
    geom_point(aes_string(color = group)) +
    geom_smooth(method = lm, aes_string(color = group)) +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    labs(color = grouplab) +
    scale_color_manual(values = color_scale_scatter) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  if (!is.null(label)) {
    p <- p +
      geom_label(
        mapping = aes(x = Inf, y = Inf, label = label),
        hjust   = 1.0,
        vjust   = 1.0
      )
  }

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }
}

main_box <- function(data = mydat, x, y, xlab = x, ylab = y, label = NULL, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {
  filename.str <- paste0("boxplot_", "main", "_", x, "_", y)

  plot.df <- droplevels(na.omit(data[, c(x, y)]))

  p <- ggplot(data = plot.df, aes_string(x = x, y = y)) +
    geom_boxplot(color = "black") +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  if (!is.null(label)) {
    p <- p +
      geom_label(
        mapping = aes(x = Inf, y = Inf, label = label),
        hjust   = 1.0,
        vjust   = 1.0
      )
  }

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }
}

inter_box <- function(data = mydat, x, y, group, model = group, xlab = x, ylab = y, grouplab = group, label = NULL, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {

  data_name.str <- deparse(substitute(data))

  if (!is.factor(data[[x]])) {
    warning(paste0(data_name.str, "$", x, " must be a factor variable. It will be coerced to one for the puproses of this plot."))
    data[[x]] <- factor(data[[x]])
  }

  if (!is.factor(data[[group]])) {
    warning(paste0(data_name.str, "$", group, " must be a factor variable. It will be coerced to one for the puproses of this plot."))
    data[[group]] <- factor(data[[group]])
  }

  filename.str <- paste0("boxplot_", model, "_", x, "_", y)

  plot.df <- droplevels(na.omit(data[, c(x, y, group)]))

  p <- ggplot(data = plot.df, aes_string(x = x, y = y)) +
    geom_boxplot(color = "black", aes_string(fill = group)) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    labs(fill = grouplab) +
    scale_fill_manual(values = color_scale_boxplot) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  if (!is.null(label)) {
    p <- p +
      geom_label(
        mapping = aes(x = Inf, y = Inf, label = label),
        hjust   = 1.0,
        vjust   = 1.0
      )
  }

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }
}

main_fitted <- function(regression_list = cs.regression.list, model, x, y, xlab = x, ylab = y, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {

  filename.str <- paste0("fitted_", "main", "_", x, "_", y)

  fit_obj <- regression_list[[x]][["main"]][[y]]

  options(datadist = NULL)

  dd <<- fit_obj$dd

  options(datadist = "dd")

  temp.list <- list(fit_obj$fit, x)
  prediction.df <- data.frame(do.call(rms::Predict, temp.list))

  p <- ggplot(data = prediction.df, aes_string(x = x, y = "yhat")) +
    geom_line(size = 0.9, color = 'black') +
    geom_ribbon(aes_string(ymin = 'lower', ymax = 'upper'), alpha = 0.2, color = 'black') +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  options(datadist = NULL)

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }

}

inter_fitted <- function(regression_list = cs.regression.list, model, x, y, group, xlab = x, ylab = y, grouplab = group, return = TRUE, savePNG = FALSE, saveTIFF = FALSE) {

  filename.str <- paste0("fitted_", model, "_", x, "_", y)

  fit_obj <- regression_list[[x]][[model]][[y]]

  options(datadist = NULL)

  dd <<- fit_obj$dd

  options(datadist = "dd")

  temp.list <- list(fit_obj$fit, x, group)
  prediction.df <- data.frame(do.call(rms::Predict, temp.list))

  p <- ggplot(data = prediction.df, aes_string(x = x, y = "yhat")) +
    geom_line(aes_string(color = group), size = 0.9) +
    geom_ribbon(aes_string(ymin = 'lower', ymax = 'upper', fill = group), alpha = 0.2) +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    xlab(xlab) +
    ylab(ylab) +
    scale_color_manual(name = grouplab, values = color_scale_fitted) +
    scale_fill_manual(name = grouplab, values = color_scale_fitted) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'white'),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    theme(aspect.ratio = 1)

  options(datadist = NULL)

  if (isTRUE(savePNG)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".png")), plot = p)
  }

  if (isTRUE(saveTIFF)) {
    ggsave(filename = file.path(proj.graphics.dir, todays_date(), paste0(filename.str, ".tiff")), plot = p)
  }

  if (isTRUE(return)) {
    return(p)
  }

}

