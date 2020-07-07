roc_forestplot <- function(roc_data, response_levels, predictor_levels, ncol = 1) {
  roc_data$Response <- factor(roc_data$Response, levels = response_levels)
  roc_data$Predictor <- factor(roc_data$Predictor, levels = rev(predictor_levels))

  p <- ggplot(
    data = roc_data,
    aes(x = Predictor, y = AUC, ymin = LCI, ymax = UCI)
  ) +
    geom_errorbar(aes(ymin = LCI, ymax = UCI, col = Predictor)) +
    geom_pointrange(size = 0.25) +
    ylim(0, 1) +
    xlab('') +
    ylab('') +
    ggtitle("Area Under Curve", subtitle = "(95% Bootstrapped Confidence Interval)") +
    facet_wrap(~ Response, ncol = ncol, scales = "free_y") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      strip.text.y = element_text(hjust = 0, vjust = 1, angle = 180, face = "bold")
    ) +
    coord_flip() +
    guides(color = guide_legend(reverse = TRUE))

  return(p)
}
