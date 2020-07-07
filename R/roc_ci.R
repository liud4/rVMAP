roc_ci <- function(data, cat_var, cont_var) {
  temp.df = droplevels(data[, c(cat_var, cont_var)])
  colnames(temp.df) <- c("cat", "cont")

  roc.ci <- suppressMessages(as.numeric(pROC::ci.auc(pROC::roc(data = temp.df, response = cat, predictor = cont))))

  output <- tibble(
    Response = cat_var,
    Predicctor = cont_var,
    LCI = roc.ci[1],
    AUC = roc.ci[2],
    UCI = roc.ci[3]
  ) %>%
    as.data.frame()

  return(output)
}
