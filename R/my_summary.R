my_summary <- function(descr.var, group.var, data, overall = TRUE, test = TRUE, ...) {
  # modified my.summaryM puts "Combined" first
  descr.var = setdiff(descr.var, group.var)
  descr.formula <- as.formula(paste0(paste(descr.var, collapse= " + "),  "~", group.var))

  summary = my.summaryM(
    descr.formula,
    data = data,
    test = test
    overall = overall,
    continuous = 6
  )

  Hmisc::html(summary, exclude1 = FALSE, long = TRUE, prmsd = TRUE, brmsd = TRUE, digits = 2)
}
