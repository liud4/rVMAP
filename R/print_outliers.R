print_outliers <- function(outliers.list) {
  outliers.list %>%
    as.data.frame() %>%
    clear_labels() %>%
    mutate(variable = colnames(.)[3]) %>%
    rename(value = colnames(.)[3]) %>%
    select(variable, map.id, enrolled.dx.factor, value, mean, sd, outlier.in.sd)
}
