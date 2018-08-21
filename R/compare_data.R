#' Function to compare two datasets with daff
#'
#' @param data1 A data frame.
#' @param data2 A second data frame.
#' @return HTML formatted output summarizing differences between `data1` and `data2`.`

compare_data <- function(data1, data2) {
  data1.name <- deparse(substitute(data1))
  data2.name <- deparse(substitute(data2))
  title <- paste0("Comparing datasets: ", data1.name, " and ", data2.name, ".")

  data.diff <- daff::diff_data(data1, data2)
  daff::render_diff(data.diff, fragment = TRUE, pretty = TRUE, title = title)
}
