compare_data <- function(data1, data2) {
  data1.name <- deparse(substitute(data1))
  data2.name <- deparse(substitute(data2))
  title <- paste0("Comparing datasets: " data1.name, " and ", data2.name, ".")

  data.diff <- daff::diff_data(y, x)
  daff::render_diff(data.diff, fragment = TRUE, pretty = TRUE, title = title)
}
