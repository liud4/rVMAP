#' Function to show comparison of two data frames with daff.
#'
#' @param data1 A data frame.
#' @param data2 A second data frame.
#' @param fragment A logical value indicating whether to return an HTML fragment for inclusion into the current report or a separate, temporary HTML file.
#' @return HTML formatted output summarizing differences between \code{data1} and \code{data2}.

compare_data <- function(data1, data2, fragment = TRUE) {
  data1.name <- deparse(substitute(data1))
  data2.name <- deparse(substitute(data2))
  title <- paste0("Comparing datasets: ", data1.name, " and ", data2.name, ".")

  data.diff <- daff::diff_data(data1, data2)
  daff::render_diff(data.diff, fragment = fragment, pretty = TRUE, title = title)
}
