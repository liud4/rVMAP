#' Compare additions, modifications, and deletions between two data frames with \code{daff::diff_data}.
#'
#' @param data1 A data frame.
#' @param data2 A second data frame.
#' @param file A path for where the output is saved.
#' @return HTML formatted output summarizing differences between \code{data1} and \code{data2}.
#' @family comparison functions

compare_data <- function(data1, data2, file = paste0(data.merge.bh.file, "_data_comparison.html")) {
  data1.name <- deparse(substitute(data1))
  data2.name <- deparse(substitute(data2))
  title <- paste0("Comparing datasets: ", data1.name, " and ", data2.name, ".")

  data.diff <- daff::diff_data(
    data1[, grep("notes|visit\\.flow", names(data1), invert = TRUE, value = T)],
    data2[, grep("notes|visit\\.flow", names(data2), invert = TRUE, value = T)]
  )

  daff::render_diff(
    data.diff,
    file = file,
    view = FALSE,
    fragment = FALSE,
    pretty = TRUE,
    title = title,
    summary = TRUE,
    use.DataTables = TRUE
  )
}
