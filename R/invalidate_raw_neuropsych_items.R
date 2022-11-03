#' Invalidate certain neuropsychological variables for certain participants in the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with invalidated neuropsychological variables.
#' @export

invalidate_raw_neuropsych_items <- function(data, invalidation_data) {
  invalidation_data <- invalidation_data %>%
    format_id()

  for (row.i in nrow(invalidation_data)) {
    np_invalidate.df <- invalidation_data[row.i, ]
    data[data$map_id %in% np_invalidate.df$map_id & data$epoch %in% np_invalidate.df$epoch, np_invalidate.df$test] <- NA
  }

  return(data)
}
