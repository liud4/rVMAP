#' Convert date variables to "Date" class.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with date variables converted to "Date" class.
#' @export

convert_dates <- function(data) {
  current_data.df <- data

  date.var <- setdiff(names(current_data.df)[grepl("(\\_|\\.)date$", names(current_data.df))], c("usa.date", "usa_date"))

  if ("dob" %in% names(current_data.df)) {
    date.var <- c(date.var, "dob")
  }

  if (length(date.var) > 0) {
    for (var.i in date.var) {
      current_date.var <- current_data.df[, var.i]

      if (is.factor(current_date.var)) {
        current_date.var <- as.character(current_date.var)
      }

      if (!is.character(current_date.var) & !is.Date(current_date.var) & !all(is.na(current_date.var))) {
        warning(paste0(var.i, " is not formatted properly and will not be converted to 'Date' class. This variable should be manually checked.\n\n"))
        next
      }

      if (!is.Date(current_date.var) & any(nchar(na.omit(current_date.var)) %nin% c(0, 10))) {
        warning(paste0(var.i, " has non-date values and will not be converted to 'Date' class. This variable should be manually checked.\n\n"))
        next
      }

      current_date.var[nchar(current_date.var) == 0] <- NA
      current_date.var[grep("1111", current_date.var)] <- NA

      current_data.df[, var.i] <- lubridate::as_date(current_date.var)
    }
  }

  return(current_data.df)
}
