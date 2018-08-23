#' Convert date variables to "Date" class.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with date variables converted to "Date" class.
#' @export

convert_dates <- function(data) {
  datevars <- setdiff(names(data)[grepl("\\.date", names(data))], c("abp.date", "usa.date"))

  if ("dob" %in% names(data)) {
    datevars <- c(datevars, "dob")
  }

  for (vname in datevars) {
    datevar <- data[, vname]

    if (is.factor(datevar)) {
      datevar <- as.character(datevar)
    }

    if (!is.character(datevar)) {
      warning(paste0(vname, " is not in character format and may not need to be converted.\n"))
      next
    }

    datevar[datevar %in% c("1111-11-11", "")] <- NA

    if (any((!is.na(datevar)) & nchar(datevar) != 10)) {
      warning(paste0(vname, " is not in yyyy-mm-dd format.\n"))
      next
    }

    data[, vname] <- as.Date(datevar)
  }

  return(data)
}
