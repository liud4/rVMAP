dates <- function(dat) {
  # Returns dat with various date variables converted to class Date
  # Note that this function is called within the Eligibility
  # processing as well as within the main merge program

  datevars <- setdiff(names(dat)[grepl("\\.date", names(dat))], c("abp.date", "usa.date"))
  # the eligibility dset doesn't have dob
  if ("dob" %in% names(dat)) {
    datevars <- c(datevars, "dob")
  }

  cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("Converting date variables\n")

  for (vname in datevars) {
    datevar <- dat[, vname]

    if (is.factor(datevar)) {
      datevar <- as.character(datevar)
    }

    if (!is.character(datevar)) {
      cat("        ", vname, " is not in character format and may not need to be converted.\n")
      next
    }

    datevar[datevar %in% c("1111-11-11", "")] <- NA

    if (any((!is.na(datevar)) & nchar(datevar) != 10)) {
      cat("        ", vname, " is not in yyyy-mm-dd format.\n")
      next
    }

    dat[, vname] <- as.Date(datevar)
  }
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  dat
}
