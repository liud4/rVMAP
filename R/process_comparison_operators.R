process_comparison_operators <- function(number, precision = 7) {
  original.digits <- getOption("digits")
  options(digits = precision)

  number <- gsub("_", "", number)
  number <- gsub("?", "", number)
  
  digits_after_dot <- ifelse(grepl("\\.", number), nchar(gsub("(.*\\.)", "", as.character(number))), NA)

  if (grepl(">", number)) {
    number <- as.numeric(gsub(">", "", number))

    ifelse(
      is.na(digits_after_dot),
      return(as.numeric(number + 1)),
      return(as.numeric(number + 10^-digits_after_dot))
    )

  } else if (grepl("<", number)) {
    number <- as.numeric(gsub("<", "", number))

    ifelse(
      is.na(digits_after_dot),
      return(as.numeric(number - 1)),
      return(as.numeric(number - 10^-digits_after_dot))
    )

  } else {
    return(as.numeric(number))
  }

  options(digits = original.digits)
}
