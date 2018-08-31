#' DEPRECATED. Apply various processing operations to the main data set.
#'
#' @param main_data A data frame containing VMAC main data.
#' @return A processed version of \code{main_data}.
#' @export

process_main <- function(main_data) {
  # Returns main_data with some preprocessing steps applied
  # To be called BEFORE the redcap-supplied R file

  # For any numeric variable, change value of -9999 to NA
  main_data <- main_data %>%
    mutate_if(
      ~ any(class(.) %in% c("numeric", "integer", "character")),
      ~ missingtoNA(., equal.val = c(-9999, -7777))
    )

  main_data <- main_data %>%
    mutate_at(
      grep("ccqself[0-9]+|ccqinform[0-9]+", names(.)),
      ~ missingtoNA(., equal.val = -2222)
    )

  # numericCols <- names(main_data)[sapply(main_data, is.numeric)]
  # main_data[, numericCols] <- apply(main_data[, numericCols], 2, minus9999)
  # main_data[, numericCols] <- apply(main_data[, numericCols], 2, minus7777)

  # As of 17 Dec 2014, the ccqself and ccqinform vars have values of -2222,
  # which is equivalent to -8888 (& we want to count as missing)
  # colsFor2222 <- c(
  #   names(main_data)[grepl("ccqself[0-9]+", names(main_data))],
  #   names(main_data)[grepl("ccqinform[0-9]+", names(main_data))])
  #
  # main_data[, colsFor2222] <- apply(main_data[, colsFor2222], 2, minus2222)

  # There is at least one variable for which we want to
  # preserve values of -8888 (i.e., we do not want to count -8888
  # as missing).
  # KG confirmed on 27 Oct 2014 (email) that we DO
  # want to treat -8888 as missing in ccqself
  # Also according to the MAP Enrollment Data Dictionary CHANGES 12-02-14.xlsx,
  # MANY variables that didn't use to have -8888 as an option have it now,
  # regardless of whether "N/A" is a reasonable response to the item.
  # In an email 8 Dec 2014, Angela said to count -8888 as missing for
  # medhx items like mhx_afib.
  # On 15 Dec 2014 in email KG said to treat -8888 as missing for all
  # complaint items.  In the meeting that day we decided to have as the default
  # to count -8888 as missing, and to specify the exceptions instead.

  # Here are the numeric vars for which we want to preserve values of -8888:

  main_data <- main_data %>%
    mutate_if(
      ~ any(class(.) %in% c("numeric", "integer", "character")) & any(!colnames(.) %in% "mhx_tobac_quit"),
      ~ missingtoNA(., equal.val = -8888)
    )

  # keep8888 <- c(
  #   "mhx_tobac_quit"
  # )
  # colsFor8888 <- setdiff(numericCols, keep8888)
  # main_data[, colsFor8888] <- apply(main_data[, colsFor8888], 2, minus8888)

  if(FALSE){
    # temp code to help:
    has2222 <- function(vec){
      -2222 %in% vec | "-2222" %in% vec
    }
    varsW2222 <- names(main_data)[apply(main_data, 2, has2222)]
    varsW2222 <- varsW2222[!grepl("notes|comments", varsW2222)]
    cat("The following variables have values of -2222 that we haven't changed to missing:\n")
    print(varsW2222)
  }

  # email from KG, 06 Aug 2014:  in ecogself, set 0 (DK) to missing
  # keeping the underscores here because the processing needs
  # to happen before the renaming
  ecogself_mem <- Hmisc::Cs(
    ecogself_mem01, ecogself_mem02, ecogself_mem03, ecogself_mem04,
    ecogself_mem05, ecogself_mem06, ecogself_mem07, ecogself_mem08
  )

  ecogself_lg <- Hmisc::Cs(
    ecogself_lang01, ecogself_lang02, ecogself_lang03,
    ecogself_lang04, ecogself_lang05, ecogself_lang06,
    ecogself_lang07, ecogself_lang08, ecogself_lang09
  )

  ecogself_vs <- Hmisc::Cs(
    ecogself_vis01, ecogself_vis02, ecogself_vis03, ecogself_vis04,
    ecogself_vis05, ecogself_vis06, ecogself_vis07
  )

  ecogself_plan <- Hmisc::Cs(
    ecogself_plan01, ecogself_plan02, ecogself_plan03,
    ecogself_plan04, ecogself_plan05
  )

  ecogself_org <- Hmisc::Cs(
    ecogself_org01, ecogself_org02, ecogself_org03,
    ecogself_org04, ecogself_org05, ecogself_org06
  )

  ecogself_att <- Hmisc::Cs(
    ecogself_attn01, ecogself_attn02,
    ecogself_attn03, ecogself_attn04
  )

  ecogself_underscore <- c(
    ecogself_mem,
    ecogself_lg,
    ecogself_vs,
    ecogself_plan,
    ecogself_org,
    ecogself_att
  )

  # main_data[, ecogself_underscore] <- apply(main_data[, ecogself_underscore], 2, zeroNA)

  main_data <- main_data %>%
    mutate_at(
      vars(ecogself_underscore),
      ~ missingtoNA(., equal.val = 0)
    )

  # 27 Feb 2015:  _notes variables
  notesvars <- names(main_data)[grepl("\\_notes\\>", names(main_data))]
  main_data[, notesvars] <- apply(main_data[, notesvars], 2, function(vec) {
    ifelse(stringr::str_trim(vec) %in% c('-8888', '-9999', ""), NA, vec) })

  return(main_data)
}
