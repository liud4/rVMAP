# Changes

# 27 Feb 2015, LS:
## Added preprocessing for .notes variables

# 04 Mar 2015, LS:
## moved missing-value function definitions to a separate file

# 15 Dec 2016, LS:
## Moved Dandan's pvlt code to mergeWithinEpochs function---
##    it can't be called from within this file because
##    in Epoch1, the pvlt vars aren't part of the main dataset

preprocessingMain <- function(dat){
  # Returns dat with some preprocessing steps applied
  # To be called BEFORE the redcap-supplied R file


  # For any numeric variable, change value of -9999 to NA
  numericCols <- names(dat)[sapply(dat, is.numeric)]
  dat[, numericCols] <- apply(dat[, numericCols], 2, minus9999)
  dat[, numericCols] <- apply(dat[, numericCols], 2, minus7777)

  # As of 17 Dec 2014, the ccqself and ccqinform vars have values of -2222,
  # which is equivalent to -8888 (& we want to count as missing)
  colsFor2222 <- c(
    names(dat)[grepl("ccqself[0-9]+", names(dat))],
    names(dat)[grepl("ccqinform[0-9]+", names(dat))])

  dat[, colsFor2222] <- apply(dat[, colsFor2222], 2, minus2222)

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
  keep8888 <- c(
    "mhx_tobac_quit"
  )
  colsFor8888 <- setdiff(numericCols, keep8888)
  dat[, colsFor8888] <- apply(dat[, colsFor8888], 2, minus8888)

  if(FALSE){
    # temp code to help:
    has2222 <- function(vec){
      -2222 %in% vec | "-2222" %in% vec
    }
    varsW2222 <- names(dat)[apply(dat, 2, has2222)]
    varsW2222 <- varsW2222[!grepl("notes|comments", varsW2222)]
    cat("The following variables have values of -2222 that we haven't changed to missing:\n")
    print(varsW2222)
  }

  # email from KG, 06 Aug 2014:  in ecogself, set 0 (DK) to missing
  # keeping the underscores here because the processing needs
  # to happen before the renaming
  ecogself_mem <- Cs(
    ecogself_mem01, ecogself_mem02, ecogself_mem03, ecogself_mem04,
    ecogself_mem05, ecogself_mem06, ecogself_mem07, ecogself_mem08
  )

  ecogself_lg <- Cs(
    ecogself_lang01, ecogself_lang02, ecogself_lang03,
    ecogself_lang04, ecogself_lang05, ecogself_lang06,
    ecogself_lang07, ecogself_lang08, ecogself_lang09
  )

  ecogself_vs <- Cs(
    ecogself_vis01, ecogself_vis02, ecogself_vis03, ecogself_vis04,
    ecogself_vis05, ecogself_vis06, ecogself_vis07
  )

  ecogself_plan <- Cs(
    ecogself_plan01, ecogself_plan02, ecogself_plan03,
    ecogself_plan04, ecogself_plan05
  )

  ecogself_org <- Cs(
    ecogself_org01, ecogself_org02, ecogself_org03,
    ecogself_org04, ecogself_org05, ecogself_org06
  )

  ecogself_att <- Cs(
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
  dat[, ecogself_underscore] <- apply(dat[, ecogself_underscore], 2, zeroNA)


  # 27 Feb 2015:  _notes variables
  notesvars <- names(dat)[grepl("\\_notes\\>", names(dat))]
  dat[, notesvars] <- apply(dat[, notesvars], 2, function(vec) {
    ifelse(str_trim(vec) %in% c('-8888', '-9999', ""), NA, vec) })

  dat
}
