# Changes

## 16 Mar 2015, LS:
#  Added processing of cdr (the redcap values are not the cdr values)

## 10 Apr 2015, LS:
#  Changed naming of cdr vars to match enrollment db
#  Changed date processing to incorporate 'missing' code from kim

## 06 May 2015 JN:
#  Added additional eligibility variables to keep

## 02 July 2015 JN & 20 July 2015 LS:
#  Added additional eligibility variables to keep

modify_eligibility <- function(data) {
  # Returns data (should be eligibility data),
  # keeping only the variables we want
  # with derived variables added
  # and with all variable names other than map.id having
  # the postfix ".elig"

  # make sure to have sourced scoringFunctions and missingsFunctions
  # before calling this function

  # For any numeric variable, change value of -9999 or -8888 to NA
  # numericCols <- names(data)[sapply(data, is.numeric)]
  # data[, numericCols] <- apply(data[, numericCols], 2,
  #                              minus9999)
  # data[, numericCols] <- apply(data[, numericCols], 2,
  #                              minus8888)

  data %<>% mutate_if(
    is.numeric,
    funs(
      missingtoNA(., equal.val = -9999),
      missingtoNA(., equal.val = -8888)
    )
  )

  # TODO: ???
  # process dates
  data <- dates(data)

  data$faq.redcap <- data$faq
  faqvars <- paste0("faq", formatC(1:10, width = 2, flag = "0"))
  # 02 Mar 2015 (meeting): AJ and KG want to check IDs w/ any FAQ
  # items missing
  data$faq <- apply(data[, faqvars], 1, totscore, threshold = 1)

  data$np.srt.immed.redcap <- data$np.srt.immed
  np.srt.immedvars <- paste0("np.srt", 1:6)
  data$np.srt.immed <- apply(data[, np.srt.immedvars], 1, totscore, threshold = 1)

  data$np.cfl.redcap <- data$np.cfl
  np.cflvars <- c(
    paste0("np.cfl.fq", 1:4),
    paste0("np.cfl.cq", 1:4),
    paste0("np.cfl.lq", 1:4)
  )
  data$np.cfl <- apply(data[, np.cflvars], 1, totscore, threshold = 1)

  data$np.veg.redcap <- data$np.veg
  np.vegvars <- paste0("np.vegq", 1:4)
  data$np.veg <- apply(data[, np.vegvars], 1, totscore, threshold = 1)

  data$np.bvrt.redcap <- data$np.bvrt
  np.bvrtvars <- paste0("np.bvrt", formatC(1:10, width = 2, flag = "0"))
  data$np.bvrt <- apply(data[, np.bvrtvars], 1, totscore, threshold = 1)

  # 21 July 2015: Truncate tmta and tmtb in eligibility
  data$np.tmta.trun <- ifelse(data$np.tmta > 150, 150, data$np.tmta)
  data$np.tmtb.trun <- ifelse(data$np.tmtb > 240, 240, data$np.tmtb)

  data <- within(data, {

    # email from KG: we will use cdr as ordered factor,
    # not as numeric values 0.5, 1, etc.
    cdr.factor <- factor(cdr,
                         levels = c(0, 1, 2, 4, 6),
                         labels = c("0","0.5","1","2","3"),
                         ordered = TRUE)

    label(cdr.date)="CDR - Date of Administration, from Eligibility"
    label(cdr)="CDR Global Score- raw REDCap value. Probably not what you want."
    label(cdr.factor)="CDR Global Score, from Eligibility, recoded"
    label(faq.date)="FAQ Date of Administration, from Eligibility"
    label(faq.redcap)="FAQ Total Score, from Eligibility"
    label(faq)="FAQ Total Score, from Eligibility, recalculated"
    label(np.date)="NP Assessment - date of administration, from Eligibility"
    label(np.moca)="MoCA total score, from Eligibility"
    label(np.srt.immed.redcap)="SRT total immediate recall, from Eligibility"
    label(np.srt.immed)="SRT total immediate recall, from Eligibility, recalculated"
    label(np.srt.ldfr)="SRT long delay free recall, from Eligibility"
    label(np.tmta)="Trails A time, from Eligibility"
    label(np.tmtb)="Trails B time, from Eligibility"
    label(np.digitsf)="Digit Span forward score, from Eligibility"
    label(np.digitsb)="Digit Span backward score, from Eligibility"
    label(np.blocks)="Block Design total score, from Eligibility"
    label(np.strp.colorword)="Stroop color-word score, from Eligibility"
    label(np.cfl.redcap)="CFL total score, from Eligibility"
    label(np.cfl)="CFL total score, from Eligibility, recalculated"
    label(np.veg.redcap)="Vegetable Naming total score, from Eligibility"
    label(np.veg)="Vegetable Naming total score, from Eligibility, recalculated"
    label(np.bnt)="BNT total, from Eligibility"
    label(np.wrat)="WRAT Reading total score, from Eligibility"
    label(np.wrat.ss)="WRAT Reading total standard score, from Eligibility"
    label(np.bvrt.redcap)="BVRT total score, from Eligibility"
    label(np.bvrt)="BVRT total score, from Eligibility, recalculated"
    label(age)="current age, from Eligibility"
    label(np.tmta.trun) <- "Trails A time (s), truncated, from Eligibility"
    label(np.tmtb.trun) <- "Trails B time (s), truncated, from Eligibility"
  })

  keep.vars <- Cs(
    age,
    cdr.date,
    cdr,
    cdr.factor,
    faq.date,
    # faq.redcap,
    # faq,
    np.date,
    np.moca,
    np.tmta,
    np.tmtb,
    np.tmtb.ss,
    np.tmtb.seqerr,
    np.tmtb.seterr,
    np.digitsf,
    np.digits,
    np.digits.ss,
    np.digitsf.span,
    np.digitsf.span.z,
    np.digitsb,
    np.blocks,
    np.blocks.ss,
    np.strp.color,
    np.strp.color.ss,
    np.strp.color.ucerr,
    np.strp.color.scerr,
    np.strp.colorword,
    np.strp.colorword.ss,
    np.strp.colorword.ucerr,
    np.strp.colorword.scerr,
    np.strp.ucerr,
    np.strp.scerr,
    # np.cfl.redcap,
    np.cfl,
    # np.veg.redcap,
    np.veg,
    np.veg.z,
    np.vegq1,
    np.vegq2,
    np.vegq3,
    np.vegq4,
    np.veg.reps,
    np.veg.intrus,
    np.bnt,
    np.wrat,
    np.wrat.ss,
    # np.bvrt.redcap,
    np.bvrt,
    np.srt1,
    np.srt2,
    np.srt3,
    np.srt4,
    np.srt5,
    np.srt6,
    np.srt.immed,
    np.srt.immed.z,
    np.srt.sdcr,
    np.srt.ldfr,
    np.srt.ldfr.z,
    np.srt.recog,
    np.srt.recog.z,
    np.srt.intrus,
    np.srt.intrus.z,
    np.srt.reps,
    np.srt.immed.redcap,
    np.strp.word,
    np.strp.word.ss,
    np.strp.word.ucerr,
    np.strp.word.scerr,
    np.tmta.trun,
    np.tmtb.trun
  )

  dataToKeep <- data[, c("map.id", keep.vars)]
  names(dataToKeep) <- c("map.id", paste0(keep.vars, ".elig"))

  dataToKeep
}
