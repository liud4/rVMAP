#' Apply various processing operations to the eligibility data set.
#'
#' @param elig_data A data frame containing VMAC eligibility data.
#' @return A processed version of \code{elig_data}.
#' @export

process_eligibility <- function(elig_data) {
  elig_data %<>% mutate_if(
    is.numeric,
    funs(
      missingtoNA(., equal.val = c(-9999, -8888))
    )
  )

  elig_data <- dates(elig_data)

  # data$faq.redcap <- data$faq
  faqvars <- paste0("faq", formatC(1:10, width = 2, flag = "0"))

  elig_data$faq <- apply(elig_data[, faqvars], 1, totscore, threshold = 1)

  elig_data$np.srt.immed.redcap <- elig_data$np.srt.immed
  np.srt.immedvars <- paste0("np.srt", 1:6)
  elig_data$np.srt.immed <- apply(elig_data[, np.srt.immedvars], 1, totscore, threshold = 1)

  elig_data$np.cfl.redcap <- elig_data$np.cfl
  np.cflvars <- c(
    paste0("np.cfl.fq", 1:4),
    paste0("np.cfl.cq", 1:4),
    paste0("np.cfl.lq", 1:4)
  )
  elig_data$np.cfl <- apply(elig_data[, np.cflvars], 1, totscore, threshold = 1)

  elig_data$np.veg.redcap <- elig_data$np.veg
  np.vegvars <- paste0("np.vegq", 1:4)
  elig_data$np.veg <- apply(elig_data[, np.vegvars], 1, totscore, threshold = 1)

  elig_data$np.bvrt.redcap <- elig_data$np.bvrt
  np.bvrtvars <- paste0("np.bvrt", formatC(1:10, width = 2, flag = "0"))
  elig_data$np.bvrt <- apply(elig_data[, np.bvrtvars], 1, totscore, threshold = 1)

  elig_data$np.tmta.trun <- ifelse(elig_data$np.tmta > 150, 150, elig_data$np.tmta)
  elig_data$np.tmtb.trun <- ifelse(elig_data$np.tmtb > 240, 240, elig_data$np.tmtb)

  elig_data <- within(elig_data, {
    cdr.factor <- factor(
      cdr,
      levels = c(0, 1, 2, 4, 6),
      labels = c("0", "0.5", "1", "2", "3"),
      ordered = TRUE
    )

    label(cdr.date)="CDR - Date of Administration, from Eligibility"
    label(cdr)="CDR Global Score- raw REDCap value. Probably not what you want."
    label(cdr.factor)="CDR Global Score, from Eligibility, recoded"
    label(faq.date)="FAQ Date of Administration, from Eligibility"
    #label(faq.redcap)="FAQ Total Score, from Eligibility"
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

  dataToKeep <- elig_data[, c("map.id", keep.vars)]
  names(dataToKeep) <- c("map.id", paste0(keep.vars, ".elig"))

  dataToKeep
}
