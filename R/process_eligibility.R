#' Apply various processing operations to the eligibility data set.
#'
#' @param elig_data A data frame containing VMAC eligibility data.
#' @return A processed version of \code{elig_data}.
#' @export

process_eligibility <- function(elig_data) {
  elig_data %<>% mutate_if(
    ~ any(class(.) %in% c("numeric", "integer", "character")),
    ~ missing_to_na(., equal.val = c(-6666, -7777, -8888, -9999), mod.val = -1111, restrict.sign = TRUE)
  )

  elig_data <- convert_dates(elig_data)

  # data$faq.redcap <- data$faq
  faqvars <- paste0("faq", formatC(1:10, width = 2, flag = "0"))

  elig_data$faq <- apply(elig_data[, faqvars], 1, total_score, threshold = 1)

  elig_data$np_srt_immed_redcap <- elig_data$np_srt_immed
  np_srt_immedvars <- paste0("np_srt", 1:6)
  elig_data$np_srt_immed <- apply(elig_data[, np_srt_immedvars], 1, total_score, threshold = 1)

  elig_data$np_cfl_redcap <- elig_data$np_cfl
  np.cflvars <- c(
    paste0("np_cfl_fq", 1:4),
    paste0("np_cfl_cq", 1:4),
    paste0("np_cfl_lq", 1:4)
  )
  elig_data$np_cfl <- apply(elig_data[, np.cflvars], 1, total_score, threshold = 1)

  elig_data$np_veg_redcap <- elig_data$np_veg
  np.vegvars <- paste0("np_vegq", 1:4)
  elig_data$np_veg <- apply(elig_data[, np.vegvars], 1, total_score, threshold = 1)

  elig_data$np_bvrt_redcap <- elig_data$np_bvrt
  np.bvrtvars <- paste0("np_bvrt", formatC(1:10, width = 2, flag = "0"))
  elig_data$np_bvrt <- apply(elig_data[, np.bvrtvars], 1, total_score, threshold = 1)

  elig_data$np_tmta_trun <- ifelse(elig_data$np_tmta > 150, 150, elig_data$np_tmta)
  elig_data$np_tmtb_trun <- ifelse(elig_data$np_tmtb > 240, 240, elig_data$np_tmtb)

  elig_data <- within(elig_data, {
    cdr_factor <- factor(
      cdr,
      levels = c(0, 1, 2, 4, 6),
      labels = c("0", "0.5", "1", "2", "3"),
      ordered = TRUE
    )

    label(cdr_date)="CDR - Date of Administration, from Eligibility"
    label(cdr)="CDR Global Score- raw REDCap value. Probably not what you want."
    label(cdr_factor)="CDR Global Score, from Eligibility, recoded"
    label(faq_date)="FAQ Date of Administration, from Eligibility"
    #label(faq_redcap)="FAQ Total Score, from Eligibility"
    label(faq)="FAQ Total Score, from Eligibility, recalculated"
    label(np_date)="NP Assessment - date of administration, from Eligibility"
    label(np_moca)="MoCA total score, from Eligibility"
    label(np_srt_immed_redcap)="SRT total immediate recall, from Eligibility"
    label(np_srt_immed)="SRT total immediate recall, from Eligibility, recalculated"
    label(np_srt_ldfr)="SRT long delay free recall, from Eligibility"
    label(np_tmta)="Trails A time, from Eligibility"
    label(np_tmtb)="Trails B time, from Eligibility"
    label(np_digitsf)="Digit Span forward score, from Eligibility"
    label(np_digitsb)="Digit Span backward score, from Eligibility"
    label(np_blocks)="Block Design total score, from Eligibility"
    label(np_strp_colorword)="Stroop color-word score, from Eligibility"
    label(np_cfl_redcap)="CFL total score, from Eligibility"
    label(np_cfl)="CFL total score, from Eligibility, recalculated"
    label(np_veg_redcap)="Vegetable Naming total score, from Eligibility"
    label(np_veg)="Vegetable Naming total score, from Eligibility, recalculated"
    label(np_bnt)="BNT total, from Eligibility"
    label(np_wrat)="WRAT Reading total score, from Eligibility"
    label(np_wrat_ss)="WRAT Reading total standard score, from Eligibility"
    label(np_bvrt_redcap)="BVRT total score, from Eligibility"
    label(np_bvrt)="BVRT total score, from Eligibility, recalculated"
    label(age)="current age, from Eligibility"
    label(np_tmta_trun) <- "Trails A time (s), truncated, from Eligibility"
    label(np_tmtb_trun) <- "Trails B time (s), truncated, from Eligibility"
  })

  keep.vars <- Cs(
    age,
    cdr_date,
    cdr,
    cdr_factor,
    faq_date,
    # faq_redcap,
    # faq,
    np_date,
    np_moca,
    np_tmta,
    np_tmtb,
    np_tmtb_ss,
    np_tmtb_seqerr,
    np_tmtb_seterr,
    np_digitsf,
    np_digits,
    np_digits_ss,
    np_digitsf_span,
    np_digitsf_span_z,
    np_digitsb,
    np_blocks,
    np_blocks_ss,
    np_strp_color,
    np_strp_color_ss,
    np_strp_color_ucerr,
    np_strp_color_scerr,
    np_strp_colorword,
    np_strp_colorword_ss,
    np_strp_colorword_ucerr,
    np_strp_colorword_scerr,
    np_strp_ucerr,
    np_strp_scerr,
    # np_cfl_redcap,
    np_cfl,
    # np_veg_redcap,
    np_veg,
    np_veg_z,
    np_vegq1,
    np_vegq2,
    np_vegq3,
    np_vegq4,
    np_veg_reps,
    np_veg_intrus,
    np_bnt,
    np_wrat,
    np_wrat_ss,
    # np_bvrt_redcap,
    np_bvrt,
    np_srt1,
    np_srt2,
    np_srt3,
    np_srt4,
    np_srt5,
    np_srt6,
    np_srt_immed,
    np_srt_immed_z,
    np_srt_sdcr,
    np_srt_ldfr,
    np_srt_ldfr_z,
    np_srt_recog,
    np_srt_recog_z,
    np_srt_intrus,
    np_srt_intrus_z,
    np_srt_reps,
    np_srt_immed_redcap,
    np_strp_word,
    np_strp_word_ss,
    np_strp_word_ucerr,
    np_strp_word_scerr,
    np_tmta_trun,
    np_tmtb_trun
  )

  dataToKeep <- elig_data[, c("map_id", keep.vars)]
  names(dataToKeep) <- c("map_id", paste0(keep.vars, "_elig"))

  # OAK 20181120: Added invalidate_neuropsych_elgibility function
  dataToKeep <- invalidate_neuropsych_eligibility(dataToKeep)
  dataToKeep <- invalidate_color_blind(dataToKeep)

  return(dataToKeep)
}
