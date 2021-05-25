#' Apply various processing operations to the eligibility data set.
#'
#' @param elig_data A data frame containing VMAC eligibility data.
#' @return A processed version of \code{elig_data}.
#' @export

process_eligibility <- function(elig_data) {

  elig_data %<>% mutate_if(
    ~ any(class(.) %in% c("numeric", "integer")),
    ~ missing_to_na(., mod.val = -1111, restrict.sign = TRUE)
  )

  elig_data <- convert_dates(elig_data)

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

  #elig_data$np_veg_redcap <- elig_data$np_veg
  np.vegvars <- paste0("np_vegq", 1:4)
  elig_data$np_veg <- apply(elig_data[, np.vegvars], 1, total_score, threshold = 1)

  # elig_data$np_bvrt_redcap <- elig_data$np_bvrt
  np.bvrtvars <- paste0("np_bvrt", formatC(1:10, width = 2, flag = "0"))
  elig_data$np_bvrt <- apply(elig_data[, np.bvrtvars], 1, total_score, threshold = 1)

  elig_data$np_tmta_trun <- ifelse(elig_data$np_tmta > 150, 150, elig_data$np_tmta)
  elig_data$np_tmtb_trun <- ifelse(elig_data$np_tmtb > 240, 240, elig_data$np_tmtb)

  keep.vars <- Cs(
    age,
    cdr_date,
    cdr,
    cdr_boxes,
    cdr_mem,
    cdr_orient,
    cdr_judg,
    cdr_affairs,
    cdr_hobbies,
    cdr_care,
    faq_date,
    faq,
    np_date,
    np_moca,
    np_tmta,
    np_tmtb,
    np_tmtb_ss,
    np_tmtb_seqerr,
    np_tmtb_seterr,
    np_wais_digitsf,
    np_wais_digitsb,
    # np_digitsf,
    # np_digits,
    # np_digits_ss,
    # np_digitsf_span,
    # np_digitsf_span_z,
    # np_digitsb,
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
    np_cfl,
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

  dataToKeep <- process_factor_variables(data = dataToKeep, data_label = "eligibility", epoch = "epoch_0")

  dataToKeep <- within(dataToKeep, {
    # cdr = as.character(as.numeric(cdr))
    # cdr_boxes = as.character(as.numeric(cdr_boxes)) # should be numeric
    # cdr_mem = as.character(as.numeric(cdr_mem))
    # cdr_orient = as.character(as.numeric(cdr_orient))
    # cdr_judg = as.character(as.numeric(cdr_judg))
    # cdr_affairs = as.character(as.numeric(cdr_affairs))
    # cdr_hobbies = as.character(as.numeric(cdr_hobbies))
    # cdr_care = as.character(as.numeric(cdr_care))

    label(faq) <- "FAQ Total Score, from Eligibility, recalculated"
    label(np_srt_immed) <- "SRT total immediate recall, from Eligibility, recalculated"
    label(np_cfl) <- "CFL total score, from Eligibility, recalculated"
    label(np_veg) <- "Vegetable Naming total score, from Eligibility, recalculated"
    label(np_bvrt) <- "BVRT total score, from Eligibility, recalculated"
    label(np_tmta_trun) <- "Trails A time (s), truncated, from Eligibility"
    label(np_tmtb_trun) <- "Trails B time (s), truncated, from Eligibility"
  })

  # dataToKeep$cdr_factor <- factor(dataToKeep$cdr, levels = sort(unique(dataToKeep$cdr)))
  # dataToKeep$cdr_mem_factor = factor(dataToKeep$cdr_mem, levels = sort(unique(dataToKeep$cdr_mem)))
  # dataToKeep$cdr_orient_factor = factor(dataToKeep$cdr_orient, levels = sort(unique(dataToKeep$cdr_orient)))
  # dataToKeep$cdr_judg_factor = factor(dataToKeep$cdr_judg, levels = sort(unique(dataToKeep$cdr_judg)))
  # dataToKeep$cdr_affairs_factor = factor(dataToKeep$cdr_affairs, levels = sort(unique(dataToKeep$cdr_affairs)))
  # dataToKeep$cdr_hobbies_factor = factor(dataToKeep$cdr_hobbies, levels = sort(unique(dataToKeep$cdr_hobbies)))
  # dataToKeep$cdr_care_factor = factor(dataToKeep$cdr_care, levels = sort(unique(dataToKeep$cdr_care)))

  # dataToKeep$cdr_boxes <- as.numeric(dataToKeep$cdr_boxes)

  names(dataToKeep) <- c("map_id", paste0(setdiff(names(dataToKeep), "map_id"), "_elig"))

  dataToKeep <- invalidate_neuropsych_eligibility(dataToKeep)
  # dataToKeep <- invalidate_color_blind(dataToKeep)

  return(dataToKeep)
}
