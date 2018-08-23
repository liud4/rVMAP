#' Derive, label, and add neuropsychological variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added neuropsychological variables.
#' @export

derive_neuropsych <- function(data) {
  np.towervars <- paste0("np.tower", formatC(1:9, width = 2, flag = "0"))
  data$np.tower <- apply(data[, np.towervars], 1, function(vec) {
    if (all(is.na(vec))) NA else sum(vec, na.rm = TRUE)
  })

  np.animvars <- paste0("np.anim.q", 1:4)
  data$np.anim <- apply(data[, np.animvars], 1, totscore, threshold = 1)

  np.anim.errvars <- Hmisc::Cs(np.anim.intrus, np.anim.rep)
  data$np.anim.err <- apply(data[, np.anim.errvars], 1, totscore, threshold = 1)

  np.biber.t1to5vars <- paste0("np.biber", 1:5)
  data$np.biber.t1to5 <- apply(data[, np.biber.t1to5vars], 1, totscore, threshold = 1)

  #  22 Jan 2015: now some new vars for Leah's animal fluency project
  np.anim.repvars <- paste0("np.anim.", 1:8, "rep")
  data$np.anim.meanrep <- apply(data[, np.anim.repvars], 1, function(vec) {
    if (all(is.na(vec))) NA else mean(vec, na.rm = TRUE)
  })

  data <- within(data, {
    np.bibercontrast.ldvs5 <- (np.biber.ld / np.biber5) * 100
    label(np.bibercontrast.ldvs5) <- "Biber Contrast: Long Delay v. Trial 5 (long-delay retention % change)"

    # labels from above
    label(np.anim) <- "Animal Naming - total score, recalculated"
    label(np.anim.err) <- "Animal Naming - errors, recalculated"
    label(np.anim.meanrep) <- "Animal Naming - mean repetition distance"
    label(np.biber.t1to5) <- "Biber total immediate recall, recalculated"
    label(np.tower) <- "DKEFS Tower total raw score, recalculated"

    # 12 Jan 2015: re-do redcap coding for tmt.contrastdiff
    np.tmt.contrastdiff <- np.tmtb - np.tmta
    label(np.tmt.contrastdiff) <- "Trails B vs A contrast difference, recalculated"

    # 21 July 2015: truncated tmta and tmtb as requested by VMAC
    np.tmta.trun <- ifelse(np.tmta > 150, 150, np.tmta)
    np.tmtb.trun <- ifelse(np.tmtb > 240, 240, np.tmtb)
    np.tmtb.trun.log <- log(np.tmtb.trun)

    label(np.tmta.trun) <- "Trails A time (s), truncated"
    label(np.tmtb.trun) <- "Trails B time (s), truncated"
    label(np.tmtb.trun.log) <- "Log of Trails B time (s), truncated"

    # OAK 20180613 add np.inhibit.trun as per Katie's instructions on 20180612
    np.inhibit.trun <- ifelse(np.inhibit > 240, 240, np.inhibit)
    label(np.inhibit.trun) <- "DKEFS Color-Word inhibition time (s), truncated"
  })

  ###

  return(data)
}
