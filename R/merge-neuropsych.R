# Changes:

# 02 Mar 2015, LS:
## Added "threshold = 1" to the score calculations below.
## This is essentially what was happening already, but I
## wanted to make Angela's and Katie's intentions explicit
## (emails 02 Mar 2015:  score most np tests only if all items present)

neuropsych <- function(dat){
    # Returns dat with neuropsych derived variables added
    # and with some totals that were calculated in REDCap recalculated
    # Make sure to source "scoringFunctions.R" before calling this function


    # 22 Jan 2015: re-calculating some np vars
    # 22 Jan 2015: AJ confirmed in email that missing 
    # should be counted as 0 in np.tower
    # unless all items are missing
    np.towervars <- paste0("np.tower", formatC(1:9, width= 2, flag= "0"))
    dat$np.tower <- apply(dat[, np.towervars], 1, function(vec) {
        if(all(is.na(vec))) NA else sum(vec, na.rm= TRUE)
    })

    np.animvars <- paste0("np.anim.q", 1:4)
    dat$np.anim <- apply(dat[, np.animvars], 1, totscore, threshold= 1)

    np.anim.errvars <- Cs(np.anim.intrus, np.anim.rep)
    dat$np.anim.err <- apply(dat[, np.anim.errvars], 1, totscore, threshold= 1)

    np.biber.t1to5vars <- paste0("np.biber", 1:5)
    dat$np.biber.t1to5 <- apply(dat[, np.biber.t1to5vars], 1, totscore, threshold= 1)

    #  22 Jan 2015: now some new vars for Leah's animal fluency project
    np.anim.repvars <- paste0("np.anim.", 1:8, "rep")
    dat$np.anim.meanrep <- apply(dat[, np.anim.repvars], 1, function(vec) {
        if(all(is.na(vec))) NA else mean(vec, na.rm= TRUE)
    })

    dat <- within(dat, {
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
        label(np.tmt.contrastdiff) <- 
            "Trails B vs A contrast difference, recalculated"
        
        # 21 July 2015: truncated tmta and tmtb as requested by VMAC
        np.tmta.trun <- ifelse(np.tmta > 150, 150, np.tmta)
        np.tmtb.trun <- ifelse(np.tmtb > 240, 240, np.tmtb)
        
        label(np.tmta.trun) <- "Trails A time (s), truncated"
        label(np.tmtb.trun) <- "Trails B time (s), truncated"
    })
    
    ###

    dat
}
