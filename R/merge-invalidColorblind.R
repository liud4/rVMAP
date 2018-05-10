invalidColorblind <- function(dat){
    # Sets selected variables for colorblind individuals to NA.
    # Note that this happens regardless of epoch!

    list.id <- c("028","033","131","169","176","201")
    vars.invalid <- Cs(np.color,
                     np.color.ss,
                     np.word,
                     np.word.ss,
                     np.inhibit,
                     np.inhibit.ss,
                     np.colorword.sum,
                     np.colorword.comp,
                     np.inhibitcolor.diff,
                     np.inhibitcolor.contrast,
                     np.color.scerr,
                     np.color.ucerr,
                     np.color.err,
                     np.color.cumpercerr,
                     np.word.scerr,
                     np.word.ucerr,
                     np.word.err,
                     np.word.cumpercerr,
                     np.inhibit.scerr,
                     np.inhibit.cumpercscerr,
                     np.inhibit.ucerr,
                     np.inhibit.cumpercucerr,
                     np.inhibit.err,
                     np.inhibit.err.ss,
                     np.strp.word.elig,
                     np.strp.word.ss.elig,
                     np.strp.word.ucerr.elig,
                     np.strp.word.scerr.elig,
                     np.strp.color.elig,
                     np.strp.color.ss.elig,
                     np.strp.color.ucerr.elig,
                     np.strp.color.scerr.elig,
                     np.strp.colorword.elig,
                     np.strp.colorword.ss.elig,
                     np.strp.colorword.ucerr.elig,
                     np.strp.colorword.scerr.elig,
                     np.strp.ucerr.elig,
                     np.strp.scerr.elig)
  
    dat[dat$map.id %in% list.id,vars.invalid] <- NA

    dat
}
