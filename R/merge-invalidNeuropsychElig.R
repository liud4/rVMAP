# Written 06 May 2015 JN

invalidNeuropsychElig <- function(dat){
  
  x <- c(
    "np.srt6.elig"              ,       "np.srt.immed.elig"               ,
    "np.srt.immed.z.elig"       ,       "np.srt.sdcr.elig"                ,
    "np.srt.ldfr.elig"          ,       "np.srt.ldfr.z.elig"              ,
    "np.srt.recog.elig"         ,       "np.srt.recog.z.elig"             ,
    "np.srt.intrus.elig"        ,       "np.srt.intrus.z.elig"            ,
    "np.srt.reps.elig"     )
  
  # Variables to set to NA for MAP 007
  dat[dat$map.id=="007",x] <- NA
  
  x1 <- c(
    "np.blocks.elig",    "np.blocks.ss.elig"
    )
  
  # Variables to set to NA for MAP 016 & MAP 213
  dat[dat$map.id %in% c("016","213"),x1] <- NA
  
  # MAP 025
  x2 <- c(
    "np.srt1.elig",
    "np.srt2.elig",
    "np.srt3.elig",
    "np.srt4.elig",
    "np.srt5.elig",       
    "np.srt6.elig",
    "np.srt.immed.elig",       
    "np.srt.immed.z.elig",
    "np.srt.sdcr.elig",
    "np.srt.ldfr.elig",
    "np.srt.ldfr.z.elig",
    "np.srt.recog.elig",       
    "np.srt.recog.z.elig",
    "np.srt.intrus.elig",       
    "np.srt.intrus.z.elig",
    "np.srt.reps.elig"
         )
  
  # Variables to set to NA for MAP 025
  dat[dat$map.id=="025",x2] <- NA
  
  # Variables for MAP 039
  x3 <- c(
    "np.tmtb.elig",
    "np.tmtb.ss.elig",
    "np.tmtb.seqerr.elig",
    "np.tmtb.seterr.elig"
    )
  
  # Set variables to missing for MAP 039
  dat[dat$map.id=="039",x3] <- NA
  
  # MAP 137
  x4 <- c(
    "np.srt.ldfr.elig",
    "np.srt.ldfr.z.elig",
    "np.srt.recog.elig",
    "np.srt.recog.z.elig",
    "np.srt.intrus.elig",
    "np.srt.intrus.z.elig"
    )
  
  # Set variables to missing for MAP 137
  dat[dat$map.id=="137",x4] <- NA
  
  # MAP 201
  x5 <- c(
    "np.strp.color.elig",
    "np.strp.color.ss.elig",
    "np.strp.color.ucerr.elig",
    "np.strp.color.scerr.elig",
    "np.strp.colorword.elig",
    "np.strp.colorword.ss.elig",
    "np.strp.colorword.ucerr.elig",
    "np.strp.colorword.scerr.elig",
    "np.strp.ucerr.elig",
    "np.strp.scerr.elig")
  
  # Set variables to missing for MAP 201
  dat[dat$map.id=="201",x5] <- NA
  
  # MAP 212 & MAP 236
  x6 <- c(
    "np.vegq1.elig",
    "np.vegq2.elig",
    "np.vegq3.elig",
    "np.vegq4.elig",
    "np.veg.elig",
    "np.veg.z.elig",
    "np.veg.reps.elig",
    "np.veg.intrus.elig"
    )
  
  # Set variables to missing for MAP 212 & 236
  dat[dat$map.id %in% c("212","236"),x6] <- NA
  
  # MAP 299
  x7 <- c(
    "np.digitsf.elig",
    "np.digits.elig",
    "np.digits.ss.elig",
    "np.digitsf.span.elig",
    "np.digitsf.span.z.elig"
    )
  
  # Set variables to missing for MAP 299
  dat[dat$map.id=="299",x7] <- NA
  
  # Output
  dat
}