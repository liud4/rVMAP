# 04/06/2015 - Changed code with help from LS - no longer requires for loop
# 04/09/2015 - DL helped put code into function, and can now easily be sourced!
invalidNeuropsychItems <- function(dat) {
  x <- c(
    "np.tower01",
    "np.tower02",
    "np.tower03",
    "np.tower04",
    "np.tower05",
    "np.tower06",
    "np.tower07",
    "np.tower08",
    "np.tower09",
    "np.tower.items",
    "np.tower.ruleviol",
    "np.tower.ruleviol.cumperc",
    "np.tower",
    "np.tower.ss"
  )

  # Variables to set to NA for MAP 007 1:14 of x
  dat[dat$map.id == "007" & dat$epoch == 1, x] <- NA

  # MAP 035 and MAP 242 - All BFLT Items invalid
  x1 <- c(
    "np.biber1",
    "np.biber1.z",
    "np.biber2",
    "np.biber2.z",
    "np.biber3",
    "np.biber3.z",
    "np.biber4",
    "np.biber4.z",
    "np.biber5",
    "np.biber5.z",
    "np.biber.t1to5",
    "np.biber.t1to5.z",
    "np.biber.t1to5.persev",
    "np.biber.t1to5.extra",
    "np.biberb",
    "np.biberb.z",
    "np.biber.sd",
    "np.biber.sd.z",
    "np.biber.ld",
    "np.biber.ld.z",
    "np.biber1.figures",
    "np.biber2.figures",
    "np.biber3.figures",
    "np.biber4.figures",
    "np.biber5.figures",
    "np.biberb.figures",
    "np.bibersd.figures",
    "np.biberld.figures",
    "np.biber.hits",
    "np.biber.related.falsealarms",
    "np.biber.unrelated.falsealarms",
    "np.biber.falsealarms",
    "np.biber.recoghitrate",
    "np.biber.falsealarm.relatedrate",
    "np.biber.falsealarm.unrelatedrate",
    "np.biber.falsealarm.totalrate",
    "np.biber.discrim",
    "np.biber.discrim.related",
    "np.biber.discrim.unrelated",
    "np.biber.bias",
    "np.biber.bias.related",
    "np.biber.bias.unrelated"
  )

  dat[dat$map.id %in% c("035", "242") & dat$epoch == 1, x1] <- NA

  # MAP 065 - some BFLT recognition items are invalid (np_biber_hits is okay)
  x2 <- c(
    "np.biber.related.falsealarms",
    "np.biber.unrelated.falsealarms",
    "np.biber.falsealarms",
    "np.biber.recoghitrate",
    "np.biber.falsealarm.relatedrate",
    "np.biber.falsealarm.unrelatedrate",
    "np.biber.falsealarm.totalrate",
    "np.biber.discrim",
    "np.biber.discrim.related",
    "np.biber.discrim.unrelated",
    "np.biber.bias",
    "np.biber.bias.related",
    "np.biber.bias.unrelated"
  )

  dat[dat$map.id == "065" & dat$epoch == 1, x2] <- NA

  # MAP 135, 220, 234
  x3 <- c(
    "np.color"                  , "np.color.ss"               ,
    "np.colorword.sum"          , "np.colorword.comp"         ,
    "np.inhibitcolor.diff"      , "np.inhibitcolor.contrast"  ,
    "np.color.scerr"            , "np.color.ucerr"            ,
    "np.color.err"              , "np.color.cumpercerr"       ,
    "np.color.cumpercerr.factor"
  )
  dat[dat$map.id %in% c("135","220","234") & dat$epoch==1,x3] <- NA

  x4 <- c(
    "np.cvltrec.hits",
    "np.cvltrec.hits.z",
    "np.cvltrec.falsepos",
    "np.cvltrecog.falsepos.z",
    "np.cvltrecog.discrim",
    "np.cvltrecog.discrim.z",
    "np.cvltrecog.sourcediscrim",
    "np.cvltrecog.sourcediscrim.z",
    "np.cvltrecog.semanticdiscrim",
    "np.cvltrecog.semanticdiscrim.z",
    "np.cvltrecog.noveldiscrim",
    "np.cvltrecog.noveldiscrim.z",
    "np.cvltrecog.responbias",
    "np.cvltrecog.responbias.z"
  )

  dat[dat$map.id == 209 & dat$epoch == 1, x4] <- NA

  ##
  ## below added by OAK 08/25/2017
  ##

  # MAP 016 – BNT invalid

  x5 <- Hmisc::Cs(
    np.bnt,
    np.bnt.z
  )

  dat[dat$map.id == '016' & dat$epoch == 2, x5] <- NA

  # MAP 033, 176 - Color–word invalid

  x6 <- Hmisc::Cs(
    np.color,
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
    np.inhibit.err.ss
  )

  dat[dat$map.id %in% c("033", "176") & dat$epoch == 2, x6] <- NA

  # MAP 111 – BNT VALID – originally marked invalid

  # MAP 265, 266 – Biber learning trials valid; Biber distractor, immediate recall, delayed recall, and recognition invalid

  x7 <- Hmisc::Cs(
    np.biberb,
    np.biberb.z,
    np.biber.sd,
    np.biber.sd.z,
    np.biber.ld,
    np.biber.ld.z,
    np.biberb.figures,
    np.bibersd.figures,
    np.biberld.figures,
    np.biber.hits,
    np.biber.related.falsealarms,
    np.biber.unrelated.falsealarms,
    np.biber.falsealarms,
    np.biber.recoghitrate,
    np.biber.falsealarm.relatedrate,
    np.biber.falsealarm.unrelatedrate,
    np.biber.falsealarm.totalrate,
    np.biber.discrim,
    np.biber.discrim.related,
    np.biber.discrim.unrelated,
    np.biber.bias,
    np.biber.bias.related,
    np.biber.bias.unrelated
  )

  dat[dat$map.id %in% c("265", "266") & dat$epoch == 2, x7] <- NA

  # 20171024 OAK: MAP 201 – All neuropsych variables from epoch 1 invalid

  x8 <- grep("^np(?!.*elig$).*$", names(dat), value = TRUE, perl = TRUE) # 20180109 OAK: get names starting with np but not ending in elig
  dat[dat$map.id == "201" & dat$epoch == 1, x8] <- NA

  ## OAK 20180224: This might be the issue since memory.composite isn't made until after this code. Commenting out.
  ## 20180115 OAK: MAP 209 - Set np.memory.composite from epoch 1 to missing
  # x9 <- Cs(np.memory.composite)
  #
  # dat[dat$map.id == "209" & dat$epoch == 1, x9] <- NA

  # DELETE 20180124 OAK: while waiting on invalid np in epoch 3 from AJ, will try smaller than -7776 to NA
  # DELETE dat[dat$epoch == 3, x10] <- lapply(dat[dat$epoch == 3, x10], function(x) ifelse(x < -7776, NA, x))

  # 20180201 OAK: after speaking to Kim, set epoch 3 np.* %in% c(-7777, -8888, -9999) to NA
  x10 <- grep("^np(?!.*notes$).*$", names(dat[sapply(dat, is.numeric)]), value = TRUE, perl = TRUE)
  dat[dat$epoch == 3, x10] <- lapply(dat[dat$epoch == 3, x10],
                                     function(x) ifelse(x %in% c(-7777, -8888, -9999), NA, x))

  # 20180202 OAK: set epoch 3 calculated np vars to NA if components %in% c(-7777, -8888, -9999, NA)
  meta.epoch3 <- dplyr::bind_rows(MAPfreeze.list[["epoch_3"]][["metadata"]], NULL)
  meta.epoch3$field_name <- gsub("\\_", "\\.", meta.epoch3$field_name)
  calc.e3.vars <- data.frame(
    meta.epoch3[meta.epoch3$field_type == "calc", c('field_name', 'select_choices_or_calculations')],
    stringsAsFactors = FALSE
  )
  calc.e3.vars.np <- calc.e3.vars[grep("np\\.", calc.e3.vars$field_name), ]

  x12 <- calc.e3.vars.np$field_name

  np.components <- regmatches(
    calc.e3.vars.np[, 2],
    gregexpr("(?<=\\[).*?(?=\\])",
             calc.e3.vars.np[, 2],
             perl = TRUE
    )
  )

  names(np.components) <- calc.e3.vars.np$field_name
  np.components <- rapply(np.components, function(x) gsub("\\_", "\\.", x), how = "list")

  for (i in 1:length(x12)) {
    np.calc.var <- x12[i]
    np.calc.var.comp <- np.components[[i]]
    dat[dat$epoch == 3, np.calc.var] <- ifelse(
      apply(dat[dat$epoch == 3, np.calc.var.comp, drop = F], 1, function(x) any(x %in% c(-7777, -8888, -9999, NA))),
      NA,
      dat[dat$epoch == 3, np.calc.var]
    )
  }

  #

  dat
}
