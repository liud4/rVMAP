psqi <- function(dat) {
  fakedate1 <- "2013-01-01"
  fakedate2 <- "2013-01-02"
  my.tz <- '' # Here I'm following the convention from the ABP program (from Xue?)
  # my.origin <- "1970-01-01" # we may not need this

  dat <- within(dat, {
    psqi.hrs.sleep <- as.numeric(as.character(psqi.hrs.sleep))
    psqi.duration <- dplyr::case_when(
      psqi.hrs.sleep < 5 ~ 3,
      psqi.hrs.sleep >= 5 & psqi.hrs.sleep < 6 ~ 2,
      psqi.hrs.sleep >= 6 & psqi.hrs.sleep < 7 ~ 1,
      psqi.hrs.sleep >= 7 ~ 0,
      TRUE ~ NA_real_
    )

    psqi.other.use <- ifelse(
      psqi.other=="n/a" | psqi.other == -8888 | psqi.other == "na"
      | psqi.other == "none" | psqi.other == "" | psqi.other == "mg made the following notes:   'n/a'"
      | psqi.other == "kw made the following notes:   'n/a'" | psqi.other == "ew made the following notes:   'n/a'"
      | psqi.other == "lal made the following notes:   'n/a'", 0, 1)

    # OAK 20180817: commented out as per Dandan's email
    # psqi.other.use[map.id == 20] <- NA

    sleep.disturb.data <- cbind(
      psqi.cant.sleep,
      psqi.wake.up,
      psqi.bathroom,
      psqi.breathe,
      psqi.cough.snore,
      psqi.cold,
      psqi.hot,
      psqi.dreams,
      psqi.pain,
      psqi.other.often
    )

    pro.rate <- round(apply(sleep.disturb.data, MARGIN = 1, FUN = avgscore))

    sleep.disturb.data[, 10] <- psqi.other.often.prorate <- ifelse(is.na(psqi.other.often), pro.rate, psqi.other.often)

    psqi.other.often.prorate <- ifelse(is.na(psqi.other.often), pro.rate, psqi.other.often)

    # Summation of PSQI survey elements
    sumdist <- rowSums(sleep.disturb.data, dims = 1)

    psqi.disturb <- dplyr::case_when(
      sumdist == 0 ~ 0,
      sumdist >= 1 & sumdist <= 9 ~ 1,
      sumdist > 9 & sumdist <= 18 ~ 2,
      sumdist > 18 ~ 3,
      TRUE ~ NA_real_
    )

    psqi.time.sleep.new <- dplyr::case_when(
      psqi.time.sleep >= 0 & psqi.time.sleep <= 15 ~ 0,
      psqi.time.sleep > 15 & psqi.time.sleep <= 30 ~ 1,
      psqi.time.sleep > 30 & psqi.time.sleep <= 60 ~ 2,
      psqi.time.sleep > 60 ~ 3,
      TRUE ~ NA_real_
    )

    psqi.latency <- dplyr::case_when(
      (psqi.cant.sleep + psqi.time.sleep.new == 0) ~ 0,
      (psqi.cant.sleep + psqi.time.sleep.new >= 1) & (psqi.cant.sleep + psqi.time.sleep.new <= 2) ~ 1,
      (psqi.cant.sleep + psqi.time.sleep.new >= 3) & (psqi.cant.sleep + psqi.time.sleep.new <= 4) ~ 2,
      (psqi.cant.sleep + psqi.time.sleep.new >= 5) & (psqi.cant.sleep + psqi.time.sleep.new <= 6) ~ 3,
      TRUE ~ NA_real_
    )

    psqi.dysfx <- dplyr::case_when(
      (psqi.awake + psqi.enthusiasm == 0) ~ 0,
      (psqi.awake + psqi.enthusiasm >= 1) & (psqi.awake + psqi.enthusiasm <= 2) ~ 1,
      (psqi.awake + psqi.enthusiasm >= 3) & (psqi.awake + psqi.enthusiasm <= 4) ~ 2,
      (psqi.awake + psqi.enthusiasm >= 5) & (psqi.awake + psqi.enthusiasm <= 6) ~ 3,
      TRUE ~ NA_real_
    )

    psqi.bedtime <- ifelse(as.character(psqi.bedtime) %in% "-9999", NA, as.character(psqi.bedtime))
    psqi.bedtime.prep <- NA
    psqi.waketime <- ifelse(as.character(psqi.waketime) %in% "-9999", NA, as.character(psqi.waketime))
    psqi.waketime.prep <- NA
  })

  psqi.bedtime.list <- strsplit(dat$psqi.bedtime, ":")
  psqi.waketime.list <- strsplit(dat$psqi.waketime, ":")

  for (i in 1:nrow(dat)) {
    # TODO: add handling of raw values of 24:00:00 for both variables
    if (length(psqi.bedtime.list[[i]]) >= 2) {
      dat[i, 'psqi.bedtime.prep'] <- paste(
        fakedate1,
        paste0(
          formatC(psqi.bedtime.list[[i]][1], width = 2, format = "d", flag = 0),
          ":",
          psqi.bedtime.list[[i]][2]
        ),
        collapse = " ")
    }

    if (length(psqi.waketime.list[[i]]) >= 2) {
      dat[i, 'psqi.waketime.prep'] <- paste(
        fakedate2, # one day later
        paste0(
          formatC(psqi.waketime.list[[i]][1], width = 2, format = "d", flag = 0),
          ":",
          psqi.waketime.list[[i]][2]
        ),
        collapse = " ")
    }
  }

  dat <- within(dat, {
    psqi.bedtime <- as.POSIXct(strptime(psqi.bedtime.prep, "%Y-%m-%d %H:%M", tz = my.tz))
    psqi.waketime <- as.POSIXct(strptime(psqi.waketime.prep, "%Y-%m-%d %H:%M", tz = my.tz))
    psqi.diffsec <- difftime(psqi.waketime, psqi.bedtime, units="secs")
    psqi.diffhour <- psqi.diffsec / 3600
    newtib <- ifelse(psqi.diffhour > 24, psqi.diffhour - 24, psqi.diffhour)
    psqi.waketime.num <- lubridate::hour(psqi.waketime) + lubridate::minute(psqi.waketime)/60
    tmphse <- psqi.waketime.num / newtib * 100

    psqi.efficiency <- dplyr::case_when(
      tmphse >= 85 ~ 0,
      tmphse >= 75 & tmphse < 85 ~ 1,
      tmphse >= 65 & tmphse < 75 ~ 2,
      tmphse < 65 ~ 3,
      TRUE ~ NA_real_
    )

    psqi <- rowSums(
      cbind(
        psqi.duration,
        psqi.disturb,
        psqi.latency,
        psqi.dysfx,
        psqi.efficiency,
        psqi.sleep.quality,
        psqi.medicine
      )
    )

    rm(
      psqi.diffsec,
      psqi.diffhour,
      newtib,
      psqi.waketime.num,
      tmphse,pro.rate,
      psqi.bedtime.prep,
      psqi.waketime.prep,
      sleep.disturb.data
    )
  })

  # OAK 20171116 : below, changed alldat to dat

  dat = upData(
    dat,
    labels = c(psqi.duration = "PSQI - Duration of Sleep",
               psqi.disturb = "PSQI - Sleep Disturbance",
               psqi.latency = "PSQI - Sleep Latency",
               psqi.dysfx = "PSQI - Daytime Dysfunction due to Sleepiness",
               psqi.efficiency = "PSQI - Sleep Efficiency",
               psqi = "PSQI - Total Score"
    )
  )

  dat
}

