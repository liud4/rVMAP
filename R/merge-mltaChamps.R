## Changes

## 06 Apr 2105, LS:
#  re-doing Champs individ kcal items so they are individually usable
#  (request from Liz & AJ, 24 Mar 2015)

mltaChamps <- function(dat){
    # Returns dat with MLTA & CHAMPS derived variables added
    # and with some totals that were calculated in REDCap recalculated
    # Make sure to source "scoringFunctions.R" before calling this function

    dat <- within(dat, {

        mlta01.kcal <- mlta01 * 3.5
        mlta02.kcal <- mlta02 * 4.5
        mlta03.kcal <- mlta03 * 5.0
        mlta04.kcal <- mlta04 * 2.5
        mlta05.kcal <- mlta05 * 4.5
        mlta06.kcal <- mlta06 * 6.0
        mlta07.kcal <- mlta07 * 4.0
        mlta08.kcal <- mlta08 * 4.5
        mlta09.kcal <- mlta09 * 5.0
        mlta10.kcal <- mlta10 * 6.0
        mlta11.kcal <- mlta11 * 4.0
        mlta12.kcal <- mlta12 * 5.5
        mlta13.kcal <- mlta13 * 5.5
        mlta14.kcal <- mlta14 * 4.5
        mlta15.kcal <- mlta15 * 6.0
        mlta16.kcal <- mlta16 * 3.0
        mlta17.kcal <- mlta17 * 3.5
        mlta18.kcal <- mlta18 * 5.0
        mlta19.kcal <- mlta19 * 5.5
        mlta20.kcal <- mlta20 * 8.0
        mlta21.kcal <- mlta21 * 6.0
        mlta22.kcal <- mlta22 * 7.0
        mlta23.kcal <- mlta23 * 3.0
        mlta24.kcal <- mlta24 * 6.0
        mlta25.kcal <- mlta25 * 6.0

        champs07.kcal <- ifelse(is.na(champs07.wk), NA, ifelse(champs07.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs07.hrs*4.5)) 
        champs09.kcal <- ifelse(is.na(champs09.wk), NA, ifelse(champs09.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs09.hrs*3  ))
        champs10.kcal <- ifelse(is.na(champs10.wk), NA, ifelse(champs10.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs10.hrs*2  ))
        champs14.kcal <- ifelse(is.na(champs14.wk), NA, ifelse(champs14.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs14.hrs*6  ))
        champs15.kcal <- ifelse(is.na(champs15.wk), NA, ifelse(champs15.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs15.hrs*4  ))
        champs16.kcal <- ifelse(is.na(champs16.wk), NA, ifelse(champs16.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs16.hrs*4.5)) 
        champs19.kcal <- ifelse(is.na(champs19.wk), NA, ifelse(champs19.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs19.hrs*3  ))
        champs20.kcal <- ifelse(is.na(champs20.wk), NA, ifelse(champs20.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs20.hrs*2.5))
        champs21.kcal <- ifelse(is.na(champs21.wk), NA, ifelse(champs21.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs21.hrs*4  ))
        champs22.kcal <- ifelse(is.na(champs22.wk), NA, ifelse(champs22.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs22.hrs*2.25)) 
        champs23.kcal <- ifelse(is.na(champs23.wk), NA, ifelse(champs23.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs23.hrs*3  ))
        champs24.kcal <- ifelse(is.na(champs24.wk), NA, ifelse(champs24.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs24.hrs*7  ))
        champs25.kcal <- ifelse(is.na(champs25.wk), NA, ifelse(champs25.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs25.hrs*6  ))
        champs26.kcal <- ifelse(is.na(champs26.wk), NA, ifelse(champs26.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs26.hrs*3.5)) 
        champs27.kcal <- ifelse(is.na(champs27.wk), NA, ifelse(champs27.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs27.hrs*2.5)) 
        champs28.kcal <- ifelse(is.na(champs28.wk), NA, ifelse(champs28.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs28.hrs*2.5)) 
        champs29.kcal <- ifelse(is.na(champs29.wk), NA, ifelse(champs29.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs29.hrs*4  ))
        champs30.kcal <- ifelse(is.na(champs30.wk), NA, ifelse(champs30.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs30.hrs*5  ))
        champs31.kcal <- ifelse(is.na(champs31.wk), NA, ifelse(champs31.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs31.hrs*3  ))
        champs32.kcal <- ifelse(is.na(champs32.wk), NA, ifelse(champs32.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs32.hrs*5  ))
        champs33.kcal <- ifelse(is.na(champs33.wk), NA, ifelse(champs33.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs33.hrs*3  ))
        champs34.kcal <- ifelse(is.na(champs34.wk), NA, ifelse(champs34.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs34.hrs*2  ))
        champs35.kcal <- ifelse(is.na(champs35.wk), NA, ifelse(champs35.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs35.hrs*2  ))
        champs36.kcal <- ifelse(is.na(champs36.wk), NA, ifelse(champs36.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs36.hrs*5  ))
        champs37.kcal <- ifelse(is.na(champs37.wk), NA, ifelse(champs37.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs37.hrs*4.5)) 
        champs38.kcal <- ifelse(is.na(champs38.wk), NA, ifelse(champs38.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs38.hrs*3  ))
        champs39.kcal <- ifelse(is.na(champs39.wk), NA, ifelse(champs39.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs39.hrs*2.5)) 
        champs40.kcal <- ifelse(is.na(champs40.wk), NA, ifelse(champs40.wk %in% c(0), 0, (3.5 * 60 * weight / 200) * champs40.hrs*5  ))
    })

    for(vname in paste0("mlta", formatC(1:25, width= 2, flag= 0))){
        label(dat[, paste0(vname, ".kcal")]) <-
            paste0(label(dat[, vname]), " (kcal, recalculated)")
    }

    dat$mlta.min <- apply(dat[, Cs(
        mlta01 , mlta02 , mlta03 , mlta04 , mlta05 , mlta06 , mlta07 , 
        mlta08 , mlta09 , mlta10 , mlta11 , mlta12 , mlta13 , mlta14 , 
        mlta15 , mlta16 , mlta17 , mlta18 , mlta19 , mlta20 , mlta21 , 
        mlta22 , mlta23 ,mlta24 , mlta25)], 1, totscore)

    dat$mlta.kcal <- apply(dat[, Cs(
        mlta01.kcal , mlta02.kcal , mlta03.kcal , mlta04.kcal , 
        mlta05.kcal , mlta06.kcal , mlta07.kcal , 
        mlta08.kcal , mlta09.kcal , mlta10.kcal , mlta11.kcal , 
        mlta12.kcal , mlta13.kcal , mlta14.kcal , 
        mlta15.kcal , mlta16.kcal , mlta17.kcal , mlta18.kcal , 
        mlta19.kcal , mlta20.kcal , mlta21.kcal , 
        mlta22.kcal , mlta23.kcal , mlta24.kcal , mlta25.kcal)], 1, totscore)

    dat$champs.caloricexpend.all <- apply(dat[, Cs(
            champs07.kcal, champs09.kcal, 
            champs10.kcal, champs14.kcal, 
            champs15.kcal, champs16.kcal, champs19.kcal, 
            champs20.kcal, champs21.kcal, champs22.kcal, champs23.kcal, champs24.kcal,
            champs25.kcal, champs26.kcal, champs27.kcal, champs28.kcal, champs29.kcal, 
            champs30.kcal, champs31.kcal, champs32.kcal, champs33.kcal, champs34.kcal, 
            champs35.kcal, champs36.kcal, champs37.kcal, champs38.kcal, champs39.kcal, 
            champs40.kcal)], 1,
            totscore)

    dat$champs.caloricexpend.mod <- apply(dat[, Cs(
            champs07.kcal, champs09.kcal,
            champs14.kcal, champs15.kcal, champs16.kcal, champs19.kcal,
            champs21.kcal, champs23.kcal, champs24.kcal,
            champs25.kcal, champs26.kcal, champs29.kcal,
            champs30.kcal, champs31.kcal, champs32.kcal, champs33.kcal,
            champs36.kcal, champs37.kcal, champs38.kcal,
            champs40.kcal)], 1, totscore)

    dat$champs.freqall <- apply(dat[, Cs(
            champs07.wk, champs09.wk, 
            champs10.wk, champs14.wk, champs15.wk, champs16.wk, champs19.wk, 
            champs20.wk, champs21.wk, champs22.wk, champs23.wk, champs24.wk,
            champs25.wk, champs26.wk, champs27.wk, champs28.wk, champs29.wk, 
            champs30.wk, champs31.wk, champs32.wk, champs33.wk, champs34.wk, 
            champs35.wk, champs36.wk, champs37.wk, champs38.wk, champs39.wk, 
            champs40.wk)], 1, totscore)

    dat$champs.freqmod <- apply(dat[, Cs(
            champs07.wk, champs09.wk,
            champs14.wk, champs15.wk, champs16.wk, champs19.wk,
            champs21.wk, champs23.wk, champs24.wk,
            champs25.wk, champs26.wk, champs29.wk,
            champs30.wk, champs31.wk, champs32.wk, champs33.wk,
            champs36.wk, champs37.wk, champs38.wk,
            champs40.wk)], 1, totscore)

    dat <- within(dat, {
        label(mlta.min) <- "MLTA Total Minutes, recalculated"
        label(mlta.kcal) <- "MLTA Total Kcal, recalculated"
        label(champs.caloricexpend.all) <- "CHAMPS Caloric Expenditure Per Week in all Activities, recalculated"
        label(champs.caloricexpend.mod) <- "CHAMPS Caloric Expenditure Per Week in Moderate Intensity Activities, recalculated"
        label(champs.freqall) <- "CHAMPS Frequency Per Week of all Activities, recalculated"
        label(champs.freqmod) <- "CHAMPS Frequency/Week of Moderate-Intensity Activities, recalculated"
    })

    # Label the champs kcal vars
    champs.kcal.names <- names(dat)[grepl("\\<champs[0-9][0-9]\\.kcal\\>",
         names(dat))]
    champs.hrs.names <- gsub("kcal", "hrs", champs.kcal.names, fixed= TRUE)
    for(i in seq_along(champs.kcal.names)){
        label(dat[, champs.kcal.names[i]]) <- gsub("hrs/wk", "kcal", 
            label(dat[, champs.hrs.names[i]]), fixed= TRUE)
    }

    dat
}
