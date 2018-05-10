# 09 Dec 2016, LS:
#  Adapting code to deal with multiple epochs
#  Now all scaling will be done within epoch

frailty <- function(dat){
  
    dat <- within(dat, {
        frail.grip.average <- (frail.grip01+frail.grip02+frail.grip03)/3
        gripstrength.index <- frail.gripbest/bmi
        gait.speed <- 4.572/frail.gait
        gait.speed.index <- (gait.speed)/(height/100)
        frail.exhaustion.cont <- frailt.effort + frail.going
        frail.exhaustion <- ifelse(frailt.effort==-9999|frail.going==-9999,NA,
                                      ifelse(frailt.effort==2|frailt.effort==3|frail.going==2|frail.going==3,1,0))

        frail.phys.active <- rep(0,nrow(dat))
        frail.phys.active[sex==-999|is.na(mlta.kcal)] <- NA
        frail.phys.active[sex==1 & mlta.kcal<=383] <- 1
        frail.phys.active[sex==2 & mlta.kcal<=270] <- 1

        frail.walk <- rep(0,nrow(dat))
        frail.walk[sex==-9999|is.na(height)|is.na(frail.gait)] <- NA
        frail.walk[sex==1 & height<=173 & frail.gait>=7] <- 1
        frail.walk[sex==1 & height>173 & frail.gait>=6] <- 1
        frail.walk[sex==2 & height<=159 & frail.gait>=7] <- 1
        frail.walk[sex==2 & height>159 & frail.gait>=6] <- 1

        frail.grip <- rep(0,nrow(dat))
        frail.grip[sex==-9999|is.na(bmi)|is.na(frail.gripbest)] <- NA
        frail.grip[sex==1 & bmi <= 24 & frail.gripbest <= 29] <- 1
        frail.grip[sex==1 & bmi >= 24.1 & bmi <= 26 & frail.gripbest <= 30] <- 1
        frail.grip[sex==1 & bmi >= 26.1 & bmi <= 28 & frail.gripbest <= 30] <- 1
        frail.grip[sex==1 & bmi > 28 & frail.gripbest <= 32] <- 1
        frail.grip[sex==2 & bmi <= 23 & frail.gripbest <= 17] <- 1
        frail.grip[sex==2 & bmi >= 23.1 & bmi <= 26 & frail.gripbest <= 17.3] <- 1
        frail.grip[sex==2 & bmi >= 26.1 & bmi <= 29 & frail.gripbest <= 18] <- 1
        frail.grip[sex==2 & bmi > 29 & frail.gripbest <= 21] <- 1

        x <- cbind(frail.weightloss,frail.exhaustion,frail.walk,frail.grip,frail.phys.active)
        frail.range <- apply(x,MARGIN=1,FUN=sum)
        frail.category <- ifelse(frail.range==0,0,1)
        frail.ordinal <- ifelse(frail.range==0,0,
                                   ifelse(frail.range==1|frail.range==2,1,2))

        frail.grip.average.zscore <- -1*ave(frail.grip.average, epoch, sex.factor, FUN=scale)

        gait.speed.zscore <- -1*ave(gait.speed, epoch, sex.factor, FUN=scale)

        phys.act.zscore <- -1*ave(mlta.kcal, epoch, sex.factor, FUN=scale)

        fatigue.zscore <- ave(frail.exhaustion.cont, epoch, sex.factor, FUN=scale)

        bmi.zscore <- -1*ave(bmi, epoch, sex.factor, FUN=scale)

        df <- cbind(frail.grip.average.zscore,gait.speed.zscore,phys.act.zscore,fatigue.zscore,bmi.zscore)
        frailty.composite.score <- apply(df, 1, mean)
        df <- cbind(frail.grip.average.zscore,gait.speed.zscore,phys.act.zscore,fatigue.zscore)
        frailty.composite.score.four <- apply(df,1,mean)

        label(frail.grip.average.zscore) <- "Average Grip Strength over Three Attempts, z-score"
        label(gait.speed.zscore) <- "Gait speed, z-score"
        label(phys.act.zscore) <- "Physical Activity, z-score" 
        label(frail.exhaustion.cont) <- "Fatigue, sum from CESD items"
        label(fatigue.zscore) <- "Fatigue, z-score (from CESD items)"
        label(bmi.zscore) <- "BMI, z-score"
        label(frailty.composite.score) <- "Frailty Composite Score (average of z-scores for 5 components)"
        label(frailty.composite.score.four) <- "Frailty Composite Score (average of z-scores for 4 components)"
        #label(frail.gripbest.zscore) <- "Best Grip Strength of Three Attempts, z-score"
        rm(df,x)
    })

    dat = upData(dat,
        frail.exhaustion.factor = factor(frail.exhaustion,levels=c(1,0),labels=c("Yes","No")),
        frail.walk.factor = factor(frail.walk,levels=c(1,0),labels=c("Yes","No")),
        frail.grip.factor = factor(frail.grip,levels=c(1,0),labels=c("Yes","No")),
        frail.phys.active.factor = factor(frail.phys.active,levels=c(1,0),labels=c("Yes","No")),
        frail.category.factor = factor(frail.category,levels=c(1,0),labels=c("Pre-frail/Frail","Not Frail")),
        frail.ordinal.factor= factor(frail.ordinal,levels=c(2,1,0),labels=c("Frail","Pre-Frail","Not Frail")),
        labels=c(frail.gripbest="Best Grip Strength of Three Attempts",
               frail.grip.average="Average Grip Strength over Three Attempts",
               gripstrength.index="Average Grip Strength Adjusted for BMI",
               gait.speed="Gait Speed in m/s",
               gait.speed.index="Gait Speed in m/s Adjusted for Height",
               frail.exhaustion.cont="Exhaustion, continuous",
               frail.exhaustion.factor = "Patient considered frail based on exhaustion",
               frail.phys.active.factor = "Patient considered frail based on physical activity",
               frail.walk.factor = "Patient considered frail according to walk time",
               frail.grip.factor = "Patient considered frail according to grip strength",
               frail.range="Frailty Range",
               frail.category="Frailty Category",
               frail.ordinal="Frailty Ordinal Score"))

    dat
}
