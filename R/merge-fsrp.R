# 01 Sep 2016, JN
## Added fsrp.minus.age.points 

# 04 April 2017, JN
## Added fsrp.2017 and fsrp.2017.minus.age.points

fsrp <- function(dat){
    # returns dat with the addition of the derived variable "fsrp"
    # as well as several other variables used in fsrp calculation.


    ######################################################
    # FSRP
    # directions are in the MAP Derived Variable Covariate List
    # also see email: On 07/29/2014 11:03 AM, Jefferson, Angela wrote:
    #1.      For men age 86 and older and women age 84 and older, I would assign the maximum # of points given for the upper limit of age
    #2.      For SBP, I would apply the max point value (ten) for SBP values that exceed 205 for men and 216 for women. I would apply the minimum point value (zero) for values that are below 97 for men and below 95 for women.
 
    dat <- within(dat, {
        age.integer <- round(age, 0)

        # sex: 1=Male; 2=Female
        fsrp.age.pts <- 
            ifelse(sex==1 & age.integer>=54 & age.integer<=56, 0, 
            ifelse(sex==1 & age.integer>=57 & age.integer<=59, 1,
            ifelse(sex==1 & age.integer>=60 & age.integer<=62, 2, 
            ifelse(sex==1 & age.integer>=63 & age.integer<=65, 3, 
            ifelse(sex==1 & age.integer>=66 & age.integer<=68, 4,
            ifelse(sex==1 & age.integer>=69 & age.integer<=72, 5, 
            ifelse(sex==1 & age.integer>=73 & age.integer<=75, 6, 
            ifelse(sex==1 & age.integer>=76 & age.integer<=78, 7,
            ifelse(sex==1 & age.integer>=79 & age.integer<=81, 8, 
            ifelse(sex==1 & age.integer>=82 & age.integer<=84, 9,
            ifelse(sex==1 & age.integer>=85, 10,
            ifelse(sex==2 & age.integer>=54 & age.integer<=56, 0, 
            ifelse(sex==2 & age.integer>=57 & age.integer<=59, 1,
            ifelse(sex==2 & age.integer>=60 & age.integer<=62, 2,
            ifelse(sex==2 & age.integer>=63 & age.integer<=64, 3,
            ifelse(sex==2 & age.integer>=65 & age.integer<=67, 4,
            ifelse(sex==2 & age.integer>=68 & age.integer<=70, 5,
            ifelse(sex==2 & age.integer>=71 & age.integer<=73, 6, 
            ifelse(sex==2 & age.integer>=74 & age.integer<=76, 7, 
            ifelse(sex==2 & age.integer>=77 & age.integer<=78, 8,
            ifelse(sex==2 & age.integer>=79 & age.integer<=81, 9,
            ifelse(sex==2 & age.integer>=82, 10, NA))))))))))))))))))))))

      # SBP 
        sbp.integer <- ifelse(is.na(sbp), NA, round(sbp, 0))

        fsrp.sbp.pts <- 
            #ifelse(sex==1 & htnrx==0 & sbp.integer>=97  & sbp.integer<=105, 0, 
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==1 & htnrx==0 & sbp.integer<=105, 0, 
            ifelse(sex==1 & htnrx==0 & sbp.integer>=106 & sbp.integer<=115, 1,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=116 & sbp.integer<=125, 2,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=126 & sbp.integer<=135, 3,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=136 & sbp.integer<=145, 4,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=146 & sbp.integer<=155, 5,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=156 & sbp.integer<=165, 6,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=166 & sbp.integer<=175, 7,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=176 & sbp.integer<=185, 8,
            ifelse(sex==1 & htnrx==0 & sbp.integer>=186 & sbp.integer<=195, 9,
            #ifelse(sex==1 & htnrx==0 & sbp.integer>=196 & sbp.integer<=205, 10,
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==1 & htnrx==0 & sbp.integer>=196, 10,

            #ifelse(sex==1 & htnrx==1 & sbp.integer>=97  & sbp.integer<=105, 0, 
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==1 & htnrx==1 & sbp.integer<=105, 0, 
            ifelse(sex==1 & htnrx==1 & sbp.integer>=106 & sbp.integer<=112, 1,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=113 & sbp.integer<=117, 2,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=118 & sbp.integer<=123, 3,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=124 & sbp.integer<=129, 4,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=130 & sbp.integer<=135, 5,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=136 & sbp.integer<=142, 6,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=143 & sbp.integer<=150, 7,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=151 & sbp.integer<=161, 8,
            ifelse(sex==1 & htnrx==1 & sbp.integer>=162 & sbp.integer<=176, 9,
            #ifelse(sex==1 & htnrx==1 & sbp.integer>=177 & sbp.integer<=205, 10,                                     
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==1 & htnrx==1 & sbp.integer>=177, 10,

            # adding per 29 Jul 2014 email from AJ
            ifelse(sex==2 & htnrx==0 & sbp.integer<=94, 0,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=95  & sbp.integer<=106, 1,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=107 & sbp.integer<=118, 2,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=119 & sbp.integer<=130, 3,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=131 & sbp.integer<=143, 4,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=144 & sbp.integer<=155, 5,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=156 & sbp.integer<=167, 6,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=168 & sbp.integer<=180, 7,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=181 & sbp.integer<=192, 8,
            ifelse(sex==2 & htnrx==0 & sbp.integer>=193 & sbp.integer<=204, 9,
            #ifelse(sex==2 & htnrx==0 & sbp.integer>=205 & sbp.integer<=216, 10,                                    
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==2 & htnrx==0 & sbp.integer>=205, 10,

            # adding per 29 Jul 2014 email from AJ
            ifelse(sex==2 & htnrx==1 & sbp.integer<=94, 0,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=95  & sbp.integer<=106, 1,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=107 & sbp.integer<=113, 2,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=114 & sbp.integer<=119, 3,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=120 & sbp.integer<=125, 4,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=126 & sbp.integer<=131, 5,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=132 & sbp.integer<=139, 6,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=140 & sbp.integer<=148, 7,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=149 & sbp.integer<=160, 8,
            ifelse(sex==2 & htnrx==1 & sbp.integer>=161 & sbp.integer<=204, 9,
            #ifelse(sex==2 & htnrx==1 & sbp.integer>=205 & sbp.integer<=216, 10, 
            # changing per 29 Jul 2014 email from AJ
            ifelse(sex==2 & htnrx==1 & sbp.integer>=205, 10, 
            NA))))))))))))))))))))))))))))))))))))))))))))

        # Diabetes
        # sex: 1=Male; 2=Female
        fsrp.diabetes.pts <- 
            ifelse(sex==1 & diabetes==0, 0, 
            ifelse(sex==1 & diabetes==1, 2,
            ifelse(sex==2 & diabetes==0, 0,
            ifelse(sex==2 & diabetes==1, 3, NA))))

        # Cigarette Smoking: CIGS
        fsrp.cigs.pts <-
            ifelse(sex==1 & currentsmoking==0, 0, 
            ifelse(sex==1 & currentsmoking==1, 3,
            ifelse(sex==2 & currentsmoking==0, 0,
            ifelse(sex==2 & currentsmoking==1, 3, NA))))

        # Cardiovascular hx: CVD 
        fsrp.cvd.pts <- 
            ifelse(sex==1 & cvd==0, 0, 
            ifelse(sex==1 & cvd==1, 4,
            ifelse(sex==2 & cvd==0, 0,
            ifelse(sex==2 & cvd==1, 2, NA))))

        # Atrial fibrillation: AF
        fsrp.afib.pts <- 
            ifelse(sex==1 & afib==0, 0, 
            ifelse(sex==1 & afib==1, 4,
            ifelse(sex==2 & afib==0, 0,
            ifelse(sex==2 & afib==1, 6, NA))))

        # Left ventricular hypertrophy: LVH
        # sex: 1=Male; 2=Female
        fsrp.lvh.pts <-
            ifelse(sex==1 & echo.lvh==0, 0, 
            ifelse(sex==1 & echo.lvh==1, 5,
            ifelse(sex==2 & echo.lvh==0, 0,
            ifelse(sex==2 & echo.lvh==1, 4, NA))))

        fsrp <- apply(cbind(fsrp.age.pts, fsrp.sbp.pts, 
            fsrp.diabetes.pts, fsrp.cigs.pts, fsrp.cvd.pts, 
            fsrp.afib.pts, fsrp.lvh.pts), MARGIN= 1, sum)
        label(fsrp) <- "FSRP"
        
        fsrp.minus.age.points <- fsrp - fsrp.age.pts
        label(fsrp.minus.age.points) <- "FSRP - Age points"
        ####################################################
        
        #### Adding updated FSRP 2017 Framingham Definition
        fsrp.2017 <- ifelse(sex.factor=='Female',
                            0.87938*(age.integer/10) + 0.51127*currentsmoking - 0.03035*cvd + 1.20720*afib + 0.39796*ifelse(age.integer>=65,1,0) + 1.07111*ifelse(age.integer<65 & diabetes==1,1,0) + 0.06565*ifelse(age.integer >= 65 & diabetes==1,1,0) + 0.13085*htnrx + ifelse(htnrx==0,0.11303*(sbp-120)/10,0.17234*(sbp-120)/10),
                            0.49716*(age.integer/10) + 0.47254*currentsmoking + 0.45341*cvd + 0.08064*afib + 0.45426*ifelse(age.integer>=65,1,0) + + 1.35304*ifelse(age.integer<65 & diabetes==1,1,0) + 0.34385*ifelse(age.integer >= 65 & diabetes==1,1,0) + 0.82598*htnrx + ifelse(htnrx==0,0.27323*(sbp-120)/10,0.09793*(sbp-120)/10))
        fsrp.2017.age.pts <- ifelse(sex.factor=='Female', 0.87938*(age.integer/10), 0.49716*(age.integer/10))
        fsrp.2017.minus.age.points <- fsrp.2017-fsrp.2017.age.pts 

        
        ####################################################

    })

    dat
}
