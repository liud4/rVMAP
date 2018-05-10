# 14 Dec 2016, LS
#   Adding age

demog <- function(dat){
    # Returns dat with demographic derived variables added
    DAYS.IN.YEAR <- 365.25

    dat <- within(dat, {
        raceethnicity <- 
            ifelse(is.na(race) | is.na(ethnicity), NA, 
            ifelse(race == 1 & ethnicity == 0, 1, 
            ifelse((race %in% c(0,2,3,4) & ethnicity %in% c(0,1)) | 
                (race ==1 & ethnicity ==1), 2, NA)))
        raceethnicity.factor <- factor(raceethnicity, levels= 1:2,
            labels= c("Non-Hispanic White", "Other"))
        label(raceethnicity) <- label(raceethnicity.factor) <-
            "Two-level race/ethnicity"

        age.redcap <- age
        # from REDCap: 
        #   rounddown((datediff([dob], [medhx_date], "y")), 0)
        age <- floor(as.numeric(difftime(medhx.date, dob,  
            units= "days")) / DAYS.IN.YEAR)
        label(age) <- "Age at medhx.date, recalculated"

    })
    dat
}
