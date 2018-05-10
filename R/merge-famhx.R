famhx <- function(dat){
    # Returns dat with family-history derived variables added
    # Code from JN, modified slightly by LS

    fam.hxprob <- paste0("famhx", 
        formatC(1:12, width=2, format= "d", flag= "0"), ".prob")
    fam.hxdx <- paste0("famhx", 
        formatC(1:12, width=2, format= "d", flag= "0"), ".dx")

    # Family History of AD
    dat$familyhxad <-
        ifelse(dat$famhx01 == 1 & 
            apply(dat[, fam.hxprob] == 1, MARGIN= 1, FUN= any) & 
            apply(dat[, fam.hxdx] == 1, MARGIN= 1, FUN= any), 1, 0)
    dat$familyhxad[is.na(dat$familyhxad)] <- 0
    dat$familyhxad[is.na(dat$famhx01)] <- NA

    # Family History of Dementia
    dat$familyhxdementia <-
        ifelse(dat$famhx01 == 1 & 
        apply(dat[, fam.hxprob] == 1, MARGIN= 1, FUN= any), 1, 0)
    dat$familyhxdementia[is.na(dat$familyhxdementia)] <- 0
    dat$familyhxdementia[is.na(dat$famhx01)] <- NA
                                     
    # Family History of Memory Loss
    dat$familyhxmemory <- 
        ifelse(dat$famhx01 == 1 & 
        apply(dat[, fam.hxprob] == 2, MARGIN= 1, FUN= any), 1, 0)
    dat$familyhxmemory[is.na(dat$familyhxmemory)] <- 0
    dat$familyhxmemory[is.na(dat$famhx01)] <- NA

    # Number of Family Members with History of AD
    dat$familyhxad.number <- ifelse(dat[, fam.hxdx] == 1, 1, 0)
    dat$familyhxad.number[is.na(dat$familyhxad.number)] <- 0
    dat$familyhxad.number <- rowSums(dat$familyhxad.number)
    dat$familyhxad.number[is.na(dat$famhx01)] <- NA

    # Number of Family Members with History of Dementia
    dat$familyhxdementia.number <- ifelse(dat[, fam.hxprob] == 1, 1, 0)
    dat$familyhxdementia.number[is.na(dat$familyhxdementia.number)] <- 0
    dat$familyhxdementia.number <- rowSums(dat$familyhxdementia.number)
    dat$familyhxdementia.number[is.na(dat$famhx01)] <- NA

    # Number of Family Members with History of Memory Problems
    dat$familyhxmemory.number <- ifelse(dat[, fam.hxprob] == 2, 1, 0)
    dat$familyhxmemory.number[is.na(dat$familyhxmemory.number)] <- 0
    dat$familyhxmemory.number <- rowSums(dat$familyhxmemory.number)
    dat$familyhxmemory.number[is.na(dat$famhx01)] <- NA

    dat <- within(dat, {
        familyhxad.factor <- factor(familyhxad,
            levels= c(1, 0), labels= c("Yes", "No"))
        familyhxdementia.factor <- factor(familyhxdementia,
            levels= c(1, 0), labels= c("Yes", "No"))
        familyhxmemory.factor <- factor(dat$familyhxmemory,
            levels=c(1, 0), labels= c("Yes", "No"))
    })
    
    dat <- upData(dat, 
        labels=c(
            familyhxad              = "Family Hx of AD",
            familyhxad.factor       = "Family Hx of AD",
            familyhxdementia        = "Family Hx of Dementia",
            familyhxdementia.factor = "Family Hx of Dementia",
            familyhxmemory          = "Family Hx of Memory Loss",
            familyhxmemory.factor   = "Family Hx of Memory Loss",
            familyhxad.number       = "Num. Family Members w/ Diagnosis of AD",
            familyhxdementia.number = "Num. Family Members w/ Dementia",
            familyhxmemory.number   = "Num. Family Members w/ Memory Problems")
        )

    dat
}
