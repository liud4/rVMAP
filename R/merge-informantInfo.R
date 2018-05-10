informantInfo <- function(dat){
    # Returns dat with informant-information derived variables added

    dat <- within(dat, {
        # convenience var for use in calculation of inform.length.totyrs
        # AJ confirmed in email 23 Feb 2015 that she wants to impute 0 for mos
        # in cases where yrs is nonmissing but mos is missing
        inform.length.imputedmosInYrs <- 
            ifelse(is.na(inform.length.yrs) & is.na(inform.length.mos), NA,
            ifelse(!is.na(inform.length.yrs) & !is.na(inform.length.mos), 
            inform.length.mos, 0)) / 12
    })

    dat$inform.length.totyrs <- 
            round(rowSums(dat[, c("inform.length.yrs", "inform.length.imputedmosInYrs")]), 2)

    dat <- within(dat, {
        label(inform.length.imputedmosInYrs) <-
            "Convenience var for use in calculation of inform.length.totyrs"
        label(inform.length.totyrs) <- "Total yrs informant has known participant (calculated)"
    })

    dat
}


