abpqc <- function(dat){
    # Returns dat with additional ABP QC derived variables added
    # (the ones that can't be derived in the ABP processing program
    # because they need the REDCap data)

    dat <- within(dat, {
        # ABP QC:
        abp.within.90d.of.np <- ifelse(is.na(days.np.date.minus.abp.date), NA,
            ifelse(abs(days.np.date.minus.abp.date) <= 90, 1, 0))
        abp.within.90d.of.np.factor <- factor(abp.within.90d.of.np,
            levels= c(1, 0), labels= c("Yes", "No"))
        label(abp.within.90d.of.np.factor) <- label(abp.within.90d.of.np) <-
            "ABP monitoring within 90 days of neuropsych testing"
        
        # the other abp qc indicator:  time.reading.indicator %in% c("Yes")
        abp.passed.QC <- ifelse(is.na(abp.within.90d.of.np.factor) &
            is.na(time.reading.indicator), NA,
            ifelse(abp.within.90d.of.np.factor %in% c("Yes") &
                time.reading.indicator %in% c("Yes"), 1, 0))
        abp.passed.QC.factor <- factor(abp.passed.QC,
            levels= c(1, 0), labels= c("Yes", "No"))
        label(abp.passed.QC.factor) <- label(abp.passed.QC) <-
            "ABP: passed QC"
    })
    dat
}
