datesDerived <- function(dat){
    # Returns dat with some date difference vars added
    dat <- within(dat, {
        days.np.date.minus.bld.date <- 
            as.numeric(difftime(np.date, bld.date, units= "days"))
        label(days.np.date.minus.bld.date) <- 
            "Days between blood draw and np test (np-bld)"

        days.np.date.minus.abp.date <- 
            as.numeric(difftime(np.date, abp.date, units= "days"))
        label(days.np.date.minus.abp.date) <- 
            "Days between ABP and np test (np-abp)"
    })
    dat
}
