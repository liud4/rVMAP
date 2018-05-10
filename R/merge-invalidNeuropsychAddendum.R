# Changes
# 22 May 2015, LS:
## Added epoch specification to JN's code    
## added .addend suffix

# 19 Sep 2016, LS:
## Making the code a little more flexible in case we ever drop the .addend suffix

invalidNeuropsychAddendum <- function(dat){

    nosuffix <- Cs(np.pvltrecog.m,
        np.pvltrecog.ts,
        np.pvltrecog.tu1,
        np.pvltrecog.ts1,
        np.pvltrecog.sem,
        np.pvltrecog.ur,
        np.pvltrecog.foil,
        np.pvltrecog.falsepos,
        np.pvltrecog.discrim
    )

    withsuffix <- paste0(nosuffix, ".addend")

    x <- intersect(names(dat), c(nosuffix, withsuffix))

    #dat[dat$vmac.id.add == 1137 & dat$epoch == 1, x] <- NA
    # 22 May 2015: Kim confirmed in email
    #   that this is the right map.id
    dat[dat$map.id == 194 & dat$epoch == 1, x] <- NA

    # 22 May 2015: Kim confirmed in email
    #   that this person is not a MAP participant
    #x1 <- paste0(Cs(np.pvlt1,
    #       np.pvlt1.intrus,
    #       np.pvlt1.pers,
    #       np.pvlt1.clust,
    #       np.pvlt2,
    #       np.pvlt2.intrus,
    #       np.pvlt2.pers,
    #       np.pvlt2.clust,
    #       np.pvlt3,
    #       np.pvlt3.intrus,
    #       np.pvlt3.pers,
    #       np.pvlt3.clust,
    #       np.pvlt4,
    #       np.pvlt4.intrus,
    #       np.pvlt4.pers,
    #       np.pvlt4.clust,
    #       np.pvlt5,
    #       np.pvlt5.intrus,
    #       np.pvlt5.pers,
    #       np.pvlt5.clust
    #), ".addend")
    #dat[dat$vmac.id.addend == 2963 & dat$epoch == 1, x1] <- NA

    dat
}
