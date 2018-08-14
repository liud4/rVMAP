cowVariants <- function(dat) {

  require(Hmisc)
  require(dplyr)

  dat <- dat %>%
    mutate(
      cow.variant2 = case_when( # derive: cow.variant2, CoW variant Rivera-Rivera
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, textbook
        cow.variant %in% c(10, 11, 21, 22, 25, 26, 29, 30) ~ 1, # 1, A1 hypoplasia
        cow.variant %in% c(12, 13, 23, 24, 27, 28, 31, 32, 35) ~ 2, # 2, P1 hypoplasia
        cow.variant %in% c(14, 15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38) ~ 3, # 3, Other
        TRUE ~ NA_real_
      ),
      cow.variant3 = case_when( # derive: cow.variant3, CoW variant Ryan
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Fully patent A1s and P1s
        (cow.a1.l %in% c(3) | cow.a1.r %in% c(3)) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & (cow.p1.l %in% c(3) | cow.p1.r %in% c(3)) ~ 2, # 2, P1 missing bilateral/unilateral
        TRUE ~ NA_real_
      ),
      cow.variant4 = case_when( # derive: cow.variant4, CoW variant Vrselja
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 0, # 0, Fully patent AcoA and PcoAs
        cow.acoa %in% c(3) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 1, # 1, AcoA missing
        cow.acoa %in% c(1, 2) & (cow.pcoa.l %in% c(3) | cow.pcoa.r %in% c(3)) ~ 2, # 2, PcoA missing bilateral/unilateral
        cow.acoa %in% c(3) & (cow.pcoa.l %in% c(3) | cow.pcoa.r %in% c(3)) ~ 3, # 3, AcoA and PcoA missing bilateral/unilateral
        TRUE ~ NA_real_
      ),
      cow.variant5 = case_when( # derive: cow.variant5, COW variant A1 Left P1 hypoplasia bilateral included
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 hypoplasia
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 hypoplasia
        cow.variant %in% c(12, 14, 23, 28, 32, 35) ~ 3, # 3, Left P1 hypoplasia (with bilateral)
        cow.variant %in% c(13, 24, 15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38, 27, 31) ~ 4, # 4, Other
        TRUE ~ NA_real_
      ),
      cow.variant6 = case_when( # derive: cow.variant5, COW variant A1 Right P1 hypoplasia bilateral included
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 hypoplasia
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 hypoplasia
        cow.variant %in% c(13, 14, 24, 27, 31, 35) ~ 3, # 3, Right P1 hypoplasia (with bilateral)
        cow.variant %in% c(12, 23, 15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38, 28, 32) ~ 4, # 4, Other
        TRUE ~ NA_real_
      ),
      cow.variant7 = case_when( # derive: cow.variant5, COW variant A1 P1 hypoplasia bilateral separate
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 hypoplasia
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 hypoplasia
        cow.variant %in% c(12, 23, 28, 32) ~ 3, # 3, Left P1 hypoplasia (without bilateral)
        cow.variant %in% c(13, 24, 27, 31) ~ 4, # 4, Right P1 hypoplasia (without bilateral)
        cow.variant %in% c(14, 35) ~ 5, # 5, Bilateral P1 hypoplasia
        cow.variant %in% c(15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38) ~ 6, # 6, Other
        TRUE ~ NA_real_
      ),
      cow.variant8 = case_when( # derive: cow.variant8, COW variant A1 Left P1 missing bilateral included
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Fully patent A1s and P1s
        cow.a1.l %in% c(3) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, Left A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(3) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 2, # 2, Right A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(3) & cow.p1.r %in% c(1, 2, 3) ~ 3, # 3, Left P1 missing (with bilateral)
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(3) ~ 4, # 4, Right P1 missing (without bilateral)
        TRUE ~ NA_real_
      ),
      cow.variant9 = case_when( # derive: cow.variant9, COW variant A1 Right P1 missing bilateral included
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Fully patent A1s and P1s
        cow.a1.l %in% c(3) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, Left A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(3) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 2, # 2, Right A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2, 3) & cow.p1.r %in% c(3) ~ 3, # 3, Right P1 missing (with bilateral)
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(3) & cow.p1.r %in% c(1, 2) ~ 4, # 4, Left P1 missing (without bilateral)
        TRUE ~ NA_real_
      ),
      cow.variant10 = case_when( # derive: cow.variant10, COW variant AcoA PcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 0, # 0, Fully patent AcoA and PcoAs
        cow.acoa %in% c(3) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 1, # 1, AcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(3) & cow.pcoa.r %in% c(1, 2) ~ 2, # 2, Left PcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(3) ~ 3, # 3, Right PcoA missing
        cow.acoa %in% c(3) & cow.pcoa.l %in% c(3) & cow.pcoa.r %in% c(1, 2) ~ 4, # 4, AcoA and Left PcoA missing
        cow.acoa %in% c(3) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(3) ~ 5, # 5, AcoA and Right PcoA missing
        cow.acoa %in% c(3) & cow.pcoa.l %in% c(3) & cow.pcoa.r %in% c(3) ~ 6, # 6, AcoA, Left PcoA, and Right PcoA missing
        TRUE ~ NA_real_
      ),
      cow.variant11 = case_when( # derive: cow.variant11, COW variant Missing A1
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2, 3) & cow.p1.r %in% c(1, 2, 3) ~ 0, # 0, Both A1s Present
        (cow.a1.l %in% c(3) | cow.a1.r %in% c(3)) & cow.p1.l %in% c(1, 2, 3) & cow.p1.r %in% c(1, 2, 3) ~ 1, # 1, A1 Missing
        TRUE ~ NA_real_
      ),
      cow.variant12 = case_when( # derive: cow.variant12, COW variant Missing P1
        cow.a1.l %in% c(1, 2, 3) & cow.a1.r %in% c(1, 2, 3) &  cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Both P1s Present
        cow.a1.l %in% c(1, 2, 3) & cow.a1.r %in% c(1, 2, 3) & (cow.p1.l %in% c(3) | cow.p1.r %in% c(3)) ~ 1, # 1, P1 Missing
        TRUE ~ NA_real_
      ),
      cow.variant13 = case_when( # derive: cow.variant13, COW variant Missing P1 Left
        cow.a1.l %in% c(1, 2, 3) & cow.a1.r %in% c(1, 2, 3) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2, 3) ~ 0, # 0, Left P1 Present
        cow.a1.l %in% c(1, 2, 3) & cow.a1.r %in% c(1, 2, 3) & cow.p1.l %in% c(3) & cow.p1.r %in% c(1, 2, 3) ~ 1, # 1, Left P1 Missing
        TRUE ~ NA_real_
      )
    )

  dat <- within(dat, {
    cow.variant2 <- factor(cow.variant2,
                           levels = c(0, 1, 2, 3),
                           labels = c("textbook",
                                      "A1 hypoplasia",
                                      "P1 hypoplasia",
                                      "Other")
    )
    label(cow.variant2) <- 'CoW variant Rivera-Rivera'

    cow.variant3 <- factor(cow.variant3,
                           levels = c(0, 1, 2),
                           labels = c("Fully patent A1s and P1s",
                                      "A1 missing",
                                      "P1 missing bilateral/unilateral")
    )
    label(cow.variant3) <- 'CoW variant Ryan'

    cow.variant4 <- factor(cow.variant4,
                           levels = c(0, 1, 2, 3),
                           labels = c("Fully patent AcoA and PcoAs",
                                      "AcoA missing",
                                      "PcoA missing bilateral/unilateral",
                                      "AcoA and PcoA missing bilateral/unilateral")
    )
    label(cow.variant4) <- 'CoW variant Vrselja'

    cow.variant5 <- factor(cow.variant5,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("textbook",
                                      "Left A1 hypoplasia",
                                      "Right A1 hypoplasia",
                                      "Left P1 hypoplasia (with bilateral)",
                                      "Other")
    )
    label(cow.variant5) <- 'COW variant A1 Left P1 hypoplasia bilateral included'

    cow.variant6 <- factor(cow.variant6,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("textbook",
                                      "Left A1 hypoplasia",
                                      "Right A1 hypoplasia",
                                      "Right P1 hypoplasia (with bilateral)",
                                      "Other")
    )
    label(cow.variant6) <- 'COW variant A1 Right P1 hypoplasia bilateral included'

    cow.variant7 <- factor(cow.variant7,
                           levels = c(0, 1, 2, 3, 4, 5, 6),
                           labels = c("textbook",
                                      "Left A1 hypoplasia",
                                      "Right A1 hypoplasia",
                                      "Left P1 hypoplasia (without bilateral)",
                                      "Right P1 hypoplasia (without bilateral)",
                                      "Bilateral P1 hypoplasia",
                                      "Other")
    )
    label(cow.variant7) <- 'COW variant A1 P1 hypoplasia bilateral separate'

    cow.variant8 <- factor(cow.variant8,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("Fully patent A1s and P1s",
                                      "Left A1 missing",
                                      "Right A1 missing",
                                      "Left P1 missing (with bilateral)",
                                      "Right P1 missing (without bilateral)")
    )
    label(cow.variant8) <- 'COW variant A1 Left P1 missing bilateral included'

    cow.variant9 <- factor(cow.variant9,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("Fully patent A1s and P1s",
                                      "Left A1 missing",
                                      "Right A1 missing",
                                      "Right P1 missing (with bilateral)",
                                      "Left P1 missing (without bilateral)")
    )
    label(cow.variant9) <- 'COW variant A1 Right P1 missing bilateral included'

    cow.variant10 <- factor(cow.variant10,
                            levels = c(0, 1, 2, 3, 4, 5, 6),
                            labels = c("Fully patent AcoA and PcoAs",
                                       "AcoA missing",
                                       "Left PcoA missing",
                                       "Right PcoA missing",
                                       "AcoA and Left PcoA missing",
                                       "AcoA and Right PcoA missing",
                                       "AcoA, Left PcoA, and Right PcoA missing")
    )
    label(cow.variant10) <- 'COW variant AcoA PcoA missing'

    cow.variant11 <- factor(cow.variant11,
                            levels = c(0, 1),
                            labels = c("Both A1s present",
                                       "A1 missing")
    )
    label(cow.variant11) <- 'COW variant Missing A1'

    cow.variant12 <- factor(cow.variant12,
                            levels = c(0, 1),
                            labels = c("Both P1s present",
                                       "P1 missing")
    )
    label(cow.variant12) <- 'COW variant Missing P1'

    cow.variant13 <- factor(cow.variant13,
                            levels = c(0, 1),
                            labels = c("Left P1 present",
                                       "Left P1 missing")
    )
    label(cow.variant13) <- 'COW variant Missing P1 Left'
  })

  dat
}
