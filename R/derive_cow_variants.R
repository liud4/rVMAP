#' Derive, label, and add Circle of Willis variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added Circle of Willis variables.
#' @export

derive_cow_variants <- function(data) {
  data <- data %>%
    mutate(
      cow.variant.archive1 = case_when( # previous: cow.variant2; CoW variant Rivera-Rivera; Variant A1s or P1s
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, Textbook
        cow.variant %in% c(10, 11, 21, 22, 25, 26, 29, 30) ~ 1, # 1, A1 variant unilateral
        cow.variant %in% c(12, 13, 14, 23, 24, 27, 28, 31, 32, 35) ~ 2, # 2, P1 variant unilateral
        cow.variant %in% c(15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38) ~ 3, # 3, Other
        TRUE ~ NA_real_
      ),
      cow.variant.missing.a1.p1 = case_when( # previous: cow.variant3; CoW variant Ryan; Missing A1s or P1s
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Patent A1s and P1s
        (cow.a1.l %in% 3 | cow.a1.r %in% 3) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, A1 missing unilateral/bilateral
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & (cow.p1.l %in% 3 | cow.p1.r %in% 3) ~ 2, # 2, P1 missing unilateral/bilateral
        (cow.a1.l %in% 3 | cow.a1.r %in% 3) & (cow.p1.l %in% 3 | cow.p1.r %in% 3) ~ 3, # 3, A1 and P1 missing unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.missing.acoa.pcoa = case_when( # previous: cow.variant4; CoW variant Vrselja; Missing AcoA or PcoAs
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 0, # 0, Patent AcoA and PcoAs
        cow.acoa %in% 3 & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 1, # 1, AcoA missing
        cow.acoa %in% c(1, 2) & (cow.pcoa.l %in% 3 | cow.pcoa.r %in% 3) ~ 2, # 2, PcoA missing unilateral/bilateral
        cow.acoa %in% 3 & (cow.pcoa.l %in% 3 | cow.pcoa.r %in% 3) ~ 3, # 3, AcoA and PcoA missing unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.missing.acoa.pcoa.ub = case_when( # new variable; Missing AcoA or PcoAs, unilateral vs. bilateral
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 0, # 0, Patent AcoA and PcoAs
        cow.acoa %in% 3 & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 1, # 1, AcoA missing
        cow.acoa %in% c(1, 2) & ((cow.pcoa.l %in% 3 & cow.pcoa.r %nin% 3) | (cow.pcoa.l %nin% 3 & cow.pcoa.r %in% 3)) ~ 2, # 2, PcoA missing unilateral
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% 3 & cow.pcoa.r %in% 3 ~ 3, # 3, PcoA missing bilateral
        cow.acoa %in% 3 & ((cow.pcoa.l %in% 3 & cow.pcoa.r %nin% 3) |  (cow.pcoa.l %nin% 3 & cow.pcoa.r %in% 3)) ~ 4, # 4, AcoA and PcoA missing unilateral
        cow.acoa %in% 3 & cow.pcoa.l %in% 3 & cow.pcoa.r %in% 3 ~ 5, # 5, AcoA and PcoA missing bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.archive2 = case_when( # previous: cow.variant5; Variant A1s or P1s, left vs. right, grouping Bilateral P1 with Left P1
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, Textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 variant
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 variant
        cow.variant %in% c(12, 14, 23, 28, 32, 35) ~ 3, # 3, Left P1 variant (with bilateral)
        cow.variant %in% c(13, 24, 15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38, 27, 31) ~ 4, # 4, Other
        TRUE ~ NA_real_
      ),
      cow.variant.archive3 = case_when( # previous: cow.variant6; Variant A1s or P1s, left vs. right, grouping Bilateral P1 with Right P1
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, Textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 variant
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 variant
        cow.variant %in% c(13, 14, 24, 27, 31, 35) ~ 3, # 3, Right P1 variant (with bilateral)
        cow.variant %in% c(12, 23, 15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38, 28, 32) ~ 4, # 4, Other
        TRUE ~ NA_real_
      ),
      cow.variant.archive4 = case_when( # previous: cow.variant7; Variant A1s or P1s, left vs. right, grouping Bilateral P1 separately
        cow.variant %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 0, # 0, Textbook
        cow.variant %in% c(10, 21, 25, 29) ~ 1, # 1, Left A1 variant
        cow.variant %in% c(11, 22, 26, 30) ~ 2, # 2, Right A1 variant
        cow.variant %in% c(12, 23, 28, 32) ~ 3, # 3, Left P1 variant (without bilateral)
        cow.variant %in% c(13, 24, 27, 31) ~ 4, # 4, Right P1 variant (without bilateral)
        cow.variant %in% c(14, 35) ~ 5, # 5, Bilateral P1 variant
        cow.variant %in% c(15, 16, 17, 18, 19, 20, 33, 34, 36, 37, 38) ~ 6, # 6, Other
        TRUE ~ NA_real_
      ),
      cow.variant.archive5 = case_when( # previous: cow.variant8; Missing A1s or P1s, left vs. right, grouping Bilateral with Left P1
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Patent A1s and P1s
        cow.a1.l %in% 3 & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, Left A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% 3 & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 2, # 2, Right A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% 3 & cow.p1.r %in% c(1, 2, 3) ~ 3, # 3, Left P1 missing (with bilateral)
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% 3 ~ 4, # 4, Right P1 missing (without bilateral)
        TRUE ~ NA_real_
      ),
      cow.variant.archive6 = case_when( # previous: cow.variant9; Missing A1s or P1s, left vs. right, grouping Bilateral with Right P1
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Patent A1s and P1s
        cow.a1.l %in% 3 & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 1, # 1, Left A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% 3 & cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 2, # 2, Right A1 missing
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% 3 & cow.p1.r %in% c(1, 2) ~ 3, # 3, Left P1 missing (without bilateral)
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) & cow.p1.l %in% c(1, 2, 3) & cow.p1.r %in% 3 ~ 4, # 4, Right P1 missing (with bilateral)
        TRUE ~ NA_real_
      ),
      cow.variant.missing.acoa.pcoa.lr = case_when( # previous: cow.variant10; Missing AcoA or PcoAs, left vs. right
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 0, # 0, Patent AcoA and PcoAs
        cow.acoa %in% 3 & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% c(1, 2) ~ 1, # 1, AcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% 3 & cow.pcoa.r %in% c(1, 2) ~ 2, # 2, Left PcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% 3 ~ 3, # 3, Right PcoA missing
        cow.acoa %in% 3 & cow.pcoa.l %in% 3 & cow.pcoa.r %in% c(1, 2) ~ 4, # 4, AcoA and Left PcoA missing
        cow.acoa %in% 3 & cow.pcoa.l %in% c(1, 2) & cow.pcoa.r %in% 3 ~ 5, # 5, AcoA and Right PcoA missing
        cow.acoa %in% c(1, 2) & cow.pcoa.l %in% 3 & cow.pcoa.r %in% 3 ~ 6, # 6, Left PcoA and Right PcoA missing
        cow.acoa %in% 3 & cow.pcoa.l %in% 3 & cow.pcoa.r %in% 3 ~ 7, # 7, Left PcoA, Right PcoA and AcoA missing
        TRUE ~ NA_real_
      ),
      cow.variant.missing.a1 = case_when( # previous: cow.variant11; Missing A1s
        cow.a1.l %in% c(1, 2) & cow.a1.r %in% c(1, 2) ~ 0, # 0, Patent A1s
        cow.a1.l %in% 3 | cow.a1.r %in% 3 ~ 1, # 1, A1 Missing unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.missing.p1 = case_when( # previous: cow.variant12; Missing P1s (Full FTP)
        cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Patent P1s
        cow.p1.l %in% 3 | cow.p1.r %in% 3 ~ 1, # 1, P1 Missing unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.archive7 = case_when( # previous: cow.variant13; Missing Left P1
        cow.p1.l %in% c(1, 2) ~ 0, # 0, Pantent Left P1
        cow.p1.l %in% 3 ~ 1, # 1, Left P1 Missing
        TRUE ~ NA_real_
      ),
      cow.variant.hypoplastic.acoa.pcoa = case_when( # previous: cow.variant14; Hypoplastic AcoA or PcoAs
        cow.acoa %in% 1 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 0, # 0, Normal AcoA and PcoAs
        cow.acoa %in% 2 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 1, # 1, AcoA hypoplasia
        cow.acoa %in% 1 & ((cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1) | (cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2) | (cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2)) ~ 2, 
        # 2, PcoA hypoplasia unilateral/bilateral
        cow.acoa %in% 2 & ((cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1) | (cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2) | (cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2)) ~ 3, 
        # 3, AcoA and PcoA hypoplasia unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.hypoplastic.acoa.pcoa.ub = case_when( # new variable; Hypoolastic AcoA or PcoAs, unilateral vs. bilateral
        cow.acoa %in% 1 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 0, # 0, Normal AcoA and PcoAs
        cow.acoa %in% 2 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 1, # 1, AcoA hypoplasia
        cow.acoa %in% 1 & ((cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1) | (cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2)) ~ 2, # 2, PcoA hypoplasia unilateral
        cow.acoa %in% 1 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2 ~ 3, # 3, PcoA hypoplasia bilateral
        cow.acoa %in% 2 & ((cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1) | (cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2)) ~ 4, # 4, AcoA hypoplasia and PcoA hypoplasia unilateral
        cow.acoa %in% 2 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2 ~ 5, # 5, AcoA hypoplasia and PcoA hypoplasia bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.hypoplastic.acoa.pcoa.lr = case_when( # previous: cow.variant15; Hypoplastic AcoA or PcoAs, left vs. right
        cow.acoa %in% 1 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 0, # 0, Normal AcoA and PcoAs
        cow.acoa %in% 2 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 1, # 1, AcoA hypoplasia
        cow.acoa %in% 1 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1 ~ 2, # 2, Left PcoA hypoplasia
        cow.acoa %in% 1 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2 ~ 3, # 3, Right PcoA hypoplasia
        cow.acoa %in% 2 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 1 ~ 4, # 4, Left PcoA and AcoA hypoplasia
        cow.acoa %in% 2 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 2 ~ 5, # 5, Right PcoA and AcoA hypoplasia
        cow.acoa %in% 1 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2 ~ 6, # 6, Left PcoA and Right PcoA hypoplasia
        cow.acoa %in% 2 & cow.pcoa.l %in% 2 & cow.pcoa.r %in% 2 ~ 7, # 7, Left PcoA, Right PcoA, and AcoA hypoplasia
        TRUE ~ NA_real_
      ),
      cow.variant.missing.hypoplastic.acoa.pcoa = case_when( # previous: cow.variant16; Missing or Hypoplastic AcoA or PcoAs
        cow.acoa %in% 1 & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 0, # 0, Normal AcoA and PcoAs
        cow.acoa %in% c(2, 3) & cow.pcoa.l %in% 1 & cow.pcoa.r %in% 1 ~ 1, # 1, AcoA hypoplasia or missing
        cow.acoa %in% 1 & (cow.pcoa.l %in% c(2, 3) | cow.pcoa.r %in% c(2, 3)) ~ 2, # 2, PcoA hypoplasia or missing unilateral/bilateral
        cow.acoa %in% c(2, 3) & (cow.pcoa.l %in% c(2, 3) | cow.pcoa.r %in% c(2, 3)) ~ 3, # 3, AcoA and PcoA missing or hypoplasia unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.missing.p1.ub = case_when( # previous: cow.variant17; Missing P1s, unilateral vs. bilateral
        cow.p1.l %in% c(1, 2) & cow.p1.r %in% c(1, 2) ~ 0, # 0, Normal circle
        (cow.p1.l %in% 3 & cow.p1.r %in% c(1, 2)) | (cow.p1.l %in% c(1, 2) & cow.p1.r %in% 3) ~ 1, # 1, P1 missing unilateral
        cow.p1.l %in% 3 & cow.p1.r %in% 3 ~ 0, # 2, P1 missing bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.partialftp.ub = case_when( # previous: cow.variant18; Partial FTP, unilateral vs. bilateral
        cow.p1.l %in% 1 & cow.p1.r %in% 1 ~ 0, # 0, Normal circle
        (cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 1) | (cow.p1.r %in% 2 & cow.pcoa.r %in% 1 & cow.p1.l %in% 1) ~ 1, # 1, Partial FTP unilateral
        cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 2 & cow.pcoa.r %in% 1 ~ 2, # 2, Partial FTP bilateral 
        TRUE ~ NA_real_
      ),
      cow.variant.partialftp = case_when( # previous: cow.variant19; Partial FTP
        cow.p1.l %in% 1 & cow.p1.r %in% 1 ~ 0, # 0, Normal circle
        (cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 1) | (cow.p1.r %in% 2 & cow.pcoa.r %in% 1 & cow.p1.l %in% 1) | 
          (cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 2 & cow.pcoa.r %in% 1) ~ 1, # 1, Partial FTP unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.missing.p1.partialftp = case_when( # previous: cow.variant20; Full or Partial FTP 
        cow.p1.l %in% 1 & cow.p1.r %in% 1 ~ 0, # 0, Normal circle
        cow.p1.l %in% 3 | cow.p1.r %in% 3 ~ 1, # 1, Full FTP unilateral/bilateral or Partial FTP unilateral/bilateral
        cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 1 ~ 1, # 1, Full FTP unilateral/bilateral or Partial FTP unilateral/bilateral
        cow.p1.r %in% 2 & cow.pcoa.r %in% 1 & cow.p1.l %in% 1 ~ 1, # 1, Full FTP unilateral/bilateral or Partial FTP unilateral/bilateral
        cow.p1.l %in% 2 & cow.pcoa.l %in% 1 & cow.p1.r %in% 2 & cow.pcoa.r %in% 1 ~ 1, # 1, Full FTP unilateral/bilateral or Partial FTP unilateral/bilateral
        TRUE ~ NA_real_
      ),
      cow.variant.typesab = case_when( # previous: cow.variant21; Type A or B 
        (cow.p1.l %in% 3 | cow.p1.r %in% 3) & cow.a1.l %in% 1 & cow.a1.r %in% 1 ~ 0, # 0, Full FTP and Normal A1s
        cow.p1.l %in% 3 & cow.p1.r %in% c(1, 2) & cow.a1.l %in% 1 & cow.a1.r %in% c(2, 3) ~ 1, # 1, Unilateral Full FTP and Absent/Hypoplastic A1 Contralateral (Type A)
        cow.p1.l %in% c(1, 2) & cow.p1.r %in% 3 & cow.a1.l %in% c(2, 3) & cow.a1.r %in% 1 ~ 1, # 1, Unilateral Full FTP and Absent/Hypoplastic A1 Contralateral (Type A)
        cow.p1.l %in% 3 & cow.p1.r %in% c(1, 2) & cow.a1.l %in% c(2, 3) & cow.a1.r %in% 1 ~ 2, # 2, Unilateral Full FTP and Absent/Hypoplastic A1 Ipsilateral (Type B)
        cow.p1.l %in% c(1, 2) & cow.p1.r %in% 3 & cow.a1.l %in% 1 & cow.a1.r %in% c(2, 3) ~ 2, # 2, Unilateral Full FTP and Absent/Hypoplastic A1 Ipsilateral (Type B)
        (cow.p1.l %in% 3 & cow.p1.r %in% 3) & (cow.a1.l %in% c(2, 3) | cow.a1.r %in% c(2, 3)) ~ 3, # 3, Bilateral Full FTP and Absent/Hypoplastic A1
        TRUE ~ NA_real_
      )
    )

  data <- within(data, {
    cow.variant.archive1 <- factor(
      cow.variant.archive1,
      levels = c(0, 1, 2, 3),
      labels = c(
        "textbook",
        "A1 variant unilateral",
        "P1 variant unilateral/bilateral",
        "Other"
      )
    )
    label(cow.variant.archive1) <- 'CoW variant Rivera-Rivera; Variant A1s or P1s'
    
    cow.variant.missing.a1.p1 <- factor(
      cow.variant.missing.a1.p1,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Patent A1s and P1s",
        "A1 missing unilateral/bilateral",
        "P1 missing unilateral/bilateral",
        "A1 and P1 missing unilateral/bilateral"
      )
    )
    label(cow.variant.missing.a1.p1) <- 'CoW variant Ryan; Missing A1s or P1s'
    
    cow.variant.missing.acoa.pcoa  <- factor(
      cow.variant.missing.acoa.pcoa ,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Patent AcoA and PcoAs",
        "AcoA missing",
        "PcoA missing unilateral/bilateral",
        "AcoA and PcoA missing unilateral/bilateral"
      )
    )
    label(cow.variant.missing.acoa.pcoa) <- 'CoW variant Vrselja; Missing AcoA or PcoAs'
    
    cow.variant.missing.acoa.pcoa.ub  <- factor(
      cow.variant.missing.acoa.pcoa.ub ,
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c(
        "Patent AcoA and PcoAs",
        "AcoA missing",
        "PcoA missing unilateral",
        "PcoA missing bilateral",
        "AcoA missing and PcoA missing unilateral",
        "AcoA missing and PcoA missing bilateral"
      )
    )
    label(cow.variant.missing.acoa.pcoa.ub) <- 'Missing AcoA or PcoAs, unilateral vs. bilateral'
    
    cow.variant.archive2 <- factor(
      cow.variant.archive2 ,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "textbook",
        "Left A1 variant",
        "Right A1 variant",
        "Left P1 variant (with bilateral)",
        "Other"
      )
    )
    label(cow.variant.archive2) <- 'Variant A1s or P1s, left vs. right, grouping Bilateral P1 with Left P1'
    
    cow.variant.archive3 <- factor(
      cow.variant.archive3,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "textbook",
        "Left A1 variant",
        "Right A1 variant",
        "Right P1 variant (with bilateral)",
        "Other"
      )
    )
    label(cow.variant.archive3) <- 'Variant A1s or P1s, left vs. right, grouping Bilateral P1 with Right P1'
    
    cow.variant.archive4 <- factor(
      cow.variant.archive4,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c(
        "textbook",
        "Left A1 variant",
        "Right A1 variant",
        "Left P1 variant (without bilateral)",
        "Right P1 variant (without bilateral)",
        "Bilateral P1 variant",
        "Other"
      )
    )
    label(cow.variant.archive4) <- 'Variant A1s or P1s, left vs. right, grouping Bilateral P1 separately'
    
    cow.variant.archive5  <- factor(
      cow.variant.archive5 ,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "Patent A1s and P1s",
        "Left A1 missing",
        "Right A1 missing",
        "Left P1 missing (with bilateral)",
        "Right P1 missing (without bilateral)"
      )
    )
    label(cow.variant.archive5) <- 'Missing A1s or P1s, left vs. right, grouping Bilateral P1 with Left P1'
    
    cow.variant.archive6 <- factor(
      cow.variant.archive6,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "Patent A1s and P1s",
        "Left A1 missing",
        "Right A1 missing",
        "Left P1 missing (without bilateral)",
        "Right P1 missing (with bilateral)"
      )
    )
    label(cow.variant.archive6) <- 'Missing A1s or P1s, left vs. right, grouping Bilateral P1 with Right P1'
    
    cow.variant.missing.acoa.pcoa.lr <- factor(
      cow.variant.missing.acoa.pcoa.lr,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7),
      labels = c(
        "Patent AcoA and PcoAs",
        "AcoA missing",
        "Left PcoA missing",
        "Right PcoA missing",
        "AcoA missing and Left PcoA",
        "AcoA missing and Right PcoA",
        "Left PcoA and Right PcoA missing",
        "Left PcoA, Right PcoA, and AcoA missing"
      )
    )
    label(cow.variant.missing.acoa.pcoa.lr) <- 'Missing AcoA or PcoAs, left vs. right'
    
    cow.variant.missing.a1 <- factor(
      cow.variant.missing.a1,
      levels = c(0, 1),
      labels = c("Patent A1s", "A1 missing unilateral/bilateral")
    )
    label(cow.variant.missing.a1) <- 'Missing A1s'
    
    cow.variant.missing.p1 <- factor(
      cow.variant.missing.p1,
      levels = c(0, 1),
      labels = c("Patent P1s", "P1 missing unilateral/bilateral")
    )
    label(cow.variant.missing.p1) <- 'Missing P1s'
    
    cow.variant.archive7 <- factor(
      cow.variant.archive7,
      levels = c(0, 1),
      labels = c("Patent Left P1", "Left P1 missing")
    )
    label(cow.variant.archive7) <- 'Missing Left P1'
    
    cow.variant.hypoplastic.acoa.pcoa <- factor(
      cow.variant.hypoplastic.acoa.pcoa,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Normal AcoA and PcoAs",
        "AcoA hypoplasia",
        "PcoA hypoplasia unilateral/bilateral",
        "AcoA and PcoA hypoplasia unilateral/bilateral"
      )
    )
    label(cow.variant.hypoplastic.acoa.pcoa) <- 'Hypoplastic AcoA or PcoAs'
    
    cow.variant.hypoplastic.acoa.pcoa.ub <- factor(
      cow.variant.hypoplastic.acoa.pcoa.ub,
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c(
        "Normal AcoA and PcoAs",
        "AcoA hypoplasia",
        "PcoA hypoplasia unilateral",
        "PcoA hypoplasia bilateral",
        "AcoA hypoplasia and PcoA hypoplasia unilateral",
        "AcoA hypoplasia and PcoA hypoplasia bilateral"
      )
    )
    label(cow.variant.hypoplastic.acoa.pcoa) <- 'Hypoplastic AcoA or PcoAs, unilateral vs. bilateral'
    
    cow.variant.hypoplastic.acoa.pcoa.lr <- factor(
      cow.variant.hypoplastic.acoa.pcoa.lr,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7),
      labels = c(
        "Normal AcoA and PcoAs",
        "AcoA hypoplasia",
        "Left PcoA hypoplasia",
        "Right PcoA hypoplasia",
        "Left PcoA and AcoA hypoplasia",
        "Right PcoA and AcoA hypoplasia",
        "Left PcoA and Right PcoA hypoplasia",
        "Left PcoA, Right PcoA, and AcoA hypoplasia"
      )
    )
    label(cow.variant.hypoplastic.acoa.pcoa.lr) <- 'Hypoplastic AcoA or PcoAs, left vs. right'
    
    cow.variant.missing.hypoplastic.acoa.pcoa <- factor(
      cow.variant.missing.hypoplastic.acoa.pcoa,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Normal AcoA and PcoAs",
        "AcoA missing or hypoplasia",
        "PcoA missing or hypoplasia unilateral/bilateral",
        "AcoA and PcoA missing or hypoplasia unilateral/bilateral"
      )
    )
    label(cow.variant.missing.hypoplastic.acoa.pcoa) <- 'Missing or Hypoplastic AcoA or PcoAs'
    
    cow.variant.missing.p1.ub <- factor(
      cow.variant.missing.p1.ub,
      levels = c(0, 1, 2),
      labels = c(
        "Normal circle",
        "P1 missing unilateral",
        "P1 missing bilateral"
      )
    )
    label(cow.variant.missing.p1.ub) <- 'Missing P1s, unilateral vs. bilateral'
    
    cow.variant.partialftp.ub <- factor(
      cow.variant.partialftp.ub,
      levels = c(0, 1, 2),
      labels = c(
        "Normal circle",
        "Partial FTP unilateral",
        "Partial FTP bilateral"
      )
    )
    label(cow.variant.partialftp) <- 'Partial FTP, unilateral vs. bilateral'
    
    cow.variant.partialftp <- factor(
      cow.variant.partialftp,
      levels = c(0, 1),
      labels = c("Normal circle", "Partial FTP unilateral/bilateral")
    )
    label(cow.variant.partialftp.ub) <- 'Partial FTP'
    
    cow.variant.missing.p1.partialftp <- factor(
      cow.variant.missing.p1.partialftp,
      levels = c(0, 1),
      labels = c(
        "Normal circle",
        "Full FTP unilateral/bilateral or Partial FTP unilateral/bilateral"
      )
    )
    label(cow.variant.missing.p1.partialftp) <- 'Full or Partial FTP'
    
    cow.variant.typesab <- factor(
      cow.variant.typesab,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Full FTP and Normal A1s",
        "Unilateral Full FTP and Absent/Hypoplastic A1 Contralateral (Type A)",
        "Unilateral Full FTP and Absent/Hypoplastic A1 Ipsilateral (Type B)",
        "Bilateral Full FTP and Absent/Hypoplastic A1"
      )
    )
    label(cow.variant.typesab) <- 'Type A or B'
    
  })
  
  return(data)
}
