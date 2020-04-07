#' Derive, label, and add cognitive complaint, FCADL, and FAQ variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added cognitive complaint, FCADL, and FAQ variables.
#' @export

derive_cognitive_complaint <- function(data) {
  reverse_suffix <- ".r" # what we will append to reverse-coded vars
  reverse_phrase <- "Reverse coding of " # what we will prepend to label

  ################################################################
  ## faq:  in meeting 09 Mar 2015, KG says it goes w/ complaint items
  faqvars <- paste0("faq", formatC(1:10, width = 2, flag = "0"))
  # 02 Mar 2015 (meeting): AJ and KG want to check IDs w/ any FAQ
  # items missing
  data$faq <- apply(data[, faqvars], 1, total_score, threshold = 1)
  label(data$faq) = "FAQ Total Score, recalculated"
  ################################################################


  ################################################################
  ## fcadl:  in meeting 09 Mar 2015, KG says it goes w/ complaint itemaa
  # in email 04 Mar 2015, AJ says to apply 85% rule
  fcadlvars <- paste0("fcadl", formatC(1:50, width = 2, format = "d", flag = "0"))
  data$fcadl <- apply(data[, fcadlvars], 1, total_score)
  label(data$fcadl) <- "FCADL total score, recalculated"
  ################################################################

  ################################################################
  ## scc: Cognitive Complaint Questionnaire Self-Report Short Form
  # High is bad for all items in scc.self, so we leave all as is
  scc <- Hmisc::Cs(
    scc01, scc02, scc03, scc04, scc05,
    scc06, scc07, scc08, scc09, scc10
  )

  data$scc.pnm <- apply(data[, scc], 1, proportion_non_missing)
  data$scc.tot <- apply(data[, scc], 1, total_score)
  ################################################################


  ################################################################
  ## ccqself: Self-Report Cognitive Changes Questionnaire
  ccqself <- Hmisc::Cs(
    ccqself01, ccqself02, ccqself03, ccqself04, ccqself05,
    ccqself06, ccqself07, ccqself08, ccqself09, ccqself10,
    ccqself11, ccqself12, ccqself13, ccqself14, ccqself15,
    ccqself16, ccqself17, ccqself18, ccqself19, ccqself20,
    ccqself21, ccqself22, ccqself23, ccqself24, ccqself25,
    ccqself26, ccqself27, ccqself28, ccqself29, ccqself30,
    ccqself31, ccqself32, ccqself33, ccqself34, ccqself35,
    ccqself36, ccqself37, ccqself38, ccqself39, ccqself40,
    ccqself41, ccqself42, ccqself43, # no 44 or 45
    ccqself46, ccqself47, ccqself48, ccqself49, ccqself50,
    ccqself51, ccqself52, ccqself53, ccqself54, ccqself55,
    ccqself56, ccqself57, ccqself58, ccqself59
  )

  data$ccqself.pnm <- apply(data[, ccqself], 1, proportion_non_missing)
  data$ccqself.tot <- apply(data[, ccqself], 1, total_score)

  # For the short scale, all items are 01, non-reversed
  ccqself.short <- Hmisc::Cs(
    ccqself01, ccqself03, ccqself05, ccqself11, ccqself15,
    ccqself25, ccqself27, ccqself32, ccqself33, ccqself37
  )
  data$ccqself.short.pnm <- apply(data[, ccqself.short], 1, proportion_non_missing)
  data$ccqself.short.tot <- apply(data[, ccqself.short], 1, total_score)
  ################################################################


  ################################################################
  ## ccqinform: Informant Cognitive Changes Questionnaire
  ccqinform <- Hmisc::Cs(
    ccqinform01, ccqinform02, ccqinform03, ccqinform04, ccqinform05,
    ccqinform06, ccqinform07, ccqinform08, ccqinform09, ccqinform10,
    ccqinform11, ccqinform12, ccqinform13, ccqinform14, ccqinform15,
    ccqinform16, ccqinform17, ccqinform18, ccqinform19, ccqinform20,
    ccqinform21, ccqinform22, ccqinform23, ccqinform24, ccqinform25,
    ccqinform26, ccqinform27, ccqinform28, ccqinform29, ccqinform30,
    ccqinform31, ccqinform32, ccqinform33, ccqinform34, ccqinform35,
    ccqinform36, ccqinform37, ccqinform38, ccqinform39, ccqinform40,
    ccqinform41, ccqinform42, ccqinform43, # ccqinform44, ccqinform45, # excluding these to be equivalent to the ccqself calc
    ccqinform46, ccqinform47, ccqinform48, ccqinform49, ccqinform50,
    ccqinform51, ccqinform52, ccqinform53, ccqinform54, ccqinform55,
    ccqinform56, ccqinform57, ccqinform58, ccqinform59
  )

  data$ccqinform.pnm <- apply(data[, ccqinform], 1, proportion_non_missing)
  data$ccqinform.tot <- apply(data[, ccqinform], 1, total_score)

  # For the short scale, all items are 01, non-reversed
  ccqinform.short <- Hmisc::Cs(
    ccqinform01, ccqinform03, ccqinform05, ccqinform11, ccqinform15,
    ccqinform25, ccqinform27, ccqinform32, ccqinform33, ccqinform37
  )
  data$ccqinform.short.pnm <- apply(data[, ccqinform.short], 1, proportion_non_missing)
  data$ccqinform.short.tot <- apply(data[, ccqinform.short], 1, total_score)

  ################################################################

  # 21 Aug 2014: now make a combined "short" var (request from KG)
  data$scc.or.ccqself.short.tot <- ifelse(!is.na(data$scc.tot), data$scc.tot, data$ccqself.short.tot)

  ################################################################
  ## cogdif: Cognitive Difficulties Questionnaire
  # High scores are bad here, so we leave as is
  cogdif <- Hmisc::Cs(
    cogdif01, cogdif02, cogdif03, cogdif04, cogdif05,
    cogdif06, cogdif07, cogdif08, cogdif09, cogdif10,
    cogdif11, cogdif12, cogdif13, cogdif14, cogdif15,
    cogdif16, cogdif17, cogdif18, cogdif19, cogdif20,
    cogdif21, cogdif22, cogdif23, cogdif24, cogdif25,
    cogdif26, cogdif27, cogdif28, cogdif29, cogdif30,
    cogdif31, cogdif32, cogdif33, cogdif34, cogdif35,
    cogdif36, cogdif37, cogdif38, cogdif39, cogdif40,
    cogdif41
  )

  data$cogdif.pnm <- apply(data[, cogdif], 1, proportion_non_missing)
  data$cogdif.tot <- apply(data[, cogdif], 1, total_score)
  ################################################################


  ################################################################
  ## mfq: Memory Functioning Questionnaire
  # scoring is ambiguous in sec 7 (mnemonics), so we omit.
  # high scores are good here in all other sections, so we recode
  mfq.freqforgetToReverse <- Hmisc::Cs(
    mfq01 ,
    mfq02a, mfq02b, mfq02c, mfq02d, mfq02e,
    mfq02f, mfq02g, mfq02h, mfq02i, mfq02j,
    mfq02k, mfq02l, mfq02m, mfq02n, mfq02o,
    mfq02p, mfq02q, mfq02r,
    mfq03a, mfq03b, mfq03c, mfq03d, mfq03e,
    mfq04a, mfq04b, mfq04c, mfq04d, mfq04e,
    mfq05a, mfq05b, mfq05c, mfq05d
  )

  mfq.seriousforgetToReverse <- Hmisc::Cs(
    mfq06a, mfq06b, mfq06c, mfq06d, mfq06e,
    mfq06f, mfq06g, mfq06h, mfq06i, mfq06j,
    mfq06k, mfq06l, mfq06m, mfq06n, mfq06o,
    mfq06p, mfq06q, mfq06r
  )

  # KG says omit, 07 Aug 2014
  #mfq.mnemonics <- Cs(
  #    mfq07a, mfq07b, mfq07c, mfq07d, mfq07e,
  #    mfq07f, mfq07g, mfq07h
  #)

  mfq.retroToReverse <- Hmisc::Cs(
    mfq08a, mfq08b, mfq08c, mfq08d, mfq08e
  )

  for (vname in mfq.freqforgetToReverse) {
    data[, paste0(vname, reverse_suffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, reverse_suffix)]) <- paste0(reverse_phrase, label(data[, vname]))
  }

  mfq.freqforget <- paste0(mfq.freqforgetToReverse, reverse_suffix)

  for (vname in mfq.seriousforgetToReverse) {
    data[, paste0(vname, reverse_suffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, reverse_suffix)]) <- paste0(reverse_phrase, label(data[, vname]))
  }

  mfq.seriousforget <- paste0(mfq.seriousforgetToReverse, reverse_suffix)

  for(vname in mfq.retroToReverse) {
    data[, paste0(vname, reverse_suffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, reverse_suffix)]) <- paste0(reverse_phrase, label(data[, vname]))
  }

  mfq.retro <- paste0(mfq.retroToReverse, reverse_suffix)

  mfq <- c(mfq.freqforget, mfq.seriousforget, mfq.retro)

  # some of these, or similar versions of some of these,
  # are auto-calculated by REDCap.
  # KG confirmed on 06 Aug 2014 that the REDCap formulas are wrong.
  # I'm renaming the redcap vars whose names match names I'm using.
  data$mfq.freqforget.pnm <- apply(data[, mfq.freqforget], 1, proportion_non_missing)
  data$mfq.freqforget.tot <- apply(data[, mfq.freqforget], 1, total_score)
  data$mfq.freqforget.avg <- apply(data[, mfq.freqforget], 1, average_score)

  data$mfq.seriousforget.pnm <- apply(data[, mfq.seriousforget], 1, proportion_non_missing)
  data$mfq.seriousforget.tot <- apply(data[, mfq.seriousforget], 1, total_score)
  data$mfq.seriousforget.avg <- apply(data[, mfq.seriousforget], 1, average_score)

  #data$mfq.mnemonics.pnm <- apply(data[, mfq.mnemonics], 1, proportion_non_missing)
  #data$mfq.mnemonics.tot <- apply(data[, mfq.mnemonics], 1, total_score)
  #data$mfq.mnemonics.avg <- apply(data[, mfq.mnemonics], 1, average_score)

  data$mfq.retro.pnm <- apply(data[, mfq.retro], 1, proportion_non_missing)
  data$mfq.retro.tot <- apply(data[, mfq.retro], 1, total_score)
  data$mfq.retro.avg <- apply(data[, mfq.retro], 1, average_score)

  # See 7 Aug 2014 email from KG: use the individual items rather than the
  # subscales to calculate the total score for MFQ
  data$mfq.pnm <- apply(data[, mfq], 1, proportion_non_missing)
  data$mfq.tot <- apply(data[, mfq], 1, total_score)
  ################################################################

  ################################################################
  ## ecogself: Everyday Cognition
  # High is bad, so we leave all items as is

  ecogself.mem <- Hmisc::Cs(
    ecogself.mem01, ecogself.mem02, ecogself.mem03, ecogself.mem04,
    ecogself.mem05, ecogself.mem06, ecogself.mem07, ecogself.mem08
  )

  ecogself.lg <- Hmisc::Cs(
    ecogself.lang01, ecogself.lang02, ecogself.lang03,
    ecogself.lang04, ecogself.lang05, ecogself.lang06,
    ecogself.lang07, ecogself.lang08, ecogself.lang09
  )

  ecogself.vs <- Hmisc::Cs(
    ecogself.vis01, ecogself.vis02, ecogself.vis03, ecogself.vis04,
    ecogself.vis05, ecogself.vis06, ecogself.vis07
  )

  ecogself.plan <- Hmisc::Cs(
    ecogself.plan01, ecogself.plan02, ecogself.plan03,
    ecogself.plan04, ecogself.plan05
  )

  ecogself.org <- Hmisc::Cs(
    ecogself.org01, ecogself.org02, ecogself.org03,
    ecogself.org04, ecogself.org05, ecogself.org06
  )

  ecogself.att <- Hmisc::Cs(
    ecogself.attn01, ecogself.attn02,
    ecogself.attn03, ecogself.attn04
  )

  ecogself <- c(
    ecogself.mem,
    ecogself.lg,
    ecogself.vs,
    ecogself.plan,
    ecogself.org,
    ecogself.att
  )

  data$ecogself.pnm <- apply(data[, ecogself], 1, proportion_non_missing)
  data$ecogself.tot <- apply(data[, ecogself], 1, total_score)
  data$ecogself.mem.pnm <- apply(data[, ecogself.mem], 1, proportion_non_missing)
  data$ecogself.mem.tot <- apply(data[, ecogself.mem], 1, total_score)
  data$ecogself.lg.pnm <- apply(data[, ecogself.lg], 1, proportion_non_missing)
  data$ecogself.lg.tot <- apply(data[, ecogself.lg], 1, total_score)
  data$ecogself.vs.pnm <- apply(data[, ecogself.vs], 1, proportion_non_missing)
  data$ecogself.vs.tot <- apply(data[, ecogself.vs], 1, total_score)
  data$ecogself.plan.pnm <- apply(data[, ecogself.plan], 1, proportion_non_missing)
  data$ecogself.plan.tot <- apply(data[, ecogself.plan], 1, total_score)
  data$ecogself.org.pnm <- apply(data[, ecogself.org], 1, proportion_non_missing)
  data$ecogself.org.tot <- apply(data[, ecogself.org], 1, total_score)
  data$ecogself.att.pnm <- apply(data[, ecogself.att], 1, proportion_non_missing)
  data$ecogself.att.tot <- apply(data[, ecogself.att], 1, total_score)
  ################################################################

  ################################################################
  ## ecoginf: Informant Everyday Cognition
  # High is bad, so we leave all items as is

  ecoginf.mem <- Hmisc::Cs(
    ecoginf.mem01, ecoginf.mem02, ecoginf.mem03, ecoginf.mem04,
    ecoginf.mem05, ecoginf.mem06, ecoginf.mem07, ecoginf.mem08
  )

  ecoginf.lg <- Hmisc::Cs(
    ecoginf.lang01, ecoginf.lang02, ecoginf.lang03,
    ecoginf.lang04, ecoginf.lang05, ecoginf.lang06,
    ecoginf.lang07, ecoginf.lang08, ecoginf.lang09
  )

  ecoginf.vs <- Hmisc::Cs(
    ecoginf.vis01, ecoginf.vis02, ecoginf.vis03, ecoginf.vis04,
    ecoginf.vis05, ecoginf.vis06, ecoginf.vis07
  )

  ecoginf.plan <- Hmisc::Cs(
    ecoginf.plan01, ecoginf.plan02, ecoginf.plan03,
    ecoginf.plan04, ecoginf.plan05
  )

  ecoginf.org <- Hmisc::Cs(
    ecoginf.org01, ecoginf.org02, ecoginf.org03,
    ecoginf.org04, ecoginf.org05, ecoginf.org06
  )

  ecoginf.att <- Hmisc::Cs(
    ecoginf.attn01, ecoginf.attn02,
    ecoginf.attn03, ecoginf.attn04
  )

  ecoginf <- c(
    ecoginf.mem,
    ecoginf.lg,
    ecoginf.vs,
    ecoginf.plan,
    ecoginf.org,
    ecoginf.att
  )

  data$ecoginf.pnm <- apply(data[, ecoginf], 1, proportion_non_missing)
  data$ecoginf.tot <- apply(data[, ecoginf], 1, total_score)
  data$ecoginf.mem.pnm <- apply(data[, ecoginf.mem], 1, proportion_non_missing)
  data$ecoginf.mem.tot <- apply(data[, ecoginf.mem], 1, total_score)
  data$ecoginf.lg.pnm <- apply(data[, ecoginf.lg], 1, proportion_non_missing)
  data$ecoginf.lg.tot <- apply(data[, ecoginf.lg], 1, total_score)
  data$ecoginf.vs.pnm <- apply(data[, ecoginf.vs], 1, proportion_non_missing)
  data$ecoginf.vs.tot <- apply(data[, ecoginf.vs], 1, total_score)
  data$ecoginf.plan.pnm <- apply(data[, ecoginf.plan], 1, proportion_non_missing)
  data$ecoginf.plan.tot <- apply(data[, ecoginf.plan], 1, total_score)
  data$ecoginf.org.pnm <- apply(data[, ecoginf.org], 1, proportion_non_missing)
  data$ecoginf.org.tot <- apply(data[, ecoginf.org], 1, total_score)
  data$ecoginf.att.pnm <- apply(data[, ecoginf.att], 1, proportion_non_missing)
  data$ecoginf.att.tot <- apply(data[, ecoginf.att], 1, total_score)
  ################################################################


  ################################################################
  ## Grand total scores
  # calculate them from the scale scores, not from individual items
  data$tot.complaint <- rowSums(data[, Cs(ecogself.tot, mfq.tot, cogdif.tot, ccqself.tot)])
  data$tot.complaint.short <- rowSums(with(data, cbind(ecogself.tot, mfq.tot, cogdif.tot, scc.or.ccqself.short.tot)))

  ################################################################
  ## 45 Item Grand total scores # OAK 20181008: https://github.com/liud4/rVMAP/issues/7

  tot.complaint.gifford.45.ef <- Cs(
    cogdif03,
    cogdif10,
    cogdif19,
    cogdif26,
    cogdif36,
    ecogself.plan04,
    ecogself.plan05,
    ecogself.vis01,
    mfq02p.r
  )

  tot.complaint.gifford.45.lang <- Cs(
    cogdif09,
    cogdif14,
    cogdif15,
    ecogself.lang02,
    ecogself.lang03,
    ecogself.lang04,
    ecogself.lang05,
    ecogself.lang07,
    ecogself.lang08,
    ecogself.lang09
  )

  tot.complaint.gifford.45.mem <- Cs(
    ccqself01,
    ccqself07,
    ccqself13,
    ccqself15,
    ccqself16,
    ccqself20,
    ccqself43,
    cogdif06,
    cogdif08,
    cogdif13,
    cogdif18,
    cogdif21,
    cogdif22,
    cogdif32,
    cogdif40,
    ecogself.mem01,
    ecogself.mem02,
    ecogself.mem03,
    ecogself.mem04,
    ecogself.mem05,
    ecogself.mem08,
    ecogself.vis03,
    mfq02b.r,
    mfq02k.r,
    mfq08b.r,
    mfq08e.r
  )

  tot.complaint.gifford.45 <- c(
    tot.complaint.gifford.45.ef,
    tot.complaint.gifford.45.lang,
    tot.complaint.gifford.45.mem
  )

  ## Functions for derivation ##

  # totscore <- function(vec, threshold = 0.85) {
  #   # calc tot score only if >= <threshold> of items are nonmissing
  #   if(proportion_non_missing(vec) < threshold) return(NA) else {
  #     vec[is.na(vec)] <- mean(vec, na.rm= TRUE)
  #     return(round(sum(vec), 1))
  #   }
  # }

  totscore.impute <- function(vec, threshold = 0.85) {
    # calc tot score only if >= <threshold> of items are nonmissing
    if(proportion_non_missing(vec) < threshold) return(vec) else {
      vec[is.na(vec)] <- mean(vec, na.rm= TRUE)
      return(vec)
    }
  }

  VTM <- function(vc, dm) {
    matrix(vc, ncol = length(vc), nrow = dm, byrow = T)
  }

#acquire ranges for each SCD items
  rescale.range<-function(v) {
    out<-matrix(0, ncol=length(v), nrow=2)
    out[, grepl('cogdif', v)]=c(0,4)
    out[, grepl('mfq', v)]=c(1,7)
    out[, grepl('ecogself', v)]=c(1,4)
    out[, grepl('ccqself', v)]=c(0,1)
    out
  }
#scale each SCD item to 0-1 range
  rescale<-function(v, data) {
    r<-rescale.range(v)
    n<-dim(data)[1]
    (data[, v]-VTM(r[1,], n))/VTM(r[2,]-r[1,], n)
  }
#rescale the scaled 0-1 SCD item back to its original scale
  rescale2 <- function(v, data) {
    r<-rescale.range(v)
    n<-dim(data)[1]
    data[, v]*VTM(r[2,]-r[1,], n)+VTM(r[1,], n)
  }

  ## Deriving tot.complaint.gifford.45

  temp.df <- data
  # temp.df <- temp.df %>% # flagged for removal
  #   mutate(
  #     tot.complaint.rescale = rowSums(rescale(Cs(ecogself.tot, mfq.tot, cogdif.tot, ccqself.tot), temp.df))
  #   )

  junk <- t(apply(rescale(tot.complaint.gifford.45, temp.df), MARGIN = 1, totscore.impute))
  junk.rescaled <- rescale2(tot.complaint.gifford.45, junk)

  # data$tot.complaint.rescale <- temp.df$tot.complaint.rescale # flagged for removal

  data$tot.complaint.gifford.45 <- rowSums(junk.rescaled)

  data$tot.complaint.gifford.45.ef <- rowSums(junk.rescaled[, tot.complaint.gifford.45.ef])

  data$tot.complaint.gifford.45.lang <- rowSums(junk.rescaled[, tot.complaint.gifford.45.lang])

  data$tot.complaint.gifford.45.mem <- rowSums(junk.rescaled[, tot.complaint.gifford.45.mem])

  ################################################################

  data <- within(data, {
    label(scc.pnm) <- "Cog Complaint Quest. Self-Report Short Form: Prop. non-miss"
    label(scc.tot) <- "Cog Complaint Quest. Self-Report Short Form: Tot score, re-calc."

    label(ccqself.pnm) <- "Self-Report Cog Changes Quest.- Full Form: Prop. non-miss"
    label(ccqself.tot) <- "Self-Report Cog Changes Quest.- Full Form: Tot score, re-calc."

    label(ccqself.short.pnm) <- "Self-Report Cog Changes Quest.- Short Form: Prop. non-miss"
    label(ccqself.short.tot) <- "Self-Report Cog Changes Quest.- Short Form: Tot score, re-calc."
    label(scc.or.ccqself.short.tot) <- "Tot score from scc if avail. or from ccqself.short if scc not avail."

    label(cogdif.pnm) <- "Cognitive Difficulties Quest.: Prop. non-miss"
    label(cogdif.tot) <- "Cognitive Difficulties Quest.: Tot score, re-calc."

    label(mfq.freqforget.pnm) <- "MFQ- Gen. Freq. of Forgetting: Prop. non-miss"
    label(mfq.freqforget.tot) <- "MFQ- Gen. Freq. of Forgetting: Tot score, re-calc."
    label(mfq.freqforget.avg) <- "MFQ- Gen. Freq. of Forgetting: Average score, re-calc."

    label(mfq.seriousforget.pnm) <- "MFQ- Seriousness of Forgetting: Prop. non-miss"
    label(mfq.seriousforget.tot) <- "MFQ- Seriousness of Forgetting: Tot score, re-calc."
    label(mfq.seriousforget.avg) <- "MFQ- Seriousness of Forgetting: Average score, re-calc."

    #label(mfq.mnemonics.pnm) <- "MFQ - Mnemonics Usage: Prop. non-miss"
    #label(mfq.mnemonics.tot) <- "MFQ - Mnemonics Usage: Tot score, re-calc."
    #label(mfq.mnemonics.avg) <- "MFQ - Mnemonics Usage: Average score, re-calc."

    label(mfq.retro.pnm) <- "MFQ - Retrospective Functioning: Prop. non-miss"
    label(mfq.retro.tot) <- "MFQ - Retrospective Functioning: Tot score, re-calc."
    label(mfq.retro.avg) <- "MFQ - Retrospective Functioning: Average score, re-calc."

    label(mfq.pnm) <- "MFQ (Memory Functioning Quest.): Prop. non-miss, re-calc."
    label(mfq.tot) <- "MFQ (Memory Functioning Quest.): Tot score, re-calc."

    label(ecogself.pnm) <- "Everyday Cognition- Self: Prop. non-miss"
    label(ecogself.tot) <- "Everyday Cognition- Self: Tot score, re-calc."
    label(ecogself.mem.pnm) <- "ECog-Self Memory: Prop. non-miss"
    label(ecogself.mem.tot) <- "ECog-Self Memory: Tot score, re-calc."
    label(ecogself.lg.pnm) <- "ECog-Self Language: Prop. non-miss"
    label(ecogself.lg.tot) <- "ECog-Self Language: Tot score, re-calc."
    label(ecogself.vs.pnm) <- "ECog-Self Visuospatial: Prop. non-miss"
    label(ecogself.vs.tot) <- "ECog-Self Visuospatial: Tot score, re-calc."
    label(ecogself.plan.pnm) <- "ECog-Self Planning: Prop. non-miss"
    label(ecogself.plan.tot) <- "ECog-Self Planning: Tot score, re-calc."
    label(ecogself.org.pnm) <- "ECog-Self Organization: Prop. non-miss"
    label(ecogself.org.tot) <- "ECog-Self Organization: Tot score, re-calc."
    label(ecogself.att.pnm) <- "ECog-Self Attention: Prop. non-miss"
    label(ecogself.att.tot) <- "ECog-Self Attention: Tot score, re-calc."

    label(tot.complaint) <- "Tot cognitive complaint score"
    label(tot.complaint.short) <- "Tot cognitive complaint score using short scale"
    # label(tot.complaint.gifford) <- "Tot Gifford cognitive complaint score"
    # label(tot.complaint.gifford.25) <- "Tot Gifford-25 cognitive complaint score"

    # label(tot.complaint.rescale) <- "Tot cognitive complaint score (rescaled)" # flagged for removal
    label(tot.complaint.gifford.45) <- "Tot Gifford-45 cognitive complaint score"
    label(tot.complaint.gifford.45.ef) <- "Tot Gifford-45 - Executive Functioning Subdomain"
    label(tot.complaint.gifford.45.lang) <- "Tot Gifford-45 - Language Subdomain"
    label(tot.complaint.gifford.45.mem) <- "Tot Gifford-45 - Memory Subdomain"
  })

  return(data)
}
