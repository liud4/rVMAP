#' Derive, label, and add cognitive complaint, FCADL, and FAQ variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added cognitive complaint, FCADL, and FAQ variables.
#' @export

derive_cognitive_complaint <- function(data) {
  # Returns data with cognitive-complaint derived variables added
  # and with some totals that were calculated in REDCap recalculated
  # also: fcadl and faq
  # original request from KG, 06 Aug 2014

  # Make sure to source "scoringFunctions.R" before calling this function


  # we will use these below
  revSuffix <- ".r" # what we will append to reverse-coded vars
  revPhrase <- "Reverse coding of " # what we will prepend to label


  ################################################################
  ## faq:  in meeting 09 Mar 2015, KG says it goes w/ complaint items
  faqvars <- paste0("faq", formatC(1:10, width = 2, flag = "0"))
  # 02 Mar 2015 (meeting): AJ and KG want to check IDs w/ any FAQ
  # items missing
  data$faq <- apply(data[, faqvars], 1, totscore, threshold = 1)
  label(data$faq) = "FAQ Total Score, recalculated"
  ################################################################


  ################################################################
  ## fcadl:  in meeting 09 Mar 2015, KG says it goes w/ complaint itemaa
  # in email 04 Mar 2015, AJ says to apply 85% rule
  fcadlvars <- paste0("fcadl", formatC(1:50, width = 2, format = "d", flag = "0"))
  data$fcadl <- apply(data[, fcadlvars], 1, totscore)

  label(data$fcadl) <- "FCADL total score, recalculated"

  ################################################################

  ################################################################
  ## scc: Cognitive Complaint Questionnaire Self-Report Short Form
  # High is bad for all items in scc.self, so we leave all as is
  scc <- Hmisc::Cs(
    scc01, scc02, scc03, scc04, scc05,
    scc06, scc07, scc08, scc09, scc10
  )

  # I couldn't get 'within' to work here
  data$scc.pnm <- apply(data[, scc], 1, propNonMissing)
  data$scc.tot <- apply(data[, scc], 1, totscore)
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

  data$ccqself.pnm <- apply(data[, ccqself], 1, propNonMissing)
  data$ccqself.tot <- apply(data[, ccqself], 1, totscore)

  # For the short scale, all items are 01, non-reversed
  ccqself.short <- Hmisc::Cs(
    ccqself01, ccqself03, ccqself05, ccqself11, ccqself15,
    ccqself25, ccqself27, ccqself32, ccqself33, ccqself37
  )
  data$ccqself.short.pnm <- apply(data[, ccqself.short], 1, propNonMissing)
  data$ccqself.short.tot <- apply(data[, ccqself.short], 1, totscore)
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

  data$cogdif.pnm <- apply(data[, cogdif], 1, propNonMissing)
  data$cogdif.tot <- apply(data[, cogdif], 1, totscore)
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
    data[, paste0(vname, revSuffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, revSuffix)]) <- paste0(revPhrase, label(data[, vname]))
  }

  mfq.freqforget <- paste0(mfq.freqforgetToReverse, revSuffix)

  for (vname in mfq.seriousforgetToReverse) {
    data[, paste0(vname, revSuffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, revSuffix)]) <- paste0(revPhrase, label(data[, vname]))
  }

  mfq.seriousforget <- paste0(mfq.seriousforgetToReverse, revSuffix)

  for(vname in mfq.retroToReverse) {
    data[, paste0(vname, revSuffix)] <- reverse1to7(data[, vname])
    label(data[, paste0(vname, revSuffix)]) <- paste0(revPhrase, label(data[, vname]))
  }

  mfq.retro <- paste0(mfq.retroToReverse, revSuffix)

  mfq <- c(mfq.freqforget, mfq.seriousforget, mfq.retro)

  # some of these, or similar versions of some of these,
  # are auto-calculated by REDCap.
  # KG confirmed on 06 Aug 2014 that the REDCap formulas are wrong.
  # I'm renaming the redcap vars whose names match names I'm using.
  data$mfq.freqforget.pnm <- apply(data[, mfq.freqforget], 1, propNonMissing)
  data$mfq.freqforget.tot <- apply(data[, mfq.freqforget], 1, totscore)
  data$mfq.freqforget.avg <- apply(data[, mfq.freqforget], 1, avgscore)

  data$mfq.seriousforget.pnm <- apply(data[, mfq.seriousforget], 1, propNonMissing)
  data$mfq.seriousforget.tot <- apply(data[, mfq.seriousforget], 1, totscore)
  data$mfq.seriousforget.avg <- apply(data[, mfq.seriousforget], 1, avgscore)

  #data$mfq.mnemonics.pnm <- apply(data[, mfq.mnemonics], 1, propNonMissing)
  #data$mfq.mnemonics.tot <- apply(data[, mfq.mnemonics], 1, totscore)
  #data$mfq.mnemonics.avg <- apply(data[, mfq.mnemonics], 1, avgscore)

  data$mfq.retro.pnm <- apply(data[, mfq.retro], 1, propNonMissing)
  data$mfq.retro.tot <- apply(data[, mfq.retro], 1, totscore)
  data$mfq.retro.avg <- apply(data[, mfq.retro], 1, avgscore)

  # See 7 Aug 2014 email from KG: use the individual items rather than the
  # subscales to calculate the total score for MFQ
  data$mfq.pnm <- apply(data[, mfq], 1, propNonMissing)
  data$mfq.tot <- apply(data[, mfq], 1, totscore)
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

  data$ecogself.pnm <- apply(data[, ecogself], 1, propNonMissing)
  data$ecogself.tot <- apply(data[, ecogself], 1, totscore)
  data$ecogself.mem.pnm <- apply(data[, ecogself.mem], 1, propNonMissing)
  data$ecogself.mem.tot <- apply(data[, ecogself.mem], 1, totscore)
  data$ecogself.lg.pnm <- apply(data[, ecogself.lg], 1, propNonMissing)
  data$ecogself.lg.tot <- apply(data[, ecogself.lg], 1, totscore)
  data$ecogself.vs.pnm <- apply(data[, ecogself.vs], 1, propNonMissing)
  data$ecogself.vs.tot <- apply(data[, ecogself.vs], 1, totscore)
  data$ecogself.plan.pnm <- apply(data[, ecogself.plan], 1, propNonMissing)
  data$ecogself.plan.tot <- apply(data[, ecogself.plan], 1, totscore)
  data$ecogself.org.pnm <- apply(data[, ecogself.org], 1, propNonMissing)
  data$ecogself.org.tot <- apply(data[, ecogself.org], 1, totscore)
  data$ecogself.att.pnm <- apply(data[, ecogself.att], 1, propNonMissing)
  data$ecogself.att.tot <- apply(data[, ecogself.att], 1, totscore)
  ################################################################


  ################################################################
  ## Grand total scores
  # calculate them from the scale scores, not from individual items
  data$tot.complaint <- rowSums(data[, Cs(ecogself.tot, mfq.tot, cogdif.tot, ccqself.tot)])
  data$tot.complaint.short <- rowSums(with(data, cbind(ecogself.tot, mfq.tot, cogdif.tot, scc.or.ccqself.short.tot)))

  gifford <- Hmisc::Cs(
    ccqself01,
    ccqself03,
    ccqself05,
    ccqself16,
    ccqself21,
    ccqself31,
    ccqself47,
    ccqself50,
    ccqself52
  )

  #data$gifford.pnm <- apply(data[, gifford], 1, propNonMissing)
  data$tot.complaint.gifford <- apply(data[, gifford], 1, totscore)

  # 24 Aug 2015: new variable from KG
  gifford25 <- Hmisc::Cs(
    ecogself.mem01,
    ccqself01,
    ccqself02,
    ccqself07,
    ccqself12,
    ccqself15,
    ccqself16,
    ccqself20,
    ccqself43,
    cogdif08,
    cogdif09,
    cogdif15,
    cogdif18,
    cogdif19,
    cogdif26,
    mfq02b.r,
    mfq02g.r,
    mfq02j.r,
    mfq02k.r,
    mfq02p.r,
    mfq02r.r,
    ecogself.mem04,
    ecogself.vis01,
    ecogself.org02,
    ecogself.org03
  )

  data$tot.complaint.gifford.25 <- apply(data[, gifford25], 1, totscore)
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
    label(tot.complaint.gifford) <- "Tot Gifford cognitive complaint score"
    label(tot.complaint.gifford.25) <- "Tot Gifford-25 cognitive complaint score"
  })

  return(data)
}