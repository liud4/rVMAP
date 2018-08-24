#' Add labels and factor levels to the merged data prior to deriving new variables.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added labels and factors.
#' @export

add_main_labels_factors <- function(data) {

  orignames <- names(data)

  label(data$map.id)="MAP ID"
  label(data$vmac.id)="VMAC ID"
  label(data$entry.primary)="Primary Data Entry Person"
  label(data$entry.secondary)="Secondary Data Entry Person"
  label(data$data.entry.complete)="Complete?"
  label(data$visit.flow)="Visit Observations & Notes"
  label(data$visit.brain)="Is there an incidental brain MRI finding?"
  label(data$visit.brain.notes)="Incidental brain MRI finding - Notes"
  label(data$visit.echo)="Is there an incidental echo finding?"
  label(data$visit.echo.notes)="Incidental echo finding - Notes"
  label(data$visit.cmr)="Is there an incidental cardiac MRI finding?"
  label(data$visit.cmr.notes)="Incidental cardiac MRI finding - Notes"
  label(data$visit.depress)="Did participant endorse depressed mood on QIDS or GDS?"
  label(data$visit.depress.note)="Is there a chart note regarding depressed mood?"
  label(data$visit.notes.complete)="Complete?"
  label(data$diagnosis)="Consensus Decision for Diagnosis"
  label(data$diagnosis.date)="Consensus Decision for Diagnosis - Date"
  label(data$nc.type)="NC Type"
  label(data$mci.amnestic)="MCI Amnestic Type"
  label(data$mci.domain)="MCI Domain Type"
  label(data$mci.stage)="MCI Stage"
  label(data$mci.notes)="MCI - form notes"
  label(data$dementia)="Dementia diagnosis"
  label(data$diagnosis.complete)="Complete?"
  label(data$cdr.exam)="CDR Examiner"
  label(data$cdr.date)="CDR - Date of Administration"
  label(data$cdr.mem)="CDR Memory"
  label(data$cdr.orient)="CDR Orientation"
  label(data$cdr.judg)="CDR Judgement & Problem Solving"
  label(data$cdr.affairs)="CDR Community Affairs"
  label(data$cdr.hobbies)="CDR Home & Hobbies"
  label(data$cdr.care)="CDR Personal Care"
  label(data$cdr)="CDR Global Score"
  label(data$cdr.boxes)="CDR Sum of Boxes"
  label(data$cdr.notes)="CDR - form notes"
  label(data$clinical.dementia.rating.complete)="Complete?"
  label(data$faq.date)="FAQ - Date of Administration"
  label(data$faq01)="FAQ 1 - Paying Bills"
  label(data$faq02)="FAQ 2 - Taxes & Business Affairs"
  label(data$faq03)="FAQ 3 - Shopping Alone"
  label(data$faq04)="FAQ 4 - Games & Hobbies"
  label(data$faq05)="FAQ 5 - Using Stove"
  label(data$faq06)="FAQ 6 - Preparing a Balanced Meal"
  label(data$faq07)="FAQ 7 - Current Events"
  label(data$faq08)="FAQ 8 - Paying Attention"
  label(data$faq09)="FAQ 9 - Remembering Dates"
  label(data$faq10)="FAQ 10 - Traveling & Driving"
  label(data$faq.notes)="FAQ - form notes"
  label(data$functional.activities.questionnaire.complete)="Complete?"
  label(data$famhx.date.ptp)="Family history - Date of Acquisition"
  label(data$famhx.date.multiple)="Did the informant have a different Family history Date of Administration?"
  label(data$famhx.inf.date)="Family history - Informant Date of Administration"
  label(data$famhx01.ptp)="Family history of memory problems?"
  label(data$famhx01.relation.ptp)="Family member 1 relationship"
  label(data$famhx01.prob.ptp)="Family member 1 memory problem type"
  label(data$famhx01.dx.ptp)="Family member 1 dementia diagnosis"
  label(data$famhx01.sx.ptp)="Family member 1 memory problem symptoms"
  label(data$famhx01.onset.ptp)="Family member 1 memory problem age of onset"
  label(data$famhx01.age.ptp)="Family member 1 memory problem age of onset"
  label(data$famhx02.ptp)="Additional family member?"
  label(data$famhx02.relation.ptp)="Family member 2 relationship"
  label(data$famhx02.prob.ptp)="Family member 2 memory problem type"
  label(data$famhx02.dx.ptp)="Family member 2 dementia diagnosis"
  label(data$famhx02.sx.ptp)="Family member 2 memory problem symptoms"
  label(data$famhx02.onset.ptp)="Family member 2 memory problem age of onset"
  label(data$famhx02.age.ptp)="Family member 2 memory problem age of onset"
  label(data$famhx03.ptp)="Additional family member?"
  label(data$famhx03.relation.ptp)="Family member 3 relationship"
  label(data$famhx03.prob.ptp)="Family member 3 memory problem type"
  label(data$famhx03.dx.ptp)="Family member 3 dementia diagnosis"
  label(data$famhx03.sx.ptp)="Family member 3 memory problem symptoms"
  label(data$famhx03.onset.ptp)="Family member 3 memory problem age of onset"
  label(data$famhx03.age.ptp)="Family member 3 memory problem age of onset"
  label(data$famhx04.ptp)="Additional family member?"
  label(data$famhx04.relation.ptp)="Family member 4 relationship"
  label(data$famhx04.prob.ptp)="Family member 4 memory problem type"
  label(data$famhx04.dx.ptp)="Family member 4 dementia diagnosis"
  label(data$famhx04.sx.ptp)="Family member 4 memory problem symptoms"
  label(data$famhx04.onset.ptp)="Family member 4 memory problem age of onset"
  label(data$famhx04.age.ptp)="Family member 4 memory problem age of onset"
  label(data$famhx05.ptp)="Additional family member?"
  label(data$famhx05.relation.ptp)="Family member 5 relationship"
  label(data$famhx05.prob.ptp)="Family member 5 memory problem type"
  label(data$famhx05.dx.ptp)="Family member 5 dementia diagnosis"
  label(data$famhx05.sx.ptp)="Family member 5 memory problem symptoms"
  label(data$famhx05.onset.ptp)="Family member 5 memory problem age of onset"
  label(data$famhx05.age.ptp)="Family member 5 memory problem age of onset"
  label(data$famhx06.ptp)="Additional family member?"
  label(data$famhx06.relation.ptp)="Family member 6 relationship"
  label(data$famhx06.prob.ptp)="Family member 6 memory problem type"
  label(data$famhx06.dx.ptp)="Family member 6 dementia diagnosis"
  label(data$famhx06.sx.ptp)="Family member 6 memory problem symptoms"
  label(data$famhx06.onset.ptp)="Family member 6 memory problem age of onset"
  label(data$famhx06.age.ptp)="Family member 6 memory problem of onset"
  label(data$famhx07.ptp)="Additional family member?"
  label(data$famhx07.relation.ptp)="Family member 7 relationship"
  label(data$famhx07.prob.ptp)="Family member 7 memory problem type"
  label(data$famhx07.dx.ptp)="Family member 7 dementia diagnosis"
  label(data$famhx07.sx.ptp)="Family member 7 memory problem symptoms"
  label(data$famhx07.onset.ptp)="Family member 7 memory problem age of onset"
  label(data$famhx07.age.ptp)="Family member 7 memory problem age of onset"
  label(data$famhx08.ptp)="Additional family member?"
  label(data$famhx08.relation.ptp)="Family member 8 relationship"
  label(data$famhx08.prob.ptp)="Family member 8 memory problem type"
  label(data$famhx08.dx.ptp)="Family member 8 dementia diagnosis"
  label(data$famhx08.sx.ptp)="Family member 8 memory problem symptoms"
  label(data$famhx08.onset.ptp)="Family member 8 memory problem age of onset"
  label(data$famhx08.age.ptp)="Family member 8 memory problem age of onset"
  label(data$famhx09.ptp)="Additional family member?"
  label(data$famhx09.relation.ptp)="Family member 9 relationship"
  label(data$famhx09.prob.ptp)="Family member 9 memory problem type"
  label(data$famhx09.dx.ptp)="Family member 9 dementia diagnosis"
  label(data$famhx09.sx.ptp)="Family member 9 memory problem symptoms"
  label(data$famhx09.onset.ptp)="Family member 9 memory problem age of onset"
  label(data$famhx09.age.ptp)="Family member 9 memory problem age of onset"
  label(data$famhx10.ptp)="Additional family member?"
  label(data$famhx10.relation.ptp)="Family member 10 relationship"
  label(data$famhx10.prob.ptp)="Family member 10 memory problem type"
  label(data$famhx10.dx.ptp)="Family member 10 dementia diagnosis"
  label(data$famhx10.sx.ptp)="Family member 10 memory problem symptoms"
  label(data$famhx10.onset.ptp)="Family member 10 memory problem age of onset"
  label(data$famhx10.age.ptp)="Family member 10 memory problem age of onset"
  label(data$famhx11.ptp)="Additional family member?"
  label(data$famhx11.relation.ptp)="Family member 11 relationship"
  label(data$famhx11.prob.ptp)="Family member 11 memory problem type"
  label(data$famhx11.dx.ptp)="Family member 11 dementia diagnosis"
  label(data$famhx11.sx.ptp)="Family member 11 memory problem symptoms"
  label(data$famhx11.onset.ptp)="Family member 11 memory problem age of onset"
  label(data$famhx11.age.ptp)="Family member 11 memory problem age of onset"
  label(data$famhx12.ptp)="Additional family member?"
  label(data$famhx12.relation.ptp)="Family member 12 relationship"
  label(data$famhx12.prob.ptp)="Family member 12 memory problem type"
  label(data$famhx12.dx.ptp)="Family member 12 dementia diagnosis"
  label(data$famhx12.sx.ptp)="Family member 12 memory problem symptoms"
  label(data$famhx12.onset.ptp)="Family member 12 memory problem age of onset"
  label(data$famhx12.age.ptp)="Family member 12 memory problem age of onset"
  label(data$famhx13.ptp)="Additional family member?"
  label(data$famhx13.relation.ptp)="Family member 13 relationship"
  label(data$famhx13.prob.ptp)="Family member 13 memory problem type"
  label(data$famhx13.dx.ptp)="Family member 13 dementia diagnosis"
  label(data$famhx13.sx.ptp)="Family member 13 memory problem symptoms"
  label(data$famhx13.onset.ptp)="Family member 13 memory problem age of onset"
  label(data$famhx13.age.ptp)="Family member 13 memory problem age of onset"
  label(data$famhx01.inf)="Family history of memory problems?"
  label(data$famhx01.relation.inf)="Family member 1 relationship"
  label(data$famhx01.prob.inf)="Family member 1 memory problem type"
  label(data$famhx01.dx.inf)="Family member 1 dementia diagnosis"
  label(data$famhx01.sx.inf)="Family member 1 memory problem symptoms"
  label(data$famhx01.onset.inf)="Family member 1 memory problem age of onset"
  label(data$famhx01.age.inf)="Family member 1 memory problem age of onset"
  label(data$famhx02.inf)="Additional family member?"
  label(data$famhx02.relation.inf)="Family member 2 relationship"
  label(data$famhx02.prob.inf)="Family member 2 memory problem type"
  label(data$famhx02.dx.inf)="Family member 2 dementia diagnosis"
  label(data$famhx02.sx.inf)="Family member 2 memory problem symptoms"
  label(data$famhx02.onset.inf)="Family member 2 memory problem age of onset"
  label(data$famhx02.age.inf)="Family member 2 memory problem age of onset"
  label(data$famhx03.inf)="Additional family member?"
  label(data$famhx03.relation.inf)="Family member 3 relationship"
  label(data$famhx03.prob.inf)="Family member 3 memory problem type"
  label(data$famhx03.dx.inf)="Family member 3 dementia diagnosis"
  label(data$famhx03.sx.inf)="Family member 3 memory problem symptoms"
  label(data$famhx03.onset.inf)="Family member 3 memory problem age of onset"
  label(data$famhx03.age.inf)="Family member 3 memory problem age of onset"
  label(data$famhx04.inf)="Additional family member?"
  label(data$famhx04.relation.inf)="Family member 4 relationship"
  label(data$famhx04.prob.inf)="Family member 4 memory problem type"
  label(data$famhx04.dx.inf)="Family member 4 dementia diagnosis"
  label(data$famhx04.sx.inf)="Family member 4 memory problem symptoms"
  label(data$famhx04.onset.inf)="Family member 4 memory problem age of onset"
  label(data$famhx04.age.inf)="Family member 4 memory problem age of onset"
  label(data$famhx05.inf)="Additional family member?"
  label(data$famhx05.relation.inf)="Family member 5 relationship"
  label(data$famhx05.prob.inf)="Family member 5 memory problem type"
  label(data$famhx05.dx.inf)="Family member 5 dementia diagnosis"
  label(data$famhx05.sx.inf)="Family member 5 memory problem symptoms"
  label(data$famhx05.onset.inf)="Family member 5 memory problem age of onset"
  label(data$famhx05.age.inf)="Family member 5 memory problem age of onset"
  label(data$famhx06.inf)="Additional family member?"
  label(data$famhx06.relation.inf)="Family member 6 relationship"
  label(data$famhx06.prob.inf)="Family member 6 memory problem type"
  label(data$famhx06.dx.inf)="Family member 6 dementia diagnosis"
  label(data$famhx06.sx.inf)="Family member 6 memory problem symptoms"
  label(data$famhx06.onset.inf)="Family member 6 memory problem age of onset"
  label(data$famhx06.age.inf)="Family member 6 memory problem of onset"
  label(data$famhx07.inf)="Additional family member?"
  label(data$famhx07.relation.inf)="Family member 7 relationship"
  label(data$famhx07.prob.inf)="Family member 7 memory problem type"
  label(data$famhx07.dx.inf)="Family member 7 dementia diagnosis"
  label(data$famhx07.sx.inf)="Family member 7 memory problem symptoms"
  label(data$famhx07.onset.inf)="Family member 7 memory problem age of onset"
  label(data$famhx07.age.inf)="Family member 7 memory problem age of onset"
  label(data$famhx08.inf)="Additional family member?"
  label(data$famhx08.relation.inf)="Family member 8 relationship"
  label(data$famhx08.prob.inf)="Family member 8 memory problem type"
  label(data$famhx08.dx.inf)="Family member 8 dementia diagnosis"
  label(data$famhx08.sx.inf)="Family member 8 memory problem symptoms"
  label(data$famhx08.onset.inf)="Family member 8 memory problem age of onset"
  label(data$famhx08.age.inf)="Family member 8 memory problem age of onset"
  label(data$famhx09.inf)="Additional family member?"
  label(data$famhx09.relation.inf)="Family member 9 relationship"
  label(data$famhx09.prob.inf)="Family member 9 memory problem type"
  label(data$famhx09.dx.inf)="Family member 9 dementia diagnosis"
  label(data$famhx09.sx.inf)="Family member 9 memory problem symptoms"
  label(data$famhx09.onset.inf)="Family member 9 memory problem age of onset"
  label(data$famhx09.age.inf)="Family member 9 memory problem age of onset"
  label(data$famhx10.inf)="Additional family member?"
  label(data$famhx10.relation.inf)="Family member 10 relationship"
  label(data$famhx10.prob.inf)="Family member 10 memory problem type"
  label(data$famhx10.dx.inf)="Family member 10 dementia diagnosis"
  label(data$famhx10.sx.inf)="Family member 10 memory problem symptoms"
  label(data$famhx10.onset.inf)="Family member 10 memory problem age of onset"
  label(data$famhx10.age.inf)="Family member 10 memory problem age of onset"
  label(data$famhx11.inf)="Additional family member?"
  label(data$famhx11.relation.inf)="Family member 11 relationship"
  label(data$famhx11.prob.inf)="Family member 11 memory problem type"
  label(data$famhx11.dx.inf)="Family member 11 dementia diagnosis"
  label(data$famhx11.sx.inf)="Family member 11 memory problem symptoms"
  label(data$famhx11.onset.inf)="Family member 11 memory problem age of onset"
  label(data$famhx11.age.inf)="Family member 11 memory problem age of onset"
  label(data$famhx12.inf)="Additional family member?"
  label(data$famhx12.relation.inf)="Family member 12 relationship"
  label(data$famhx12.prob.inf)="Family member 12 memory problem type"
  label(data$famhx12.dx.inf)="Family member 12 dementia diagnosis"
  label(data$famhx12.sx.inf)="Family member 12 memory problem symptoms"
  label(data$famhx12.onset.inf)="Family member 12 memory problem age of onset"
  label(data$famhx12.age.inf)="Family member 12 memory problem age of onset"
  label(data$famhx13.inf)="Additional family member?"
  label(data$famhx13.relation.inf)="Family member 13 relationship"
  label(data$famhx13.prob.inf)="Family member 13 memory problem type"
  label(data$famhx13.dx.inf)="Family member 13 dementia diagnosis"
  label(data$famhx13.sx.inf)="Family member 13 memory problem symptoms"
  label(data$famhx13.onset.inf)="Family member 13 memory problem age of onset"
  label(data$famhx13.age.inf)="Family member 13 memory problem age of onset"
  label(data$famhx.notes)="Family history - form notes"
  label(data$family.history.complete)="Complete?"
  label(data$inform.date)="Informant Information - Date of Acquisition"
  label(data$inform.method)="Informant Information Data Collection Method"
  label(data$inform.source...1)="Source of Documentation (choice=MAP Participant Tracking Database)"
  label(data$inform.source...2)="Source of Documentation (choice=Eligibility CDR Documentation)"
  label(data$inform.source...3)="Source of Documentation (choice=Eligibility Informant Questionnaire)"
  label(data$inform.source...4)="Source of Documentation (choice=Other Source of Eligibility Chart)"
  label(data$inform.source...5)="Source of Documentation (choice=Reminder Binder)"
  label(data$inform.source...6)="Source of Documentation (choice=Follow-up Telephone Inquiry)"
  label(data$inform.source...7)="Source of Documentation (choice=Eligibility Visit)"
  label(data$inform.source...8)="Source of Documentation (choice=Enrollment Visit)"
  label(data$inform.source...9)="Source of Documentation (choice=Follow-up Visit)"
  label(data$inform.source...10)="Source of Documentation (choice=Other)"
  label(data$inform.source....9999)="Source of Documentation (choice=missing)"
  label(data$inform.source....8888)="Source of Documentation (choice=N/A)"
  label(data$inform.source.notes)="Source of Documentation - notes"
  label(data$inform.initials)="Informant Initials"
  label(data$inform.sex)="Informant Sex"
  label(data$inform.educ)="Informant Years of Education"
  label(data$inform.relation)="Informants Relationship to Participant"
  label(data$inform.relation.notes)="Informants Relationship to Participant - Specify Other"
  label(data$inform.length.yrs)="Length of Time Informant Has Known Participant - years"
  label(data$inform.length.mos)="Length of Time Informant Has Known Participant - months"
  label(data$inform.live)="Does Informant Live with Participant?"
  label(data$inform.phone)="If no, approximate frequency of telephone contact"
  label(data$inform.visit)="If no, approximate frequency of in-person visits"
  label(data$inform.reliability)="Is there a concern about the informants reliability?"
  label(data$inform.reliability.notes)="If yes, explain"
  label(data$inform.notes)="Informant - form notes"
  label(data$informant.complete)="Complete?"
  label(data$medhx.date)="Medical History - Date of Acquisition"
  label(data$age)="Age"
  label(data$dob)="Date of birth"
  label(data$sex)="Sex"
  label(data$marital.stat)="Marital status"
  label(data$ethnicity)="Ethnicity"
  label(data$race)="Race"
  label(data$race.other)="Other race"
  label(data$usa.born)="Born in United States"
  label(data$usa.date)="Year moved to United States"
  label(data$lang.prim)="Primary language"
  label(data$lang.prim.other)="Primary language - other"
  label(data$lang.second...0)="Secondary language (choice=No)"
  label(data$lang.second...1)="Secondary language (choice=English)"
  label(data$lang.second...2)="Secondary language (choice=Spanish)"
  label(data$lang.second...3)="Secondary language (choice=French)"
  label(data$lang.second...4)="Secondary language (choice=German)"
  label(data$lang.second...5)="Secondary language (choice=Japanese)"
  label(data$lang.second...9)="Secondary language (choice=Other)"
  label(data$lang.second...9999)="Secondary language (choice=Missing)"
  label(data$lang.second....8888)="Secondary language (choice=N/A)"
  label(data$lang.second.other)="Secondary language - other"
  label(data$education)="Education (years)"
  label(data$prim.occup)="Primary occupation"
  label(data$meds)="Current medications"
  label(data$med01.name)="Medication 1 - name"
  label(data$med01.dose)="Medication 1 - dosage"
  label(data$med01.units)="Medication 1 - dosage units"
  label(data$med01.freq)="Medication 1 - frequency"
  label(data$med01.freq.units)="Medication 1 - frequency units"
  label(data$med01.dose.day)="Medication 1 - dosage/day"
  label(data$med01.units.day)="Medication 1 - dosage units/day"
  label(data$med01.dur)="Medication 1 - duration of use"
  label(data$med01.dur.units)="Medication 1 - duration units"
  label(data$med02)="Add another medication?"
  label(data$med02.name)="Medication 2 - name"
  label(data$med02.dose)="Medication 2- dosage"
  label(data$med02.units)="Medication 2 - dosage units"
  label(data$med02.freq)="Medication 2 - frequency"
  label(data$med02.freq.units)="Medication 2 - frequency units"
  label(data$med02.dose.day)="Medication 2 - dosage/day"
  label(data$med02.units.day)="Medication 2 - dosage units/day"
  label(data$med02.dur)="Medication 2 - duration of use"
  label(data$med02.dur.units)="Medication 2 - duration units"
  label(data$med03)="Add another medication?"
  label(data$med03.name)="Medication 3 - name"
  label(data$med03.dose)="Medication 3 - dosage"
  label(data$med03.units)="Medication 3 - dosage units"
  label(data$med03.freq)="Medication 3 - frequency"
  label(data$med03.freq.units)="Medication 3 - frequency units"
  label(data$med03.dose.day)="Medication 3- dosage/day"
  label(data$med03.units.day)="Medication 3 - dosage units/day"
  label(data$med03.dur)="Medication 3 - duration of use"
  label(data$med03.dur.units)="Medication 3 - duration units"
  label(data$med04)="Add another medication?"
  label(data$med04.name)="Medication 4 - name"
  label(data$med04.dose)="Medication 4 - dosage"
  label(data$med04.units)="Medication 4 - dosage units"
  label(data$med04.freq)="Medication 4 - frequency"
  label(data$med04.freq.units)="Medication 4 - frequency units"
  label(data$med04.dose.day)="Medication 4 - dosage/day"
  label(data$med04.units.day)="Medication 4 - dosage units/day"
  label(data$med04.dur)="Medication 4 - duration of use"
  label(data$med04.dur.units)="Medication 4 - duration units"
  label(data$med05)="Add another medication?"
  label(data$med05.name)="Medication 5 - name"
  label(data$med05.dose)="Medication 5 - dosage"
  label(data$med05.units)="Medication 5 - dosage units"
  label(data$med05.freq)="Medication 5 - frequency"
  label(data$med05.freq.units)="Medication 5 - frequency units"
  label(data$med05.dose.day)="Medication 5 - dosage/day"
  label(data$med05.units.day)="Medication 5 - dosage units/day"
  label(data$med05.dur)="Medication 5 - duration of use"
  label(data$med05.dur.units)="Medication 5 - duration units"
  label(data$med06)="Add another medication?"
  label(data$med06.name)="Medication 6 - name"
  label(data$med06.dose)="Medication 6 - dosage"
  label(data$med06.units)="Medication 6 - dosage units"
  label(data$med06.freq)="Medication 6 - frequency"
  label(data$med06.freq.units)="Medication 6 - frequency units"
  label(data$med06.dose.day)="Medication 6 - dosage/day"
  label(data$med06.units.day)="Medication 6 - dosage units/day"
  label(data$med06.dur)="Medication 6 - duration of use"
  label(data$med06.dur.units)="Medication 6 - duration units"
  label(data$med07)="Add another medication?"
  label(data$med07.name)="Medication 7 - name"
  label(data$med07.dose)="Medication 7 - dosage"
  label(data$med07.units)="Medication 7 - dosage units"
  label(data$med07.freq)="Medication 7 - frequency"
  label(data$med07.freq.units)="Medication 7 - frequency units"
  label(data$med07.dose.day)="Medication 7 - dosage/day"
  label(data$med07.units.day)="Medication 7 - dosage units/day"
  label(data$med07.dur)="Medication 7 - duration of use"
  label(data$med07.dur.units)="Medication 7 - duration units"
  label(data$med08)="Add another medication?"
  label(data$med08.name)="Medication 8 - name"
  label(data$med08.dose)="Medication 8 - dosage"
  label(data$med08.units)="Medication 8 - dosage units"
  label(data$med08.freq)="Medication 8 - frequency"
  label(data$med08.freq.units)="Medication 8 - frequency units"
  label(data$med08.dose.day)="Medication 8 - dosage/day"
  label(data$med08.units.day)="Medication 8 - dosage units/day"
  label(data$med08.dur)="Medication 8 - duration of use"
  label(data$med08.dur.units)="Medication 8 - duration units"
  label(data$med09)="Add another medication?"
  label(data$med09.name)="Medication 9 - name"
  label(data$med09.dose)="Medication 9 - dosage"
  label(data$med09.units)="Medication 9 - dosage units"
  label(data$med09.freq)="Medication 9 - frequency"
  label(data$med09.freq.units)="Medication 9 - frequency units"
  label(data$med09.dose.day)="Medication 9 - dosage/day"
  label(data$med09.units.day)="Medication 9 - dosage units/day"
  label(data$med09.dur)="Medication 9 - duration of use"
  label(data$med09.dur.units)="Medication 9 - duration units"
  label(data$med10)="Add another medication?"
  label(data$med10.name)="Medication 10 - name"
  label(data$med10.dose)="Medication 10 - dosage"
  label(data$med10.units)="Medication 10 - dosage units"
  label(data$med10.freq)="Medication 10 - frequency"
  label(data$med10.freq.units)="Medication 10 - frequency units"
  label(data$med10.dose.day)="Medication 10 - dosage/day"
  label(data$med10.units.day)="Medication 10 - dosage units/day"
  label(data$med10.dur)="Medication 10 - duration of use"
  label(data$med10.dur.units)="Medication 10 - duration units"
  label(data$med11)="Add another medication?"
  label(data$med11.name)="Medication 11 - name"
  label(data$med11.dose)="Medication 11 - dosage"
  label(data$med11.units)="Medication 11 - dosage units"
  label(data$med11.freq)="Medication 11 - frequency"
  label(data$med11.freq.units)="Medication 11 - frequency units"
  label(data$med11.dose.day)="Medication 11 - dosage/day"
  label(data$med11.units.day)="Medication 11 - dosage units/day"
  label(data$med11.dur)="Medication 11 - duration of use"
  label(data$med11.dur.units)="Medication 11 - duration units"
  label(data$med12)="Add another medication?"
  label(data$med12.name)="Medication 12 - name"
  label(data$med12.dose)="Medication 12 - dosage"
  label(data$med12.units)="Medication 12 - dosage units"
  label(data$med12.freq)="Medication 12 - frequency"
  label(data$med12.freq.units)="Medication 12 - frequency units"
  label(data$med12.dose.day)="Medication 12 - dosage/day"
  label(data$med12.units.day)="Medication 12 - dosage units/day"
  label(data$med12.dur)="Medication 12 - duration of use"
  label(data$med12.dur.units)="Medication 12 - duration units"
  label(data$med13)="Add another medication?"
  label(data$med13.name)="Medication 13 - name"
  label(data$med13.dose)="Medication 13 - dosage"
  label(data$med13.units)="Medication 13 - dosage units"
  label(data$med13.freq)="Medication 13 - frequency"
  label(data$med13.freq.units)="Medication 13 - frequency units"
  label(data$med13.dose.day)="Medication 13 - dosage/day"
  label(data$med13.units.day)="Medication 13 - dosage units/day"
  label(data$med13.dur)="Medication 13 - duration of use"
  label(data$med13.dur.units)="Medication 13 - duration units"
  label(data$med14)="Add another medication?"
  label(data$med14.name)="Medication 14 - name"
  label(data$med14.dose)="Medication 14 - dosage"
  label(data$med14.units)="Medication 14 - dosage units"
  label(data$med14.freq)="Medication 14 - frequency"
  label(data$med14.freq.units)="Medication 14 - frequency units"
  label(data$med14.dose.day)="Medication 14 - dosage/day"
  label(data$med14.units.day)="Medication 14 - dosage units/day"
  label(data$med14.dur)="Medication 14 - duration of use"
  label(data$med14.dur.units)="Medication 14 - duration units"
  label(data$med15)="Add another medication?"
  label(data$med15.name)="Medication 15 - name"
  label(data$med15.dose)="Medication 15 - dosage"
  label(data$med15.units)="Medication 15 - dosage units"
  label(data$med15.freq)="Medication 15 - frequency"
  label(data$med15.freq.units)="Medication 15 - frequency units"
  label(data$med15.dose.day)="Medication 15 - dosage/day"
  label(data$med15.units.day)="Medication 15 - dosage units/day"
  label(data$med15.dur)="Medication 15 - duration of use"
  label(data$med15.dur.units)="Medication 15 - duration units"
  label(data$med16)="Add another medication?"
  label(data$med16.name)="Medication 16 - name"
  label(data$med16.dose)="Medication 16 - dosage"
  label(data$med16.units)="Medication 16 - dosage units"
  label(data$med16.freq)="Medication 16 - frequency"
  label(data$med16.freq.units)="Medication 16 - frequency units"
  label(data$med16.dose.day)="Medication 16 - dosage/day"
  label(data$med16.units.day)="Medication 16 - dosage units/day"
  label(data$med16.dur)="Medication 16 - duration of use"
  label(data$med16.dur.units)="Medication 16 - duration units"
  label(data$med17)="Add another medication?"
  label(data$med17.name)="Medication 17 - name"
  label(data$med17.dose)="Medication 17 - dosage"
  label(data$med17.units)="Medication 17 - dosage units"
  label(data$med17.freq)="Medication 17 - frequency"
  label(data$med17.freq.units)="Medication 17 - frequency units"
  label(data$med17.dose.day)="Medication 17 - dosage/day"
  label(data$med17.units.day)="Medication 17 - dosage units/day"
  label(data$med17.dur)="Medication 17 - duration of use"
  label(data$med17.dur.units)="Medication 17 - duration units"
  label(data$med18)="Add another medication?"
  label(data$med18.name)="Medication 18 - name"
  label(data$med18.dose)="Medication 18 - dosage"
  label(data$med18.units)="Medication 18 - dosage units"
  label(data$med18.freq)="Medication 18 - frequency"
  label(data$med18.freq.units)="Medication 18 - frequency units"
  label(data$med18.dose.day)="Medication 18 - dosage/day"
  label(data$med18.units.day)="Medication 18 - dosage units/day"
  label(data$med18.dur)="Medication 18 - duration of use"
  label(data$med18.dur.units)="Medication 18 - duration units"
  label(data$med19)="Add another medication?"
  label(data$med19.name)="Medication 19 - name"
  label(data$med19.dose)="Medication 19 - dosage"
  label(data$med19.units)="Medication 19 - dosage units"
  label(data$med19.freq)="Medication 19 - frequency"
  label(data$med19.freq.units)="Medication 19 - frequency units"
  label(data$med19.dose.day)="Medication 19 - dosage/day"
  label(data$med19.units.day)="Medication 19 - dosage units/day"
  label(data$med19.dur)="Medication 19 - duration of use"
  label(data$med19.dur.units)="Medication 19 - duration units"
  label(data$med20)="Add another medication?"
  label(data$med20.name)="Medication 20 - name"
  label(data$med20.dose)="Medication 20 - dosage"
  label(data$med20.units)="Medication 20 - dosage units"
  label(data$med20.freq)="Medication 20 - frequency"
  label(data$med20.freq.units)="Medication 20 - frequency units"
  label(data$med20.dose.day)="Medication 20 - dosage/day"
  label(data$med20.units.day)="Medication 20 - dosage units/day"
  label(data$med20.dur)="Medication 20 - duration of use"
  label(data$med20.dur.units)="Medication 20 - duration units"
  label(data$med21)="Add another medication?"
  label(data$med21.name)="Medication 21- name"
  label(data$med21.dose)="Medication 21 - dosage"
  label(data$med21.units)="Medication 21 - dosage units"
  label(data$med21.freq)="Medication 21 - frequency"
  label(data$med21.freq.units)="Medication 21 - frequency units"
  label(data$med21.dose.day)="Medication 21 - dosage/day"
  label(data$med21.units.day)="Medication 21 - dosage units/day"
  label(data$med21.dur)="Medication 21 - duration of use"
  label(data$med21.dur.units)="Medication 21 - duration units"
  label(data$med22)="Add another medication?"
  label(data$med22.name)="Medication 22 - name"
  label(data$med22.dose)="Medication 22 - dosage"
  label(data$med22.units)="Medication 22 - dosage units"
  label(data$med22.freq)="Medication 22 - frequency"
  label(data$med22.freq.units)="Medication 22 - frequency units"
  label(data$med22.dose.day)="Medication 22 - dosage/day"
  label(data$med22.units.day)="Medication 22 - dosage units/day"
  label(data$med22.dur)="Medication 22 - duration of use"
  label(data$med22.dur.units)="Medication 22 - duration units"
  label(data$med23)="Add another medication?"
  label(data$med23.name)="Medication 23 - name"
  label(data$med23.dose)="Medication 23 - dosage"
  label(data$med23.units)="Medication 23 - dosage units"
  label(data$med23.freq)="Medication 23 - frequency"
  label(data$med23.freq.units)="Medication 23 - frequency units"
  label(data$med23.dose.day)="Medication 23 - dosage/day"
  label(data$med23.units.day)="Medication 23 - dosage units/day"
  label(data$med23.dur)="Medication 23 - duration of use"
  label(data$med23.dur.units)="Medication 23 - duration units"
  label(data$med24)="Add another medication?"
  label(data$med24.name)="Medication 24 - name"
  label(data$med24.dose)="Medication 24 - dosage"
  label(data$med24.units)="Medication 24 - dosage units"
  label(data$med24.freq)="Medication 24 - frequency"
  label(data$med24.freq.units)="Medication 24 - frequency units"
  label(data$med24.dose.day)="Medication 24 - dosage/day"
  label(data$med24.units.day)="Medication 24 - dosage units/day"
  label(data$med24.dur)="Medication 24 - duration of use"
  label(data$med24.dur.units)="Medication 24 - duration units"
  label(data$med25)="Add another medication?"
  label(data$med25.name)="Medication 25 - name"
  label(data$med25.dose)="Medication 25 - dosage"
  label(data$med25.units)="Medication 25 - dosage units"
  label(data$med25.freq)="Medication 25 - frequency"
  label(data$med25.freq.units)="Medication 25 - frequency units"
  label(data$med25.dose.day2.4d0)="Medication 25 - dosage/day"
  label(data$med25.units.day)="Medication 25 - dosage units/day"
  label(data$med25.dur)="Medication 25 - duration of use"
  label(data$med25.dur.units)="Medication 25 - duration units"
  label(data$med26)="Add another medication?"
  label(data$med26.name)="Medication 26 - name"
  label(data$med26.dose)="Medication 26 - dosage"
  label(data$med26.units)="Medication 26 - dosage units"
  label(data$med26.freq)="Medication 26 - frequency"
  label(data$med26.freq.units)="Medication 26 - frequency units"
  label(data$med26.dose.day)="Medication 26 - dosage/day"
  label(data$med26.units.day)="Medication 26 - dosage units/day"
  label(data$med26.dur)="Medication 26 - duration of use"
  label(data$med26.dur.units)="Medication 26 - duration units"
  label(data$med27)="Add another medication?"
  label(data$med27.name)="Medication 27 - name"
  label(data$med27.dose)="Medication 27 - dosage"
  label(data$med27.units)="Medication 27 - dosage units"
  label(data$med27.freq)="Medication 27 - frequency"
  label(data$med27.freq.units)="Medication 27 - frequency units"
  label(data$med27.dose.day)="Medication 27 - dosage/day"
  label(data$med27.units.day)="Medication 27 - dosage units/day"
  label(data$med27.dur)="Medication 27 - duration of use"
  label(data$med27.dur.units)="Medication 27 - duration units"
  label(data$med28)="Add another medication?"
  label(data$med28.name)="Medication 28 - name"
  label(data$med28.dose)="Medication 28 - dosage"
  label(data$med28.units)="Medication 28 - dosage units"
  label(data$med28.freq)="Medication 28 - frequency"
  label(data$med28.freq.units)="Medication 28 - frequency units"
  label(data$med28.dose.day)="Medication 28 - dosage/day"
  label(data$med28.units.day)="Medication 28 - dosage units/day"
  label(data$med28.dur)="Medication 28 - duration of use"
  label(data$med28.dur.units)="Medication 28 - duration units"
  label(data$med29)="Add another medication?"
  label(data$med29.name)="Medication 29 - name"
  label(data$med29.dose)="Medication 29 - dosage"
  label(data$med29.units)="Medication 29 - dosage units"
  label(data$med29.freq)="Medication 29 - frequency"
  label(data$med29.freq.units)="Medication 29 - frequency units"
  label(data$med29.dose.day)="Medication 29 - dosage/day"
  label(data$med29.units.day)="Medication 29 - dosage units/day"
  label(data$med29.dur)="Medication 29 - duration of use"
  label(data$med29.dur.units)="Medication 29 - duration units"
  label(data$med30)="Add another medication?"
  label(data$med30.name)="Medication 30 - name"
  label(data$med30.dose)="Medication 30 - dosage"
  label(data$med30.units)="Medication 30 - dosage units"
  label(data$med30.freq)="Medication 30 - frequency"
  label(data$med30.freq.units)="Medication 30 - frequency units"
  label(data$med30.dose.day)="Medication 30 - dosage/day"
  label(data$med30.units.day)="Medication 30 - dosage units/day"
  label(data$med30.dur)="Medication 30 - duration of use"
  label(data$med30.dur.units)="Medication 30 - duration units"
  label(data$med31)="Add another medication?"
  label(data$med31.name)="Medication 31 - name"
  label(data$med31.dose)="Medication 31 - dosage"
  label(data$med31.units)="Medication 31 - dosage units"
  label(data$med31.freq)="Medication 31 - frequency"
  label(data$med31.freq.units)="Medication 31 - frequency units"
  label(data$med31.dose.day)="Medication 31 - dosage/day"
  label(data$med31.units.day)="Medication 31 - dosage units/day"
  label(data$med31.dur)="Medication 31 - duration of use"
  label(data$med31.dur.units)="Medication 31 - duration units"
  label(data$med32)="Add another medication?"
  label(data$med32.name)="Medication 32 - name"
  label(data$med32.dose)="Medication 32 - dosage"
  label(data$med32.units)="Medication 32 - dosage units"
  label(data$med32.freq)="Medication 32 - frequency"
  label(data$med32.freq.units)="Medication 32 - frequency units"
  label(data$med32.dose.day)="Medication 32 - dosage/day"
  label(data$med32.units.day)="Medication 32 - dosage units/day"
  label(data$med32.dur)="Medication 32 - duration of use"
  label(data$med32.dur.units)="Medication 32 - duration units"
  label(data$med33)="Add another medication?"
  label(data$med33.name)="Medication 33- name"
  label(data$med33.dose)="Medication 33 - dosage"
  label(data$med33.units)="Medication 33 - dosage units"
  label(data$med33.freq)="Medication 33 - frequency"
  label(data$med33.freq.units)="Medication 33 - frequency units"
  label(data$med33.dose.day)="Medication 33 - dosage/day"
  label(data$med33.units.day)="Medication 33 - dosage units/day"
  label(data$med33.dur)="Medication 33 - duration of use"
  label(data$med33.dur.units)="Medication 33 - duration units"
  label(data$med34)="Add another medication?"
  label(data$med34.name)="Medication 34 - name"
  label(data$med34.dose)="Medication 34 - dosage"
  label(data$med34.units)="Medication 34 - dosage units"
  label(data$med34.freq)="Medication 34 - frequency"
  label(data$med34.freq.units)="Medication 34 - frequency units"
  label(data$med34.dose.day)="Medication 34 - dosage/day"
  label(data$med34.units.day)="Medication 34 - dosage units/day"
  label(data$med34.dur)="Medication 34 - duration of use"
  label(data$med34.dur.units)="Medication 34 - duration units"
  label(data$med35)="Add another medication?"
  label(data$med35.name)="Medication 35 - name"
  label(data$med35.dose)="Medication 35 - dosage"
  label(data$med35.units)="Medication 35 - dosage units"
  label(data$med35.freq)="Medication 35 - frequency"
  label(data$med35.freq.units)="Medication 35 - frequency units"
  label(data$med35.dose.day)="Medication 35 - dosage/day"
  label(data$med35.units.day)="Medication 35 - dosage units/day"
  label(data$med35.dur)="Medication 35 - duration of use"
  label(data$med35.dur.units)="Medication 35 - duration units"
  label(data$med36)="Add another medication?"
  label(data$med36.name)="Medication 36 - name"
  label(data$med36.dose)="Medication 36 - dosage"
  label(data$med36.units)="Medication 36 - dosage units"
  label(data$med36.freq)="Medication 36 - frequency"
  label(data$med36.freq.units)="Medication 36 - frequency units"
  label(data$med36.dose.day)="Medication 36 - dosage/day"
  label(data$med36.units.day)="Medication 36 - dosage units/day"
  label(data$med36.dur)="Medication 36 - duration of use"
  label(data$med36.dur.units)="Medication 36 - duration units"
  label(data$med37)="Add another medication?"
  label(data$med37.name)="Medication 37 - name"
  label(data$med37.dose)="Medication 37 - dosage"
  label(data$med37.units)="Medication 37 - dosage units"
  label(data$med37.freq)="Medication 37 - frequency"
  label(data$med37.freq.units)="Medication 37 - frequency units"
  label(data$med37.dose.day2.4d0)="Medication 37 - dosage/day"
  label(data$med37.units.day)="Medication 37 - dosage units/day"
  label(data$med37.dur)="Medication 37 - duration of use"
  label(data$med37.dur.units)="Medication 37 - duration units"
  label(data$med38)="Add another medication?"
  label(data$med38.name)="Medication 38 - name"
  label(data$med38.dose)="Medication 38 - dosage"
  label(data$med38.units)="Medication 38 - dosage units"
  label(data$med38.freq)="Medication 38 - frequency"
  label(data$med38.freq.units)="Medication 38 - frequency units"
  label(data$med38.dose.day)="Medication 38 - dosage/day"
  label(data$med38.units.day)="Medication 38 - dosage units/day"
  label(data$med38.dur)="Medication 38 - duration of use"
  label(data$med38.dur.units)="Medication 38 - duration units"
  label(data$med39)="Add another medication?"
  label(data$med39.name)="Medication 39 - name"
  label(data$med39.dose)="Medication 39 - dosage"
  label(data$med39.units)="Medication 39 - dosage units"
  label(data$med39.freq)="Medication 39 - frequency"
  label(data$med39.freq.units)="Medication 39 - frequency units"
  label(data$med39.dose.day)="Medication 39 - dosage/day"
  label(data$med39.units.day)="Medication 39 - dosage units/day"
  label(data$med39.dur)="Medication 39 - duration of use"
  label(data$med39.dur.units)="Medication 39 - duration units"
  label(data$med40)="Add another medication?"
  label(data$med40.name)="Medication 40 - name"
  label(data$med40.dose)="Medication 40 - dosage"
  label(data$med40.units)="Medication 40 - dosage units"
  label(data$med40.freq)="Medication 40 - frequency"
  label(data$med40.freq.units)="Medication 40 - frequency units"
  label(data$med40.dose.day)="Medication 40 - dosage/day"
  label(data$med40.units.day)="Medication 40 - dosage units/day"
  label(data$med40.dur)="Medication 40 - duration of use"
  label(data$med40.dur.units)="Medication 40 - duration units"
  label(data$med41)="Add another medication?"
  label(data$med41.name)="Medication 41 - name"
  label(data$med41.dose)="Medication 41 - dosage"
  label(data$med41.units)="Medication 41 - dosage units"
  label(data$med41.freq)="Medication 41 - frequency"
  label(data$med41.freq.units)="Medication 41 - frequency units"
  label(data$med41.dose.day)="Medication 41 - dosage/day"
  label(data$med41.units.day)="Medication 41 - dosage units/day"
  label(data$med41.dur)="Medication 41 - duration of use"
  label(data$med41.dur.units)="Medication 41 - duration units"
  label(data$med42)="Add another medication?"
  label(data$med42.name)="Medication 42 - name"
  label(data$med42.dose)="Medication 42 - dosage"
  label(data$med42.units)="Medication 42 - dosage units"
  label(data$med42.freq)="Medication 42 - frequency"
  label(data$med42.freq.units)="Medication 42 - frequency units"
  label(data$med42.dose.day)="Medication 42 - dosage/day"
  label(data$med42.units.day)="Medication 42 - dosage units/day"
  label(data$med42.dur)="Medication 42 - duration of use"
  label(data$med42.dur.units)="Medication 42 - duration units"
  label(data$med43)="Add another medication?"
  label(data$med43.name)="Medication 43- name"
  label(data$med43.dose)="Medication 43 - dosage"
  label(data$med43.units)="Medication 43 - dosage units"
  label(data$med43.freq)="Medication 43 - frequency"
  label(data$med43.freq.units)="Medication 43 - frequency units"
  label(data$med43.dose.day)="Medication 43 - dosage/day"
  label(data$med43.units.day)="Medication 43 - dosage units/day"
  label(data$med43.dur)="Medication 43 - duration of use"
  label(data$med43.dur.units)="Medication 43 - duration units"
  label(data$med44)="Add another medication?"
  label(data$med44.name)="Medication 44 - name"
  label(data$med44.dose)="Medication 44 - dosage"
  label(data$med44.units)="Medication 44 - dosage units"
  label(data$med44.freq)="Medication 44 - frequency"
  label(data$med44.freq.units)="Medication 44 - frequency units"
  label(data$med44.dose.day)="Medication 44 - dosage/day"
  label(data$med44.units.day)="Medication 44 - dosage units/day"
  label(data$med44.dur)="Medication 44 - duration of use"
  label(data$med44.dur.units)="Medication 44 - duration units"
  label(data$med45)="Add another medication?"
  label(data$med45.name)="Medication 45 - name"
  label(data$med45.dose)="Medication 45 - dosage"
  label(data$med45.units)="Medication 45 - dosage units"
  label(data$med45.freq)="Medication 45 - frequency"
  label(data$med45.freq.units)="Medication 45 - frequency units"
  label(data$med45.dose.day)="Medication 45 - dosage/day"
  label(data$med45.units.day)="Medication 45 - dosage units/day"
  label(data$med45.dur)="Medication 45 - duration of use"
  label(data$med45.dur.units)="Medication 45 - duration units"
  label(data$med46)="Add another medication?"
  label(data$med46.name)="Medication 46 - name"
  label(data$med46.dose)="Medication 46 - dosage"
  label(data$med46.units)="Medication 46 - dosage units"
  label(data$med46.freq)="Medication 46 - frequency"
  label(data$med46.freq.units)="Medication 46 - frequency units"
  label(data$med46.dose.day)="Medication 46 - dosage/day"
  label(data$med46.units.day)="Medication 46 - dosage units/day"
  label(data$med46.dur)="Medication 46 - duration of use"
  label(data$med46.dur.units)="Medication 46 - duration units"
  label(data$med47)="Add another medication?"
  label(data$med47.name)="Medication 47 - name"
  label(data$med47.dose)="Medication 47 - dosage"
  label(data$med47.units)="Medication 47 - dosage units"
  label(data$med47.freq)="Medication 47 - frequency"
  label(data$med47.freq.units)="Medication 47 - frequency units"
  label(data$med47.dose.day2.4d0)="Medication 47 - dosage/day"
  label(data$med47.units.day)="Medication 47 - dosage units/day"
  label(data$med47.dur)="Medication 47 - duration of use"
  label(data$med47.dur.units)="Medication 47 - duration units"
  label(data$med48)="Add another medication?"
  label(data$med48.name)="Medication 48 - name"
  label(data$med48.dose)="Medication 48 - dosage"
  label(data$med48.units)="Medication 48 - dosage units"
  label(data$med48.freq)="Medication 48 - frequency"
  label(data$med48.freq.units)="Medication 48 - frequency units"
  label(data$med48.dose.day)="Medication 48 - dosage/day"
  label(data$med48.units.day)="Medication 48 - dosage units/day"
  label(data$med48.dur)="Medication 48 - duration of use"
  label(data$med48.dur.units)="Medication 48 - duration units"
  label(data$med49)="Add another medication?"
  label(data$med49.name)="Medication 49 - name"
  label(data$med49.dose)="Medication 49 - dosage"
  label(data$med49.units)="Medication 49 - dosage units"
  label(data$med49.freq)="Medication 49 - frequency"
  label(data$med49.freq.units)="Medication 49 - frequency units"
  label(data$med49.dose.day)="Medication 49 - dosage/day"
  label(data$med49.units.day)="Medication 49 - dosage units/day"
  label(data$med49.dur)="Medication 49 - duration of use"
  label(data$med49.dur.units)="Medication 49 - duration units"
  label(data$med50)="Add another medication?"
  label(data$med50.name)="Medication 50 - name"
  label(data$med50.dose)="Medication 50 - dosage"
  label(data$med50.units)="Medication 50 - dosage units"
  label(data$med50.freq)="Medication 50 - frequency"
  label(data$med50.freq.units)="Medication 50 - frequency units"
  label(data$med50.dose.day)="Medication 50 - dosage/day"
  label(data$med50.units.day)="Medication 50 - dosage units/day"
  label(data$med50.dur)="Medication 50 - duration of use"
  label(data$med50.dur.units)="Medication 50 - duration units"
  label(data$med51)="Add another medication?"
  label(data$med51.name)="Medication 51 - name"
  label(data$med51.dose)="Medication 51 - dosage"
  label(data$med51.units)="Medication 51 - dosage units"
  label(data$med51.freq)="Medication 51 - frequency"
  label(data$med51.freq.units)="Medication 51 - frequency units"
  label(data$med51.dose.day)="Medication 51 - dosage/day"
  label(data$med51.units.day)="Medication 51 - dosage units/day"
  label(data$med51.dur)="Medication 51 - duration of use"
  label(data$med51.dur.units)="Medication 51 - duration units"
  label(data$med.add)="Add another medication?"
  label(data$med.add.list)="Please list any additional medication"
  label(data$mhx.angina)="Med Hx 1 - Angina"
  label(data$mhx.angina.age)="Med Hx 1 - Angina - age of onset"
  label(data$mhx.angina.exer)="Med Hx 1 - Agina - limit ability to exercise?"
  label(data$mhx.arrhyth)="Med Hx 2 - Arrhythmia"
  label(data$mhx.arrhyth.age)="Med Hx 2 - Arrhythmia - age of onset"
  label(data$mhx.arthritis)="Med Hx 3 - Arthritis"
  label(data$mhx.arthritis.age)="Med Hx 3 - Arthritis - age of onset"
  label(data$mhx.arthritis.type)="Med Hx 3 - Arthritis - type"
  label(data$mhx.arthritis.type.other)="Med Hx 3 - Arthritis - type other"
  label(data$mhx.afib)="Med Hx 4 - AFIB"
  label(data$mhx.afib.age)="Med Hx 4 - AFIB - age of onset"
  label(data$mhx.bp)="Med Hx 5 - Blood pressure"
  label(data$mhx.bp.age)="Med Hx 5 - Blood pressure - age of onset"
  label(data$mhx.bp.type)="Med Hx 5 - Blood pressure - type"
  label(data$mhx.arrest)="Med Hx 6 - Cardiac arrest"
  label(data$mhx.arrest.age)="Med Hx 6 - Cardiac arrest - age of onset"
  label(data$mhx.cad)="Med Hx 7 - CAD"
  label(data$mhx.cad.age)="Med Hx 7 - CAD - age of onset"
  label(data$mhx.bypass)="Med Hx 8 - CABG"
  label(data$mhx.bypass.age)="Med Hx 8 - CABG - age of onset"
  label(data$mhx.bypass.n)="Med Hx 8 - CABG - number bypassed"
  label(data$mhx.cancer)="Med Hx 9 - Cancer"
  label(data$mhx.cancer.age)="Med Hx 9 - Cancer - age of onset"
  label(data$mhx.cancer.type)="Med Hx 9 - Cancer - type"
  label(data$mhx.hf)="Med Hx 10 - Heart failure"
  label(data$mhx.hf.age)="Med Hx 10 - Heart failure- age of onset"
  label(data$mhx.diabetes)="Med Hx 11 - Diabetes"
  label(data$mhx.diabetes.age)="Med Hx 11 - Diabetes - age of onset"
  label(data$mhx.diabetes.control...1)="Med Hx 11 - Diabetes - tx method (choice=Diet)"
  label(data$mhx.diabetes.control...2)="Med Hx 11 - Diabetes - tx method (choice=Oral Medication)"
  label(data$mhx.diabetes.control...3)="Med Hx 11 - Diabetes - tx method (choice=Insulin)"
  label(data$mhx.diabetes.control....9999)="Med Hx 11 - Diabetes - tx method (choice=Missing)"
  label(data$mhx.diabetes.control....8888)="Med Hx 11 - Diabetes - tx method (choice=N/A)"
  label(data$mhx.hep)="Med Hx 12 - Hepatitis"
  label(data$mhx.hep.age)="Med Hx 12 - Hepatitis - age of onset"
  label(data$mhx.hep.type)="Med Hx 12 - Hepatitis - type"
  label(data$mhx.chol)="Med Hx 13 - High cholesterol"
  label(data$mhx.chol.age)="Med Hx 13 - High cholesterol - age of onset"
  label(data$mhx.mi)="Med Hx 14 - Heart attack"
  label(data$mhx.mi.age)="Med Hx 14 - Heart attack- age of onset"
  label(data$mhx.heartsurg)="Med Hx 15 - Heart surgery"
  label(data$mhx.heartsurg.age)="Med Hx 15 - Heart surgery - age of onset"
  label(data$mhx.glauc)="Med Hx 16 - Glaucoma"
  label(data$mhx.glauc.age)="Med Hx 16 - Glaucoma - age of onset"
  label(data$mhx.cataracts)="Med Hx 17 - Cataracts"
  label(data$mhx.cataracts.age)="Med Hx 17 - Cataracts - age of onset"
  label(data$mhx.kidney)="Med Hx 18 - Kidney disease"
  label(data$mhx.kidney.age)="Med Hx 18 - Kidney disease - age of onset"
  label(data$mhx.kidney.type)="Med Hx 18 - Kidney disease - type"
  label(data$mhx.liver)="Med Hx 19 - Liver disease"
  label(data$mhx.liver.age)="Med Hx 19 - Liver disease - age of onset"
  label(data$mhx.liver.type)="Med Hx 19 - Liver disease - type"
  label(data$mhx.lupus)="Med Hx 20 - Lupus"
  label(data$mhx.lupus.age)="Med Hx 20 - Lupus - age of onset"
  label(data$mhx.lyme)="Med Hx 21 - Lyme disease"
  label(data$mhx.lyme.age)="Med Hx 21 - Lyme disease - age of onset"
  label(data$mhx.mvp)="Med Hx 22 - Mitral valve prolapse"
  label(data$mhx.mvp.age)="Med Hx 22 - Mitral valve prolapse - age of onset"
  label(data$mhx.seiz)="Med Hx 23 - Seizure or epilepsy"
  label(data$mhx.seiz.age)="Med Hx 23 - Seizure or epilepsy - age of onset"
  label(data$mhx.angio)="Med Hx 24 - Stent insertion"
  label(data$mhx.angio.age)="Med Hx 24 - Stent insertion - age of onset"
  label(data$mhx.angio.number)="Med Hx 24 - Stent insertion - number"
  label(data$mhx.thyroid)="Med Hx 25 - Thyroid disease"
  label(data$mhx.thyroid.age)="Med Hx 25 - Thyroid disease - age of onset"
  label(data$mhx.thyroid.type)="Med Hx 25 - Thyroid disease - type"
  label(data$mhx.tremors)="Med Hx 26 - Tremors"
  label(data$mhx.tremors.age)="Med Hx 26 - Tremors - age of onset"
  label(data$mhx.tb)="Med Hx 27 - Tuberculosis"
  label(data$mhx.tb.age)="Med Hx 27 - Tuberculosis - age of onset"
  label(data$mhx.valve)="Med Hx 28 - Valve replacement or repair"
  label(data$mhx.valve.age)="Med Hx 28 - Valve replacement or repair - age of onset"
  label(data$mhx.valve.number)="Med Hx 28 - Valve replacement or repair - number"
  label(data$mhx.ad)="Med Hx 29 - AD"
  label(data$mhx.ad.age)="Med Hx 29 - AD - age of onset"
  label(data$mhx.mci)="Med Hx 30 - MCI"
  label(data$mhx.mci.age)="Med Hx 30 - MCI - age of onset"
  label(data$mhx.tbi)="Med Hx 31 - Head Injury"
  label(data$mhx.tbi.age)="Med Hx 31 - Head injury - age of onset"
  label(data$mhx.tbi.consc)="Med Hx 31 - Head injury - LOC"
  label(data$mhx.bleed)="Med Hx 32 - Brain hemorrhage or bleed"
  label(data$mhx.bleed.age)="Med Hx 32 - Brain hemorrhage or bleed - age of onset"
  label(data$mhx.bleed.loc)="Med Hx 32 - Brain hemorrhage or bleed - location"
  label(data$mhx.stroke)="Med Hx 33 - Stroke"
  label(data$mhx.stroke.age)="Med Hx 33 - Stroke - age of onset"
  label(data$mhx.stroke.location)="Med Hx 33 - Stroke - location"
  label(data$mhx.tia)="Med Hx 34 - TIA"
  label(data$mhx.tia.age)="Med Hx 34 - TIA - age of onset"
  label(data$mhx.tia.location)="Med Hx 34 - TIA - location"
  label(data$mhx.infect)="Med Hx 35 - Brain infection or meningitis"
  label(data$mhx.infect.age)="Med Hx 35 - Brain infection or meningitis - age of onset"
  label(data$mhx.headaches)="Med Hx 36 - Headaches"
  label(data$mhx.headaches.age)="Med Hx 36 - Headaches - age of onset"
  label(data$mhx.headaches.descript)="Med Hx 36 - Headaches - description"
  label(data$mhx.ms)="Med Hx 37 - Multiple sclerosis"
  label(data$mhx.ms.age)="Med Hx 37 - Multiple sclerosis - age of onset"
  label(data$mhx.pd)="Med Hx 38 - Parkinsons disease"
  label(data$mhx.pd.age)="Med Hx 38 - Parkinsons disease - age of onset"
  label(data$mhx.anx)="Med Hx 39 - Anxiety"
  label(data$mhx.anx.age)="Med Hx 39 - Anxiety - age of onset"
  label(data$mhx.anx.tx)="Med Hx 39 - Anxiety - tx"
  label(data$mhx.depress)="Med Hx 40 - Depression"
  label(data$mhx.depress.age)="Med Hx 40 - Depression - age of onset"
  label(data$mhx.depress.tx)="Med Hx 40 - Depression - tx"
  label(data$mhx.schiz)="Med Hx 41 - Schizophrenia"
  label(data$mhx.schiz.age)="Med Hx 41 - Schizophrenia - age of onset"
  label(data$mhx.schiz.tx)="Med Hx 41 - Schizophrenia - tx"
  label(data$mhx.psych)="Med Hx 42 - Psychiatric illness"
  label(data$mhx.psych.age)="Med Hx 42 - Psychiatric illness - age of onset"
  label(data$mhx.psych.descript)="Med Hx 42 - Psychiatric illness - describe"
  label(data$mhx.etoh)="Med Hx 43 - Chronic alcohol use"
  label(data$mhx.etoh.age)="Med Hx 43 - Chronic alcohol use - age of onset"
  label(data$mhx.etoh.tx)="Med Hx 43 - Chronic alcohol use - tx"
  label(data$mhx.abuse)="Med Hx 44 - Chronic drug use"
  label(data$mhx.abuse.age)="Med Hx 44 - Chronic drug use - age of onset"
  label(data$mhx.abuse.tx)="Med Hx 44 - Chronic drug use - tx"
  label(data$mhx.tobac)="Med Hx 45 - Cigarette smoking - tobacco"
  label(data$mhx.tobac.age)="Med Hx 45 - Cigarette smoking - age of onset"
  label(data$mhx.tobac.pks)="Med Hx 45 - Cigarette smoking - ppd"
  label(data$mhx.tobac.quit)="Med Hx 45 - Cigarette smoking - quit"
  label(data$mhx.ld)="Med Hx 46 - Learning disabilities"
  label(data$mhx.ld.age)="Med Hx 46 - Learning disabilities - age of onset"
  label(data$mhx.ld.descript)="Med Hx 46 - Learning disabilities - describe"
  label(data$mhx.troub.learning)="Med Hx 47 - Trouble learning in school"
  label(data$mhx.troub.learning.age)="Med Hx 47 - Trouble learning in school - age of onset"
  label(data$mhx.troub.learning.descript)="Med Hx 47 - Trouble learning in school - describe"
  label(data$mhx.add)="Med Hx 48 - Attention deficit disorder"
  label(data$mhx.add.age)="Med Hx 48 - Attention deficit disorder - age of onset"
  label(data$mhx.add.tx)="Med Hx 48 - Attention deficit disorder - treatment"
  label(data$mhx.pain)="Med Hx 49 - Chronic back pain"
  label(data$mhx.pain.age)="Med Hx 49 - Chronic back pain - age of onset"
  label(data$mhx.neuropathy)="Med Hx 50 - Neuropathy"
  label(data$mhx.neuropathy.age)="Med Hx 50 - Neuropathy - age of onset"
  label(data$mhx.neuropathy.location)="Med Hx 50 - Neuropathy - location"
  label(data$mhx.asthma)="Med Hx 51 - Asthma"
  label(data$mhx.asthma.age)="Med Hx 51 - Asthma - age of onset"
  label(data$mhx.copd)="Med Hx 52 - COPD"
  label(data$mhx.copd.age)="Med Hx 52 - COPD - age of onset"
  label(data$mhx.murmur)="Med Hx 53 - Murmur"
  label(data$mhx.murmur.age)="Med Hx 53 - Murmur - age of onset"
  label(data$mhx.rhythm)="Med Hx 54 - Heart rhythm issues"
  label(data$mhx.rhythm.age)="Med Hx 54 - Heart rhythm issues - age of onset"
  label(data$mhx.rhythm.descript)="Med Hx 54 - Heart rhythm issues - description"
  label(data$mhx.athero)="Med Hx 55 - Atherosclerosis"
  label(data$mhx.athero.age)="Med Hx 55 - Atherosclerosis - age of onset"
  label(data$mhx.branch)="Med Hx 56 - Partial branch block"
  label(data$mhx.branch.age)="Med Hx 56 - Partial branch block - age of onset"
  label(data$mhx.macular)="Med Hx 57 - Macular degeneration"
  label(data$mhx.macular.age)="Med Hx 57 - Macular degeneration - age of onset"
  label(data$mhx.restlessleg)="Med Hx 58 - Restless leg syndrome"
  label(data$mhx.restlessleg.age)="Med Hx 58 - Restless leg syndrome - age of onset"
  label(data$mhx.apnea)="Med Hx 59 - Sleep apnea"
  label(data$mhx.apnea.age)="Med Hx 59 - Sleep apnea - age of onset"
  label(data$mhx.rem)="Med Hx 60 - REM sleep behavior disorder"
  label(data$mhx.rem.age)="Med Hx 60 - REM sleep behavior disorder - age of onset"
  label(data$mhx.pad)="Med Hx 61 - Peripheral artery disease"
  label(data$mhx.pad.age)="Med Hx 61 - Peripheral artery disease - age of onset"
  label(data$mhx.cblind)="Med Hx 62 - Color blindness "
  label(data$mhx.cblind.age)="Med Hx 62 - Color blindness - age of onset"
  label(data$mhx.other1)="Med Hx - Other medical issues?"
  label(data$mhx.other1.age)="Med Hx - Other medical issue #1"
  label(data$mhx.other1.descript)="Med Hx - Other medical issue #1 - describe"
  label(data$mhx.other2)="Med Hx - Add another medical issue?"
  label(data$mhx.other2.age)="Med Hx - Other medical issue #2 - age of onset"
  label(data$mhx.other2.decript)="Med Hx - Other medical issue #2 - describe"
  label(data$mhx.other3)="Med Hx - Add another medical issue?"
  label(data$mhx.other3.age)="Med Hx - Other medical issue #3 - age of onset"
  label(data$mhx.other3.descript)="Med hx - Other medical issue #3 - describe"
  label(data$mhx.other4)="Med Hx - Add another medical issue?"
  label(data$mhx.other4.age)="Med Hx - Other medical issue #4 - age of onset"
  label(data$mhx.other4.descript)="Med Hx - Other medical issue #4 - describe"
  label(data$mhx.other5)="Med Hx - Add another medical issue?"
  label(data$mhx.other5.age)="Med Hx - Other medical issue #5 - age of onset"
  label(data$mhx.other5.descript)="Med Hx - Other medical issue #5 - describe"
  label(data$mhx.other.add)="Med Hx - List additional issues"
  label(data$mhx.notes)="Med Hx - form notes"
  label(data$medical.history.complete)="Complete?"
  label(data$surg01)="Prior surgeries"
  label(data$surg01.name)="Prior surgery 1 - name"
  label(data$surg01.year)="Prior surgery 1 - year"
  label(data$surg01.location)="Prior surgery 1 - anatomical location"
  label(data$surg01.implant)="Prior surgery 1 - implant"
  label(data$surg01.specs)="Prior surgery 1 - implant specifications"
  label(data$surg01.specs2)="Prior surgery 1 - implant specifications"
  label(data$surg01.specs3)="Prior surgery 1 - implant specifications"
  label(data$surg02)="Add another surgery?"
  label(data$surg02.name)="Prior surgery 2 - name"
  label(data$surg02.year)="Prior surgery 2 - year"
  label(data$surg02.location)="Prior surgery 2 - anatomical location"
  label(data$surg02.implant)="Prior surgery 2 - implant"
  label(data$surg02.specs)="Prior surgery 2 - implant specifications"
  label(data$surg02.specs2)="Prior surgery 2 - implant specifications"
  label(data$surg02.specs3)="Prior surgery 2 - implant specifications"
  label(data$surg03)="Add another surgery?"
  label(data$surg03.name)="Prior surgery 3- name"
  label(data$surg03.year)="Prior surgery 3 - year"
  label(data$surg03.location)="Prior surgery 3 - anatomical location"
  label(data$surg03.implant)="Prior surgery 3 - implant"
  label(data$surg03.specs)="Prior surgery 3 - implant specifications"
  label(data$surg03.specs2)="Prior surgery 3 - implant specifications"
  label(data$surg03.specs3)="Prior surgery 3 - implant specifications"
  label(data$surg04)="Add another surgery?"
  label(data$surg04.name)="Prior surgery 4 - name"
  label(data$surg04.year)="Prior surgery 4 - year"
  label(data$surg04.location)="Prior surgery 4 - anatomical location"
  label(data$surg04.implant)="Prior surgery 4 - implant"
  label(data$surg04.specs)="Prior surgery 4 - implant specifications"
  label(data$surg04.specs2)="Prior surgery 4 - implant specifications"
  label(data$surg04.specs3)="Prior surgery 4 - implant specifications"
  label(data$surg05)="Add another surgery?"
  label(data$surg05.name)="Prior surgery 5 - name"
  label(data$surg05.year)="Prior surgery 5 - year"
  label(data$surg05.location)="Prior surgery 5 - anatomical location"
  label(data$surg05.implant)="Prior surgery 5 - implant"
  label(data$surg05.specs)="Prior surgery 5 - implant specifications"
  label(data$surg05.specs2)="Prior surgery 5 - implant specifications"
  label(data$surg05.specs3)="Prior surgery 5 - implant specifications"
  label(data$surg06)="Add another surgery?"
  label(data$surg06.name)="Prior surgery 6 - name"
  label(data$surg06.year)="Prior surgery 6 - year"
  label(data$surg06.location)="Prior surgery 6 - anatomical location"
  label(data$surg06.implant)="Prior surgery 6 - implant"
  label(data$surg06.specs)="Prior surgery 6 - implant specifications"
  label(data$surg06.specs2)="Prior surgery 6 - implant specifications"
  label(data$surg06.specs3)="Prior surgery 6 - implant specifications"
  label(data$surg07)="Add another surgery?"
  label(data$surg07.name)="Prior surgery 7 - name"
  label(data$surg07.year)="Prior surgery 7 - year"
  label(data$surg07.location)="Prior surgery 7 - anatomical location"
  label(data$surg07.implant)="Prior surgery 7 - implant"
  label(data$surg07.specs)="Prior surgery 7 - implant specifications"
  label(data$surg07.specs2)="Prior surgery 7 - implant specifications"
  label(data$surg07.specs3)="Prior surgery 7 - implant specifications"
  label(data$surg08)="Add another surgery?"
  label(data$surg08.name)="Prior surgery 8 - name"
  label(data$surg08.year)="Prior surgery 8 - year"
  label(data$surg08.location)="Prior surgery 8 - anatomical location"
  label(data$surg08.implant)="Prior surgery 8 - implant"
  label(data$surg08.specs)="Prior surgery 8 - implant specifications"
  label(data$surg08.specs2)="Prior surgery 8 - implant specifications"
  label(data$surg08.specs3)="Prior surgery 8 - implant specifications"
  label(data$surg09)="Add another surgery?"
  label(data$surg09.name)="Prior surgery 9 - name"
  label(data$surg09.year)="Prior surgery 9 - year"
  label(data$surg09.location)="Prior surgery 9 - anatomical location"
  label(data$surg09.implant)="Prior surgery 9 - implant"
  label(data$surg09.specs)="Prior surgery 9 - implant specifications"
  label(data$surg09.specs2)="Prior surgery 9 - implant specifications"
  label(data$surg09.specs3)="Prior surgery 9 - implant specifications"
  label(data$surg10)="Add another surgery?"
  label(data$surg10.name)="Prior surgery 10 - name"
  label(data$surg10.year)="Prior surgery 10 - year"
  label(data$surg10.location)="Prior surgery 10 - anatomical location"
  label(data$surg10.implant)="Prior surgery 10 - implant"
  label(data$surg10.specs)="Prior surgery 10 - implant specifications"
  label(data$surg10.specs2)="Prior surgery 10 - implant specifications"
  label(data$surg10.specs3)="Prior surgery 10 - implant specifications"
  label(data$surg11)="Add another surgery?"
  label(data$surg11.name)="Prior surgery 11 - name"
  label(data$surg11.year)="Prior surgery 11 - year"
  label(data$surg11.location)="Prior surgery 11 - anatomical location"
  label(data$surg11.implant)="Prior surgery 11 - implant"
  label(data$surg11.specs)="Prior surgery 11 - implant specifications"
  label(data$surg11.specs2)="Prior surgery 11 - implant specifications"
  label(data$surg11.specs3)="Prior surgery 11 - implant specifications"
  label(data$surg12)="Add another surgery?"
  label(data$surg12.name)="Prior surgery 12 - name"
  label(data$surg12.year)="Prior surgery 12 - year"
  label(data$surg12.location)="Prior surgery 12 - anatomical location"
  label(data$surg12.implant)="Prior surgery 12 - implant"
  label(data$surg12.specs)="Prior surgery 12 - implant specifications"
  label(data$surg12.specs2)="Prior surgery 12 - implant specifications"
  label(data$surg12.specs3)="Prior surgery 12 - implant specifications"
  label(data$surg13)="Add another surgery?"
  label(data$surg13.name)="Prior surgery 13 - name"
  label(data$surg13.year)="Prior surgery 13 - year"
  label(data$surg13.location)="Prior surgery 13 - anatomical location"
  label(data$surg13.implant)="Prior surgery 13 - implant"
  label(data$surg13.specs)="Prior surgery 13 - implant specifications"
  label(data$surg13.specs2)="Prior surgery 1 - implant specifications"
  label(data$surg13.specs3)="Prior surgery 13 -implant specifications"
  label(data$surg14)="Add another surgery?"
  label(data$surg14.name)="Prior surgery 14 - name"
  label(data$surg14.year)="Prior surgery 14 - year"
  label(data$surg14.location)="Prior surgery 14 - anatomical location"
  label(data$surg14.implant)="Prior surgery 14 - implant"
  label(data$surg14.specs)="Prior surgery 14 - implant specifications"
  label(data$surg14.specs2)="Prior surgery 14 -implant specifications"
  label(data$surg14.specs3)="Prior surgery 14 -implant specifications"
  label(data$surg15)="Add another surgery?"
  label(data$surg15.name)="Prior surgery 15 - name"
  label(data$surg15.year)="Prior surgery 15 - year"
  label(data$surg15.location)="Prior surgery 15 - anatomical location"
  label(data$surg15.implant)="Prior surgery 15 - implant"
  label(data$surg15.specs)="Prior surgery 15 - implant specifications"
  label(data$surg15.specs2)="Prior surgery 15 -implant specifications"
  label(data$surg15.specs3)="Prior surgery 15 -implant specifications"
  label(data$surg16)="Add another surgery?"
  label(data$surg16.name)="Prior surgery 16 - name"
  label(data$surg16.year)="Prior surgery 16 - year"
  label(data$surg16.location)="Prior surgery 16 - anatomical location"
  label(data$surg16.implant)="Prior surgery 16 - implant"
  label(data$surg16.specs)="Prior surgery 16 - implant specifications"
  label(data$surg16.specs2)="Prior surgery 16 -implant specifications"
  label(data$surg16.specs3)="Prior surgery 16 -implant specifications"
  label(data$surg17)="Add another surgery?"
  label(data$surg17.name)="Prior surgery 17 - name"
  label(data$surg17.year)="Prior surgery 17 - year"
  label(data$surg17.location)="Prior surgery 17 - anatomical location"
  label(data$surg17.implant)="Prior surgery 17 - implant"
  label(data$surg17.specs)="Prior surgery 17 - implant specifications"
  label(data$surg17.specs2)="Prior surgery 17 -implant specifications"
  label(data$surg17.specs3)="Prior surgery 17 -implant specifications"
  label(data$surg18)="Add another surgery?"
  label(data$surg18.name)="Prior surgery 18 - name"
  label(data$surg18.year)="Prior surgery 18 - year"
  label(data$surg18.location)="Prior surgery 18 - anatomical location"
  label(data$surg18.implant)="Prior surgery 18 - implant"
  label(data$surg18.specs)="Prior surgery 18 - implant specifications"
  label(data$surg18.specs2)="Prior surgery 18 -implant specifications"
  label(data$surg18.specs3)="Prior surgery 18 -implant specifications"
  label(data$surg19)="Add another surgery?"
  label(data$surg19.name)="Prior surgery 19 - name"
  label(data$surg19.year)="Prior surgery 19 - year"
  label(data$surg19.location)="Prior surgery 19 - anatomical location"
  label(data$surg19.implant)="Prior surgery 19 - implant"
  label(data$surg19.specs)="Prior surgery 19 - implant specifications"
  label(data$surg19.specs2)="Prior surgery 19 -implant specifications"
  label(data$surg19.specs3)="Prior surgery 19 -implant specifications"
  label(data$surg20)="Add another surgery?"
  label(data$surg20.name)="Prior surgery 20 - name"
  label(data$surg20.year)="Prior surgery 20 - year"
  label(data$surg20.location)="Prior surgery 20 - anatomical location"
  label(data$surg20.implant)="Prior surgery 20 - implant"
  label(data$surg20.specs)="Prior surgery 20 - implant specifications"
  label(data$surg20.specs2)="Prior surgery 20 -implant specifications"
  label(data$surg20.specs3)="Prior surgery 20 -implant specifications"
  label(data$surgical.history.complete)="Complete?"
  label(data$scc.date)="Self Cognitive Complaint (SCC) - Date of Completion"
  label(data$scc01)="SCC 1 - Do you have complaints about your memory in the last 2 years?"
  label(data$scc02)="SCC 2 - Is your memory worse than five years ago?"
  label(data$scc03)="SCC 3 - I dont remember things as well as I used to"
  label(data$scc04)="SCC 4 - Do you have problems with your memory?"
  label(data$scc05)="SCC 5 - Do memory problems make it harder to complete tasks that used to be easy?"
  label(data$scc06)="SCC 6 - Do you have trouble finding the word you want to use?"
  label(data$scc07)="SCC 7 - Do you have trouble following plot of a story that you are reading or have read?"
  label(data$scc08)="SCC 8 - Do you have difficulty remembering 2-3 items to buy without a list?"
  label(data$scc09)="SCC 9 - Do you have difficulty remembering to turn off stove or lights?"
  label(data$scc10)="SCC 10 - Are you unable to recall names of good friends?"
  label(data$scc.notes)="SCC - form notes"
  label(data$cognitive.complaint.self.complete)="Complete?"
  label(data$icc.date)="Informant Cognitive Complaint (ICC) - Date of Completion"
  label(data$icc01)="ICC 1 - Do you have complaints about participants memory in the last 2 years?"
  label(data$icc02)="ICC 2 - Is the participants memory worse than five years ago?"
  label(data$icc03)="ICC 3 - The participant does not remember things as well as s/he used to"
  label(data$icc04)="ICC 4 - Does participant have problems with his/her memory?"
  label(data$icc05)="ICC 5 - Do memory problems make it harder for participant to complete tasks that used to be easy?"
  label(data$icc06)="ICC 6 - Does participant often have trouble finding the word s/he wants to use?"
  label(data$icc07)="ICC 7 - Does participant have trouble following plot of story that s/he is reading or has read?"
  label(data$icc08)="ICC 8 - Does participant have difficulty remembering 2-3 items to buy without a list?"
  label(data$icc09)="ICC 9 - Does participant have difficulty remembering to turn off stove or lights?"
  label(data$icc10)="ICC 10 - Is participant unable to recall names of good friends?"
  label(data$icc)="ICC Total Score"
  label(data$icc.notes)="ICC - form notes"
  label(data$cognitive.complaint.informant.complete)="Complete?"
  label(data$ccqself.date)="Self Cognitive Complaint Questionnaire (CCQ) - Date of Completion"
  label(data$ccqself01)="CCQ Self 1 - Do you think you have problems with your memory?"
  label(data$ccqself02)="CCQ Self 2 - Do you feel you can remember things as well as you used to?"
  label(data$ccqself03)="CCQ Self 3 - Do you think your memory is worse than 5 years ago?"
  label(data$ccqself04)="CCQ Self 4 - Do you think your memory is worse than 2 years ago?"
  label(data$ccqself05)="CCQ Self 5 - Do you have complaints about your memory in the last 2 years?"
  label(data$ccqself06)="CCQ Self 6 - Has your memory changed?"
  label(data$ccqself07)="CCQ Self 7 - Has your memory changed significantly?"
  label(data$ccqself08)="CCQ Self 8 - Do you have difficulty with your memory?"
  label(data$ccqself09)="CCQ Self 9 - If you have memory difficulties, are they concerning you?"
  label(data$ccqself10)="CCQ Self 10 - If you have memory difficulties, do you think they are significant?"
  label(data$ccqself11)="CCQ Self 11 - I dont remember things as well as I used to. Agree=Yes, Disagree=No"
  label(data$ccqself12)="CCQ Self 12 - Do you consider your memory to be worse than others your age?"
  label(data$ccqself13)="CCQ Self 13 - Do you feel you have more memory problems than most?"
  label(data$ccqself14)="CCQ Self 14 - Do you feel your everyday life is difficult now due to your memory decline?"
  label(data$ccqself15)="CCQ Self 15 - Do memory problems make it harder to complete tasks that used to be easy?"
  label(data$ccqself16)="CCQ Self 16 - Do you think you have problems remembering things you want to do or say?"
  label(data$ccqself17)="CCQ Self 17 - Do you have difficulty recalling date or day of week?"
  label(data$ccqself18)="CCQ Self 18 - Do you have more trouble remembering things that happened recently?"
  label(data$ccqself19)="CCQ Self 19 - Do you ever have difficulty remembering an event that occured last week?"
  label(data$ccqself20)="CCQ Self 20 - Do you have trouble remembering things from one moment to the next?"
  label(data$ccqself21)="CCQ Self 21 - Do you have difficulty remembering a conversation from a few days ago?"
  label(data$ccqself22)="CCQ Self 22 - Do you feel you are unable to follow a conversation?"
  label(data$ccqself23)="CCQ Self 23 - Do you notice yourself repeating the same question or story?"
  label(data$ccqself24)="CCQ Self 24 - Do other people say you ask the same question or repeat the same story?"
  label(data$ccqself25)="CCQ Self 25 - Do you often have trouble finding the word you want to use in everyday conversation?"
  label(data$ccqself26)="CCQ Self 26 - Do you talk less because of memory or word-finding difficulties?"
  label(data$ccqself27)="CCQ Self 27 - Do you have any trouble following plot of a story you are reading/have read?"
  label(data$ccqself28)="CCQ Self 28 - Do you have difficulty remembering where you placed objects?"
  label(data$ccqself29)="CCQ Self 29 - Do you lose objects more often than previously?"
  label(data$ccqself30)="CCQ Self 30 - Are you worse at remembering where belongings are kept?"
  label(data$ccqself31)="CCQ Self 31 - Do you feel you are forgetting where things were placed?"
  label(data$ccqself32)="CCQ Self 32 - Do you have difficulty remembering 2-3 items to buy when shopping without list?"
  label(data$ccqself33)="CCQ Self 33 - Do you have difficulty remembering to turn off stove or lights?"
  label(data$ccqself34)="CCQ Self 34 - Have you become lost driving or walking in areas near your home?"
  label(data$ccqself35)="CCQ Self 35 - Have you been unsure of how to navigate to a familiar location (store, pharmacy)?"
  label(data$ccqself36)="CCQ Self 36 - Do you have difficulty recalling names of family?"
  label(data$ccqself37)="CCQ Self 37 - Do you feel you are unable to recall the names of good friends?"
  label(data$ccqself38)="CCQ Self 38 - Do you have difficulty recognizing familiar people?"
  label(data$ccqself39)="CCQ Self 39 - Do you have difficulty remembering phone numbers of your children?"
  label(data$ccqself40)="CCQ Self 40 - Do you have difficulty remembering medical appointments?"
  label(data$ccqself41)="CCQ Self 41 - Do you have trouble remembering social arrangements?"
  label(data$ccqself42)="CCQ Self 42 - Are you able to remember appointments without writing them down or using calendar?"
  label(data$ccqself43)="CCQ Self 43 - Do you think your memory is good or poor?"
  label(data$ccqself46)="CCQ Self - 1. Phone numbers youve just checked"
  label(data$ccqself47)="CCQ Self - 2. Phone numbers you use frequently"
  label(data$ccqself48)="CCQ Self - 3. Things people tell you"
  label(data$ccqself49)="CCQ Self - 4. Keeping up correspondence"
  label(data$ccqself50)="CCQ Self - 5. Personal dates (e.g., birthdays)"
  label(data$ccqself51)="CCQ Self - 6. Words"
  label(data$ccqself52)="CCQ Self - 7. Going to the store and forgetting what you wanted to buy"
  label(data$ccqself53)="CCQ Self - 8. Beginning to do something and forgetting what you were doing"
  label(data$ccqself54)="CCQ Self - 9. Losing train of thought in conversation"
  label(data$ccqself55)="CCQ Self - 10. Knowing whether youve already told someone something"
  label(data$ccqself56)="CCQ Self - A. 1 year ago?"
  label(data$ccqself57)="CCQ Self - B. 5 years ago?"
  label(data$ccqself58)="CCQ Self - C. 10 years ago?"
  label(data$ccqself59)="CCQ Self - D. 20 years ago?"
  label(data$ccq.notes.self)="CCQ Self - form notes"
  label(data$cognitive.questionnaire.self.complete)="Complete?"
  label(data$ccqinform.date)="CCQ Informant - Date of Completion"
  label(data$ccqinform01)="CCQ Informant 1 - Do you think participant has problems with his/her memory?"
  label(data$ccqinform02)="CCQ Informant 2 - Do you feel participant can remember things as well as s/he used to?"
  label(data$ccqinform03)="CCQ Informant 3 - Do you think participants memory is worse than 5 years ago?"
  label(data$ccqinform04)="CCQ Informant 4 - Do you think participants memory is worse than 2 years ago?"
  label(data$ccqinform05)="CCQ Informant 5 - Do you have complaints about participants memory in the last 2 years?"
  label(data$ccqinform06)="CCQ Informant 6 - Has participants memory changed?"
  label(data$ccqinform07)="CCQ Informant 7 - Has participants memory changed significantly?"
  label(data$ccqinform08)="CCQ Informrant 8 - Does participant have difficulty with memory?"
  label(data$ccqinform09)="CCQ Informant 9 - If s/he has memory difficulties, are they concerning?"
  label(data$ccqinform10)="CCQ Informant 10 - If s/he has memory difficulties, do you think they are significant?"
  label(data$ccqinform11)="CCQ Informant 11 - The participant doesnt remember things as well as s/he used to"
  label(data$ccqinform12)="CCQ Informant 12 - Do you consider the participants memory worse than others the same age?"
  label(data$ccqinform13)="CCQ Informant 13 - Do you feel participant has more memory problems than most?"
  label(data$ccqinform14)="CCQ Informant 14 - Do you feel participants everyday life is difficult now due to memory decline?"
  label(data$ccqinform15)="CCQ Informant 15 - Do memory problems make it harder for participant to complete tasks?"
  label(data$ccqinform16)="CCQ Informant 16 - Do you think participant has problems remembering things s/he wants to do or say?"
  label(data$ccqinform17)="CCQ Informant 17 - Does participant have difficulty recalling date or day of week?"
  label(data$ccqinform18)="CCQ Informant 18 - Does participant have more trouble remembering things that happened recently?"
  label(data$ccqinform19)="CCQ Informant 19 - Does participant have difficulty remembering an event that occurred last week?"
  label(data$ccqinform20)="CCQ Informant 20 - Does participant have trouble remembering things from one moment to next?"
  label(data$ccqinform21)="CCQ Informant 21 - Does participant have difficulty remembering a conversation from a few days ago?"
  label(data$ccqinform22)="CCQ Informant 22 - Do you feel participant is unable to follow a conversation?"
  label(data$ccqinform23)="CCQ Informant 23 - Do you notice participant repeating the same question or story?"
  label(data$ccqinform24)="CCQ Informant 24 - Do other people say participant asks same question or repeats same story?"
  label(data$ccqinform25)="CCQ Informant 25 - Does participant often have trouble finding word s/he wants to use in everyday conversation?"
  label(data$ccqinform26)="CCQ Informant 26 - Does participant talk less because of memory or word-finding difficulties?"
  label(data$ccqinform27)="CCQ Informant 27 - Does participant have trouble following plot of story s/he is reading/has read?"
  label(data$ccqinform28)="CCQ Informant 28 - Does participant have difficulty remembering where s/he placed objects?"
  label(data$ccqinform29)="CCQ Informant 29 - Does participant lose objects more often than previously?"
  label(data$ccqinform30)="CCQ Informant 30 - Is participant worse at remembering where belongings are kept?"
  label(data$ccqinform31)="CCQ Informant 31 - Do you feel participant is forgetting where things were placed?"
  label(data$ccqinform32)="CCQ Informant 32 - Does participant have difficulty remembering 2-3 items to buy when shopping without list?"
  label(data$ccqinform33)="CCQ Informant 33 - Does participant have difficulty remembering to turn off stove or lights?"
  label(data$ccqinform34)="CCQ Informant 34 - Has participant become lost driving or walking in areas near participants home?"
  label(data$ccqinform35)="CCQ Informant 35 - Has participant been unsure of how to navigate to familiar location?"
  label(data$ccqinform36)="CCQ Informant 36 - Does participant have difficulty recalling names of family?"
  label(data$ccqinform37)="CCQ Informant 37 - Do you feel participant is unable to recall names of good friends?"
  label(data$ccqinform38)="CCQ Informant 38 - Does participant have difficulty recognizing familiar people?"
  label(data$ccqinform39)="CCQ Informant 39 - Does participant have difficulty remembering phone numbers of participants own children?"
  label(data$ccqinform40)="CCQ Informant 40 - Does participant have difficulty remembering medical appointments?"
  label(data$ccqinform41)="CCQ Informant 41 - Does participant have trouble remembering social arrangements?"
  label(data$ccqinform42)="CCQ Informant 42 - Is participant able to remember appointments without writing them down or using a calendar? "
  label(data$ccqinform43)="CCQ Informant 43 - Do you think participants memory is good or poor?"
  label(data$ccqinform44)="CCQ Informant - 1. Phone numbers just checked"
  label(data$ccqinform45)="CCQ Informant - 2. Phone numbers used frequently"
  label(data$ccqinform46)="CCQ Informant - 3. Things people tell the participant"
  label(data$ccqinform47)="CCQ Informant - 4. Keeping up correspondence"
  label(data$ccqinform48)="CCQ Informant - 5. Personal dates (e.g., birthdays)"
  label(data$ccqinform49)="CCQ Informant - 6. Words"
  label(data$ccqinform50)="CCQ Informant - 7. Going to the store and forgetting to buy what he/she wanted to buy"
  label(data$ccqinform51)="CCQ Informant - 8. Beginning to do something and forgetting what he/she was doing"
  label(data$ccqinform52)="CCQ Informant - 9. Losing the train of thought in conversation"
  label(data$ccqinform53)="CCQ Informant - 10. Knowing whether he/she has already told someone something"
  label(data$ccqinform54)="CCQ Informant - A. 1 year ago"
  label(data$ccqinform55)="CCQ Informant - B. 5 years ago"
  label(data$ccqinform56)="CCQ Informant - C. 10 years ago"
  label(data$ccqinform57)="CCQ Informant - D. 20 years ago"
  label(data$ccqinform)="CCQ Informant - Total Score"
  label(data$ccqinform.short)="CCQ Informant - Short Total Score"
  label(data$ccq.notes.inform)="SCC Informant - form notes"
  label(data$cognitive.questionnaire.informant.complete)="Complete?"
  label(data$cogdif.date)="Cognitive Difficulties - Date of Completion"
  label(data$cogdif01)="CogDiff 1 - I have trouble recalling frequently used phone numbers"
  label(data$cogdif02)="CogDiff 2 - I put down things (glasses, keys, wallet, purse, papers), and have trouble finding them"
  label(data$cogdif03)="CogDiff 3 - When interrupted during reading, I have trouble finding my place again"
  label(data$cogdif04)="CogDiff 4 - I need a written list when I do errands to avoid forgetting things"
  label(data$cogdif05)="CogDiff 5 - I forget appointments, dates, or classes"
  label(data$cogdif06)="CogDiff 6 - I forget to return phone calls"
  label(data$cogdif07)="CogDiff 7 - I have trouble putting my keys into a lock"
  label(data$cogdif08)="CogDiff 8 - I forget errands I planned to do on my way home"
  label(data$cogdif09)="CogDiff 9 - I have trouble recalling names of people I know"
  label(data$cogdif10)="CogDiff 10 - I find it hard to keep my mind on a task or job"
  label(data$cogdif11)="CogDiff 11 - I have trouble describing a program I just watched on tv"
  label(data$cogdif12)="CogDiff 12 - I dont quite say what I mean"
  label(data$cogdif13)="CogDiff 13 - I fail to recognize people I know"
  label(data$cogdif14)="CogDiff 14 - I have trouble getting out information that is at the tip of my tongue"
  label(data$cogdif15)="CogDiff 15 - I have trouble thinking of names of objects"
  label(data$cogdif16)="CogDiff 16 - I find it hard to understand what I read"
  label(data$cogdif17)="CogDiff 17 - I miss the point of what other people are saying"
  label(data$cogdif18)="CogDiff 18 - I forget names of people soon after being introduced"
  label(data$cogdif19)="CogDiff 19 - I lose my train of thought as I listen to somebody else"
  label(data$cogdif20)="CogDiff 20 - I forget steps in recipes I know well and have to look them up"
  label(data$cogdif21)="CogDiff 21 - I forget what day of the week it is"
  label(data$cogdif22)="CogDiff 22 - I forget to button or zip my clothing"
  label(data$cogdif23)="CogDiff 23 - I need to check or double-check whether I locked the door, turned off the stove, etc"
  label(data$cogdif24)="CogDiff 24 - I make mistakes in writing, typing, or operating a calculator"
  label(data$cogdif25)="CogDiff 25 - I cannot keep my mind on one thing"
  label(data$cogdif26)="CogDiff 26 - I need to have instructions repeated several times"
  label(data$cogdif27)="CogDiff 27 - I leave out ingredients when I cook"
  label(data$cogdif28)="CogDiff 28 - I have trouble manipulating buttons, fasteners, scissors, or bottle caps"
  label(data$cogdif29)="CogDiff 29 - I misplace my clothing"
  label(data$cogdif30)="CogDiff 30 - I have trouble sewing or mending"
  label(data$cogdif31)="CogDiff 31 - I find it hard to keep my mind on what Im reading"
  label(data$cogdif32)="CogDiff 32 - I forget right away what people say to me"
  label(data$cogdif33)="CogDiff 33 - When walking or riding, I forget how Ive gotten from one point to another"
  label(data$cogdif34)="CogDiff 34 - I have trouble deciding if I have received correct change"
  label(data$cogdif35)="CogDiff 35 - I forget to pay bills, record checks, or mail letters"
  label(data$cogdif36)="CogDiff 36 - I have to do things very slowly to make sure Im doing them right"
  label(data$cogdif37)="CogDiff 37 - My mind goes blank at times"
  label(data$cogdif38)="CogDiff 38 - I forget the date of the month"
  label(data$cogdif39)="CogDiff 39 - I have trouble using tools"
  label(data$cogdif40)="CogDiff 40 - I worry about my memory or ability to pay attention"
  label(data$cogdif41)="CogDiff 41 - I have trouble with my memory, paying attention, or understanding what others say to me"
  label(data$cogdif.notes)="CogDiff - form notes"
  label(data$cognitive.difficulties.complete)="Complete?"
  label(data$mfq.date)="MFQ - Date of Completion"
  label(data$mfq01)="MFQ 1 - How would you rate your memory in terms of the kinds of problems you have?"
  label(data$mfq02a)="MFQ 2a - Names"
  label(data$mfq02b)="MFQ 2b - Faces"
  label(data$mfq02c)="MFQ 2c - Appointments"
  label(data$mfq02d)="MFQ 2d - Where you put things (e.g., keys)"
  label(data$mfq02e)="MFQ 2e - Performing household chores"
  label(data$mfq02f)="MFQ 2f - Directions to places"
  label(data$mfq02g)="MFQ 2g - Phone numbers youve just checked"
  label(data$mfq02h)="MFQ 2h - Phone numbers you use frequently"
  label(data$mfq02i)="MFQ 2i - Things people tell you"
  label(data$mfq02j)="MFQ 2j - Keeping up correspondence"
  label(data$mfq02k)="MFQ 2k - Personal dates (e.g., birthdays)"
  label(data$mfq02l)="MFQ 2l - Words"
  label(data$mfq02m)="MFQ 2m - Going to store and forgetting what you wanted to buy"
  label(data$mfq02n)="MFQ 2n - Taking a test"
  label(data$mfq02o)="MFQ 2o - Beginning to do something and forgetting what you were doing"
  label(data$mfq02p)="MFQ 2p - Losing the thread of thought in conversation"
  label(data$mfq02q)="MFQ 2q - Losing the thread of thought in public speaking"
  label(data$mfq02r)="MFQ 2r - Knowing whether youve already told someone something"
  label(data$mfq03a)="MFQ 3a - In the opening chapters, once you have finished the book"
  label(data$mfq03b)="MFQ 3b - 3 or 4 chapters before the one you are currently reading"
  label(data$mfq03c)="MFQ 3c - The chapter before the one you are currently reading"
  label(data$mfq03d)="MFQ 3d - The paragraph just before the one you are currently reading"
  label(data$mfq03e)="MFQ 3e - The sentence before the one you are currently reading"
  label(data$mfq04a)="MFQ 4a - In the opening paragraphs, once you have finished the article"
  label(data$mfq04b)="MFQ 4b - 3 or 4 paragraphs before the one you are currently reading"
  label(data$mfq04c)="MFQ 4c - The paragraph before the one you are currently reading"
  label(data$mfq04d)="MFQ 4d - 3 or 4 sentences before the one you are currently reading"
  label(data$mfq04e)="MFQ 4e - the sentence before the one you are currently reading"
  label(data$mfq05a)="MFQ 5a - Last month"
  label(data$mfq05b)="MFQ 5b - Between 6 mos and 1 year ago"
  label(data$mfq05c)="MFQ 5c - Between 1 and 5 years ago"
  label(data$mfq05d)="MFQ 5d - Between 6 and 10 years ago"
  label(data$mfq06a)="MFQ 6a - Names"
  label(data$mfq06b)="MFQ 6b - Faces"
  label(data$mfq06c)="MFQ 6c - Appointments"
  label(data$mfq06d)="MFQ 6d - Where you put things (e.g., keys)"
  label(data$mfq06e)="MFQ 6e - Performing household chores"
  label(data$mfq06f)="MFQ 6f - Directions to places"
  label(data$mfq06g)="MFQ 6g - Phone numbers youve just checked"
  label(data$mfq06h)="MFQ 6h - Phone numbers you use frequently"
  label(data$mfq06i)="MFQ 6i - Things people tell you"
  label(data$mfq06j)="MFQ 6j - Keeping up correspondence"
  label(data$mfq06k)="MFQ 6k - Personal dates (e.g., birthdays)"
  label(data$mfq06l)="MFQ 6l - Words"
  label(data$mfq06m)="MFQ 6m - Going to the store and forgetting what you wanted to buy"
  label(data$mfq06n)="MFQ 6n - Taking a test"
  label(data$mfq06o)="MFQ 6o - Beginning to do something and forgetting what you were doing"
  label(data$mfq06p)="MFQ 6p - Losing the thread of thought in conversation"
  label(data$mfq06q)="MFQ 6q - Losing the thread of thought in public speaking"
  label(data$mfq06r)="MFQ 6r - Knowing whether youve already told someone something"
  label(data$mfq07a)="MFQ 7a - Keep an appointment book"
  label(data$mfq07b)="MFQ 7b - Write yourself reminder notes"
  label(data$mfq07c)="MFQ 7c - Make lists of things to do"
  label(data$mfq07d)="MFQ 7d - Make grocery lists"
  label(data$mfq07e)="MFQ 7e - Plan your daily schedule in advance"
  label(data$mfq07f)="MFQ 7f - Mental repetition"
  label(data$mfq07g)="MFQ 7g - Associations with other things"
  label(data$mfq07h)="MFQ 7h - Keep things you need to do in a prominent place where you will notice them"
  label(data$mfq08a)="MFQ 8a - 1 year ago"
  label(data$mfq08b)="MFQ 8b - 5 years ago"
  label(data$mfq08c)="MFQ 8c - 10 years ago"
  label(data$mfq08d)="MFQ 8d - 20 years ago"
  label(data$mfq08e)="MFQ 8e - When you were 18"
  label(data$mem.mnemonics)="MFQ - Mnemonics Usage Total"
  label(data$mem.mnemonics.avg)="MFQ - Mnemonics Usage Average"
  label(data$mem.notes)="MFQ - form notes"
  label(data$memory.functioning.questionnaire.complete)="Complete?"
  label(data$ecogself.date)="ECog Self - Date of Completion"
  label(data$ecogself.mem01)="ECog Self - Mem 1 - Remembering a few shopping items without a list"
  label(data$ecogself.mem02)="ECog Self - Mem 2 - Remembering things that happened recently (such as recent outings, events in the news)"
  label(data$ecogself.mem03)="ECog Self - Mem 3 - Recalling conversations a few days later"
  label(data$ecogself.mem04)="ECog Self - Mem 4 - Remembering where I have placed objects"
  label(data$ecogself.mem05)="ECog Self - Mem 5 - Repeating stories and/or questions"
  label(data$ecogself.mem06)="ECog Self - Mem 6 - Remembering current date or day of the week"
  label(data$ecogself.mem07)="ECog Self - Mem 7 - Remembering I have already told someone something"
  label(data$ecogself.mem08)="ECog Self - Mem 8 - Remembering appointments, meetings, or engagements"
  label(data$ecogself.lang01)="ECog Self - Lang 1 - Forgetting the names of objects"
  label(data$ecogself.lang02)="ECog Self - Lang 2 - Verbally giving instructions to others"
  label(data$ecogself.lang03)="ECog Self - Lang 3 - Finding the right words to use in a conversation"
  label(data$ecogself.lang04)="ECog Self - Lang 4 - Communicating thoughts in a conversation"
  label(data$ecogself.lang05)="ECog Self - Lang 5 - Following a story in a book or on TV"
  label(data$ecogself.lang06)="ECog Self - Lang 6 - Understanding the point of what other people are trying to say"
  label(data$ecogself.lang07)="ECog Self - Lang 7 - Remembering the meaning of common words"
  label(data$ecogself.lang08)="ECog Self - Lang 8 - Describing a program I have watched on TV"
  label(data$ecogself.lang09)="ECog Self - Lang 9 - Understanding spoken directions or instructions"
  label(data$ecogself.vis01)="ECog Self - Percept 1 - Following a map to find a new location"
  label(data$ecogself.vis02)="ECog Self - Percept 2 - Reading a map and helping with directions when someone else is driving"
  label(data$ecogself.vis03)="ECog Self - Percept 3 - Finding my car in a parking lot"
  label(data$ecogself.vis04)="ECog Self - Percept 4 - Finding the way back to a meeting spot in the mall or other location"
  label(data$ecogself.vis05)="ECog Self - Percept 5 - Finding my way around a familiar neighborhood"
  label(data$ecogself.vis06)="ECog Self - Percept 6 - Finding my way around a familiar store"
  label(data$ecogself.vis07)="ECog Self - Percept 7 - Finding my way around a house visited many times"
  label(data$ecogself.plan01)="ECog Self - Plan 1 - Planning the sequence of stops on a shopping trip"
  label(data$ecogself.plan02)="ECog Self - Plan 2 - Ability to anticipate weather changes and plan accordingly"
  label(data$ecogself.plan03)="ECog Self - Plan 3 - Developing a schedule in advance of anticipated events"
  label(data$ecogself.plan04)="ECog Self - Plan 4 - Thinking things through before acting"
  label(data$ecogself.plan05)="ECog Self - Plan 5 - Thinking ahead"
  label(data$ecogself.org01)="ECog Self - Org 1 - Keeping living and work space organized"
  label(data$ecogself.org02)="ECog Self - Org 2 - Balancing the checkbook without error"
  label(data$ecogself.org03)="ECog Self - Org 3 - Keeping financial records organized"
  label(data$ecogself.org04)="ECog Self - Org 4 - Prioritizing tasks by importance"
  label(data$ecogself.org05)="ECog Self - Org 5 - Keeping mail and papers organized"
  label(data$ecogself.org06)="ECog Self - Org 6 - Using an organized strategy to manage a medication schedule involving multiple medications"
  label(data$ecogself.attn01)="ECog Self - Att 1 - The ability to do two things at once"
  label(data$ecogself.attn02)="ECog Self - Att 2 - Returning to a task after being interrupted"
  label(data$ecogself.attn03)="ECog Self - Att 3 - The ability to concentrate on a task without being distracted by external things in the environment"
  label(data$ecogself.notes)="ECog Self - form notes"
  label(data$everyday.cognition.self.complete)="Complete?"
  label(data$ecoginf.date)="ECog Informant - Date of Completion"
  label(data$ecoginf.mem01)="ECog Informant - Mem 1 - Remembering a few shopping items without a list"
  label(data$ecoginf.mem02)="ECog Informant - Mem 2 - Remembering things that happened recently (such as recent outings, events in the news)"
  label(data$ecoginf.mem03)="ECog Informant - Mem 3 - Recalling conversations a few days later"
  label(data$ecoginf.mem04)="ECog Informant - Mem 4 - Remembering where I have placed objects"
  label(data$ecoginf.mem05)="ECog Informant - Mem 5 - Repeating stories and/or questions"
  label(data$ecoginf.mem06)="ECog Informant - Mem 6 - Remembering current date or day of the week"
  label(data$ecoginf.mem07)="ECog Informant - Mem 7 - Remembering s/he has already told someone something"
  label(data$ecoginf.mem08)="ECog Informant - Mem 8 - Remembering appointments, meetings, or engagements"
  label(data$ecoginf.lang01)="ECog Informant - Lang 1 - Forgetting the names of objects"
  label(data$ecoginf.lang02)="ECog Informant - Lang 2 - Verbally giving instructions to others"
  label(data$ecoginf.lang03)="ECog Informant - Lang 3 - Finding the right words to use in a conversation"
  label(data$ecoginf.lang04)="ECog Informant - Lang 4 - Communicating thoughts in a conversation"
  label(data$ecoginf.lang05)="ECog Informant - Lang 5 - Following a story in a book or on TV"
  label(data$ecoginf.lang06)="ECog Informant - Lang 6 - Understanding the point of what other people are trying to say"
  label(data$ecoginf.lang07)="ECog Informant - Lang 7 - Remembering meaning of common words"
  label(data$ecoginf.lang08)="ECog Informant - Lang 8 - Describing a program s/he has watched on TV"
  label(data$ecoginf.lang09)="ECog Informant - Lang 9 - Understanding spoken directions or instructions"
  label(data$ecoginf.vis01)="ECog Informant - Percept 1 - Following a map to find a new location"
  label(data$ecoginf.vis02)="ECog Informant - Percept 2 - Reading a map and helping with directions when someone else is driving"
  label(data$ecoginf.vis03)="ECog Informant - Percept 3 - Finding ones car in a parking lot"
  label(data$ecoginf.vis04)="ECog Informant - Percept 4 - Finding the way back to a meeting spot in mall or other location"
  label(data$ecoginf.vis05)="ECog Informant - Percept 5 - Finding his/her way around a familiar neighborhood"
  label(data$ecoginf.vis06)="ECog Informant - Percept 6 - Finding his/her way around a familiar store"
  label(data$ecoginf.vis07)="ECog Informant - Percept 7 - Finding his/her way around a house visited many times"
  label(data$ecoginf.plan01)="ECog Informant - Plan 1 - Planning the sequence of stops on a shopping trip"
  label(data$ecoginf.plan02)="ECog Informant - Plan 2 - The ability to anticipate weather changes and plan accordingly (i.e., bring a coat or umbrella)"
  label(data$ecoginf.plan03)="ECog Informant - Plan 3 - Developing a schedule in advance of anticipated events"
  label(data$ecoginf.plan04)="ECog Informant - Plan 4 - Thinking things through before acting"
  label(data$ecoginf.plan05)="ECog Informant - Plan 5 - Thinking ahead"
  label(data$ecoginf.org01)="ECog Informant - Org 1 - Keeping living and work space organized"
  label(data$ecoginf.org02)="ECog Informant - Org 2 - Balancing the checkbook without error"
  label(data$ecoginf.org03)="ECog Informant - Org 3 - Keeping financial records organized"
  label(data$ecoginf.org04)="ECog Informant - Org 4 - Prioritizing tasks by importance"
  label(data$ecoginf.org05)="ECog Informant - Org 5 - Keeping mail and papers organized"
  label(data$ecoginf.org06)="ECog Informant - Org 6 - Using an organized strategy to manage a medication schedule involving multiple medications"
  label(data$ecoginf.attn01)="ECog Informant - Att 1 - The ability to do two things at once"
  label(data$ecoginf.attn02)="ECog Informant - Att 2 - Returning to a task after being interrupted"
  label(data$ecoginf.attn03)="ECog Informant - Att 3 - The ability to concentrate on a task without being distracted by external things in the environment"
  label(data$ecoginf.attn04)="ECog Informant - Att 4 - Cooking or working and talking at the same time"
  label(data$ecoginf)="ECog Informant - Total Score"
  label(data$ecoginf.prorate)="ECog Informant - Prorated Total"
  label(data$ecoginf.mem)="ECog Informant - Memory Total"
  label(data$ecoginf.lang)="ECog Informant - Language Total"
  label(data$ecoginf.vis)="ECog Informant - Visuospatial Total"
  label(data$ecoginf.plan)="ECog Informant - Planning Total"
  label(data$ecoginf.org)="ECog Informant - Organization Total"
  label(data$ecoginf.attn)="ECog Informant - Attention Total"
  label(data$ecoginf.notes)="ECog Informant - form notes"
  label(data$everyday.cognition.informant.complete)="Complete?"
  label(data$fcadl.date)="FCADL - Date of Completion"
  label(data$fcadl01)="FCADL 1 - Is unable to read"
  label(data$fcadl02)="FCADL 2 - Wanders outside the house and gets lost"
  label(data$fcadl03)="FCADL 3 - Misses content of the newspaper article or television"
  label(data$fcadl04)="FCADL 4 - Seems disoriented and confused"
  label(data$fcadl05)="FCADL 5 - Accuses others of taking things"
  label(data$fcadl06)="FCADL 6 - Has difficulty dressing and undressing him/herself"
  label(data$fcadl07)="FCADL 7 - Bumps into objects"
  label(data$fcadl08)="FCADL 8 - Loses track of time"
  label(data$fcadl09)="FCADL 9 - Gets lost in unfamiliar places"
  label(data$fcadl10)="FCADL 10 - Misplaces objects"
  label(data$fcadl11)="FCADL 11 - Acts as if he or she were in a dream world"
  label(data$fcadl12)="FCADL 12 - Is unable to feed him/herself"
  label(data$fcadl13)="FCADL 13 - Gets lost in familiar places"
  label(data$fcadl14)="FCADL 14 - Has difficulty recognizing faces of people he or she has only met once or twice"
  label(data$fcadl15)="FCADL 15 - Gets mixed up about where s/he is"
  label(data$fcadl16)="FCADL 16 - Cannot watch and follow television programs"
  label(data$fcadl17)="FCADL 17 - Does not look both ways before crossing the street"
  label(data$fcadl18)="FCADL 18 - Does not use tools (e.g., hammer, spoon, remote control) for proposed use"
  label(data$fcadl19)="FCADL 19 - Has difficulty driving"
  label(data$fcadl20)="FCADL 20 - Does not recognize emotional expressions on face of others"
  label(data$fcadl21)="FCADL 21 - Does not keep him/herself busy by doing useful things"
  label(data$fcadl22)="FCADL 22 - Forgets and leaves the stove turned on"
  label(data$fcadl23)="FCADL 23 - Becomes tired for no apparent reason"
  label(data$fcadl24)="FCADL 24 - Cannot write a letter or make a drawing"
  label(data$fcadl25)="FCADL 25 - Does not understand what is said to him or her"
  label(data$fcadl26)="FCADL 26 - Talks without making any sense"
  label(data$fcadl27)="FCADL 27 - Does not recognize faces of familiar people (e.g., family members, President)"
  label(data$fcadl28)="FCADL 28 - Cries often and for no apparent reason"
  label(data$fcadl29)="FCADL 29 - Forgets where s/he is"
  label(data$fcadl30)="FCADL 30 - Hears things that are not there"
  label(data$fcadl31)="FCADL 31 - Cannot read a clock to tell time"
  label(data$fcadl32)="FCADL 32 - Cannot concentrate on one thing"
  label(data$fcadl33)="FCADL 33 - Has a loss of appetite"
  label(data$fcadl34)="FCADL 34 - Sees people or things that arent there"
  label(data$fcadl35)="FCADL 35 - Does or says the same thing over and over again without reason"
  label(data$fcadl36)="FCADL 36 - Avoids certain social situations"
  label(data$fcadl37)="FCADL 37 - Misjudges distances when reaching for things"
  label(data$fcadl38)="FCADL 38 - Is irritable and easily upset"
  label(data$fcadl39)="FCADL 39 - Mistakes pictures and television characters for real people"
  label(data$fcadl40)="FCADL 40 - Has trouble falling asleep"
  label(data$fcadl41)="FCADL 41 - Thinks people are talking about him or her"
  label(data$fcadl42)="FCADL 42 - Has trouble recalling familiar phone numbers"
  label(data$fcadl43)="FCADL 43 - Confuses one object for another"
  label(data$fcadl44)="FCADL 44 - Is more confused at night than during the day"
  label(data$fcadl45)="FCADL 45 - Forgets details of recent events"
  label(data$fcadl46)="FCADL 46 - Is losing weight"
  label(data$fcadl47)="FCADL 47 - Has difficulty walking"
  label(data$fcadl48)="FCADL 48 - Seems frightened of objects, people, or situations that are not harmful"
  label(data$fcadl49)="FCADL 49 - Has poor memory for events from past"
  label(data$fcadl50)="FCADL 50 - Has developed poor personal habits"
  label(data$fcadl.prorate)="FCADL Prorated Score"
  label(data$fcadl.notes)="FCADL - form notes"
  label(data$functional.capacities.for.adls.complete)="Complete?"
  label(data$food.date)="NCI Quick Food Scan - Date of Completion"
  label(data$food01a)="Food 1a - Cold cereal"
  label(data$food01b)="Food 1b - Skim milk on cereal or to drink"
  label(data$food01c)="Food 1c - Eggs, fried or scrambled in margarine, butter, or oil"
  label(data$food01d)="Food 1d - Sausage or bacon, regular-fat"
  label(data$food01e)="Food 1e - Margarine or butter on bread, rolls, pancakes"
  label(data$food01f)="Food 1f - Orange juice or grapefruit juice"
  label(data$food01g)="Food 1g - Fruit (not juices)"
  label(data$food01h)="Food 1h - Beef or pork hot dogs, regular-fat"
  label(data$food01i)="Food 1i - Cheese or cheese spread, regular-fat"
  label(data$food01j)="Food 1j - French fries, home fries, or hash brown potatoes"
  label(data$food01k)="Food 1k - Margarine or butter on vegetables, including potatoes"
  label(data$food01l)="Food 1l - Mayonnaise, regular-fat"
  label(data$food01m)="Food 1m - Salad dressings, regular-fat"
  label(data$food01n)="Food 1n - Rice"
  label(data$food01o)="Food 1o - Margarine, butter, or oil on rice or pasta"
  label(data$food02)="Food 2 - Over the past 12 mos, when you prepared foods with margarine or ate margarine, how often did you use reduced-fat margarine?"
  label(data$food03)="Food 3 - Overall, when you think about foods you ate over the past 12 mos, would you say your diet was high, medium, or low in fat?"
  label(data$nci.quick.food.scan.complete)="Complete?"
  label(data$mlta.date)="MLTA - Date of Completion"
  label(data$mlta01)="MLTA 1 - Walking for pleasure or exercise"
  label(data$mlta02)="MLTA 2 - Chores: in the home (moderate)"
  label(data$mlta03)="MLTA 3 - Chores: outside"
  label(data$mlta04)="MLTA 4 - Mowing the lawn: rider mower"
  label(data$mlta05)="MLTA 5 - Mowing the lawn: walking behind power"
  label(data$mlta06)="MLTA 6 - Mowing the lawn: push mower"
  label(data$mlta07)="MLTA 7 - Raking the yard"
  label(data$mlta08)="MLTA 8 - Gardening: weeding /cultivation"
  label(data$mlta09)="MLTA 9 - Gardening: digging"
  label(data$mlta10)="MLTA 10 - Jogging"
  label(data$mlta11)="MLTA 11 - Biking for pleasure"
  label(data$mlta12)="MLTA 12 - Exercise cycling"
  label(data$mlta13)="MLTA 13 - Dancing"
  label(data$mlta14)="MLTA 14 - Aerobics: home"
  label(data$mlta15)="MLTA 15 - Aerobics: health club"
  label(data$mlta16)="MLTA 16 - Bowling"
  label(data$mlta17)="MLTA 17 - Golf: driving cart"
  label(data$mlta18)="MLTA 18 - Golf: walking /clubs on cart"
  label(data$mlta19)="MLTA 19 - Golf: walking and carrying clubs"
  label(data$mlta20)="MLTA 20 - Tennis singles"
  label(data$mlta21)="MLTA 21 - Tennis doubles"
  label(data$mlta22)="MLTA 22 - Racquet ball"
  label(data$mlta23)="MLTA 23 - Calisthenics"
  label(data$mlta24)="MLTA 24 - Swimming"
  label(data$mlta25)="MLTA 25 - Hiking"
  label(data$mlta.notes)="MLTA - form notes"
  label(data$minnesota.leisure.time.activities.complete)="Complete?"
  label(data$champs.date)="CHAMPS - Date of Completion"
  label(data$champs01.wk)="CHAMPS 1 - visit with friends or family other than those you live with (times/wk)"
  label(data$champs02.wk)="CHAMPS 2 - Go to senior center (times/wk)"
  label(data$champs03.wk)="CHAMPS 3 - Do volunteer work (times/wk)"
  label(data$champs04.wk)="CHAMPS 4 - Attend church or take part in church activities (times/wk)"
  label(data$champs05.wk)="CHAMPS 5 - Attend other club or group meetings (times/wk)"
  label(data$champs06.wk)="CHAMPS 6 - Use a computer (times/wk)"
  label(data$champs07.wk)="CHAMPS 7 - Dance (e.g., square, folk, line, ballroom - do not count aerobic dance here) (times/wk)"
  label(data$champs08.wk)="CHAMPS 8 - Do woodworking, needlework, drawing, or other arts/crafts (times/wk)"
  label(data$champs09.wk)="CHAMPS 9 - Play golf, carrying or pulling your equipment (count walking time only) (times/wk)"
  label(data$champs10.wk)="CHAMPS 10 - Play golf, riding a cart (count walking time only) (times/wk)"
  label(data$champs11.wk)="CHAMPS 11 - Attend concert, movie, lecture, or sport event (times/wk)"
  label(data$champs12.wk)="CHAMPS 12 - Play cards, bingo, or other board games with other people (times/wk)"
  label(data$champs13.wk)="CHAMPS 13 - Shoot pool or billiards (times/wk)"
  label(data$champs14.wk)="CHAMPS 14 - Play singles tennis (do not count doubles) (times/wk)"
  label(data$champs15.wk)="CHAMPS 15 - Play doubles tennis (do not count singles) (times/wk)"
  label(data$champs16.wk)="CHAMPS 16 - Skate (ice, roller, in-line) (times/wk)"
  label(data$champs17.wk)="CHAMPS 17 - Play a musical instrument (times/wk)"
  label(data$champs18.wk)="CHAMPS 18 - Read (times/wk)"
  label(data$champs19.wk)="CHAMPS 19 - Do heavy work around the house (e.g., washing windows, cleaning gutters) (times/wk)"
  label(data$champs20.wk)="CHAMPS 20 - Do light work around house (e.g., sweeping, vacuuming) (times/wk)"
  label(data$champs21.wk)="CHAMPS 21 - Do heavy gardening (e.g., spading, raking) (times/wk)"
  label(data$champs22.wk)="CHAMPS 22 - Do light gardening (e.g., watering plants) (times/wk)"
  label(data$champs23.wk)="CHAMPS 23 - Work on your car, truck, lawn mower, or other machinery (times/wk)"
  label(data$champs24.wk)="CHAMPS 24 - Jog or run (times/wk)"
  label(data$champs25.wk)="CHAMPS 25 - Walk uphill or hike uphill (count only uphill part) (times/wk)"
  label(data$champs26.wk)="CHAMPS 26 - Walk fast or briskly for exercise (do not count walking leisurely or uphill) (times/wk)"
  label(data$champs27.wk)="CHAMPS 27 - Walk to do errands e.g., to/from store or to take children to school (count walk time only) (times/wk)"
  label(data$champs28.wk)="CHAMPS 28 - Walk leisurely for exercise or pleasure (times/wk)"
  label(data$champs29.wk)="CHAMPS 29 - Ride a bicycle or stationary cycle (times/wk)"
  label(data$champs30.wk)="CHAMPS 30 - Do other aerobic machines, e.g., rowing or step machines (do not count treadmill or stationary cycle) (times/wk)"
  label(data$champs31.wk)="CHAMPS 31 - Do water exercises (do not count other swimming) (times/wk)"
  label(data$champs32.wk)="CHAMPS 32 - Swim moderately or fast (times/wk)"
  label(data$champs33.wk)="CHAMPS 33 - Swim gently (times/wk)"
  label(data$champs34.wk)="CHAMPS 34 - Do stretching or flexibility exercises (do not count yoga or tai-chi) (times/wk)"
  label(data$champs35.wk)="CHAMPS 35 - Do yoga or tai-chi (times/wk)"
  label(data$champs36.wk)="CHAMPS 36 - Do aerobics or aerobic dancing (times/wk)"
  label(data$champs37.wk)="CHAMPS 37 - Do moderate to heavy strength training (e.g., hand-held weights of >5 lbs, weight machines, or push-ups) (times/wk)"
  label(data$champs38.wk)="CHAMPS 38 - Do light strength training (e.g., hand-held weights of 5 lbs or less or elastic bands) (times/wk)"
  label(data$champs39.wk)="CHAMPS 39 - Do general conditioning exercises, e.g., light calisthenics or chair exercises (do not count strength training) (times/wk)"
  label(data$champs40.wk)="CHAMPS 40 - Play basketball, soccer, or racquetball (do not count time on sidelines) (times/wk)"
  label(data$champs41.wk.other1)="CHAMPS 41 - Do other types of physical activity not previously mentioned?"
  label(data$champs41.wk.other1.descrip)="CHAMPS 41 - Other activity 1 - description"
  label(data$champs41.wk.other1.times)="CHAMPS 41 - Other activity 1 (times/wk)"
  label(data$champs41.wk.other2)="CHAMPS 41 - Add another activity not previously mentioned?"
  label(data$champs41.wk.other2.descrip)="CHAMPS 41 - Other activity 2 - description"
  label(data$champs41.wk.other2.times)="CHAMPS 41 - Other activity 2 (times/wk)"
  label(data$champs41.wk.other3)="CHAMPS 41 - Add another activity not previously mentioned?"
  label(data$champs41.wk.other3.descrip)="CHAMPS 41 - Other activity 3 - description"
  label(data$champs41.wk.other3.times)="CHAMPS 41 - Other activity 3 (times/wk)"
  label(data$champs01.hrs)="CHAMPS 1 - Visit with friends or family (other than those you live with) (hrs/wk)"
  label(data$champs02.hrs)="CHAMPS 2 - Go to senior center (hrs/wk)"
  label(data$champs03.hrs)="CHAMPS 3 - Do volunteer work (hrs/wk)"
  label(data$champs04.hrs)="CHAMPS 4 - Attend church or take part in church activities (hrs/wk)"
  label(data$champs05.hrs)="CHAMPS 5 - Attend other club or group meetings (hrs/wk)"
  label(data$champs06.hrs)="CHAMPS 6 - Use a computer (hrs/wk)"
  label(data$champs07.hrs)="CHAMPS 7 - Dance (e.g., square, folk, line, ballroom - do not count aerobic dance here) (hrs/wk)"
  label(data$champs08.hrs)="CHAMPS 8 - Do woodworking, needlework, drawing, or other arts and crafts (hrs/wk)"
  label(data$champs09.hrs)="CHAMPS 9 - Play golf, carrying or pulling your equipment (count walking time only) (hrs/wk)"
  label(data$champs10.hrs)="CHAMPS 10 - Play golf, riding a cart (count walking time only) (hrs/wk)"
  label(data$champs11.hrs)="CHAMPS 11 - Attend a concert, movie, lecture, or sport event (hrs/wk)"
  label(data$champs12.hrs)="CHAMPS 12 - Play cards, bingo, or other board games with other people (hrs/wk)"
  label(data$champs13.hrs)="CHAMPS 13 - Shoot pool or billiards (hrs/wk)"
  label(data$champs14.hrs)="CHAMPS 14 - Play singles tennis (do not count doubles) (hrs/wk)"
  label(data$champs15.hrs)="CHAMPS 15 - Play doubles tennis (do not count singles) (hrs/wk)"
  label(data$champs16.hrs)="CHAMPS 16 - Skate (ice, roller, in-line) (hrs/wk)"
  label(data$champs17.hrs)="CHAMPS 17 - Play a musical instrument (hrs/wk)"
  label(data$champs18.hrs)="CHAMPS 18 - Read (hrs/wk)"
  label(data$champs19.hrs)="CHAMPS 19 - Do heavy work around the house (e.g., washing windows, cleaning gutters) (hrs/wk)"
  label(data$champs20.hrs)="CHAMPS 20 - Do light work around house (e.g., sweeping, vacuuming) (hrs/wk)"
  label(data$champs21.hrs)="CHAMPS 21 - Do heavy gardening (e.g., spading, raking) (hrs/wk)"
  label(data$champs22.hrs)="CHAMPS 22 - Do light gardening (e.g., watering plants) (hrs/wk)"
  label(data$champs23.hrs)="CHAMPS 23 - Work on car, truck, lawn mower, or other machinery (hrs/wk)"
  label(data$champs24.hrs)="CHAMPS 24 - Jog or run (hrs/wk)"
  label(data$champs25.hrs)="CHAMPS 25 - Walk uphill or hike uphill (count only uphill part) (hrs/wk)"
  label(data$champs26.hrs)="CHAMPS 26 - Walk fast or briskly for exercise (do not count walking leisurely or uphill) (hrs/wk)"
  label(data$champs27.hrs)="CHAMPS 27 - Walk to do errands, e.g., to/from store or to take children to school (count walk time only) (hrs/wk)"
  label(data$champs28.hrs)="CHAMPS 28 - Walk leisurely for exercise or pleasure (hrs/wk)"
  label(data$champs29.hrs)="CHAMPS 29 - Ride a bicycle or stationary cycle (hrs/wk)"
  label(data$champs30.hrs)="CHAMPS 30 - Do other aerobic machine, e.g., rowing or step machines (do not count treadmill or stationary cycle) (hrs/wk)"
  label(data$champs31.hrs)="CHAMPS 31 - Do water exercises (do not count other swimming) (hrs/wk)"
  label(data$champs32.hrs)="CHAMPS 32 - Swim moderately or fast (hrs/wk)"
  label(data$champs33.hrs)="CHAMPS 33 - Swim gently (hrs/wk)"
  label(data$champs34.hrs)="CHAMPS 34 - Do stretching or flexibility exercises (do not count yoga or tai-chi) (hrs/wk)"
  label(data$champs35.hrs)="CHAMPS 35 - Do yoga or tai-chi (hrs/wk)"
  label(data$champs36.hrs)="CHAMPS 36 - Do aerobics or aerobic dancing (hrs/wk)"
  label(data$champs37.hrs)="CHAMPS 37 - Do moderate to heavy strength training (e.g., hand-held weights of more than 5 lbs, weight machines, or push-ups) (hrs/wk)"
  label(data$champs38.hrs)="CHAMPS 38 - Do light strength training (e.g., hand-held weights of 5 lbs or less or elastic bands) (hrs/wk)"
  label(data$champs39.hrs)="CHAMPS 39 - Do general conditioning exercises, e.g., light calisthenics or chair exercises (do not count strength training) (hrs/wk)"
  label(data$champs40.hrs)="CHAMPS 40 - Play basketball, soccer, or racquetball (do not count time on sidelines) (hrs/wk)"
  label(data$champs41.other1.hrs)="CHAMPS 41 - Other activity 1 (hrs/wk)"
  label(data$champs41.other2.hrs)="CHAMPS 41 - Other activity 2 (hrs/wk)"
  label(data$champs41.other3.hrs)="CHAMPS 41 - Other activity 3 (hrs/wk)"
  label(data$champs.notes)="CHAMPS - form notes"
  label(data$champs.complete)="Complete?"
  label(data$psqi.date)="PSQI - Date of Completion"
  label(data$psqi.completer)="PSQI - Person completing form"
  label(data$psqi.bedtime)="PSQI 1 - During the past month, what time have you usually gone to bed at night? (military time)"
  label(data$psqi.time.sleep)="PSQI 2 - During the past month, how long (in minutes) has it usually taken you to fall asleep each night?"
  label(data$psqi.waketime)="PSQI 3 - During the past month, what time have you usually gotten up in the morning? (military time)"
  label(data$psqi.hrs.sleep)="PSQI 4 - During the past month, how many hours of actual sleep did you get at night?"
  label(data$psqi.cant.sleep)="PSQI 5a - Cannot get to sleep within 30 minutes"
  label(data$psqi.wake.up)="PSQI 5b - Wake up in the middle of the night or early morning"
  label(data$psqi.bathroom)="PSQI 5c - Have to get up to use the bathroom"
  label(data$psqi.breathe)="PSQI 5d - Cannot breathe comfortably"
  label(data$psqi.cough.snore)="PSQI 5e - Cough or snore loudly"
  label(data$psqi.cold)="PSQI 5f - Feel too cold"
  label(data$psqi.hot)="PSQI 5g - Feel too hot"
  label(data$psqi.dreams)="PSQI 5h - Had bad dreams"
  label(data$psqi.pain)="PSQI 5i - Have pain"
  label(data$psqi.other)="PSQI 5j - Other reason(s), please describe"
  label(data$psqi.other.often)="PSQI 5j - How often during the past month have you had trouble sleeping because of this?"
  label(data$psqi.sleep.quality)="PSQI 6 - During the past month, how would you rate your sleep quality overall?"
  label(data$psqi.medicine)="PSQI 7 - During the past month, how often have you taken medicine to help you sleep (prescribed or over the counter)?"
  label(data$psqi.awake)="PSQI 8 - During the past month, how often have you had trouble staying awake while driving, eating meals, or engaging in social activity?"
  label(data$psqi.enthusiasm)="PSQI 9 - During the past month, how much of a problem has it been for you to keep up enough enthusiasm to get things done?"
  label(data$psqi.partner)="PSQI 10 - Do you have a bed partner or room mate?"
  label(data$psqi.loud.snoring)="PSQI 10a - Loud snoring"
  label(data$psqi.pauses.breath)="PSQI 10b - Long pauses between breaths while asleep"
  label(data$psqi.twitching)="PSQI 10c - Legs twitching or jerking while you sleep"
  label(data$psqi.disorientation)="PSQI 10d - Episodes of disorientation or confusion during sleep"
  label(data$psqi.restlessness)="PSQI 10e - Other restlessness while you sleep, please describe"
  label(data$psqi.restlessness.often)="PSQI 10e - How often in the past month has this been a problem?"
  label(data$psqi.notes)="PSQI - form notes"
  label(data$pittsburgh.sleep.quality.index.complete)="Complete?"
  label(data$qids.date)="QIDS - Date of Completion"
  label(data$qids01)="QIDS 1 - Falling asleep"
  label(data$qids02)="QIDS 2 - Sleep during the night"
  label(data$qids03)="QIDS 3 - Waking up too early"
  label(data$qids04)="QIDS 4 - Sleeping too much"
  label(data$qids05)="QIDS 5 - Feeling sad"
  label(data$qids06)="QIDS 6 - Decreased appetite"
  label(data$qids07)="QIDS 7 - Increased appetite"
  label(data$qids08)="QIDS 8 - Decreased weight"
  label(data$qids09)="QIDS 9 - Increased weight"
  label(data$qids10)="QIDS 10 - Concentration/decision making"
  label(data$qids11)="QIDS 11 - View of myself"
  label(data$qids12)="QIDS 12 - Thoughts of death or suicide"
  label(data$qids13)="QIDS 13 - General interest"
  label(data$qids14)="QIDS 14 - Energy level"
  label(data$qids15)="QIDS 15 - Feeling slowed down"
  label(data$qids16)="QIDS 16 - Feeling restless"
  label(data$qids.sleep)="QIDS Sleep Domain Score"
  label(data$qids.weight)="QIDS Weight Domain Score"
  label(data$qids.motor)="QIDS Psychomotor Changes Score"
  label(data$qids.depress)="QIDS Depressed Mood Score"
  label(data$qids.interest)="QIDS Decreased Interest Score"
  label(data$qids.fatigue)="QIDS Fatigue Score"
  label(data$qids.guilt)="QIDS Guilt Score"
  label(data$qids.concen)="QIDS Concentration Score"
  label(data$qids.suicide)="QIDS Suicidal Ideation Score"
  label(data$qids.notes)="QIDS - form notes"
  label(data$qids.complete)="Complete?"
  label(data$gds.date)="GDS - Date of Completion"
  label(data$gds1)="GDS 1 - Are you basically satisfied with your life"
  label(data$gds2)="GDS 2 - Have you dropped many of your activities and interests"
  label(data$gds3)="GDS 3 - Do you feel that your life is empty"
  label(data$gds4)="GDS 4 - Do you often get bored"
  label(data$gds5)="GDS 5 - Are you hopeful about the future"
  label(data$gds6)="GDS 6 - Are you bothered by thoughts you cant get out of your head"
  label(data$gds7)="GDS 7 - Are you in good spirits most of the time"
  label(data$gds8)="GDS 8 - Are you afraid that something bad is going to happen to you"
  label(data$gds9)="GDS 9 - Do you feel happy most of the time"
  label(data$gds10)="GDS 10 - Do you often feel helpless"
  label(data$gds11)="GDS 11 - Do you often get restless and fidgety"
  label(data$gds12)="GDS 12 - Do you prefer to stay at home rather than going out and doing new things"
  label(data$gds13)="GDS 13 - Do you frequently worry about the future"
  label(data$gds14)="GDS 14 - Do you feel you have more problems with your memory than most"
  label(data$gds15)="GDS 15 - Do you think it is wonderful to be alive now"
  label(data$gds16)="GDS 16 - Do you often feel downhearted and blue"
  label(data$gds17)="GDS 17 - Do you feel pretty worthless the way you are now"
  label(data$gds18)="GDS 18 - Do you worry a lot about the past"
  label(data$gds19)="GDS 19 - Do you find life very exciting"
  label(data$gds20)="GDS 20 - Is it hard for you to get started on new projects"
  label(data$gds21)="GDS 21 - Do you feel full of energy"
  label(data$gds22)="GDS 22 - Do you feel that your situation is hopeless"
  label(data$gds23)="GDS 23 - Do you think that most people are better off than you are"
  label(data$gds24)="GDS 24 - Do you frequently get upset over little things"
  label(data$gds25)="GDS 25 - Do you frequently feel like crying"
  label(data$gds26)="GDS 26 - Do you have trouble concentrating"
  label(data$gds27)="GDS 27 - Do you enjoy getting up in the morning"
  label(data$gds28)="GDS 28 - Do you prefer to avoid social gatherings"
  label(data$gds29)="GDS 29 - Is it easy for you to make decisions"
  label(data$gds30)="GDS 30 - Is your mind as clear as it used to be"
  label(data$gds.notes)="GDS administrative issues"
  label(data$gds.complete)="Complete?"
  label(data$phys.date)="Physical Exam - Date of Completion"
  label(data$phys.time)="Physical Exam - acquisition time (military)"
  label(data$height)="Physical Exam - height (cm)"
  label(data$weight)="Physical Exam - weight (kg)"
  label(data$waist)="Physical Exam - waist circumference (cm)"
  label(data$hip)="Physical Exam - hip circumference (cm)"
  label(data$bldfast.last.ate.date)="Fasting Compliance - date last ate"
  label(data$bldfast.last.ate.time)="Fasting Compliance - time last ate (military time)"
  label(data$bldfast.last.ate.what)="Fasting Compliance - what ate last"
  label(data$bldast.waking)="Fasting Compliance - did participate eat since waking up?"
  label(data$frail.grip01)="Frailty Assess - grip strength trial 1"
  label(data$frail.grip02)="Frailty Assess - grip strength trial 2"
  label(data$frail.grip03)="Frailty Assess - grip strength trial 3"
  label(data$frail.gripbest)="Frailty Assess - grip strength best of 3 trials"
  label(data$frail.gait)="Frailty Assess - gate speed time (units?)"
  label(data$frail.weightloss)="Frailty Assess - recent weight loss (10 lb or >10% weight loss over last 6 mos)"
  label(data$frailt.effort)="Frailty Assess - effort (CESD 7 - I felt that everything I did was an effort)"
  label(data$frail.going)="Frailty Assess - going (CESD 20 - I felt I could not get going)"
  label(data$phys.notes)="Physical Exam - form notes"
  label(data$physical.and.frailty.assessment.complete)="Complete?"
  label(data$echo.date)="Echo - date"
  label(data$echo.time)="Echo - time (military)"
  label(data$echo.read)="Echo - reader"
  label(data$echo.sonog)="Echo - sonographer"
  label(data$echo.sbp)="Echo - systolic bp - 1st measurement"
  label(data$echo.dbp)="Echo - diastolic bp - 1st measurement"
  label(data$echo.sbp2)="Echo - systolic bp - 2nd measurement"
  label(data$echo.dbp2)="Echo - diastolic bp - 2nd measurement"
  label(data$echo.sbp3)="Echo - systolic bp - 3rd measurement"
  label(data$echo.dbp3)="Echo - diastolic bp - 3rd measurement"
  label(data$echo.ivsd)="Echo - interventricular septal thickness at end diastole"
  label(data$echo.ivss)="Echo - interventricular septal thickness at end systole"
  label(data$echo.lvpwd)="Echo - LV posterior wall thickness at end diastole"
  label(data$echo.lvidd)="Echo - LV interior dimension at end diastole"
  label(data$echo.lvids)="Echo - LV interior dimension at end systole"
  label(data$echo.lvot)="Echo - LV outflow tract"
  label(data$echo.fs)="Echo - fractional shortening"
  label(data$echo.ef)="Echo - biplane simpson ejection fraction"
  label(data$echo.edv)="Echo - end diastolic volume"
  label(data$echo.esv)="Echo - end systolic volume"
  label(data$echo.la.vol)="Echo - LA volume"
  label(data$echo.la.vol.index)="Echo - LA volume index"
  label(data$echo.strokevol.dir)="Echo - stroke volume direct"
  label(data$echo.nlwm...1)="Echo - general wall motion (choice=0 - Normal)"
  label(data$echo.nlwm...2)="Echo - general wall motion (choice=1 - Hypokinesis)"
  label(data$echo.nlwm...3)="Echo - general wall motion (choice=2 - Akinesis)"
  label(data$echo.nlwm....9999)="Echo - general wall motion (choice=Missing)"
  label(data$echo.akinwm...1)="Echo - areas of akinesis (choice=0 - Global)"
  label(data$echo.akinwm...2)="Echo - areas of akinesis (choice=1 - Anterior)"
  label(data$echo.akinwm...3)="Echo - areas of akinesis (choice=2 - Inferior)"
  label(data$echo.akinwm...4)="Echo - areas of akinesis (choice=3 - Lateral)"
  label(data$echo.akinwm...5)="Echo - areas of akinesis (choice=4 - Posterior)"
  label(data$echo.akinwm...6)="Echo - areas of akinesis (choice=5 - Apex)"
  label(data$echo.akinwm....9999)="Echo - areas of akinesis (choice=Missing)"
  label(data$echo.hypowm...1)="Echo - areas of hypokinesis (choice=0 - Global)"
  label(data$echo.hypowm...2)="Echo - areas of hypokinesis (choice=1 - Anterior)"
  label(data$echo.hypowm...3)="Echo - areas of hypokinesis (choice=2 - Inferior)"
  label(data$echo.hypowm...4)="Echo - areas of hypokinesis (choice=3 - Lateral)"
  label(data$echo.hypowm...5)="Echo - areas of hypokinesis (choice=4 - Posterior)"
  label(data$echo.hypowm...6)="Echo - areas of hypokinesis (choice=5 - Apex)"
  label(data$echo.hypowm....9999)="Echo - areas of hypokinesis (choice=Missing)"
  label(data$echo.vd)="Echo - valve disease"
  label(data$echo.vd.ts)="Echo - tricuspid valve stenosis - valve disease"
  label(data$echo.vd.tr)="Echo - tricuspid regurgitation - valve disease"
  label(data$echo.vd.mr)="Echo - mitral regurgitation - valve disease"
  label(data$echo.vd.ms)="Echo - mitral stenosis - valve disease"
  label(data$echo.vd.ar)="Echo - aortic regurgitation - valve disease"
  label(data$echo.vd.as)="Echo - aortic stenosis - valve disease"
  label(data$echo.vd.pr)="Echo - pulmonic regurgitation - valve disease"
  label(data$echo.vd.ps)="Echo - pulmonic stenosis - valve disease"
  label(data$echo.vemaxvel)="Echo - early maximum filling velocity"
  label(data$echo.vamaxvel)="Echo - atrial maximum filling velocity"
  label(data$echo.tissue.dop.e)="Echo - early diastolic myocardial velocity lateral"
  label(data$echo.tissue.dop.e.med)="Echo - early diastolic myocardial velocity medial"
  label(data$echo.tissue.dop.a)="Echo - late diastolic myocardial velocity lateral"
  label(data$echo.tissue.dop.a.med)="Echo - late diastolic myocardial velocity medial"
  label(data$echo.dop.ivrt)="Echo - doppler isovolumteric relaxation time"
  label(data$echo.mv.dt)="Echo - mitral valve deceleration time"
  label(data$echo.rap)="Echo - RA pressure"
  label(data$echo.systvel.rvbase)="Echo - systolic velocity RV base"
  label(data$echo.aov.maxvel)="Echo - AoV max velocity"
  label(data$echo.aov.vti)="Echo - AoV velocity time integral"
  label(data$echo.lvot.vti)="Echo - LV outflow tract velocity time integral"
  label(data$echo.stroke.vol.vti)="Echo - stroke volume velocity time integral"
  label(data$echo.tr.maxvel)="Echo - tricuspid regurgitation max velocity"
  label(data$echo.tapse)="Echo - tricuspid annular plane systolic excursion"
  label(data$echo.glob.long.strain)="Echo - global longitudinal strain"
  label(data$echo.glob.cir.strain)="Echo - global circumferential strain"
  label(data$echo.dfxn)="Echo - diastolic function"
  label(data$echo.effus)="Echo - pericardial effusion"
  label(data$echo.results)="Echo - overall test results"
  label(data$echo.clinicallysig)="Echo - clinically abnormal findings"
  label(data$echo.hrate)="Echo - heart rate"
  label(data$echo.rhythm)="Echo - heart rhythm"
  label(data$echo.rhythm.other)="Echo - heart rhythm other"
  label(data$echo.comments)="Echo - physician readers comments"
  label(data$echo.notes)="Echo - form notes"
  label(data$echocardiogram.complete)="Complete?"
  label(data$bld.date)="Fasting Blood Draw - date"
  label(data$bld.time)="Fasting Blood Draw - time (military)"
  label(data$bld.nurse)="Fasting Blood Draw - nurse"
  label(data$bld.c.wbc)="Clinical Blood - white blood cell (leukocytes, WBC, thou/uL)"
  label(data$bld.c.hgb)="Clinical Blood - hemoglobin (Hgb g/dL)"
  label(data$bld.c.pcv)="Clinical Blood - hematocrit (packed cell volume, PCV, %)"
  label(data$bld.c.mpv)="Clinical Blood - mean platelet volume (MPV, fL)"
  label(data$bld.c.rbc)="Clinical Blood - red blood cell (erythrocytes, RBC, mil/uL)"
  label(data$bld.c.mcv)="Clinical Blood - mean cell volume (MCV, fL)"
  label(data$bld.c.mch)="Clinical Blood - mean cell hemoglobin (MCH, pg)"
  label(data$bld.c.mchc)="Clinical Blood - mean cell hemoglobin concentration (MCHC, g/dL)"
  label(data$bld.c.rdwsd)="Clinical Blood - red cell distribution width standard deviation (RDWSD, fL)"
  label(data$bld.c.rdw)="Clinical Blood - red cell distribution width (RDW, %)"
  label(data$bld.c.nrbc)="Clinical Blood - nucleated red blood cells (NRBC, ? unit)"
  label(data$bld.c.nrbc.number)="Clinical Blood - nucleated red blood cell number (NRBC #, thou/mL)"
  label(data$bld.c.sodium)="Clinical Blood - sodium (Na, mEq/L)"
  label(data$bld.c.potassium)="Clinical Blood - potassium (K, mEq/L)"
  label(data$bld.c.chloride)="Clinical Blood - chloride (Cl, mEq/L)"
  label(data$bld.c.co2)="Clinical Blood - carbon dioxide (CO2, mmol/L)"
  label(data$bld.c.urea.nitrogen)="Clinical Blood - urea nitrogen (BUN, mg/dL)"
  label(data$bld.c.creatinine)="Clinical Blood - creatinine (Creat, mg/dL)"
  label(data$bld.c.glucose)="Clinical Blood - glucose (mg/dL)"
  label(data$bld.c.calcium)="Clinical Blood - calcium (mg/dL)"
  label(data$bld.c.anion.gap)="Clinical Blood - anion GAP (mEq/L)"
  label(data$bld.c.protein.t)="Clinical Blood - protein total (g/dL)"
  label(data$bld.c.albumin)="Clinical Blood - albumin (g/dL)"
  label(data$bld.c.bilirubin.t)="Clinical Blood - bilirubin total (mg/dL)"
  label(data$bld.c.alkphos)="Clinical Blood - alkaline phosphatase (U/L)"
  label(data$bld.c.ast)="Clinical Blood - AST (U/L)"
  label(data$bld.c.alt)="Clinical Blood - ALT (U/L)"
  label(data$bld.c.chol)="Clinical Blood - cholesterol (mg/dL)"
  label(data$bld.c.trig)="Clinical Blood - triglycerides (mg/dL)"
  label(data$bld.c.hdlc)="Clinical Blood - HDL (mg/dL)"
  label(data$bld.c.ldlc)="Clinical Blood - LDL (mg/dL)"
  label(data$bld.c.crp)="Clinical Blood - hs-CRP (mg/L)"
  label(data$bld.c.igg)="Clinical Blood - APA - IgG"
  label(data$bld.c.igm)="Clinical Blood - APA - IgM"
  label(data$bld.c.iga)="Clinical Blood - APA - IgA"
  label(data$bld.c.coag)="Clinical Blood - coagulation interpretation"
  label(data$bld.c.tsh)="Clinical Blood - TSH (mcU/mL)"
  label(data$bld.c.insulin)="Clinical Blood - insulin (mcU/mL)"
  label(data$bld.c.hgba1c)="Clinical Blood - HGB A1C Glycosylated"
  label(data$bld.c.notes)="Clinical Blood - form notes"
  label(data$clinical.blood.work.complete)="Complete?"
  label(data$cmr.date)="CMR - date of acquisition"
  label(data$cmr.time)="CMR - time (military)"
  label(data$cmr.read)="CMR - reader"
  label(data$cmr.quality)="CMR - study quality"
  label(data$cmr.findings.rhythm)="CMR - heart rhythm"
  label(data$cmr.findings.pulse)="CMR - pulse (BPM)"
  label(data$cmr.findings.sbp)="CMR - systolic bp (mmHg)"
  label(data$cmr.findings.dbp)="CMR - diastolic bp (mmHg)"
  label(data$cmr.findings.pp)="CMR - pulse pressure (mmHg)"
  label(data$cmr.findings.lv)="CMR - left ventricle findings"
  label(data$cmr.findings.la)="CMR - left atrium findings"
  label(data$cmr.findings.rv)="CMR - right ventricle findings"
  label(data$cmr.findings.ra)="CMR - right atrium findings"
  label(data$cmr.findings.aov)="CMR - aortic valve findings"
  label(data$cmr.findings.mitralvalve)="CMR - mitral valve findings"
  label(data$cmr.findings.tricuspidvalve)="CMR - tricuspid valve findings"
  label(data$cmr.findings.vessels)="CMR - great vessels findings"
  label(data$cmr.findings.pericardium)="CMR - pericardium findings"
  label(data$cmr.findings.other)="CMR - other findings"
  label(data$cmr.ivsd)="CMR - LV interventricular septal thickness at end diastole (IVSd, mm)"
  label(data$cmr.lvpwd)="CMR - LV posterior wall thickness at end diastole (LVPWd, mm)"
  label(data$cmr.lvmass)="CMR - LV mass (LVM, g)"
  label(data$cmr.lvmass.index)="CMR - LV mass index (g/m2)"
  label(data$cmr.lvedd)="CMR - LV end diastolic diameter (LVEDD, mm)"
  label(data$cmr.lvedv)="CMR - LV end diastolic volume (LVEDV, mL)"
  label(data$cmr.lvedvi)="CMR - LV end diastolic volume index (mL/m2)"
  label(data$cmr.lvesd)="CMR - LV end systolic diameter (LVESD, mm)"
  label(data$cmr.lvesv)="CMR - LV end systolic volume (LVESV, mL)"
  label(data$cmr.lvesvi)="CMR - LV end systolic volume index (mL/m2)"
  label(data$cmr.lvef)="CMR - LV ejection fraction (LVEF, %)"
  label(data$cmr.lvsv)="CMR - LV stroke volume (LVSV, mL)"
  label(data$cmr.lvsvi)="CMR - LV stroke volume index (mL/m2)"
  label(data$cmr.peaktime)="CMR - time to peak filling rate (msec)"
  label(data$cmr.pfr)="CMR - peak filling rate (EDV/sec)"
  label(data$cmr.transaortic.sv)="CMR - transaortic stroke volume (mL)"
  label(data$cmr.transaortic.velocity)="CMR - transaortic velocity (cm/s)"
  label(data$cmr.co)="CMR - cardiac output (L/min)"
  label(data$cmr.ci)="CMR - cardiac index (L/min/m2)"
  label(data$cmr.rvedd)="CMR - RV end diastolic diameter (mm)"
  label(data$cmr.ra)="CMR - right atrium (RA, mm)"
  label(data$cmr.la)="CMR - left atrium (LA, mm)"
  label(data$cmr.lav)="CMR - LA volume (mL)"
  label(data$cmr.lavi)="CMR - LA volume index (mL/m2)"
  label(data$cmr.ao.root)="CMR - Aortic Root (mm)"
  label(data$cmr.ascending.ao)="CMR - ascending aorta (mm)"
  label(data$cmr.descending.ao)="CMR - descending aorta (mm)"
  label(data$cmr.ascending.ao.min.area)="CMR - ascending aorta min area (cm2)"
  label(data$cmr.ascending.ao.max.area)="CMR - ascending aorta max area (cm2)"
  label(data$cmr.ao.compliance)="CMR - aortic compliance (mm2/mmHg)"
  label(data$cmr.mpa)="CMR - main pulmonary artery (mm)"
  label(data$cmr.rpa)="CMR - right pulmonary artery (mm)"
  label(data$cmr.lpa)="CMR - left pulmonary artery (mm)"
  label(data$cmr.ivc.pars.hepatica)="CMR - inferior vena cava pars hepatica (mm)"
  label(data$cmr.ivc.pars.suprahepatica)="CMR - inferior vena cava pars suprahepatica (mm)"
  label(data$cmr.lv.anteroseptal)="CMR - LV anteroseptal wall motion"
  label(data$cmr.lv.anterior)="CMR - LV anterior wall motion"
  label(data$cmr.lv.anterolateral)="CMR - LV anterolateral wall motion"
  label(data$cmr.lv.inferolateral)="CMR - LV inferolateral wall motion"
  label(data$cmr.lv.inferior)="CMR - LV inferior wall motion"
  label(data$cmr.lv.inferoseptal)="CMR - LV inferoseptal wall motion"
  label(data$cmr.lv.apical)="CMR - LV apical wall motion"
  label(data$cmr.msvr)="CMR - Modified systemic vascular resistance"
  label(data$cmr.finalimpress.lvs.lvd)="CMR - Final Impression1 - Systolic/diastolic function"
  label(data$cmr.finalimpress.lv.size)="CMR - Final Impression 2 - LV size"
  label(data$cmr.finalimpress.valve)="CMR - Final Impression 3 - Valve"
  label(data$cmr.finalimpress.ao)="CMR - Final Impression 4 -Aorta"
  label(data$cmr.notes)="CMR - form notes"
  label(data$cardiac.mri.complete)="Complete?"
  label(data$np.date)="NP Assessment - date of administration"
  label(data$np.time)="NP Assessment - time (military)"
  label(data$np.examiner)="NP Assessment - examiner"
  label(data$np.notes)="NP Assessment - examiner notes"
  label(data$np.moca)="MoCA total score"
  label(data$np.moca.notes)="MoCA administrative issues"
  label(data$np.moca.invalid)="MoCA invalidated by administrative issue"
  label(data$np.cvlt1)="CVLT-II trial 1"
  label(data$np.cvlt1z)="CVLT-II trial 1 z-score"
  label(data$np.cvlt2)="CVLT-II trial 2"
  label(data$np.cvlt2z)="CVLT-II trial 2 z-score"
  label(data$np.cvlt3)="CVLT-II trial 3"
  label(data$np.cvlt3z)="CVLT-II trial 3 z-score"
  label(data$np.cvlt4)="CVLT-II trial 4"
  label(data$np.cvlt4z)="CVLT-II trial 4 z-score"
  label(data$np.cvlt5)="CVLT-II trial 5"
  label(data$np.cvlt5z)="CVLT-II trial 5 z-score"
  label(data$np.cvlt1to5)="CVLT-II total immediate recall"
  label(data$np.cvlt1to5.tscore)="CVLT-II total immediate recall t-score"
  label(data$np.cvltb)="CVLT-II trial B"
  label(data$np.cvltbz)="CVLT-II trial B z-score"
  label(data$np.cvlt.sdfr)="CVLT-II short delay free recall"
  label(data$np.cvlt.sdfr.z)="CVLT-II short delay free recall z-score"
  label(data$np.cvlt.sdcr)="CVLT-II short delay cued recall"
  label(data$np.cvlt.sdcr.z)="CVLT-II short delay cued recall z-score"
  label(data$np.cvlt.ldfr)="CVLT-II long delay free recall"
  label(data$np.cvlt.ldfr.z)="CVLT-II long delay free recall z-score"
  label(data$np.cvlt.ldcr)="CVLT-II long delay cued recall"
  label(data$np.cvlt.ldcr.z)="CVLT-II long delay cued recall z-score"
  label(data$np.cvlt1to5.semclust)="CVLT-II trial 1-5 semantic clustering"
  label(data$np.cvlt1to5.semclust.z)="CVLT-II trial 1-5 semantic clustering z-score"
  label(data$np.cvlt1to5.serialclustfwd)="CVLT-II trial 1-5 serial clustering forward"
  label(data$np.cvlt1to5.serialclustfwd.z)="CVLT-II trial 1-5 serial clustering forward z-score"
  label(data$np.cvlt1to5.serialclustbirect)="CVLT-II trial 1-5 serial clustering bidirectional"
  label(data$np.cvlt1to5.serialclustbidirect.z)="CVLT-II trial 1-5 serial clustering bidirectional z-score"
  label(data$np.cvlt1to5.primacy)="CVLT-II trial 1-5 % recall from primacy"
  label(data$np.cvlt1to5.primacy.z)="CVLT-II trial 1-5 % recall from primacy z-score"
  label(data$np.cvlt1to5.middle)="CVLT-II trial 1-5 % recall from middle"
  label(data$np.cvlt1to5.middle.z)="CVLT-II trial 1-5 % recall from middle z-score"
  label(data$np.cvlt1to5.recency)="CVLT-II trial 1-5 % recall from recency"
  label(data$np.cvlt1to5.recency.z)="CVLT-II trial 1-5 % recall from recency z-score"
  label(data$np.cvltslope)="CVLT-II trial 1-5 total learning slope"
  label(data$np.cvltslope.z)="CVLT-II trial 1-5 total learning slope z-score"
  label(data$np.cvltslope.t1to2)="CVLT-II learning slope trials 1-2"
  label(data$np.cvltslope.t1to2.z)="CVLT-II learning slope trials 1-2 z-score"
  label(data$np.cvltslope.t2to5)="CVLT-II learning slope trials 2-5"
  label(data$np.cvltslope.t2to5.z)="CVLT-II learning slope trials 2-5 z-score"
  label(data$np.cvlt.learnconsist)="CVLT-II across-trial recall consistency"
  label(data$np.cvlt.learnconsist.z)="CVLT-II across-trial recall consistency z-score"
  label(data$np.cvltcontrast.bvs1)="CVLT-II contrast: list B vs. trial 1 (proactive interference) % change"
  label(data$np.cvltcontrast.bvs1.z)="CVLT-II contrast: list B vs. trial 1 (proactive interference) z-score difference"
  label(data$np.cvltcontrast.sdvs5)="CVLT-II contrast: short delay vs trial 5 (retroactive interference) % change"
  label(data$np.cvltcontrast.sdvs5.z)="CVLT-II contrast: short delay vs trial 5 (retroactive interference) z-score difference"
  label(data$np.cvltcontrast.ldvs5)="CVLT-II contrast: long delay vs trial 5 (long-delay retention) % change"
  label(data$np.cvltcontrast.ldvs5.z)="CVLT-II contrast: long delay vs trial 5 (long-delay retention) z-score difference"
  label(data$np.cvltcontrast.ldvssd)="CVLT-II contrast: long delay vs short delay % change"
  label(data$np.cvltcontrast.ldvssd.z)="CVLT-II contrast: long delay vs short delay z-score"
  label(data$np.cvlt.reps)="CVLT-II total repetitions"
  label(data$np.cvlt.reps.z)="CVLT-II total repetitions z-score"
  label(data$np.cvlt.intrus)="CVLT-II total intrusions"
  label(data$np.cvlt.intrus.z)="CVLT-II total intrusions z-score"
  label(data$np.cvltrec.hits)="CVLT-II delay recog: total hits"
  label(data$np.cvltrec.hits.z)="CVLT-II delay recog: total hits z-score"
  label(data$np.cvltrec.falsepos)="CVLT-II delay recog: total false positives"
  label(data$np.cvltrecog.falsepos.z)="CVLT-II delay recog: total false positives z-score"
  label(data$np.cvltrecog.discrim)="CVLT-II delay recog: total discriminability (d)"
  label(data$np.cvltrecog.discrim.z)="CVLT-II delay recog: total discriminability (d) z-score"
  label(data$np.cvltrecog.sourcediscrim)="CVLT-II delay recog: source recog discriminability (d)"
  label(data$np.cvltrecog.sourcediscrim.z)="CVLT-II delay recog: source recog discriminability (d) z-score"
  label(data$np.cvltrecog.semanticdiscrim)="CVLT-II delay recog: semantic recog discriminability (d)"
  label(data$np.cvltrecog.semanticdiscrim.z)="CVLT-II delay recog: semantic recog discriminability (d) z-score"
  label(data$np.cvltrecog.noveldiscrim)="CVLT-II delay recog: novel recog discriminability (d)"
  label(data$np.cvltrecog.noveldiscrim.z)="CVLT-II delay recog: novel recog discriminability (d) z-score"
  label(data$np.cvltrecog.responbias)="CVLT-II delay recog: total response bias"
  label(data$np.cvltrecog.responbias.z)="CVLT-II delay recog: total response bias z-score"
  label(data$np.cvlt.software)="CVLT-II software inconsistencies"
  label(data$np.cvlt.notes)="CVLT-II administrative issues"
  label(data$np.cvlt.invalid)="CVLT-II invalidated by administrative issue"
  label(data$np.tower01)="DKEFS Tower 1 score"
  label(data$np.tower02)="DKEFS Tower 2 score"
  label(data$np.tower03)="DKEFS Tower 3 score"
  label(data$np.tower04)="DKEFS Tower 4 score"
  label(data$np.tower05)="DKEFS Tower 5 score"
  label(data$np.tower06)="DKEFS Tower 6 score"
  label(data$np.tower07)="DKEFS Tower 7 score"
  label(data$np.tower08)="DKEFS Tower 8 score"
  label(data$np.tower09)="DKEFS Tower 9 score"
  label(data$np.tower.items)="DKEFS Tower total items administered"
  label(data$np.tower.ruleviol)="DKEFS Tower total rule violations"
  label(data$np.tower.ruleviol.cumperc)="DKEFS Tower total rule violations cumulative percentile rank"
  label(data$np.tower.ss)="DKEFS Tower total scaled score"
  label(data$np.tower.notes)="DKEFS Tower administrative issues"
  label(data$np.tower.invalid)="DKEFS Tower invalidated by administrative issue"
  label(data$np.digsymb)="Digit-Symbol Coding total correct"
  label(data$np.digsymb.ss)="Digit-Symbol Coding scaled score"
  label(data$np.digsymb.notes)="Digit-Symbol Coding administrative issues"
  label(data$np.digsymb.invalid)="Digit-Symbol Coding invalidated by administrative issue"
  label(data$np.color)="DKEFS Color-Word color time (s)"
  label(data$np.color.ss)="DKEFS Color-Word color scaled score"
  label(data$np.word)="DKEFS Color-Word word time (s)"
  label(data$np.word.ss)="DKEFS Color-Word word scaled score"
  label(data$np.inhibit)="DKEFS Color-Word inhibition time (s)"
  label(data$np.inhibit.ss)="DKEFS Color-Word inhibition scaled score"
  label(data$np.colorword.sum)="DKEFS Color-Word color + word sum of scaled scores"
  label(data$np.colorword.comp)="DKEFS Color-Word color + word composite scaled score"
  label(data$np.inhibitcolor.diff)="DKEFS Color-Word inhibition vs color-naming scaled-score difference"
  label(data$np.inhibitcolor.contrast)="DKEFS Color-Word inhibition vs color-naming contrast scaled score"
  label(data$np.color.scerr)="DKEFS Color-Word color corrected errors"
  label(data$np.color.ucerr)="DKEFS Color-Word color uncorrected errors"
  label(data$np.color.err)="DKEFS Color-Word color total errors"
  label(data$np.color.cumpercerr)="DKEFS Color-Word color errors cumulative percentile rank"
  label(data$np.word.scerr)="DKEFS Color-Word word corrected errors"
  label(data$np.word.ucerr)="DKEFS Color-Word word uncorrected errors"
  label(data$np.word.err)="DKEFS Color-Word word total errors"
  label(data$np.word.cumpercerr)="DKEFS Color-Word word errors cumulative percentile rank"
  label(data$np.inhibit.scerr)="DKEFS Color-Word inhibition corrected errors"
  label(data$np.inhibit.cumpercscerr)="DKEFS Color-Word inhibition corrected errors cumulative percentile rank"
  label(data$np.inhibit.ucerr)="DKEFS Color-Word inhibition uncorrected errors"
  label(data$np.inhibit.cumpercucerr)="DKEFS Color-Word inhibition uncorrected errors cumulative percentile rank"
  label(data$np.inhibit.err)="DKEFS Color-Word inhibition total errors"
  label(data$np.inhibit.err.ss)="DKEFS Color-Word inhibition total errors scaled score"
  label(data$np.cw.notes)="DKEFS Color-Word administrative issues"
  label(data$np.cw.invalid)="DKEFS Color-Word invalidated by administrative issue"
  label(data$np.hvot)="HVOT total score"
  label(data$np.hvot.tscore)="HVOT t-score"
  label(data$np.hvot.notes)="HVOT administrative issues"
  label(data$np.hvot.invalid)="HVOT invalidated by administrative issue"
  label(data$np.fas.fq1)="FAS - F: quartile 1 (0-15s)"
  label(data$np.fas.fq2)="FAS - F: quartile 2 (16-30s)"
  label(data$np.fas.fq3)="FAS - F: quartile 3 (31-45s)"
  label(data$np.fas.fq4)="FAS - F: quartile 4 (46-60s)"
  label(data$np.fas.f)="FAS - F: total"
  label(data$np.fas.f.intrus)="FAS - F: intrusions"
  label(data$np.fas.f.reps)="FAS - F: repetitions"
  label(data$np.fas.aq1)="FAS - A: quartile 1 (0-15s)"
  label(data$np.fas.aq2)="FAS - A: quartile 2 (16-30s)"
  label(data$np.fas.aq3)="FAS - A: quartile 3 (31-45s)"
  label(data$np.fas.aq4)="FAS - A: quartile 4 (46-60s)"
  label(data$np.fas.a)="FAS - A: total"
  label(data$np.fas.a.intrus)="FAS - A: intrusions"
  label(data$np.fas.a.reps)="FAS - A: repetitions"
  label(data$np.fas.sq1)="FAS - S: quartile 1 (0-15s)"
  label(data$np.fas.sq2)="FAS - S: quartile 2 (16-30s)"
  label(data$np.fas.sq3)="FAS - S: quartile 3 (31-45s)"
  label(data$np.fas.sq4)="FAS - S: quartile 4 (46-60s)"
  label(data$np.fas.s)="FAS - S: total"
  label(data$np.fas.s.intrus)="FAS - S: intrusions"
  label(data$np.fas.s.reps)="FAS - S: repetitions"
  label(data$np.fas.q1)="FAS - Q1: avearge"
  label(data$np.fas.q2)="FAS - Q2: average"
  label(data$np.fas.q3)="FAS - Q3: average"
  label(data$np.fas.q4)="FAS - Q4: average"
  label(data$np.fas)="FAS - total score"
  label(data$np.fas.tscore)="FAS - total t-score"
  label(data$np.fas.intrus)="FAS - total intrustions"
  label(data$np.fas.rep)="FAS - total repetitions"
  label(data$np.fas.notes)="FAS administrative issue"
  label(data$np.fas.invalid)="FAS invalidated by administrative issue"
  label(data$np.anim.q1)="Animal Naming - quartile 1 (0-15s)"
  label(data$np.anim.q2)="Animal Naming - quartile 2 (16-30s)"
  label(data$np.anim.q3)="Animal Naming - quartile 3 (31-45s)"
  label(data$np.anim.q4)="Animal Naming - quartile 4 (46-60s)"
  label(data$np.anim.intrus)="Animal Naming - intrusions"
  label(data$np.anim.rep)="Animal Naming - perseverations"
  label(data$np.anim.q1err)="Animal Naming - quartile 1 (0-15s) errors"
  label(data$np.anim.q2err)="Animal Naming - quartile 2 (16-30s) errors"
  label(data$np.anim.q3err)="Animal Naming - quartile 3 (31-45s) errors"
  label(data$np.anim.q4err)="Animal Naming - quartile 4 (46-60s) errors"
  label(data$np.anim.1rep)="Animal Naming - 1st repetition distance"
  label(data$np.anim.2rep)="Animal Naming - 2nd repetition distance"
  label(data$np.anim.3rep)="Animal Naming - 3rd repetition distance"
  label(data$np.anim.4rep)="Animal Naming - 4th repetition distance"
  label(data$np.anim.5rep)="Animal Naming - 5th repetition distance"
  label(data$np.anim.6rep)="Animal Naming - 6th repetition distance"
  label(data$np.anim.7rep)="Animal Naming - 7th repetition distance"
  label(data$np.anim.8rep)="Animal Naming - 8th repetition distance"
  label(data$np.anim.9rep)="Animal Naming - 9th repetition distance"
  label(data$np.anim.10rep)="Animal Naming - 10th repetition distance"
  label(data$np.anim.tscore)="Animal Naming - total t-score"
  label(data$np.anim.ai)="Animal Naming - Association Index"
  label(data$np.anim.notes)="Animal Naming administrative issues"
  label(data$np.anim.invalid)="Animal Naming invalidated by administrative issue"
  label(data$np.tmta)="Trails A time (s)"
  label(data$np.tmta.ss)="Trails A scaled score"
  label(data$np.tmtb)="Trails B time (s)"
  label(data$np.tmtb.ss)="Trails B scaled score"
  label(data$np.tmt.contrastdiff.ss)="Trails B vs A contrast scaled score"
  label(data$np.tmta.seqerr)="Trails A sequencing errors"
  label(data$np.tmta.cumperc.seqerr)="Trails A sequencing errors cumulative percentile rank"
  label(data$np.tmta.seterr)="Trails A set-loss errors"
  label(data$np.tmta.cumperc.seterr)="Trails A set-loss errors cumulative percentile rank"
  label(data$np.tmtb.seqerr)="Trails B sequencing errors"
  label(data$np.tmtb.cumperc.seqerr)="Trails B sequencing errors cumulative percentile rank"
  label(data$np.tmtb.seterr)="Trails B set-loss errors"
  label(data$np.tmtb.cumperc.seterr)="Trails B set-loss errors cumulative percentile rank"
  label(data$np.tmta.notes)="Trails A administrative issues"
  label(data$np.tmta.invalid)="Trails A invalidated by administrative issue"
  label(data$np.tmtb.notes)="Trails B administrative issues"
  label(data$np.tmtb.invalid)="Trails B invalidated by administrative issue"
  label(data$np.bnt)="BNT  total"
  label(data$np.bnt.z)="BNT total z-score"
  label(data$np.bnt.notes)="BNT administrative issues"
  label(data$np.bnt.invalid)="BNT invalidated by administrative issue"
  label(data$np.biber1)="Biber trial 1"
  label(data$np.biber1.z)="Biber trial 1 z-score"
  label(data$np.biber2)="Biber trial 2"
  label(data$np.biber2.z)="Biber trial 2 z-score"
  label(data$np.biber3)="Biber trial 3"
  label(data$np.biber3.z)="Biber trial 3 z-score"
  label(data$np.biber4)="Biber trial 4"
  label(data$np.biber4.z)="Biber trial 4 z-score"
  label(data$np.biber5)="Biber trial 5"
  label(data$np.biber5.z)="Biber trial 5 z-score"
  label(data$np.biber.t1to5.z)="Biber total immediate recall z-score"
  label(data$np.biber.t1to5.persev)="Biber total immediate recall perservations"
  label(data$np.biber.t1to5.extra)="Biber total immediate recall extraneous responses"
  label(data$np.biberb)="Biber distractor set"
  label(data$np.biberb.z)="Biber distractor set z-score"
  label(data$np.biber.sd)="Biber short delay"
  label(data$np.biber.sd.z)="Biber short delay z-score"
  label(data$np.biber.ld)="Biber long delay"
  label(data$np.biber.ld.z)="Biber long delay z-score"
  label(data$np.biber1.figures)="Biber trial 1 total perfect figures generated"
  label(data$np.biber2.figures)="Biber trial 2 total perfect figures generated"
  label(data$np.biber3.figures)="Biber trial 3 total perfect figures generated"
  label(data$np.biber4.figures)="Biber trial 4 total perfect figures generated"
  label(data$np.biber5.figures)="Biber trial 5 total perfect figures generated"
  label(data$np.biberb.figures)="Biber trial distractor set total perfect figures generated"
  label(data$np.bibersd.figures)="Biber trial short delay total perfect figures generated"
  label(data$np.biberld.figures)="Biber trial long delay total perfect figures generated"
  label(data$np.biber.hits)="Biber delayed recognition - hits"
  label(data$np.biber.related.falsealarms)="Biber delayed recognition - distractor/related false alarms"
  label(data$np.biber.unrelated.falsealarms)="Biber delayed recognition - unrelated false alarms"
  label(data$np.biber.falsealarms)="Biber delayed recognition - total false alarms"
  label(data$np.biber.recoghitrate)="Biber delayed recognition - hit rate"
  label(data$np.biber.falsealarm.relatedrate)="Biber delayed recognition - distractor/related false alarm rate"
  label(data$np.biber.falsealarm.unrelatedrate)="Biber delayed recognition - unrelated false alarm rate"
  label(data$np.biber.falsealarm.totalrate)="Biber delayed recognition - total false alarm rate"
  label(data$np.biber.discrim)="Biber delayed recognition - discrimination total"
  label(data$np.biber.discrim.related)="Biber delayed recognition - discrimination distractor/related"
  label(data$np.biber.discrim.unrelated)="Biber delayed recognition - discrimination unrelated"
  label(data$np.biber.bias)="Biber delayed recognition - response bias total"
  label(data$np.biber.bias.related)="Biber delayed recognition - response bias distractor/related"
  label(data$np.biber.bias.unrelated)="Biber delayed recognition - response bias unrelated"
  label(data$np.biber.notes)="Biber administrative issues"
  label(data$np.biber.invalid)="Biber invalidated by administrative issue"
  label(data$neuropsychological.assessment.complete)="Complete?"
  #Setting Units


  #Setting Factors(will create new variable for factors)
  data$entry.primary.factor = factor(data$entry.primary,levels=c("4","20","12","7","22","14"))
  data$entry.secondary.factor = factor(data$entry.secondary,levels=c("1","4","20","12","11","7","22","10","13","2","21","14"))
  data$data.entry.complete.factor = factor(data$data.entry.complete,levels=c("0","1","2"))
  data$visit.brain.factor = factor(data$visit.brain,levels=c("1","0","-9999","-8888"))
  data$visit.echo.factor = factor(data$visit.echo,levels=c("1","0","-9999","-8888"))
  data$visit.cmr.factor = factor(data$visit.cmr,levels=c("1","0","-9999","-8888"))
  data$visit.depress.factor = factor(data$visit.depress,levels=c("1","0","-9999","-8888"))
  data$visit.depress.note.factor = factor(data$visit.depress.note,levels=c("1","0","-9999","-8888"))
  data$visit.notes.complete.factor = factor(data$visit.notes.complete,levels=c("0","1","2"))
  data$diagnosis.factor = factor(data$diagnosis,levels=c("1","2","3","4","-9999"))
  data$nc.type.factor = factor(data$nc.type,levels=c("1","2"))
  data$mci.amnestic.factor = factor(data$mci.amnestic,levels=c("1","0","-9999","-8888"))
  data$mci.domain.factor = factor(data$mci.domain,levels=c("1","2","-9999","-8888"))
  data$mci.stage.factor = factor(data$mci.stage,levels=c("1","2","3","-9999","-8888"))
  data$dementia.factor = factor(data$dementia,levels=c("1"))
  data$diagnosis.complete.factor = factor(data$diagnosis.complete,levels=c("0","1","2"))
  data$cdr.exam.factor = factor(data$cdr.exam,levels=c("8","10","3","5","6","1","4","2","7","9","-9999"))
  data$cdr.mem.factor = factor(data$cdr.mem,levels=c("0","1","2","4","6","-9999"))
  data$cdr.orient.factor = factor(data$cdr.orient,levels=c("0","1","2","4","6","-9999"))
  data$cdr.judg.factor = factor(data$cdr.judg,levels=c("0","1","2","4","6","-9999"))
  data$cdr.affairs.factor = factor(data$cdr.affairs,levels=c("0","1","2","4","6","-9999"))
  data$cdr.hobbies.factor = factor(data$cdr.hobbies,levels=c("0","1","2","4","6","-9999"))
  data$cdr.care.factor = factor(data$cdr.care,levels=c("0","1","2","4","6","-9999"))
  data$cdr.factor = factor(data$cdr,levels=c("0","1","2","4","6","-9999"))
  data$clinical.dementia.rating.complete.factor = factor(data$clinical.dementia.rating.complete,levels=c("0","1","2"))
  data$functional.activities.questionnaire.complete.factor = factor(data$functional.activities.questionnaire.complete,levels=c("0","1","2"))
  data$famhx.date.multiple.factor = factor(data$famhx.date.multiple,levels=c("1","0"))
  data$famhx01.ptp.factor = factor(data$famhx01.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx01.relation.ptp.factor = factor(data$famhx01.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx01.prob.ptp.factor = factor(data$famhx01.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx01.dx.ptp.factor = factor(data$famhx01.dx.ptp,levels=c("1","6","2","3","7","4","5","-9999"))
  data$famhx01.onset.ptp.factor = factor(data$famhx01.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx02.ptp.factor = factor(data$famhx02.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx02.relation.ptp.factor = factor(data$famhx02.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx02.prob.ptp.factor = factor(data$famhx02.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx02.dx.ptp.factor = factor(data$famhx02.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx02.onset.ptp.factor = factor(data$famhx02.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx03.ptp.factor = factor(data$famhx03.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx03.relation.ptp.factor = factor(data$famhx03.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx03.prob.ptp.factor = factor(data$famhx03.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx03.dx.ptp.factor = factor(data$famhx03.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx03.onset.ptp.factor = factor(data$famhx03.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx04.ptp.factor = factor(data$famhx04.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx04.relation.ptp.factor = factor(data$famhx04.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx04.prob.ptp.factor = factor(data$famhx04.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx04.dx.ptp.factor = factor(data$famhx04.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx04.onset.ptp.factor = factor(data$famhx04.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx05.ptp.factor = factor(data$famhx05.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx05.relation.ptp.factor = factor(data$famhx05.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx05.prob.ptp.factor = factor(data$famhx05.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx05.dx.ptp.factor = factor(data$famhx05.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx05.onset.ptp.factor = factor(data$famhx05.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx06.ptp.factor = factor(data$famhx06.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx06.relation.ptp.factor = factor(data$famhx06.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx06.prob.ptp.factor = factor(data$famhx06.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx06.dx.ptp.factor = factor(data$famhx06.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx06.onset.ptp.factor = factor(data$famhx06.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx07.ptp.factor = factor(data$famhx07.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx07.relation.ptp.factor = factor(data$famhx07.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx07.prob.ptp.factor = factor(data$famhx07.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx07.dx.ptp.factor = factor(data$famhx07.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx07.onset.ptp.factor = factor(data$famhx07.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx08.ptp.factor = factor(data$famhx08.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx08.relation.ptp.factor = factor(data$famhx08.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx08.prob.ptp.factor = factor(data$famhx08.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx08.dx.ptp.factor = factor(data$famhx08.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx08.onset.ptp.factor = factor(data$famhx08.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx09.ptp.factor = factor(data$famhx09.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx09.relation.ptp.factor = factor(data$famhx09.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx09.prob.ptp.factor = factor(data$famhx09.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx09.dx.ptp.factor = factor(data$famhx09.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx09.onset.ptp.factor = factor(data$famhx09.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx10.ptp.factor = factor(data$famhx10.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx10.relation.ptp.factor = factor(data$famhx10.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx10.prob.ptp.factor = factor(data$famhx10.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx10.dx.ptp.factor = factor(data$famhx10.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx10.onset.ptp.factor = factor(data$famhx10.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx11.ptp.factor = factor(data$famhx11.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx11.relation.ptp.factor = factor(data$famhx11.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx11.prob.ptp.factor = factor(data$famhx11.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx11.dx.ptp.factor = factor(data$famhx11.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx11.onset.ptp.factor = factor(data$famhx11.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx12.ptp.factor = factor(data$famhx12.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx12.relation.ptp.factor = factor(data$famhx12.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx12.prob.ptp.factor = factor(data$famhx12.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx12.dx.ptp.factor = factor(data$famhx12.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx12.onset.ptp.factor = factor(data$famhx12.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx13.ptp.factor = factor(data$famhx13.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx13.relation.ptp.factor = factor(data$famhx13.relation.ptp,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx13.prob.ptp.factor = factor(data$famhx13.prob.ptp,levels=c("1","2","-9999","-8888"))
  data$famhx13.dx.ptp.factor = factor(data$famhx13.dx.ptp,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx13.onset.ptp.factor = factor(data$famhx13.onset.ptp,levels=c("1","0","-9999","-8888"))
  data$famhx01.inf.factor = factor(data$famhx01.inf,levels=c("1","0","-9999","-8888"))
  data$famhx01.relation.inf.factor = factor(data$famhx01.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx01.prob.inf.factor = factor(data$famhx01.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx01.dx.inf.factor = factor(data$famhx01.dx.inf,levels=c("1","6","2","3","7","4","5","-9999"))
  data$famhx01.onset.inf.factor = factor(data$famhx01.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx02.inf.factor = factor(data$famhx02.inf,levels=c("1","0","-9999","-8888"))
  data$famhx02.relation.inf.factor = factor(data$famhx02.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx02.prob.inf.factor = factor(data$famhx02.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx02.dx.inf.factor = factor(data$famhx02.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx02.onset.inf.factor = factor(data$famhx02.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx03.inf.factor = factor(data$famhx03.inf,levels=c("1","0","-9999","-8888"))
  data$famhx03.relation.inf.factor = factor(data$famhx03.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx03.prob.inf.factor = factor(data$famhx03.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx03.dx.inf.factor = factor(data$famhx03.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx03.onset.inf.factor = factor(data$famhx03.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx04.inf.factor = factor(data$famhx04.inf,levels=c("1","0","-9999","-8888"))
  data$famhx04.relation.inf.factor = factor(data$famhx04.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx04.prob.inf.factor = factor(data$famhx04.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx04.dx.inf.factor = factor(data$famhx04.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx04.onset.inf.factor = factor(data$famhx04.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx05.inf.factor = factor(data$famhx05.inf,levels=c("1","0","-9999","-8888"))
  data$famhx05.relation.inf.factor = factor(data$famhx05.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx05.prob.inf.factor = factor(data$famhx05.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx05.dx.inf.factor = factor(data$famhx05.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx05.onset.inf.factor = factor(data$famhx05.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx06.inf.factor = factor(data$famhx06.inf,levels=c("1","0","-9999","-8888"))
  data$famhx06.relation.inf.factor = factor(data$famhx06.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx06.prob.inf.factor = factor(data$famhx06.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx06.dx.inf.factor = factor(data$famhx06.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx06.onset.inf.factor = factor(data$famhx06.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx07.inf.factor = factor(data$famhx07.inf,levels=c("1","0","-9999","-8888"))
  data$famhx07.relation.inf.factor = factor(data$famhx07.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx07.prob.inf.factor = factor(data$famhx07.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx07.dx.inf.factor = factor(data$famhx07.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx07.onset.inf.factor = factor(data$famhx07.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx08.inf.factor = factor(data$famhx08.inf,levels=c("1","0","-9999","-8888"))
  data$famhx08.relation.inf.factor = factor(data$famhx08.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx08.prob.inf.factor = factor(data$famhx08.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx08.dx.inf.factor = factor(data$famhx08.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx08.onset.inf.factor = factor(data$famhx08.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx09.inf.factor = factor(data$famhx09.inf,levels=c("1","0","-9999","-8888"))
  data$famhx09.relation.inf.factor = factor(data$famhx09.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx09.prob.inf.factor = factor(data$famhx09.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx09.dx.inf.factor = factor(data$famhx09.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx09.onset.inf.factor = factor(data$famhx09.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx10.inf.factor = factor(data$famhx10.inf,levels=c("1","0","-9999","-8888"))
  data$famhx10.relation.inf.factor = factor(data$famhx10.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx10.prob.inf.factor = factor(data$famhx10.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx10.dx.inf.factor = factor(data$famhx10.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx10.onset.inf.factor = factor(data$famhx10.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx11.inf.factor = factor(data$famhx11.inf,levels=c("1","0","-9999","-8888"))
  data$famhx11.relation.inf.factor = factor(data$famhx11.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx11.prob.inf.factor = factor(data$famhx11.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx11.dx.inf.factor = factor(data$famhx11.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx11.onset.inf.factor = factor(data$famhx11.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx12.inf.factor = factor(data$famhx12.inf,levels=c("1","0","-9999","-8888"))
  data$famhx12.relation.inf.factor = factor(data$famhx12.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx12.prob.inf.factor = factor(data$famhx12.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx12.dx.inf.factor = factor(data$famhx12.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx12.onset.inf.factor = factor(data$famhx12.onset.inf,levels=c("1","0","-9999","-8888"))
  data$famhx13.inf.factor = factor(data$famhx13.inf,levels=c("1","0","-9999","-8888"))
  data$famhx13.relation.inf.factor = factor(data$famhx13.relation.inf,levels=c("01","02","03","04","05","06","07","08","09","10","31","32","33","34","11","12","13","14","15","16","17","18","26","27","19","20","21","30","29","25","22","23","24","28","0","-9999","-8888"))
  data$famhx13.prob.inf.factor = factor(data$famhx13.prob.inf,levels=c("1","2","-9999","-8888"))
  data$famhx13.dx.inf.factor = factor(data$famhx13.dx.inf,levels=c("1","6","2","3","4","5","-9999"))
  data$famhx13.onset.inf.factor = factor(data$famhx13.onset.inf,levels=c("1","0","-9999","-8888"))
  data$family.history.complete.factor = factor(data$family.history.complete,levels=c("0","1","2"))
  data$inform.method.factor = factor(data$inform.method,levels=c("1","2","-9999","-8888"))
  data$inform.source...1.factor = factor(data$inform.source...1,levels=c("0","1"))
  data$inform.source...2.factor = factor(data$inform.source...2,levels=c("0","1"))
  data$inform.source...3.factor = factor(data$inform.source...3,levels=c("0","1"))
  data$inform.source...4.factor = factor(data$inform.source...4,levels=c("0","1"))
  data$inform.source...5.factor = factor(data$inform.source...5,levels=c("0","1"))
  data$inform.source...6.factor = factor(data$inform.source...6,levels=c("0","1"))
  data$inform.source...7.factor = factor(data$inform.source...7,levels=c("0","1"))
  data$inform.source...8.factor = factor(data$inform.source...8,levels=c("0","1"))
  data$inform.source...9.factor = factor(data$inform.source...9,levels=c("0","1"))
  data$inform.source...10.factor = factor(data$inform.source...10,levels=c("0","1"))
  data$inform.source....9999.factor = factor(data$inform.source....9999,levels=c("0","1"))
  data$inform.source....8888.factor = factor(data$inform.source....8888,levels=c("0","1"))
  data$inform.sex.factor = factor(data$inform.sex,levels=c("1","2","-9999","-8888"))
  data$inform.relation.factor = factor(data$inform.relation,levels=c("1","2","3","4","5","6","-9999","-8888"))
  data$inform.live.factor = factor(data$inform.live,levels=c("1","0","-9999","-8888"))
  data$inform.phone.factor = factor(data$inform.phone,levels=c("1","2","3","4","5","6","-9999","-8888"))
  data$inform.visit.factor = factor(data$inform.visit,levels=c("1","2","3","4","5","6","-9999","-8888"))
  data$inform.reliability.factor = factor(data$inform.reliability,levels=c("1","0","-9999","-8888"))
  data$informant.complete.factor = factor(data$informant.complete,levels=c("0","1","2"))
  data$sex.factor = factor(data$sex,levels=c("1","2"))
  data$marital.stat.factor = factor(data$marital.stat,levels=c("1","5","2","3","4","6","-9999","-8888"))
  data$ethnicity.factor = factor(data$ethnicity,levels=c("1","0","-9999","-8888"))
  data$race.factor = factor(data$race,levels=c("1","2","3","4","0","-9999","-8888"))
  data$usa.born.factor = factor(data$usa.born,levels=c("1","0","-9999","-8888"))
  data$lang.prim.factor = factor(data$lang.prim,levels=c("1","2","3","4","5","0","-9999","-8888"))
  data$lang.second...0.factor = factor(data$lang.second...0,levels=c("0","1"))
  data$lang.second...1.factor = factor(data$lang.second...1,levels=c("0","1"))
  data$lang.second...2.factor = factor(data$lang.second...2,levels=c("0","1"))
  data$lang.second...3.factor = factor(data$lang.second...3,levels=c("0","1"))
  data$lang.second...4.factor = factor(data$lang.second...4,levels=c("0","1"))
  data$lang.second...5.factor = factor(data$lang.second...5,levels=c("0","1"))
  data$lang.second...9.factor = factor(data$lang.second...9,levels=c("0","1"))
  data$lang.second...9999.factor = factor(data$lang.second...9999,levels=c("0","1"))
  data$lang.second....8888.factor = factor(data$lang.second....8888,levels=c("0","1"))
  data$meds.factor = factor(data$meds,levels=c("1","0","-9999"))
  data$med01.units.factor = factor(data$med01.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med01.freq.units.factor = factor(data$med01.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med01.units.day.factor = factor(data$med01.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med01.dur.units.factor = factor(data$med01.dur.units,levels=c("1","2","3","4","-9999"))
  data$med02.factor = factor(data$med02,levels=c("1","0"))
  data$med02.units.factor = factor(data$med02.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med02.freq.units.factor = factor(data$med02.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med02.units.day.factor = factor(data$med02.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med02.dur.units.factor = factor(data$med02.dur.units,levels=c("1","2","3","4","-9999"))
  data$med03.factor = factor(data$med03,levels=c("1","0"))
  data$med03.units.factor = factor(data$med03.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med03.freq.units.factor = factor(data$med03.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med03.units.day.factor = factor(data$med03.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med03.dur.units.factor = factor(data$med03.dur.units,levels=c("1","2","3","4","-9999"))
  data$med04.factor = factor(data$med04,levels=c("1","0"))
  data$med04.units.factor = factor(data$med04.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med04.freq.units.factor = factor(data$med04.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med04.units.day.factor = factor(data$med04.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med04.dur.units.factor = factor(data$med04.dur.units,levels=c("1","2","3","4","-9999"))
  data$med05.factor = factor(data$med05,levels=c("1","0"))
  data$med05.units.factor = factor(data$med05.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med05.freq.units.factor = factor(data$med05.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med05.units.day.factor = factor(data$med05.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med05.dur.units.factor = factor(data$med05.dur.units,levels=c("1","2","3","4","-9999"))
  data$med06.factor = factor(data$med06,levels=c("1","0"))
  data$med06.units.factor = factor(data$med06.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med06.freq.units.factor = factor(data$med06.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med06.units.day.factor = factor(data$med06.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med07.factor = factor(data$med07,levels=c("1","0"))
  data$med07.units.factor = factor(data$med07.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med07.freq.units.factor = factor(data$med07.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med07.units.day.factor = factor(data$med07.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med07.dur.units.factor = factor(data$med07.dur.units,levels=c("1","2","3","4","-9999"))
  data$med08.factor = factor(data$med08,levels=c("1","0"))
  data$med08.units.factor = factor(data$med08.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med08.freq.units.factor = factor(data$med08.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med08.units.day.factor = factor(data$med08.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med08.dur.units.factor = factor(data$med08.dur.units,levels=c("1","2","3","4","-9999"))
  data$med09.factor = factor(data$med09,levels=c("1","0"))
  data$med09.units.factor = factor(data$med09.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med09.freq.units.factor = factor(data$med09.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med09.units.day.factor = factor(data$med09.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med09.dur.units.factor = factor(data$med09.dur.units,levels=c("1","2","3","4","-9999"))
  data$med10.factor = factor(data$med10,levels=c("1","0"))
  data$med10.units.factor = factor(data$med10.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med10.freq.units.factor = factor(data$med10.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med10.units.day.factor = factor(data$med10.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med10.dur.units.factor = factor(data$med10.dur.units,levels=c("1","2","3","4","-9999"))
  data$med11.factor = factor(data$med11,levels=c("1","0"))
  data$med11.units.factor = factor(data$med11.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med11.freq.units.factor = factor(data$med11.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med11.units.day.factor = factor(data$med11.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med11.dur.units.factor = factor(data$med11.dur.units,levels=c("1","2","3","4","-9999"))
  data$med12.factor = factor(data$med12,levels=c("1","0"))
  data$med12.units.factor = factor(data$med12.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med12.freq.units.factor = factor(data$med12.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med12.units.day.factor = factor(data$med12.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med12.dur.units.factor = factor(data$med12.dur.units,levels=c("1","2","3","4","-9999"))
  data$med13.factor = factor(data$med13,levels=c("1","0"))
  data$med13.units.factor = factor(data$med13.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med13.freq.units.factor = factor(data$med13.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med13.units.day.factor = factor(data$med13.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med13.dur.units.factor = factor(data$med13.dur.units,levels=c("1","2","3","4","-9999"))
  data$med14.factor = factor(data$med14,levels=c("1","0"))
  data$med14.units.factor = factor(data$med14.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med14.freq.units.factor = factor(data$med14.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med14.units.day.factor = factor(data$med14.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med14.dur.units.factor = factor(data$med14.dur.units,levels=c("1","2","3","4","-9999"))
  data$med15.factor = factor(data$med15,levels=c("1","0"))
  data$med15.units.factor = factor(data$med15.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med15.freq.units.factor = factor(data$med15.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med15.units.day.factor = factor(data$med15.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med15.dur.units.factor = factor(data$med15.dur.units,levels=c("1","2","3","4","-9999"))
  data$med16.factor = factor(data$med16,levels=c("1","0"))
  data$med16.units.factor = factor(data$med16.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med16.freq.units.factor = factor(data$med16.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med16.units.day.factor = factor(data$med16.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med16.dur.units.factor = factor(data$med16.dur.units,levels=c("1","2","3","4","-9999"))
  data$med17.factor = factor(data$med17,levels=c("1","0"))
  data$med17.units.factor = factor(data$med17.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med17.freq.units.factor = factor(data$med17.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med17.units.day.factor = factor(data$med17.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med17.dur.units.factor = factor(data$med17.dur.units,levels=c("1","2","3","4","-9999"))
  data$med18.factor = factor(data$med18,levels=c("1","0"))
  data$med18.units.factor = factor(data$med18.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med18.freq.units.factor = factor(data$med18.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med18.units.day.factor = factor(data$med18.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med18.dur.units.factor = factor(data$med18.dur.units,levels=c("1","2","3","4","-9999"))
  data$med19.factor = factor(data$med19,levels=c("1","0"))
  data$med19.units.factor = factor(data$med19.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med19.freq.units.factor = factor(data$med19.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med19.units.day.factor = factor(data$med19.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med19.dur.units.factor = factor(data$med19.dur.units,levels=c("1","2","3","4","-9999"))
  data$med20.factor = factor(data$med20,levels=c("1","0"))
  data$med20.units.factor = factor(data$med20.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med20.freq.units.factor = factor(data$med20.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med20.units.day.factor = factor(data$med20.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med20.dur.units.factor = factor(data$med20.dur.units,levels=c("1","2","3","4","-9999"))
  data$med21.factor = factor(data$med21,levels=c("1","0"))
  data$med21.units.factor = factor(data$med21.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med21.freq.units.factor = factor(data$med21.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med21.units.day.factor = factor(data$med21.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med21.dur.units.factor = factor(data$med21.dur.units,levels=c("1","2","3","4","-9999"))
  data$med22.factor = factor(data$med22,levels=c("1","0"))
  data$med22.units.factor = factor(data$med22.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med22.freq.units.factor = factor(data$med22.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med22.units.day.factor = factor(data$med22.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med22.dur.units.factor = factor(data$med22.dur.units,levels=c("1","2","3","4","-9999"))
  data$med23.factor = factor(data$med23,levels=c("1","0"))
  data$med23.units.factor = factor(data$med23.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med23.freq.units.factor = factor(data$med23.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med23.units.day.factor = factor(data$med23.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med23.dur.units.factor = factor(data$med23.dur.units,levels=c("1","2","3","4","-9999"))
  data$med24.factor = factor(data$med24,levels=c("1","0"))
  data$med24.units.factor = factor(data$med24.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med24.freq.units.factor = factor(data$med24.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med24.units.day.factor = factor(data$med24.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med24.dur.units.factor = factor(data$med24.dur.units,levels=c("1","2","3","4","-9999"))
  data$med25.factor = factor(data$med25,levels=c("1","0"))
  data$med25.units.factor = factor(data$med25.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med25.freq.units.factor = factor(data$med25.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med25.units.day.factor = factor(data$med25.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med25.dur.units.factor = factor(data$med25.dur.units,levels=c("1","2","3","4","-9999"))
  data$med26.factor = factor(data$med26,levels=c("1","0"))
  data$med26.units.factor = factor(data$med26.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med26.freq.units.factor = factor(data$med26.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med26.units.day.factor = factor(data$med26.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med26.dur.units.factor = factor(data$med26.dur.units,levels=c("1","2","3","4","-9999"))
  data$med27.factor = factor(data$med27,levels=c("1","0"))
  data$med27.units.factor = factor(data$med27.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med27.freq.units.factor = factor(data$med27.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med27.units.day.factor = factor(data$med27.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med27.dur.units.factor = factor(data$med27.dur.units,levels=c("1","2","3","4","-9999"))
  data$med28.factor = factor(data$med28,levels=c("1","0"))
  data$med28.units.factor = factor(data$med28.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med28.freq.units.factor = factor(data$med28.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med28.units.day.factor = factor(data$med28.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med28.dur.units.factor = factor(data$med28.dur.units,levels=c("1","2","3","4","-9999"))
  data$med29.factor = factor(data$med29,levels=c("1","0"))
  data$med29.units.factor = factor(data$med29.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med29.freq.units.factor = factor(data$med29.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med29.units.day.factor = factor(data$med29.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med29.dur.units.factor = factor(data$med29.dur.units,levels=c("1","2","3","4","-9999"))
  data$med30.factor = factor(data$med30,levels=c("1","0"))
  data$med30.units.factor = factor(data$med30.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med30.freq.units.factor = factor(data$med30.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med30.units.day.factor = factor(data$med30.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med30.dur.units.factor = factor(data$med30.dur.units,levels=c("1","2","3","4","-9999"))
  data$med31.factor = factor(data$med31,levels=c("1","0"))
  data$med31.units.factor = factor(data$med31.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med31.freq.units.factor = factor(data$med31.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med31.units.day.factor = factor(data$med31.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med31.dur.units.factor = factor(data$med31.dur.units,levels=c("1","2","3","4","-9999"))
  data$med32.factor = factor(data$med32,levels=c("1","0"))
  data$med32.units.factor = factor(data$med32.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med32.freq.units.factor = factor(data$med32.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med32.units.day.factor = factor(data$med32.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med32.dur.units.factor = factor(data$med32.dur.units,levels=c("1","2","3","4","-9999"))
  data$med33.factor = factor(data$med33,levels=c("1","0"))
  data$med33.units.factor = factor(data$med33.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med33.freq.units.factor = factor(data$med33.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med33.units.day.factor = factor(data$med33.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med33.dur.units.factor = factor(data$med33.dur.units,levels=c("1","2","3","4","-9999"))
  data$med34.factor = factor(data$med34,levels=c("1","0"))
  data$med34.units.factor = factor(data$med34.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med34.freq.units.factor = factor(data$med34.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med34.units.day.factor = factor(data$med34.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med34.dur.units.factor = factor(data$med34.dur.units,levels=c("1","2","3","4","-9999"))
  data$med35.factor = factor(data$med35,levels=c("1","0"))
  data$med35.units.factor = factor(data$med35.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med35.freq.units.factor = factor(data$med35.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med35.units.day.factor = factor(data$med35.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med35.dur.units.factor = factor(data$med35.dur.units,levels=c("1","2","3","4","-9999"))
  data$med36.factor = factor(data$med36,levels=c("1","0"))
  data$med36.units.factor = factor(data$med36.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med36.freq.units.factor = factor(data$med36.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med36.units.day.factor = factor(data$med36.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med36.dur.units.factor = factor(data$med36.dur.units,levels=c("1","2","3","4","-9999"))
  data$med37.factor = factor(data$med37,levels=c("1","0"))
  data$med37.units.factor = factor(data$med37.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med37.freq.units.factor = factor(data$med37.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med37.units.day.factor = factor(data$med37.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med37.dur.units.factor = factor(data$med37.dur.units,levels=c("1","2","3","4","-9999"))
  data$med38.factor = factor(data$med38,levels=c("1","0"))
  data$med38.units.factor = factor(data$med38.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med38.freq.units.factor = factor(data$med38.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med38.units.day.factor = factor(data$med38.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med38.dur.units.factor = factor(data$med38.dur.units,levels=c("1","2","3","4","-9999"))
  data$med39.factor = factor(data$med39,levels=c("1","0"))
  data$med39.units.factor = factor(data$med39.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med39.freq.units.factor = factor(data$med39.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med39.units.day.factor = factor(data$med39.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med39.dur.units.factor = factor(data$med39.dur.units,levels=c("1","2","3","4","-9999"))
  data$med40.factor = factor(data$med40,levels=c("1","0"))
  data$med40.units.factor = factor(data$med40.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med40.freq.units.factor = factor(data$med40.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med40.units.day.factor = factor(data$med40.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med40.dur.units.factor = factor(data$med40.dur.units,levels=c("1","2","3","4","-9999"))
  data$med41.factor = factor(data$med41,levels=c("1","0"))
  data$med41.units.factor = factor(data$med41.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med41.freq.units.factor = factor(data$med41.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med41.units.day.factor = factor(data$med41.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med41.dur.units.factor = factor(data$med41.dur.units,levels=c("1","2","3","4","-9999"))
  data$med42.factor = factor(data$med42,levels=c("1","0"))
  data$med42.units.factor = factor(data$med42.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med42.freq.units.factor = factor(data$med42.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med42.units.day.factor = factor(data$med42.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med42.dur.units.factor = factor(data$med42.dur.units,levels=c("1","2","3","4","-9999"))
  data$med43.factor = factor(data$med43,levels=c("1","0"))
  data$med43.units.factor = factor(data$med43.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med43.freq.units.factor = factor(data$med43.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med43.units.day.factor = factor(data$med43.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med43.dur.units.factor = factor(data$med43.dur.units,levels=c("1","2","3","4","-9999"))
  data$med44.factor = factor(data$med44,levels=c("1","0"))
  data$med44.units.factor = factor(data$med44.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med44.freq.units.factor = factor(data$med44.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med44.units.day.factor = factor(data$med44.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med44.dur.units.factor = factor(data$med44.dur.units,levels=c("1","2","3","4","-9999"))
  data$med45.factor = factor(data$med45,levels=c("1","0"))
  data$med45.units.factor = factor(data$med45.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med45.freq.units.factor = factor(data$med45.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med45.units.day.factor = factor(data$med45.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med45.dur.units.factor = factor(data$med45.dur.units,levels=c("1","2","3","4","-9999"))
  data$med46.factor = factor(data$med46,levels=c("1","0"))
  data$med46.units.factor = factor(data$med46.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med46.freq.units.factor = factor(data$med46.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med46.units.day.factor = factor(data$med46.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med46.dur.units.factor = factor(data$med46.dur.units,levels=c("1","2","3","4","-9999"))
  data$med47.factor = factor(data$med47,levels=c("1","0"))
  data$med47.units.factor = factor(data$med47.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med47.freq.units.factor = factor(data$med47.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med47.units.day.factor = factor(data$med47.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med47.dur.units.factor = factor(data$med47.dur.units,levels=c("1","2","3","4","-9999"))
  data$med48.factor = factor(data$med48,levels=c("1","0"))
  data$med48.units.factor = factor(data$med48.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med48.freq.units.factor = factor(data$med48.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med48.units.day.factor = factor(data$med48.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med48.dur.units.factor = factor(data$med48.dur.units,levels=c("1","2","3","4","-9999"))
  data$med49.factor = factor(data$med49,levels=c("1","0"))
  data$med49.units.factor = factor(data$med49.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med49.freq.units.factor = factor(data$med49.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med49.units.day.factor = factor(data$med49.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med49.dur.units.factor = factor(data$med49.dur.units,levels=c("1","2","3","4","-9999"))
  data$med50.factor = factor(data$med50,levels=c("1","0"))
  data$med50.units.factor = factor(data$med50.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med50.freq.units.factor = factor(data$med50.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med50.units.day.factor = factor(data$med50.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med50.dur.units.factor = factor(data$med50.dur.units,levels=c("1","2","3","4","-9999"))
  data$med51.factor = factor(data$med51,levels=c("1","0"))
  data$med51.units.factor = factor(data$med51.units,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med51.freq.units.factor = factor(data$med51.freq.units,levels=c("1","2","3","5","4","-9999"))
  data$med51.units.day.factor = factor(data$med51.units.day,levels=c("16","8","1","2","9","11","13","3","10","4","5","6","15","14","12","18","7","-9999"))
  data$med51.dur.units.factor = factor(data$med51.dur.units,levels=c("1","2","3","4","-9999"))
  data$med.add.factor = factor(data$med.add,levels=c("1","0"))
  data$mhx.angina.factor = factor(data$mhx.angina,levels=c("1","0","-9999","-8888"))
  data$mhx.angina.exer.factor = factor(data$mhx.angina.exer,levels=c("1","0","-9999","-8888"))
  data$mhx.arrhyth.factor = factor(data$mhx.arrhyth,levels=c("1","0","-9999","-8888"))
  data$mhx.arthritis.factor = factor(data$mhx.arthritis,levels=c("1","0","-9999","-8888"))
  data$mhx.arthritis.type.factor = factor(data$mhx.arthritis.type,levels=c("1","2","0","-9999","-8888"))
  data$mhx.afib.factor = factor(data$mhx.afib,levels=c("1","0","-9999","-8888"))
  data$mhx.bp.factor = factor(data$mhx.bp,levels=c("1","0","-9999","-8888"))
  data$mhx.bp.type.factor = factor(data$mhx.bp.type,levels=c("1","2","-9999","-8888"))
  data$mhx.arrest.factor = factor(data$mhx.arrest,levels=c("1","0","-9999","-8888"))
  data$mhx.cad.factor = factor(data$mhx.cad,levels=c("1","0","-9999","-8888"))
  data$mhx.bypass.factor = factor(data$mhx.bypass,levels=c("1","0","-9999","-8888"))
  data$mhx.cancer.factor = factor(data$mhx.cancer,levels=c("1","0","-9999","-8888"))
  data$mhx.hf.factor = factor(data$mhx.hf,levels=c("1","0","-9999","-8888"))
  data$mhx.diabetes.factor = factor(data$mhx.diabetes,levels=c("1","0","-9999","-8888"))
  data$mhx.diabetes.control...1.factor = factor(data$mhx.diabetes.control...1,levels=c("0","1"))
  data$mhx.diabetes.control...2.factor = factor(data$mhx.diabetes.control...2,levels=c("0","1"))
  data$mhx.diabetes.control...3.factor = factor(data$mhx.diabetes.control...3,levels=c("0","1"))
  data$mhx.diabetes.control....9999.factor = factor(data$mhx.diabetes.control....9999,levels=c("0","1"))
  data$mhx.diabetes.control....8888.factor = factor(data$mhx.diabetes.control....8888,levels=c("0","1"))
  data$mhx.hep.factor = factor(data$mhx.hep,levels=c("1","0","-9999","-8888"))
  data$mhx.hep.type.factor = factor(data$mhx.hep.type,levels=c("1","2","3","4","-9999","-8888"))
  data$mhx.chol.factor = factor(data$mhx.chol,levels=c("1","0","-9999","-8888"))
  data$mhx.mi.factor = factor(data$mhx.mi,levels=c("1","0","-9999","-8888"))
  data$mhx.heartsurg.factor = factor(data$mhx.heartsurg,levels=c("1","0","-9999","-8888"))
  data$mhx.glauc.factor = factor(data$mhx.glauc,levels=c("1","0","-9999","-8888"))
  data$mhx.cataracts.factor = factor(data$mhx.cataracts,levels=c("1","0","-9999","-8888"))
  data$mhx.kidney.factor = factor(data$mhx.kidney,levels=c("1","0","-9999","-8888"))
  data$mhx.liver.factor = factor(data$mhx.liver,levels=c("1","0","-9999","-8888"))
  data$mhx.lupus.factor = factor(data$mhx.lupus,levels=c("1","0","-9999","-8888"))
  data$mhx.lyme.factor = factor(data$mhx.lyme,levels=c("1","0","-9999","-8888"))
  data$mhx.mvp.factor = factor(data$mhx.mvp,levels=c("1","0","-9999","-8888"))
  data$mhx.seiz.factor = factor(data$mhx.seiz,levels=c("1","0","-9999","-8888"))
  data$mhx.angio.factor = factor(data$mhx.angio,levels=c("1","0","-9999","-8888"))
  data$mhx.thyroid.factor = factor(data$mhx.thyroid,levels=c("1","0","-9999","-8888"))
  data$mhx.thyroid.type.factor = factor(data$mhx.thyroid.type,levels=c("1","2","-9999","-8888"))
  data$mhx.tremors.factor = factor(data$mhx.tremors,levels=c("1","0","-9999","-8888"))
  data$mhx.tb.factor = factor(data$mhx.tb,levels=c("1","0","-9999","-8888"))
  data$mhx.valve.factor = factor(data$mhx.valve,levels=c("1","0","-9999","-8888"))
  data$mhx.ad.factor = factor(data$mhx.ad,levels=c("1","0","-9999","-8888"))
  data$mhx.mci.factor = factor(data$mhx.mci,levels=c("1","0","-9999","-8888"))
  data$mhx.tbi.factor = factor(data$mhx.tbi,levels=c("1","0","-9999","-8888"))
  data$mhx.tbi.consc.factor = factor(data$mhx.tbi.consc,levels=c("1","0","-9999","-8888"))
  data$mhx.bleed.factor = factor(data$mhx.bleed,levels=c("1","0","-9999","-8888"))
  data$mhx.stroke.factor = factor(data$mhx.stroke,levels=c("1","0","-9999","-8888"))
  data$mhx.tia.factor = factor(data$mhx.tia,levels=c("1","0","-9999","-8888"))
  data$mhx.infect.factor = factor(data$mhx.infect,levels=c("1","0","-9999","-8888"))
  data$mhx.headaches.factor = factor(data$mhx.headaches,levels=c("1","0","-9999","-8888"))
  data$mhx.ms.factor = factor(data$mhx.ms,levels=c("1","0","-9999","-8888"))
  data$mhx.pd.factor = factor(data$mhx.pd,levels=c("1","0","-9999","-8888"))
  data$mhx.anx.factor = factor(data$mhx.anx,levels=c("1","0","-9999","-8888"))
  data$mhx.anx.tx.factor = factor(data$mhx.anx.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.depress.factor = factor(data$mhx.depress,levels=c("1","0","-9999","-8888"))
  data$mhx.depress.tx.factor = factor(data$mhx.depress.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.schiz.factor = factor(data$mhx.schiz,levels=c("1","0","-9999","-8888"))
  data$mhx.schiz.tx.factor = factor(data$mhx.schiz.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.psych.factor = factor(data$mhx.psych,levels=c("1","0","-9999","-8888"))
  data$mhx.etoh.factor = factor(data$mhx.etoh,levels=c("1","0","-9999","-8888"))
  data$mhx.etoh.tx.factor = factor(data$mhx.etoh.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.abuse.factor = factor(data$mhx.abuse,levels=c("1","0","-9999","-8888"))
  data$mhx.abuse.tx.factor = factor(data$mhx.abuse.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.tobac.factor = factor(data$mhx.tobac,levels=c("1","0","-9999","-8888"))
  data$mhx.ld.factor = factor(data$mhx.ld,levels=c("1","0","-9999","-8888"))
  data$mhx.troub.learning.factor = factor(data$mhx.troub.learning,levels=c("1","0","-9999","-8888"))
  data$mhx.add.factor = factor(data$mhx.add,levels=c("1","0","-9999","-8888"))
  data$mhx.add.tx.factor = factor(data$mhx.add.tx,levels=c("1","0","-9999","-8888"))
  data$mhx.pain.factor = factor(data$mhx.pain,levels=c("1","0","-9999","-8888"))
  data$mhx.neuropathy.factor = factor(data$mhx.neuropathy,levels=c("1","0","-9999","-8888"))
  data$mhx.asthma.factor = factor(data$mhx.asthma,levels=c("1","0","-9999","-8888"))
  data$mhx.copd.factor = factor(data$mhx.copd,levels=c("1","0","-9999","-8888"))
  data$mhx.murmur.factor = factor(data$mhx.murmur,levels=c("1","0","-9999","-8888"))
  data$mhx.rhythm.factor = factor(data$mhx.rhythm,levels=c("1","0","-9999","-8888"))
  data$mhx.athero.factor = factor(data$mhx.athero,levels=c("1","0","-9999","-8888"))
  data$mhx.branch.factor = factor(data$mhx.branch,levels=c("1","0","-9999","-8888"))
  data$mhx.macular.factor = factor(data$mhx.macular,levels=c("1","0","-9999","-8888"))
  data$mhx.restlessleg.factor = factor(data$mhx.restlessleg,levels=c("1","0","-9999","-8888"))
  data$mhx.apnea.factor = factor(data$mhx.apnea,levels=c("1","0","-9999","-8888"))
  data$mhx.rem.factor = factor(data$mhx.rem,levels=c("1","0","-9999","-8888"))
  data$mhx.pad.factor = factor(data$mhx.pad,levels=c("1","0","-9999","-8888"))
  data$mhx.cblind.factor = factor(data$mhx.cblind,levels=c("1","0","-9999","-8888"))
  data$mhx.other1.factor = factor(data$mhx.other1,levels=c("1","0","-9999","-8888"))
  data$mhx.other2.factor = factor(data$mhx.other2,levels=c("1","0","-9999","-8888"))
  data$mhx.other3.factor = factor(data$mhx.other3,levels=c("1","0","-9999","-8888"))
  data$mhx.other4.factor = factor(data$mhx.other4,levels=c("1","0","-9999","-8888"))
  data$mhx.other5.factor = factor(data$mhx.other5,levels=c("1","0","-9999","-8888"))
  data$medical.history.complete.factor = factor(data$medical.history.complete,levels=c("0","1","2"))
  data$surg01.factor = factor(data$surg01,levels=c("1","0","-9999","-8888"))
  data$surg01.implant.factor = factor(data$surg01.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg02.factor = factor(data$surg02,levels=c("1","0"))
  data$surg02.implant.factor = factor(data$surg02.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg03.factor = factor(data$surg03,levels=c("1","0"))
  data$surg03.implant.factor = factor(data$surg03.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg04.factor = factor(data$surg04,levels=c("1","0"))
  data$surg04.implant.factor = factor(data$surg04.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg05.factor = factor(data$surg05,levels=c("1","0"))
  data$surg05.implant.factor = factor(data$surg05.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg06.factor = factor(data$surg06,levels=c("1","0"))
  data$surg06.implant.factor = factor(data$surg06.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg07.factor = factor(data$surg07,levels=c("1","0"))
  data$surg07.implant.factor = factor(data$surg07.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg08.factor = factor(data$surg08,levels=c("1","0"))
  data$surg08.implant.factor = factor(data$surg08.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg09.factor = factor(data$surg09,levels=c("1","0"))
  data$surg09.implant.factor = factor(data$surg09.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg10.factor = factor(data$surg10,levels=c("1","0"))
  data$surg10.implant.factor = factor(data$surg10.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg11.factor = factor(data$surg11,levels=c("1","0"))
  data$surg11.implant.factor = factor(data$surg11.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg12.factor = factor(data$surg12,levels=c("1","0"))
  data$surg12.implant.factor = factor(data$surg12.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg13.factor = factor(data$surg13,levels=c("1","0"))
  data$surg13.implant.factor = factor(data$surg13.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg14.factor = factor(data$surg14,levels=c("1","0"))
  data$surg14.implant.factor = factor(data$surg14.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg15.factor = factor(data$surg15,levels=c("1","0"))
  data$surg15.implant.factor = factor(data$surg15.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg16.factor = factor(data$surg16,levels=c("1","0"))
  data$surg16.implant.factor = factor(data$surg16.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg17.factor = factor(data$surg17,levels=c("1","0"))
  data$surg17.implant.factor = factor(data$surg17.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg18.factor = factor(data$surg18,levels=c("1","0"))
  data$surg18.implant.factor = factor(data$surg18.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg19.factor = factor(data$surg19,levels=c("1","0"))
  data$surg19.implant.factor = factor(data$surg19.implant,levels=c("1","2","0","-9999","-8888"))
  data$surg20.factor = factor(data$surg20,levels=c("1","0"))
  data$surg20.implant.factor = factor(data$surg20.implant,levels=c("1","2","0","-9999","-8888"))
  data$surgical.history.complete.factor = factor(data$surgical.history.complete,levels=c("0","1","2"))
  data$scc01.factor = factor(data$scc01,levels=c("1","0","-9999","-8888"))
  data$scc02.factor = factor(data$scc02,levels=c("1","0","-9999","-8888"))
  data$scc03.factor = factor(data$scc03,levels=c("1","0","-9999","-8888"))
  data$scc04.factor = factor(data$scc04,levels=c("1","0","-9999","-8888"))
  data$scc05.factor = factor(data$scc05,levels=c("1","0","-9999","-8888"))
  data$scc06.factor = factor(data$scc06,levels=c("1","0","-9999","-8888"))
  data$scc07.factor = factor(data$scc07,levels=c("1","0","-9999","-8888"))
  data$scc08.factor = factor(data$scc08,levels=c("1","0","-9999","-8888"))
  data$scc09.factor = factor(data$scc09,levels=c("1","0","-9999","-8888"))
  data$scc10.factor = factor(data$scc10,levels=c("1","0","-9999","-8888"))
  data$cognitive.complaint.self.complete.factor = factor(data$cognitive.complaint.self.complete,levels=c("0","1","2"))
  data$icc01.factor = factor(data$icc01,levels=c("1","0","-9999","-8888"))
  data$icc02.factor = factor(data$icc02,levels=c("1","0","-9999","-8888"))
  data$icc03.factor = factor(data$icc03,levels=c("1","0","-9999","-8888"))
  data$icc04.factor = factor(data$icc04,levels=c("1","0","-9999","-8888"))
  data$icc05.factor = factor(data$icc05,levels=c("1","0","-9999","-8888"))
  data$icc06.factor = factor(data$icc06,levels=c("1","0","-9999","-8888"))
  data$icc07.factor = factor(data$icc07,levels=c("1","0","-9999","-8888"))
  data$icc08.factor = factor(data$icc08,levels=c("1","0","-9999","-8888"))
  data$icc09.factor = factor(data$icc09,levels=c("1","0","-9999","-8888"))
  data$icc10.factor = factor(data$icc10,levels=c("1","0","-9999","-8888"))
  data$cognitive.complaint.informant.complete.factor = factor(data$cognitive.complaint.informant.complete,levels=c("0","1","2"))
  data$ccqself01.factor = factor(data$ccqself01,levels=c("1","0","-9999","-8888"))
  data$ccqself02.factor = factor(data$ccqself02,levels=c("0","1","-9999","-8888"))
  data$ccqself03.factor = factor(data$ccqself03,levels=c("1","0","-9999","-8888"))
  data$ccqself04.factor = factor(data$ccqself04,levels=c("1","0","-9999","-8888"))
  data$ccqself05.factor = factor(data$ccqself05,levels=c("1","0","-9999","-8888"))
  data$ccqself06.factor = factor(data$ccqself06,levels=c("1","0","-9999","-8888"))
  data$ccqself07.factor = factor(data$ccqself07,levels=c("1","0","-9999","-8888"))
  data$ccqself08.factor = factor(data$ccqself08,levels=c("1","0","-9999","-8888"))
  data$ccqself09.factor = factor(data$ccqself09,levels=c("1","0","-9999","-8888"))
  data$ccqself10.factor = factor(data$ccqself10,levels=c("1","0","-9999","-8888"))
  data$ccqself11.factor = factor(data$ccqself11,levels=c("1","0","-9999","-8888"))
  data$ccqself12.factor = factor(data$ccqself12,levels=c("1","0","-9999","-8888"))
  data$ccqself13.factor = factor(data$ccqself13,levels=c("1","0","-9999","-8888"))
  data$ccqself14.factor = factor(data$ccqself14,levels=c("1","0","-9999","-8888"))
  data$ccqself15.factor = factor(data$ccqself15,levels=c("1","0","-9999","-8888"))
  data$ccqself16.factor = factor(data$ccqself16,levels=c("1","0","-9999","-8888"))
  data$ccqself17.factor = factor(data$ccqself17,levels=c("1","0","-9999","-8888"))
  data$ccqself18.factor = factor(data$ccqself18,levels=c("1","0","-9999","-8888"))
  data$ccqself19.factor = factor(data$ccqself19,levels=c("1","0","-9999","-8888"))
  data$ccqself20.factor = factor(data$ccqself20,levels=c("1","0","-9999","-8888"))
  data$ccqself21.factor = factor(data$ccqself21,levels=c("1","0","-9999","-8888"))
  data$ccqself22.factor = factor(data$ccqself22,levels=c("1","0","-9999","-8888"))
  data$ccqself23.factor = factor(data$ccqself23,levels=c("1","0","-9999","-8888"))
  data$ccqself24.factor = factor(data$ccqself24,levels=c("1","0","-9999","-8888"))
  data$ccqself25.factor = factor(data$ccqself25,levels=c("1","0","-9999","-8888"))
  data$ccqself26.factor = factor(data$ccqself26,levels=c("1","0","-9999","-8888"))
  data$ccqself27.factor = factor(data$ccqself27,levels=c("1","0","-9999","-8888"))
  data$ccqself28.factor = factor(data$ccqself28,levels=c("1","0","-9999","-8888"))
  data$ccqself29.factor = factor(data$ccqself29,levels=c("1","0","-9999","-8888"))
  data$ccqself30.factor = factor(data$ccqself30,levels=c("1","0","-9999","-8888"))
  data$ccqself31.factor = factor(data$ccqself31,levels=c("1","0","-9999","-8888"))
  data$ccqself32.factor = factor(data$ccqself32,levels=c("1","0","-9999","-8888"))
  data$ccqself33.factor = factor(data$ccqself33,levels=c("1","0","-9999","-8888"))
  data$ccqself34.factor = factor(data$ccqself34,levels=c("1","0","-9999","-8888"))
  data$ccqself35.factor = factor(data$ccqself35,levels=c("1","0","-9999","-8888"))
  data$ccqself36.factor = factor(data$ccqself36,levels=c("1","0","-9999","-8888"))
  data$ccqself37.factor = factor(data$ccqself37,levels=c("1","0","-9999","-8888"))
  data$ccqself38.factor = factor(data$ccqself38,levels=c("1","0","-9999","-8888"))
  data$ccqself39.factor = factor(data$ccqself39,levels=c("1","0","-9999","-8888"))
  data$ccqself40.factor = factor(data$ccqself40,levels=c("1","0","-9999","-8888"))
  data$ccqself41.factor = factor(data$ccqself41,levels=c("1","0","-9999","-8888"))
  data$ccqself42.factor = factor(data$ccqself42,levels=c("0","1","-9999","-8888"))
  data$ccqself43.factor = factor(data$ccqself43,levels=c("0","1","-9999","-8888"))
  data$ccqself46.factor = factor(data$ccqself46,levels=c("3","2","1","-9999","-8888"))
  data$ccqself47.factor = factor(data$ccqself47,levels=c("3","2","1","-9999","-8888"))
  data$ccqself48.factor = factor(data$ccqself48,levels=c("3","2","1","-9999","-8888"))
  data$ccqself49.factor = factor(data$ccqself49,levels=c("3","2","1","-9999","-8888"))
  data$ccqself50.factor = factor(data$ccqself50,levels=c("3","2","1","-9999","-8888"))
  data$ccqself51.factor = factor(data$ccqself51,levels=c("3","2","1","-9999","-8888"))
  data$ccqself52.factor = factor(data$ccqself52,levels=c("3","2","1","-9999","-8888"))
  data$ccqself53.factor = factor(data$ccqself53,levels=c("3","2","1","-9999","-8888"))
  data$ccqself54.factor = factor(data$ccqself54,levels=c("3","2","1","-9999","-8888"))
  data$ccqself55.factor = factor(data$ccqself55,levels=c("3","2","1","-9999","-8888"))
  data$ccqself56.factor = factor(data$ccqself56,levels=c("3","2","1","-9999","-8888"))
  data$ccqself57.factor = factor(data$ccqself57,levels=c("3","2","1","-9999","-8888"))
  data$ccqself58.factor = factor(data$ccqself58,levels=c("3","2","1","-9999","-8888"))
  data$ccqself59.factor = factor(data$ccqself59,levels=c("3","2","1","-9999","-8888"))
  data$cognitive.questionnaire.self.complete.factor = factor(data$cognitive.questionnaire.self.complete,levels=c("0","1","2"))
  data$ccqinform01.factor = factor(data$ccqinform01,levels=c("1","0","-9999","-8888"))
  data$ccqinform02.factor = factor(data$ccqinform02,levels=c("0","1","-9999","-8888"))
  data$ccqinform03.factor = factor(data$ccqinform03,levels=c("1","0","-9999","-8888"))
  data$ccqinform04.factor = factor(data$ccqinform04,levels=c("1","0","-9999","-8888"))
  data$ccqinform05.factor = factor(data$ccqinform05,levels=c("1","0","-9999","-8888"))
  data$ccqinform06.factor = factor(data$ccqinform06,levels=c("1","0","-9999","-8888"))
  data$ccqinform07.factor = factor(data$ccqinform07,levels=c("1","0","-9999","-8888"))
  data$ccqinform08.factor = factor(data$ccqinform08,levels=c("1","0","-9999","-8888"))
  data$ccqinform09.factor = factor(data$ccqinform09,levels=c("1","0","-9999","-8888"))
  data$ccqinform10.factor = factor(data$ccqinform10,levels=c("1","0","-9999","-8888"))
  data$ccqinform11.factor = factor(data$ccqinform11,levels=c("1","0","-9999","-8888"))
  data$ccqinform12.factor = factor(data$ccqinform12,levels=c("1","0","-9999","-8888"))
  data$ccqinform13.factor = factor(data$ccqinform13,levels=c("1","0","-9999","-8888"))
  data$ccqinform14.factor = factor(data$ccqinform14,levels=c("1","0","-9999","-8888"))
  data$ccqinform15.factor = factor(data$ccqinform15,levels=c("1","0","-9999","-8888"))
  data$ccqinform16.factor = factor(data$ccqinform16,levels=c("1","0","-9999","-8888"))
  data$ccqinform17.factor = factor(data$ccqinform17,levels=c("1","0","-9999","-8888"))
  data$ccqinform18.factor = factor(data$ccqinform18,levels=c("1","0","-9999","-8888"))
  data$ccqinform19.factor = factor(data$ccqinform19,levels=c("1","0","-9999","-8888"))
  data$ccqinform20.factor = factor(data$ccqinform20,levels=c("1","0","-9999","-8888"))
  data$ccqinform21.factor = factor(data$ccqinform21,levels=c("1","0","-9999","-8888"))
  data$ccqinform22.factor = factor(data$ccqinform22,levels=c("1","0","-9999","-8888"))
  data$ccqinform23.factor = factor(data$ccqinform23,levels=c("1","0","-9999","-8888"))
  data$ccqinform24.factor = factor(data$ccqinform24,levels=c("1","0","-9999","-8888"))
  data$ccqinform25.factor = factor(data$ccqinform25,levels=c("1","0","-9999","-8888"))
  data$ccqinform26.factor = factor(data$ccqinform26,levels=c("1","0","-9999","-8888"))
  data$ccqinform27.factor = factor(data$ccqinform27,levels=c("1","0","-9999","-8888"))
  data$ccqinform28.factor = factor(data$ccqinform28,levels=c("1","0","-9999","-8888"))
  data$ccqinform29.factor = factor(data$ccqinform29,levels=c("1","0","-9999","-8888"))
  data$ccqinform30.factor = factor(data$ccqinform30,levels=c("1","0","-9999","-8888"))
  data$ccqinform31.factor = factor(data$ccqinform31,levels=c("1","0","-9999","-8888"))
  data$ccqinform32.factor = factor(data$ccqinform32,levels=c("1","0","-9999","-8888"))
  data$ccqinform33.factor = factor(data$ccqinform33,levels=c("1","0","-9999","-8888"))
  data$ccqinform34.factor = factor(data$ccqinform34,levels=c("1","0","-9999","-8888"))
  data$ccqinform35.factor = factor(data$ccqinform35,levels=c("1","0","-9999","-8888"))
  data$ccqinform36.factor = factor(data$ccqinform36,levels=c("1","0","-9999","-8888"))
  data$ccqinform37.factor = factor(data$ccqinform37,levels=c("1","0","-9999","-8888"))
  data$ccqinform38.factor = factor(data$ccqinform38,levels=c("1","0","-9999","-8888"))
  data$ccqinform39.factor = factor(data$ccqinform39,levels=c("1","0","-9999","-8888"))
  data$ccqinform40.factor = factor(data$ccqinform40,levels=c("1","0","-9999","-8888"))
  data$ccqinform41.factor = factor(data$ccqinform41,levels=c("1","0","-9999","-8888"))
  data$ccqinform42.factor = factor(data$ccqinform42,levels=c("0","1","-9999","-8888"))
  data$ccqinform43.factor = factor(data$ccqinform43,levels=c("0","1","-9999","-8888"))
  data$ccqinform44.factor = factor(data$ccqinform44,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform45.factor = factor(data$ccqinform45,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform46.factor = factor(data$ccqinform46,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform47.factor = factor(data$ccqinform47,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform48.factor = factor(data$ccqinform48,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform49.factor = factor(data$ccqinform49,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform50.factor = factor(data$ccqinform50,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform51.factor = factor(data$ccqinform51,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform52.factor = factor(data$ccqinform52,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform53.factor = factor(data$ccqinform53,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform54.factor = factor(data$ccqinform54,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform55.factor = factor(data$ccqinform55,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform56.factor = factor(data$ccqinform56,levels=c("3","2","1","-9999","-8888"))
  data$ccqinform57.factor = factor(data$ccqinform57,levels=c("3","2","1","-9999","-8888"))
  data$cognitive.questionnaire.informant.complete.factor = factor(data$cognitive.questionnaire.informant.complete,levels=c("0","1","2"))
  data$cogdif01.factor = factor(data$cogdif01,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif02.factor = factor(data$cogdif02,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif03.factor = factor(data$cogdif03,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif04.factor = factor(data$cogdif04,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif05.factor = factor(data$cogdif05,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif06.factor = factor(data$cogdif06,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif07.factor = factor(data$cogdif07,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif08.factor = factor(data$cogdif08,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif09.factor = factor(data$cogdif09,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif10.factor = factor(data$cogdif10,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif11.factor = factor(data$cogdif11,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif12.factor = factor(data$cogdif12,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif13.factor = factor(data$cogdif13,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif14.factor = factor(data$cogdif14,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif15.factor = factor(data$cogdif15,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif16.factor = factor(data$cogdif16,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif17.factor = factor(data$cogdif17,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif18.factor = factor(data$cogdif18,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif19.factor = factor(data$cogdif19,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif20.factor = factor(data$cogdif20,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif21.factor = factor(data$cogdif21,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif22.factor = factor(data$cogdif22,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif23.factor = factor(data$cogdif23,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif24.factor = factor(data$cogdif24,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif25.factor = factor(data$cogdif25,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif26.factor = factor(data$cogdif26,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif27.factor = factor(data$cogdif27,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif28.factor = factor(data$cogdif28,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif29.factor = factor(data$cogdif29,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif30.factor = factor(data$cogdif30,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif31.factor = factor(data$cogdif31,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif32.factor = factor(data$cogdif32,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif33.factor = factor(data$cogdif33,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif34.factor = factor(data$cogdif34,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif35.factor = factor(data$cogdif35,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif36.factor = factor(data$cogdif36,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif37.factor = factor(data$cogdif37,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif38.factor = factor(data$cogdif38,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif39.factor = factor(data$cogdif39,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif40.factor = factor(data$cogdif40,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cogdif41.factor = factor(data$cogdif41,levels=c("0","1","2","3","4","-9999","-8888"))
  data$cognitive.difficulties.complete.factor = factor(data$cognitive.difficulties.complete,levels=c("0","1","2"))
  data$mfq01.factor = factor(data$mfq01,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02a.factor = factor(data$mfq02a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02b.factor = factor(data$mfq02b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02c.factor = factor(data$mfq02c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02d.factor = factor(data$mfq02d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02e.factor = factor(data$mfq02e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02f.factor = factor(data$mfq02f,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02g.factor = factor(data$mfq02g,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02h.factor = factor(data$mfq02h,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02i.factor = factor(data$mfq02i,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02j.factor = factor(data$mfq02j,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02k.factor = factor(data$mfq02k,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02l.factor = factor(data$mfq02l,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02m.factor = factor(data$mfq02m,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02n.factor = factor(data$mfq02n,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02o.factor = factor(data$mfq02o,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02p.factor = factor(data$mfq02p,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02q.factor = factor(data$mfq02q,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq02r.factor = factor(data$mfq02r,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq03a.factor = factor(data$mfq03a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq03b.factor = factor(data$mfq03b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq03c.factor = factor(data$mfq03c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq03d.factor = factor(data$mfq03d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq03e.factor = factor(data$mfq03e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq04a.factor = factor(data$mfq04a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq04b.factor = factor(data$mfq04b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq04c.factor = factor(data$mfq04c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq04d.factor = factor(data$mfq04d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq04e.factor = factor(data$mfq04e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq05a.factor = factor(data$mfq05a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq05b.factor = factor(data$mfq05b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq05c.factor = factor(data$mfq05c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq05d.factor = factor(data$mfq05d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06a.factor = factor(data$mfq06a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06b.factor = factor(data$mfq06b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06c.factor = factor(data$mfq06c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06d.factor = factor(data$mfq06d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06e.factor = factor(data$mfq06e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06f.factor = factor(data$mfq06f,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06g.factor = factor(data$mfq06g,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06h.factor = factor(data$mfq06h,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06i.factor = factor(data$mfq06i,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06j.factor = factor(data$mfq06j,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06k.factor = factor(data$mfq06k,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06l.factor = factor(data$mfq06l,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06m.factor = factor(data$mfq06m,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06n.factor = factor(data$mfq06n,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06o.factor = factor(data$mfq06o,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06p.factor = factor(data$mfq06p,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06q.factor = factor(data$mfq06q,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq06r.factor = factor(data$mfq06r,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07a.factor = factor(data$mfq07a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07b.factor = factor(data$mfq07b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07c.factor = factor(data$mfq07c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07d.factor = factor(data$mfq07d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07e.factor = factor(data$mfq07e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07f.factor = factor(data$mfq07f,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07g.factor = factor(data$mfq07g,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq07h.factor = factor(data$mfq07h,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq08a.factor = factor(data$mfq08a,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq08b.factor = factor(data$mfq08b,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq08c.factor = factor(data$mfq08c,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq08d.factor = factor(data$mfq08d,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$mfq08e.factor = factor(data$mfq08e,levels=c("1","2","3","4","5","6","7","-9999","-8888"))
  data$memory.functioning.questionnaire.complete.factor = factor(data$memory.functioning.questionnaire.complete,levels=c("0","1","2"))
  data$ecogself.mem01.factor = factor(data$ecogself.mem01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem02.factor = factor(data$ecogself.mem02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem03.factor = factor(data$ecogself.mem03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem04.factor = factor(data$ecogself.mem04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem05.factor = factor(data$ecogself.mem05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem06.factor = factor(data$ecogself.mem06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem07.factor = factor(data$ecogself.mem07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.mem08.factor = factor(data$ecogself.mem08,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang01.factor = factor(data$ecogself.lang01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang02.factor = factor(data$ecogself.lang02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang03.factor = factor(data$ecogself.lang03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang04.factor = factor(data$ecogself.lang04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang05.factor = factor(data$ecogself.lang05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang06.factor = factor(data$ecogself.lang06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang07.factor = factor(data$ecogself.lang07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang08.factor = factor(data$ecogself.lang08,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.lang09.factor = factor(data$ecogself.lang09,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis01.factor = factor(data$ecogself.vis01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis02.factor = factor(data$ecogself.vis02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis03.factor = factor(data$ecogself.vis03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis04.factor = factor(data$ecogself.vis04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis05.factor = factor(data$ecogself.vis05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis06.factor = factor(data$ecogself.vis06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.vis07.factor = factor(data$ecogself.vis07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.plan01.factor = factor(data$ecogself.plan01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.plan02.factor = factor(data$ecogself.plan02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.plan03.factor = factor(data$ecogself.plan03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.plan04.factor = factor(data$ecogself.plan04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.plan05.factor = factor(data$ecogself.plan05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org01.factor = factor(data$ecogself.org01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org02.factor = factor(data$ecogself.org02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org03.factor = factor(data$ecogself.org03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org04.factor = factor(data$ecogself.org04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org05.factor = factor(data$ecogself.org05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.org06.factor = factor(data$ecogself.org06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.attn01.factor = factor(data$ecogself.attn01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.attn02.factor = factor(data$ecogself.attn02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.attn03.factor = factor(data$ecogself.attn03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecogself.attn04.factor = factor(data$ecogself.attn04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$everyday.cognition.self.complete.factor = factor(data$everyday.cognition.self.complete,levels=c("0","1","2"))
  data$ecoginf.mem01.factor = factor(data$ecoginf.mem01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem02.factor = factor(data$ecoginf.mem02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem03.factor = factor(data$ecoginf.mem03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem04.factor = factor(data$ecoginf.mem04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem05.factor = factor(data$ecoginf.mem05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem06.factor = factor(data$ecoginf.mem06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem07.factor = factor(data$ecoginf.mem07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.mem08.factor = factor(data$ecoginf.mem08,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang01.factor = factor(data$ecoginf.lang01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang02.factor = factor(data$ecoginf.lang02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang03.factor = factor(data$ecoginf.lang03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang04.factor = factor(data$ecoginf.lang04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang05.factor = factor(data$ecoginf.lang05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang06.factor = factor(data$ecoginf.lang06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang07.factor = factor(data$ecoginf.lang07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang08.factor = factor(data$ecoginf.lang08,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.lang09.factor = factor(data$ecoginf.lang09,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis01.factor = factor(data$ecoginf.vis01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis02.factor = factor(data$ecoginf.vis02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis03.factor = factor(data$ecoginf.vis03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis04.factor = factor(data$ecoginf.vis04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis05.factor = factor(data$ecoginf.vis05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis06.factor = factor(data$ecoginf.vis06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.vis07.factor = factor(data$ecoginf.vis07,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.plan01.factor = factor(data$ecoginf.plan01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.plan02.factor = factor(data$ecoginf.plan02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.plan03.factor = factor(data$ecoginf.plan03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.plan04.factor = factor(data$ecoginf.plan04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.plan05.factor = factor(data$ecoginf.plan05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org01.factor = factor(data$ecoginf.org01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org02.factor = factor(data$ecoginf.org02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org03.factor = factor(data$ecoginf.org03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org04.factor = factor(data$ecoginf.org04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org05.factor = factor(data$ecoginf.org05,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.org06.factor = factor(data$ecoginf.org06,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.attn01.factor = factor(data$ecoginf.attn01,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.attn02.factor = factor(data$ecoginf.attn02,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.attn03.factor = factor(data$ecoginf.attn03,levels=c("1","2","3","4","0","-9999","-8888"))
  data$ecoginf.attn04.factor = factor(data$ecoginf.attn04,levels=c("1","2","3","4","0","-9999","-8888"))
  data$everyday.cognition.informant.complete.factor = factor(data$everyday.cognition.informant.complete,levels=c("0","1","2"))
  data$fcadl01.factor = factor(data$fcadl01,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl02.factor = factor(data$fcadl02,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl03.factor = factor(data$fcadl03,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl04.factor = factor(data$fcadl04,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl05.factor = factor(data$fcadl05,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl06.factor = factor(data$fcadl06,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl07.factor = factor(data$fcadl07,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl08.factor = factor(data$fcadl08,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl09.factor = factor(data$fcadl09,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl10.factor = factor(data$fcadl10,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl11.factor = factor(data$fcadl11,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl12.factor = factor(data$fcadl12,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl13.factor = factor(data$fcadl13,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl14.factor = factor(data$fcadl14,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl15.factor = factor(data$fcadl15,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl16.factor = factor(data$fcadl16,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl17.factor = factor(data$fcadl17,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl18.factor = factor(data$fcadl18,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl19.factor = factor(data$fcadl19,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl20.factor = factor(data$fcadl20,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl21.factor = factor(data$fcadl21,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl22.factor = factor(data$fcadl22,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl23.factor = factor(data$fcadl23,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl24.factor = factor(data$fcadl24,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl25.factor = factor(data$fcadl25,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl26.factor = factor(data$fcadl26,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl27.factor = factor(data$fcadl27,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl28.factor = factor(data$fcadl28,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl29.factor = factor(data$fcadl29,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl30.factor = factor(data$fcadl30,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl31.factor = factor(data$fcadl31,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl32.factor = factor(data$fcadl32,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl33.factor = factor(data$fcadl33,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl34.factor = factor(data$fcadl34,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl35.factor = factor(data$fcadl35,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl36.factor = factor(data$fcadl36,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl37.factor = factor(data$fcadl37,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl38.factor = factor(data$fcadl38,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl39.factor = factor(data$fcadl39,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl40.factor = factor(data$fcadl40,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl41.factor = factor(data$fcadl41,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl42.factor = factor(data$fcadl42,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl43.factor = factor(data$fcadl43,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl44.factor = factor(data$fcadl44,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl45.factor = factor(data$fcadl45,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl46.factor = factor(data$fcadl46,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl47.factor = factor(data$fcadl47,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl48.factor = factor(data$fcadl48,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl49.factor = factor(data$fcadl49,levels=c("1","2","3","4","5","-9999","-8888"))
  data$fcadl50.factor = factor(data$fcadl50,levels=c("1","2","3","4","5","-9999","-8888"))
  data$functional.capacities.for.adls.complete.factor = factor(data$functional.capacities.for.adls.complete,levels=c("0","1","2"))
  data$food01a.factor = factor(data$food01a,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01b.factor = factor(data$food01b,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01c.factor = factor(data$food01c,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01d.factor = factor(data$food01d,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01e.factor = factor(data$food01e,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01f.factor = factor(data$food01f,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01g.factor = factor(data$food01g,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01h.factor = factor(data$food01h,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01i.factor = factor(data$food01i,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01j.factor = factor(data$food01j,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01k.factor = factor(data$food01k,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01l.factor = factor(data$food01l,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01m.factor = factor(data$food01m,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01n.factor = factor(data$food01n,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food01o.factor = factor(data$food01o,levels=c("0","1","2","3","4","5","6","7","-9999","-8888"))
  data$food02.factor = factor(data$food02,levels=c("0","1","2","3","4","5","-9999","-8888"))
  data$food03.factor = factor(data$food03,levels=c("1","2","3","-9999","-8888"))
  data$nci.quick.food.scan.complete.factor = factor(data$nci.quick.food.scan.complete,levels=c("0","1","2"))
  data$minnesota.leisure.time.activities.complete.factor = factor(data$minnesota.leisure.time.activities.complete,levels=c("0","1","2"))
  data$champs41.wk.other1.factor = factor(data$champs41.wk.other1,levels=c("1","0","-9999","-8888"))
  data$champs41.wk.other2.factor = factor(data$champs41.wk.other2,levels=c("1","0","-9999","-8888"))
  data$champs41.wk.other3.factor = factor(data$champs41.wk.other3,levels=c("1","0","-9999","-8888"))
  data$champs01.hrs.factor = factor(data$champs01.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs02.hrs.factor = factor(data$champs02.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs03.hrs.factor = factor(data$champs03.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs04.hrs.factor = factor(data$champs04.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs05.hrs.factor = factor(data$champs05.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs06.hrs.factor = factor(data$champs06.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs07.hrs.factor = factor(data$champs07.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs08.hrs.factor = factor(data$champs08.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs09.hrs.factor = factor(data$champs09.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs10.hrs.factor = factor(data$champs10.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs11.hrs.factor = factor(data$champs11.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs12.hrs.factor = factor(data$champs12.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs13.hrs.factor = factor(data$champs13.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs14.hrs.factor = factor(data$champs14.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs15.hrs.factor = factor(data$champs15.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs16.hrs.factor = factor(data$champs16.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs17.hrs.factor = factor(data$champs17.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs18.hrs.factor = factor(data$champs18.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs19.hrs.factor = factor(data$champs19.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs20.hrs.factor = factor(data$champs20.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs21.hrs.factor = factor(data$champs21.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs22.hrs.factor = factor(data$champs22.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs23.hrs.factor = factor(data$champs23.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs24.hrs.factor = factor(data$champs24.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs25.hrs.factor = factor(data$champs25.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs26.hrs.factor = factor(data$champs26.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs27.hrs.factor = factor(data$champs27.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs28.hrs.factor = factor(data$champs28.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs29.hrs.factor = factor(data$champs29.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs30.hrs.factor = factor(data$champs30.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs31.hrs.factor = factor(data$champs31.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs32.hrs.factor = factor(data$champs32.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs33.hrs.factor = factor(data$champs33.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs34.hrs.factor = factor(data$champs34.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs35.hrs.factor = factor(data$champs35.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs36.hrs.factor = factor(data$champs36.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs37.hrs.factor = factor(data$champs37.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs38.hrs.factor = factor(data$champs38.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs39.hrs.factor = factor(data$champs39.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs40.hrs.factor = factor(data$champs40.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs41.other1.hrs.factor = factor(data$champs41.other1.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs41.other2.hrs.factor = factor(data$champs41.other2.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs41.other3.hrs.factor = factor(data$champs41.other3.hrs,levels=c("0.5","1.75","3.75","5.75","7.75","9.75","-9999","-8888"))
  data$champs.complete.factor = factor(data$champs.complete,levels=c("0","1","2"))
  data$psqi.completer.factor = factor(data$psqi.completer,levels=c("0","1"))
  data$psqi.cant.sleep.factor = factor(data$psqi.cant.sleep,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.wake.up.factor = factor(data$psqi.wake.up,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.bathroom.factor = factor(data$psqi.bathroom,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.breathe.factor = factor(data$psqi.breathe,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.cough.snore.factor = factor(data$psqi.cough.snore,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.cold.factor = factor(data$psqi.cold,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.hot.factor = factor(data$psqi.hot,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.dreams.factor = factor(data$psqi.dreams,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.pain.factor = factor(data$psqi.pain,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.other.often.factor = factor(data$psqi.other.often,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.sleep.quality.factor = factor(data$psqi.sleep.quality,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.medicine.factor = factor(data$psqi.medicine,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.awake.factor = factor(data$psqi.awake,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.enthusiasm.factor = factor(data$psqi.enthusiasm,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.partner.factor = factor(data$psqi.partner,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.loud.snoring.factor = factor(data$psqi.loud.snoring,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.pauses.breath.factor = factor(data$psqi.pauses.breath,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.twitching.factor = factor(data$psqi.twitching,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.disorientation.factor = factor(data$psqi.disorientation,levels=c("0","1","2","3","-9999","-8888"))
  data$psqi.restlessness.often.factor = factor(data$psqi.restlessness.often,levels=c("0","1","2","3","-9999","-8888"))
  data$pittsburgh.sleep.quality.index.complete.factor = factor(data$pittsburgh.sleep.quality.index.complete,levels=c("0","1","2"))
  data$qids01.factor = factor(data$qids01,levels=c("0","1","2","3","-9999","-8888"))
  data$qids02.factor = factor(data$qids02,levels=c("0","1","2","3","-9999","-8888"))
  data$qids03.factor = factor(data$qids03,levels=c("0","1","2","3","-9999","-8888"))
  data$qids04.factor = factor(data$qids04,levels=c("0","1","2","3","-9999","-8888"))
  data$qids05.factor = factor(data$qids05,levels=c("0","1","2","3","-9999","-8888"))
  data$qids06.factor = factor(data$qids06,levels=c("0","1","2","3","-9999","-8888"))
  data$qids07.factor = factor(data$qids07,levels=c("0","1","2","3","-9999","-8888"))
  data$qids08.factor = factor(data$qids08,levels=c("0","1","2","3","-9999","-8888"))
  data$qids09.factor = factor(data$qids09,levels=c("0","1","2","3","-9999","-8888"))
  data$qids10.factor = factor(data$qids10,levels=c("0","1","2","3","-9999","-8888"))
  data$qids11.factor = factor(data$qids11,levels=c("0","1","2","3","-9999","-8888"))
  data$qids12.factor = factor(data$qids12,levels=c("0","1","2","3","-9999","-8888"))
  data$qids13.factor = factor(data$qids13,levels=c("0","1","2","3","-9999","-8888"))
  data$qids14.factor = factor(data$qids14,levels=c("0","1","2","3","-9999","-8888"))
  data$qids15.factor = factor(data$qids15,levels=c("0","1","2","3","-9999","-8888"))
  data$qids16.factor = factor(data$qids16,levels=c("0","1","2","3","-9999","-8888"))
  data$qids.complete.factor = factor(data$qids.complete,levels=c("0","1","2"))
  data$gds1.factor = factor(data$gds1,levels=c("0","1","-9999","-8888"))
  data$gds2.factor = factor(data$gds2,levels=c("1","0","-9999","-8888"))
  data$gds3.factor = factor(data$gds3,levels=c("1","0","-9999","-8888"))
  data$gds4.factor = factor(data$gds4,levels=c("1","0","-9999","-8888"))
  data$gds5.factor = factor(data$gds5,levels=c("0","1","-9999","-8888"))
  data$gds6.factor = factor(data$gds6,levels=c("1","0","-9999","-8888"))
  data$gds7.factor = factor(data$gds7,levels=c("0","1","-9999","-8888"))
  data$gds8.factor = factor(data$gds8,levels=c("1","0","-9999","-8888"))
  data$gds9.factor = factor(data$gds9,levels=c("0","1","-9999","-8888"))
  data$gds10.factor = factor(data$gds10,levels=c("1","0","-9999","-8888"))
  data$gds11.factor = factor(data$gds11,levels=c("1","0","-9999","-8888"))
  data$gds12.factor = factor(data$gds12,levels=c("1","0","-9999","-8888"))
  data$gds13.factor = factor(data$gds13,levels=c("1","0","-9999","-8888"))
  data$gds14.factor = factor(data$gds14,levels=c("1","0","-9999","-8888"))
  data$gds15.factor = factor(data$gds15,levels=c("0","1","-9999","-8888"))
  data$gds16.factor = factor(data$gds16,levels=c("1","0","-9999","-8888"))
  data$gds17.factor = factor(data$gds17,levels=c("1","0","-9999","-8888"))
  data$gds18.factor = factor(data$gds18,levels=c("1","0","-9999","-8888"))
  data$gds19.factor = factor(data$gds19,levels=c("0","1","-9999","-8888"))
  data$gds20.factor = factor(data$gds20,levels=c("1","0","-9999","-8888"))
  data$gds21.factor = factor(data$gds21,levels=c("0","1","-9999","-8888"))
  data$gds22.factor = factor(data$gds22,levels=c("1","0","-9999","-8888"))
  data$gds23.factor = factor(data$gds23,levels=c("1","0","-9999","-8888"))
  data$gds24.factor = factor(data$gds24,levels=c("1","0","-9999","-8888"))
  data$gds25.factor = factor(data$gds25,levels=c("1","0","-9999","-8888"))
  data$gds26.factor = factor(data$gds26,levels=c("1","0","-9999","-8888"))
  data$gds27.factor = factor(data$gds27,levels=c("0","1","-9999","-8888"))
  data$gds28.factor = factor(data$gds28,levels=c("1","0","-9999","-8888"))
  data$gds29.factor = factor(data$gds29,levels=c("0","1","-9999","-8888"))
  data$gds30.factor = factor(data$gds30,levels=c("0","1","-9999","-8888"))
  data$gds.complete.factor = factor(data$gds.complete,levels=c("0","1","2"))
  data$bldast.waking.factor = factor(data$bldast.waking,levels=c("1","0","-9999","-8888"))
  data$frail.weightloss.factor = factor(data$frail.weightloss,levels=c("1","0","-9999","-8888"))
  data$frailt.effort.factor = factor(data$frailt.effort,levels=c("0","1","2","3","-9999","-8888"))
  data$frail.going.factor = factor(data$frail.going,levels=c("0","1","2","3","-9999","-8888"))
  data$physical.and.frailty.assessment.complete.factor = factor(data$physical.and.frailty.assessment.complete,levels=c("0","1","2"))
  data$echo.read.factor = factor(data$echo.read,levels=c("3","1","2"))
  data$echo.sonog.factor = factor(data$echo.sonog,levels=c("1"))
  data$echo.nlwm...1.factor = factor(data$echo.nlwm...1,levels=c("0","1"))
  data$echo.nlwm...2.factor = factor(data$echo.nlwm...2,levels=c("0","1"))
  data$echo.nlwm...3.factor = factor(data$echo.nlwm...3,levels=c("0","1"))
  data$echo.nlwm....9999.factor = factor(data$echo.nlwm....9999,levels=c("0","1"))
  data$echo.akinwm...1.factor = factor(data$echo.akinwm...1,levels=c("0","1"))
  data$echo.akinwm...2.factor = factor(data$echo.akinwm...2,levels=c("0","1"))
  data$echo.akinwm...3.factor = factor(data$echo.akinwm...3,levels=c("0","1"))
  data$echo.akinwm...4.factor = factor(data$echo.akinwm...4,levels=c("0","1"))
  data$echo.akinwm...5.factor = factor(data$echo.akinwm...5,levels=c("0","1"))
  data$echo.akinwm...6.factor = factor(data$echo.akinwm...6,levels=c("0","1"))
  data$echo.akinwm....9999.factor = factor(data$echo.akinwm....9999,levels=c("0","1"))
  data$echo.hypowm...1.factor = factor(data$echo.hypowm...1,levels=c("0","1"))
  data$echo.hypowm...2.factor = factor(data$echo.hypowm...2,levels=c("0","1"))
  data$echo.hypowm...3.factor = factor(data$echo.hypowm...3,levels=c("0","1"))
  data$echo.hypowm...4.factor = factor(data$echo.hypowm...4,levels=c("0","1"))
  data$echo.hypowm...5.factor = factor(data$echo.hypowm...5,levels=c("0","1"))
  data$echo.hypowm...6.factor = factor(data$echo.hypowm...6,levels=c("0","1"))
  data$echo.hypowm....9999.factor = factor(data$echo.hypowm....9999,levels=c("0","1"))
  data$echo.vd.factor = factor(data$echo.vd,levels=c("1","2","-9999"))
  data$echo.vd.ts.factor = factor(data$echo.vd.ts,levels=c("0","1","2","3","4","5","-9999"))
  data$echo.vd.tr.factor = factor(data$echo.vd.tr,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.mr.factor = factor(data$echo.vd.mr,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.ms.factor = factor(data$echo.vd.ms,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.ar.factor = factor(data$echo.vd.ar,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.as.factor = factor(data$echo.vd.as,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.pr.factor = factor(data$echo.vd.pr,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.vd.ps.factor = factor(data$echo.vd.ps,levels=c("0","1","2","3","4","5","6","7","-9999"))
  data$echo.rap.factor = factor(data$echo.rap,levels=c("3","8","15","-9999"))
  data$echo.dfxn.factor = factor(data$echo.dfxn,levels=c("1","2","3","4","5","6","-9999"))
  data$echo.effus.factor = factor(data$echo.effus,levels=c("0","1","2","3","-9999"))
  data$echo.results.factor = factor(data$echo.results,levels=c("1","2","-9999"))
  data$echo.clinicallysig.factor = factor(data$echo.clinicallysig,levels=c("1","0","-9999","-8888"))
  data$echo.rhythm.factor = factor(data$echo.rhythm,levels=c("1","2","3","4","5","0","-9999"))
  data$echocardiogram.complete.factor = factor(data$echocardiogram.complete,levels=c("0","1","2"))
  data$bld.nurse.factor = factor(data$bld.nurse,levels=c("13","6","2","4","14","18","7","12","16","5","8","1","17","11","19","10","15","9","3","-9999"))
  data$clinical.blood.work.complete.factor = factor(data$clinical.blood.work.complete,levels=c("0","1","2"))
  data$cmr.read.factor = factor(data$cmr.read,levels=c("1","2","9","-9999"))
  data$cmr.quality.factor = factor(data$cmr.quality,levels=c("1","2","3","-9999"))
  data$cardiac.mri.complete.factor = factor(data$cardiac.mri.complete,levels=c("0","1","2"))
  data$np.examiner.factor = factor(data$np.examiner,levels=c("5","7","1","2","3","6","4","8","-9999"))
  data$np.moca.invalid.factor = factor(data$np.moca.invalid,levels=c("1","0"))
  data$np.cvlt.invalid.factor = factor(data$np.cvlt.invalid,levels=c("1","0"))
  data$np.tower01.factor = factor(data$np.tower01,levels=c("0","1","2","-9999"))
  data$np.tower02.factor = factor(data$np.tower02,levels=c("0","1","2","-9999"))
  data$np.tower03.factor = factor(data$np.tower03,levels=c("0","1","2","3","-9999"))
  data$np.tower04.factor = factor(data$np.tower04,levels=c("0","1","2","3","-9999","-8888"))
  data$np.tower05.factor = factor(data$np.tower05,levels=c("0","1","2","3","4","-9999","-8888"))
  data$np.tower06.factor = factor(data$np.tower06,levels=c("0","1","2","3","4","-9999","-8888"))
  data$np.tower07.factor = factor(data$np.tower07,levels=c("0","1","2","3","4","-9999","-8888"))
  data$np.tower08.factor = factor(data$np.tower08,levels=c("0","1","2","3","4","-9999","-8888"))
  data$np.tower09.factor = factor(data$np.tower09,levels=c("0","1","2","3","4","-9999","-8888"))
  data$np.tower.ruleviol.cumperc.factor = factor(data$np.tower.ruleviol.cumperc,levels=c("0","1","2","3","4","5","7","9","10","11","13","15","16","17","18","20","22","23","24","34","37","48","49","50","56","67","69","74","100","-9999"))
  data$np.tower.invalid.factor = factor(data$np.tower.invalid,levels=c("1","0"))
  data$np.digsymb.invalid.factor = factor(data$np.digsymb.invalid,levels=c("1","0"))
  data$np.color.cumpercerr.factor = factor(data$np.color.cumpercerr,levels=c("100","30","25","18","5","2","1","-9999"))
  data$np.word.cumpercerr.factor = factor(data$np.word.cumpercerr,levels=c("100","10","2","1","-9999"))
  data$np.inhibit.cumpercscerr.factor = factor(data$np.inhibit.cumpercscerr,levels=c("100","30","28","25","23","20","15","12","10","8","5","7","4","2","1","-9999"))
  data$np.inhibit.cumpercucerr.factor = factor(data$np.inhibit.cumpercucerr,levels=c("100","65","40","38","22","20","25","15","12","10","8","6","5","3","2","1","-9999"))
  data$np.cw.invalid.factor = factor(data$np.cw.invalid,levels=c("1","0"))
  data$np.hvot.invalid.factor = factor(data$np.hvot.invalid,levels=c("1","0"))
  data$np.fas.invalid.factor = factor(data$np.fas.invalid,levels=c("1","0"))
  data$np.anim.invalid.factor = factor(data$np.anim.invalid,levels=c("1","0"))
  data$np.tmta.cumperc.seqerr.factor = factor(data$np.tmta.cumperc.seqerr,levels=c("100","9","7","5","3","1","-9999"))
  data$np.tmta.cumperc.seterr.factor = factor(data$np.tmta.cumperc.seterr,levels=c("100","2","1","0","-9999"))
  data$np.tmtb.cumperc.seqerr.factor = factor(data$np.tmtb.cumperc.seqerr,levels=c("100","52","42","40","28","20","19","17","9","8","7","6","5","4","3","2","1","-9999"))
  data$np.tmtb.cumperc.seterr.factor = factor(data$np.tmtb.cumperc.seterr,levels=c("100","50","34","30","20","14","12","7","6","5","4","3","2","1","-9999"))
  data$np.tmta.invalid.factor = factor(data$np.tmta.invalid,levels=c("1","0"))
  data$np.tmtb.invalid.factor = factor(data$np.tmtb.invalid,levels=c("1","0"))
  data$np.bnt.invalid.factor = factor(data$np.bnt.invalid,levels=c("1","0"))
  data$np.biber.invalid.factor = factor(data$np.biber.invalid,levels=c("1","0"))
  data$neuropsychological.assessment.complete.factor = factor(data$neuropsychological.assessment.complete,levels=c("0","1","2"))

  levels(data$entry.primary.factor)=c("Erin Gainey","Mary Godfrey","Shereen Haj-Hassan","Elizabeth Lane","Christy Lee","Elle Wiggins")
  levels(data$entry.secondary.factor)=c("Cherna Cherfrere","Erin Gainey","Mary Godfrey","Shereen Haj-Hassan","Kelli Klein","Elizabeth Lane","Christy Lee","Sebastian Norrdahl","Taylor Shone","Sarah Smith","Adrianna Swift","Elle Wiggins")
  levels(data$data.entry.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$visit.brain.factor)=c("Yes","No","Missing","N/A")
  levels(data$visit.echo.factor)=c("Yes","No","Missing","N/A")
  levels(data$visit.cmr.factor)=c("Yes","No","Missing","N/A")
  levels(data$visit.depress.factor)=c("Yes","No","Missing","N/A")
  levels(data$visit.depress.note.factor)=c("Yes","No","Missing","N/A")
  levels(data$visit.notes.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$diagnosis.factor)=c("Normal","MCI","Dementia","Ambiguous At Risk","Missing")
  levels(data$nc.type.factor)=c("NC","NC At Risk")
  levels(data$mci.amnestic.factor)=c("Amnestic","Non-amnestic","Missing","N/A")
  levels(data$mci.domain.factor)=c("Single-domain","Multi-domain","Missing","N/A")
  levels(data$mci.stage.factor)=c("Early stage","Middle stage","Late stage","Missing","N/A")
  levels(data$dementia.factor)=c("Alzheimers")
  levels(data$diagnosis.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$cdr.exam.factor)=c("Amy Berman","Heather Joppich","Jeff Phillips","Joe Kim","Jeannine Skinner","Katie Gifford","Laura Logan","Liz Lane","Shereen Haj-Hassan","Yasmeen Iqbal","Missing")
  levels(data$cdr.mem.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.orient.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.judg.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.affairs.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.hobbies.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.care.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$cdr.factor)=c("0","0.5","1","2","3","Missing")
  levels(data$clinical.dementia.rating.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$functional.activities.questionnaire.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$famhx.date.multiple.factor)=c("Yes","No")
  levels(data$famhx01.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx01.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx01.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx01.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Parkinsons","Vascular","Unknown dementia type","missing")
  levels(data$famhx01.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx02.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx02.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx02.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx02.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx02.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx03.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx03.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx03.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx03.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx03.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx04.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx04.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx04.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx04.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx04.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx05.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx05.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx05.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx05.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx05.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx06.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx06.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx06.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx06.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx06.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx07.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx07.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx07.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx07.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx07.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx08.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx08.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx08.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx08.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx08.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx09.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx09.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx09.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx09.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx09.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx10.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx10.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx10.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx10.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx10.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx11.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx11.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx11.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx11.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx11.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx12.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx12.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx12.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx12.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx12.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx13.ptp.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx13.relation.ptp.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx13.prob.ptp.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx13.dx.ptp.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx13.onset.ptp.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx01.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx01.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx01.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx01.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Parkinsons","Vascular","Unknown dementia type","missing")
  levels(data$famhx01.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx02.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx02.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx02.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx02.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx02.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx03.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx03.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx03.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx03.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx03.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx04.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx04.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx04.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx04.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx04.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx05.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx05.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx05.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx05.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx05.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx06.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx06.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx06.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx06.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx06.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx07.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx07.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx07.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx07.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx07.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx08.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx08.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx08.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx08.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx08.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx09.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx09.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx09.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx09.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx09.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx10.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx10.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx10.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx10.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx10.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx11.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx11.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx11.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx11.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx11.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx12.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx12.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx12.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx12.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx12.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$famhx13.inf.factor)=c("Yes","No","Missing","N/A")
  levels(data$famhx13.relation.inf.factor)=c("mother","father","sister","brother","daughter","son","maternal grandmother","maternal grandfather","paternal grandmother","paternal grandfather","maternal great grandmother","paternal great grandmother","maternal great grandfather","paternal great grandfather","maternal aunt","maternal uncle","paternal aunt","paternal uncle","maternal great aunt","maternal great uncle","paternal great aunt","paternal great uncle","maternal 1st cousin","paternal 1st cousin","aunt","uncle","grandfather","great grandfather","grandmother","great grandmother","half brother","nephew","niece","cousin 1st","other","missing","N/A")
  levels(data$famhx13.prob.inf.factor)=c("Dementia","No diagnosis but symptoms","Missing","N/A")
  levels(data$famhx13.dx.inf.factor)=c("Alzheimers","Down syndrome","Frontotemporal/Picks","Lewy body","Vascular","Unknown dementia type","missing")
  levels(data$famhx13.onset.inf.factor)=c("Known","Unknown","Missing","N/A")
  levels(data$family.history.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$inform.method.factor)=c("Retrospective","Prospective","Missing","N/A")
  levels(data$inform.source...1.factor)=c("Unchecked","Checked")
  levels(data$inform.source...2.factor)=c("Unchecked","Checked")
  levels(data$inform.source...3.factor)=c("Unchecked","Checked")
  levels(data$inform.source...4.factor)=c("Unchecked","Checked")
  levels(data$inform.source...5.factor)=c("Unchecked","Checked")
  levels(data$inform.source...6.factor)=c("Unchecked","Checked")
  levels(data$inform.source...7.factor)=c("Unchecked","Checked")
  levels(data$inform.source...8.factor)=c("Unchecked","Checked")
  levels(data$inform.source...9.factor)=c("Unchecked","Checked")
  levels(data$inform.source...10.factor)=c("Unchecked","Checked")
  levels(data$inform.source....9999.factor)=c("Unchecked","Checked")
  levels(data$inform.source....8888.factor)=c("Unchecked","Checked")
  levels(data$inform.sex.factor)=c("Male","Female","Missing","N/A")
  levels(data$inform.relation.factor)=c("Spouse/Partner","Adult Child","Friend/Neighbor","Sibiling","Other relative, specify","Other relationship specify","Missing","N/A")
  levels(data$inform.live.factor)=c("Yes","No","Missing","N/A")
  levels(data$inform.phone.factor)=c("Daily","At least 3x/weekly","Weekly","At least 3x/month","Monthly","Less than 1x/month","Missing","N/A")
  levels(data$inform.visit.factor)=c("Daily","At least 3x/weekly","Weekly","At least 3x/month","Monthly","Less than 1x/month","Missing","N/A")
  levels(data$inform.reliability.factor)=c("Yes","No","Missing","N/A")
  levels(data$informant.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$sex.factor)=c("Male","Female")
  levels(data$marital.stat.factor)=c("Married","Domestic partner","Single","Divorced","Separated","Widowed","Missing","N/A")
  levels(data$ethnicity.factor)=c("Hispanic or Latino","Not Hispanic or Latino","Missing","N/A")
  levels(data$race.factor)=c("White/Caucasian","Black/African American","American Indian/Alaska Native","Asian","Other","Missing","N/A")
  levels(data$usa.born.factor)=c("Yes","No","Missing","N/A")
  levels(data$lang.prim.factor)=c("English","Spanish","French","German","Japanese","Other","Missing","N/A")
  levels(data$lang.second...0.factor)=c("Unchecked","Checked")
  levels(data$lang.second...1.factor)=c("Unchecked","Checked")
  levels(data$lang.second...2.factor)=c("Unchecked","Checked")
  levels(data$lang.second...3.factor)=c("Unchecked","Checked")
  levels(data$lang.second...4.factor)=c("Unchecked","Checked")
  levels(data$lang.second...5.factor)=c("Unchecked","Checked")
  levels(data$lang.second...9.factor)=c("Unchecked","Checked")
  levels(data$lang.second...9999.factor)=c("Unchecked","Checked")
  levels(data$lang.second....8888.factor)=c("Unchecked","Checked")
  levels(data$meds.factor)=c("Yes","No","Missing")
  levels(data$med01.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med01.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med01.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med01.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med02.factor)=c("Yes","No")
  levels(data$med02.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med02.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med02.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med02.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med03.factor)=c("Yes","No")
  levels(data$med03.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med03.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med03.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med03.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med04.factor)=c("Yes","No")
  levels(data$med04.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med04.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med04.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med04.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med05.factor)=c("Yes","No")
  levels(data$med05.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med05.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med05.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med05.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med06.factor)=c("Yes","No")
  levels(data$med06.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med06.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med06.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med07.factor)=c("Yes","No")
  levels(data$med07.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med07.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med07.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med07.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med08.factor)=c("Yes","No")
  levels(data$med08.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med08.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med08.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med08.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med09.factor)=c("Yes","No")
  levels(data$med09.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med09.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med09.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med09.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med10.factor)=c("Yes","No")
  levels(data$med10.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med10.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med10.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med10.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med11.factor)=c("Yes","No")
  levels(data$med11.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med11.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med11.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med11.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med12.factor)=c("Yes","No")
  levels(data$med12.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med12.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med12.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med12.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med13.factor)=c("Yes","No")
  levels(data$med13.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med13.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med13.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med13.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med14.factor)=c("Yes","No")
  levels(data$med14.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med14.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med14.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med14.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med15.factor)=c("Yes","No")
  levels(data$med15.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med15.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med15.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med15.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med16.factor)=c("Yes","No")
  levels(data$med16.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med16.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med16.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med16.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med17.factor)=c("Yes","No")
  levels(data$med17.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med17.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med17.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med17.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med18.factor)=c("Yes","No")
  levels(data$med18.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med18.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med18.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med18.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med19.factor)=c("Yes","No")
  levels(data$med19.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med19.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med19.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med19.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med20.factor)=c("Yes","No")
  levels(data$med20.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med20.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med20.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med20.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med21.factor)=c("Yes","No")
  levels(data$med21.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med21.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med21.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med21.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med22.factor)=c("Yes","No")
  levels(data$med22.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med22.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med22.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med22.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med23.factor)=c("Yes","No")
  levels(data$med23.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med23.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med23.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med23.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med24.factor)=c("Yes","No")
  levels(data$med24.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med24.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med24.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med24.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med25.factor)=c("Yes","No")
  levels(data$med25.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med25.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med25.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med25.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med26.factor)=c("Yes","No")
  levels(data$med26.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med26.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med26.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med26.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med27.factor)=c("Yes","No")
  levels(data$med27.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med27.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med27.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med27.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med28.factor)=c("Yes","No")
  levels(data$med28.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med28.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med28.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med28.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med29.factor)=c("Yes","No")
  levels(data$med29.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med29.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med29.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med29.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med30.factor)=c("Yes","No")
  levels(data$med30.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med30.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med30.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med30.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med31.factor)=c("Yes","No")
  levels(data$med31.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med31.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med31.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med31.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med32.factor)=c("Yes","No")
  levels(data$med32.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med32.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med32.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med32.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med33.factor)=c("Yes","No")
  levels(data$med33.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med33.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med33.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med33.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med34.factor)=c("Yes","No")
  levels(data$med34.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med34.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med34.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med34.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med35.factor)=c("Yes","No")
  levels(data$med35.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med35.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med35.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med35.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med36.factor)=c("Yes","No")
  levels(data$med36.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med36.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med36.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med36.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med37.factor)=c("Yes","No")
  levels(data$med37.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med37.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med37.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med37.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med38.factor)=c("Yes","No")
  levels(data$med38.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med38.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med38.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med38.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med39.factor)=c("Yes","No")
  levels(data$med39.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med39.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med39.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med39.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med40.factor)=c("Yes","No")
  levels(data$med40.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med40.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med40.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med40.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med41.factor)=c("Yes","No")
  levels(data$med41.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med41.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med41.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med41.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med42.factor)=c("Yes","No")
  levels(data$med42.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med42.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med42.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med42.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med43.factor)=c("Yes","No")
  levels(data$med43.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med43.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med43.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med43.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med44.factor)=c("Yes","No")
  levels(data$med44.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med44.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med44.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med44.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med45.factor)=c("Yes","No")
  levels(data$med45.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med45.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med45.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med45.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med46.factor)=c("Yes","No")
  levels(data$med46.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med46.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med46.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med46.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med47.factor)=c("Yes","No")
  levels(data$med47.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med47.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med47.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med47.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med48.factor)=c("Yes","No")
  levels(data$med48.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med48.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med48.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med48.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med49.factor)=c("Yes","No")
  levels(data$med49.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med49.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med49.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med49.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med50.factor)=c("Yes","No")
  levels(data$med50.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med50.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med50.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med50.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med51.factor)=c("Yes","No")
  levels(data$med51.units.factor)=c("capsule","drop(s)","g","g/mL","IU","IU/mL","IU/kg","mcg","mEq","mg","mg/mL","mL","ppm","spray(s)","tablet","unit","Other","missing")
  levels(data$med51.freq.units.factor)=c("x/day","x/week","x/month","x/year","Other","missing")
  levels(data$med51.units.day.factor)=c("capsule/day","drop(s) / day","g / day","g/mL / day","IU / day","IU/mL / day","IU/kg / day","mcg / day","mEq / day","mg / day","mg/mL / day","mL / day","ppm / day","spray(s) / day","tablet / day","unit / day","Other","missing")
  levels(data$med51.dur.units.factor)=c("day(s)","week(s)","month(s)","year(s)","missing")
  levels(data$med.add.factor)=c("Yes","No")
  levels(data$mhx.angina.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.angina.exer.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.arrhyth.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.arthritis.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.arthritis.type.factor)=c("Osteoarthritis","Rheumatoid","Other","Missing","N/A")
  levels(data$mhx.afib.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.bp.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.bp.type.factor)=c("Too high","Too low","Missing","N/A")
  levels(data$mhx.arrest.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.cad.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.bypass.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.cancer.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.hf.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.diabetes.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.diabetes.control...1.factor)=c("Unchecked","Checked")
  levels(data$mhx.diabetes.control...2.factor)=c("Unchecked","Checked")
  levels(data$mhx.diabetes.control...3.factor)=c("Unchecked","Checked")
  levels(data$mhx.diabetes.control....9999.factor)=c("Unchecked","Checked")
  levels(data$mhx.diabetes.control....8888.factor)=c("Unchecked","Checked")
  levels(data$mhx.hep.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.hep.type.factor)=c("Hepatitis A","Hepatitis B","Hepatitis C","Unknown","missing","N/A")
  levels(data$mhx.chol.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.mi.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.heartsurg.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.glauc.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.cataracts.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.kidney.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.liver.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.lupus.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.lyme.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.mvp.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.seiz.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.angio.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.thyroid.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.thyroid.type.factor)=c("Hypothyroidism (underactive)","Hyperthyroidism (overactive)","Missing","N/A")
  levels(data$mhx.tremors.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.tb.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.valve.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.ad.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.mci.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.tbi.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.tbi.consc.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.bleed.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.stroke.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.tia.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.infect.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.headaches.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.ms.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.pd.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.anx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.anx.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.depress.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.depress.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.schiz.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.schiz.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.psych.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.etoh.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.etoh.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.abuse.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.abuse.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.tobac.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.ld.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.troub.learning.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.add.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.add.tx.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.pain.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.neuropathy.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.asthma.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.copd.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.murmur.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.rhythm.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.athero.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.branch.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.macular.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.restlessleg.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.apnea.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.rem.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.pad.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.cblind.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.other1.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.other2.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.other3.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.other4.factor)=c("Yes","No","Missing","N/A")
  levels(data$mhx.other5.factor)=c("Yes","No","Missing","N/A")
  levels(data$medical.history.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$surg01.factor)=c("Yes","No","Missing","N/A")
  levels(data$surg01.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg02.factor)=c("Yes","No")
  levels(data$surg02.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg03.factor)=c("Yes","No")
  levels(data$surg03.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg04.factor)=c("Yes","No")
  levels(data$surg04.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg05.factor)=c("Yes","No")
  levels(data$surg05.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg06.factor)=c("Yes","No")
  levels(data$surg06.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg07.factor)=c("Yes","No")
  levels(data$surg07.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg08.factor)=c("Yes","No")
  levels(data$surg08.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg09.factor)=c("Yes","No")
  levels(data$surg09.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg10.factor)=c("Yes","No")
  levels(data$surg10.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg11.factor)=c("Yes","No")
  levels(data$surg11.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg12.factor)=c("Yes","No")
  levels(data$surg12.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg13.factor)=c("Yes","No")
  levels(data$surg13.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg14.factor)=c("Yes","No")
  levels(data$surg14.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg15.factor)=c("Yes","No")
  levels(data$surg15.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg16.factor)=c("Yes","No")
  levels(data$surg16.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg17.factor)=c("Yes","No")
  levels(data$surg17.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg18.factor)=c("Yes","No")
  levels(data$surg18.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg19.factor)=c("Yes","No")
  levels(data$surg19.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surg20.factor)=c("Yes","No")
  levels(data$surg20.implant.factor)=c("Yes, Single","Yes, Multiple","No","Missing","N/A")
  levels(data$surgical.history.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$scc01.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc02.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc03.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc04.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc05.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc06.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc07.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc08.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc09.factor)=c("Yes","No","Missing","N/A")
  levels(data$scc10.factor)=c("Yes","No","Missing","N/A")
  levels(data$cognitive.complaint.self.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$icc01.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc02.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc03.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc04.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc05.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc06.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc07.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc08.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc09.factor)=c("Yes","No","Missing","N/A")
  levels(data$icc10.factor)=c("Yes","No","Missing","N/A")
  levels(data$cognitive.complaint.informant.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$ccqself01.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself02.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself03.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself04.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself05.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself06.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself07.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself08.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself09.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself10.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself11.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself12.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself13.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself14.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself15.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself16.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself17.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself18.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself19.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself20.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself21.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself22.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself23.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself24.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself25.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself26.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself27.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself28.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself29.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself30.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself31.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself32.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself33.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself34.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself35.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself36.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself37.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself38.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself39.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself40.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself41.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself42.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqself43.factor)=c("Good","Poor","Missing","N/A or choose not to answer")
  levels(data$ccqself46.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself47.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself48.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself49.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself50.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself51.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself52.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself53.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself54.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself55.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqself56.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqself57.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqself58.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqself59.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$cognitive.questionnaire.self.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$ccqinform01.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform02.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform03.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform04.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform05.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform06.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform07.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform08.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform09.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform10.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform11.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform12.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform13.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform14.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform15.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform16.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform17.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform18.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform19.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform20.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform21.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform22.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform23.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform24.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform25.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform26.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform27.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform28.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform29.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform30.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform31.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform32.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform33.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform34.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform35.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform36.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform37.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform38.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform39.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform40.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform41.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform42.factor)=c("Yes","No","Missing","N/A or choose not to answer")
  levels(data$ccqinform43.factor)=c("Good","Poor","Missing","N/A or choose not to answer")
  levels(data$ccqinform44.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform45.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform46.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform47.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform48.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform49.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform50.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform51.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform52.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform53.factor)=c("Always","Sometimes","Never","Missing","N/A or choose not to answer")
  levels(data$ccqinform54.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqinform55.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqinform56.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$ccqinform57.factor)=c("Major Problems","Some Minor Problems","No Problems","Missing","N/A or choose not to answer")
  levels(data$cognitive.questionnaire.informant.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$cogdif01.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif02.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif03.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif04.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif05.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif06.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif07.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif08.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif09.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif10.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif11.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif12.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif13.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif14.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif15.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif16.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif17.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif18.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif19.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif20.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif21.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif22.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif23.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif24.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif25.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif26.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif27.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif28.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif29.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif30.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif31.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif32.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif33.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif34.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif35.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif36.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif37.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif38.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif39.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif40.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cogdif41.factor)=c("Not at all","Rarely","Sometimes","Often","Very often","Missing","N/A")
  levels(data$cognitive.difficulties.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$mfq01.factor)=c("1 - Major Problems","2 - Major Problems","3 - Some Minor Problems","4 - Some Minor Problems","5 - Some Minor Problems","6 - No Problems","7 - No Problems","Missing","N/A")
  levels(data$mfq02a.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02b.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02c.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02d.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02e.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02f.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02g.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02h.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02i.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02j.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02k.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02l.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02m.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02n.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02o.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02p.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02q.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq02r.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq03a.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq03b.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq03c.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq03d.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq03e.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq04a.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq04b.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq04c.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq04d.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq04e.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq05a.factor)=c("1 - Very Badly","2 - Very Badly","3 - Fair","4 - Fair","5 - Fair","6 - Very Well","7 - Very Well","Missing","N/A")
  levels(data$mfq05b.factor)=c("1 - Very Badly","2 - Very Badly","3 - Fair","4 - Fair","5 - Fair","6 - Very Well","7 - Very Well","Missing","N/A")
  levels(data$mfq05c.factor)=c("1 - Very Badly","2 - Very Badly","3 - Fair","4 - Fair","5 - Fair","6 - Very Well","7 - Very Well","Missing","N/A")
  levels(data$mfq05d.factor)=c("1 - Very Badly","2 - Very Badly","3 - Fair","4 - Fair","5 - Fair","6 - Very Well","7 - Very Well","Missing","N/A")
  levels(data$mfq06a.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06b.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06c.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06d.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06e.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06f.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06g.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06h.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06i.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06j.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06k.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06l.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06m.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06n.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06o.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06p.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06q.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq06r.factor)=c("1 - Very Serious","2 - Very Serious","3 - Somewhat Serious","4 - Somewhat Serious","5 - Somewhat Serious","6 - Not Serious","7 - Not Serious","Missing","N/A")
  levels(data$mfq07a.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07b.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07c.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07d.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07e.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07f.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07g.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq07h.factor)=c("1 - Always","2 - Always","3 - Sometimes","4 - Sometimes","5 - Sometimes","6 - Never","7 - Never","Missing","N/A")
  levels(data$mfq08a.factor)=c("1 - Much Worse","2 - Much Worse","3 - Same","4 - Same","5 - Same","6 - Much Better","7 - Much Better","Missing","N/A")
  levels(data$mfq08b.factor)=c("1 - Much Worse","2 - Much Worse","3 - Same","4 - Same","5 - Same","6 - Much Better","7 - Much Better","Missing","N/A")
  levels(data$mfq08c.factor)=c("1 - Much Worse","2 - Much Worse","3 - Same","4 - Same","5 - Same","6 - Much Better","7 - Much Better","Missing","N/A")
  levels(data$mfq08d.factor)=c("1 - Much Worse","2 - Much Worse","3 - Same","4 - Same","5 - Same","6 - Much Better","7 - Much Better","Missing","N/A")
  levels(data$mfq08e.factor)=c("1 - Much Worse","2 - Much Worse","3 - Same","4 - Same","5 - Same","6 - Much Better","7 - Much Better","Missing","N/A")
  levels(data$memory.functioning.questionnaire.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$ecogself.mem01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.mem08.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang08.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.lang09.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.vis07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.plan01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.plan02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.plan03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.plan04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.plan05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.org06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.attn01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.attn02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.attn03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$ecogself.attn04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","9 - Dont know","Missing","N/A")
  levels(data$everyday.cognition.self.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$ecoginf.mem01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.mem08.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang08.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.lang09.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.vis07.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.plan01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.plan02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.plan03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.plan04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.plan05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org05.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.org06.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.attn01.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.attn02.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.attn03.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$ecoginf.attn04.factor)=c("1 - Better or no change","2 - Questionable or occasionally worse","3 - Consistently a little worse","4 - Consistently much worse","0 - Dont know","Missing","N/A")
  levels(data$everyday.cognition.informant.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$fcadl01.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl02.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl03.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl04.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl05.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl06.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl07.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl08.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl09.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl10.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl11.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl12.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl13.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl14.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl15.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl16.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl17.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl18.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl19.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl20.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl21.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl22.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl23.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl24.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl25.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl26.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl27.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl28.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl29.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl30.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl31.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl32.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl33.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl34.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl35.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl36.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl37.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl38.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl39.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl40.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl41.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl42.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl43.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl44.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl45.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl46.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl47.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl48.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl49.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$fcadl50.factor)=c("Not at all","A little bit","Moderately","Quite a bit","Very much","Missing","N/A")
  levels(data$functional.capacities.for.adls.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$food01a.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01b.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01c.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01d.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01e.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01f.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01g.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01h.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01i.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01j.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01k.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01l.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01m.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01n.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food01o.factor)=c("Never","Less than once per month","1-3 times per month","1-2 times per week","3-4 times per week","5-6 times per week","1 time per day","2 or more times per day","Missing","N/A")
  levels(data$food02.factor)=c("Didnt use margarine","Almost never","About 1/4 of the time","About 1/2 of the time","About 3/4 of the time","Almost always or always","Missing","N/A")
  levels(data$food03.factor)=c("High","Medium","Low","Missing","N/A")
  levels(data$nci.quick.food.scan.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$minnesota.leisure.time.activities.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$champs41.wk.other1.factor)=c("Yes","No","Missing","N/A")
  levels(data$champs41.wk.other2.factor)=c("Yes","No","missing","N/A")
  levels(data$champs41.wk.other3.factor)=c("Yes","No","missing","N/A")
  levels(data$champs01.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs02.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs03.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs04.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs05.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs06.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs07.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs08.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs09.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs10.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs11.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs12.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs13.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs14.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs15.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs16.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs17.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs18.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs19.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs20.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs21.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs22.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs23.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs24.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs25.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs26.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs27.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs28.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs29.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs30.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs31.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs32.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs33.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs34.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs35.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs36.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs37.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs38.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs39.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs40.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs41.other1.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs41.other2.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs41.other3.hrs.factor)=c("Less than 1 hour","1-2.5 hours","3-4.5 hours","5-6.5 hours","7-8.5 hours","9 or more hours","Missing","N/A")
  levels(data$champs.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$psqi.completer.factor)=c("Participant","Proxy")
  levels(data$psqi.cant.sleep.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.wake.up.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.bathroom.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.breathe.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.cough.snore.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.cold.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.hot.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.dreams.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.pain.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.other.often.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.sleep.quality.factor)=c("Very good","Fairly good","Fairly bad","Very bad","Missing","N/A")
  levels(data$psqi.medicine.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.awake.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.enthusiasm.factor)=c("No problem at all","Only a very slight problem","Somewhat of a problem","A very big problem","Missing","N/A")
  levels(data$psqi.partner.factor)=c("No bed partner or room mate","Partner/room mate in other room","Partner in same room, but not same bed","Partner in same bed","Missing","N/A")
  levels(data$psqi.loud.snoring.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.pauses.breath.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.twitching.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.disorientation.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$psqi.restlessness.often.factor)=c("Not during the past month","Less than once a week","Once or twice a week","Three or more times a week","Missing","N/A")
  levels(data$pittsburgh.sleep.quality.index.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$qids01.factor)=c("0 - I never take longer than 30 minutes to fall asleep","1 - I take at least 30 minutes to fall asleep, less than half the time","2 - I take at least 30 minutes to fall asleep, more than half the time","3 - I take more than 60 minutes to fall asleep, more than half the time","missing","N/A")
  levels(data$qids02.factor)=c("0 - I do not wake up at night","1 - I have a restless, light sleep with a few brief awakenings each night","2 - I wake up at least once a night, but I go back to sleep easily","3 - I awaken more than once a night and stay awake for 20 minutes or more, more than half the time","missing","N/A")
  levels(data$qids03.factor)=c("0 - Most of the time, I awaken no more than 30 minutes before I need to get up.","1 - More than half the time, I awaken more than 30 minutes before I need to get up.","2 - I almost always awaken at least one hour or so before I need to, but I go back to sleep eventually.","3 - I awaken at least one hour before I need to, and cant go back to sleep.","missing","N/A")
  levels(data$qids04.factor)=c("0 - I sleep no longer than 7-8 hours/night, without napping during the day.","1 - I sleep no longer than 10 hours in a 24-hour period including naps.","2 - I sleep no longer than 12 hours in a 24-hour period including naps.","3 - I sleep longer than 12 hours in a 24-hour period including naps.","missing","N/A")
  levels(data$qids05.factor)=c("0 - I do not feel sad.","1 - I feel sad less than half the time.","2 - I feel sad more than half the time.","3 - I feel sad nearly all of the time.","Missing","N/A")
  levels(data$qids06.factor)=c("0 - There is no change in my usual appetite.","1 - I eat somewhat less often or lesser amounts of food than usual.","2 - I eat much less than usual and only with personal effort.","3 - I rarely eat within a 24-hour period, and only with extreme personal effort or when others persuade me to eat.","Missing","N/A")
  levels(data$qids07.factor)=c("0 - There is no change from my usual appetite.","1 - I feel a need to eat more frequently than usual.","2 - I regularly eat more often and/or greater amounts of food than usual.","3 - I feel driven to overeat both at mealtime and between meals.","Missing","N/A")
  levels(data$qids08.factor)=c("0 - I have not had a change in my weight.","1 - I feel as if I have had a slight weight loss.","2 - I have lost 2 pounds or more.","3 - I have lost 5 pounds or more.","Missing","N/A")
  levels(data$qids09.factor)=c("0 - I have not had a change in my weight.","1 - I feel as if I have had a slight weight gain.","2 - I have gained 2 pounds or more.","3 - I have gained 5 pounds or more.","Missing","N/A")
  levels(data$qids10.factor)=c("0 - There is no change in my usual capacity to concentrate or make decisions.","1 - I occasionally feel indecisive or find that my attention wanders.","2 - Most of the time, I struggle to focus my attention or to make decisions.","3 - I cannot concentrate well enough to read or cannot make even minor decisions.","Missing","N/A")
  levels(data$qids11.factor)=c("0 - I see myself as equally worthwhile and deserving as other people.","1 - I am more self-blaming than usual.","2 - I largely believe that I cause problems for others.","3 - I think almost constantly about major and minor defects in myself.","Missing","N/A")
  levels(data$qids12.factor)=c("0 - I do not think of suicide or death.","1 - I feel that life is empty or wonder if its worth living.","2 - I think of suicide or death several times a week for several minutes.","3 - I think of suicide or death several times a day in some detail, or I have made specific plans for suicide or have actually tried to take my life.","Missing","N/A")
  levels(data$qids13.factor)=c("0 - There is no change from usual in how interested I am in other people or activities.","1 - I notice that I am less interested in people or activities.","2 - I find I have interest in only one or two of my formerly pursued activities.","3 - I have virtually no interest in formerly pursued activities.","Missing","N/A")
  levels(data$qids14.factor)=c("0 - There is no change in my usual level of energy","1 - I get tired more easily than usual.","2 - I have to make a big effort to start or finish my usual daily activities (for example, shopping, homework, cooking, or going to work).","3 - I really cannot carry out most of my usual daily activities because I just dont have the energy.","Missing","N/A")
  levels(data$qids15.factor)=c("0 - I think, speak, and move at my usual rate of speed.","1 - I find that my thinking is slowed down or my voice sounds dull or flat.","2 - It takes me several seconds to respond to most questions and Im sure my thinking is slowed.","3 - I am often unable to respond to questions without extreme effort.","Missing","N/A")
  levels(data$qids16.factor)=c("0 - I do not feel restless.","1 - Im often fidgety, wringing my hands, or need to shift how I am sitting.","2 - I have impulses to move about and am quite restless.","3 - At times, I am unable to stay seated and need to pace around.","Missing","N/A")
  levels(data$qids.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$gds1.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds2.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds3.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds4.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds5.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds6.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds7.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds8.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds9.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds10.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds11.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds12.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds13.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds14.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds15.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds16.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds17.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds18.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds19.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds20.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds21.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds22.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds23.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds24.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds25.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds26.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds27.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds28.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds29.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds30.factor)=c("Yes","No","Missing","N/A")
  levels(data$gds.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$bldast.waking.factor)=c("Yes","No","Missing","N/A")
  levels(data$frail.weightloss.factor)=c("Yes","No","Missing","N/A")
  levels(data$frailt.effort.factor)=c("0 - Rarely or none of the time (< 1 day)","1 - Some or a little of the time (1-2 days)","2 - Moderate amount of the time (3-4 days)","3 - Most of the time","Missing","N/A")
  levels(data$frail.going.factor)=c("0 - Rarely or none of the time (< 1 day)","1 - Some or a little of the time (1-2 days)","2 - Moderate amount of the time (3-4 days)","3 - Most of the time","Missing","N/A")
  levels(data$physical.and.frailty.assessment.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$echo.read.factor)=c("Deepak Gupta, MD","Lisa Mendes, MD","Other")
  levels(data$echo.sonog.factor)=c("JoAnn Gottleib")
  levels(data$echo.nlwm...1.factor)=c("Unchecked","Checked")
  levels(data$echo.nlwm...2.factor)=c("Unchecked","Checked")
  levels(data$echo.nlwm...3.factor)=c("Unchecked","Checked")
  levels(data$echo.nlwm....9999.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...1.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...2.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...3.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...4.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...5.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm...6.factor)=c("Unchecked","Checked")
  levels(data$echo.akinwm....9999.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...1.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...2.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...3.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...4.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...5.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm...6.factor)=c("Unchecked","Checked")
  levels(data$echo.hypowm....9999.factor)=c("Unchecked","Checked")
  levels(data$echo.vd.factor)=c("0 - Normal","1 - Abnormal","Missing")
  levels(data$echo.vd.ts.factor)=c("0 - None","1 - Mild","2 - Mild-Moderate","3 - Moderate","4 - Moderate-Severe","5 - Severe","Missing")
  levels(data$echo.vd.tr.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.mr.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.ms.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.ar.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.as.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.pr.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.vd.ps.factor)=c("0 - None","1 - Trace","2 - Mild","3 - Mild-Moderate","4 - Moderate","5 - Moderate-Severe","6 - Severe","7 - Unable to assess","Missing")
  levels(data$echo.rap.factor)=c("3","8","15","Missing")
  levels(data$echo.dfxn.factor)=c("0 - Normal","1 - Impaired Relaxation","2 - Pseudonormal","3 - Restrictive","4 - Unable to Assess","5 - Indeterminate","Missing")
  levels(data$echo.effus.factor)=c("0 - None","1 - Small","2 - Moderate","3 - Large","Missing")
  levels(data$echo.results.factor)=c("0 - Normal","1 - Abnormal","Missing")
  levels(data$echo.clinicallysig.factor)=c("Yes","No","Missing","N/A")
  levels(data$echo.rhythm.factor)=c("0 - Normal Sinus Rhythm","1 - Atrial Fibrillation","2 - Atrial Flutter","3 - Sinus Bradycardia","4 - Sinus Tachycardia","5 - Other","Missing")
  levels(data$echocardiogram.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$bld.nurse.factor)=c("Ben Small","Christa Hedstrom","Crystal Rice","Deborah King (Ragsdale)","Debra Poplar","Deloris Lee","Diane Anders","Hal Bowman","Lamar Bowman","Lana Howard","Lisa Sposa","Melissa Lehman","Michelle Clark","Robin Perkins","Ray Romano","Samantha Saalwaechter","Sheila Beavers","Sherri Hails","Timothy Smith","Missing")
  levels(data$clinical.blood.work.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$cmr.read.factor)=c("Mark Lawson","Rick Ruberg","Other","Missing")
  levels(data$cmr.quality.factor)=c("Good","Fair","Poor","Missing")
  levels(data$cardiac.mri.complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$np.examiner.factor)=c("Elle Wiggins","Elleena Benson","Laura Fritzsche","Laura Logan","Katie Gifford","Kristi Wisniewski","Liz Lane","Mary Godfrey","Missing")
  levels(data$np.moca.invalid.factor)=c("Yes","No")
  levels(data$np.cvlt.invalid.factor)=c("Yes","No")
  levels(data$np.tower01.factor)=c("0 = Failed","1 = >1 moves","2 = 1 move","Missing")
  levels(data$np.tower02.factor)=c("0 = Failed","1 = >2 moves","2 = 2 moves","Missing")
  levels(data$np.tower03.factor)=c("0 = Failed","1 = >4 moves","2 = 4 moves","3 = 3 moves","Missing")
  levels(data$np.tower04.factor)=c("0 = Failed","1 = >5 moves","2 = 5 moves","3 = 4 moves","Missing","Not Administered")
  levels(data$np.tower05.factor)=c("0 = Failed","1 = >9 moves","2 = 9 moves","3 = 8 moves","4 = 7 moves","Missing","Not Administered")
  levels(data$np.tower06.factor)=c("0 = Failed","1 = >11 moves","2 = 11 moves","3 = 10 moves","4 = 9 moves","Missing","Not Administered")
  levels(data$np.tower07.factor)=c("0 = Failed","1 = >15 moves","2 = 15 moves","3 = 14 moves","4 = 13 moves","Missing","Not Administered")
  levels(data$np.tower08.factor)=c("0 = Failed","1 = > 24 moves","2 = 23-24 moves","3 = 21-22 moves","4 = 20 moves","Missing","Not Administered")
  levels(data$np.tower09.factor)=c("0 = Failed","1 = > 32 moves","2 = 30-32 moves","3 = 27-29 moves","4 = 26 moves","Missing","Not Administered")
  levels(data$np.tower.ruleviol.cumperc.factor)=c("< 1","1","2","3","4","5","7","9","10","11","13","15","16","17","18","20","22","23","24","34","37","48","49","50","56","67","69","74","100","Missing")
  levels(data$np.tower.invalid.factor)=c("Yes","No")
  levels(data$np.digsymb.invalid.factor)=c("Yes","No")
  levels(data$np.color.cumpercerr.factor)=c("100","30","25","18","5","2","1","missing")
  levels(data$np.word.cumpercerr.factor)=c("100","10","2","1","missing")
  levels(data$np.inhibit.cumpercscerr.factor)=c("100","30","28","25","23","20","15","12","10","8","5","7","4","2","1","Missing")
  levels(data$np.inhibit.cumpercucerr.factor)=c("100","65","40","38","22","20","25","15","12","10","8","6","5","3","2","1","Missing")
  levels(data$np.cw.invalid.factor)=c("Yes","No")
  levels(data$np.hvot.invalid.factor)=c("Yes","No")
  levels(data$np.fas.invalid.factor)=c("Yes","No")
  levels(data$np.anim.invalid.factor)=c("Yes","No")
  levels(data$np.tmta.cumperc.seqerr.factor)=c("100","9","7","5","3","1","Missing")
  levels(data$np.tmta.cumperc.seterr.factor)=c("100","2","1","< 1","Missing")
  levels(data$np.tmtb.cumperc.seqerr.factor)=c("100","52","42","40","28","20","19","17","9","8","7","6","5","4","3","2","1","Missing")
  levels(data$np.tmtb.cumperc.seterr.factor)=c("100","50","34","30","20","14","12","7","6","5","4","3","2","1","Missing")
  levels(data$np.tmta.invalid.factor)=c("Yes","No")
  levels(data$np.tmtb.invalid.factor)=c("Yes","No")
  levels(data$np.bnt.invalid.factor)=c("Yes","No")
  levels(data$np.biber.invalid.factor)=c("Yes","No")
  levels(data$neuropsychological.assessment.complete.factor)=c("Incomplete","Unverified","Complete")

  # 25 Feb 2015:  Something has changed with the creation of
  # variable labels for factor variables, either in the
  # REDCap R code generation or somewhere in R.
  # We want to add variable labels to the factor vars that were
  # created by the REDCap-generated R script.
  for (vname in names(data)) {
    if ((!(vname %in% orignames)) & grepl("\\.factor\\>", vname)) {
      nonfactorName <- sub("\\.factor\\>", "", vname)
      label(data[, vname]) <- label(data[, nonfactorName])
    }
  }

  # 20180124 OAK: Releveling diagnosis.factor / enrolled.dx.factor as per AJ

  data$diagnosis.factor <- factor(
    data$diagnosis.factor,
    levels = c("Normal", "Ambiguous At Risk", "MCI", "Dementia", "Missing")
  )

  data
}
