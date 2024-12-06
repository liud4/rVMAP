#' Derive, label, and add demographic variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added demographic variables.
#' @export

derive_demographics <- function(data) {

  days_in_one_year <- 365.25
  
  data <- within(data, {
    dob <- dob.questionnaires
    label(dob) <- "Date of Birth"
    
    raceethnicity <- ifelse(is.na(race) | is.na(ethnicity),
                            NA,
                            ifelse(race == 1 & ethnicity == 0,
                                   1,
                                   ifelse(
                                     (race %in% c(0, 2, 3, 4) &
                                        ethnicity %in% c(0, 1)) |
                                       (race == 1 & ethnicity == 1),
                                     2,
                                     NA
                                   )))
    
    label(raceethnicity) <- "Two-level race/ethnicity"
    
    raceethnicity.factor <- factor(
      raceethnicity,
      levels = 1:2,
      labels = c("Non-Hispanic White", "Other")
    )
    
    label(raceethnicity.factor) <- "Two-level race/ethnicity"
    
    age.redcap <- age
    
    age <-
      ifelse(
        is.na(vf.arrival.date.time) | (map.id %in% c(1L:336L) & epoch == 1),
        floor(as.numeric(difftime(
          medhx.date, dob.questionnaires, units = "days"
        )) / days_in_one_year),
        floor(as.numeric(
          difftime(vf.arrival.date.time, dob.questionnaires, units = "days")
        ) / days_in_one_year)
      )
    
    label(age) <-
      "Age at the visit"
  })
  
  return(data)
}
