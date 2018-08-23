#' Derive, label, and add demographic variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added demographic variables.
#' @export

derive_demographics <- function(data) {
  days_in_one_year <- 365.25

  data <- within(data, {
    raceethnicity <- ifelse(
      is.na(race) | is.na(ethnicity),
      NA,
      ifelse(
        race == 1 & ethnicity == 0,
        1,
        ifelse(
          (race %in% c(0,2,3,4) & ethnicity %in% c(0,1)) | (race ==1 & ethnicity ==1),
          2,
          NA
        )
      )
    )

    raceethnicity.factor <- factor(
      raceethnicity,
      levels = 1:2,
      labels = c("Non-Hispanic White", "Other")
    )

    label(raceethnicity) <- label(raceethnicity.factor) <-"Two-level race/ethnicity"

    age.redcap <- age

    age <- floor(
      as.numeric(difftime(medhx.date, dob, units= "days")) / days_in_one_year
    )

    label(age) <- "Age at medhx.date, recalculated"
  })

  return(data)
}
