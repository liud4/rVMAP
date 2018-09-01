#' Derive, label, and add family history variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added family history variables.
#' @export

derive_family_hx <- function(data) {
  fam.hxprob <- paste0("famhx",
                       formatC(1:12, width = 2, format = "d", flag = "0"), ".prob")
  fam.hxdx <- paste0("famhx",
                     formatC(1:12, width = 2, format = "d", flag = "0"), ".dx")

  # Family History of AD
  data$familyhxad <-
    ifelse(data$famhx01 == 1 &
             apply(data[, fam.hxprob] == 1, MARGIN = 1, FUN = any) &
             apply(data[, fam.hxdx] == 1, MARGIN = 1, FUN = any), 1, 0)
  data$familyhxad[is.na(data$familyhxad)] <- 0
  data$familyhxad[is.na(data$famhx01)] <- NA

  # Family History of Dementia
  data$familyhxdementia <-
    ifelse(data$famhx01 == 1 &
             apply(data[, fam.hxprob] == 1, MARGIN = 1, FUN = any), 1, 0)
  data$familyhxdementia[is.na(data$familyhxdementia)] <- 0
  data$familyhxdementia[is.na(data$famhx01)] <- NA

  # Family History of Memory Loss
  data$familyhxmemory <-
    ifelse(data$famhx01 == 1 &
             apply(data[, fam.hxprob] == 2, MARGIN = 1, FUN = any), 1, 0)
  data$familyhxmemory[is.na(data$familyhxmemory)] <- 0
  data$familyhxmemory[is.na(data$famhx01)] <- NA

  # Number of Family Members with History of AD
  data$familyhxad.number <- ifelse(data[, fam.hxdx] == 1, 1, 0)
  data$familyhxad.number[is.na(data$familyhxad.number)] <- 0
  data$familyhxad.number <- rowSums(data$familyhxad.number)
  data$familyhxad.number[is.na(data$famhx01)] <- NA

  # Number of Family Members with History of Dementia
  data$familyhxdementia.number <- ifelse(data[, fam.hxprob] == 1, 1, 0)
  data$familyhxdementia.number[is.na(data$familyhxdementia.number)] <- 0
  data$familyhxdementia.number <- rowSums(data$familyhxdementia.number)
  data$familyhxdementia.number[is.na(data$famhx01)] <- NA

  # Number of Family Members with History of Memory Problems
  data$familyhxmemory.number <- ifelse(data[, fam.hxprob] == 2, 1, 0)
  data$familyhxmemory.number[is.na(data$familyhxmemory.number)] <- 0
  data$familyhxmemory.number <- rowSums(data$familyhxmemory.number)
  data$familyhxmemory.number[is.na(data$famhx01)] <- NA

  data <- within(data, {
    familyhxad.factor <- factor(familyhxad,
                                levels = c(1, 0), labels = c("Yes", "No"))
    familyhxdementia.factor <- factor(familyhxdementia,
                                      levels = c(1, 0), labels = c("Yes", "No"))
    familyhxmemory.factor <- factor(data$familyhxmemory,
                                    levels = c(1, 0), labels = c("Yes", "No"))

    label(familyhxad) <- "Family Hx of AD"
    label(familyhxad.factor) <- "Family Hx of AD"
    label(familyhxdementia) <- "Family Hx of Dementia"
    label(familyhxdementia.factor) <- "Family Hx of Dementia"
    label(familyhxmemory) <- "Family Hx of Memory Loss"
    label(familyhxmemory.factor) <- "Family Hx of Memory Loss"
    label(familyhxad.number) <- "Num. Family Members w/ Diagnosis of AD"
    label(familyhxdementia.number) <- "Num. Family Members w/ Dementia"
    label(familyhxmemory.number) <- "Num. Family Members w/ Memory Problems"
  })

  return(data)
}
