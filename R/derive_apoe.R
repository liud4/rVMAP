#' Derive, label, and add ApoE variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added ApoE variables.
#' @export

derive_apoe <- function(data) {
  CountAlleles <- function(string, allele) {
    ifelse(
      is.na(string),
      NA,
      ifelse(
        string %in% c("Undetermined"),
        NA,
        sum(grepl(allele, strsplit(string, "/", fixed = TRUE)[[1]], fixed = TRUE))
      )
    )
  }

  data <- within(data, {
    # 02 Feb 2015:  The other APOE coding schemes from Tim's 18 Nov 2014 email
    # note misspelling of alleles in varname:
    apoe4count <- unlist(lapply(allelles, CountAlleles, "E4"))
    label(apoe4count) <- 'Count of ApoE E4 alleles'

    apoe2count <- unlist(lapply(allelles, CountAlleles, "E2"))
    label(apoe2count) <- 'Count of ApoE E2 alleles'

    # added 19 Jan 2015, modified 02 Feb 2015
    apoe4pos <- ifelse(is.na(apoe4count), NA, as.numeric(apoe4count > 0))
    label(apoe4pos) <- 'ApoE4+ (at least one E4 allele)'

    apoe4pos.factor <- factor(
      apoe4pos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(apoe4pos.factor) <- 'ApoE4+ (at least one E4 allele)'

    # and 02 Feb 2015:  The other schemes from Tim's 18 Nov 2014 email, cont'd
    apoe2pos <- ifelse(is.na(apoe2count), NA, as.numeric(apoe2count > 0))
    label(apoe2pos) <- 'ApoE2+ (at least one E2 allele)'

    apoe2pos.factor <- factor(
      apoe2pos,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
    label(apoe2pos.factor) <- 'ApoE2+ (at least one E2 allele)'
  })

  return(data)
}
