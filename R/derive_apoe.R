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

  # 02 Feb 2015:  The other APOE coding schemes from Tim's 18 Nov 2014 email
  # note misspelling of alleles in varname:
  data$apoe4count <- unlist(lapply(data$alleles, CountAlleles, "E4"))
  label(data$apoe4count) <- 'Count of ApoE E4 alleles'

  data$apoe2count <- unlist(lapply(data$alleles, CountAlleles, "E2"))
  label(data$apoe2count) <- 'Count of ApoE E2 alleles'

  # added 19 Jan 2015, modified 02 Feb 2015
  data$apoe4pos <- ifelse(is.na(data$apoe4count), NA, as.numeric(data$apoe4count > 0))
  label(data$apoe4pos) <- 'ApoE4+ (at least one E4 allele)'

  data$apoe4pos.factor <- factor(
    data$apoe4pos,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
  label(data$apoe4pos.factor) <- 'ApoE4+ (at least one E4 allele)'

  # and 02 Feb 2015:  The other schemes from Tim's 18 Nov 2014 email, cont'd
  data$apoe2pos <- ifelse(is.na(data$apoe2count), NA, as.numeric(data$apoe2count > 0))
  label(data$apoe2pos) <- 'ApoE2+ (at least one E2 allele)'

  data$apoe2pos.factor <- factor(
    data$apoe2pos,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
  label(data$apoe2pos.factor) <- 'ApoE2+ (at least one E2 allele)'

  # alleles.factor code added 20190926:
  data$alleles.factor <- dplyr::case_when(
    clear_labels(data$alleles) %in% c('E2/E2', 'E2/E3') ~ 'E2/E2, E2/E3',
    clear_labels(data$alleles) %in% c('E3/E4', 'E4/E4') ~ 'E3/E4, E4/E4',
    TRUE ~ clear_labels(data$alleles)
  )

  data$alleles.factor <- factor(data$alleles.factor)

  data$alleles.factor <- relevel(data$alleles.factor, ref = 'E3/E3')
  label(data$alleles.factor) <- label("Genotype - Alleles")

  return(data)
}
