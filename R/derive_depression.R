#' Derive, label, and add depression variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added depression variables.
#' @export

derive_depression <- function(data) {
  # 16 Jan 2015: re-calculating gds total score
  # 02 Mar 2015:  AJ and KG confirmed in meeting that they
  # want to use the 85% rule for this one
  gdsvars <- paste0("gds", 1:30)
  data$gds <- apply(data[, gdsvars], 1, total_score)

  #QIDS
  qids.sub <- paste0("qids", formatC(1:16, width = 2, format = "d", flag = "0"))
  my.pmax <- function(x) {
    apply(x, MARGIN = 1, FUN = function(y) ifelse(sum(!is.na(y)) == 0, NA, max(y, na.rm = TRUE)))
  }

  data$qids <- rowSums(
    cbind(
      my.pmax(data[, qids.sub[1:4]]),
      my.pmax(data[, qids.sub[6:9]]),
      my.pmax(data[, qids.sub[15:16]]),
      data[, qids.sub[c(5, 10:14)]]
    ),
    na.rm = TRUE
  )

  data <- within(data, {
    # 09 Jan 2015: modified gds variable for KG
    gds.minus.gds14 <- gds - gds14
    label(gds.minus.gds14) <- "GDS tot score minus gds14 (memory prob.)"

    # 19 Oct 2015: modified gds variable for KG
    # Email from Laurie 08 Oct 2015
    gds.minus.cog <- gds - (gds14 + gds26 + gds29 + gds30)
    label(gds.minus.cog) <- "GDS tot score minus cog. items (14, 26, 29, 30)"

    # Dichotomizations
    depress.gds         <- ifelse(is.na(gds), NA,
                                  ifelse(gds >= 10, 1, 0))
    depress.gds.factor  <- factor(depress.gds,
                                  levels = c(1, 0), labels = c("Yes", "No"))
    depress.qids        <- ifelse(is.na(qids), NA,
                                  ifelse(qids >= 6, 1, 0))
    depress.qids.factor <- factor(depress.qids,
                                  levels = c(1, 0), labels = c("Yes", "No"))

    label(gds) <- "GDS total score, recalculated"
    label(qids) <- "QIDS total score, recalculated"
    label(depress.gds) <- "Depression - GDS Definition"
    label(depress.gds.factor) <- "Depression - GDS Definition"
    label(depress.qids) <- "Depression - QIDS Definition"
    label(depress.qids.factor) <- "Depression - QIDS Definition"
  })

  return(data)
}
