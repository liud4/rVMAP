red_text <- formatter("span", style = x ~ ifelse(x < 0.05, "color:red", NA))

format_p <- function(vector){
  vector <- as.numeric(vector)
  formattable(
    vector,
    preproc = red_text,
    postproc = function(str, x)
      ifelse(x < 1e-6,
             format(x, scientific = TRUE, digits = 2),
             format(x, scientific = FALSE, digits = 2, nsmall = 6)
      )
  )
}

# DEPRECATED
# format_p <- function(x, dig = 2, nsm = 6, eps = 1/10^nsm) {
#   pvals <- rep(NA, length(x))
#   sciInd <- which(x < eps)
#   if (length(sciInd)) {
#     pvals[sciInd] <- format(x[sciInd], scientific = TRUE, digits = 2)
#     pvals[-sciInd] <- format(x[-sciInd], digits = dig, nsmall = nsm)
#   } else pvals <- format(x, digits = dig, nsmall = nsm)
#   pvals
# }
