#' Format a vector of p-values to be colored red if less than the specified alpha (default = 0.05).
#'
#' @param vector A vector containing p-values.
#' @param alpha A number that specified significance.
#' @return ???
#' @export

# OAK 20180814: This function may need to be written for a data frame, not a vector.

format_p <- function(vector, alpha = 0.05) {
  vector <- as.numeric(vector)
  formattable::formattable(
    vector,
    preproc = formattable::formatter("span", style = vector ~ ifelse(vector < alpha, "color:red", NA)),
    postproc = function(str, x)
      ifelse(
        x < 1e-6,
        format(x, scientific = TRUE, digits = 2),
        format(x, scientific = FALSE, digits = 2, nsmall = 6)
      )
  )
}
