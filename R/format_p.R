#' Format a vector of p-values to be colored red if less than the specified alpha.
#'
#' @param vec A numeric vector containing p-values.
#' @param alpha A numeric significance value.
#' @return A \code{formattable} object containing the formatted vector of p-values.
#' @export
#'
#' @examples
#' \dontrun{
#' formattable::formattable(summary.all.df, list(`p-Value` = formatP, `Total p-Value` = formatP))
#' }

format_p <- function(vec, alpha = 0.05) {
  vec <- as.numeric(vec)
  formattable::formattable(
    vec,
    preproc = formattable::formatter("span", style = vec ~ ifelse(vec < alpha, "color:red", NA)),
    postproc = function(str, x)
      ifelse(
        x < 1e-6,
        format(x, scientific = TRUE, digits = 2),
        format(x, scientific = FALSE, digits = 2, nsmall = 6)
      )
  )
}
