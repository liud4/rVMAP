#' Use in statistical reports to print MAP IDs excluded from a specific exclusion step.
#'
#' @param step An integer specifying the exclusion step number (stored in a list called \code{id.excl}).
#' @return A sentence stating that no participants were excluded in the specified step or a sentence with a comma separated list of excluded participants.
#' @export
#'
#' @examples
#' \dontrun{
#' excl_map_id(step = 2)
#' }

print_excluded_participants <- function(step) {
  if (all.equal(clear.labels(excl$id.excl[[step]]), character(0)) == TRUE) {
    output_text <- paste0("No participants were excluded in this step.")
  } else {
    output_text <- paste0('The following participants were excluded: ', paste0(clear.labels(excl$id.excl[[step]]), collapse = ", "), ".")
  }
  return(output_text)
}
