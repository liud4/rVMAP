excl_map_id <- function(x) {
  if (all.equal(clear.labels(excl$id.excl[[x]]), character(0)) == TRUE) {
    out <- paste0("No participants were excluded in this step.")
    out
  } else {
    out <- paste0('The following participants were excluded: ', paste0(clear.labels(excl$id.excl[[x]]), collapse = ", "), ".")
    out
  }
}
