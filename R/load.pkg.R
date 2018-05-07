#' A general-use function for loading packages, installing them first if they
#' are not installed.
#'
#' @param pkg A character vector containing the names of CRAN packages.
#' @export

load.pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
