#' A preprocessing function to replace values with NA.
#'
#' @param vec A numeric, integer, or character vector.
#' @param equal.val A vector of length 1 or greater containing any combination of numbers, characters, or strings that match the target values exactly.
#' @param mod.val A single number that divides into the target numeric or integer values with zero remainder.
#' @param restrict.sign A logical, to be paired with \code{mod.val}, indicating whether the numeric or integer value acted upon should have the same sign as \code{mod.val}.
#' @return The input vector with all matching values set to missing.
#' @examples
#' test.df <- data_frame(a = 1:5, b = -2:-6, c = rnorm(5), d = LETTERS[1:5])
#'
#' missing_to_na(test.df$b, mod.val = 3)
#'
#' test.df %>% mutate_all(
#'   ~ missing_to_na(., equal.val = 2)
#' )
#'
#' test.df %>% mutate_if(
#'  ~ any(class(.) %in% c("numeric", "integer", "character")),
#'  ~ missing_to_na(., equal.val = c(2, 3))
#' )
#'
#' test.df %>% mutate_at(
#'   c("a", "b"),
#'   ~ missing_to_na(., equal.val = c(2, 3, "E"), mod.val = -2, restrict.sign = TRUE)
#' )
#'
#' \dontrun{
#' epoch1.main.df %>% mutate_at(
#'   grep("^ecogself\\_(mem|lang|vis|plan|org|attn)\\d{2}$", names(.)),
#'    ~ missing_to_na(., equal.val = 0)
#' )
#' }
#' @export

missing_to_na <- function(vec, equal.val, mod.val, restrict.sign = TRUE) {
  # output error if key arguments missing
  if (missing(vec)) {
    stop("Please specify a vector of data.")
  }
  if (missing(mod.val) & missing(equal.val)) {
    stop("Please specify at least one argument to match.")
  }
  if (missing(mod.val) & !missing(equal.val)) { # equal.val only
    if (any(class(vec) == "numeric")) {
      case_when(
        vec %in% equal.val ~ NA_real_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "integer")) {
      case_when(
        vec %in% equal.val ~ NA_integer_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "character")) {
      case_when(
        as.character(vec) %in% equal.val ~ NA_character_,
        TRUE ~ vec
      )
    } else {
      warning("Vector must be of class numeric, integer, or character.")
    }
  } else if (!missing(mod.val) & missing(equal.val) & restrict.sign == TRUE) { # mod.val only, restrict sign
    if (any(class(vec) == "numeric")) {
      case_when(
        sign(mod.val) == sign(vec) & vec %% mod.val == 0 ~ NA_real_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "integer")) {
      case_when(
        sign(mod.val) == sign(vec) & vec %% mod.val == 0 ~ NA_integer_,
        TRUE ~ vec
      )
    } else {
      # OAK 20180319 : cannot do division on character class
      warning("Vector must be of class numeric or integer.")
    }
  } else if (!missing(mod.val) & missing(equal.val) & restrict.sign == FALSE) { # mod.val only, any sign
    if (any(class(vec) == "numeric")) {
      case_when(
        vec %% mod.val == 0 ~ NA_real_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "integer")) {
      case_when(
        vec %% mod.val == 0 ~ NA_integer_,
        TRUE ~ vec
      )
    } else {
      # OAK 20180319 : cannot do division on character class
      warning("Vector must be of class numeric or integer.")
    }
  } else { # both equal.val and mod.val
    if (any(class(vec) == "numeric") & restrict.sign == TRUE) {
      case_when(
        sign(mod.val) == sign(vec) & vec %% mod.val == 0 ~ NA_real_,
        vec %in% equal.val ~ NA_real_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "numeric") & restrict.sign == FALSE) {
      case_when(
        vec %% mod.val == 0 ~ NA_real_,
        vec %in% equal.val ~ NA_real_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "integer") & restrict.sign == TRUE) {
      case_when(
        sign(mod.val) == sign(vec) & vec %% mod.val == 0 ~ NA_integer_,
        vec %in% equal.val ~ NA_integer_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "integer") & restrict.sign == FALSE) {
      case_when(
        vec %% mod.val == 0 ~ NA_integer_,
        vec %in% equal.val ~ NA_integer_,
        TRUE ~ vec
      )
    } else if (any(class(vec) == "character")) {
      case_when(
        # cannot do division on character class
        vec %in% equal.val ~ NA_character_,
        TRUE ~ vec
      )
    } else {
      warning("Vector must be of class numeric, integer, or character.")
    }
  }
}
