#' Apply exclusion criteria to a data set, ideally as part of an inclusion/exclusion section of the code.
#'
#' @param diagnosis A character vector containing one or more of the following values: \code{"Dementia"}, \code{"Normal"}, \code{"Ambiguous At Risk"}, and/or \code{"MCI"}. Each diagnostic group will be removed in its own step. A value of NULL for this argument will skip this step.
#' @param predictors A character vector containing predictor variable names. Observations with all predictors missing will be removed in one step. A value of NULL for this argument will skip this step.
#' @param outcomes A character vector containing outcome variable names. Observations with all outcomes missing will be removed in one step. A value of NULL for this argument will skip this step.
#' @param covariates A character vector containing covariate variable names. For each covariate, observations with a missing value will be removed in its own step. A value of NULL for this argument will skip this step.
#' @param data A data frame to which the exclusion criteria should be applied.
#' @param data_name Optional. By default, this will be set to the name of the object supplied in \code{data}. Supplying a value will overwrite this default behavior.
#' @param data_type An atomic character string of either \code{"b"} for baseline/cross-sectional data or \code{"l"} for longitudinal data.
#' @return \code{data} with exclusion criteria applied.
#' @examples
#' exclude_participants(diagnosis = c("Dementia", "Ambiguous At Risk"), predictors = predictors.var, outocmes = outcomes.var, covariates = covariates.var, data = mydat0.long, data_type = "l")
#' @export

exclude_participants <- function(
  diagnosis, # c("Dementia", "Ambiguous At Risk")
  predictors, # predictors.var
  outcomes, # outcomes.var
  covariates, # covariates.var
  data,
  data_name = NULL,
  data_type # "b" for baseline or "l" for longitudinal
) {
  data.df <- data
  data_name <- ifelse(is.null(data_name), deparse(substitute(data)), data_name)

  # First Step
  generate_inclusion_exclusion(step = "first", data = data.df, data_name = data_name, data_type = data_type)

  # Diagnosis
  if (!is.null(diagnosis)) {
    for (diagnosis.i in diagnosis) {
      data.df <- data.df[data.df$diagnosis.factor.base %nin% diagnosis.i, ]
      generate_inclusion_exclusion(step = "diagnosis", diagnosis = diagnosis.i, data = data.df, data_name = data_name, data_type = data_type)
    }
  }

  # Predictor
  if (!is.null(predictors)) {
  # for (predictor.i in predictor) {
    data.df <- data.df[!apply(as.matrix(data.df[, predictors]), 1, function(x) all(is.na(x))), ]
    generate_inclusion_exclusion(step = "predictors", vars = predictors, data = data.df, data_name = data_name, data_type = data_type)
  }

  # Outcome
  if (!is.null(outcomes)) {
  # for (outcome.i in outcome) {
    data.df <- data.df[!apply(as.matrix(data.df[, outcomes]), 1, function(x) all(is.na(x))), ]
    generate_inclusion_exclusion(step = "outcomes", vars = outcomes, data = data.df, data_name = data_name, data_type = data_type)
  }

  # Covariates
  if (!is.null(covariates)) {
    for (covariate.i in covariates) {
      data.df <- data.df[!apply(as.matrix(data.df[, covariate.i]), 1, function(x) is.na(x)), ]
      generate_inclusion_exclusion(step = "covariates", vars = covariate.i, data = data.df, data_name = data_name, data_type = data_type)
    }
  }

  return(data.df)
}

#' Generate a data frame that summarizes the inclusion/exclusion criteria. This function is not intended to be explicitly called by the user.
#'
#' @param step An atomic character string from the following values: \code{"first"}, \code{"diagnosis"}, \code{"predictor"}, \code{"outcome"}, \code{"covariate"}, \code{"final"}.
#' @param diagnosis Optional. Supply only if the argument step is set to "diagnosis." An atomic character string containing one of the following values: \code{"Dementia"}, \code{"Normal"}, \code{"Ambiguous At Risk"}, \code{"MCI"}.
#' @param vars Optional. Supply only if the argument step is set to "predictor," "outcome," or "covariate." A character vector containing the variable or variables to check for missing values.
#' @param data A data frame to which the exclusion criteria is applied.
#' @param data_name The name of the data object to print in the output.
#' @param data_type An atomic character string of either \code{"b"} for baseline/cross-sectional data or \code{"l"} for longitudinal data.
#' @return A data frame named \code{flow_chart.df} containing a summary of the exclusion process.

generate_inclusion_exclusion <- function(
  step, #first, diagnosis, predictor, outcome, covariate, final
  diagnosis = NULL, # only for step = diagnosis, eg: "Dementia"
  vars = NULL, # only for step = predictor, outcome --> this will be used to generate the descriptive text
  data,
  data_name,
  data_type # "b" for baseline or "l" for longitudinal
) {

  data.df <- data
  obj <- data_name

  if (data_type == "b") {
    new_data_type <- "Project Specific Baseline"
  } else if (data_type == "l") {
    new_data_type <- "Project Specific Longitudinal"
  } else {
    stop("Argument `data_type` must be either 'b' for baseline or 'l' for longitudinal.\n")
  }

  if (step == "first") {

    if (exists(flow_chart.df)) {
      stop("An object by the name of 'flow_chart.df' exists. Please remove this object from the global environment and retry.\n")
    }

    if ("epoch" %in% names(data.df)) {
      epochs <- unique(data.df$epoch)
    } else {
      epochs <- 1
      warning("The dataset specified does not have an `epoch` column. The number of epochs is assumed to be 1. Please check that this is the intended result.\n")
    }

    column_names <- c(
      "obj",
      "data_type",
      "description",
      'n_id',
      "n_obs",
      paste0("n_e", epochs),
      paste0("e", epochs, "_id_included"),
      paste0("e", epochs, "_id_excluded")
    )

    flow_chart.df <<- setNames(
      data.frame(matrix(ncol = length(column_names), nrow = 0), stringsAsFactors = FALSE),
      column_names
    )

    new_description <- ifelse(
      all(epochs == 1),
      "We begin with all participants and observations in Epoch 1.",
      paste0(
        "We begin with all participants and observations in Epochs ",
        paste0(epochs[1:length(epochs) - 1], collapse = ", "),
        ", and ",
        epochs[length(epochs)],
        "."
      )
    )

    new_n_id <- length(unique(data.df[["map.id"]]))

    new_n_obs <- nrow(data.df)

    new_row_ne <- purrr::map_int(epochs, function(x) nrow(data.df[data.df$epoch == x, ]))

    new_row_id_included <- purrr::map_chr(epochs, function(x) paste0(data.df[data.df$epoch == x, "map.id"], collapse = ", "))

    new_row_id_excluded <- rep("", length(epochs))

    new_row <- setNames(
      c(obj, new_data_type, new_description, new_n_id, new_n_obs, new_row_ne, new_row_id_included, new_row_id_excluded),
      column_names
    )

    if (length(new_row) != length(column_names)) { # check
      stop("The number of elements in the new row does not match the number of columns in flow_chart.mat.\n")
    }

    flow_chart.df <<- dplyr::bind_rows(
      flow_chart.df,
      new_row
    )
  }

  else if (step == "diagnosis") {

    epochs <- as.numeric(gsub("e", "", gsub("_id_included", "", grep("_id_included", names(flow_chart.df), v = T))))

    column_names <- c(
      "obj",
      "data_type",
      "description",
      'n_id',
      "n_obs",
      paste0("n_e", epochs),
      paste0("e", epochs, "_id_included"),
      paste0("e", epochs, "_id_excluded")
    )

    new_description <- paste0("Exclude participants with a baseline diagnosis of ", diagnosis, ".")

    new_n_id <- length(unique(data.df[["map.id"]]))

    new_n_obs <- nrow(data.df)

    new_row_ne <- purrr::map_int(epochs, function(x) nrow(data.df[data.df$epoch == x, ]))

    new_row_id_included <- purrr::map_chr(epochs, function(x) paste0(data.df[data.df$epoch == x, "map.id"], collapse = ", "))

    new_row_id_excluded <- purrr::map_chr(
      epochs,
      function(epoch.i) {
        column_index <- 5 + # "obj", "data_type", "description", 'n_id', "n_obs"
          length(epochs) + # paste0("n_e", epochs)
          epoch.i

        prev_id_included <- strsplit(flow_chart.df[nrow(flow_chart.df), column_index], ", ")[[1]]
        current_id_included <- data.df[data.df$epoch == epoch.i, "map.id"]

        current_id_excluded <- prev_id_included[prev_id_included %nin% current_id_included]

        return(
          paste0(current_id_excluded, collapse = ", ")
        )
      }
    )

    new_row <- setNames(
      c(obj, new_data_type, new_description, new_n_id, new_n_obs, new_row_ne, new_row_id_included, new_row_id_excluded),
      column_names
    )

    if (length(new_row) != length(column_names)) { # check
      stop("The number of elements in the new row does not match the number of columns in flow_chart.mat.\n")
    }

    flow_chart.df <<- dplyr::bind_rows(
      flow_chart.df,
      new_row
    )
  }

  else if (step == "predictors") {

    epochs <- as.numeric(gsub("e", "", gsub("_id_included", "", grep("_id_included", names(flow_chart.df), v = T))))

    column_names <- c(
      "obj",
      "data_type",
      "description",
      'n_id',
      "n_obs",
      paste0("n_e", epochs),
      paste0("e", epochs, "_id_included"),
      paste0("e", epochs, "_id_excluded")
    )

    if (is.null(vars)) stop("For this step, the argument `vars` must be specified.\n")

    new_description <- paste0("Exclude observations with all of the following predictors missing: ", paste0(vars, collapse = ", "), ".")

    new_n_id <- length(unique(data.df[["map.id"]]))

    new_n_obs <- nrow(data.df)

    new_row_ne <- purrr::map_int(epochs, function(x) nrow(data.df[data.df$epoch == x, ]))

    new_row_id_included <- purrr::map_chr(epochs, function(x) paste0(data.df[data.df$epoch == x, "map.id"], collapse = ", "))

    new_row_id_excluded <- purrr::map_chr(
      epochs,
      function(epoch.i) {
        column_index <- 5 + # "obj", "data_type", "description", 'n_id', "n_obs"
          length(epochs) + # paste0("n_e", epochs)
          epoch.i

        prev_id_included <- strsplit(flow_chart.df[nrow(flow_chart.df), column_index], ", ")[[1]]
        current_id_included <- data.df[data.df$epoch == epoch.i, "map.id"]

        current_id_excluded <- prev_id_included[prev_id_included %nin% current_id_included]

        return(
          paste0(current_id_excluded, collapse = ", ")
        )
      }
    )

    new_row <- setNames(
      c(obj, new_data_type, new_description, new_n_id, new_n_obs, new_row_ne, new_row_id_included, new_row_id_excluded),
      column_names
    )

    if (length(new_row) != length(column_names)) { # check
      stop("The number of elements in the new row does not match the number of columns in flow_chart.mat.\n")
    }

    flow_chart.df <<- dplyr::bind_rows(
      flow_chart.df,
      new_row
    )
  }

  else if (step == "outcomes") {

    epochs <- as.numeric(gsub("e", "", gsub("_id_included", "", grep("_id_included", names(flow_chart.df), v = T))))

    column_names <- c(
      "obj",
      "data_type",
      "description",
      'n_id',
      "n_obs",
      paste0("n_e", epochs),
      paste0("e", epochs, "_id_included"),
      paste0("e", epochs, "_id_excluded")
    )

    if (is.null(vars)) stop("For this step, the argument `vars` must be specified.\n")

    new_description <- paste0("Exclude observations with all of the following outcomes missing: ", paste0(vars, collapse = ", "), ".")

    new_n_id <- length(unique(data.df[["map.id"]]))

    new_n_obs <- nrow(data.df)

    new_row_ne <- purrr::map_int(epochs, function(x) nrow(data.df[data.df$epoch == x, ]))

    new_row_id_included <- purrr::map_chr(epochs, function(x) paste0(data.df[data.df$epoch == x, "map.id"], collapse = ", "))

    new_row_id_excluded <- purrr::map_chr(
      epochs,
      function(epoch.i) {
        column_index <- 5 + # "obj", "data_type", "description", 'n_id', "n_obs"
          length(epochs) + # paste0("n_e", epochs)
          epoch.i

        prev_id_included <- strsplit(flow_chart.df[nrow(flow_chart.df), column_index], ", ")[[1]]
        current_id_included <- data.df[data.df$epoch == epoch.i, "map.id"]

        current_id_excluded <- prev_id_included[prev_id_included %nin% current_id_included]

        return(
          paste0(current_id_excluded, collapse = ", ")
        )
      }
    )

    new_row <- setNames(
      c(obj, new_data_type, new_description, new_n_id, new_n_obs, new_row_ne, new_row_id_included, new_row_id_excluded),
      column_names
    )

    if (length(new_row) != length(column_names)) { # check
      stop("The number of elements in the new row does not match the number of columns in flow_chart.mat.\n")
    }

    flow_chart.df <<- dplyr::bind_rows(
      flow_chart.df,
      new_row
    )
  }

  else if (step == "covariates") {

    epochs <- as.numeric(gsub("e", "", gsub("_id_included", "", grep("_id_included", names(flow_chart.df), v = T))))

    column_names <- c(
      "obj",
      "data_type",
      "description",
      'n_id',
      "n_obs",
      paste0("n_e", epochs),
      paste0("e", epochs, "_id_included"),
      paste0("e", epochs, "_id_excluded")
    )

    if (is.null(vars)) stop("For this step, the argument `vars` must be specified.\n")

    new_description <- paste0("Exclude observations with the following covariate missing: ", paste0(vars, collapse = ", "), ".")

    new_n_id <- length(unique(data.df[["map.id"]]))

    new_n_obs <- nrow(data.df)

    new_row_ne <- purrr::map_int(epochs, function(x) nrow(data.df[data.df$epoch == x, ]))

    new_row_id_included <- purrr::map_chr(epochs, function(x) paste0(data.df[data.df$epoch == x, "map.id"], collapse = ", "))

    new_row_id_excluded <- purrr::map_chr(
      epochs,
      function(epoch.i) {
        column_index <- 5 + # "obj", "data_type", "description", 'n_id', "n_obs"
          length(epochs) + # paste0("n_e", epochs)
          epoch.i

        prev_id_included <- strsplit(flow_chart.df[nrow(flow_chart.df), column_index], ", ")[[1]]
        current_id_included <- data.df[data.df$epoch == epoch.i, "map.id"]

        current_id_excluded <- prev_id_included[prev_id_included %nin% current_id_included]

        return(
          paste0(current_id_excluded, collapse = ", ")
        )
      }
    )

    new_row <- setNames(
      c(obj, new_data_type, new_description, new_n_id, new_n_obs, new_row_ne, new_row_id_included, new_row_id_excluded),
      column_names
    )

    if (length(new_row) != length(column_names)) { # check
      stop("The number of elements in the new row does not match the number of columns in flow_chart.mat.\n")
    }

    flow_chart.df <<- dplyr::bind_rows(
      flow_chart.df,
      new_row
    )
  }
}
