#' Simulate Counterfactual Predictions
#'
#' @param model A model_list object. The data the model was trained on must have
#'   been prepared, either by training with \code{\link{machine_learn}} or by
#'   preparing with \code{\link{prep_data}}
#' @param vary Which variables to vary? Default is 4; if it is a single integer
#'   (n), the n most important variables are varied (see details). If it is a
#'   vector of integers, those rankings of variables are used (e.g. \code{vary =
#'   2:4} varies the 2nd, 3rd, and 4th most-important variables). Alternatively,
#'   a vector of variable names to vary may be provided. Or, for the finest
#'   level of control, a list with names being variable names and entries being
#'   values to use may be provided; in this case \code{numerics} and
#'   \code{characters} are ignored.
#' @param hold How to choose the values of variables not being varied? If this
#'   is an integer, the values from that row in the training dataset are used.
#'   Otherwise, it is a length-2 list with names "numerics" and "characters"
#'   each giving functions for how to handle that data type. The default is
#'   list(numerics = median, characters = Mode)
#' @param numerics For numeric variables being varied, how to select values. By
#'   default, the minimum, 25th-, 50th-, 75th-percentile, and maximum values
#'   from the training dataset will be used. If this is an integer, it specifies
#'   the number of evenly spaced quantiles to use. If it is a numeric vector, it
#'   should be in [0, 1] and specifies the quantiles to use. A function that
#'   takes a numeric vector (the column from training data) and returns a
#'   numeric vector (the values to be used) may also be provided
#' @param characters For categorical variables being varied, how to select
#'   values? By default all unique values are used. If an integer (n) is
#'   provided, the n-most common categories in the training dataset will be
#'   used. A function that takes a character vector (the column from training
#'   data) and returns a character vector (the values to be used) may also be
#'   provided
#'
#' @return A tibble with values of variables used to make predictions and
#'   predictions. Has class \code{simulated_df} and \code{predicted_df}.
#' @export
#'
#' @details If \code{vary} is an integer, the most important variables are
#'   determined by \code{\link{get_variable_importance}}, unless glm is the only
#'   model present, in which case \code{\link{interpret}} is used with a
#'   warning.
#'
#'   When selecting the most important variables to vary over, for categorical
#'   variables the sum of variable importance of all the levels as dummy
#'   variables is used.
#'
#' @examples
simulate <- function(models,
                     vary = 4,
                     hold = list(numerics = median, characters = Mode),
                     numerics = 5,
                     characters = unique) {

  if (!is.model_list(models))
    stop("model must be a model_list object; this is a ", class(models)[1])

  rec <- attr(models, "recipe")
  if (is.null(rec))
    stop("model must have been trained on prepped data, either through ",
         "machine_learn or prep_data.")

  variables <-
    rec$var_info %>%
    dplyr::filter(role == "predictor") %>%
    split(., .$type) %>%
    purrr::map(pull, variable)


  create_varying_df()
  add_static_columns()




}

# Create the data frame containing combinations of variable values on which to
# make counterfactual predictions, but not containing fixed predictors.
create_varying_df <- function(models, vary, variables) {

  # If vary is a list, our job is easy
  if (is.list(vary)) {
    test_presence(names(vary), variables)
  } else {
    # If vary is numeric, we must first identify which variables to vary
    if (is.numeric(vary)) {
      vary <- choose_variables(models, vary, variables)
    } else if (is.character(vary)) {
      test_presence(vary, variables)
    }
    # Now we have a character vector of variables to vary, need to choose values,
    # which come back as a named list
    vary <- choose_values(models, vary, variables, numerics, characters)
  }

  expand.grid(vary) %>%
    return()

}

# Test whether all vary are in variables, which is a list, and stop if not
test_presence <- function(vary, variables) {
  present <- vary %in% unlist(variables)
  if (!all(present))
    stop("The following variable(s) passed to vary aren't predictors: ",
         list_variables(vary[!present]))
}

# Choose which variables to vary for CF predictions
choose_variables <- function(models, vary, variables) {
  vi <-
    if (!dplyr::setequal(names(models), "glmnet")) {
      # Suppress warning that glmnet is best model if it is
      suppressWarnings( get_variable_importance(models) )
    } else {
      # Format interpret to function as variable importance
      interpret(models) %>%
        dplyr::transmute(variable = variable, importance = abs(coefficient)) %>%
        dplyr::filter(variable != "(Intercept)")
    }
  # Only keep the top level of any categoricals
  vi <-
    purrr::map_df(variables$nominal, function(var_name) {
      vi %>%
        filter(stringr::str_detect(variable, var_name)) %>%
        dplyr::summarize(variable = var_name, importance = sum(importance))
    }) %>%
    # Add numerics and sort by importance
    dplyr::bind_rows(dplyr::filter(vi, variable %in% variables$numeric)) %>%
    arrange(desc(importance))
  if (length(vary) == 1)
    vary <- seq(1, vary)
  dplyr::slice(vi, vary) %>%
    dplyr::pull(variable)
}


choose_values <- function(models, vary, variables, numerics, characters) {

}

attr(models, "original_data_str")

