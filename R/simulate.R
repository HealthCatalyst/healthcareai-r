#' Simulate Counterfactual Predictions
#'
#' @param models A model_list object. The data the model was trained on must have
#'   been prepared, either by training with \code{\link{machine_learn}} or by
#'   preparing with \code{\link{prep_data}} before model training
#' @param vary Which (or how many) variables to vary? Default is 4; if
#'   \code{vary} is a single integer (n), the n-most-important variables are
#'   varied (see details for importance is determined). If \code{vary} is a
#'   vector of integers, those rankings of variables are used (e.g. \code{vary =
#'   2:4} varies the 2nd, 3rd, and 4th most-important variables). Alternatively,
#'   a vector of variable names to vary may be provided. Or, for the finest
#'   level of control, a list with names being variable names and entries being
#'   values to use; in this case \code{numerics} and \code{characters} are
#'   ignored.
#' @param hold How to choose the values of variables not being varied? This can
#'   either be a length-2 list with names "numerics" and "characters", each
#'   giving functions for how to handle that data type. The default is
#'   \code{list(numerics = median, characters = Mode)}; numerics is applied to
#'   the column from the training data, \code{characters} is applied to a
#'   frequency table of the column from the training data. Alternatively, this
#'   can be a row of a data frame from the training data if, for example, you
#'   want to run simulations for a particular patient. It can also be a list or
#'   one-row data frame containing values of variables at which to hold; in this
#'   case values of variables that vary are ignored.
#' @param numerics For numeric variables being varied, how to select values? By
#'   default, the minimum, 25th-, 50th-, 75th-percentile, and maximum values
#'   from the training dataset will be used. If this is an integer, it specifies
#'   the number of evenly spaced quantiles to use (default = 5). If it is a
#'   numeric vector, it specifies the quantiles to use and so must be in [0, 1].
#' @param characters Integer. For categorical variables being varied, how many
#'   values to use? Values are used from most- to least-common; default is all
#'   (Inf).
#'
#' @return A tibble with values of variables used to make predictions and
#'   predictions. Has class \code{simulated_df} and \code{predicted_df}.
#' @export
#'
#' @description This function makes predictions for observations that vary over
#'   variables of interest. At the model level, it can be used to understand how
#'   different combinations of variables influence predictions, and the
#'   individual level it can be used to explore what an observation's prediction
#'   would be if certain variables were different. \strong{Note that causality
#'   cannot be established by this function.}
#'
#' @details If \code{vary} is an integer, the most important variables are
#'   determined by \code{\link{get_variable_importance}}, unless glm is the only
#'   model present, in which case \code{\link{interpret}} is used with a
#'   warning. When selecting the most important variables to vary over, for
#'   categorical variables the sum of variable importance of all the levels as
#'   dummy variables is used.
#'
#' @examples
#' # First, we need a model on which to run simulations
#' set.seed(5176)
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
#'                    tune = FALSE, models = "xgb")
#'
#' # By default, the four most important variables are varied, with numeric
#' # variables taking their 0, 25%, 50%, 75%, and 100% quantile values. Others are
#' # held at their median and modal values for numeric and categorical variables,
#' # respectively. This can help to understand how the model responds to different
#' # variables
#' simulate(m)
#'
#' # You can specify which variables vary and what values they take in a variety of
#' # ways. For example, you could vary only "weight_class" and "plasma_glucose"
#' simulate(m, vary = c("weight_class", "plasma_glucose"))
#'
#' # You can also control what values non-varying variables take.
#' # For example, if you want to simulate alternative scenarios for patient 321
#' patient321 <- dplyr::filter(pima_diabetes, patient_id == 321)
#' patient321
#' simulate(m, hold = patient321)
simulate <- function(models,
                     vary = 4,
                     hold = list(numerics = median, characters = Mode),
                     numerics = 5,
                     characters = Inf) {

  if (!is.model_list(models))
    stop("models must be a model_list object; this is a ", class(models)[1])

  rec <- attr(models, "recipe")
  if (is.null(rec))
    stop("models must have been trained on prepped data, either through ",
         "machine_learn or prep_data.")

  variables <-
    rec$var_info %>%
    dplyr::filter(role == "predictor") %>%
    split(., .$type) %>%
    purrr::map(pull, variable)

  d <- create_varying_df(models = models, vary = vary, variables = variables,
                         numerics = numerics, characters = characters)
  static <- choose_static_values(models = models,
                                 static_variables = purrr::map(variables, dplyr::setdiff, names(d)),
                                 hold = hold)
  static <- do.call(dplyr::bind_rows, replicate(nrow(d), static, simplify = FALSE))
  d <- dplyr::bind_cols(d, static)
  suppressMessages( preds <- predict(models, d) )
  # Intentionally leave off the predicted_df class here to avoid performance-
  # in-training info printing
  structure(preds, class = c("simulated_df", class(d)))
}

############## plot method




############## helper functions

# Create the data frame containing combinations of variable values on which to
# make counterfactual predictions, but not containing fixed predictors.
create_varying_df <- function(models, vary, variables, numerics, characters) {

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
    vary <- choose_values(models = models, vary = vary, variables = variables,
                          numerics = numerics, characters = characters)
  }

  expand.grid(vary, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
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
  # Choose nominal values
  noms_to_use <- dplyr::intersect(variables$nominal, vary)
  noms <-
    if (length(noms_to_use)) {
      rec <- attr(models, "recipe")
      attr(rec, "factor_levels")[noms_to_use] %>%
        purrr::map(function(vartab) {
          names(sort(vartab, decreasing = TRUE))[seq_len(min(length(vartab), characters))]
        })
    } else {
      NULL
    }
  # Choose numeric values
  nums_to_use <- dplyr::intersect(variables$numeric, vary)
  nums <-
    if (length(nums_to_use)) {
      # Create quantiles if numerics is a single number in (0, 1)
      if (length(numerics) == 1 && numerics > 1) {
        numerics <- seq(0, 1, len = numerics)
      }
      dplyr::select(models[[1]]$trainingData, nums_to_use) %>%
        purrr::map(quantile, numerics, na.rm = TRUE)
    } else {
      NULL
    }
  return(c(noms, nums))
}

choose_static_values <- function(models, static_variables, hold) {
  if (setequal(names(hold), c("numerics", "characters"))) {
    # hold is two functions
    not_funs <- purrr::map_chr(hold, class) %>% .[. != "function"]
    if (length(not_funs))
      stop("`hold` must either be values of non-varying variables to use ",
           'or a length-2 list with names "numerics" and "characters" containing ',
           "functions to determine what values of non-varying numeric and ",
           "character variables to use. You provided the following non-functions: ",
           list_variables(paste(names(not_funs), not_funs, sep = " is ")))
    num_holds <-
      dplyr::select(models[[1]]$trainingData, static_variables$numeric) %>%
      purrr::map_dbl(hold$numerics)
    nom_holds <-
      attr(attr(models, "recipe"), "factor_levels")[static_variables$nominal] %>%
      purrr::map_chr(hold$characters)
    hold_values <- purrr::flatten(list(num_holds, nom_holds))
  } else {
    # hold is list/df of values to use
    not_provided <- setdiff(unlist(static_variables), names(hold))
    if (length(not_provided))
      stop("`hold` must either be values of non-varying variables to use ",
           'or a length-2 list with names "numerics" and "characters" containing ',
           "functions to determine what values of non-varying numeric and ",
           "character variables to use. You provided ", list_variables(names(hold)),
           " but didn't provide values for the folloiwng variables: ",
           list_variables(not_provided))
    hold_values <- hold[names(hold) %in% unname(unlist(static_variables))]
  }
  return(hold_values)
}
