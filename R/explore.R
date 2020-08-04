#' Explore a model's "reasoning" via counterfactual predictions
#'
#' @param models A model_list object. The data the model was trained on must
#'   have been prepared, either by training with \code{\link{machine_learn}} or
#'   by preparing with \code{\link{prep_data}} before model training.
#' @param vary Which (or how many) features to vary? Default is 4; if
#'   \code{vary} is a single integer (n), the n-most-important features are
#'   varied (see Details for how importance is determined). If \code{vary} is a
#'   vector of integers, those rankings of features are used (e.g. \code{vary =
#'   2:4} varies the 2nd, 3rd, and 4th most-important features). Alternatively,
#'   you can specify which features to vary by passing a vector of feature
#'   names. For the finest level of control, you can choose the alternative
#'   values to use by passing a list with names being features names and entries
#'   being values to use; in this case \code{numerics} and \code{characters} are
#'   ignored.
#' @param hold How to choose the values of features not being varied? To make
#'   counterfactual predictions for a particular patient, this can be a row of
#'   the training data frame (or a one-row data frame containing values for all
#'   of the non-varying features). Alternatively, this can be functions to
#'   determine the values of non-varying features, in which case it must be a
#'   length-2 list with names "numerics" and "characters", each being a function
#'   to determine the values of non-varying features of that data type. The
#'   default is \code{list(numerics = median, characters = Mode)}; numerics is
#'   applied to the column from the training data, characters is applied to a
#'   frequency table of the column from the training data.
#' @param numerics How to determine values of numeric features being varied? By
#'   default, the 5th, 25th, 50th (median), 75th, and 95th percentile values
#'   from the training dataset will be used. To specify evenly spaced quantiles,
#'   starting with the 5th and ending with the 95th, pass an integer to this
#'   argument. To specify which quantiles to use, pass a numeric vector in [0,
#'   1] to this argument, e.g. \code{c(0, .5, 1)} for the minimum, median, and
#'   maximum values from the training dataset.
#' @param characters Integer. For categorical variables being varied, how many
#'   values to use? Values are used from most- to least-common; default is 5.
#'
#' @return A tibble with values of features used to make predictions and
#'   predictions. Has class \code{explore_df} and attribute \code{vi} giving
#'   information about the varying features.
#' @export
#' @importFrom stats median
#'
#' @description Make predictions for observations that vary over features of
#'   interest. There are two major use cases for this function. One is to
#'   understand how the model responds to features, not just individually but
#'   over combinations of features (i.e. interaction effects). The other is to
#'   explore how an individual prediction would vary if feature values were
#'   different. \strong{Note, however, that this function does not establish
#'   causality and the latter use case should be deployed judiciously.}
#'
#' @details If \code{vary} is an integer, the most important features are
#'   determined by \code{\link{get_variable_importance}}, unless glm is the only
#'   model present, in which case \code{\link{interpret}} is used with a
#'   warning. When selecting the most important features to vary, for
#'   categorical features the sum of feature importance of all the levels as
#'   dummies is used.
#'
#' @seealso \code{\link{plot.explore_df}}
#'
#' @examples
#' # First, we need a model on which to make counterfactual predictions
#' set.seed(5176)
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
#'                    tune = FALSE, models = "xgb")
#'
#' # By default, the four most important features are varied, with numeric
#' # features taking their 5, 25, 50, 75, and 95 percentile values, and
#' # categoricals taking their five most common values. Others features are
#' # held at their median and modal values for numeric and categorical features,
#' # respectively. This can provide insight into how the model responds to
#' # different features
#' explore(m)
#'
#' # It is easy to plot counterfactual predictions. By default, only the two most
#' # important features are plotted over; see `?plot.explore_df` for
#' # customization options
#' explore(m) %>%
#'   plot()
#'
#' # You can specify which features vary and what values they take in a variety of
#' # ways. For example, you could vary only "weight_class" and "plasma_glucose"
#' explore(m, vary = c("weight_class", "plasma_glucose"))
#'
#' # You can also control what values non-varying features take.
#' # For example, if you want to simulate alternative scenarios for patient 321
#' patient321 <- dplyr::filter(pima_diabetes, patient_id == 321)
#' patient321
#' explore(m, hold = patient321)
#'
#' # Here is an example in which both the varying and non-varying feature values
#' # are explicitly specified.
#' explore(m,
#'         vary = list(weight_class = c("normal", "overweight", "obese"),
#'                     plasma_glucose = seq(60, 200, 10)),
#'         hold = list(pregnancies = 2,
#'                     pedigree = .5,
#'                     age = 25,
#'                     insulin = NA,
#'                     skinfold = NA,
#'                     diastolic_bp = 85)) %>%
#'   plot()
explore <- function(models,
                    vary = 4,
                    hold = list(numerics = median, characters = Mode),
                    numerics = c(.05, .25, .5, .75, .95),
                    characters = 5) {

  if (!is.model_list(models))
    stop("models must be a model_list object; this is a ", class(models)[1])

  if (extract_model_info(models)$m_class == "Multiclass")
    stop("explore doesn't support multiclass models yet.")

  rec <- attr(models, "recipe")
  if (is.null(rec))
    stop("models must have been trained on prepped data, either through ",
         "machine_learn or prep_data.")
  if (is.null(attr(models, "recipe")$template))
    stop("Explore requires that data is attached to the model object. You must",
         " use `save_models(model, sanitize_phi = FALSE)` to keep it.")
  training_data <- attr(models, "recipe")$orig_data

  variables <-
    rec$var_info %>%
    dplyr::filter(role == "predictor") %>%
    dplyr::mutate(type = dplyr::case_when(type == "logical" ~ "numeric",
                                          TRUE ~ type)) %>%
    split(., .$type) %>%
    purrr::map(dplyr::pull, variable)

  d <- create_varying_df(models = models, vary = vary, variables = variables,
                         numerics = numerics, characters = characters,
                         training_data = training_data)
  static <- choose_static_values(models = models,
                                 static_variables = purrr::map(variables, dplyr::setdiff, names(d)),
                                 hold = hold,
                                 training_data = training_data)
  static <- do.call(dplyr::bind_rows, replicate(nrow(d), static, simplify = FALSE))
  if (!all(dim(static) == c(0, 0))) {
    d <- dplyr::bind_cols(d, static)
  }
  suppressWarnings(suppressMessages(preds <- predict(models, d)))
  # Intentionally leave off the predicted_df class here to avoid performance-
  # in-training info printing
  structure(preds, class = c("explore_df", class(d)))
}

# Create the data frame containing combinations of variable values on which to
# make counterfactual predictions, but not containing fixed predictors.
create_varying_df <- function(models, vary, variables, numerics, characters, training_data) {

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
                          numerics = numerics, characters = characters, training_data)
  }

  # Collect variable attributes to keep with the explore_df object
  vi <- map_df(names(vary), ~ tibble(variable = .x,
                                     numeric = is.numeric(vary[[.x]]),
                                     nlev = length(vary[[.x]])))

  expand.grid(vary, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    structure(vi = vi) %>%
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
  var_imp <-
    if (!dplyr::setequal(names(models), "glmnet")) {
      # Suppress warning that glmnet is best model if it is
      suppressWarnings(get_variable_importance(models))
    } else {
      # Format interpret to function as variable importance
      if (!"scale" %in% tidy(attr(models, "recipe"))$type)
        warning("glm is the only model present in models, so coefficients will be used ",
                "to determine which variables to vary. However, variables weren't ",
                "scaled in `prep_data`, which means coefficients aren't a good ",
                "measure of variable importance. Consider using another algorithm or ",
                "`prep_data(scale = TRUE)` prior to model training.")
      interpret(models) %>%
        dplyr::transmute(variable = variable, importance = abs(coefficient)) %>%
        dplyr::filter(variable != "(Intercept)")
    }
  # Only keep the top level of any categoricals
  var_imp <-
    purrr::map_df(variables$nominal, function(var_name) {
      var_imp %>%
        filter(stringr::str_detect(variable, var_name)) %>%
        dplyr::summarize(variable = var_name, importance = sum(importance))
    }) %>%
    # Add numerics and sort by importance
    dplyr::bind_rows(dplyr::filter(var_imp, variable %in% variables$numeric)) %>%
    arrange(desc(importance))
  if (length(vary) == 1)
    vary <- seq(1, vary)
  dplyr::slice(var_imp, vary) %>%
    dplyr::pull(variable)
}


choose_values <- function(models, vary, variables, numerics, characters, training_data) {
  vary_order <- vary
  # Choose nominal values
  noms_to_use <- dplyr::intersect(variables$nominal, vary)
  noms <-
    if (length(noms_to_use)) {
      dplyr::select(training_data, noms_to_use) %>%
        purrr::map(function(var) {
          ft <- sort(table(var, useNA = "ifany"), decreasing = TRUE)
          names(ft)[seq_len(min(length(ft), characters))]
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
        numerics <- seq(.05, .95, len = numerics)
      }
      dplyr::select(training_data, nums_to_use) %>%
        purrr::map(stats::quantile, numerics, na.rm = TRUE)
    } else {
      NULL
    }
  both <- c(noms, nums)
  # Keeps variables in order by variable importance
  both[order(match(names(both), vary_order))] %>%
    return()
}

choose_static_values <- function(models, static_variables, hold, training_data) {
  if (!rlang::is_named(hold))
    stop("`hold` must be a named list (or data frame), whether it contains ",
         "functions to determine values or values themselves.")
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
      # simplify?
      dplyr::select(training_data,
                    which(names(training_data) %in% static_variables$numeric)) %>%
      purrr::map_dbl(hold$numerics, na.rm = TRUE)
    nom_holds <-
      dplyr::select(training_data, static_variables$nominal) %>%
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
