# Everything in the return list gets assigned in the calling environment (tune_models and flash_models)
setup_training <- function(d, outcome, model_class, models, metric, positive_class, n_folds) {

  # Get recipe and remove columns to be ignored in training
  recipe <- attr(d, "recipe")
  if (is.null(recipe))
    stop("data must be prepped before training models. If you want to prep ",
         "data yourself (not recommended), use `prep_data(..., no_prep = TRUE)`")
  outcome_chr <- rlang::quo_name(outcome)
  if (outcome_chr %in% attr(recipe, "ignored_columns"))
    stop("You specified ", outcome_chr, " as your outcome variable, but ",
         "the recipe you used to prep your data says to ignore it. ",
         "Did you forget to specify `outcome = ` in prep_data?")
  d <- remove_ignored(d, recipe)

  # Check outcome provided, agrees with outcome in prep_data, present in d
  outcome <- check_outcome(outcome, names(d), recipe)
  outcome_chr <- rlang::quo_name(outcome)

  # Get original data structure
  original_data_str <- get_original_data(d, outcome_chr)

  # Get any best_levels attributes from d
  best_levels <- attr(d, "best_levels")

  # Remove all-unique charactor/factor columns with a warning
  to_ignore <- find_columns_to_ignore(d, already_ignored = outcome_chr)
  d <- d[, !names(d) %in% to_ignore, drop = FALSE]

  # tibbles upset some algorithms, so make it a data frame
  d <- as.data.frame(d)
  # glmnet doesn't allow any character/factor predictors so stop if they're present
  non_numerics <-
    get_classes_sorted(select_not(d, outcome)) %>%
    dplyr::filter(!is_numeric) %>%
    dplyr::pull(variable)
  if (length(non_numerics))
    stop("All predictors must be numeric, but the following variables are not ",
         "numeric. Consider using prep_data to get data ready for model ",
         "training: ", list_variables(non_numerics))

  if (missing(models)) {
    models <- get_supported_models()
  } else {
    # Make `models` case insensitive
    models <- tolower(models)
  }
  # Make sure outcome's class works with model_class, or infer it
  model_class <- set_model_class(model_class,
                                 dplyr::pull(d, !!outcome),
                                 outcome_chr)
  outcome_tab <- table(d[[outcome_chr]])
  if (model_class == "classification") {
    if (missing(positive_class))
      positive_class <- NULL
    d[[outcome_chr]] <-
      d[[outcome_chr]] %>%
      # Some algorithms need the response to be factor instead of char or lgl
      as.factor() %>%
      # Get rid of unused levels if they're present
      droplevels() %>%
      # Choose positive class and set it to the factor reference level
      set_outcome_class(positive_class = positive_class,
                        original_classes = names(attributes(recipe)$factor_levels[[outcome_chr]]))
    # Make sure there can be at least one instance of outcome in each fold
  }
  if (model_class == "classification") {
    if (min(outcome_tab) < n_folds)
      stop("There must be at least one instance of each outcome class ",
           "for each cross validation fold. Observed frequencies in d:\n",
           paste(names(outcome_tab), outcome_tab, sep = " = ", collapse = ", "),
           "\nYou could try turning n_folds down from its current value of ", n_folds,
           ", but it's hard to train a good model with few observations of an outcome.")
  }
  if (model_class == "multiclass")
    if (max(outcome_tab) / nrow(d) <= 0.05)
      warning("Your outcome variable has categories that are sparsely ",
              "distributed. It may be hard for the model to correctly predict ",
              "them.")

  # Choose metric if not provided
  if (missing(metric))
    metric <- set_default_metric(model_class)
  else
    metric <- check_metric(model_class, metric)

  # Make sure models are supported
  models <- check_models(models)

  # Make sure there's no missingness in predictors
  miss <- missingness(select_not(d, outcome_chr), return_df = FALSE)
  if (any(miss > 0))
    stop("There is missingness in the following predictors. You can impute values to fill in ",
         "the missingness by calling `prep_data` before tuning models, or using `machine_learn` ",
         "to prep the data and train models.\n",
         paste0(names(miss)[miss > 0], collapse = ", "))

  return(list(d = d, outcome = outcome, model_class = model_class,
              models = models, metric = metric, recipe = recipe,
              best_levels = best_levels, original_data_str = original_data_str))
}

check_outcome <- function(outcome, d_names, recipe) {
  if (rlang::quo_is_missing(outcome)) {
    if (is.null(recipe))
      stop("Your data is not prepared. Either provide provide an outcome ",
           "variable to tune_models and flash_models, or prepare your data in ",
           "prep_data")
    else
      outcome <- recipe$var_info$variable[recipe$var_info$role == "outcome"]
  }
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% d_names)
    stop(outcome_chr, " isn't a column in d.")
  if (!is.null(recipe)) {
    prep_outcome <- recipe$var_info$variable[recipe$var_info$role == "outcome"]
    if (length(prep_outcome) && prep_outcome != outcome_chr)
      stop("outcome in prep_data (", prep_outcome, ") and outcome in tune models (",
           outcome_chr, ") are different. They need to be the same.")
  }
  return(outcome)
}

set_outcome_class <- function(vec, positive_class, original_classes) {
  if (length(levels(vec)) != 2)
    stop(paste0("The outcome variable must have two levels for classification, ",
                "but this has ", length(levels(vec)), ": ", paste(levels(vec), collapse = ", ")))
  if (!all(original_classes %in% vec))
    stop("It looks like outcome levels have changed between data prep and ",
         "model training. Change them before prep_data instead.\nPre-prep ",
         "values: ", list_variables(original_classes), "\nCurrent values: ",
         list_variables(unique(vec)))
  if (is.null(positive_class)) {
    positive_class <-
      if ("Y" %in% levels(vec)) {
        "Y"
      } else if ("yes" %in% levels(vec)) {
        "yes"
      } else {
        levels(vec)[1]
      }
  }
  if (!positive_class %in% levels(vec))
    stop("positive_class, ", positive_class, ", not found in the outcome column. ",
         "Outcome has values ", list_variables(levels(vec)))
  vec <- stats::relevel(vec, positive_class)
  return(vec)
}

remove_ignored <- function(d, recipe) {
  # Pull columns ignored in prep_data out of d
  ignored <- attr(recipe, "ignored_columns")
  # Only ignored columns that are present now
  ignored <- ignored[ignored %in% names(d)]
  if (!is.null(ignored) && length(ignored)) {
    d <- dplyr::select(d, -dplyr::one_of(ignored))
    message("Variable(s) ignored in prep_data won't be used to tune models: ",
            list_variables(ignored))
  }
  return(d)
}

set_model_class <- function(model_class, outcome, outcome_chr) {
  n_outcomes <- dplyr::n_distinct(outcome)
  outcome_class <- class(outcome)
  looks_categorical <- outcome_class %in% c("character", "factor")
  looks_numeric <- outcome_class %in% c("integer", "numeric")
  if (!looks_categorical && !looks_numeric) {
    # outcome is weird class such as list
    stop(outcome_chr, " is ", outcome_class,
         ", and tune_models doesn't know what to do with that.")
  } else if (missing(model_class)) {
    # Need to infer model_class
    if (looks_categorical) {
      if (n_outcomes > 2) {
        mes <- paste0(outcome_chr, " looks multiclass, so training multiclass algorithms.")
        model_class <- "multiclass"
      } else {
        mes <- paste0(outcome_chr, " looks categorical, so training classification algorithms.")
        model_class <- "classification"
      }
    } else {
      mes <- paste0(outcome_chr, " looks numeric, so training regression algorithms.")
      model_class <- "regression"
      # User provided model_class, so check it
    }
    message("\n", mes)
  } else {
    # Check user-provided model_class
    supported_classes <- get_supported_model_classes()
    if (!model_class %in% supported_classes)
      stop("Supported model classes are: ",
           list_variables(supported_classes),
           ". You supplied this unsupported class: ", model_class)
    if (looks_categorical && model_class == "regression") {
      stop(outcome_chr, " looks categorical but you're trying to train a regression model.")
    } else if (looks_numeric && model_class %in% c("classification", "multiclass")) {
      stop(outcome_chr, " looks numeric but you're trying to train a classification ",
           "model. If that's what you want convert it explicitly with as.factor().")
    } else if (looks_categorical && model_class == "classification" && n_outcomes > 2) {
      stop(outcome_chr, " looks multiclass but you're trying to train a 2-class,
           classification model. Use model_class = 'Multiclass'")
    }
  }
  return(model_class)
}


check_models <- function(models) {
  available <- get_supported_models()
  unsupported <- models[!models %in% available]
  if (length(unsupported))
    stop("Currently supported algorithms are: ",
         list_variables(available),
         ". You supplied these unsupported algorithms: ",
         list_variables(unsupported))
  return(models)
}

check_metric <- function(model_class, metric) {
  if (is.na(metric)) {
    metric <- set_default_metric(model_class)
    warning("The given metric is NA, evaluating models with ", metric,
            " instead")
  } else if ((model_class == "regression" &&
               !(metric %in% c("MAE", "RMSE", "Rsquared"))) ||
              (model_class == "classification" &&
               !(metric %in% c("ROC", "PR"))) ||
              (model_class == "multiclass" &&
               !(metric %in% c("Accuracy", "Kappa")))) {
      new_metric <- set_default_metric(model_class)
      warning("Healthcareai does not support ", metric,
              ", evaluating models with ", new_metric, " instead")
      metric <- new_metric
  } else if (!(model_class %in%
               c("regression", "classification", "multiclass"))) {
    stop("Healthcareai does not support ", model_class, " yet.")
  }
  return(metric)
}

set_default_metric <- function(model_class) {
  if (model_class == "regression") {
    return("RMSE")
  } else if (model_class == "classification") {
    return("ROC")
  } else if (model_class == "multiclass") {
    return("Accuracy")
  } else {
    stop("Don't have default metric for model class ", model_class)
  }
}

setup_train_control <- function(model_class, metric, n_folds) {
  # Always use grid. We'll make our own, one row if not tuning, in train_models
  train_control <- caret::trainControl(method = "cv",
                                       number = n_folds,
                                       search = "grid",
                                       savePredictions = "final")
  # trainControl defaults are good for regression. Change for classification
  if (model_class == "classification") {
    if (metric == "PR") {
      train_control$summaryFunction <- caret::prSummary
    } else {
      train_control$summaryFunction <- caret::twoClassSummary
    }
    train_control$classProbs <- TRUE
  }
  return(train_control)
}

character_in_quo <- function(x)
  is.character(purrr::safely(rlang::eval_tidy)(x)$result)

get_original_data <- function(d, outcome_chr) {
  # If it's on d, that's it
  ods <- attr(d, "original_data_str")
  # Otherwise, create it
  if (is.null(ods))
    ods <- d[0, -which(names(d) == outcome_chr), drop = FALSE]
  return(ods)
}

# ddim is dim(d)
# hpdim is map_int(hyperparameters, nrow)
check_training_time <- function(ddim, hpdim, n_folds) {
  # Minus one column for the outcome
  ddim[2] <- ddim[2] - 1L
  ncells <- prod(ddim)
  n_models <-
    paste0(n_folds * hpdim, " ", names(hpdim), "'s") %>%
    list_variables()
  mes <- paste(
    "\nAfter data processing, models are being trained on", format(ddim[2], big.mark = ","),
    "features with", format(ddim[1], big.mark = ","), "observations.\nBased on n_folds =",
    n_folds, "and hyperparameter settings, the following number of",
    "models will be trained:", n_models, "\n"
  )
  # N is a rough indicator of model training time, developed in ml.internal/r-pkg/package_profiling
  # rows * columns * hp-depth * n_folds summed over models
  N <- log10(ncells * sum(hpdim) * n_folds)
  mes <- paste0(
    mes,
    if (N > 10) {
      paste("WARNING: MODEL TRAINING COULD TAKE A REALLY LONG TIME. If you don't",
            "know what you've just started, start smaller by turning off tuning,",
            "training fewer algorithms, or using a subset of oberservations, and",
            "work your way up once you have a sense of training time.",
            "This vignette on improving model training perfomance may be helpful:",
            "https://docs.healthcare.ai/articles/site_only/performance.html")
    } else if (N > 9) {
      paste("Model training may take hours. If you'd like to speed things up,",
            "this vignette on improving model training perfomance may be helpful:",
            "https://docs.healthcare.ai/articles/site_only/performance.html")
    } else if (N > 8)  {
      "Model training may take a few minutes."
    }
  )
  return(mes)
}
