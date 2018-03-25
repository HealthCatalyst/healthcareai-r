#' Identify the best performing model by tuning hyperparameters via
#' cross-validation
#'
#' @param d A data frame
#' @param outcome Name of the column to predict
#' @param model_class "regression" or "classification". If not provided, this
#'   will be determined by the class of `outcome` with the determination
#'   displayed in a message.
#' @param models Names of models to try, by default "rf" for random forest and
#'   "knn" for k-nearest neighbors. See \code{\link{supported_models}} for
#'   available models.
#' @param n_folds How many folds to use in cross-validation? Default = 5.
#' @param tune_depth How many hyperparameter combinations to try? Defualt = 10.
#' @param tune_method How to search hyperparameter space? Default = "random".
#' @param metric What metric to use to assess model performance? Options for
#'   regression: "RMSE" (root-mean-squared error, default), "MAE" (mean-absolute
#'   error), or "Rsquared." For classification: "ROC" (area under the receiver
#'   operating characteristic curve), or "PR" (area under the precision-recall
#'   curve).
#' @param hyperparameters Currently not supported.
#'
#' @export
#' @importFrom kknn kknn
#' @importFrom ranger ranger
#' @importFrom dplyr mutate
#' @importFrom rlang quo_name
#'
#' @seealso \code{\link{prep_data}}, \code{\link{predict.model_list}},
#'   \code{\link{supported_models}}
#'
#' @return A model_list object
#' @details Note that this function is training a lot of models (100 by default)
#'   and so can take a while to execute. In general a model is trained for each
#'   hyperparameter combination in each fold for each model, so run time is a
#'   function of length(models) x n_folds x tune_depth. At the default settings,
#'   a 1000 row, 10 column data frame should complete in about 30 seconds on a
#'   good laptop.
#'
#' @examples
#' \dontrun{
#' ### Takes ~20 seconds
#' # Prepare data for tuning
#' d <- prep_data(pima_diabetes, patient_id, outcome = diabetes)
#'
#' # Tune random forest and k-nearest neighbors classification models
#' m <- tune_models(d, outcome = diabetes)
#'
#' # Get some info about the tuned models
#' m
#'
#' # Get more detailed info
#' summary(m)
#'
#' # Plot performance over hyperparameter values for each algorithm
#' plot(m)
#'
#' # Extract confusion matrix for random forest (the model with best-performing
#' # hyperparameter values is used)
#' caret::confusionMatrix(m$`Random Forest`, norm = "none")
#'
#' # Compare performance of algorithms at best hyperparameter values
#' rs <- resamples(m)
#' dotplot(rs)
#' }
tune_models <- function(d,
                        outcome,
                        model_class,
                        models = c("rf", "knn"),
                        n_folds = 5,
                        tune_depth = 10,
                        tune_method = "random",
                        metric,
                        hyperparameters) {

  recipe <- attr(d, "recipe")
  if (!is.null(recipe))
    d <- remove_ignored(d, recipe)

  outcome <- check_outcome(rlang::enquo(outcome), names(d), recipe)
  outcome_chr <- rlang::quo_name(outcome)

  # tibbles upset some algorithms, so make it a data frame
  d <- as.data.frame(d)
  # Convert all character variables to factors. kknn sometimes chokes on chars
  d <- dplyr::mutate_if(d, is.character, as.factor)
  # Make `models` case insensitive
  models <- tolower(models)
  if (n_folds <= 1)
    stop("n_folds must be greater than 1.")

  # Make sure outcome's class works with model_class, or infer it
  model_class <- set_model_class(model_class, class(dplyr::pull(d, !!outcome)), outcome_chr)

  # Some algorithms need the response to be factor instead of char or lgl
  # Get rid of unused levels if they're present
  if (model_class == "classification")
    d <- dplyr::mutate(d,
                       !!outcome_chr := as.factor(!!outcome),
                       !!outcome_chr := droplevels(!!outcome))

  # Choose metric if not provided
  if (missing(metric))
    metric <- set_default_metric(model_class)

  # Make sure models are supported
  models <- setup_models(models)

  # Set up cross validation details
  train_control <- setup_training(tune_method, n_folds, model_class, metric)
  if (metric == "PR")
    metric <- "AUC" # For caret internal function

  # Crude check for training that will take a while
  n_mod <- n_folds * tune_depth * length(models)
  obs <- nrow(d)
  if ( (obs > 1000 && n_mod > 10) || (obs > 100 && n_mod > 100) )
    message("You've chosen to tune ", n_mod, " models (n_folds = ", n_folds,
            " x tune_depth = ", tune_depth, " x ", "length(models) = ",
            length(models), ") on a ", format(obs, big.mark = ","), " row dataset. ",
            "This may take a while...")

  # Loop over models, tuning each
  train_list <-
    lapply(models, function(model) {
      message("Running cross validation for ",
              caret::getModelInfo(model)[[1]]$label)
      # Reduce kmax for kknn
      if (model == "kknn")
        model <- adjust_knn()
      # Train models
      suppressPackageStartupMessages(
        caret::train(x = dplyr::select(d, -!!outcome),
                     y = dplyr::pull(d, !!outcome),
                     method = model,
                     metric = metric,
                     trControl = train_control,
                     tuneLength = tune_depth
        )
      )
    })

  # Add classes
  train_list <- as.model_list(listed_models = train_list, target = outcome_chr)

  # Add recipe object if one came in on d
  attr(train_list, "recipe") <- recipe

  return(train_list)
}

check_outcome <- function(outcome, d_names, recipe) {
  if (rlang::quo_is_missing(outcome))
    stop("You must provide an outcome variable to tune_models.")
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

remove_ignored <- function(d, recipe) {
  # Pull columns ignored in prep_data out of d
  ignored <- attr(recipe, "ignored_columns")
  # Only ignored columns that are present now
  ignored <- ignored[ignored %in% names(d)]
  if (!is.null(ignored) && length(ignored)) {
    d <- dplyr::select(d, -dplyr::one_of(ignored))
    message("Variable(s) ignored in prep_data won't be used to tune models: ",
            paste(ignored, collapse = ", "))
  }
  return(d)
}

set_model_class <- function(model_class, outcome_class, outcome_chr) {
  looks_categorical <- outcome_class %in% c("character", "factor")
  looks_numeric <- outcome_class %in% c("integer", "numeric")
  if (!looks_categorical && !looks_numeric) {
    # outcome is weird class such as list
    stop(outcome_chr, " is ", outcome_class,
         ", and tune_models doesn't know what to do with that.")
  } else if (missing(model_class)) {
    # Need to infer model_class
    if (looks_categorical) {
      mes <- paste0(outcome_chr, " looks categorical, so training classification algorithms.")
      model_class <- "classification"
    } else {
      mes <- paste0(outcome_chr, " looks numeric, so training regression algorithms.")
      model_class <- "regression"
      # User provided model_class, so check it
    }
    message(mes)
  } else {
    # Check user-provided model_class
    supported_classes <- get_supported_model_classes()
    if (!model_class %in% supported_classes)
      stop("Supported model classes are: ",
           paste(supported_classes, collapse = ", "),
           ". You supplied this unsupported class: ", model_class)
    if (looks_categorical && model_class == "regression") {
      stop(outcome_chr, " looks categorical but you're trying to train a regression model.")
    } else if (looks_numeric && model_class == "classification") {
      stop(outcome_chr, " looks numeric but you're trying to train a classification ",
           "model. If that's what you want convert it explicitly with as.factor().")
    }
  }
  return(model_class)
}


setup_models <- function(models) {
  available <- get_supported_models()
  unsupported <- models[!models %in% available]
  if (length(unsupported))
    stop("Currently supported algorithms are: ",
         paste(available, collapse = ", "),
         ". You supplied these unsupported algorithms: ",
         paste(unsupported, collapse = ", "))
  # We use kknn and ranger, but user input is "knn" and "rf"
  models[models == "knn"] <- "kknn"
  models[models == "rf"] <- "ranger"
  return(models)
}

set_default_metric <- function(model_class) {
  if (model_class == "regression") {
    return("RMSE")
  } else if (model_class == "classification") {
    return("ROC")
  } else {
    stop("Don't have default metric for model class", model_class)
  }
}


setup_training <- function(tune_method, n_folds, model_class, metric) {
  if (tune_method == "random") {
    train_control <-caret::trainControl(method = "cv",
                                        number = n_folds,
                                        search = "random",
                                        savePredictions = "final"
    )
  } else {
    stop("Currently tune_method = \"random\" is the only supported method",
         " but you supplied tune_method = \"", tune_method, "\"")
  }
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
