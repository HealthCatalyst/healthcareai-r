#' Identify the best performing model by tuning hyperparameters via
#' cross-validation
#'
#' @param d A data frame
#' @param outcome Name of the column to predict
#' @param model_class One of "regression", "classification", "multiclass", or
#'   "unsupervised", but only regression and classification are currently
#'   supported.
#' @param models Names of models to try, by default for regression and
#'   classification "rf" for random forest and "knn" for k-nearest neighbors.
#'   See \code{\link{supported_models}} for available models.
#' @param n_folds How many folds to use in cross-validation? Default = 5.
#' @param tune_depth How many hyperparameter combinations to try? Defualt = 10.
#' @param tune_method How to search hyperparameter space? Only "random" is
#'   currently supported. Eventually, "random" (default) or "grid".
#' @param metric What metric to use to assess model performance? Options for
#'   regression: "RMSE" (root-mean-squared error, default), "MAE" (mean-absolute
#'   error), or "Rsquared." For classification: "ROC" (area under the receiver
#'   operating characteristic curve).
#' @param hyperparameters Currently not supported. Optional. A list of
#'   hyperparameter values to tune over. Overrides \code{tune_depth}. A list of
#'   lists. The names of the outer-list must match \code{models}. The names of
#'   each inner-list must match the hyperparameters available to tune over for
#'   the respective model. Entries in each inner-list are the values of the
#'   hyperparameter to try. These will be expanded to run a full grid search
#'   over every combination of values. For details on support models and
#'   hyperparameters see \code{\link{supported_models}}.
#'
#' @return A model_list object
#' @export
#'
#' @importFrom kknn kknn
#' @importFrom ranger ranger
#'
#' @details Note that in general a model is trained for each hyperparameter
#'   combination in each fold for each model, so run time is a function of
#'   length(models) x n_folds x tune_depth.
#'
#' @examples
#' \dontrun{
#' ### Takes ~7 seconds
#' # Remove identifier variables and rows with missingness,
#' # and choose 100 rows to speed tuning
#' d <- dplyr::select(pima_diabetes, -PatientID)
#' d <- stats::na.omit(d)
#' d <- dplyr::sample_n(d, 100)
#' m <- tune(d, outcome = Diabetes, model_class = "classification")
#' # Plot performance over hyperparameter values for random forest
#' ggplot2::ggplot(m$rf)
#' # Extract confusion matrix for KNN
#' caret::confusionMatrix(m$knn, norm = "none")
#' # Compare performance of algorithms at best hyperparameter values
#' rs <- resamples(m)
#' dotplot(rs)
#' }
tune <- function(d,
                        outcome,
                        model_class,
                        models = c("rf", "knn"),
                        n_folds = 5,
                        tune_depth = 10,
                        tune_method = "random",
                        metric,
                        hyperparameters) {

  # Organize arguments and defaults
  outcome <- rlang::enquo(outcome)
  # Make sure outcome's class works with model_class, or infer it
  outcome_class <- class(dplyr::pull(d, !!outcome))
  looks_categorical <- outcome_class %in% c("character", "factor", "logical")
  looks_numeric <- is.numeric(dplyr::pull(d, !!outcome))
  if (!looks_categorical && !looks_numeric) {
    # outcome is weird class
    stop(rlang::quo_name(outcome), " is ", class(dplyr::pull(d, !!outcome)),
         ", and tune doesn't know what to do with that.")
  } else if (missing(model_class)) {
    # Need to infer model_class
    if (looks_categorical) {
      message(rlang::quo_name(outcome),
              " looks categorical, so training classification algorithms.")
      model_class <- "classification"
    } else {
      message(rlang::quo_name(outcome),
              " looks numeric, so training regression algorithms.")
      model_class <- "regression"
      # User provided model_class, so check it
    }
  } else {
    # Check user-provided model_class
    supported_classes <- c("regression", "classification")
    if (!model_class %in% supported_classes)
      stop("Supported model classes are: ",
           paste(supported_classes, collapse = ", "),
           ". You supplied this unsupported class: ", model_class)
    if (looks_categorical && model_class == "regression") {
      stop(rlang::quo_name(outcome), " is ", outcome_class, " but you're ",
           "trying to train a regression model.")
    } else if (looks_numeric && model_class == "classification") {
      stop(rlang::quo_name(outcome), " is ", outcome_class, " but you're ",
           "trying to train a classification model. If that's what you want ",
           "convert it explicitly with as.factor().")
    }
  }
  # Choose metric if not provided
  if (missing(metric)) {
    metric <-
      if (model_class == "regression") {
        "RMSE"
      } else if (model_class == "classification") {
        "ROC"
      }
  }
  # Make sure models are supported
  available <- c("rf", "knn")
  unsupported <- models[!models %in% available]
  if (length(unsupported))
    stop("Currently supported algorithms are: ",
         paste(available, collapse = ", "),
         ". You supplied these unsupported algorithms: ",
         paste(unsupported, collapse = ", "))
  # We use kknn and ranger, but user input is "knn" and "rf"
  provided_models <- models  # Keep user names to name model_list
  models[models == "knn"] <- "kknn"
  models[models == "rf"] <- "ranger"

  # Set up cross validation details
  if (tune_method == "random") {
    train_control <-
      caret::trainControl(method = "cv",
                          number = n_folds,
                          search = "random",
                          savePredictions = "final"
      )
  } else {
    stop("Currently tune_method = \"random\" is the only supported method",
         " but you supplied tune_method = \"", tune_method, "\"")
  }

  # trainControl defaults are good for regression. Change for other model_class:
  if (model_class == "classification") {
    train_control$summaryFunction <- twoClassSummary  # nolint
    train_control$classProbs <- TRUE  # nolint
  }

  # Loop over models, tuning each
  train_list <-
    lapply(models, function(model) {
      message("Running cross validation for ", model)
      caret::train(x = dplyr::select(d, -!!outcome),
                   y = dplyr::pull(d, !!outcome),
                   method = model,
                   metric = metric,
                   trControl = train_control,
                   tuneLength = tune_depth
      )
    })
  # Add model names
  names(train_list) <- provided_models

  # Add classes
  train_list <- as.model_list(listed_models = train_list, type = model_class)

  return(train_list)
}
