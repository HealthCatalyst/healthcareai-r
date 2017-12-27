#' Identify the best performing model by tuning hyperparameters via
#' cross-validation
#'
#' @param d A data frame
#' @param outcome Name of the column to predict
#' @param model_class One of "regression", "classification",
#' "multiclass", or "unsupervised", but only regression and classification are
#' being implimented initially.
#' @param models Names of models to try, by default for regression and
#' classification "RF" for random forest and "lasso" for regularized regression.
#' See \code{\link{supported_models}} for available models.
#' @param n_folds How many folds to use in cross-validation? Default = 5.
#' @param tune_depth How many hyperparameter combinations to try? Defualt = 10.
#' @param tune_method How to search hyperparameter space? Only "random" is
#' currently supported. Eventually, "random" (default) or "grid".
#' @param metric What metric to use to assess model performance? Defaults to
#' RMSE for model_class = regression and AUROC for model_class = classification.
#' @param hyperparameters Optional, a list of overrides \code{tune_depth} and
#' \code{tune_method} if provided. A list of lists. The names of the outer-list
#' must match \code{models}. The names of each inner-list must match the
#' hyperparameters available to tune over for the respective model. Entries
#' in each inner-list are the values of the hyperparameter to try. These will
#' be expanded to run a full grid search over every combination of values. For
#' details on support models and hyperparameters see
#' \code{\link{supported_models}}.
#'
#' @return A model_list object
#' @export
#'
#' @importFrom kknn kknn
#' @importFrom randomForest randomForest
#'
#' @details Note that in general a model is trained for each hyperparameter
#' combination in each fold for each model, so run time is a function of
#' length(models) x n_folds x tune_depth.
#'
tune_models <- function(d,
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
         ".\nNot supported: ", paste(unsupported, collapse = ", "))
  # We use kknn and ranger, but user input is "knn" and "rf
  models[models == "knn"] <- "kknn"
  models[models == "rf"] <- "ranger"
  # Make sure outcome's class works with model_class
  outcome_class <- class(dplyr::pull(d, !!outcome))
  if (outcome_class %in% c("character", "factor") &&
      model_class == "regression") {
    stop(rlang::quo_name(outcome), " is ", outcome_class, " but you're ",
         "trying to train a regression model.")
  } else if (is.numeric(dplyr::pull(d, !!outcome)) &&
             model_class == "classification") {
    stop(rlang::quo_name(outcome), " is ", outcome_class, " but you're ",
         "trying to train a classification model. If that's what you want ",
         "convert it explicitly with as.factor().")
  }

  if (tune_method == "random") {
    train_control <-
      caret::trainControl(method = "cv",
                          number = n_folds,
                          search = "random"
      )
  } else {
    stop("Currently tune_method = \"random\" is the only supported method",
         " but you supplied tune_method = \"", tune_method, "\"")
    #### Not needed for only random search
    # if (missing(hyperparameters)) {
    #   # hyperparameters <- create_hyperparameters(models, tune_depth, n_var = ncol(d) - 1)
    #   hyperparameters <- list(rf = list(mtry = floor(c())))
    # }

    # if (tune_method == "grid") {
    #   tune_grid <- do.call(expand.grid, hyperparameters)
    #   train_control <-
    #     caret::trainControl(method = "cv",
    #                         number = n_folds,
    #                         search = "grid"
    #                         )
    #   #### Grid is provided in train
    #
    # }
  }

  # trainControl defaults are good for regression. Change for other model_class
  if (model_class == "classification") {
    train_control$summaryFunction <- twoClassSummary  # nolint
    train_control$classProbs <- TRUE  # nolint
  }

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

  class(train_list) <- c(paste0(model_class, "_list"),
                         "model_list",
                         class(train_list))
  return(train_list)
}
