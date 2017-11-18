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
#' @param tune_method How to search hyperparameter space? One of "random"
#' (default) or "grid".
#' @param metric What metric to use to assess model performance? Defaults to
#' RMSE for model_class = regression and AUROC for model_class = classification.
#' @param hyperparameters Optional, overrides \code{tune_depth} and
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
#' @details Note that in general a model is trained for each hyperparameter
#' combination in each fold for each model, so run time is a function of
#' length(models) x n_folds x tune_depth.
#'
#' @examples
#' tune()
tune <- function(d,
                 outcome,
                 model_class,
                 models = c("RF", "lasso"),
                 n_folds = 5,
                 tune_depth = 10,
                 tune_method = "random",
                 metric,
                 hyperparameters) {

  outcome <- rlang::enquo(outcome)

  if (missing(metric)) {
    metric <-
      if (model_class == "regression") {
        "RMSE"
      } else if (model_class == "classification") {
        "AUROC"
      }
  }

  if (missing(hyperparameters)) {
    # hyperparameters <- create_hyperparameters(models, tune_depth, n_var = ncol(d) - 1)
    hyperparameters <- list(rf = list(mtry = floor(c())))
  }

  if (tune_method = "grid") {
    tune_grid <- do.call(expand.grid, hyperparameters)
  }

  train_control <-
    caret::trainControl(method = "cv",
                        number = n_folds,
                        summaryFunction = twoClassSummary,
                        search = tune_method)

  train_list <-
    lapply(models, function(model) {

      caret::train(x = dplyr::select(d, !!outcome),
                   y = dplyr::pull(d, !!outcome),
                   method = "rf",
                   tuneLength = tune_depth,
                   metric = metric,
                   ntree = c(50, 100)
      )


  })

}
