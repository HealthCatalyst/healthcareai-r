#' Supported models and their hyperparameters
#'
#' @description
#'
#' \strong{Random Forest}: "rf". Regression and classification.
#' Implemented via \code{ranger}.
#' \itemize{
#'   \item{mtry: Number of variables to consider for each split}
#'   \item{splitrule: Splitting rule. For classification either "gini" or
#'   "extratrees". For regression either "variance" or "extratrees".}
#'   \item{min.node.size: Minimal node size.}
#' }
#'
#' \strong{XGBoost}: "xgb". eXtreme Gradient Boosting
#' Implemented via \code{xgboost}. Note that XGB has many more hyperparameters
#' than the other models. Because of this, it may require greater tune_depth
#' to optimize performance.
#' \itemize{
#'   \item{eta: Control for learning rate, [0, 1]}
#'   \item{gamma: Threshold for further cutting of leaves, [0, Inf].
#'   Larger is more conservative.}
#'   \item{max_depth: Maximum tree depth, [0, Inf]. Larger means more complex
#'   models and so greater likelihood of overfitting. 0 produces no limit on depth.}
#'   \item{subsample: Fraction of data to use in each training instance, (0, 1].}
#'   \item{colsample_bytree: Fraction of features to use in each tree, (0, 1].}
#'   \item{min_child_weight: Minimum sum of instance weight need to keep partitioning,
#'   [0, Inf]. Larger values mean more conservative models.}
#'   \item{nrounds: Number of rounds of boosting, [0, Inf). Larger values produce
#'   a greater likelihood of overfitting.}
#' }
#'
#' \strong{Regularized regression}: "glm". Regression and classification.
#' Implemented via \code{glmnet}.
#' \itemize{
#'   \item{alpha: Elasticnet mixing parameter, in [0, 1]. 0 = ridge regression;
#'   1 = lasso.}
#'   \item{lambda: Regularization parameter, > 0. Larger values make for
#'   stronger regularization.}
#' }
#' @export
#' @importFrom ranger ranger
#' @importFrom glmnet glmnet
#' @importFrom xgboost xgb.train
#' @importFrom e1071 naiveBayes
#  ^ This is a placeholder. ranger needs e1071
#' @seealso \code{\link{hyperparameters}} for more detail on hyperparameter
#' defaults and specifications
#' @return Vector of currently-supported algorithms.
#' @aliases supported_models models models_supported
get_supported_models <- function() {
    return(c("rf", "xgb", "glm"))
}

#' get_supported_model_classes
#' @noRd
#' @details Vector of currently-supported model classes.
get_supported_model_classes <- function() {
  return(c("regression", "classification", "multiclass"))
}

#' Translate user provided model specifications to caret's expectation or
#' vice-versa (which way to go is automatic)
#' @noRd
translate_model_names <- function(models) {
  key <- c(rf = "ranger", xgb = "xgbTree", glm = "glmnet")
  if (all(models %in% key))
    key <- structure(names(key), names = key)
  return(unname(key[models]))
}

#' Get dataframe to translate caret's metric names to ours as well as goal
#' function associated with each metric.
#' @noRd
get_metric_names <- function() {
  tibble::tribble(
    ~ caret,    ~ ours,       ~ goal,
      "ROC",      "AUROC",      max,
      "AUC",      "AUPR",       max,
      "RMSE",     "RMSE",       min,
      "MAE",      "MAE",        min,
      "Rsquared", "Rsquared",   max,
      "Accuracy", "Accuracy",   max,
      "Kappa",    "Kappa",      max
  )
}
