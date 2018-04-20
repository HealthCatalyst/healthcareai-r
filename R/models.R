#' Supported models and their hyperparameters
#'
#' @description
#'
#' \strong{Random Forest}: "rf". Regression and classification.
#' Implemented via \code{ranger}.
#' \itemize{
#'   \item{mtry: Fraction of variables to consider for each split}
#'   \item{splitrule: Splitting rule. For classification either "gini" or
#'   "extratrees". For regression either "variance", "extratrees", or
#'   "maxstat".}
#'   \item{min.node.size: Minimal node size.}
#' }
#'
#' \strong{k-nearest neighbors}: "knn". Regression and classification.
#' Implemented via \code{kknn}.
#' \itemize{
#'   \item{kmax: Number of neighbors to consider.}
#'   \item{distance: Minkowsky distance parameter, (0, Inf). 1 = Manhatten, 2 =
#'   Euclidian, -> Inf = Chebyshev.}
#'   \item{kernal: Kernal to use. Possible choices are "rectangular" (standard
#'   knn), "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv",
#'   "gaussian", "rank", or "optimal".}
#' }
#' @export
#' @importFrom e1071 naiveBayes
#  ^ This is a placeholder. ranger needs e1071
#' @seealso \code{\link{hyperparameters}} for more detail on hyperparameter
#' defaults and specifications
#' @return Vector of currently-supported algorithms.
#' @aliases supported_models models models_supported
get_supported_models <- function() {
    return(c("rf", "knn"))
}

#' get_supported_model_classes
#' @noRd
#' @details Vector of currently-supported model classes.
get_supported_model_classes <- function() {
  return(c("regression", "classification"))
}

#' Translate user provided model specifications to caret's expectation or
#' vice-versa (which way to go is automatic)
#' @noRd
translate_model_names <- function(models) {
  key <- c(rf = "ranger", knn = "kknn")
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
      "Rsquared", "Rsquared",   max
  )
}
