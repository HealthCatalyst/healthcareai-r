#' Supported models and their hyperparameters
#'
#' @description
#'
#' \strong{Random Forest}: "RF". Regression and classification.
#' \itemize{
#'   \item{mtry: Fraction of variables to consider for each split}
#'   \item{ntree: Number of trees to grow}
#' }
#'
#' \strong{Regularized Regression}: "lasso". Regression and classification.
#' \itemize{
#'   \item{alpha: 0 ridge 1 lasso}
#'   \item{lambda: strength of regularization}
#' }
#'
#' @name supported_models
#' @aliases models models_supported
NULL
