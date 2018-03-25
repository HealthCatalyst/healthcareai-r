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
#'
#' @name supported_models
#' @aliases models models_supported
NULL


#############################################
##### Adjustments to stock caret models #####
#############################################

#' adjust_knn
#' @noRd
#' @return grid for kknn
#' @details This is an adjustment to caret's implementation of kknn
#'   hyperparameter search to reduce training time by reducing k_max. It is
#'   meant to be called within the lapply(models, ...) loop in tune_models.
adjust_knn <- function() {
  kn <- caret::getModelInfo("kknn")$kknn
  kn$grid <- function(x, y, len = NULL, search = "grid") {
    if(search == "grid") {
      out <- data.frame(kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0],
                        distance = 2,
                        kernel = "optimal")
    } else {
      by_val <- if(is.factor(y)) length(levels(y)) else 1
      kerns <- c("rectangular", "triangular", "epanechnikov", "biweight", "triweight",
                 "cos", "inv", "gaussian")
      # Editted line:
      out <- data.frame(kmax = sample(seq(1, floor(log(nrow(x)) * 3), by = by_val),
                                      size = len, replace = TRUE),
                        distance = stats::runif(len, min = 0, max = 3),
                        kernel = sample(kerns, size = len, replace = TRUE))
    }
    out
  }
  return(kn)
}
