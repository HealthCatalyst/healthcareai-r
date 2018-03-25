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

#' get_hyperparameter_defaults
#' @noRd
#' @details Get default hyperparameters to use, e.g. in flash_models. list
#'   (algorithms) of lists (hyperparameters).
get_hyperparameter_defaults <- function(models = get_supported_models()) {
  defaults <-
    list(
      rf = list(
        mtry = 5,
        splitrule = "extratrees",
        min.node.size = 10),
      knn = list(
        kmax = 10,
        distance = 2,
        kernel = "gaussian"
      )
    )
  defaults[models]
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

#############################################
##### Adjustments to stock caret models #####
#############################################

# nolint start

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

# nolint end
