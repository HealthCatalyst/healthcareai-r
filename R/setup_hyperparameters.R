#' Get default hyperparameter lists that can be passed to flash_models
#'
#' @param models which algorithms?
#' @param n Number observations
#' @param k Number features
#' @param model_class "classification" or "regression"
#'
#' @return List of named data frames that can be passed to
#'   \code{\link{flash_models}}
#' @export
#' @details Get default hyperparameters to use, e.g. in flash_models. Each data
#'   frame in the list corresponds to an algorithm, and each column in each data
#'   fram corresponds to a hyperparameter for that algorithm.
#'
#'   k-NN defaults are from the kknn package: kmax = 7, distance = 2
#'   (Minkowski's exponent, i.e. Euclidean distance), kernal = "optimal"
#'
#'   Random forest defaults are from Intro to Statistical Learning and caret:
#'   mtry = sqrt(k), splitrule = "extratrees", min.node.size = 1 for
#'   classification, 5 for regression
get_hyperparameter_defaults <- function(models = get_supported_models(),
                                        n = 100,
                                        k = 10,
                                        model_class = "classification") {
  defaults <-
    list(
      rf = tibble::tibble(
        mtry = floor(sqrt(k)),
        splitrule = "extratrees",
        min.node.size = if (model_class == "classification") 1L else 5L),
      knn = data.frame(
        kmax = 7,
        distance = 2,
        kernel = "optimal"
      )
    )
  return(defaults[models])
}

#' Get grids of random combos of hyperparameters for train
#'
#' @param models which algorithms?
#' @param n Number observations
#' @param k Number features
#' @param tune_depth How many combinations of hyperparameter values?
#' @param model_class "classification" or "regression"
#'
#' @importFrom stats runif
#' @return List of named data frames
#' @noRd
get_random_hyperparameters <- function(models = get_supported_models(),
                                       n = 100,
                                       k = 10,
                                       tune_depth = 5,
                                       model_class = "classification") {
  replace_ks <- k < tune_depth
  grids <- list()
  if ("rf" %in% models) {
    split_rules <-
      if (model_class == "classification") {
        c("gini", "extratrees")
      } else {
        c("variance", "extratrees", "maxstat")
      }
    grids$rf <-
      tibble::tibble(
        mtry = sample(seq_len(k), tune_depth, TRUE, prob = 1 / seq_len(k) ^ 1.5),
        splitrule = sample(split_rules, tune_depth, TRUE),
        min.node.size = sample(min(n, 20), tune_depth, TRUE)
      )
  }
  if ("knn" %in% models) {
    kmax_limit <- min(10, n)
    grids$knn <-
      tibble::tibble(
        kmax = sample(kmax_limit, tune_depth, TRUE),
        distance = runif(tune_depth, 0, 3),
        kernel = sample(c("rectangular", "epanechnikov", "triweight",
                          "cos", "gaussian", "optimal"), tune_depth, TRUE)
      )
  }
  return(grids)
}
