#'Get hyperparameter values
#'
#'@param models which algorithms?
#'@param n Number observations
#'@param k Number features
#'@param model_class "classification" or "regression"
#'
#'@return Named list of data frames. Each data frame corresponds to an
#'  algorithm, and each column in each data fram corresponds to a hyperparameter
#'  for that algorithm. This is the same format that should be provided to
#'  \code{tune_models(hyperparameters = )} to specify hyperparameter values.
#'
#'@export
#'@aliases hyperparameters
#'@seealso \code{\link{models}} for model and hyperparameter details
#'@details Get hyperparameters for model training.
#'  \code{get_hyperparameter_defaults} returns a list of 1-row data frames
#'  (except for glm, which is a 10-row data frame) with default hyperparameter
#'  values that are used by \code{flash_models}.
#'  \code{get_random_hyperparameters} returns a list of data frames with
#'  combinations of random values of hyperparameters to tune over in
#'  \code{tune_models}; the number of rows in the data frames is given by
#'  `tune_depth`.
#'
#'  For \code{get_hyperparameter_defaults}
#'  XGBoost defaults are from caret and XGBoost documentation:
#'  eta = 0.3, gamma = 0, max_depth = 6, subsample = 0.7,
#'  colsample_bytree = 0.8, min_child_weight = 1, and nrounds = 50.
#'  Random forest defaults are from Intro to
#'  Statistical Learning and caret: mtry = sqrt(k), splitrule = "extratrees",
#'  min.node.size = 1 for classification, 5 for regression.
#'  glm defaults are
#'  from caret: alpha = 1, and because glmnet fits sequences of lambda nearly as
#'  fast as an individual value, lambda is a sequence from 1e-4 to 8.
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
      xgb = tibble::tibble(
        eta = .3,
        gamma = 0,
        max_depth = 6,
        subsample = .7,
        colsample_bytree = .8,
        min_child_weight = 1,
        nrounds = 50
      ),
      # For glmnet, fitting 10 lambdas is only ~30% slower than an individual
      # value, and it's so important for performance, so go ahead and fit 10
      glm = tibble::tibble(
          alpha = 1,
          lambda = 2 ^ seq(-10, 3, len = 10)
        )
    )
  return(defaults[models])
}

#' @param tune_depth How many combinations of hyperparameter values?
#' @export
#' @importFrom stats runif
#' @rdname get_hyperparameter_defaults
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
        c("variance", "extratrees")
      }
    grids$rf <-
      tibble::tibble(
        mtry = sample(seq_len(k), tune_depth, TRUE, prob = 1 / seq_len(k)),
        splitrule = sample(split_rules, tune_depth, TRUE),
        min.node.size = sample(min(n, 20), tune_depth, TRUE)
      )
  }
  if ("xgb" %in% models) {
    grids$xgb <-
      tibble::tibble(
        eta = runif(tune_depth, 0.001, .5),
        gamma = runif(tune_depth, 0, 10),
        max_depth = sample(10, tune_depth, replace = TRUE),
        subsample = runif(tune_depth, .35, 1),
        colsample_bytree = runif(tune_depth, .5, .9),
        min_child_weight = stats::rexp(tune_depth, .2),
        nrounds = sample(25:1000, tune_depth, prob = 1 / (25:1000))
      )
  }
  if ("glm" %in% models) {
    grids$glm <-
      expand.grid(
        alpha = c(0, 1),
        lambda = 2 ^ runif(tune_depth, -10, 3)
      ) %>%
      dplyr::arrange(alpha) %>%
      tibble::as_tibble()
  }
  return(grids)
}
