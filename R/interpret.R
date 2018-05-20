#' Interpret a model via glmnet coefficient estimates
#'
#' @param x a model_list object contain a glmnet mdoel
#' @param sparsity If NULL (default) coefficients for the best-performing model
#'   will be returned. Otherwise, a value in [0, 1] that determines the
#'   sparseness of the model for which coefficients will be returned, with 0
#'   being maximally sparse (i.e. having the fewest non-zero coefficients) and 1
#'   being minimally sparse.
#'
#' @return A data frame of variables and their regularized regression
#'   coefficient estimates
#' @details The value of alpha
#' @export
#'
#' @examples
interpret <- function(x, sparsity = NULL) {
  if (!is.model_list(x))
    stop("x must be a model_list")
  if (!"glmnet" %in% names(x))
    stop("Only glmnet models can be interpreted, and there's no glmnet model ",
         "in x. If there's a random forest, you could use get_variable_importance.")
  mi <- extract_model_info(x)
  if (mi$best_model_name != "glmnet")
    warning("Interpreting glmnet model, but ", mi$best_model_name, " performed ",
            "best in cross-validation and will be used to make predictions. ",
            "To use the glmnet model for predictions, extract it with ",
            "x['glmnet'].")
  g <- x$glmnet

  g$results

  coef(g, x$glmnet$bestTune$lambda)


}

#
# fit <- glmnet::glmnet(as.matrix(dplyr::select(mtcars, -mpg)), mtcars$mpg, alpha = 0)
#
# lambda <- unique(fit$lambda)
#
# tm <- tune_models(mtcars, mpg, models = "glm", hyperparameters = data.frame(
#   alpha = 1,
#   lambda = .001
# ))
# fm <- flash_models(mtcars, mpg, models = "glm")
# tm
# fm
# plot(predict(tm, mtcars)$predicted_mpg, predict(fm, mtcars)$predicted_mpg)
# tm$glmnet$finalModel$lambdaOpt
# fm$glmnet$finalModel$lambdaOpt
#
# plot(tm)
# tm$glmnet$finalModel$beta  # This is at the best alpha value
