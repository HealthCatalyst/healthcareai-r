#' Interpret a model via regularized coefficient estimates
#'
#' @param x a model_list object containing a glmnet mdoel
#' @param sparsity If NULL (default) coefficients for the best-performing model
#'   will be returned. Otherwise, a value in [0, 1] that determines the
#'   sparseness of the model for which coefficients will be returned, with 0
#'   being maximally sparse (i.e. having the fewest non-zero coefficients) and 1
#'   being minimally sparse.
#'
#' @return A data frame of variables and their regularized regression
#'   coefficient estimates.
#'
#' @details If x was trained with more than one value of alpha the best value of
#'   alpha is used; sparsity is determined only via the selection of lambda.
#'
#'   Coefficients are on the scale of the predictors; they are not standardized,
#'   so unless features were scaled before training (e.g. with
#'   \code{prep_data(..., scale = TRUE)}, the magnitude of coefficients does not
#'   necessarily reflect their importance.
#' @export
#'
#' @examples
#' m <- machine_learn(pima_diabetes, patient_id, outcome = age, models = "glm")
#' interpret(m)
#' interpret(m, .2)
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

  g <- x$glmnet$finalModel
  coefs <- coef(g)

  # If user didn't specify sparsity, use best lambda
  if (is.null(sparsity)) {
    coefs <- coef(g, s = g$lambdaOpt)
  } else {
    if (!is.numeric(sparsity) || sparsity < 0 || sparsity > 1)
      stop("sparsity must be a numeric value between 1 and 100")
    lam <- round(quantile(seq_len(ncol(coefs)), sparsity), 0)
    coefs <- coef(g)[, lam, drop = FALSE]
  }
  coefs <-
    tibble::tibble(variable = rownames(coefs), coefficient = coefs[, 1]) %>%
    dplyr::arrange(desc(variable == "(Intercept)"), desc(abs(coefficient)))
  structure(coefs, class = c("coefs", class(coefs)))
}
