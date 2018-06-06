#' Interpret a model via regularized coefficient estimates
#'
#' @param x a model_list object containing a glmnet mdoel
#' @param sparsity If NULL (default) coefficients for the best-performing model
#'   will be returned. Otherwise, a value in [0, 1] that determines the
#'   sparseness of the model for which coefficients will be returned, with 0
#'   being maximally sparse (i.e. having the fewest non-zero coefficients) and 1
#'   being minimally sparse.
#' @param remove_zeros Remove features with coefficients equal to 0? Default is
#'   TRUE.
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
interpret <- function(x, sparsity = NULL, remove_zeros = TRUE) {
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
    lambda <- g$lambdaOpt
    coefs <- coef(g, s = lambda)
  } else {
    if (!is.numeric(sparsity) || sparsity < 0 || sparsity > 1)
      stop("sparsity must be a numeric value between 1 and 100")
    j <- round(quantile(seq_len(ncol(coefs)), sparsity), 0)
    coefs <- coef(g)[, j, drop = FALSE]
    lambda <- g$lambda[j]
  }
  coefs <-
    tibble::tibble(variable = rownames(coefs), coefficient = coefs[, 1]) %>%
    dplyr::arrange(desc(variable == "(Intercept)"), desc(abs(coefficient)))

  if (remove_zeros)
    coefs <- dplyr::filter(coefs, coefficient != 0)

  structure(coefs,
            class = c("coefs", class(coefs)),
            m_class = mi$m_class,
            target = mi$target,
            lambda = lambda,
            alpha = mi$best_model_tune$alpha)
}

plot.interpret <- function(x, include_intercept = FALSE, remove_zeros = FALSE,
                           title = NULL, font_size = 11, point_size = 3,
                           print = TRUE, caption, ... ) {

  if ( (is.data.frame(x) && names(x) != c("variable", "coefficient") ) ||
       !is.data.frame(x))
    stop("x must be a data frame from interpret, or at least look like one!")

  if (remove_zeros)
    x <- dplyr::filter(x, coefficient != 0, variable != "(Intercept)")

  if (!include_intercept)
    x <- dplyr::filter(x, variable != "(Intercept)")

  if (missing(caption))
    caption <-

  limits <- max(abs(x$coefficient)) * c(-1.05, 1.05)
  # the_plot <-
  x %>%
    ggplot(aes(x = reorder(variable, coefficient), y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .6) +
    geom_point(size = point_size) +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = "Coefficient Estimate",
                       limits = limits) +
    ggtitle(title) +
    theme_gray(base_size = font_size)

  if (print)
    print(the_plot)
  return(invisible(the_plot))

}
