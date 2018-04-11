#' Get model performance metrics
#'
#' @param x Object to be evaluted
#' @param ... Not used
#'
#' @export
#' @rdname evaluate
#'
#' @details This function gets model performance from a model_list object that
#'   comes from \code{\link{machine_learn}}, \code{\link{tune_models}},
#'   \code{\link{flash_models}}, or a data frame of predictions from
#'   \code{\link{predict.model_list}}. For the latter, the data passed to
#'   \code{predict.model_list} must contain observed outcomes. If you have
#'   predictions and outcomes in a different format, see
#'   \code{\link{evaluate_classification}} or \code{\link{evaluate_regression}}
#'   instead.
#'
#'   You may notice that \code{evaluate(models)} and
#'   \code{evaluate(predict(models))} return slightly different performance
#'   metrics, even though they are being calculated on the same (out-of-fold)
#'   predictions. This is because metrics in training (returned from
#'   \code{evaluate(models)}) are calculated within each cross-validation fold
#'   and then averaged, while metrics calculated on the prediction data frame
#'   (\code{evaluate(predict(models))}) are calculated once on all observations.
#'
#' @examples
#' models <- machine_learn(pima_diabetes[1:40, ],
#'                         patient_id,
#'                         outcome = diabetes,
#'                         models = "rf",
#'                         tune_depth = 3)
#' evaluate(models)
#' predictions <- predict(models, newdata = pima_diabetes[41:50, ])
#' evaluate(predictions)
evaluate <- function(x, ...) {
  UseMethod("evaluate")
}

#' @export
#' @rdname evaluate
evaluate.predicted_df <- function(x, ...) {
  outcome <- attr(x, "model_info")[["target"]]
  if (!outcome %in% names(x))
    stop("Outcome variable: ", outcome, " not found in d. You must have actual outcomes in ",
         "the data passed to predict to use evaluate. If the outcomes are ",
         "somewhere else, consider using ",
         paste0("evaluate_", tolower(attr(x, "model_info")[["type"]])))

  obs <- x[[outcome]]
  pred <- x[[paste0("predicted_", outcome)]]

  if (attr(x, "model_info")[["type"]] == "Regression") {
    scores <- evaluate_regression(predicted = pred, actual = obs)
  } else if (attr(x, "model_info")[["type"]] == "Classification") {
    obs <- ifelse(obs == attr(x, "model_info")$positive_class, 1L, 0L)
    scores <- evaluate_classification(predicted = pred, actual = obs)
  } else {
    stop("Somthing's gone wrong. I don't know how to deal with model type ",
         attr(x, "model_info")[["type"]])
  }
  return(scores)
}

#' @export
#' @rdname evaluate
evaluate.model_list <- function(x, ...) {
  if (!length(x))
    stop("Can't evaluate an empty model_list")
  x <- change_metric_names(x)
  mi <- extract_model_info(x)
  mod <- mi$best_model_name
  if (mi$m_class == "Regression") {
    f <- evaluate_regression
    pred_col <- "pred"
  } else if (mi$m_class == "Classification") {
    f <- evaluate_classification
    pred_col <- mi$positive_class
    x[[mod]]$pred$obs <- ifelse(x[[mod]]$pred$obs == mi$positive_class, 1L, 0L)
  }
  split(x[[mod]]$pred, x[[mod]]$pred$Resample) %>%
    purrr::map_df(~ f(predicted = .x[[pred_col]], actual = .x$obs) %>%
                    dplyr::bind_rows()) %>%
    colMeans()
}

#' Get performance metrics for classification predictions
#'
#' @param predicted Vector of predicted probabilities
#' @param actual Vector of realized outcomes, must be 0/1
#'
#' @importFrom MLmetrics PRAUC
#' @importFrom MLmetrics AUC
#'
#' @return Numeric vector of scores with metric as names
#' @export
#'
#' @examples
#' evaluate_classification(c(.7, .1, .6, .9, .4), c(1, 0, 0, 1, 1))
evaluate_classification <- function(predicted, actual) {
  c("AUPR" = MLmetrics::PRAUC(y_pred = predicted, y_true = actual),
    "AUROC" = MLmetrics::AUC(y_pred = predicted, y_true = actual)
  )
}

#' Get performance metrics for regression predictions
#'
#' @param predicted Vector of predicted values
#' @param actual Vector of realized values
#'
#' @return Numeric vector of scores with metric as names
#' @export
#'
#' @examples
#' evaluate_regression(c(2, 4, 6), c(1.5, 4.1, 6.2))
evaluate_regression <- function(predicted, actual) {
  c("RMSE" = caret::RMSE(pred = predicted, obs = actual),
    "MAE" = caret::MAE(pred = predicted, obs = actual),
    "Rsquared" = caret::R2(pred = predicted, obs = actual)
  )
}
