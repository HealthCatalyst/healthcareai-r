#' Get performance metrics from predictions data frame
#'
#' @param d Data frame from \code{\link{predict.model_list}} containing realized
#'   outcomes
#'
#' @return Vector of scores with metrics as names
#' @export
#' @details This function is designed to work with predictions coming from
#'   \code{\link{machine_learn}} / \code{\link{tune_models}} /
#'   \code{\link{flash_models}} through \code{\link{predict.model_list}}. The
#'   data passed to \code{predict} must contain observed outcomes. If you have
#'   predictions and outcomes in a different format, use
#'   \code{\link{evaluate_classification}} or \code{\link{evaluate_regression}}.
#'
#' @examples
#' models <- machine_learn(pima_diabetes[1:40, ], patient_id, outcome = diabetes,
#'                         models = "rf", tune_depth = 3)
#' predictions <- predict(models, newdata = pima_diabetes[41:50, ])
#' evaluate(predictions)
evaluate <- function(d) {
  outcome <- attr(d, "model_info")[["target"]]
  if (!outcome %in% names(d))
    stop("Outcome variable: ", outcome, " not found in d. You must have actual outcomes in ",
         "the data passed to predict to use evaluate. If the outcomes are ",
         "somewhere else, consider using ",
         paste0("evaluate_", tolower(attr(d, "model_info")[["type"]])))

  obs <- d[[outcome]]
  pred <- d[[paste0("predicted_", outcome)]]

  if (attr(d, "model_info")[["type"]] == "Regression") {
    scores <- evaluate_regression(predicted = pred, actual = obs)
  } else if (attr(d, "model_info")[["type"]] == "Classification") {
    obs <- ifelse(obs == attr(d, "positive_class"), 1L, 0L)
    scores <- evaluate_classification(predicted = pred, actual = obs)
  } else {
    stop("Somthing's gone wrong. I don't know how to deal with model type ",
         attr(d, "model_info")[["type"]])
  }
  return(scores)
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
#' @importFrom MLmetrics RMSE
#' @importFrom MLmetrics MAE
#' @importFrom MLmetrics R2_Score
#'
#' @return Numeric vector of scores with metric as names
#' @export
#'
#' @examples
#' evaluate_regression(c(2, 4, 6), c(1.5, 4.1, 6.2))
evaluate_regression <- function(predicted, actual) {
  c("RMSE" = MLmetrics::RMSE(y_pred = predicted, y_true = actual),
    "MAE" = MLmetrics::MAE(y_pred = predicted, y_true = actual),
    "Rsquared" = MLmetrics::R2_Score(y_pred = predicted, y_true = actual)
  )
}
