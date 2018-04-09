#' Get performance metrics from prediction data frame
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
evaluate.hcai_predicted_df <- function(df) {

  # if observed values not present, stop
  # Dispatch passing vectors
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
