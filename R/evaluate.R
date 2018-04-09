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

#' Get performance metrics for classification models
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
evaluate_classification <- function(predicted, actual) {
  c("AUPR" = MLmetrics::PRAUC(y_pred = predicted, y_true = actual),
    "AUROC" = MLmetrics::AUC(y_pred = predicted, y_true = actual)
  )
}

evaluate_regression <- function(predicted, actual) {

}

