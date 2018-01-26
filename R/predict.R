#' Make predictions
#'
#' @param models model_list object, as from `tune_models`
#' @param newdata data frame with same structure as the input to `tune_models`
#'   that generated `models`. If the data that models were tuned on was
#'   prepaired via `prep_data`, newdata will be prepared the same way, unless
#'   prepdata is FALSE.
#' @param prepdata Logical, rarely needs to be set by the user. By default, if
#'   `newdata` hasn't been prepped, it will be prepped by `prep_data` before
#'   predictions are made. Set this to TRUE to force already-prepped data
#'   through `prep_data` again, or set to FALSE to prevent `newdata` from being
#'   sent through `prep_data`.
#' @param ... Passed to `predict.regression_list` or
#'   `predict.classification_list`
#'
#' @return A tibble data frame: newdata with an additional column for the
#'   predictions in "predicted_TARGET" where TARGET is the name of the variable
#'   being predicted. The tibble will have child class "hcai_predicted_df" and
#'   attribute "model_info" that contains information about the model used to
#'   make predictions.
#' @export
#'
#' @details prepping data inside `predict` has the advantage of returning your
#'   predictions with the data frame in its original (unprepped) format. To do
#'   this, simply pass the data frame on which predictions are to be generated
#'   to `newdata` in the same format as the training data was passed to
#'   `prep_data` or `train_models`.
#'
#' @examples
predict.model_list <- function(models, newdata, prepdata, ...) {
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")
  mi <- extract_model_info(models)
  # If prepdata provided by user; follow that. Else, prep if newdata hasn't been
  prep <-
    if (!missing(prepdata)) {
      prepdata
    } else {
      !inherits(newdata, "hcai_prepped_df")
    }
  newdata[[paste0("predicted_", mi$target)]] <-
    if (prep) {
      prep_data(newdata, rec_obj = attr(models, "rec_obj")) %>%
        predict(models[[mi$best_model_name]], ., type = "raw")
    } else {
      newdata %>%
        predict(models[[mi$best_model_name]], ., type = "raw")
    }
  newdata <- tibble::as_tibble(newdata)
  class(newdata) <- c("hcai_predicted_df", class(newdata))
  attr(newdata, "model_info") <-
    list(target = mi$target,
         algorithm = mi$best_model_name,
         performance_metric = mi$metric,
         hyperparameters = structure(mi$best_model_tune,
                                     "row.names" = "optimal:"))
  return(newdata)
}
