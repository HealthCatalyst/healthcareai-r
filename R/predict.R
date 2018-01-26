#' Make predictions
#'
#' @param models model_list object, as from `tune_models`
#' @param newdata data frame with same structure as the input to `tune_models`
#'   that generated `models`. If the data that models were tuned on was
#'   prepaired via `prep_data`, newdata will be prepared the same way, unless
#'   prepdata is FALSE.
#' @param prepdata By default, if and only if `newdata` hasn't been prepped, it
#'   will be before predictions are made, so this argument doesn't need to be
#'   set by the user. Set this to TRUE to force already-prepped data through
#'   `prep_data` again, or set to FALSE to prevent `newdata` from being sent
#'   through `prep_data`.
#' @param ... Passed to `predict.regression_list` or
#'   `predict.classification_list`
#'
#' @return Data frame of predictions
#' @export
#'
#' @examples
predict.model_list <- function(models, newdata, prepdata, ...) {
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")
  if (isTRUE(prepdata) || !inherits(newdata, "hcai_prepped_df"))
    newdata <- prep_data(newdata, rec_obj = attr(models, "rec_obj"))
  NextMethod(models, newdata, ...)
}

predict.regression_list <- function(models, newdata, ...) {



}

predict.classification_list <- function(models, newdata, ...) {

}
