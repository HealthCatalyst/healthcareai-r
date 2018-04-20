#' Make predictions using the best-performing model
#'
#' @param object model_list object, as from `tune_models`
#' @param newdata data on which to make predictions. If missing, out-of-fold
#'   predictions from training will be returned If you want new predictions on
#'   training data using the final model, pass the training data to this
#'   argument, but know that you're getting over-fit predictions that very
#'   likely overestimate model performance relative to what will be achieved on
#'   new data. Should have the same structure as the input to
#'   `prep_data`,`tune_models` or `train_models`. `predict` will try to figure
#'   out if the data need to be sent through `prep_data` before making
#'   predictions; this can be overriden by setting `prepdata = FALSE`, but this
#'   should rarely be needed.
#' @param prepdata Logical, this should rarely be set by the user. By default,
#'   if `newdata` hasn't been prepped, it will be prepped by `prep_data` before
#'   predictions are made. Set this to TRUE to force already-prepped data
#'   through `prep_data` again, or set to FALSE to prevent `newdata` from being
#'   sent through `prep_data`.
#' @param write_log Write prediction metadata to a file? Default is FALSE. If
#'   TRUE, will create or append-at-top a file called "prediction_log.txt" in
#'   the current directory with metadata about predictions. If a character, is
#'   the name of a file to create or append with prediction metadata. If you
#'   want a unique log file each time predictions are made, use something like
#'   \code{write_log = paste0(Sys.time(), " predictions.txt")}.
#' @param ... Unused.
#'
#' @return A tibble data frame: newdata with an additional column for the
#'   predictions in "predicted_TARGET" where TARGET is the name of the variable
#'   being predicted. If classification, the new column will contain predicted
#'   probabilities. The tibble will have child class "predicted_df" and
#'   attribute "model_info" that contains information about the model used to
#'   make predictions. You can call \code{plot} or \code{evaluate} on a
#'   predicted_df.
#' @export
#' @importFrom caret predict.train
#' @seealso \code{\link{plot.predicted_df}}, \code{\link{evaluate.predicted_df}}
#'
#' @details The model and hyperparameter values with the best out-of-fold
#'   performance in model training according to the selected metric is used to
#'   make predictions. Prepping data inside `predict` has the advantage of
#'   returning your predictions with the newdata in its original format.
#'
#' @examples
#' # Tune models using only the first 40 rows to keep computation fast
#'
#' models <- machine_learn(pima_diabetes[1:40, ], patient_id,
#'                         outcome = diabetes, tune = FALSE)
#'
#' # Make prediction on the next 10 rows. This uses the best-performing model from
#' # tuning cross validation, and it also prepares the new data in the same way as
#' # the training data was prepared.
#'
#' predictions <- predict(models, newdata = pima_diabetes[41:50, ])
#' predictions
#' evaluate(predictions)
#' plot(predictions)
predict.model_list <- function(object,
                               newdata,
                               prepdata,
                               write_log = FALSE,
                               ...) {

  # Pull info
  mi <- extract_model_info(object)
  best_models <- object[[mi$best_model_name]]
  training_data <- object[[1]]$trainingData

  # If newdata not provided, pull training data from object
  if (missing(newdata)) {
    newdata <-
      training_data %>%
      dplyr::mutate(!!mi$target := .outcome) %>%
      dplyr::select(- .outcome)
    # caret::train strips prepped_df class from newdata. So,
    # check recipe attr to see if it was prepped and if so convert.
    if ("recipe" %in% names(attributes(object)))
      class(newdata) <- c("prepped_df", class(newdata))
    using_training_data <- TRUE
  } else {
    using_training_data <- FALSE
  }
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")

  # Decide whether data needs to be prepped, check data, and prep if appropriate
  if (missing(prepdata))
    prepdata <- determine_prep(object, newdata, mi)
  to_pred <-
    if (prepdata) {
      ready_with_prep(object, newdata, mi)
    } else {
      ready_no_prep(training_data, newdata)
    }

  # If predicting on training, use out-of-fold; else make predictions
  if (using_training_data) {
    preds <- get_oof_predictions(object, mi)
  } else {
    # If classification, want probabilities. If regression, raw's the only option
    type <- if (is.classification_list(object)) "prob" else "raw"
    preds <- caret::predict.train(best_models, to_pred, type = type)
    # Probs get returned for no and yes. Keep only positive class as set in training
    if (is.classification_list(object))
      preds <- preds[[mi$positive_class]]
  }

  pred_name <- paste0("predicted_", mi$target)
  newdata[[pred_name]] <- preds
  newdata <- tibble::as_tibble(newdata)
  # Put predictions and, if present, the outcome at left of newdata
  newdata <- dplyr::select(newdata, pred_name, dplyr::everything())
  if (mi$target %in% names(newdata))
    newdata <- dplyr::select(newdata, mi$target, dplyr::everything())
  # Add class and attributes to data frame
  class(newdata) <- c("predicted_df", class(newdata))
  attr(newdata, "model_info") <-
    list(type = mi$m_class,
         target = mi$target,
         positive_class = mi$positive_class,
         algorithm = mi$best_model_name,
         metric = mi$metric,
         performance = mi$best_model_perf,
         timestamp = mi$timestamp,
         hyperparameters = structure(mi$best_model_tune,
                                     "row.names" = "optimal:"))
  if (isTRUE(write_log))
    write_log <- "prediction_log.txt"
  if (is.character(write_log))
    log_predictions(filename = write_log,
                    target = mi$target,
                    n_preds = nrow(newdata),
                    trained_time = attr(object, "timestamp"))
  return(newdata)
}

get_oof_predictions <- function(x, mi = extract_model_info(x)) {
  mod <- mi$best_model_name
  preds <- dplyr::arrange(x[[mod]]$pred, rowIndex)
  if (mi$m_class == "Regression")
    return(preds$pred)
  if (mi$m_class == "Classification")
    return(preds[[mi$positive_class]])
  stop("Eh? What kind of model is that?")
}
