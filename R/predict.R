#' Make predictions using the best-performing model from tuning
#'
#' @param object model_list object, as from `tune_models`
#' @param newdata data on which to make predictions. If missing, predictions
#'   will be made on the training data. Should have the same structure as the
#'   input to `prep_data`,`tune_models` or `train_models`. `predict` will try to
#'   figure out if the data need to be sent through `prep_data` before making
#'   predictions; this can be overriden by setting `prepdata = FALSE`, but this
#'   should rarely be needed.
#' @param positive_class For classification only, which outcome level is the
#'   "yes" case, i.e. is associated with high probabilities? Defaults to
#'   the second level of the outcome variable (second alphabetically if the
#'   training data outcome was not already a factor).
#' @param prepdata Logical, this should rarely be set by the user. By default,
#'   if `newdata` hasn't been prepped, it will be prepped by `prep_data` before
#'   predictions are made. Set this to TRUE to force already-prepped data
#'   through `prep_data` again, or set to FALSE to prevent `newdata` from being
#'   sent through `prep_data`.
#' @param ... Unused.
#'
#' @return A tibble data frame: newdata with an additional column for the
#'   predictions in "predicted_TARGET" where TARGET is the name of the variable
#'   being predicted. If classification, the new column will contain predicted
#'   probabilities. The tibble will have child class "hcai_predicted_df" and
#'   attribute "model_info" that contains information about the model used to
#'   make predictions.
#' @export
#' @importFrom caret predict.train
#' @seealso \code{\link{plot.hcai_predicted_df}}, \code{\link{tune_models}},
#'   \code{\link{prep_data}}
#'
#' @details The model and hyperparameter values with the best out-of-fold
#'   performance in model training according to the selected metric is used to
#'   make predictions. Prepping data inside `predict` has the advantage of
#'   returning your predictions with the newdata in its original format.
#'
#' @examples
#' # Tune models using only the first 40 rows to keep computation fast
#'
#' models <- machine_learn(pima_diabetes[1:40, ], patient_id, outcome = diabetes)
#'
#' # Make prediction on the next 10 rows. This uses the best-performing model from
#' # tuning cross validation, and it also prepares the new data in the same way as
#' # the training data was prepared.
#'
#' predictions <- predict(models, newdata = pima_diabetes[41:50, ])
#' predictions
#' plot(predictions)
predict.model_list <- function(object, newdata, prepdata, positive_class, ...) {

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
    # caret::train strips hcai_prepped_df class from newdata. So,
    # check recipe attr to see if it was prepped and if so convert.
    if ("recipe" %in% names(attributes(object)))
      class(newdata) <- c("hcai_prepped_df", class(newdata))
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

  # Make predictions
  # If classification, want probabilities. If regression, raw's the only option
  type <- if (is.classification_list(object)) "prob" else "raw"
  preds <- caret::predict.train(best_models, to_pred, type = type)

  # Set positive class if missing and check if present in outcome
  if (is.classification_list(object)) {
    outcome_levels <- levels(training_data[[".outcome"]])
    if (missing(positive_class)) {
      positive_class <- outcome_levels[2]
    } else {
      if (!positive_class %in% outcome_levels)
        stop(positive_class, " not found in ", mi$target, ". ", mi$target,
             " has values ", paste(outcome_levels, collapse = " and "))
    }
    # Probs get returned for no and yes. Keep only positive class
    preds <- preds[[positive_class]]
  } else {
    if (!missing(positive_class))
      warning("positive_class is only for classification models. Will be ignored.")
    positive_class <- NULL
  }

  pred_name <- paste0("predicted_", mi$target)
  newdata[[pred_name]] <- preds
  newdata <- tibble::as_tibble(newdata)
  # Put predictions and, if present, the outcome at left of newdata
  newdata <- dplyr::select(newdata, pred_name, dplyr::everything())
  if (mi$target %in% names(newdata))
    newdata <- dplyr::select(newdata, mi$target, dplyr::everything())
  # Add class and attributes to data frame
  class(newdata) <- c("hcai_predicted_df", class(newdata))
  attr(newdata, "model_info") <-
    list(type = mi$m_class,
         target = mi$target,
         algorithm = mi$best_model_name,
         metric = mi$metric,
         performance = mi$best_model_perf,
         timestamp = mi$timestamp,
         hyperparameters = structure(mi$best_model_tune,
                                     "row.names" = "optimal:"))
  attr(newdata, "positive_class") <- positive_class
  return(newdata)
}
