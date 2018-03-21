#' Make predictions using the best-performing model from tuning
#'
#' @param object model_list object, as from `tune_models`
#' @param newdata data on which to make predictions. If missing, predictions
#'   will be made on the training data. Should have the same structure as the
#'   input to `prep_data`,`tune_models` or `train_models`. `predict` will try to
#'   figure out if the data need to be sent through `prep_data` before making
#'   predictions; this can be overriden by setting `prepdata = FALSE`, but this
#'   should rarely be needed.
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
#' @seealso \code{\link{tune_models}}, \code{\link{prep_data}}
#'
#' @details The model and hyperparameter values with the best out-of-fold
#'   performance in model training according to the selected metric is used to
#'   make predictions. Prepping data inside `predict` has the advantage of
#'   returning your predictions with the newdata in its original format.
#'
#' @examples
#' # Tune models using only the first 50 rows to keep computation fast
#' models <- machine_learn(pima_diabetes[1:50, ], outcome = diabetes)
#' # Make prediction on the next 20 rows. This uses the best-performing model from
#' # tuning cross validation, and it also prepares the new data in the same way as
#' # the training data was prepared.
#' predictions <- predict(models, newdata = pima_diabetes[51:70, ])
#' predictions
#' ggplot(predictions, aes(x = predicted_diabetes, fill = diabetes)) +
#'   geom_density(alpha = .5)
predict.model_list <- function(object, newdata, prepdata, ...) {

  # Pull info
  mi <- extract_model_info(object)
  best_models <- object[[mi$best_model_name]]

  # If newdata not provided, pull training data from object and prepare it
  if (missing(newdata)) {
    newdata <-
      best_models$trainingData %>%
      dplyr::mutate(!!mi$target := .outcome) %>%
      dplyr::select(- .outcome)
    # caret::train strips hcai_prepped_df class from newdata. So,
    # check recipe attr to see if it was prepped and if so convert.
    if ("recipe" %in% names(attributes(object)))
      class(newdata) <- c("hcai_prepped_df", class(newdata))
  }
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")

  # If prepdata provided by user; follow that. Else, prep if newdata hasn't been
  # prepped and the variables used to tune models aren't present.
  prep <-
    if (!missing(prepdata)) {
      prepdata
    } else {
      # If there's a recipe, it looks like prep is needed
      needs_prep <- "recipe" %in% names(attributes(object))
      # If newdata has prepped signature, then prepping definitely isn't needed
      been_prepped <- inherits(newdata, "hcai_prepped_df")
      # If data was prepped in training and newdata doesn't appear to have been prepped,
      # then we will prep, but check to see if it looks like newdata has already been
      # and issue a warning if so
      if (needs_prep && !been_prepped) {
        trainvars <- get_classes_sorted(dplyr::select(best_models$trainingData, -.outcome)) # nolint
        predvars <- get_classes_sorted(dplyr::select(newdata, -which(names(newdata) == mi$target)))
        joined <- dplyr::left_join(trainvars, predvars, by = "variable")
        if (isTRUE(all.equal(joined$is_numeric.x, joined$is_numeric.y)))
          warning("The data used in model training was prepped using `prep_data`. ",
                  "Therefore, the data you want to make predictions on must also be prepped. ",
                  "It looks like you might have done that by passing `newdata` ",
                  "through `prep_data` before passing it to `predict`, but I can't be sure, ",
                  "so it will be prepped now before making predictions. ",
                  "If you passed the prediction data through `prep_data` before ",
                  "`predict`, set `predict(prepdata = FALSE)`.")
        TRUE
      } else {
        FALSE
      }
    }

  # If classification, want probabilities. If regression, raw's the only option
  type <- if (is.classification_list(object)) "prob" else "raw"

  # This bit of repition avoids copying newdata if it's not being prepped
  preds <-
    if (prep) {
      recipe <- attr(object, "recipe")
      if (is.null(recipe))
        stop("Can't prep data in prediction without a recipe from training data.")

      # Check for missingness not present in training and warn if present
      missing_train <- missingness(recipe$template, return_df = FALSE) %>% .[. > 0] %>% names()
      missing_now <- missingness(newdata, return_df = FALSE) %>% .[. > 0] %>% names()
        # Don't care about missingness in the outcome
      new_missing <- dplyr::setdiff(missing_now, c(missing_train, mi$target))
      if (length(new_missing))
        warning("The following variables have missingness that was not present in model training: ",
                paste(new_missing, collapse = ", "))
      # Check for new levels in factors not present in training and warn if present
      new_levels <-
        find_new_levels(newdata,
                        # Remove outcome if present because don't care if missingness there:
                        recipe$template[, which(!names(recipe$template) %in% mi$target), drop = FALSE]) %>%
        format_new_levels()
      if (length(new_levels))
        warning("The following variables(s) had the following value(s) in ",
                "predict that were not observed in training. ", new_levels)
      # Make predictions
      prep_data(newdata, recipe = recipe) %>%
        caret::predict.train(best_models, ., type = type)
    } else {
      # Select variables to be used in prediction:
      td <- dplyr::select(best_models$trainingData, -.outcome)
      # Pull off columns not used in prediction, but leave newdata alone for return
      to_pred <- newdata[, names(newdata) %in% names(td), drop = FALSE]
      # Check for no missingness
      has_missing <- missingness(to_pred, FALSE) > 0
      if (any(has_missing))
        stop("The following variables have missingness that needs to be ",
             "addressed before making predictions. ",
             "Consider using prep_data to address this.\n\t",
             paste(names(has_missing)[has_missing], collapse = ", "))
      # Check for no new levels in factors
      missing_levels <-
        find_new_levels(to_pred, best_models$trainingData) %>%
        format_new_levels()
      if (length(missing_levels))
        stop("The following variable(s) had the following value(s) ",
             "in predict that were not observed in training. ",
             "Consider using prep_data to address this.", missing_levels)
      # Make predictions
      to_pred %>%
        caret::predict.train(best_models, ., type = type)
    }

  # Probs get returned for no and yes. Take just positive class from 2nd column
  if (is.data.frame(preds))
    preds <- preds[, 2]
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
    list(target = mi$target,
         algorithm = mi$best_model_name,
         metric = mi$metric,
         performance = mi$best_model_perf,
         hyperparameters = structure(mi$best_model_tune,
                                     "row.names" = "optimal:"))
  return(newdata)
}
