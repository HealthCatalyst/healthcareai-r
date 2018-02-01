#' Make predictions using the best-performing model from tuning
#'
#' @param models model_list object, as from `tune_models`
#' @param newdata data on which to make predictions. If missing, predictions
#'   will be made on the training data. Should have the same structure as the
#'   input to `prep_data`,`tune_models` or `train_models`. `predict` will try to
#'   figure out if the data need to be sent through `prep_data` before making
#'   predictions; this can be overriden by setting `prepdata = FALSE``, but this
#'   should rarely be needed.
#' @param prepdata Logical, rarely needs to be set by the user. By default, if
#'   `newdata` hasn't been prepped, it will be prepped by `prep_data` before
#'   predictions are made. Set this to TRUE to force already-prepped data
#'   through `prep_data` again, or set to FALSE to prevent `newdata` from being
#'   sent through `prep_data`.
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
#' models <- machine_learn(pima_diabetes[1:50, ], diabetes)
#' # Make prediction on the next 20 rows. This uses the best-performing model from
#' # tuning cross validation, and it also prepares the new data in the same way as
#' # the training data was prepared.
#' predictions <- predict(models, newdata = slice(pima_diabetes, 51:70))
#' predictions
#' ggplot(predictions, aes(x = predicted_diabetes, fill = diabetes)) +
#'   geom_density(alpha = .5)
predict.model_list <- function(models, newdata, prepdata) {
  mi <- extract_model_info(models)
  best_models <- models[[mi$best_model_name]]
  if (missing(newdata)) {
    newdata <-
      best_models$trainingData %>%
      dplyr::mutate(!!mi$target := .outcome) %>%
      dplyr::select(- .outcome)
    # caret::train strips hcai_prepped_df class from newdata. So,
    # check recipe attr to see if it was prepped and if so convert.
    if ("recipe" %in% names(attributes(models)))
      class(newdata) <- c("hcai_prepped_df", class(newdata))
  }
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")
  # If prepdata provided by user; follow that. Else, prep if newdata hasn't been
  # and the variables used to tune models aren't present.
  prep <-
    if (!missing(prepdata)) {
      prepdata
    } else {
      needs_prep <- "recipe" %in% names(attributes(models))
      been_prepped <- inherits(newdata, "hcai_prepped_df")
      # If going to prep; check if it looks like it has been and warn if so
      if (needs_prep && !been_prepped) {
        trainvars <- get_classes_sorted(dplyr::select(best_models$trainingData, -.outcome))
        predvars <- get_classes_sorted(dplyr::select(newdata, -which(names(newdata) == mi$target)))
        in_both <- intersect(names(predvars), names(trainvars))
        if (isTRUE(all.equal(trainvars, predvars[in_both])))
          warning("newdata looks like it may have been prepped, but I'm not ",
                  "sure, so I will try passing it to prep_data before making ",
                  "predictions. If you already prepped it, set ",
                  "`predict(prepdata = FALSE)`.")
        TRUE
      } else { FALSE }
    }

  # If classification, want probabilities. If regression, raw's the only option
  type <- if (is.classification_list(models)) "prob" else "raw"
  # This bit of repition avoids copying newdata if it's not being prepped
  preds <-
    if (prep) {
      prep_data(newdata, recipe = attr(models, "recipe")) %>%
        caret::predict.train(best_models, ., type = type)
    } else {
      newdata %>%
        caret::predict.train(best_models, ., type = type)
    }
  if (is.data.frame(preds))
    preds <- dplyr::pull(preds, Y)  # nolint
  pred_name <- paste0("predicted_", mi$target)
  newdata[[pred_name]] <- preds
  newdata <- tibble::as_tibble(newdata)
  # Put predictions and, if present, the outcome at left of newdata
  newdata <- dplyr::select(newdata, pred_name, dplyr::everything())
  if (mi$target %in% names(newdata))
      newdata <- dplyr::select(newdata, mi$target, dplyr::everything())
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

#' Summary method for predicted data frame
#' @export
#' @param d data frame from `predict.model_list`
#' @return list of model info
#' @noRd
summary.hcai_predicted_df <- function(d, ...) {
  mi <- attr(d, "model_info")
  mes <- paste0("predicted_", mi$target, " generated by ", mi$algorithm
                , ". Training performance (out-of-fold) was ", mi$metric,
                " = ", round(mi$performance, 3)
  )
  message(mes)
  tibble:::print.tbl_df(d)
  return(invisible(mi))
}

#' get_classes_sorted
#' @noRd
get_classes_sorted <- function(d) {
  classes <- purrr::map_lgl(d, is.numeric)
  return(classes[order(names(classes))])
}
