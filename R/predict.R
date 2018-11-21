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
#'   TRUE, will create or append a file called "prediction_log.txt" in
#'   the current directory with metadata about predictions. If a character, is
#'   the name of a file to create or append with prediction metadata. If you
#'   want a unique log file each time predictions are made, use something like
#'   \code{write_log = paste0(Sys.time(), " predictions.txt")}. This param
#'   modifies error behavior and is best used in production. See details.
#' @param ... Unused.
#' @param ensemble Default FALSE, user can set ensemble is TRUE or FALSE.
#'
#' @return A tibble data frame: newdata with an additional column for the
#'   predictions in "predicted_TARGET" where TARGET is the name of the variable
#'   being predicted. If classification, the new column will contain predicted
#'   probabilities. The tibble will have child class "predicted_df" and
#'   attribute "model_info" that contains information about the model used to
#'   make predictions. You can call \code{plot} or \code{evaluate} on a
#'   predicted_df. If \code{write_log} is TRUE and this function errors, a
#'   zero-row dataframe will be returned.
#'
#'   Returned data will contain an attribute, "prediction_log" that contains a
#'   tibble of  logging info for writing to database. If \code{write_log}
#'   is TRUE and predict errors, an empty dataframe with the "prediction_log"
#'   attribute will still be returned. Extract this attribute using
#'   \code{attr(pred, "prediction_log")}.
#'
#'   Data will also contain a "failed" attribute to easily filter for errors
#'   after prediction. Extract using \code{attr(pred, "failed")}.
#' @export
#' @importFrom caret predict.train
#' @seealso \code{\link{plot.predicted_df}}, \code{\link{evaluate.predicted_df}}
#'
#' @details The model and hyperparameter values with the best out-of-fold
#'   performance in model training according to the selected metric is used to
#'   make predictions. Prepping data inside `predict` has the advantage of
#'   returning your predictions with the newdata in its original format.
#'
#'   If \code{write_log} is TRUE and an error is encountered, \code{predict}
#'   will not stop. It will return the error message as:
#'   - A warning in the console
#'   - A field in the log file
#'   - A column in the "prediction_log" attribute
#'   - A zero-row data frame will be returned
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
                               ensemble = FALSE,
                               write_log = FALSE,
                               ...) {
  start <- Sys.time()
  if (write_log == FALSE) {
    out <- predict_model_list_main(object,
                                   newdata,
                                   prepdata,
                                   ensemble,
                                   write_log,
                                   ...)
  } else {
    out <- safe_predict_model_list_main(object,
                                        newdata,
                                        prepdata,
                                        ensemble,
                                        write_log,
                                        ...)

    mi <- extract_model_info(object)
    out <- parse_safe_n_quiet(out, mi, object)
    # Get log file name
    if (isTRUE(write_log)) {
      write_log <- paste0(mi$model_name, "_prediction_log.txt")
    }
  }

  run <- Sys.time() - start
  attr(out, "prediction_log")$run_time <- as.numeric(lubridate::seconds(run))
  if (write_log != FALSE) {
    log_predictions(filename = write_log, d = attr(out, "prediction_log"))
  }
  return(out)
}

#' The bulk of the predict code is here. It's done this way so that we can call
#' safe_predict_model_list_main and get output for telemetry regardless of
#' exit status.
#' @noRd
predict_model_list_main <- function(object,
                                    newdata,
                                    prepdata,
                                    ensemble = FALSE,
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
  # Align column order
  ord <- match(names(training_data), names(to_pred))
  # The ord part gets columns that are in training_data; the which part retains any other columns
  to_pred <- to_pred[, c(ord[!is.na(ord)], which(!names(to_pred) %in% names(training_data)))]

  # If predicting on training, use out-of-fold; else make predictions
  if (using_training_data) {
    preds <- get_oof_predictions(object, mi, to_pred, ensemble)
  } else {
    # If classification, want probabilities. If regression, raw's the only option
    type <- if (is.classification_list(object)) "prob" else "raw"
    preds <- caret::predict.train(best_models, to_pred, type = type)
    # Probs get returned for no and yes. Keep only positive class as set in training
    if (is.classification_list(object)) {
      if (ensemble == FALSE) {
        preds <- preds[[mi$positive_class]]
      }
      else{
        preds_all <- lapply(object, caret::predict.train, newdata = to_pred, type = type)
        preds <- weighted_average_ensemble(object, mi = extract_model_info(object), preds_all, mi$positive_class)
      }
    }
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

  return(newdata)
}

#' Predict code that always returns the dataframe tibble.
#' @noRd
safe_predict_model_list_main <- safe_n_quiet(predict_model_list_main)

get_oof_predictions <- function(x, mi = extract_model_info(x), to_pred, ensemble = FALSE) {
  mod <- mi$best_model_name
  preds <- dplyr::arrange(x[[mod]]$pred, rowIndex)
  # To solve a mysterious bug in test-predict: predict handles positive class specified in training
  # Where each observation appeared twice in object$`Random Forest`$pred:
  ## Now even hackier because this error popped up on Debian CRAN checks in
  ## tune_models tests for picking positive class even with the hacky fix of
  ## removing any fully duplicated lines from preds
  ### Cannot reproduce locally, so not sure what is wrong, but two changes:
  ### convert to tibble to avoid potential parstial name matching and other such gotchas and
  ### Take the first value of each rowIndex if any appears twice (which they should not!)
  preds <-
    tibble::as_tibble(preds) %>%
    .[!duplicated(.$rowIndex), ]
  if (mi$m_class == "Regression"){
    if (ensemble == FALSE){
      return(preds$pred)
    }
    else{
      preds_all <- lapply(x, caret::predict.train, newdata = to_pred, type = "raw")
      preds$pred <- weighted_average_ensemble(x, mi = extract_model_info(x), preds_all)
      return(preds$pred)
    }

  }
  if (mi$m_class == "Classification"){
    if (ensemble == FALSE){
      return(preds[[mi$positive_class]])
    }
    else{
      preds_all <- lapply(x, caret::predict.train, newdata = to_pred, type = "prob")
      preds[[mi$positive_class]] <- weighted_average_ensemble(x, mi = extract_model_info(x), preds_all, mi$positive_class)
      return(preds[[mi$positive_class]])
    }

  }
  stop("Eh? What kind of model is that?")
}

weighted_average_ensemble <- function(x, mi = extract_model_info(x), preds_all, pred_class) {
  mod <- mi$best_model_name
  if (missing(pred_class)){
    preds <- preds_all
  }else{
    preds <- map(preds_all, pred_class)
  }
  target <- function(x) {
    if (x == mod) {
      x <- "target_variable"
    }
    else{(x != mod)
      x <- x
    }
  }
  names(preds) <- lapply(names(preds), target)
  preds_data_frame <- data.frame(preds)
  weight_modeling <- glm(target_variable ~ ., data = preds_data_frame)
  weight_model_coefficient <- summary(weight_modeling)$coefficient
  weight_model <- data.frame(weight_model_coefficient)
  weight_model$variables <- row.names(weight_model)
  weight_model <- weight_model[c("variables", "Estimate")][-1, ]
  Preds_new <- list.remove(preds, 'target_variable')
  Weighted_model <- Map("*", Preds_new, weight_model$Estimate)
  Weighted_model_updated <- list.append(Weighted_model, preds$target_variable)
  add <- function(x) Reduce("+", x)
  preds_outcome <- add(Weighted_model_updated)
  return(preds_outcome)
}

get_pred_summary <- function(x) {
  pred_summary <- x %>%
    dplyr::select(starts_with("predicted_")) %>%
    dplyr::pull() %>%
    summary() %>%
    dplyr::bind_rows()
  return(pred_summary)
}


#' Get cutoff values for group predictions
#'
#' @param x Data frame from \code{\link{predict.model_list}} where
#'   \code{outcome_groups} or \code{risk_groups} was specified
#'
#' @return A message is printed about the thresholds. If \code{outcome_groups}
#'   were defined the return value is a single numeric value, the threshold used
#'   to separate predicted probabilities into outcome groups. If
#'   \code{risk_groups} were defined the return value is a data frame with one
#'   column giving the group names and another column giving the minimum
#'   predicted probability for an observation to be in that group.
#' @export
#'
#' @examples
#' machine_learn(pima_diabetes[1:20, ], patient_id, outcome = diabetes,
#'               models = "xgb", tune = FALSE) %>%
#'   predict(risk_groups = 5) %>%
#'   get_cutoffs()
get_cutoffs <- function(x) {
  if (!is.predicted_df(x))
    stop("`get_cutoffs` only works on the output of `predict`. Do you want `get_thresholds`?")
  if (!"predicted_group" %in% names(x))
    stop("`get_cutoffs` requires that groups were created during prediction. ",
         "Specify `risk_groups` or `outcome_groups` in your `predict` call.")

  ats <- attributes(x$predicted_group)

  if (ats$group_type == "outcome") {
    message("Predicted outcomes ", list_variables(ats$levels),
            " separated at: ", signif(ats$cutpoints, 3))
    return(invisible(ats$cutpoints))
  } else {
    message("Risk groups defined by the following thresholds:\n")
    d <-
      tibble::tibble(
        group = ats$levels,
        minimum_probability = ats$cutpoints[-length(ats$cutpoints)]
      ) %>%
      dplyr::arrange(desc(minimum_probability))
    print(d)
    return(invisible(d))
  }
}
