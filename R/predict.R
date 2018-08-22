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
#' @param risk_groups Should predictions be grouped into risk groups and
#'   returned in column "predicted_group"? If this is NULL (default), they will
#'   not be. If this is a single number, that number of groups will be created
#'   with names "risk_group1", "risk_group2", etc. "risk_group1" is always the
#'   highest risk (highest predicted probability). The groups will have equal
#'   expected sizes, based on the distribution of out-of-fold predictions on the
#'   training data. If this is a character vector, its entries will be used as
#'   the names of the risk groups, in increasing order of risk, again with equal
#'   expected sizes of groups. If you want unequal-size groups, this can be a
#'   named numeric vector, where the names will be the names of the risk groups,
#'   in increasing order of risk, and the entries will be the relative
#'   proportion of observations in the group, again based on the distribution of
#'   out-of-fold predictions on the training data. For example,
#'   \code{risk_groups = c(low = 2, mid = 1, high = 1)} will put the bottom half
#'   of predicted probabilities in the "low" group, the next quarter in the
#'   "mid" group, and the highest quarter in the "high" group. You can get the
#'   cutoff values used to separate groups by passing the output of
#'   \code{predict} to \code{\link{get_cutoffs}}
#' @param outcome_groups Should predictions be grouped into outcome classes and
#'   returned in column "predicted_group"? If this is NULL (default), they will
#'   not be. The threshold for splitting outcome classes is determined on the
#'   training data via \code{\link{get_thresholds}}. If this is TRUE, the
#'   threshold is chosen to maximize accuracy, i.e. false positives and false
#'   negatives are equally weighted. If this is a number it is the ratio of cost
#'   (badnesss) of false negatives (missed detections) to false positives (false
#'   alarms). For example, \code{outcome_groups = 5} indicates a prefered ratio
#'   of five false alarms to every missed detection, and \code{outcome_groups =
#'   .5} indicates that two missed detections is as bad as one false alarm. This
#'   value is passed to the \code{cost_fn} argument of
#'   \code{\link{get_thresholds}}. You can get the cutoff values used to
#'   separate groups by passing the output of \code{predict} to
#'   \code{\link{get_cutoffs}}
#' @param prepdata Logical, this should rarely be set by the user. By default,
#'   if `newdata` hasn't been prepped, it will be prepped by `prep_data` before
#'   predictions are made. Set this to TRUE to force already-prepped data
#'   through `prep_data` again, or set to FALSE to prevent `newdata` from being
#'   sent through `prep_data`.
#' @param write_log Write prediction metadata to a file? Default is FALSE. If
#'   TRUE, will create or append a file called "prediction_log.txt" in the
#'   current directory with metadata about predictions. If a character, is the
#'   name of a file to create or append with prediction metadata. If you want a
#'   unique log file each time predictions are made, use something like
#'   \code{write_log = paste0(Sys.time(), " predictions.txt")}. This param
#'   modifies error behavior and is best used in production. See details.
#' @param ... Unused.
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
#'   tibble of  logging info for writing to database. If \code{write_log} is
#'   TRUE and predict errors, an empty dataframe with the "prediction_log"
#'   attribute will still be returned. Extract this attribute using
#'   \code{attr(pred, "prediction_log")}.
#'
#'   Data will also contain a "failed" attribute to easily filter for errors
#'   after prediction. Extract using \code{attr(pred, "failed")}.
#' @export
#' @importFrom caret predict.train
#' @seealso \code{\link{plot.predicted_df}}, \code{\link{evaluate.predicted_df}},
#' \code{\link{get_thresholds}}, \code{\link{get_cutoffs}}
#'
#' @details The model and hyperparameter values with the best out-of-fold
#'   performance in model training according to the selected metric is used to
#'   make predictions. Prepping data inside `predict` has the advantage of
#'   returning your predictions with the newdata in its original format.
#'
#'   If \code{write_log} is TRUE and an error is encountered, \code{predict}
#'   will not stop. It will return the error message as: - A warning in the
#'   console - A field in the log file - A column in the "prediction_log"
#'   attribute - A zero-row data frame will be returned
#'
#' @examples
#' # Prepare data and train models using only the first 100 rows to keep computation fast
#' models <-
#'   pima_diabetes[1:100, ] %>%
#'   prep_data(patient_id, outcome = diabetes) %>%
#'   flash_models(outcome = diabetes)
#'
#' # Make prediction on the next 100 rows using the model that performed best in
#' # cross validation during model training. Before predictions are made, newdata
#' # is automatically prepared the same way the training data was.
#' predictions <- predict(models, newdata = pima_diabetes[101:200, ])
#' predictions
#' evaluate(predictions)
#' plot(predictions)
#'
#' # If you want class predictions in addition to predicted probabilities for
#' # a classification model, specify outcome_groups. The number passed to
#' # outcome groups is the cost of a false negative relative to a false positive.
#' # This specifies that one missed detection is as bad as three false alarms,
#' # and the resulting confusion matrix reflects this preference.
#' class_preds <- predict(models, newdata = pima_diabetes[101:200, ], outcome_groups = 3)
#' table(actual = class_preds$diabetes, predicted = class_preds$predicted_group)
#'
#' # Alternatively, you can stratify observations into risk groups by specifying
#' # the risk_groups parameter. For example, this creates five equal size groups
#' # with custom names
#' predict(models, pima_diabetes[101:200, ],
#'         risk_groups = c("very low", "low", "medium", "high", "very high")) %>%
#'   plot()
predict.model_list <- function(object,
                               newdata,
                               risk_groups = NULL,
                               outcome_groups = NULL,
                               prepdata,
                               write_log = FALSE,
                               ...) {
  start <- Sys.time()
  if (write_log == FALSE) {
    out <- predict_model_list_main(object,
                                   newdata,
                                   risk_groups,
                                   outcome_groups,
                                   prepdata,
                                   write_log,
                                   ...)
  } else {
    out <- safe_predict_model_list_main(object,
                                        newdata,
                                        risk_groups,
                                        outcome_groups,
                                        prepdata,
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
                                    risk_groups,
                                    outcome_groups,
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
  # Align column order
  ord <- match(names(training_data), names(to_pred))
  # The ord part gets columns that are in training_data; the which part retains any other columns
  to_pred <- to_pred[, c(ord[!is.na(ord)], which(!names(to_pred) %in% names(training_data)))]

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

  # Add groups if desired
  newdata <- add_groups(object, mi, newdata, pred_name, risk_groups, outcome_groups)

  # Put predictions and, if present, the outcome and predicted group at left of newdata
  newdata <- dplyr::select(newdata, pred_name, dplyr::everything())
  if ("predicted_group" %in% names(newdata))
    newdata <- dplyr::select(newdata, pred_name, predicted_group, dplyr::everything())
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

# Add "predicted_group" column to newdata. For risk_groups,
# note that `cut` needs 0 for the lowest break but one fewer labels, hense
# the adding of a 0 to risk_groups which in the `cut` call propigates to
# `breaks` but is removed for `labels`
add_groups <- function(object, mi, newdata, pred_name, risk_groups, outcome_groups) {
  if (!is.null(outcome_groups) && !is.null(risk_groups))
    stop("You can only get `risk_groups` or `outcome_groups`. If you really ",
         "want both, call `predict` twice and `cbind` the results.")

  oof <- get_oof_predictions(object)

  if (!is.null(risk_groups)) {

    if (rlang::is_named(risk_groups)) {
      # Using custom-size groups
      cutter <- tibble::tibble(
        names = c("zero", names(risk_groups)),
        quantiles = c(0, cumsum(risk_groups) / sum(risk_groups))
      )
    } else if (is.character(risk_groups)) {
      # Using custom-name, even-size groups
      cutter <- tibble::tibble(
        names = c("zero", risk_groups),
        quantiles = seq(0, 1, len = length(risk_groups) + 1)
      )
    } else {
      # Using default name and size groups
      if (risk_groups == 1)
        stop("risk_groups = 1 just puts everyone in the same group. ",
             "Do you want outcome_groups = 1?")
      cutter <- tibble::tibble(
        names = c("zero", paste0("risk_group", rev(seq_len(risk_groups)))),
        quantiles = seq(0, 1, len = risk_groups + 1)
      )
    }
    # Replace outer bounds with 0 and 1 so that any predicted prob is in bounds
    cutter <-
      cutter %>%
      dplyr::mutate(cutpoints = stats::quantile(oof, quantiles),
                    cutpoints = dplyr::case_when(
                      quantiles == 0 ~ 0,
                      quantiles == 1 ~ 1,
                      TRUE ~ cutpoints))
    newdata$predicted_group <-
      cut(newdata[[pred_name]],
          breaks = cutter$cutpoints,
          labels = cutter$names[-1],
          include.lowest = TRUE) %>%
      structure(group_type = "risk",
                cutpoints = cutter$cutpoints)

  } else if (!is.null(outcome_groups)) {
    if (!outcome_groups)
      stop("outcome_groups should be a number, you provided outcome_groups = FALSE")
    cutpoint <-
      get_thresholds(object,
                     optimize = "cost",
                     measures = "cost",
                     cost_fn = outcome_groups) %>%
      dplyr::filter(optimal) %>%
      dplyr::pull(threshold)
    neg_class <- levels(object[[1]]$trainingData$.outcome)[1]
    newdata$predicted_group <-
      ifelse(newdata[[pred_name]] >= cutpoint, mi$positive_class, neg_class) %>%
      factor(levels = c(neg_class, mi$positive_class)) %>%
      structure(group_type = "outcome", cutpoints = cutpoint)
  }
  return(newdata)
}

#' Predict code that always returns the dataframe tibble.
#' @noRd
safe_predict_model_list_main <- safe_n_quiet(predict_model_list_main)

get_oof_predictions <- function(x, mi = extract_model_info(x)) {
  mod <- mi$best_model_name
  preds <- dplyr::arrange(x[[mod]]$pred, rowIndex)
  # To solve a mysterious bug in test-predict: predict handles positive class specified in training
  # Where each observation appeared twice in object$`Random Forest`$pred:
  ## Now even hackier because this error popped up on Debian CRAN checks in
  ## tune_models tests for picking positive class even with the hacky fix of
  ## removing any fully duplicated lines from preds
  ### Cannot reproduce locally, so not sure what is wrong, but two changes:
  ### convert to tibble to avoid potential partial name matching and other such gotchas and
  ### Take the first value of each rowIndex if any appears twice (which they should not!)
  preds <-
    tibble::as_tibble(preds) %>%
    .[!duplicated(.$rowIndex), ]
  if (mi$m_class == "Regression")
    return(preds$pred)
  if (mi$m_class == "Classification")
    return(preds[[mi$positive_class]])
  stop("Eh? What kind of model is that?")
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
  if(!"predicted_group" %in% names(x))
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
