#' Get predictions
#'
#' @description Make predictions using the best-performing model. For
#'   classification models, predicted probabilities are always returned, and you
#'   can get either predicted outcome class by specifying \code{outcome_groups}
#'   or risk groups by specifying \code{risk_groups}.
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
#'   predictions; this can be overridden by setting `prepdata = FALSE`, but this
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
#'   \code{predict} to \code{\link{get_cutoffs}}. Note that only one of
#'   \code{risk_groups} and \code{outcome_groups} can be specified.
#' @param outcome_groups Should predictions be grouped into outcome classes and
#'   returned in column "predicted_group"? If this is NULL (default), they will
#'   not be. The threshold for splitting outcome classes is determined on the
#'   training data via \code{\link{get_thresholds}}. If this is TRUE, the
#'   threshold is chosen to maximize accuracy, i.e. false positives and false
#'   negatives are equally weighted. If this is a number it is the ratio of cost
#'   (badnesss) of false negatives (missed detections) to false positives (false
#'   alarms). For example, \code{outcome_groups = 5} indicates a preferred ratio
#'   of five false alarms to every missed detection, and \code{outcome_groups =
#'   .5} indicates that two missed detections is as bad as one false alarm. This
#'   value is passed to the \code{cost_fn} argument of
#'   \code{\link{get_thresholds}}. You can get the cutoff values used to
#'   separate groups by passing the output of \code{predict} to
#'   \code{\link{get_cutoffs}}. Note that only one of \code{risk_groups} and
#'   \code{outcome_groups} can be specified.
#' @param prepdata Defunct. Data are always prepped in prediction.
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
#' @seealso \code{\link{plot.predicted_df}},
#'   \code{\link{evaluate.predicted_df}}, \code{\link{get_thresholds}},
#'   \code{\link{get_cutoffs}}
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
#' ### Data prep and model training ###
#' ####################################
#'
#' set.seed(7510)
#' # Split the first 200 rows in pima_diabetes into a model-training dataset
#' # containing 3/4 of the data and a test dataset containing 1/4 of the data.
#' d <- split_train_test(pima_diabetes[1:200, ], diabetes, .75)
#'
#' # Prep the training data for model training and train regularized regression
#' # and extreme gradient boosted models
#' models <-
#'   d$train %>%
#'   prep_data(patient_id, outcome = diabetes) %>%
#'   flash_models(outcome = diabetes, models = c("glm", "xgb"))
#'
#' ### Making predictions ###
#' ##########################
#'
#' # Make prediction on test data using the model that performed best in
#' # cross validation during model training. Before predictions are made, the test
#' # data is automatically prepared the same way the training data was.
#' predictions <- predict(models, newdata = d$test)
#' predictions
#' evaluate(predictions)
#' plot(predictions)
#'
#' ### Outcome class predictions ###
#' #################################
#'
#' # If you want class predictions in addition to predicted probabilities for
#' # a classification model, specify outcome_groups. The number passed to
#' # outcome groups is the cost of a false negative relative to a false positive.
#' # This example specifies that one missed detection is as bad as ten false
#' # alarms, and the resulting confusion matrix reflects this preference.
#' class_preds <- predict(models, newdata = d$test, outcome_groups = 10)
#' table(actual = class_preds$diabetes, predicted = class_preds$predicted_group)
#'
#' # You can extract the threshold used to separate predicted Y from predicted N
#' get_cutoffs(class_preds)
#'
#' # And you can visualize that cutoff by simply plotting the predictions
#' plot(class_preds)
#'
#' ### Risk stratification ###
#' ###########################
#'
#' # Alternatively, you can stratify observations into risk groups by specifying
#' # the risk_groups parameter. For example, this creates five risk groups
#' # with custom names. Risk group assignment is based on the distribution of
#' # predicted probabilities in model training. This is useful because it preserves
#' # a consistent notion of risk; for example, if you make daily predictions and
#' # one day happens to contain only low-risk patients, those patients will all
#' # be classified as low risk. Over the long run, group sizes will be consistent,
#' # but in any given round of predictions they may differ. If you want fixed
#' # group sizes, see the following examples.
#' predict(models, d$test,
#'         risk_groups = c("very low", "low", "medium", "high", "very high")) %>%
#'   plot()
#'
#' ### Fixed size groups ###
#' #########################
#'
#' # If you want groups of fixed sizes, e.g. say you have capacity to admit the three
#' # highest-risk patients, treat the next five, and have to discharge the remainder,
#' # you can use predicted probabilities to do that. One way to do that is to
#' # arrange the predictions data frame in descending order of risk, and then use the
#' # row numbers to stratify patients
#' library(dplyr)
#' predict(models, d$test) %>%
#'   arrange(desc(predicted_diabetes)) %>%
#'   mutate(action = case_when(
#'     row_number() <= 3 ~ "admit",
#'     row_number() <= 8 ~ "treat",
#'     TRUE ~ "discharge"
#'   )) %>%
#'   select(predicted_diabetes, action, everything())
#'
#' # Finally, if you want a fixed group size that is further down on the risk
#' # scale, you can achieve that with a combination of risk groups and the
#' # stratifying approach in the last example. For example, say you have capacity
#' # to admit 5 patients, but you don't want to admit patients in the top 10% of
#' # risk scores.
#' predict(models, d$test,
#'         risk_groups = c("risk acceptable" = 90, "risk too high" = 10)) %>%
#'   filter(predicted_group == "risk acceptable") %>%
#'   top_n(n = 5, wt = predicted_diabetes)
predict.model_list <- function(object,
                               newdata,
                               risk_groups = NULL,
                               outcome_groups = NULL,
                               prepdata,
                               write_log = FALSE,
                               ...) {
  if (missing(prepdata)) {
    prepdata <- TRUE
  } else if (!prepdata) {
    stop("Data must be prepped during prediction. If this is really ",
         "problematic for you, consider using healthcare v2.1.")
  } else {
    warning("Specifying `prepdata` to `predict` is defunct. Data are ",
            "always prepped prior to predictions being made.")
  }
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
  recipe <- attr(object, "recipe")
  if (missing(newdata)) {
    # Get prep-prep training data
    newdata <- recipe$template
    # Use out-of-fold predictions from training
    oof_preds <- get_oof_predictions(object, mi)
    preds <- oof_preds$preds
    outcomes <- oof_preds$outcomes
  } else {
    if (inherits(newdata, "prepped_df"))
      stop("predict will prep your data for you. Please pass the pre-prep ",
           "version to newdata (without passing it through `prep_data` first.)")
    if (!inherits(newdata, "data.frame"))
      stop("newdata must be a data frame")
    to_pred <- ready_with_prep(object, newdata, mi)
    type <- if (is.classification_list(object)) "prob" else "raw"
    preds <- caret::predict.train(best_models, to_pred, type = type)
    outcomes <- to_pred[[mi$target]]
    # Probs get returned for no and yes. Keep only positive class as set in training
    if (is.classification_list(object))
      preds <- preds[[mi$positive_class]]
  }
  pred_name <- paste0("predicted_", mi$target)
  newdata[[pred_name]] <- preds
  # Replace outcome as it came in with baked version, either from get_oof or
  # from ready_with_prep (or with NULL if newdata doesn't have it)
  newdata[[mi$target]] <- outcomes
  newdata <- tibble::as_tibble(newdata)

  # Add groups if desired
  if (mi$m_class == "Multiclass") {
    if (!is.null(risk_groups) || !is.null(outcome_groups)) {
      warning("Multiclass models don't support risk or outcome groups. ",
              "Predict will continue without those parameters.")
      risk_groups <- NULL
      outcome_groups <- NULL
    }
  }
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
      dplyr::mutate(cutpoints = stats::quantile(oof$preds, quantiles),
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
    neg_class <- dplyr::setdiff(oof$outcomes, mi$positive_class)
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
  predictions <-
    if (mi$m_class == "Regression") {
      preds$pred
    } else if (mi$m_class == "Classification") {
      preds[[mi$positive_class]]
    } else if (mi$m_class == "Multiclass") {
      preds$pred
    } else stop("Eh? What kind of model is that?")
  obs <- preds$obs
  tibble::tibble(preds = predictions, outcomes = obs) %>%
    return()
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
