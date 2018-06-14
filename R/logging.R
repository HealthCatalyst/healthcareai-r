#' Create or append log files from predict
#' @noRd
log_predictions <- function(filename, d) {
  the_log <- paste0(
    "Model loaded from: ", d$loaded_from,
    "\n\t- Predictions made successfully: ", d$predictions_made,
    "\n\t- Model name: ", d$model_name,
    "\n\t- Variable predicted: ", d$outcome_variable,
    "\n\t- Predictions made time: ", d$predict_time,
    "\n\t- Number predictions: ", d$n_predictions,
    "\n\t- Days since model trained: ", d$days_since_trained,
    "\nSummary of predictions: ",
    "\n\t- Minimum: ", d$prediction_min,
    "\n\t- 1st Quartile: ", d$prediction_q1,
    "\n\t- Mean: ", d$prediction_mean,
    "\n\t- Median: ", d$prediction_median,
    "\n\t- 3rd Quartile: ", d$prediction_q3,
    "\n\t- Maximum: ", d$prediction_max,
    "\nSummary of missingness in new data (%):",
    "\n\t- Minimum: ", d$missingness_min,
    "\n\t- 1st Quartile: ", d$missingness_q1,
    "\n\t- Mean: ", d$missingness_mean,
    "\n\t- Median: ", d$missingness_median,
    "\n\t- 3rd Quartile: ", d$missingness_q3,
    "\n\t- Maximum: ", d$missingness_max,
    "\n Error status: ", !is.na(d$error_message),
    "\n Error message: ", d$error_message,
    "\n Error location: ", d$error_call,
    "\n Warnings: ", d$warnings,
    "\n=======================================",
    "\n"
  )
  write(the_log, filename, append = TRUE)
  return(d)
}

#' Sets defaults for telemetry tibble.
#' @noRd
set_inital_telemetry <- function(mi) {
  d <- tibble::tibble(
    loaded_from = mi$from_rds,
    model_name = mi$model_name,
    outcome_variable = mi$target,
    predict_time = Sys.time(),
    predictions_made = FALSE,
    n_predictions = 0,
    run_time = NA,
    days_since_trained =
      round(difftime(Sys.time(), mi$timestamp, units = "days"), 1),
    prediction_min = NA,
    prediction_q1 = NA,
    prediction_mean = NA,
    prediction_median = NA,
    prediction_q3 = NA,
    prediction_max = NA,
    missingness_min = NA,
    missingness_q1 = NA,
    missingness_mean = NA,
    missingness_median = NA,
    missingness_q3 = NA,
    missingness_max = NA,
    error_message = NA,
    error_call = NA,
    warnings = NA)

  return(d)
}

#' Updates telemetry tibble prior to writing. These fields should be set by
#' set_default_telemetry.
#' @noRd
update_telemetry <- function(d, newdata) {
  pred_summary <- get_pred_summary(newdata)
  missingness <- missingness(newdata)
  missingness <- summary(missingness$percent_missing) %>%
    bind_rows()

  d$n_predictions <- nrow(newdata)
  d$predictions_made <- TRUE
  d$prediction_mean <- pred_summary$Mean
  d$prediction_min <- pred_summary$Min.
  d$prediction_q1 <- pred_summary$`1st Qu.`
  d$prediction_median <- pred_summary$Median
  d$prediction_q3 <- pred_summary$`3rd Qu.`
  d$prediction_max <- pred_summary$Max.
  d$missingness_mean <- missingness$Mean
  d$missingness_min <- missingness$Min.
  d$missingness_q1 <- missingness$`1st Qu.`
  d$missingness_median <- missingness$Median
  d$missingness_q3 <- missingness$`3rd Qu.`
  d$missingness_max <- missingness$Max.
  return(d)
}

#' Purrr functions safe and quiet are returned to save all types of output.
#' @noRd
safe_n_quiet <- function(.f, otherwise = NULL) {
  retfun <- purrr::quietly(purrr::safely(.f,
                                         otherwise = otherwise,
                                         quiet = FALSE))
  function(...) {
    ret <- retfun(...)
    list(result = ret$result$result,
         output = ret$output,
         messages = ret$messages,
         warnings = ret$warnings,
         error = ret$result$error)
  }
}

#' Parse output from safe_n_quiet
#' @noRd
parse_safe_n_quiet <- function(x, mi, mod) {
  d_log <- set_inital_telemetry(mi)
  if (length(x$warnings)) {
    d_log$warnings <- stringr::str_c(x$warnings, collapse = " ")
    warning(x$warnings)
  }
  # No error
  if (is.null(x$error)) {
    d_log <- d_log %>%
      update_telemetry(x$result)
    attr(x$result, "prediction_log") <- d_log
    attr(x$result, "failed") <- FALSE
  }
  # Yes error
  if (!is.null(x$error)) {
    warning("#########################################################\n",
            x$error,
            "#########################################################")
    x$result <- bind_cols(tibble(a = 0, b = 0)[0, ],
                          attr(mod, "original_data_str"))
    names(x$result)[1:2] <- c(mi$target, paste0("predicted_", mi$target))

    d_log$error_message <- x$error$message
    d_log$error_call <- as.character(x$error$call)[1] # function name
    attr(x$result, "prediction_log") <- d_log
    attr(x$result, "failed") <- TRUE
  }
  return(x$result)
}

#' @title
#' Defunct
#' @description Defunct
#' @param ... Defunct
start_prod_logs <- function(...) {
  .Defunct("predict.model_list")
}

#' @title
#' Defunct
#' @description Defunct
#' @param ... Defunct
stop_prod_logs <- function(...) {
  .Defunct("predict.model_list")
}

#' @title
#' Defunct
#' @description Defunct
#' @param ... Defunct
catalyst_test_deploy_in_prod <- function(...) {
  .Defunct("predict.model_list")
}
