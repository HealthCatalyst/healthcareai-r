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
    "\n=======================================",
    "\n"
  )
  write(the_log, filename, append = TRUE)
  return(d)
}

#' Sets defaults for telemetry tibble.
#' @noRd
set_inital_telemetry <- function(m) {
  mi <- extract_model_info(m)
  d <- tibble::tibble(
    loaded_from = mi$from_rds,
    model_name = mi$model_name,
    outcome_variable = mi$target,
    predict_time = Sys.time(),
    predictions_made = FALSE,
    n_predictions = NA,
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
    error_call = NA)

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
  d$prediction_mean <- round(pred_summary$Mean, 3)
  d$prediction_min <- round(pred_summary$Min., 3)
  d$prediction_q1 <- round(pred_summary$`1st Qu.`, 3)
  d$prediction_median <- round(pred_summary$Median, 3)
  d$prediction_q3 <- round(pred_summary$`3rd Qu.`, 3)
  d$prediction_max <- round(pred_summary$Max., 3)
  d$missingness_mean <- round(missingness$Mean, 3)
  d$missingness_min <- round(missingness$Min., 3)
  d$missingness_q1 <- round(missingness$`1st Qu.`, 3)
  d$missingness_median <- round(missingness$Median, 3)
  d$missingness_q3 <- round(missingness$`3rd Qu.`, 3)
  d$missingness_max <- round(missingness$Max., 3)
  return(d)
}

#' @title
#' Depreciated
#' @param ... Depreciated
start_prod_logs <- function(...) {
  # Create file name and open connection
  file_name <- paste0("deployConsoleLog",
                      "_",
                      format(Sys.time(), paste("%Y-%m-%d_%H.%M.%OS", 3, sep = "")),
                      ".txt")
  closeAllConnections() # clean up connections before creating a new one.
  file_connection <- file(description = file_name, open = "wt")

  # Set console output and messages (special type of output) to get logged.
  sink(file = file_connection)
  sink(file = file_connection, type = "message")

  # Confirm working directory
  print(paste("Working directory is", getwd()))
  print(paste("Logging to", file_name))
}

#' @title
#' Depreciated
#' @param ... Depreciated
stop_prod_logs <- function(...) {
  # Stop writing to the file
  sink(type = "message")
  sink()
}

#' @title
#' Depreciated
#' @param ... Depreciated
catalyst_test_deploy_in_prod <- function(...) {
  print("If you're reading this in the log file,
    deployment is ready to go in prod.")
  library(healthcareai)
  print(paste("Healthcareai version: ", utils::packageVersion("healthcareai")))
}
