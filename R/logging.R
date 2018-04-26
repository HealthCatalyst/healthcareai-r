#' Create or append log files from predict
#' @noRd
log_predictions <- function(filename, from_rds, target, n_preds, trained_time,
                            model_name, pred_summary, missingness) {

  missingness <- summary(missingness$percent_missing) %>% bind_rows()

  d <- tibble::tibble(
    loaded_from = from_rds,
    predictions_made = Sys.time(),
    model_name = model_name,
    outcome_variable = target,
    n_predictions = n_preds,
    days_since_trained =
      round(difftime(Sys.time(), trained_time, units = "days"), 1),
    prediction_mean = round(pred_summary$Mean, 3),
    prediction_min = round(pred_summary$Min., 3),
    prediction_q1 = round(pred_summary$`1st Qu.`, 3),
    prediction_median = round(pred_summary$Median, 3),
    prediction_q3 = round(pred_summary$`3rd Qu.`, 3),
    prediction_max = round(pred_summary$Max., 3),
    missingness_mean = round(missingness$Mean, 3),
    missingness_min = round(missingness$Min., 3),
    missingness_q1 = round(missingness$`1st Qu.`, 3),
    missingness_median = round(missingness$Median, 3),
    missingness_q3 = round(missingness$`3rd Qu.`, 3),
    missingness_max = round(missingness$Max., 3)
  )

  the_log <- paste0(
    "Model loaded from: ", d$loaded_from,
    "\n\t- Model predictions made: ", d$predictions_made,
    "\n\t- Model name: ", d$model_name,
    "\n\t- Variable predicted: ", d$outcome_variable,
    "\n\t- Number predictions: ", d$n_predictions,
    "\n\t- Days since model trained: ", d$days_since_trained,
    "\nSummary of predictions: ",
    "\n\t- Mean: ", d$prediction_mean,
    "\n\t- Minimum: ", d$prediction_min,
    "\n\t- 1st Quartile: ", d$prediction_q1,
    "\n\t- Median: ", d$prediction_median,
    "\n\t- 3rd Quartile: ", d$prediction_q3,
    "\n\t- Maximum: ", d$prediction_max,
    "\nSummary of missingness in new data (%):",
    "\n\t- Mean: ", d$missingness_mean,
    "\n\t- Minimum: ", d$missingness_min,
    "\n\t- 1st Quartile: ", d$missingness_q1,
    "\n\t- Median: ", d$missingness_median,
    "\n\t- 3rd Quartile: ", d$missingness_q3,
    "\n\t- Maximum: ", d$missingness_max,
    "\n"
  )

  write(the_log, filename, append = TRUE)
  return(d)
}

#' @title
#' Sets console logging to a file in the working directory.
#'
#' @description Logs all console messages and output to a timestamped file
#' called, "deployConsoleLog..." in the working directory. This is meant to be
#' used in production to monitor console output on ETL servers.
#' @export
#' @seealso \code{\link{stop_prod_logs}}
#' @examples
#' \dontrun{
#' # Object Attribute Base should contain a script like this:
#' # Move to working directory
#' setwd("Prod Server/Machine Learning/MyProject")
#'
#' # Start console logging
#' start_prod_logs()
#'
#' # Deploy -------------------------------------------------
#' # Test deployment. Comment/delete when working
#' catalyst_test_deploy_in_prod()
#'
#' # Deploy model
#' source("myDeployScript.R")
#'
#' # Stop console logging
#' stop_prod_logs()
#' }
start_prod_logs <- function() {
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
#' Stops all console logging.
#'
#' @description Stops all console logging. This is meant to be used in
#' production to monitor console output on ETL servers.
#' @export
#' @seealso \code{\link{start_prod_logs}}
stop_prod_logs <- function() {
  # Stop writing to the file
  sink(type = "message")
  sink()
}

#' @title
#' Test function to check that the production environment is active.
#'
#' @description This is meant to test the production environment on ETL servers.
#' Use it instead of a long running deploy script.
#' @export
#' @seealso \code{\link{start_prod_logs}}
#' @seealso \code{\link{stop_prod_logs}}
catalyst_test_deploy_in_prod <- function() {
  print("If you're reading this in the log file,
    deployment is ready to go in prod.")
  library(healthcareai)
  print(paste("Healthcareai version: ", utils::packageVersion("healthcareai")))
}
