#' Create or append log files from predict
#' @noRd
log_predictions <- function(filename, target, n_preds, trained_time) {
  new_log <- paste0(
    "Model predictions made: ", Sys.time(),
    "\n\t- Variable predicted: ", target,
    "\n\t- Number predictions: ", n_preds,
    "\n\t- Days since model trained: ",
    round(difftime(Sys.time(), trained_time, units = "days"), 1), "\n"
  )
  if (!file.exists(filename))
    file.create(filename)
  con <- file(filename, "r+")
  old_log <- readLines(con)
  writeLines(c(new_log, old_log), con = con)
  close(con)
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
