#' @title
#' Sets console logging to a file in the working directory.
#'
#' @description Logs all console messages and output to a timestamped file called, "deployConsoleLog..."
#' in the working directory. This is meant to be used in production to monitor console output on ETL servers.
#' @export
#' @seealso \code{\link{stopProdLogs}}
#' @examples
#' # Object Attribute Base should contain a script like this:
#' # Move to working directory 
#' # setwd("Prod Server/Machine Learning/MyProject")
#' 
#' Start console logging
#' startProdLogs()
#' 
#' # Deploy -------------------------------------------------
#' # Test deployment. Comment/delete when working
#' catalyst_test_deploy_in_prod()
#'
#' # Deploy model
#' # source("myDeployScript.R")
#' 
#' # Stop console logging
#' stopProdLogs()
startProdLogs = function() {
  # Create file name and open connection
  file_name <- paste0("deployConsoleLog", 
                      "_",
                      format(Sys.time(), paste("%Y-%m-%d_%H.%M.%OS", 3, sep = "")), 
                      ".txt")
  closeAllConnections() # clean up connections before creating a new one.
  file_connection <- file(description = paste0(getwd(),"/",log_name), open = "wt")

  # Set console output and messages (special type of output) to get logged.
  sink(file = file_connection)
  sink(file = file_connection, type = "message")

  # Confirm working directory
  print(paste("Working directory is", getwd()))
  print(paste("Logging to", log_name))
}

#' @title
#' Stops all console logging.
#'
#' @description Stops all console logging. This is meant to be used in production to monitor console output on ETL servers.
#' @export
#' @seealso \code{\link{startProdLogs}}
stopProdLogs = function() {
  # Stop writing to the file
  sink(type = "message")
  sink()
}

#' @title
#' Test function to check that the production environment is active.
#'
#' @description This is meant to test the production environment on ETL servers. Use it instead of a long running deploy script.
#' @export
#' @seealso \code{\link{startProdLogs}}
#' @seealso \code{\link{stopProdLogs}}
catalyst_test_deploy_in_prod = function() {
  print("If you're reading this in the log file, deployment is ready to go in prod.")
  library(healthcareai)
  print(paste("Healthcareai version: ", packageVersion("healthcareai")))
}