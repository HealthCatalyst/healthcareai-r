#' @title
#' Function to initialize and populate the SupervisedModelDevelopmentParams each 
#' time a unit test is run.
#'
#' @description Initialize and populate SupervisedModelDevelopmentParams
#' @param df A data frame to use with the new supervised model.
#' @return Supervised Model Development Params class
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
initializeParamsForTesting <- function(df) {
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$personCol = NULL
  p$impute = TRUE
  p$debug = FALSE
  p$cores = 1
  p$tune = FALSE
  p$trees = 201
  return(p)
}

#' @title
#' Function to suppress specific warnings in unit tests
#'
#' @description This function allows one to suppress one or more specific 
#' warnings.  The intended purpose is to suppress warning messages that are 
#' expected when running specific unit tests, but are unrelated to the 
#' functionality being tested.
#' @param code A piece of code to run, for which the warnings should be 
#' suppressed
#' @param wRegexps A vector of regular expressions corresponding to the 
#' warnings that should be suppressed
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
ignoreSpecWarn <- function(code, wRegexps) {
  h <- function(w) {
    for (r in wRegexps) {
      if (any(grepl(r, w))) {
        invokeRestart("muffleWarning")
      }
    }
  }
  withCallingHandlers(code, warning = h)
}

#' @title
#' Function to skip specific tests if MSSQL/specific databases not set up on 
#' user's machine.
#'
#' @description This function is used in the testing files where R is used to 
#' write to MSSQL.  It first checks for a connection to MSSQL.  If no connection
#' to MSSQL, it will skip the test.  If there is a good connection but the
#' specific database is not present it will skip the test as well.
#' @param tableName What table to look for if connection is good. Entered as a 
#' string.
#' @param connString Connection string to use when trying to connect to MSSQL
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
skip_if_no_MSSQL <- function(tableName, connString) {
  if (class(try((DBI::dbConnect(odbc::odbc(),
                                .connection_string = connString)), 
                silent = TRUE)) == "try-error") {
    testthat::skip("No DB connection made")
  } else if (DBI::dbExistsTable(conn = DBI::dbConnect(odbc::odbc(),
                                                      .connection_string = 
                                                      connString), 
                                name = tableName) == FALSE) {
    testthat::skip("No DB found")
  }
}