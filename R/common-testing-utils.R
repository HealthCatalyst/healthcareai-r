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
#' Function to skip specific tests if they are not being run on Appveyor.
#'
#' @description This function will skip a test if it's not being run on Appveyor. 
#' Used for SQL Server related tests, since we don't know if the environment will be
#' present. These tests are are run on Appveyor.
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
skip_on_not_appveyor = function() {
    if (identical(Sys.getenv("APPVEYOR"), "True")) {
        return()
    }
    testthat::skip("Not on Appveyor")
}