#' @title
#' Function to initialize and populate the SupervisedModelDevelopmentParams each 
#' time a unit test is run.
#'
#' @description Initialize and populate SupervisedModelDevelopmentParams
#' @param df A data frame to use with the new supervised model.
#' @return Supervised Model Development Params class
#' 
#' @export
#' @references \url{http://healthcare.ai}
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
  p$numberOfTrees = 201
  return(p)
}