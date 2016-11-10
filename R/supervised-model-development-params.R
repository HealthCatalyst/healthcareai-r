# Import the common functions.
library(R6)

#' SupervisedModelDevelopmentParams class to set up parameters required to build SupervisedModel classes
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @importFrom R6 R6Class
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#'
#' @export

SupervisedModelDevelopmentParams <- R6Class("SupervisedModelDevelopmentParams",

  #Public members
  public = list(

    #parameters
    df = NULL,
    grainCol = "",
    predictedCol = NULL,
    personCol = "",
    groupCol = NULL,
    type = "",
    impute = FALSE,
    debug = FALSE,
    varImp = TRUE,
    printResults = TRUE,
    cores = 4,
    tune = FALSE,
    numberOfTrees = 201,

    #Constructor
    initialize = function() {
    }
  )
)
