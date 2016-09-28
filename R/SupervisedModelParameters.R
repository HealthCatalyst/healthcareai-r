# Import the common functions.
library(R6)

#' SupervisedModelParameters class to set up parameters required to build SupervisedModel classes
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @importFrom R6 R6Class
#' @references \url{http://healthcareml.org/}
#' @seealso \code{\link{HCRTools}}
#'
#' @export

SupervisedModelParameters <- R6Class("SupervisedModelParameters",

  #Public members
  public = list(

    #parameters
    df = NULL,
    grainCol = "",
    predictedCol = NULL,
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
    initialize = function () {
    }
  )

)
