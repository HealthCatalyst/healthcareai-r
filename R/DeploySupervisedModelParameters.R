# Import the common functions.
library(R6)

#' DeploySupervisedModelParameters class to set up parameters required to deploy SupervisedModel classes
#'
#' @description This step allows one to create deploy models on your data
#' and helps determine which performs best.
#' @docType class
#' @importFrom R6 R6Class
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{HCRTools}}
#'
#' @export

DeploySupervisedModelParameters <- R6Class("DeploySupervisedModelParameters",

  #Public members
  public = list(

    #parameters
    df = NULL,
    type = "",
    grainCol = "",
    predictedCol = NULL,
    testWindowCol = "",
    useSavedModel = FALSE,
    impute = TRUE,
    debug = FALSE,

    model = NULL,
    cores = 4,
    sqlConn = NULL,
    destSchemaTable = NULL,
    rfmtry = "",
    trees = 201,

    #Constructor
    initialize = function () {
    }
  )

)
