# Import the common functions.
library(R6)

#' SupervisedModelDevelopmentParams class to set up parameters required to build SupervisedModel classes
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @importFrom R6 R6Class
#' @references \url{http://healthcareai-r.readthedocs.io}
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
    cores = 2,
    tune = FALSE,
    trees = 201,
    xgb_numberOfClasses = NA,
    xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = NA,
                  "max_depth" = 6, 
                  "eta" = 0.1, 
                  "silent" = 0, 
                  "nthread" = 1),
    xgb_nrounds = 50,
    xgb_targetNames = NA,
    modelName = NULL,

    #Constructor
    initialize = function() {
    }
  )
)
