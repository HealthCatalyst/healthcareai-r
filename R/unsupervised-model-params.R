# Import the common functions.
library(R6)

#' UnsupervisedModelParams class to set up parameters required to build UnsupervisedModel classes
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @importFrom R6 R6Class
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#'
#' @export

UnsupervisedModelParams <- R6Class("UnsupervisedModelParams",
                                   
  #Public members
  public = list(
    
    #parameters
    df = NA,
    type = NA,
    debug = FALSE,
    printResults = TRUE,
    featureReduction = FALSE,
    numOfClusters = NULL,
    labelCol = NULL,
    grainCol = "",
    cores = 4,
    
    #Constructor
    initialize = function() {
      
    }
    
  )
)
