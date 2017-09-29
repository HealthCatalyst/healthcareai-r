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
#' @seealso \code{\link{KmeansClustering}}
#'
#' @export

UnsupervisedModelParams <- R6Class("UnsupervisedModelParams",
                                   
  #Public members
  public = list(
    
    #parameters
    df = NA,
    debug = FALSE,
    impute = FALSE,
    numOfClusters = NULL,
    labelCol = "",
    grainCol = "",
    cores = 4,
    usePCA = FALSE,
    numOfPCA = NULL,
    
    #Constructor
    initialize = function() {
    }
  )
)
