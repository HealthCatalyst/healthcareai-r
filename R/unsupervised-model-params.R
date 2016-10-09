# Import the common functions.
library(R6)

UnsupervisedModelParams <- R6Class("UnsupervisedModelParams",

  #Public members
  public = list(

    #parameters
    df = NA,
    type = NA,
    debug = FALSE,
    printResults = TRUE,
    cores = 4,

    #Constructor
    initialize = function() {
    }
  )
)
