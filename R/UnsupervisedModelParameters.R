# Import the common functions.
library(R6)

UnsupervisedModelParameters <- R6Class("UnsupervisedModelParameters",

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
