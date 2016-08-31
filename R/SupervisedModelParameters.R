# Import the common functions.
library(R6)

SupervisedModelParameters <- R6Class("SupervisedModelParameters",

  #Public members
  public = list(

    #parameters
    df = NA,
    grainCol = NA,
    predictedCol = NA,
    type = NA,
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
