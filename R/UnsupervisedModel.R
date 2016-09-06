# Import the common functions.
library(R6)
library(caret)

source('R/common.R')
source('R/UnsupervisedModelParameters.R')

UnsupervisedModel <- R6Class("UnsupervisedModel",

  #Private members
  private = list(

    ###########
    # Variables

    clustersOnCores = NA,

    ###########
    # Functions

    registerClustersOnCores = function () {
      if (self$params$cores > 1) {
        suppressMessages(library(doParallel))
        private$clustersOnCores <-
          makeCluster(self$params$cores)
        registerDoParallel(private$clustersOnCores)
      }
    },

    stopClustersOnCores = function () {
      if (self$params$cores > 1) {
        stopCluster(private$clustersOnCores)
        registerDoSEQ()
      }
    },

    #Set config parameters for the algorithm
    setConfigs = function (p) {
      self$params <- UnsupervisedModelParameters$new()

      if (!is.null(p$df))
        self$params$df <- p$df

      if (!is.null(p$type)) {
        self$params$type <- p$type

        # validation on type string values

      }

      if (!is.null(p$debug))
        self$params$debug <- p$debug

      if (!is.null(p$printResults))
        self$params$printResults <- p$printResults

      if (!is.null(p$cores))
        self$params$cores <- p$cores

      #Set additional settings
      if (isTRUE(self$params$debug)) {

      }

      #Set up clusters on cores
      private$registerClustersOnCores()

    },

    #Load data
    loadData = function () {

    }

  ),

  #Public members
  public = list(
    ###########
    # Variables

    #parameters
    params = NA,

    ###########
    # Functions

    #Constructor
    #p: new UnsuperviseModelParameters class object, i.e. p = UnsuperviseModelParameters$new()
    initialize = function (p) {

      #Set config parameters
      private$setConfigs(p)

      #Load data
      private$loadData()

    },

    #Build the Model
    buildModel = function () {

    },

    #Run the Model
    run = function () {

    }

  )

)
