#' Deploy a production-ready predictive RandomForest model
#'
#' @description This step allows one to
#' \itemize{
#' \item Load a saved model from \code{\link{RandomForestDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server
#' }
#' @docType class
#' @usage XGBoostDeployment(type, df, grainCol, testWindowCol, 
#' predictedCol, impute, debug)
#' @import caret
#' @import doParallel
#' @import xgboost
#' @importFrom dplyr mutate
#' @import magrittr
#' @importFrom R6 R6Class
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param testWindowCol Y or N. This column dictates the split between model 
#' training and test sets. Those rows with N in this column indicate the 
#' training set while those that have Y indicate the test set
#' @param predictedCol Column that you want to predict. If you're doing
#' classification then this should be Y/N.
#' @param impute For training df, set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @export
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 


XGBoostDeployment <- R6Class("XGBoostDeployment",
  #Inheritance
  inherit = SupervisedModelDeployment,

  #Private members
  private = list(

    # variables
    outDf = NA,
    
    fitXGB = NA,
    predictions = NA,
    params = list(),

    # functions
    # Perform prediction
    performPrediction = function() {
      cat('Initializing XGBoost Deploy...','\n')
      private$predictions <- caret::predict.train(object = private$fitXGB,
                                                  newdata = private$dfTestTemp,
                                                  type = 'prob')
      private$predictions <- private$predictions[,2]
      
      if (isTRUE(self$params$debug)) {
        cat('Rows in regression prediction: ', length(private$predictions), '\n')
        cat('First 10 raw regression predictions (with row # first)', '\n')
        print(round(private$predictions[1:10],2))
      }
    },

    calculateOrderedFactors = function() {
      # Calculate ordered factors of importance for each row's prediction
      private$orderedFactors <- t(sapply
                                  (1:nrow(private$multiplyRes),
                                  function(i)
pcolnames(private$multiplyRes[order(private$multiplyRes[i, ],
                                                                        decreasing = TRUE)])))

      if (isTRUE(self$params$debug)) {
        cat('Data frame after getting column importance ordered', '\n')
        print(private$orderedFactors[1:10, ])
      }
    },

    createDf = function() {
      dtStamp <- as.POSIXlt(Sys.time())

      # Combine grain.col, prediction, and time to be put back into SAM table
      private$outDf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtStamp,                           # LastLoadDTS
        private$grainTest,                 # GrainID
        private$predictions,               # PredictedProbab
        private$orderedFactors[, 1:3])     # Top 3 Factors

      predictedResultsName = ""
      if (self$params$type == 'classification') {
        predictedResultsName = "PredictedProbNBR"
      } else if (self$params$type == 'regression') {
        predictedResultsName = "PredictedValueNBR"
      }
      colnames(private$outDf) <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol,
        predictedResultsName,
        "Factor1TXT",
        "Factor2TXT",
        "Factor3TXT"
      )

      if (isTRUE(self$params$debug)) {
        cat('Dataframe with predictions:', '\n')
        cat(str(private$outDf), '\n')
      }
    }
  ),

  #Public members
  public = list(
    #Constructor
    #p: new SupervisedModelDeploymentParams class object,
    #   i.e. p = SupervisedModelDeploymentParams$new()
    initialize = function(p) {
      cat('Initializing XGBoost Deploy...','\n')
      super$initialize(p)
      print(head(self$dfTestTemp))
    },

    #Override: deploy the model
    deploy = function() {
      cat('Loading XGB Model...','\n')

      # Try to load the model
      tryCatch({
        load("rmodel_probability_XGB.rda") # Produces fit object (for probability)
        private$fitXGB <- fitObj
       }, error = function(e) {
        # temporary fix until all models are working.
        stop('You must use a saved model. Run XGBoost development to train 
              and save the model, then XGBoost deployment to make predictions.
              See ?XGBoostDevelopment')
      })
      
      # Predict
      private$performPrediction()

      # Calculate Ordered Factors
      private$calculateOrderedFactors()

      # create dataframe for output
      private$createDf()
    },
    
    # Surface outDf as attribute for export to Oracle, MySQL, etc
    getOutDf = function() {
      return(private$outDf)
    }
  )
)
