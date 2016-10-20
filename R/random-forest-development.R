# Import the common functions.
source('R/common.R')
source('R/supervised-model-development.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a random forest model, based on
#' your data.
#' @docType class
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @import RODBC
#' @param object of SuperviseModelParameters class for $new() constructor
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param predictedCol Column that you want to predict.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://hctools.org/}
#' @seealso \code{\link{LassoDevelopment}}
#' @seealso \code{\link{LinearMixedModelDevelopment}}
#' @seealso \code{\link{HCRTools}}
#' @examples
#'
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' data(iris)
#' head(iris)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = iris
#' p$type = 'regression'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'Sepal.Width'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' print(proc.time() - ptm)
#'
#' #### Example using csv data ####
#' library(HCRTools)
#' #setwd("C:/Your/script/location") # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "HCRTools")
#'
#' df <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings =  c('NULL', 'NA', ""))
#'
#' head(df)
#'
#' df$InTestWindowFLG <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'regression'
#' p$impute = TRUE
#' p$grainCol = 'PatientID'
#' p$predictedCol = 'A1CNBR'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' print(proc.time() - ptm)
#'
#' #### Example using SQL Server data ####
#' # This example requires:
#' #    1) That you alter your connection string / query
#'
#' ptm <- proc.time()
#' library(HCRTools)
#' library(RODBC)
#'
#' connection.string = "
#' driver={SQL Server};
#' server=localhost;
#' database=SAM;
#' trusted_connection=true
#' "
#'
#' query = "
#' SELECT
#'  [PatientEncounterID]
#' ,[PatientID]
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' ,[InTestWindowFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' WHERE InTestWindowFLG = 'N'
#' "
#'
#' df <- selectData(connection.string, query)
#' head(df)
#'
#' df$InTestWindowFLG <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = 'PatientID'
#' p$predictedCol = 'ThirtyDayReadmitFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' # Plot ROC
#' rocs = list(rf$getROC(), lasso$getROC())
#' names = c('Random Forest','Lasso')
#' legendLoc = 'bottomright'
#' plotROCs(rocs, names, legendLoc)
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' lasso$getCutOffs(tpr=.8)
#'
#' print(proc.time() - ptm)
#'
#' @export

RandomForestDevelopment <- R6Class("RandomForestDevelopment",

  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Grid object for grid search
    grid = NA,

    # Git random forest model
    fitRF = NA,

    predictions = NA,

    # Performance metrics
    confMatrix = NA,
    ROC = NA,
    AUC = NA,
    rmse = NA,
    mae = NA,
    perf = NA,
    prevalence = NA,

    # Start of functions
    buildGrid = function() {
      if (isTRUE(self$params$tune)) {
        optimal <- NA

        # Create reasonable gridsearch for mtry
        # This optimal value comes from randomForest documentation
        # TODO: make mtry calc a function (incl both tune and not)
        if (self$params$type == 'classification') {
          optimal <- floor(sqrt(ncol(private$dfTrain)))
        }
        else if (self$params$type == 'regression') {
          optimal <- max(floor(ncol(private$dfTrain)/3), 1)
        }

        mtryList <- c(optimal - 1, optimal, optimal + 1)
        # Make it such that lowest mtry is 2
        if (length(which(mtryList < 0)) > 0) {
          mtryList <- mtryList + 3
        } else if (length(which(mtryList == 0)) > 0) {
          mtryList <- mtryList + 2
        } else if (length(which(mtryList == 1)) > 0) {
          mtryList <- mtryList + 1
        }

        print(paste(c('Performing grid search across these mtry values: ',
                      mtryList), collapse = " "))

        private$grid <-  data.frame(mtry = mtryList) # Number of features/tree
      }
      else {
        if (self$params$type == 'classification') {
          private$grid <- data.frame(.mtry = floor(sqrt(ncol(private$dfTrain))))
        }
        else if (self$params$type == 'regression') {
          private$grid <- data.frame(.mtry = max(floor(ncol(private$dfTrain)/3), 1))
        }
      }
    }
  ),

  # Public members
  public = list(

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)

      if (!is.null(p$tune)) {
        self$params$tune = p$tune
      }
      if (!is.null(p$numberOfTrees)) {
        self$params$numberOfTrees = p$numberOfTrees
      }
    },

    # Override: build RandomForest model
    buildModel = function() {
      trainControlParams.method <- ""
      trainControlParams.number <- 1

      rfTrainParams.metric <- ""

      # Build grid for grid search
      private$buildGrid()

      if (isTRUE(self$params$tune)) {
        trainControlParams.method <- "CV"
        trainControlParams.number <- 5
      } else {
        trainControlParams.method <- "none"
        trainControlParams.number <- 1
      }

      # Create train control object
      train.control <- NA
      if (self$params$type == 'classification') {

        train.control <- trainControl(
          method = trainControlParams.method,
          number = trainControlParams.number,
          verboseIter = isTRUE(self$params$debug),
          classProbs = TRUE,
          summaryFunction = twoClassSummary
        )

        rfTrainParams.metric <- "ROC"
      }
      # Regression
      else if (self$params$type == 'regression') {

        train.control <- trainControl(
          method = trainControlParams.method,
          number = trainControlParams.number,
          verboseIter = isTRUE(self$params$debug)
        )

        rfTrainParams.metric <- "RMSE"
      }

      # Train RandomForest
      adjustedY <- NA
      if (self$params$type == 'classification') {
        adjustedY <- factor(private$dfTrain[[self$params$predictedCol]])
      }
      else if (self$params$type == 'regression') {
        adjustedY <- private$dfTrain[[self$params$predictedCol]]
      }
      private$fitRF <- train(
        x = private$dfTrain[ ,!(colnames(private$dfTrain) ==
                                  self$params$predictedCol)],
        y = adjustedY,
        method = "ranger",
        importance = "impurity",
        metric = rfTrainParams.metric,
        num.trees = self$params$numberOfTrees,
        tuneGrid = private$grid,
        trControl = train.control
      )
    },

    # Perform prediction
    performPrediction = function() {
      if (self$params$type == 'classification') {
        private$predictions <- predict(object = private$fitRF,
                                      newdata = private$dfTest,
                                      type = 'prob')
      } else if (self$params$type == 'regression') {
        private$predictions <- predict(private$fitRF, newdata = private$dfTest)
      }
    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {

      if (self$params$type == 'classification') {
        predictProb <- private$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', nrow(predictProb)))
          print('First 10 raw classification probability predictions')
          print(round(predictProb[1:10,2],2))
        }

        ytest <- as.numeric(private$dfTest[[self$params$predictedCol]])
        pred <- prediction(predictProb[,2], ytest)
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        predictClass <- predict(private$fitRF, newdata = private$dfTest)

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in discrete prediction: ', nrow(predictProb)))
          print('First 10 raw classification discrete predictions')
          print(predictClass[1:10])
        }

        private$ROC <- roc(ytest~predictProb[,2])
        private$AUC <- auc(private$ROC)

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('AUC: ', round(private$AUC, 2)))
          print(paste0('95% CI AUC: (', round(ci(private$AUC)[1],2),
                       ',',
                       round(ci(private$AUC)[3],2), ')'))
        }
      }

      # Regression
      else if (self$params$type == 'regression') {
        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ',
                       length(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }

        ytest <- as.numeric(private$dfTest[[self$params$predictedCol]])

        # Error measures
        private$rmse <- sqrt(mean((ytest - private$predictions) ^ 2))
        private$mae <- mean(abs(ytest - private$predictions))

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 8)))
          print(paste0('MAE: ', round(private$mae, 8)))
        }
      }

      private$stopClustersOnCores()

      if (isTRUE(self$params$varImp)) {
        self$params$varImp <- varImp(private$fitRF, top = 20)
        print(self$params$varImp)
      }

      return(invisible(private$fitRF))
    },

    # Override: run RandomForest algorithm
    run = function() {

      # Build Model
      self$buildModel()

      # Perform prediction
      self$performPrediction()

      # Generate performance metrics
      self$generatePerformanceMetrics()
    },

    getROC = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        print("ROC is not created because the column you're predicting is not binary")
        return(NULL)
      }

      return(private$ROC)
    },

    getAUC = function() {
      return(private$AUC)
    },

    getRMSE = function() {
      return(private$rmse)
    },

    getMAE = function() {
      return(private$mae)
    },

    getPerf = function() {
      return(private$perf)
    },

    getCutOffs = function(tpr) {
      # Get index of when true-positive rate is > tpr
      indy <- which(as.numeric(unlist(private$perf@y.values)) > tpr)

      # Correpsonding probability cutoff value (ie when category falls to 1)
      print('Corresponding cutoff for 0/1 fallover:')
      print(private$perf@alpha.values[[1]][indy[1]])

      # Corresponding false-positive rate
      print('Corresponding false-positive rate:')
      print(private$perf@x.values[[1]][indy[1]][[1]])
    }
  )
)
