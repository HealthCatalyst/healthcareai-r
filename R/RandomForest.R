# Import the common functions.
source('R/common.R')
source('R/SupervisedModel.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
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
#' @param grain.col The dataframe's column that has IDs pertaining to the grain
#' @param predicted.col Column that you want to predict.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{DeploySupervisedModel}}
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
#' p <- SupervisedModelParameters$new()
#' p$df = iris
#' p$type = 'regression'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'Sepal.Width'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
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
#' csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
#'
#' df <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' head(df)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = FALSE
#' p$grainCol = ''
#' p$predictedCol = 'SalariedFlag'
#' p$debug = TRUE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' rf$getCutOffs(tpr=0.8)
#' print(proc.time() - ptm)
#'
#'
#' #### Example using SQL Server data ####
#' # This example requires:
#' #    1) That your local SQL Server has AdventureWorks2012 installed
#'
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' connection.string = "
#' driver={SQL Server};
#' server=localhost;
#' trusted_connection=true
#' "
#'
#' query = "
#' SELECT
#'  [OrganizationLevel]
#' ,[MaritalStatus]
#' ,[Gender]
#' ,IIF([SalariedFlag]=0,'N','Y') AS SalariedFlag
#' ,[VacationHours]
#' ,[SickLeaveHours]
#' FROM [AdventureWorks2012].[HumanResources].[Employee]
#' "
#'
#' df <- SelectData(connection.string, query)
#' head(df)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'SalariedFlag'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' #Plot ROCs from both supervised model classes
#' plot(rf$getROC(), col = "red", legacy.axes=TRUE, mar=c(4, 4, 3, 2)+.1)
#' title(main = "ROC")
#' legend("bottomright",
#'        c("RandomForest"),
#'        cex = 0.8,
#'        col = c("red"),
#'        lty = 1:2,
#'        inset = .1)
#'
#' print(proc.time() - ptm)
#'
#' @export


RandomForest <- R6Class("RandomForest",

  #Inheritance
  inherit = SupervisedModel,

  #Private members
  private = list(

    # grid object for grid search
    grid = NA,

    # fit random forest model
    fit.rf = NA,

    #predictions
    predictions = NA,

    #performance metrics
    confMatrix = NA,
    ROC = NA,
    AUC = NA,
    rmse = NA,
    mae = NA,
    perf = NA,
    prevalence = NA,

    #functions
    buildGrid = function() {

      if (isTRUE(self$params$tune)) {
        optimal = NA

        # Create reasonable gridsearch for mtry
        # This optimal value comes from randomForest documentation
        if (self$params$type == 'classification') {
          optimal = floor(sqrt(ncol(private$dfTrain)))
        }
        else if (self$params$type == 'regression') {
          optimal = max(floor(ncol(private$dfTrain)/3), 1)
        }


        mtry_list = c(optimal - 1, optimal, optimal + 1)
        # Make it such that lowest mtry is 2
        if (length(which(mtry_list < 0)) > 0) {
          mtry_list = mtry_list + 3
        } else if (length(which(mtry_list == 0)) > 0) {
          mtry_list = mtry_list + 2
        } else if (length(which(mtry_list == 1)) > 0) {
          mtry_list = mtry_list + 1
        }

        print(paste(c('Performing grid search across these mtry values: ',
                      mtry_list), collapse = " "))

        private$grid <-  data.frame(mtry = mtry_list) # Number of features/tree
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

  #Public members
  public = list(

    #Constructor
    #p: new SuperviseModelParameters class object, i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)

      #variables
      if (!is.null(p$tune)) {
        self$params$tune = p$tune
      }
      if (!is.null(p$numberOfTrees)) {
        self$params$numberOfTrees = p$numberOfTrees
      }

    },

    #Override: build RandomForest model
    buildModel = function() {

      trainControlParams.method = ""
      trainControlParams.number = 1

      rfTrainParams.metric = ""

      # build grid for grid search
      private$buildGrid()

      if (isTRUE(self$params$tune)) {
        trainControlParams.method = "CV"
        trainControlParams.number = 5
      } else {
        trainControlParams.method = "none"
        trainControlParams.number = 1
      }

      # create train control object
      train.control = NA
      if (self$params$type == 'classification') {

        train.control <- trainControl(
          method = trainControlParams.method,
          number = trainControlParams.number,
          verboseIter = isTRUE(self$params$debug),
          classProbs = TRUE,
          summaryFunction = twoClassSummary
        )

        rfTrainParams.metric = "ROC"
      }
      #regression
      else if (self$params$type == 'regression') {

        train.control <- trainControl(
          method = trainControlParams.method,
          number = trainControlParams.number,
          verboseIter = isTRUE(self$params$debug)
        )

        rfTrainParams.metric = "RMSE"
      }

      #Train RandomForest
      adjustedY = NA
      if (self$params$type == 'classification') {
        adjustedY = factor(private$dfTrain[[self$params$predictedCol]])
      }
      else if (self$params$type == 'regression') {
        adjustedY = private$dfTrain[[self$params$predictedCol]]
      }
      private$fit.rf = train(
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
        private$predictions = predict(object = private$fit.rf,
                                      newdata = private$dfTest,
                                      type = 'prob')
      }
      else if (self$params$type == 'regression') {
        private$predictions = predict(private$fit.rf, newdata = private$dfTest)
      }

    },

    #generate performance metrics
    generatePerformanceMetrics = function() {

      if (self$params$type == 'classification') {
        predictprob <- private$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', nrow(predictprob)))
          print('First 10 raw classification probability predictions')
          print(round(predictprob[1:10,2],2))
        }

        ytest = as.numeric(private$dfTest[[self$params$predictedCol]])
        pred <- prediction(predictprob[,2], ytest)
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        predictclass = predict(private$fit.rf, newdata = private$dfTest)

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in discrete prediction: ', nrow(predictprob)))
          print('First 10 raw classification discrete predictions')
          print(predictclass[1:10])
        }

        private$ROC = roc(ytest~predictprob[,2])
        private$AUC = auc(private$ROC)

        #Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('AUC: ', round(private$AUC, 2)))
          print(paste0('95% CI AUC: (', round(ci(private$AUC)[1],2),
                       ',',
                       round(ci(private$AUC)[3],2), ')'))
        }
      }
      #regression
      else if (self$params$type == 'regression') {

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ',
                       length(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }

        ytest = as.numeric(private$dfTest[[self$params$predictedCol]])

        # error measures
        private$rmse = sqrt(mean((ytest - private$predictions)^2))
        private$mae = mean(abs(ytest - private$predictions))

        #Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 8)))
          print(paste0('MAE: ', round(private$mae, 8)))
        }

      }

      private$stopClustersOnCores()

      if (isTRUE(self$params$varImp)) {
        self$params$varImp <- varImp(private$fit.rf, top = 20)

        print(self$params$varImp)
        print(dotPlot(self$params$varImp))
      }

      return(invisible(private$fit.rf))
    },

    #Override: run RandomForest algorithm
    run = function() {

      # Build Model
      self$buildModel()

      # Perform prediction
      self$performPrediction()

      # Generate performance metrics
      self$generatePerformanceMetrics()
    },

    #get ROC
    getROC = function() {
      if (!IsBinary(self$params$df[[self$params$predictedCol]])) {
        print("ROC is not created because the column you're predicting is not binary")
        return(NULL)
      }

      return(private$ROC)
    },

    #getAUC
    getAUC = function() {
      return(private$AUC)
    },

    #getRMSE
    getRMSE = function() {
      return(private$rmse)
    },

    #getMAE
    getMAE = function() {
      return(private$mae)
    },

    #getPerformanceMetric
    getPerf = function() {
      return(private$perf)
    },

    #getCutOffs
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
