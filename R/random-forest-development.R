#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a random forest model, based on
#' your data.
#' @docType class
#' @usage RandomForestDevelopment(object, type, df, grainCol, predictedCol, 
#' impute, debug)
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @param object of SuperviseModelParameters class for $new() constructor
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param predictedCol Column that you want to predict. If you're doing
#' classification then this should be Y/N.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://hctools.org/}
#' @seealso \code{\link{LassoDevelopment}}
#' @seealso \code{\link{LinearMixedModelDevelopment}}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(healthcareai)
#'
#' data(iris)
#' head(iris)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- iris
#' p$type <- "regression"
#' p$impute <- TRUE
#' p$grainCol <- ""
#' p$predictedCol <- "Sepal.Width"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' set.seed(42)
#' # Run RandomForest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' print(proc.time() - ptm)
#'
#' #### Example using csv data ####
#' library(healthcareai)
#' # setwd('C:/Your/script/location') # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#'
#' # Replace csvfile with 'your/path'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#'
#' head(df)
#'
#' df$PatientID <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "regression"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$predictedCol <- "A1CNBR"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' set.seed(42) 
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' print(proc.time() - ptm)
#'
#' \donttest{
#' #### Example using SQL Server data ####
#'
#' ptm <- proc.time()
#' library(healthcareai)
#'
#' connection.string <- "
#' driver={SQL Server};
#' server=localhost;
#' database=SAM;
#' trusted_connection=true
#' "
#'
#' query <- "
#' SELECT
#' [PatientEncounterID]
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#'
#' df <- selectData(connection.string, query)
#' head(df)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "classification"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$predictedCol <- "ThirtyDayReadmitFLG"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Run Lasso
#' lasso <- LassoDevelopment$new(p)
#' lasso$run()
#'
#' set.seed(42) 
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' # Plot ROC
#' rocs <- list(rf$getROC(), lasso$getROC())
#' names <- c("Random Forest", "Lasso")
#' legendLoc <- "bottomright"
#' plotROCs(rocs, names, legendLoc)
#' 
#' # Plot PR Curve
#' rocs <- list(rf$getPRCurve(), lasso$getPRCurve())
#' names <- c("Random Forest", "Lasso")
#' legendLoc <- "bottomleft"
#' plotPRCurve(rocs, names, legendLoc)
#'
#' print(proc.time() - ptm)
#' }
#'
#' @export

RandomForestDevelopment <- R6Class("RandomForestDevelopment",

  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Grid object for grid search
    grid = NA,

    # Get random forest model
    fitRF = NA,
    fitLogit = NA,

    predictions = NA,

    # Performance metrics
    ROCPlot = NA,
    PRCurvePlot = NA,
    AUROC = NA,
    AUPR = NA,
    RMSE = NA,
    MAE = NA,

    # Start of functions
    saveModel = function() {
      if (isTRUE(self$params$debug)) {
        print('Saving model...')
      }
      
        fitLogit <- private$fitLogit
        fitObj <- private$fitRF
        
        save(fitLogit, file = "rmodel_var_import_RF.rda")
        save(fitObj, file = "rmodel_probability_RF.rda")
      },
    
    # this function must be in here for the row-wise predictions.
    # can be replaced when LIME-like functionality is complete.
    fitGeneralizedLinearModel = function() {
      if (isTRUE(self$params$debug)) {
        print('generating fitLogit for row-wise guidance...')
      }
      
      if (self$params$type == 'classification') {
        private$fitLogit <- glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          family = binomial(link = "logit"),
          metric = "ROC",
          control = list(maxit = 10000),
          trControl = caret::trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
        )
        
      } else if (self$params$type == 'regression') {
        private$fitLogit <- glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          metric = "RMSE",
          control = list(maxit = 10000)
        )
      }

      # Add factor levels (calculated in SMD) to fitLogit object
      private$fitLogit$factorLevels <- private$factorLevels 
    },
    
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
      set.seed(43)
      super$initialize(p)

      if (!is.null(p$tune)) {
        self$params$tune = p$tune
      }
      if (!is.null(p$numberOfTrees)) {
        self$params$numberOfTrees = p$numberOfTrees
      }
    },
    getPredictions = function(){
      return(private$predictions)
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

        train.control <- caret::trainControl(
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

        train.control <- caret::trainControl(
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
        private$predictions <- caret::predict.train(object = private$fitRF,
                                      newdata = private$dfTest,
                                      type = 'prob')
        private$predictions <- private$predictions[,2]
        
        if (isTRUE(self$params$debug)) {
          print(paste0('Number of predictions: ', nrow(private$predictions)))
          print('First 10 raw classification probability predictions')
          print(round(private$predictions[1:10],2))
        }
        
      } else if (self$params$type == 'regression') {
        private$predictions <- caret::predict.train(private$fitRF, newdata = private$dfTest)
        
        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ',
                       length(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }
      }

    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {
      
      ytest <- as.numeric(private$dfTest[[self$params$predictedCol]])

      calcObjList <- calculatePerformance(private$predictions, 
                                           ytest, 
                                           self$params$type)
      
      # Make these objects available for plotting and unit tests
      private$ROCPlot <- calcObjList[[1]]
      private$PRCurvePlot <- calcObjList[[2]]
      private$AUROC <- calcObjList[[3]]
      private$AUPR <- calcObjList[[4]]
      private$RMSE <- calcObjList[[5]]
      private$MAE <- calcObjList[[6]]
      
      print(caret::varImp(private$fitRF, top = 20))
      
      return(invisible(private$fitRF))
    },

    # Override: run RandomForest algorithm
    run = function() {
      
      # Start default logit (for row-wise var importance)
      # can be replaced with LIME-like functionality
      private$fitGeneralizedLinearModel()

      # Build Model
      self$buildModel()
      
      # save model
      private$saveModel()

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
      return(private$ROCPlot)
    },
    
    getPRCurve = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        print("PR Curve is not created because the column you're predicting is not binary")
        return(NULL)
      }
      return(private$PRCurvePlot)
    },

    getAUROC = function() {
      return(private$AUROC)
    },

    getAUPR = function() {
      return(private$AUPR)
    },
    
    getRMSE = function() {
      return(private$RMSE)
    },

    getMAE = function() {
      return(private$MAE)
    }, 
    
    getCutOffs = function() {
      warning("`getCutOffs` is deprecated. Please use `generateAUC` instead. See 
              ?generateAUC", call. = FALSE)
    }
  )
)
