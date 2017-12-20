#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a random forest model, based on
#' your data. Random forest is an ensemble model, well suited for non-linear data. It's fast
#' to train and often a good starting point.
#' @docType class
#' @usage RandomForestDevelopment(type, df, grainCol, predictedCol, 
#' impute, debug, cores, trees, tune, modelName)
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param predictedCol Column that you want to predict. If you're doing
#' classification then this should be Y/N.
#' @param impute Set all-column imputation to T or F.
#' If T, this uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' Values are saved for deployment.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @param cores Number of cores you'd like to use. Defaults to 2.
#' @param trees Number of trees in the forest. Defaults to 201.
#' @param tune If TRUE, automatically tune model for better performance. Creates grid using mtry param and 5-fold 
#' cross validation. Note this takes longer.
#' @param modelName Optional string. Can specify the model name. If used, you must load the same one in the deploy step.
#' @section Methods: 
#' The above describes params for initializing a new randomForestDevelopment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new random forest development class using the 
#' parameters saved in \code{p}, documented above. This method loads, cleans, and prepares data for
#' model training. \cr
#' \emph{Usage:} \code{$new(p)}
#' @section \code{$run()}:
#' Trains model, displays feature importance and performance. \cr
#' \emph{Usage:}\code{$new()} 
#' @section \code{$getPredictions()}:
#' Returns the predictions from test data. \cr
#' \emph{Usage:} \code{$getPredictions()} \cr
#' @section \code{$getROC()}:
#' Returns the ROC curve object for \code{\link{plotROCs}}. Classification models only. \cr
#' \emph{Usage:} \code{$getROC()} \cr
#' @section \code{$getPRCurve()}:
#' Returns the PR curve object for \code{\link{plotPRCurve}}. Classification models only. \cr
#' \emph{Usage:} \code{$getROC()} \cr
#' @section \code{$getAUROC()}:
#' Returns the area under the ROC curve from testing for classification models. \cr
#' \emph{Usage:} \code{$getAUROC()} \cr
#' @section \code{$getRMSE()}:
#' Returns the RMSE from test data for regression models. \cr
#' \emph{Usage:} \code{$getRMSE()} \cr
#' @section \code{$getMAE()}:
#' Returns the RMSE from test data for regression models. \cr
#' \emph{Usage:} \code{$getMAE()} \cr
#' @section \code{$getVariableImportanceList()}:
#' Returns the variable importance list. \cr
#' \emph{Usage:} \code{$getVariableImportanceList(numTopVariables = NULL)} \cr
#'  Params: \cr
#'   - \code{numTopVariables:} The maximum number of variables to include. If no
#'   value is specified, all variables are used. \cr
#' @section \code{$plotVariableImportance()}:
#' Plots the variable importance list. \cr
#' \emph{Usage:} \code{$plotVariableImportance(numTopVariables = NULL)} \cr
#'  Params: \cr
#'   - \code{numTopVariables:} The maximum number of variables to include. If no
#'   value is specified, all variables are used. \cr
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{LassoDevelopment}}
#' @seealso \code{\link{LinearMixedModelDevelopment}}
#' @seealso \code{\link{selectData}}
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
#' # This query should pull only rows for training. They must have a label.
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

    predictions = NA,
    
    algorithmShortName = "RF",

    # Performance metrics
    ROCPlot = NA,
    PRCurvePlot = NA,
    AUROC = NA,
    AUPR = NA,
    RMSE = NA,
    MAE = NA,
    variableImportanceList = NA,

    # Start of functions
    
    buildGrid = function() {
      if (isTRUE(self$params$tune)) {
        
        # Create reasonable gridsearch for mtry
        ## Could make tuning length a parameter. For now just 5.
        mtry_length <- 5L
        nvar <- ncol(private$dfTrain) - 1L
        mtryList <- 
          if (nvar <= mtry_length) {
            mtryList <- 1:nvar
          } else {
            # Focus mtry search on smaller side of nvar
            mtryList <- unique(floor(seq(1, sqrt(nvar), length.out = mtry_length) ^ 2))
          }
        
        # Choose split rules
        if (self$params$type == 'classification') {
          ourSplitrule <- c('gini', 'extratrees')
        } else if (self$params$type == 'regression') {
          ourSplitrule <- c('variance', 'extratrees')
        }
        
        # Choose minimum node size
        min_node_size <- c(1L, 5L)
        
        # Build grid
        private$grid <- expand.grid(mtry = mtryList, 
                                    splitrule = ourSplitrule,
                                    min.node.size = min_node_size)
        message('Performing grid search over the following grid. This may take some ',
                'time, especially if your data frame is large.')
        print(private$grid)
        
      }
      else {  # If not tuning, use ranger's defaults
        if (self$params$type == 'classification') {
          private$grid <- data.frame(mtry = floor(sqrt(ncol(private$dfTrain))), 
                                     splitrule = 'gini',
                                     min.node.size = 1)
        }
        else if (self$params$type == 'regression') {
          private$grid <- data.frame(mtry = max(floor(ncol(private$dfTrain)/3), 1), 
                                     splitrule = 'variance',
                                     min.node.size = 5)
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
      if (!is.null(p$trees)) {
        self$params$trees = p$trees
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
        num.trees = self$params$trees,
        tuneGrid = private$grid,
        trControl = train.control
      )
      if (self$params$tune) {
        suppressWarnings(print(ggplot(private$fitRF)))
        message("The best performing hyperparameter combination was:\n")
        print(structure(private$fitRF$bestTune, row.names = ""))
        message("\n")
      }
    },

    # Perform prediction
    performPrediction = function() {
      if (self$params$type == 'classification') {
        private$predictions <- caret::predict.train(object = private$fitRF,
                                      newdata = private$dfTest,
                                      type = 'prob')
        private$predictions <- private$predictions[,2]
        names(private$predictions) <- row.names(private$dfTest) # add row nums
        
        if (isTRUE(self$params$debug)) {
          print(paste0('Number of predictions: ', nrow(private$predictions)))
          print('First 10 raw classification probability predictions')
          print(round(private$predictions[1:10],2))
        }
        
      } else if (self$params$type == 'regression') {
        private$predictions <- caret::predict.train(private$fitRF, newdata = private$dfTest)
        names(private$predictions) <- row.names(private$dfTest) # add row nums
        
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
      
      # Construct variable importance list
      impList <- caret::varImp(private$fitRF)$importance
      impList = data.frame(rownames(impList), impList$Overall)
      colnames(impList) = c("variable", "importance")
      # list in decreasing order
      impList = impList[order(impList$importance, decreasing = TRUE), ]
      row.names(impList) <- NULL
      private$variableImportanceList <- impList
      rm(impList)
      
      return(invisible(private$fitRF))
    },

    # Override: run RandomForest algorithm
    run = function() {
      
      # Start default logit (for row-wise var importance)
      # can be replaced with LIME-like functionality
      super$fitGeneralizedLinearModel()

      # Build Model
      self$buildModel()
      
      # save model
      super$saveModel(fitModel = private$fitRF)

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
    },
    
    getVariableImportanceList = function(numTopVariables = NULL) {
      top <- min(numTopVariables, nrow(private$variableImportanceList))
      if (!is.null(numTopVariables)) {
        if (top < numTopVariables) {
          warning(paste0("You requested ", 
                         numTopVariables, 
                         " variables but the random forest was only trained ",
                         "on ", 
                         top, 
                         " variables. Returning all variables."))
        }
      }
      return(private$variableImportanceList[1:top, ])
    },
    
    plotVariableImportance = function(numTopVariables = NULL) {
      # Get information for plot before changing margins
      vIL <- self$getVariableImportanceList(numTopVariables)
      last_row <- nrow(vIL)
      # have most important variables on top, as in list
      values <- vIL$importance[last_row:1]
      labels <- as.character(vIL$variable[last_row:1])
      maxLabelLength <- max(nchar(labels))
      limit = 23
      if (maxLabelLength > limit) {
        print(paste0("Long variable names have been abbreviated in the plot. ",
                       "The (ordered) labels are as follows:"))
        print(vIL)
        print("These can also be accessed via $getVariableImportanceList()")
        labels <- sapply(X = labels, FUN = function(label) {
          labelLength <- nchar(label)
          if (labelLength > limit) {
            label <- paste0(substring(label, 1, (limit - 3)/2), 
                            "...", 
                            substring(label, 
                                      labelLength - (limit - 3)/2, 
                                      labelLength))
          }
          return(label)
        })
        maxLabelLength <- limit
      }
      old_mai = par()$mai # save old margins
      # Change margins in tryCatch so that margins are always reset even if
      # plotting fails
      tryCatch({
        # indent plot so that variable names can be read, indentation depends
        # on the maximum label length
        par(mai = c(1, 1 + maxLabelLength*0.12, 1, 1))
        barplot(values,
                names.arg = labels,
                horiz = TRUE, las = 1, cex.names = 1, xlab = "Importance",
                main = "Random Forest Variable Importance", col = "navy")
      }, error = function(e) {
        message(e)
      }, finally = {
        par(mai = old_mai) # reset margins
      })
    }
  )
)
