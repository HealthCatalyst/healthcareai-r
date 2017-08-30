#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a Lasso model, based on
#' your data. Lasso is a linear model, best suited for linearly separable data. It's fast
#' to train and often a good starting point.
#' @docType class
#' @usage LassoDevelopment(type, df, grainCol, predictedCol, impute,
#' debug, cores, modelName)
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import pROC
#' @importFrom R6 R6Class
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
#' @param modelName Optional string. Can specify the model name. If used, you must load the same one in the deploy step.
#' @section Methods: 
#' The above describes params for initializing a new lassoDevelopment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new lasso development class using the 
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
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{RandomForestDevelopment}}
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
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' cat(proc.time() - ptm,"\n")
#'
#' #### Example using csv data ####
#' library(healthcareai)
#' # setwd('C:/Your/script/location') # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "healthcareai")
#' # Replace csvfile with '/path/to/yourfile'
#' df <- read.csv(file = csvfile, header = TRUE, na.strings = c("NULL", "NA", ""))
#'
#' head(df)
#'
#' df$PatientID <- NULL
#'
#' set.seed(42)
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
#' cat(proc.time() - ptm,"\n")
#'
#' \donttest{
#' #### Example using SQL Server data #### This example requires: 1) That you alter
#' #### your connection string / query
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
#' cat(proc.time() - ptm,"\n")
#' }
#'
#' @export

LassoDevelopment <- R6Class("LassoDevelopment",
  
  # Inheritance
  inherit = SupervisedModelDevelopment,
  
  # Private members
  private = list(
    # Data related
    dfTrainTemp = NA,
    dfTestTemp = NA,

    # Groups for the grouped Lasso model
    group = NA,
    
    # Fit model and lamda values
    fitGrLasso = NA,
    indLambda1se = NA,
    lambda1se = NA,
    modFmla = NA,
    modMat = NA,
    predictions = NA,
    
    algorithmShortName = "lasso",
    
    # Performance metrics
    ROCPlot = NA,
    PRCurvePlot = NA,
    AUROC = NA,
    AUPR = NA,
    RMSE = NA,
    MAE = NA
    
    # functions
  ),
  
  # Public members
  public = list(
    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)
    },

    getPredictions = function(){
      "Returns predictions from test data."
      return(private$predictions)
    },

    # Override: build Grouped Lasso model
    buildModel = function() {
      private$dfTrainTemp <- private$dfTrain
      private$dfTestTemp <- private$dfTest
      
      # Create a model formula, without the predicted variable, for use in
      # creating the model matrix.
      private$modFmla <-
        as.formula(paste("~", 
                   paste(names(private$dfTrainTemp[, !(colnames(private$dfTrainTemp) == self$params$predictedCol)]),
          collapse = "+"
        )))
      
      # Create the model matrix, without the intercept column, to be used in the
      # grouped Lasso function.
      private$modMat <-
        model.matrix(private$modFmla, data = private$dfTrainTemp)[, -1]
      
      # Make sure the dependent variable is numeric in both the train and
      # test set. If it is a factor, the first level alphabetically will be
      # set to 0 and the other level will be set to 1.
      if (is.factor(private$dfTrainTemp[[self$params$predictedCol]])) {
        private$dfTrainTemp[[self$params$predictedCol]] <-
          ifelse(private$dfTrainTemp[[self$params$predictedCol]] == levels(private$dfTrainTemp[[self$params$predictedCol]])[1],
                 0,
                 1)
      }
      
      if (is.factor(private$dfTestTemp[[self$params$predictedCol]])) {
        private$dfTestTemp[[self$params$predictedCol]] <-
          ifelse(private$dfTestTemp[[self$params$predictedCol]] == levels(private$dfTestTemp[[self$params$predictedCol]])[1], 
                0, 
                1)
      }
      
      # Creating the groups for the grouped Lasso model.
      # Factor variables have a group that is one less than the number of levels.
      # Everything else has length one.
      # The length of this vector should be the same as the number of columns in modMat.
      private$group <- rep(1:(ncol(private$dfTrainTemp) - 1),
        times <- sapply(private$dfTrainTemp[, !(names(private$dfTrainTemp) == self$params$predictedCol)],
                 function(x)
                 ifelse(is.factor(x), length(levels(x)) - 1, 1)))
      
      if (length(private$group) != ncol(private$modMat)) {
        stop('There is a mismatch in group definition and model matrix definition. Check the way the groups are defined versus 
          the size of the original data using debug <- TRUE')
        # This message should likely be refined, perhaps something different for
        # greater than or less than ...
      }
      
      # Generate fit grLasso object
      familyModuleName <- ""
      if (self$params$type == 'classification') {
        familyModuleName <- "binomial"
      } else if (self$params$type == 'regression') {
        familyModuleName <- "gaussian"
      }
      
      private$fitGrLasso <-
        cv.grpreg(
          X = private$modMat,
          y = private$dfTrainTemp[[self$params$predictedCol]],
          group = private$group,
          #lambda = can enter values here,
          #  but we will use default
          family = familyModuleName,
          penalty = "grLasso",
          nfolds = 5
        )
      
      # add column names to the model object for deploy's performPredictions
      private$fitGrLasso$modFmla <- private$modFmla
      private$fitGrLasso$modMat <- private$modMat
    },
    
    # Predict results
    performPrediction = function() {
      # Index of largest lambda within one cvse of the lambda with lowest cve:
      # These are sorted from largest to smallest lambda, hence pulling the
      # minimum index.
      private$indLambda1se <- min(which(private$fitGrLasso$cve <= (private$fitGrLasso$cve + private$fitGrLasso$cvse)[private$fitGrLasso$min]))
      
      # Largest lambda within one cvse of the lambda with lowest cve (ie. lambda
      # to use in final fit):
      private$lambda1se <- private$fitGrLasso$lambda[private$indLambda1se]
      
      # Predictions (in terms of probability)
      private$predictions <- predict(object = private$fitGrLasso,
                                     X = model.matrix(private$modFmla, data = private$dfTestTemp)[,-1],
                                     lambda = private$lambda1se,
                                     type = "response")
      
      if (isTRUE(self$params$debug)) {
        cat("Rows in prob prediction: ", nrow(private$predictedVals),"\n")
        if (self$params$type == 'classification') {
          cat("First 10 raw classification probability predictions","\n")
          cat(round(private$predictions[1:10], 2),"\n")
        } else if (self$params$type == 'regression') {
          cat("First 10 raw regression value predictions","\n")
          cat(round(private$predictions[1:10], 2),"\n")
        }
      }
    },
    
    # Generate performance metrics
    generatePerformanceMetrics = function() {
      ytest <- as.numeric(private$dfTest[[self$params$predictedCol]])
      
      calcObjList <-
        calculatePerformance(private$predictions, ytest, self$params$type)
      
      # Make these objects available for plotting and unit tests
      private$ROCPlot <- calcObjList[[1]]
      private$PRCurvePlot <- calcObjList[[2]]
      private$AUROC <- calcObjList[[3]]
      private$AUPR <- calcObjList[[4]]
      private$RMSE <- calcObjList[[5]]
      private$MAE <- calcObjList[[6]]
      
      if (isTRUE(self$params$printResults)) {
        cat("Grouped Lasso coefficients:","\n")
        print(private$fitGrLasso$fit$beta[, private$indLambda1se])
      }
      
      if (isTRUE(self$params$varImp)) {
        imp = names(private$dfTrainTemp[, !(colnames(private$dfTrainTemp) == self$params$predictedCol)])[predict(
          private$fitGrLasso,
          private$modMat,
          type = "groups",
          lambda = private$lambda1se
        )]
        cat("Variables with non-zero coefficients: ", imp,"\n")
        
      }
      
      return(invisible(private$fitGrLasso))
    },
    
    # Override: run prediction
    run = function() {
      # Start default logit (for row-wise var importance)
      # can be replaced with LIME-like functionality
      super$fitGeneralizedLinearModel()
      
      # Build Model
      self$buildModel()
      
      # save model
      super$saveModel(fitModel = private$fitGrLasso)
      
      # Perform prediction
      self$performPrediction()
      
      # Generate performance metrics
      self$generatePerformanceMetrics()
    },
    
    getROC = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        cat("ROC is not created because the column you're predicting is not binary","\n")
        return(NULL)
      }
      return(private$ROCPlot)
    },
    
    getPRCurve = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        cat("PR Curve is not created because the column you're predicting is not binary","\n")
        return(NULL)
      }
      return(private$PRCurvePlot)
    },
    
    getAUROC = function() {
      return(private$AUROC)
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