# Import the common functions.
source('R/common.R')
source('R/supervised-model-development.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a Lasso model, based on
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
#' @references \url{http://healthcareml.org/}
#' @seealso \code{\link{RandomForestDevelopment}}
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
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
#'
#' # Run Random Forest
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
#' csvfile <- system.file("extdata", "DiabetesClinical.csv", package = "HCRTools")
#'
#' df <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings =  c('NULL', 'NA', ""))
#'
#' head(df)
#'
#' df$PatientID <- NULL
#' df$InTestWindowFLG <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = 'PatientEncounterID'
#' p$predictedCol = 'ThirtyDayReadmitFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
#'
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' Lasso$getCutOffs(tpr=.8)
#' print(proc.time() - ptm)
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
#' FROM [SAM].[dbo].[DiabetesClinical]
#' WHERE InTestWindowFLG = 'N'
#' "
#'
#' df <- selectData(connection.string, query)
#' head(df)
#'
#' df$PatientID <- NULL
#' df$InTestWindowFLG <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = 'PatientEncounterID'
#' p$predictedCol = 'ThirtyDayReadmitFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
#'
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' Lasso$getCutOffs(tpr=.8)
#'
#' print(proc.time() - ptm)
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

  	# Models for formula and matrix
  	modFmla = NA,
  	modMat = NA,

  	# Groups for the grouped Lasso model
  	group = NA,

  	# Fit model and lamda values
  	fitGrLasso = NA,
  	indLambda1se = NA,
  	lambda1se = NA,

  	predictions = NA,

  	# Performance metrics
  	confMatrix = NA,
  	ROC = NA,
  	AUC = NA,
  	rmse = NA,
  	mae = NA,
  	perf = NA,
  	prevalence = NA

  ),

  # Public members
  public = list(

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)
    },

    # Override: build Grouped Lasso model
    buildModel = function() {

      private$dfTrainTemp = private$dfTrain
      private$dfTestTemp = private$dfTest

      # Create a model formula, without the predicted variable, for use in
      # creating the model matrix.
      private$modFmla = as.formula(paste("~",paste(names(private$dfTrainTemp[ ,!(colnames(private$dfTrainTemp) == self$params$predictedCol)]),
                                           collapse = "+")))

      # Create the model matrix, without the intercept column, to be used in the
      # grouped Lasso function.
      private$modMat = model.matrix(private$modFmla, data = private$dfTrainTemp)[,-1]

      # Make sure the dependent variable is numeric in both the train and
      # test set. If it is a factor, the first level alphabetically will be
      # set to 0 and the other level will be set to 1.
      if (is.factor(private$dfTrainTemp[[self$params$predictedCol]])) {
        private$dfTrainTemp[[self$params$predictedCol]] =
          ifelse(private$dfTrainTemp[[self$params$predictedCol]] == levels(private$dfTrainTemp[[self$params$predictedCol]])[1],0,1)
      }

      if (is.factor(private$dfTestTemp[[self$params$predictedCol]])) {
        private$dfTestTemp[[self$params$predictedCol]] =
          ifelse(private$dfTestTemp[[self$params$predictedCol]] == levels(private$dfTestTemp[[self$params$predictedCol]])[1],0,1)
      }

      # Creating the groups for the grouped Lasso model.
      # Factor variables have a group that is one less than the number of levels.
      # Everything else has length one.
      # The length of this vector should be the same as the number of columns in modMat.
      private$group <- rep(1:(ncol(private$dfTrainTemp) - 1 ),
                  times <- sapply(private$dfTrainTemp[ ,!(colnames(private$dfTrainTemp) == self$params$predictedCol)],
                               function(x) ifelse(is.factor(x), length(levels(x)) - 1, 1)))

      if (length(private$group) != ncol(private$modMat)) {
        stop('There is a mismatch in group definition and model matrix definition')
        # This message should likely be refined, perhaps something different for
        # greater than or less than ...
      }

      # Generate fit grLasso object
      familyModuleName = ""
      if (self$params$type == 'classification')
        familyModuleName = "binomial"
      else if (self$params$type == 'regression')
        familyModuleName = "gaussian"

      private$fitGrLasso = cv.grpreg(X = private$modMat,
                                      y = private$dfTrainTemp[[self$params$predictedCol]],
                                      group = private$group,
                                      #lambda = can enter values here,
                                      #  but we will use default
                                      family = familyModuleName,
                                      penalty = "grLasso",
                                      nfolds = 5)

    },

    # Predict results
    performPrediction = function() {

      # Index of largest lambda within one cvse of the lambda with lowest cve:
      # These are sorted from largest to smallest lambda, hence pulling the
      # minimum index.
      private$indLambda1se = min(which(private$fitGrLasso$cve <= (private$fitGrLasso$cve + private$fitGrLasso$cvse)[private$fitGrLasso$min]))

      # Largest lambda within one cvse of the lambda with lowest cve (ie. lambda
      # to use in final fit):
      private$lambda1se = private$fitGrLasso$lambda[private$indLambda1se]

      # Predictions (in terms of probability)
      private$predictions = predict(object = private$fitGrLasso,
                                    X = model.matrix(private$modFmla, data = private$dfTestTemp)[,-1],
                                    lambda = private$lambda1se,
                                    type = "response")
    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {

      # Classification
      if (self$params$type == 'classification') {

        predictProb <- private$predictions

        # Prediction
        ytest = private$dfTestTemp[[self$params$predictedCol]]
        pred <- ROCR::prediction(private$predictions, ytest)

        # Performance
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        # NOTE THAT THE CLASS WILL BE 0 OR 1.
        predictClass = predict(object = private$fitGrLasso,
                               X = model.matrix(private$modFmla, data = private$dfTestTemp)[,-1],
                               lambda = private$lambda1se,
                               type = "class")

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', length(predictProb)))
          print('First 10 raw classification probability predictions')
          print(round(predictProb[1:10],2))
        }

        private$ROC = roc(ytest~predictProb)
        private$AUC = auc(private$ROC)

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('AUC: ', round(private$AUC, 2)))
          print(paste0('95% CI AUC: (', round(ci(private$AUC)[1],2), ',', round(ci(private$AUC)[3],2), ')'))
        }
      }
      # Regression
      else if (self$params$type == 'regression') {

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ', nrow(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }

        # Necessary to convert col to numeric, even though it's N/Y
        ytest = as.numeric(private$dfTestTemp[[self$params$predictedCol]])

        # Show error measures for Regression
        private$rmse = sqrt(mean((ytest - private$predictions) ^ 2))
        private$mae = mean(abs(ytest - private$predictions))

        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 2)))
          print(paste0('MAE: ', round(private$mae, 2)))
        }
      }

      if (isTRUE(self$params$printResults)) {
        print("Grouped Lasso coefficients:")
        print(private$fitGrLasso$fit$beta[,private$indLambda1se])
      }

      private$stopClustersOnCores()

      if (isTRUE(self$params$varImp)) {
        imp = names(private$dfTrainTemp[ ,!(colnames(private$dfTrainTemp) == self$params$predictedCol)])[predict(private$fitGrLasso, private$modMat,type = "groups", lambda = private$lambda1se)]
        print(paste0("Variables with non-zero coefficients: ",paste0(imp, collapse = ", ")))
      }

      return(invisible(private$fitGrLasso))
    },

    # Override: run prediction
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
