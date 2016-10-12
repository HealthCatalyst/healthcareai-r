source('R/common.R')
source('R/supervised-model-development.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import lme4
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @import RODBC
#' @param object of SuperviseModelParameters class for $new() constructor
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The data frame's ID column pertaining to the grain
#' @param personCol The data frame's ID column pertaining to the person/patient
#' @param predictedCol Column that you want to predict.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://healthcareml.org/}
#' @seealso \code{\link{HCRTools}}
#' @examples
#'
#' ### Built-in example; Doing classification
#' library(HCRTools)
#' library(lme4)
#'
#' df <- sleepstudy
#'
#' str(df)
#'
#' # Create binary column for classification
#' df$ReactionFLG <- ifelse(df$Reaction > 300, 'Y','N')
#' df$Reaction <- NULL
#'
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$personCol = 'Subject' # Think of this as PatientID
#' p$predictedCol = 'ReactionFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' ### Doing regression
#' library(HCRTools)
#' library(lme4)
#'
#' # SQL query and connection goes here - see SelectData function.
#'
#' df <- sleepstudy
#'
#' # Add GrainID, which is equivalent to PatientEncounterID
#' df$GrainID <- seq.int(nrow(df))
#'
#' str(df)
#'
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'regression'
#' p$impute = TRUE
#' p$grainCol = 'GrainID'  # Think of this as PatientEnounterID
#' p$personCol = 'Subject' # Think of this as PatientID
#' p$predictedCol = 'Reaction'
#' p$debug = TRUE
#' p$cores = 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
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
#' df$InTestWindowFLG <- NULL
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = 'PatientEncounterID'
#' p$personCol = 'PatientID'
#' p$predictedCol = 'ThirtyDayReadmitFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' # Run Lasso
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
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
#' p$grainCol = 'PatientEncounterID'
#' p$personCol = 'PatientID'
#' p$predictedCol = 'ThirtyDayReadmitFLG'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' # Run Lasso
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' Lasso$getCutOffs(tpr=.8)
#'
#' print(proc.time() - ptm)
#' @export

LinearMixedModelDevelopment <- R6Class("LinearMixedModelDevelopment",


  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Mixed model-specific datasets
    trainTest = NA,
    lmmTrain = NA,
    lmmTest = NA,

    # Git random forest model
    fitLmm = NA,

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

    # Start of functions
    buildDataset = function(){
      # TODO Soon: Prepare data according to InTestWindow column, in case
      # user wants to predict for row that's not the last in the person group
      # Combine test/train (which was randomly generated in base class)

      private$trainTest <- rbind(private$dfTrain,private$dfTest)

      if (isTRUE(self$params$debug)) {
        print('Re-combined train/test for MM specific use')
        print(str(private$trainTest))
      }

      # TODO Later: figure out why orderering in sql query is better auc than internal
      # ordering. http://stackoverflow.com/a/1296745/5636012
      # If ordering using with, access PersonID col via df[[PersonID]]

      # Split out test/train by taking last row of each PersonID for test set
      # TODO Soon: do this split using the InTestWindowCol
      private$lmmTrain <- data.table::setDT(private$trainTest)[, .SD[1:.N-1], by = eval(self$params$personCol)]
      private$lmmTest <- data.table::setDT(private$trainTest)[, .SD[.N], by = eval(self$params$personCol)]

      if (isTRUE(self$params$debug)) {
        print('Mixed model-specific training set after creation')
        print(str(private$lmmTrain))
        print('Mixed model-specific test set after creation')
        print(str(private$lmmTest))
      }

    },

    # Override: build model
    # Linear Mixed model (random intercept with fixed mean)
    buildModel = function() {

      # Start build formula by grabbing column names
      colList <- colnames(private$lmmTrain)

      # Remove target col from list
      colList <- colList[colList != self$params$predictedCol]

      # Remove grain col from list
      colList <- colList[colList != self$params$grainCol]

      # Remove random-effects col from list
      fixedColsTemp <- colList[colList != self$params$personCol]

      # Collapse columns in list into a large string of cols
      fixedCols <- paste(fixedColsTemp, "+ ", collapse = "")

      formula <- paste0(self$params$predictedCol, " ~ ",
                        fixedCols,
                        "(1|", self$params$personCol, ")")

      if (isTRUE(self$params$debug)) {
        print('Formula to be used:')
        print(formula)
        print('Training the general linear mixed-model...')
        print('Using random intercept with fixed mean...')
      }

      if (self$params$type == 'classification') {
        private$fitLmm = glmer(formula = formula,
                                data = private$lmmTrain,
                                family = binomial(link = 'logit'))
      }
      else if (self$params$type == 'regression') {
        private$fitLmm = lmer(formula = formula,
                                data = private$lmmTrain)
      }
    },

    # Perform prediction
    performPrediction = function() {

      if (self$params$type == 'classification') {
        private$predictions <- predict(object = private$fitLmm,
                                       newdata = private$lmmTest,
                                       allow.new.levels = TRUE,
                                       type = "response")
      }
      else if (self$params$type == 'regression') {
        private$predictions <- predict(object = private$fitLmm,
                                       newdata = private$lmmTest,
                                       allow.new.levels = TRUE)
      }
    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {

      if (self$params$type == 'classification') {
        predictProb <- private$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', nrow(predictProb)))
          print('First 10 raw classification probability predictions')
          print(round(predictProb[1:10],2))
        }

        ytest = as.numeric(private$lmmTest[[self$params$predictedCol]])
        pred <- prediction(predictProb, ytest)
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        private$ROC = pROC::roc(ytest~predictProb)
        private$AUC = pROC::auc(private$ROC)

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

        ytest = as.numeric(private$lmmTest[[self$params$predictedCol]])

        # Error measures
        private$rmse = sqrt(mean((ytest - private$predictions) ^ 2))
        private$mae = mean(abs(ytest - private$predictions))

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 8)))
          print(paste0('MAE: ', round(private$mae, 8)))
        }
      }

      private$stopClustersOnCores()

      return(invisible(private$fitLmm))
    },

    # Override: run RandomForest algorithm
    run = function() {

      self$buildDataset()

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
