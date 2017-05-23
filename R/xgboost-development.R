#' Compare predictive models, created on your data
#'
#' @description This step allows you to create an XGBoost model, based on
#' your data.
#' @docType class
#' @usage XGBoost Development(object, type, df, grainCol, predictedCol, 
#' impute, debug)
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
#' df$InTestWindowFLG <- NULL
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

XGBoostDevelopment <- R6Class("RandomForestDevelopment",

  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Grid object for grid search
    grid = NA,

    # fitLogit = NA,
    predictions = NA,
    test_label = NA,

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
      
        fitObj <- private$fitXBG
        save(fitObj, file = "rmodel_probability_XGB.rda")
      }

      # TODO: Cross validation and random search
  ),

  # Public members
  public = list(
    # xgboost specific placeholders
    xgb_trainMatrix = NA,
    xgb_testMatrix = NA,
    
    # Get xgb model
    fitXGB = NA,

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      # check that it's a multiclass type
      if (p$type != 'multiclass') {
        cat('XGBoost currently only supports "multiclass" type.', '\n')
      }

      set.seed(43)
      super$initialize(p)

      # TODO set up tuning to actually work.
      if (!is.null(p$tune)) {
        self$params$tune = p$tune
      }

      # print xgb params (for sanity)
      cat('xgb_params are:', '\n')
      cat(str(self$params$xgb_params), '\n')
    
    },

    # Prepare data for XGBoost
    xgbPrepareData = function() {
      cat('Preparing data...', '\n')
      # XGB requires data.matrix format, not data.frame.
      # R factors are 1 indexed, XGB is 0 indexed, so we must subtract 1 from the labels. They must be numeric.
      temp_train_data <- data.matrix(private$dfTrain[ ,!(colnames(private$dfTrain) == self$params$predictedCol)])
      temp_train_label <- data.matrix(as.numeric(private$dfTrain[[self$params$predictedCol]])) - 1 
      self$xgb_trainMatrix <- xgb.DMatrix(data = temp_train_data, label = temp_train_label)
      rm(temp_train_data, temp_train_label) # clean temp variables

      temp_test_data <- data.matrix(private$dfTest[ ,!(colnames(private$dfTest) == self$params$predictedCol)])
      temp_test_label <- data.matrix(as.numeric(private$dfTest[[self$params$predictedCol]])) - 1 
      self$xgb_testMatrix <- xgb.DMatrix(data = temp_test_data, label = temp_test_label) 
      private$test_label <- temp_test_label # save for confusion matrix and output
      rm(temp_test_data, temp_test_label) # clean temp variables

    },

    getPredictions = function(){
      cat('Retrieving raw predictions...', '\n')
      return(private$predictions)
    },

    buildModel = function() {
      cat('Building model...', '\n')
      self$fitXGB <- xgb.train(params = self$params$xgb_params,
                             data = self$xgb_trainMatrix,
                             nrounds = self$params$xgb_nrounds)
    },

    # Perform prediction
    performPrediction = function() {
      cat('Generating predictions...', '\n')
      temp_predictions <- predict(self$fitXGB, newdata = self$xgb_testMatrix, reshape = TRUE)

      # Build prediction output
      private$predictions <- temp_predictions %>% 
        data.frame() %>%
        mutate(predicted_label = max.col(.),
               true_label = private$test_label + 1)

      # Set column names to match input targets
      colnames(private$predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames

      # Set up a mapping for the values themselves
      from <- 1:self$params$xgb_numberOfClasses
      to <- self$params$xgb_targetNames
      map = setNames(to,from)
      private$predictions$predicted_label <- map[private$predictions$predicted_label] # note square brackets
      private$predictions$true_label <- map[private$predictions$true_label] 

      # Prepare output 
      private$predictions <- cbind(private$grainTest, private$predictions)
      colnames(private$predictions)[1] <- self$params$grainCol
    },

    # Generate performance metrics
    generateConfusionMatrix = function() {
      cat('Generating confusion matrix...', '\n')
      caret::confusionMatrix(private$predictions$true_label,
                private$predictions$predicted_label,
                mode = "everything")
    },

   # Run XGBoost Multiclass
   run = function() {
      # Prepare data for xgboost
      self$xgbPrepareData()

      # Build Model
      self$buildModel()
      
      # save model
      private$saveModel()

      # Perform prediction
      self$performPrediction()

      # Generate confusion matrix
      self$generateConfusionMatrix()
    }
  )
)
