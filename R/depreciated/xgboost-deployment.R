#' Deploy a production-ready predictive XGBoost model
#'
#' @description This step allows one to
#' \itemize{
#' \item Automatically load a saved model from \code{\link{XGBoostDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server or CSV
#' }
#' @docType class
#' @usage XGBoostDeployment(type, df, grainCol,
#' predictedCol, impute, debug, cores, modelName)
#' @import caret
#' @import doParallel
#' @import xgboost
#' @importFrom R6 R6Class
#' @param type The type of model (must be multiclass)
#' @param df Dataframe whose columns are used for new predictions. Data structure should match development as 
#' much as possible. Number of columns, names, types, grain, and predicted must be the same.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param predictedCol Column that you want to predict.
#' @param impute For training df, set all-column imputation to T or F.
#' If T, this uses values calculated in development.
#' F leads to removal of rows containing NULLs and is not recommended.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @param cores Number of cores you'd like to use.  Defaults to 2.
#' @param modelName Optional string. Can specify the model name. If used, you must load the same one in the deploy step.
#' @section Methods: 
#' The above describes params for initializing a new XGBoostDeployment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new XGBoost deployment class using the 
#' parameters saved in \code{p}, documented above. This method loads, cleans, and prepares data for
#' generating predictions. \cr
#' \emph{Usage:} \code{$new(p)}
#' @section \code{$deploy()}:
#' Generate new predictions and prepare the output dataframe. \cr
#' \emph{Usage:} \code{$deploy()} 
#' @section \code{$getPredictions()}:
#' Return the grain and predictions for each class. \cr
#' \emph{Usage:} \code{$getPredictions()} \cr
#' @section \code{$getOutDf()}:
#' Returns a dataframe containing the grain column, the top 3 probabilities for each row, 
#' and the classes associated with those probabilities. \cr
#' \emph{Usage:} \code{$getOutDf()} 
#' @export
#' @seealso \code{\link{healthcareai}}
#' @seealso \code{\link{writeData}}
#' @seealso \code{\link{selectData}}
#' @examples
#' #### Example using csv dataset ####
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # 1. Load data. Categorical columns should be characters.
#' # can delete these system.file lines in your work
#' csvfile <- system.file("extdata", 
#'                       "dermatology_multiclass_data.csv", 
#'                       package = "healthcareai")

#'# Read in CSV; replace csvfile with 'path/file'
#'df <- read.csv(file = csvfile, 
#'               header = TRUE, 
#'              stringsAsFactors = FALSE,
#'               na.strings = c("NULL", "NA", "", "?"))
#' 
#' str(df) # check the types of columns
#' dfDeploy <- df[347:366,] # reserve 20 rows for deploy step.
#' 
#' # 2. Develop and save model (saving is automatic)
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "multiclass"
#' p$impute <- TRUE
#' p$grainCol <- "PatientID"
#' p$predictedCol <- "target"
#' p$debug <- FALSE
#' p$cores <- 1
#' # xgb_params must be a list with all of these things in it. 
#' # if you would like to tweak parameters, go for it! 
#' # Leave objective and eval_metric as they are.
#' p$xgb_params <- list("objective" = "multi:softprob",
#'                      "eval_metric" = "mlogloss",
#'                      "max_depth" = 6, # max depth of each learner
#'                      "eta" = 0.1, # learning rate
#'                      "silent" = 0, # verbose output when set to 1
#'                      "nthread" = 2) # number of processors to use
#' 
#' # Run model
#' boost <- XGBoostDevelopment$new(p)
#' boost$run()
#' 
#' ## 3. Load saved model (automatic) and use DEPLOY to generate predictions. 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "multiclass"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientID"
#' p2$predictedCol <- "target"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' 
#' # Deploy model to make new predictions
#' boostD <- XGBoostDeployment$new(p2)
#' boostD$deploy()
#' 
#' # Get output dataframe for csv or SQL
#' outDf <- boostD$getOutDf()
#' head(outDf)
#' 
#' # If you want to write to sqlite:
#' # sqliteFile <- system.file("extdata",
#' #                          "unit-test.sqlite",
#' #                         package = "healthcareai")
#' # writeData(SQLiteFileName = sqliteFile,
#' #         df = outDf,
#' #         tableName = "dermatologyDeployMulticlassBASE")
#' 
#' # Write to CSV (or JSON, MySQL, etc) using plain R syntax
#' # write.csv(df,'path/predictionsfile.csv')
#' 
#' # Get raw predictions if you want
#' # rawPredictions <- boostD$getPredictions()
#' 
#' # If you have known labels, check your prediction accuracy like this:
#' # caret::confusionMatrix(true_label,
#' #              predicted_label,
#' #              mode = "everything")
#' 
#' print(proc.time() - ptm)
#' 
#' \donttest{
#' #### Example pulling from CSV and writing to SQL server ####
#' # This example requires you to first create a table in SQL Server
#' # If you prefer to not use SAMD, execute this in SSMS to create output table:
#' # CREATE TABLE [dbo].[dermatologyDeployClassificationBASE](
#' # [BindingID] [int] NULL,[BindingNM] [varchar](255) NULL,
#' # [LastLoadDTS] [datetime2](7) NULL,
#' # [PatientID] [decimal](38, 0) NULL,
#' # [PredictedProb1] [decimal](38, 2) NULL,
#' # [PredictedClass1] [varchar](255) NULL,
#' # [PredictedProb2] [decimal](38, 2) NULL,
#' # [PredictedClass2] [varchar](255) NULL,
#' # [PredictedProb3] [decimal](38, 2) NULL,
#' # [PredictedClass3] [varchar](255) NULL)
#' 
#' 
#' # 1. Load data. Categorical columns should be characters.
#' csvfile <- system.file("extdata", 
#'                        "dermatology_multiclass_data.csv", 
#'                        package = "healthcareai")
#' 
#' # Replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                stringsAsFactors = FALSE,
#'                na.strings = c("NULL", "NA", "", "?"))
#' 
#' str(df) # check the types of columns
#' dfDeploy <- df[347:366,] # reserve 20 rows for deploy step.
#' 
#' 
#' # 2. Develop and save model (saving is automatic)
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "multiclass"
#' p$impute <- TRUE
#' p$grainCol <- "PatientID"
#' p$predictedCol <- "target"
#' p$debug <- FALSE
#' p$cores <- 1
#' # xgb_params must be a list with all of these things in it. 
#' # if you would like to tweak parameters, go for it! 
#' # Leave objective and eval_metric as they are.
#' p$xgb_params <- list("objective" = "multi:softprob",
#'                    "eval_metric" = "mlogloss",
#'                    "max_depth" = 6, # max depth of each learner
#'                    "eta" = 0.1, # learning rate
#'                    "silent" = 0, # verbose output when set to 1
#'                    "nthread" = 2) # number of processors to use
#' 
#' # Run model
#' boost <- XGBoostDevelopment$new(p)
#' boost$run()
#' 
#' ## 3. Load saved model (automatic) and use DEPLOY to generate predictions. 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "multiclass"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientID"
#' p2$predictedCol <- "target"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' 
#' # Deploy model to make new predictions
#' boostD <- XGBoostDeployment$new(p2)
#' boostD$deploy()
#' 
#' # Get output dataframe for csv or SQL
#' outDf <- boostD$getOutDf()
#' head(outDf)
#' 
#' # Save the output to SQL server
#' 
#' connection.string <- "
#' driver={SQL Server};
#' server=localhost;
#' database=SAM;
#' trusted_connection=true
#' "
#' writeData(MSSQLConnectionString = connection.string,
#'        df = outDf,
#'        tableName = 'dermatologyDeployClassificationBASE')
#'        
#' # Get raw predictions if you want
#' # rawPredictions <- boostD$getPredictions()
#' print(proc.time() - ptm)
#' }

 XGBoostDeployment <- R6Class("XGBoostDeployment",
  #Inheritance
  inherit = SupervisedModelDeployment,

  #Private members
  private = list(

    # variables
    outDf = NA,
    test_label = NA,
    
    fitXGB = NA,
    predictions = NA,
    temp_predictions = NA,
    orderedProbs = NA,

    algorithmShortName = 'XGB',
    algorithmName = 'XGBoost',

    # functions
    # Prepare data for XGBoost
    xgbPrepareData = function() {
      cat('Preparing data...', '\n')
      # XGB requires data.matrix format, not data.frame.
      # R factors are 1 indexed, XGB is 0 indexed, so we must subtract 1 from the labels. They must be numeric.
      temp_test_data <- self$params$df[ ,!(colnames(self$params$df) == self$params$predictedCol)]
      temp_test_data[] <- lapply(temp_test_data, as.numeric)
      self$xgb_testMatrix <- xgb.DMatrix(data = data.matrix(temp_test_data)) 
      rm(temp_test_data) # clean temp variables

      # For multiclass xgboost initialization:
      # 1. Load the class names from development.
      # 2. Get the number of classes.
      # 3. Save the grain column for output.
      # Names
      self$params$xgb_targetNames <- private$fitXGB$xgb_targetNames
      # Number
      self$params$xgb_numberOfClasses <- length(self$params$xgb_targetNames)
      # Grain
      private$dfGrain <- self$params$df[[self$params$grainCol]]
      # prints
      if (isTRUE(self$params$debug)) {
        cat('Unique classes found:', '\n')
        print(self$params$xgb_targetNames)
        cat('Number of classes:', '\n')
        print(self$params$xgb_numberOfClasses)
      }
    },

    # Perform prediction
    performPrediction = function() {
      cat('Generating Predictions...','\n')
      private$temp_predictions <- predict(object = private$fitXGB,
                                  newdata = self$xgb_testMatrix,
                                  reshape = TRUE)
      
      # Build prediction output
      private$predictions <- as.data.frame(private$temp_predictions)
      private$predictions$predicted_label = max.col(private$predictions)
      
      # Set column names to match input targets
      colnames(private$predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames
      colnames(private$temp_predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames

      # Set up a mapping for the values themselves
      from <- 1:self$params$xgb_numberOfClasses
      to <- self$params$xgb_targetNames
      map = setNames(to,from)
      private$predictions$predicted_label <- map[private$predictions$predicted_label] # note square brackets

      # Prepare output 
      private$predictions <- cbind(private$grainTest, private$predictions)
      colnames(private$predictions)[1] <- self$params$grainCol
      
      if (isTRUE(self$params$debug)) {
        cat('Rows in multiclass prediction: ', length(private$predictions), '\n')
        cat('First 10 raw probability predictions (with row # first)', '\n')
        print(private$predictions[1:10,])
      }
    },

    calculateOrderedFactors = function() {
      cat('Ordering top probabilities...', '\n')
      # Calculate ordered factors of importance for each row's prediction
      # Get column indices of max values in each row
      nRows = dim(private$temp_predictions)[1]
      maxColInds <- t(sapply(1:nRows, function(i) 
        order(private$temp_predictions[i,], decreasing=TRUE)))
      
      # sort column values in each row
      maxColVals <- t(sapply(1:nRows, function(i) 
        private$temp_predictions[i,maxColInds[i,]]))

      # get names of maximum values for each row
      maxColNames <- t(sapply(1:nRows, 
        function(i) colnames(private$temp_predictions)[maxColInds[i,]]))

      # combine top 3 maxColVals and maxColNames to make maxProbDF
      # if there are only 2 response classes, only use 2
      if (ncol(maxColVals) >= 3) {# 3 or more classes -> get top 3
        private$orderedProbs <- data.frame(as.numeric(maxColVals[,1]), as.character(maxColNames[,1]), 
                                           as.numeric(maxColVals[,2]), as.character(maxColNames[,2]),
                                           as.numeric(maxColVals[,3]), as.character(maxColNames[,3]), 
                                           stringsAsFactors = FALSE)
      } else {# only 2 classes -> only use 2
        private$orderedProbs <- data.frame(as.numeric(maxColVals[,1]), as.character(maxColNames[,1]),
                                           as.numeric(maxColVals[,2]), as.character(maxColNames[,2]),
                                           stringsAsFactors = FALSE)
      }

      # update column names, also dealing with case of only 2 classes
      colnames(private$orderedProbs) <- c('PredictedProb1','PredictedClass1',
        'PredictedProb2','PredictedClass2',
        'PredictedProb3','PredictedClass3')[1:ncol(private$orderedProbs)]

      # update row names
      row.names(private$orderedProbs) <- 1:nRows

      if (isTRUE(self$params$debug)) {
        cat('Data frame after getting column importance ordered', '\n')
        print(private$orderedProbs[1:10, ])
      }
    },

    createDf = function() {
      cat('Creating ouput dataframe', '\n')
      dtStamp <- as.POSIXlt(Sys.time())

      # Combine grain.col, prediction, and time to be put back into SAM table
      private$outDf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtStamp,                           # LastLoadDTS
        private$grainTest,                 # GrainID
        private$orderedProbs)              # Top 3 Factors

      predictedResultsName = ""
      if (self$params$type == 'classification') {
        predictedResultsName = "PredictedProbNBR"
      } else if (self$params$type == 'regression') {
        predictedResultsName = "PredictedValueNBR"
      }
      colnames(private$outDf)[1:4] <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol
      )

      if (isTRUE(self$params$debug)) {
        cat('Dataframe with predictions:', '\n')
        cat(str(private$outDf), '\n')
      }
    }
  ),

  #Public members
  public = list(
    xgb_testMatrix = NA,
    #Constructor
    #p: new SupervisedModelDeploymentParams class object,
    #   i.e. p = SupervisedModelDeploymentParams$new()
    initialize = function(p) {
      cat('Initializing XGBoost Deploy...','\n')
      super$initialize(p)
    },

    #Override: deploy the model
    deploy = function() {
      cat('Loading XGB Model...','\n')
      
      # Start sink to capture console ouptut
      sink("tmp_prediction_console_output.txt", append = FALSE, split = TRUE)

      # Try to load the model
      private$fitXGB <- private$fitObj
      private$fitObj <- NULL
      
      # Make sure factor columns have the training data factor levels
      super$formatFactorColumns()
      # Update self$params$df to reflect the training data factor levels
      self$params$df <- private$dfTestRaw
      
      # Prepare data for xgboost
      private$xgbPrepareData()

      # Predict
      private$performPrediction()

      # Calculate Ordered Factors
      private$calculateOrderedFactors()

      # create dataframe for output
      private$createDf()
      
      sink()  # Close connection
      # Get metadata, attach to output DF and write to text file
      super$getMetadata()
      
    },
    
    # Surface outDf as attribute for export to Oracle, MySQL, etc
    getOutDf = function() {
      return(private$outDf)
    },
    # surface raw predictions for inspection
    getPredictions = function() {
      return(private$predictions)
    }
  )
)
