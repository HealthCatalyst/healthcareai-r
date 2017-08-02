#' Deploy a production-ready predictive RandomForest model
#'
#' @description This step allows one to
#' \itemize{
#' \item Load a saved model from \code{\link{RandomForestDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server
#' }
#' @docType class
#' @usage RandomForestDeployment(type, df, grainCol, testWindowCol, 
#' predictedCol, impute, debug)
#' @import caret
#' @import doParallel
#' @importFrom R6 R6Class
#' @import ranger
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param testWindowCol (depreciated) All data now receives a prediction
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
#' #### Classification Example using csv data ####
#' ## 1. Loading data and packages.
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # setwd('C:/Yourscriptlocation/Useforwardslashes') # Uncomment if using csv
#' 
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#' 
#' # Replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#' 
#' df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed remove this column
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## 2. Train and save the model using DEVELOP
#' print('Historical, development data:')
#' str(df)
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
#' # Run RandomForest
#' RandomForest <- RandomForestDevelopment$new(p)
#' RandomForest$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' print('Fake production data:')
#' str(dfDeploy)
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- RandomForestDeployment$new(p2)
#' dL$deploy()
#' 
#' dfOut <- dL$getOutDf()
#' head(dfOut)
#' # Write to CSV (or JSON, MySQL, etc) using plain R syntax
#' # write.csv(dfOut,'path/predictionsfile.csv')
#' 
#' print(proc.time() - ptm)
#' 
#' \donttest{
#' #### Classification example using SQL Server data ####
#' # This example requires you to first create a table in SQL Server
#' # If you prefer to not use SAMD, execute this in SSMS to create output table:
#' # CREATE TABLE dbo.HCRDeployClassificationBASE(
#' #   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#' #   PatientEncounterID int, <--change to match inputID
#' #   PredictedProbNBR decimal(38, 2),
#' #   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
#' # )
#' 
#' ## 1. Loading data and packages.
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
#' [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#' 
#' df <- selectData(connection.string, query)
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## 2. Train and save the model using DEVELOP
#' print('Historical, development data:')
#' str(df)
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
#' # Run RandomForest
#' RandomForest <- RandomForestDevelopment$new(p)
#' RandomForest$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' print('Fake production data:')
#' str(dfDeploy)
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- RandomForestDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(MSSQLConnectionString = connection.string,
#'           df = dfOut,
#'           tableName = 'HCRDeployClassificationBASE')
#' 
#' print(proc.time() - ptm)
#' }
#' 
#' \donttest{
#' #### Regression Example using SQL Server data ####
#' # This example requires you to first create a table in SQL Server
#' # If you prefer to not use SAMD, execute this in SSMS to create output table:
#' # CREATE TABLE dbo.HCRDeployRegressionBASE(
#' #   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#' #   PatientEncounterID int, <--change to match inputID
#' #   PredictedValueNBR decimal(38, 2),
#' #   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
#' # )
#' 
#' ## 1. Loading data and packages.
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
#' [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#' 
#' df <- selectData(connection.string, query)
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## 2. Train and save the model using DEVELOP
#' print('Historical, development data:')
#' str(df)
#' 
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "regression"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$predictedCol <- "A1CNBR"
#' p$debug <- FALSE
#' p$cores <- 1
#' 
#' # Run Random Forest
#' RandomForest <- RandomForestDevelopment$new(p)
#' RandomForest$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' dfDeploy$A1CNBR <- NULL # You won't know the response in production
#' print('Fake production data:')
#' str(dfDeploy)
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "regression"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "A1CNBR"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- RandomForestDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(MSSQLConnectionString = connection.string,
#'           df = dfOut,
#'           tableName = 'HCRDeployRegressionBASE')
#' 
#' print(proc.time() - ptm)
#' }
#' 
#' #' #### Classification example pulling from CSV and writing to SQLite ####
#' 
#' ## 1. Loading data and packages.
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # Can delete these system.file lines in your work
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#'                        
#' sqliteFile <- system.file("extdata",
#'                           "unit-test.sqlite",
#'                           package = "healthcareai")
#' 
#' # Read in CSV; replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#' 
#' df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed remove this column
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## 2. Train and save the model using DEVELOP
#' print('Historical, development data:')
#' str(df)
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
#' # Run Random Forest
#' RandomForest <- RandomForestDevelopment$new(p)
#' RandomForest$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' print('Fake production data:')
#' str(dfDeploy)
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- RandomForestDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(SQLiteFileName = sqliteFile,
#'           df = dfOut,
#'           tableName = 'HCRDeployClassificationBASE')
#' 
#' print(proc.time() - ptm)
#' 
#' #### Regression example pulling from CSV and writing to SQLite ####
#' 
#' ## 1. Loading data and packages.
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # Can delete these system.file lines in your work
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#' 
#' sqliteFile <- system.file("extdata",
#'                           "unit-test.sqlite",
#'                           package = "healthcareai")
#' 
#' # Read in CSV; replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#' 
#' df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed remove this column
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## 2. Train and save the model using DEVELOP
#' print('Historical, development data:')
#' str(df)
#' 
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "regression"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$predictedCol <- "A1CNBR"
#' p$debug <- FALSE
#' p$cores <- 1
#' 
#' # Run Random Forest
#' RandomForest<- RandomForestDevelopment$new(p)
#' RandomForest$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' dfDeploy$A1CNBR <- NULL # You won't know the response in production
#' print('Fake production data:')
#' str(dfDeploy)
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "regression"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "A1CNBR"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- RandomForestDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(SQLiteFileName = sqliteFile,
#'           df = dfOut,
#'           tableName = 'HCRDeployRegressionBASE')
#' 
#' print(proc.time() - ptm)

RandomForestDeployment <- R6Class("RandomForestDeployment",
  #Inheritance
  inherit = SupervisedModelDeployment,

  #Private members
  private = list(

    # variables
    coefficients = NA,
    multiplyRes = NA,
    orderedFactors = NA,
    predictedValsForUnitTest = NA,
    outDf = NA,
    
    fitRF = NA,
    predictions = NA,
    modelName = 'RF',
    algorithmName = 'RandomForest',

    # functions
    # Perform prediction
    performPrediction = function() {
      if (self$params$type == 'classification') {
        private$predictions <- caret::predict.train(object = private$fitRF,
                                                    newdata = self$params$df,
                                                    type = 'prob')
        private$predictions <- private$predictions[,2]
        
        if (isTRUE(self$params$debug)) {
          cat('Number of predictions: ', nrow(private$predictions), '\n')
          cat('First 10 raw classification probability predictions', '\n')
          print(round(private$predictions[1:10],2))
        }
        
      } else if (self$params$type == 'regression') {
        private$predictions <- caret::predict.train(private$fitRF, newdata = self$params$df)
        
        if (isTRUE(self$params$debug)) {
          cat('Rows in regression prediction: ', length(private$predictions), '\n')
          cat('First 10 raw regression predictions (with row # first)', '\n')
          print(round(private$predictions[1:10],2))
        }
      }
    },

    calculateCoeffcients = function() {
      # Do semi-manual calc to rank cols by order of importance
      coeffTemp <- self$modelInfo$fitLogit$coefficients

      if (isTRUE(self$params$debug)) {
        cat('Coefficients for the default logit (for ranking var import)', '\n')
        print(coeffTemp)
      }

      private$coefficients <-
        coeffTemp[2:length(coeffTemp)] # drop intercept
    },

    calculateMultiplyRes = function() {
      if (isTRUE(self$params$debug)) {
        cat("Test set to be multiplied with coefficients", '\n')
        cat(str(private$dfTestRaw), '\n')
      }

      # Apply multiplication of coeff across each row of test set
      private$multiplyRes <- sweep(private$dfTestRaw, 2, private$coefficients, `*`)

      if (isTRUE(self$params$debug)) {
        cat('Data frame after multiplying raw vals by coeffs', '\n')
        print(private$multiplyRes[1:10, ])
      }
    },

    calculateOrderedFactors = function() {
      # Calculate ordered factors of importance for each row's prediction
      private$orderedFactors <- t(sapply
                                  (1:nrow(private$multiplyRes),
                                  function(i)
                                    colnames(private$multiplyRes[order(private$multiplyRes[i, ],
                                                                        decreasing = TRUE)])))

      if (isTRUE(self$params$debug)) {
        cat('Data frame after getting column importance ordered', '\n')
        print(head(private$orderedFactors, n = 10))
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
        # need three lines for case of single prediction
        private$orderedFactors[, 1],     # Top 1 Factor
        private$orderedFactors[, 2],     # Top 2 Factor
        private$orderedFactors[, 3])     # Top 3 Factor

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
      
      # Remove row names so df can be written to DB
      # TODO: in writeData function, find how to ignore row names
      rownames(private$outDf) <- NULL

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

      super$initialize(p)

      if (!is.null(p$rfmtry))
        self$params$rfmtry <- p$rfmtry

      if (!is.null(p$trees))
        self$params$trees <- p$trees
    },

    #Override: deploy the model
    deploy = function() {

      # Try to load the model
      private$fitRF <- private$fitObj
      private$fitObj <- NULL
      
      # Make sure factor columns have the training data factor levels
      super$formatFactorColumns()
      # Update self$params$df to reflect the training data factor levels
      self$params$df <- private$dfTestRaw

      # Predict
      private$performPrediction()

      # Get dummy data based on factors from develop
      super$makeFactorDummies()

      # Calculate Coeffcients
      private$calculateCoeffcients()

      # Calculate MultiplyRes
      private$calculateMultiplyRes()

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
