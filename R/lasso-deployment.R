#' Deploy a production-ready predictive Lasso model
#'
#' @description This step allows one to
#' \itemize{
#' \item Load a saved model from \code{\link{LassoDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server
#' }
#' @docType class
#' @usage LassoDeployment(type, df, grainCol, testWindowCol, predictedCol, 
#' impute, debug)
#' @import caret
#' @import doParallel
#' @importFrom R6 R6Class
#' @import ranger
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param testWindowCol Y or N. This column dictates the split between model 
#' training and test sets. Those rows with N in this column indicate the 
#' training set while those that have Y indicate the test set
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
#' head(df)
#' str(df)
#' 
#' ## 2. Train and save the model using DEVELOP
#' df$PatientID <- NULL
#' inTest <- df$InTestWindowFLG # save this for later.
#' df$InTestWindowFLG <- NULL
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
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' df$InTestWindowFLG <- inTest
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- df
#' p2$testWindowCol <- "InTestWindowFLG"
#' p2$grainCol <- "PatientEncounterID"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#'
#' dL <- LassoDeployment$new(p2)
#' dL$deploy()
#' 
#' df <- dL$getOutDf()
#' # Write to CSV (or JSON, MySQL, etc) using plain R syntax
#' # write.csv(df,'path/predictionsfile.csv')
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
#' query <- "
#' SELECT
#' [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' ,[InTestWindowFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#' df <- selectData(connection.string, query)
#' 
#' head(df)
#' str(df)
#' 
#' ## 2. Train and save the model using DEVELOP
#' set.seed(42)
#' inTest <- df$InTestWindowFLG # save this for deploy
#' df$InTestWindowFLG <- NULL
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
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- df
#' p2$grainCol <- "PatientEncounterID"
#' p2$testWindowCol <- "InTestWindowFLG"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- LassoDeployment$new(p2)
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
#' query <- "
#' SELECT
#' [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' ,[InTestWindowFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#' df <- selectData(connection.string, query)
#' 
#' head(df)
#' str(df)
#' 
#' ## 2. Train and save the model using DEVELOP
#' #' set.seed(42)
#' inTest <- df$InTestWindowFLG # save this for deploy
#' df$InTestWindowFLG <- NULL
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
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "regression"
#' p2$df <- df
#' p2$grainCol <- "PatientEncounterID"
#' p2$testWindowCol <- "InTestWindowFLG"
#' p2$predictedCol <- "A1CNBR"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- LassoDeployment$new(p2)
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
#' #### Classification example pulling from CSV and writing to SQLite ####
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
#' head(df)
#' str(df)
#' 
#' ## 2. Train and save the model using DEVELOP
#' set.seed(42)
#' df$PatientID <- NULL # We'll instead use PatientEncounterID
#' inTest <- df$InTestWindowFLG # save this for deploy
#' df$InTestWindowFLG <- NULL
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
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "classification"
#' p2$df <- df
#' p2$grainCol <- "PatientEncounterID"
#' p2$testWindowCol <- "InTestWindowFLG"
#' p2$predictedCol <- "ThirtyDayReadmitFLG"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- LassoDeployment$new(p2)
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
#' head(df)
#' str(df)
#' 
#' ## 2. Train and save the model using DEVELOP
#' set.seed(42)
#' df$PatientID <- NULL # We'll instead use PatientEncounterID
#' inTest <- df$InTestWindowFLG # save this for deploy
#' df$InTestWindowFLG <- NULL
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
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
#' df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
#' 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "regression"
#' p2$df <- df
#' p2$grainCol <- "PatientEncounterID"
#' p2$testWindowCol <- "InTestWindowFLG"
#' p2$predictedCol <- "A1CNBR"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' p2$cores <- 1
#' 
#' dL <- LassoDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(SQLiteFileName = sqliteFile,
#'           df = dfOut,
#'           tableName = 'HCRDeployRegressionBASE')
#' 
#' print(proc.time() - ptm)

LassoDeployment <- R6Class(
  "LassoDeployment",
  
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
    
    fitGrLasso = NA,
    fitLogit = NA,
    indLambda1se = NA,
    lambda1se = NA,
    modFmla = NA,
    modMat = NA,
    
    predictions = NA,
    
    # functions
    connectDataSource = function() {
      RODBC::odbcCloseAll()
      # Convert the connection string into a real connection object.
      self$params$sqlConn <- RODBC::odbcDriverConnect(self$params$sqlConn)
      
    },

    closeDataSource = function() {
      RODBC::odbcCloseAll()
    },

    # Predict results
    performPrediction = function() {
      # Index of largest lambda within one cvse of the lambda with lowest cve:
      # These are sorted from largest to smallest lambda, hence pulling the
      # minimum index.
      private$indLambda1se <- min(which(private$fitGrLasso$cve <= 
                                       (private$fitGrLasso$cve + 
                                        private$fitGrLasso$cvse)[private$fitGrLasso$min]))
      
      # Largest lambda within one cvse of the lambda with lowest cve (ie. lambda
      # to use in final fit):
      private$lambda1se <- private$fitGrLasso$lambda[private$indLambda1se]
      
      # Predictions (in terms of probability)
      private$predictions <- stats::predict(object = private$fitGrLasso,
                                     X = model.matrix(private$modFmla, data = private$dfTestTemp)[,-1],
                                     lambda = private$lambda1se,
                                     type = "response")
      
      if (isTRUE(self$params$debug)) {
        cat("Rows in prob prediction: ", nrow(private$predictedVals), '\n')
        if (self$params$type == 'classification') {
          cat("First 10 raw classification probability predictions", '\n')
          print(round(private$predictions[1:10], 2))
        } else if (self$params$type == 'regression') {
          cat("First 10 raw regression value predictions", '\n')
          print(round(private$predictions[1:10], 2))
        }
      }
    },
    
    calculateCoeffcients = function() {
      # Do semi-manual calc to rank cols by order of importance
      coeffTemp <- private$fitLogit$coefficients

      if (isTRUE(self$params$debug)) {
        cat("Coefficients for the default logit (for ranking var import)", '\n')
        print(coeffTemp)
      }

      private$coefficients <- coeffTemp[2:length(coeffTemp)]  # drop intercept
    },

    calculateMultiplyRes = function() {
      # Apply multiplication of coeff across each row of test set Remove y (label) so
      # we do multiplication only on X (features)
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        cat("Test set after removing predicted column", '\n')
        cat(str(private$dfTest), '\n')
      }

      private$multiplyRes <- sweep(private$dfTestRaw, 2, private$coefficients, `*`)

      if (isTRUE(self$params$debug)) {
        cat("Data frame after multiplying raw vals by coeffs", '\n')
        print(private$multiplyRes[1:10, ])
      }
    },

    calculateOrderedFactors = function() {
      # Calculate ordered factors of importance for each row's prediction
      private$orderedFactors <- t(sapply(1:nrow(private$multiplyRes), function(i) colnames(private$multiplyRes[order(private$multiplyRes[i,
                                                                                                                                         ], decreasing = TRUE)])))
      
      if (isTRUE(self$params$debug)) {
        cat("Data frame after getting column importance ordered", '\n')
        print(private$orderedFactors[1:10, ])
      }
    },

    createDf = function() {
      dtStamp <- as.POSIXlt(Sys.time())

      # Combine grain.col, prediction, and time to be put back into SAM table
      # TODO: use a common function to reduce lasso-specific code here
      private$outDf <- data.frame(
        0,    # BindingID
        'R',  # BindingNM
        dtStamp,                      # LastLoadDTS
        private$grainTest,            # GrainID
        private$predictions,          # PredictedProbab
        private$orderedFactors[, 1:3] # Top 3 Factors
      )    

      predictedResultsName <- ""
      if (self$params$type == "classification") {
        predictedResultsName <- "PredictedProbNBR"
      } else if (self$params$type == "regression") {
        predictedResultsName <- "PredictedValueNBR"
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
    }
  ),

  # Public members
  public = list(
    type = 'classification',
    df = NA,
    grainCol = NA,
    testWindowCol  = NA,
    predictedCol = NA,
    impute = NA,
    # Constructor
    # p: new SupervisedModelDeploymentParams class object,
    # i.e. p = SupervisedModelDeploymentParams$new()
    initialize = function(p) {
      super$initialize(p)
    },

    #Override: deploy the model
    deploy = function() {

      # Try to load the model
      tryCatch({
        load("rmodel_var_import_lasso.rda")  # Produces fitLogit object
        private$fitLogit <- fitLogit
        load("rmodel_probability_lasso.rda") # Produces fit object (for probability)
          private$fitGrLasso <- fitObj
          private$modMat <- fitObj$modMat
          private$modFmla <- fitObj$modFmla
          fitObj$modMat <- NULL
          fitObj$modFmla <- NULL
       }, error = function(e) {
        # temporary fix until all models are working.
        stop('You must use a saved model. Run lasso development to train and save
              the model, then lasso deployment to make predictions. See ?LassoDeployment')
      })
      
      # Predict
      private$performPrediction()

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
