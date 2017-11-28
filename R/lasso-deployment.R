#' Deploy a production-ready predictive Lasso model
#'
#' @description This step allows one to
#' \itemize{
#' \item Load a saved model from \code{\link{LassoDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server
#' \item Identify factors that could benefit outcomes (see final examples)
#' }
#' @docType class
#' @usage LassoDeployment(type, df, grainCol, predictedCol, impute, debug, cores, modelName)
#' @import caret
#' @import doParallel
#' @importFrom R6 R6Class
#' @import ranger
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for new predictions. Data structure should match development as 
#' much as possible. Number of columns, names, types, grain, and predicted must be the same.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param predictedCol Column that you want to predict.
#' @param impute For training df, set all-column imputation to TRUE or FALSE.
#' If TRUE, this uses values calculated in development.
#' FALSE leads to removal of rows containing NULLs and is not recommended.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @param cores Number of cores you'd like to use. Defaults to 2.
#' @param modelName Optional string. Can specify the model name. If used, you must load the same one in the deploy step.
#' @section Methods: 
#' The above describes params for initializing a new lassoDeployment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new lasso deployment class using the 
#' parameters saved in \code{p}, documented above. This method loads, cleans, and prepares data for
#' generating predictions. \cr
#' \emph{Usage:} \code{$new(p)}
#' @section \code{$deploy()}:
#' Generate new predictions, calculate top factors, and prepare the output dataframe. \cr
#' \emph{Usage:}\code{$deploy()} 
#' @section \code{$getTopFactors()}:
#' Return the grain, all top factors, and their weights. \cr
#' \emph{Usage:} \code{$getTopFactors(numberOfFactors = NA, includeWeights = FALSE)} \cr
#' Params: \cr
#'   - \code{numberOfFactors} returns the top \code{n} factors. Defaults to all factors. \cr
#'   - \code{includeWeights} If \code{TRUE}, returns weights associated with each factor.
#' @section \code{$getOutDf()}:
#' Returns the output dataframe. \cr
#' \emph{Usage:} \code{$getOutDf()} 
#' @section \code{$performNewPredictions()}:
#' Performs predictions on new data which has gone through the necessary
#' preprocessing. \cr
#' \emph{Usage:} \code{$performNewPredictions(newData)} \cr
#' Params: \cr
#'   - \code{newData} A dataframe on which the model can make predictions.
#' @section \code{$getProcessVariablesDf()}:
#' Get a data frame with expected changes in outcomes conditional on process
#' variable changes. \cr
#' \emph{Usage:} \code{$getProcessVariablesDf(modifiableVariables, 
#' variableLevels = NULL, grainColumnValues = NULL, smallerBetter = TRUE, 
#' repeatedFactors = FALSE, numTopFactors = 3)} \cr
#' Params: \cr
#'   - \code{modifiableVariables} A vector of names of modifiable variables.\cr
#'   - \code{variableLevels} A list of variable values indexed by 
#'   modifiable variable names. This allows one to use numeric variables by 
#'   specifying baselines and to restrict categorical variables by limiting 
#'   which factors can be surfaced.\cr
#'   - \code{grainColumnIDs} A vector of grain column IDs. If \code{NULL}, the 
#'   whole deployment dataframe will be used.\cr
#'   - \code{smallerBetter} A boolean determining whether or not lower 
#'   predictions/probabilities are more desirable. \cr
#'   - \code{repeatedFactors} A boolean determining whether or not a single 
#'   modifiable factor can be listed several times. \cr
#'   - \code{numTopFactors} The number of modifiable process variables to 
#'   include in each row.
#' @export
#' @seealso \code{\link{healthcareai}}
#' @seealso \code{\link{writeData}}
#' @seealso \code{\link{selectData}}
#' @examples
#' 
#' 
#' #### Classification Example using csv data ####
#' ## 1. Loading data and packages.
#' ptm <- proc.time()
#' library(healthcareai)
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
#' # Run Lasso
#' Lasso<- LassoDevelopment$new(p)
#' Lasso$run()
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
#' dL <- LassoDeployment$new(p2)
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
#' [PatientEncounterID] --Only need one ID column for lasso
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
#' # Run Lasso
#' Lasso<- LassoDevelopment$new(p)
#' Lasso$run()
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
#' 
#' query <- "
#' SELECT
#' [PatientEncounterID] --Only need one ID column for lasso
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
#' # Run lasso
#' Lasso<- LassoDevelopment$new(p)
#' Lasso$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
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
#' df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed
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
#' # Run lasso
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
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
#' # Run lasso
#' Lasso<- LassoDevelopment$new(p)
#' Lasso$run()
#' 
#' ## 3. Load saved model and use DEPLOY to generate predictions. 
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
#' dL <- LassoDeployment$new(p2)
#' dL$deploy()
#' dfOut <- dL$getOutDf()
#' 
#' writeData(SQLiteFileName = sqliteFile,
#'           df = dfOut,
#'           tableName = 'HCRDeployRegressionBASE')
#' 
#' print(proc.time() - ptm)
#' 
#' #### Identify factors that could benefit outcomes: getProcessVariablesDf ####
#' #############################################################################
#' 
#' # getProcessVariableDf() identifies opportunities for improved outcomes at
#' # the grain level. It is important that the variables ("modifiableVariables")
#' # and values ("variableLevels") used in this function are under the control 
#' # of the care management process. The best use case for this function is a
#' # "natural experiment" where otherwise similar groups had different 
#' # treatments applied to them, and that treatment is the modifiable variable
#' # of interest.
#'
#' # This example shows how to use the getProcessVariableDf() function, using 
#' # another readmission-prediction model. In this example systolic blood pressure 
#' # is converted into a categorical variable to demonstrate functionality.
#' # Because of the lasso's automatic feature selection, this example is fairly
#' # limited. For a wider variety of examples, see ?RandomForestDeployment
#' 
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#' 
#' # Replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#' 
#' df$PatientID <- NULL # Remove extra ID
#' 
#' # Save a dataframe for validation later on
#' dfDeploy <- df[951:1000,]
#' 
#' ## Develop and Deploy the model
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
#' Lasso <- LassoDevelopment$new(p)
#' Lasso$run()
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
#' dL <- LassoDeployment$new(p2)
#' dL$deploy()
#' 
#' ## Get predicted outcome changes using getProcessVariablesDf
#' 
#' # getProcessVariablesDf only uses variables with non-zero coefficients, 
#' # automatically discarding the rest. In this example, only A1CNBR had a
#' # nonzerocoefficient. Even though this variable is numeric, we can still use
#' # getProcessVariablesDf using the variableLevels parameter. For examples with
#' # categorical variables and additional details about getProcessVariablesDf,
#' # see the examples in ?RandomForestDeployment
#' 
#' dL$getProcessVariablesDf(modifiableVariables = c("A1CNBR"),
#'                          variableLevels = list(A1CNBR = c(5.6, 6.0, 6.5)))
#' 
#' # By default, the function returns predictions for all rows, but we can 
#' # restrict to specific rows using the grainColumnIDs parameter
#' dL$getProcessVariablesDf(modifiableVariables = c("A1CNBR"),
#'                          variableLevels = list(A1CNBR = c(5.6, 6.0, 6.5)), 
#'                          grainColumnIDs = c(951, 975))
#' 
#' # The repeatedFactors parameter allows one to get multiple predictions  
#' # forthe same variable. For example, reducing A1C to 5.6 might most improve a 
#' # patient's risk, but reducing A1C to 5.9 is likely to also reduce the risk.
#' dL$getProcessVariablesDf(modifiableVariables = c("A1CNBR"),
#'                          variableLevels = list(A1CNBR = c(5.6, 5.9, 6.2, 6.5)),
#'                          repeatedFactors = TRUE)
#' 
#' # The numTopFactors parameter allows one to set the maximum number of 
#' # predictions to display (with the default being 3)
#' dL$getProcessVariablesDf(modifiableVariables = c("A1CNBR"),
#'                          variableLevels = list(A1CNBR = c(5.6, 5.9, 6.2, 6.5)),
#'                          repeatedFactors = TRUE, 
#'                          numTopFactors = 2)
#' 
#' # If we want to make predictions for increasing the probability (not
#' # likely in the case of readmissions), we can do so using the smallerBetter
#' # parameter. (Here, all the deltas will be non-negative, corresponding to an 
#' # increased risk)
#' dL$getProcessVariablesDf(modifiableVariables = c("A1CNBR"),
#'                          variableLevels = list(A1CNBR = c(5.6, 6.0, 6.5)),
#'                          smallerBetter = FALSE)

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
    indLambda1se = NA,
    lambda1se = NA,
    modFmla = NA,
    modMat = NA,
    
    predictions = NA,
    algorithmShortName = 'lasso',
    algorithmName = 'Lasso',
    
    # Functions
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
      private$predictions <- self$performNewPredictions(self$params$df)
      
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
      coeffTemp <- self$modelInfo$fitLogit$coefficients

      if (isTRUE(self$params$debug)) {
        cat("Coefficients for the default logit (for ranking var import)", '\n')
        print(coeffTemp)
      }

      private$coefficients <- coeffTemp[2:length(coeffTemp)]  # drop intercept
    },

    calculateMultiplyRes = function() {
      # Apply multiplication of coeff across each row of test set Remove y (label) so
      # we do multiplication only on X (features)

      if (isTRUE(self$params$debug)) {
        cat("Test set to be multiplied with coefficients", '\n')
        cat(str(private$dfTestRaw), '\n')
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
        print(head(private$orderedFactors, n = 10))
      }
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
      
      # Start sink to capture console ouptut
      sink("tmp_prediction_console_output.txt", append = FALSE, split = TRUE)

      # Try to load the model
      private$fitGrLasso <- private$fitObj
      private$modMat <- private$fitObj$modMat
      private$modFmla <- private$fitObj$modFmla
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
      super$createDf()

      sink()  # Close connection
      # Get metadata, attach to output DF and write to text file
      super$getMetadata()

    },
    
    # Surface outDf as attribute for export to Oracle, MySQL, etc
    getOutDf = function() {
      return(private$outDf)
    }, 
    
    # Perform predictions on new data
    performNewPredictions = function(newData) {
      stats::predict(object = private$fitGrLasso,
                     X = model.matrix(private$modFmla, data = newData)[,-1],
                     lambda = private$lambda1se,
                     type = "response")
    }
  )
)
