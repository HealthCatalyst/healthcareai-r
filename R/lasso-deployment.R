#' Deploy a production-ready predictive Lasso model
#'
#' @description This step allows one to
#' \itemize{
#' \item Create a final model on all of your training data
#' \item Automatically save the model
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
#' #### Regression Example using csv data ####
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
#' # Remove unnecessary columns
#' df$PatientID <- NULL
#'
#' p <- SupervisedModelDeploymentParams$new()
#' p$type <- "regression"
#' p$df <- df
#' p$grainCol <- "PatientEncounterID"
#' p$testWindowCol <- "InTestWindowFLG"
#' p$predictedCol <- "A1CNBR"
#' p$impute <- TRUE
#' p$debug <- FALSE
#' p$useSavedModel <- FALSE
#' p$cores <- 1
#' p$writeToDB <- FALSE
#'
#' dL <- LassoDeployment$new(p)
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
#'  [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' ,[InTestWindowFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' --no WHERE clause, because we want train AND test
#' "
#'
#' df <- selectData(connection.string, query)
#'
#' head(df)
#' str(df)
#'
#' p <- SupervisedModelDeploymentParams$new()
#' p$type <- "classification"
#' p$df <- df
#' p$grainCol <- "PatientEncounterID"
#' p$testWindowCol <- "InTestWindowFLG"
#' p$predictedCol <- "ThirtyDayReadmitFLG"
#' p$impute <- TRUE
#' p$debug <- FALSE
#' p$useSavedModel <- FALSE
#' p$cores <- 1
#' p$sqlConn <- connection.string
#' p$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
#'
#' dL <- LassoDeployment$new(p)
#' dL$deploy()
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
#'  [PatientEncounterID] --Only need one ID column for random forest
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' ,[InTestWindowFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' --no WHERE clause, because we want train AND test
#' "
#'
#' df <- selectData(connection.string, query)
#'
#' head(df)
#' str(df)
#'
#' p <- SupervisedModelDeploymentParams$new()
#' p$type <- "regression"
#' p$df <- df
#' p$grainCol <- "PatientEncounterID"
#' p$testWindowCol <- "InTestWindowFLG"
#' p$predictedCol <- "A1CNBR"
#' p$impute <- TRUE
#' p$debug <- FALSE
#' p$useSavedModel <- FALSE
#' p$cores <- 1
#' p$sqlConn <- connection.string
#' p$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
#'
#' dL <- LassoDeployment$new(p)
#' dL$deploy()
#'
#' print(proc.time() - ptm)
#' }

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
    
    
    fit = NA,
    fitLogit = NA,
    indLambda1se = NA,
    lambda1se = NA,
    modFmla = NA,
    modMat = NA,
    
    predictions = NA,
    
  
    # functions
    connectDataSource = function() {
      RODBC::odbcCloseAll()
      if (isTRUE(self$params$writeToDB)) {
        # Convert the connection string into a real connection object.
        self$params$sqlConn <- RODBC::odbcDriverConnect(self$params$sqlConn)
      }
    },

    closeDataSource = function() {
      RODBC::odbcCloseAll()
    },

    # fitGeneralizedLinearModel = function() {
    #   if (isTRUE(self$params$debug)) {
    #     print("generating fitLogit...")
    #   }
    # 
    #   if (self$params$type == "classification") {
    #     private$fitLogit <- glm(as.formula(paste(self$params$predictedCol, ".", sep = " ~ ")),
    #                             data = private$dfTrain, family = binomial(link = "logit"), metric = "ROC",
    #                             control = list(maxit = 10000), trControl = trainControl(classProbs = TRUE,
    #                                                                                     summaryFunction = twoClassSummary))
    # 
    #   } else if (self$params$type == "regression") {
    #     private$fitLogit <- glm(as.formula(paste(self$params$predictedCol, ".", sep = " ~ ")),
    #                             data = private$dfTrain, metric = "RMSE", control = list(maxit = 10000))
    #   }
    # },

    # saveModel = function() {
    #   if (isTRUE(self$params$debug)) {
    #     print("Saving model...")
    #   }
    # 
    #   # NOTE: save(private$fit, ...) does not work!
    #   if (isTRUE(!self$params$useSavedModel)) {
    #     fitObj <- private$fit
    #     save(fitObj, file = "rmodel_combined.rda")
    #   }
    # 
    #   # This isn't needed if formula interface is used in randomForest
    #   private$dfTest[[self$params$predictedCol]] = NULL
    # 
    #   if (isTRUE(self$params$debug)) {
    #     print("Test set before being used in predict(), after removing y")
    #     print(str(private$dfTest))
    #   }
    # },

    # Predict results
    performPrediction = function() {
      browser
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
        print(paste0("Rows in prob prediction: ", nrow(private$predictedVals)))
        print("First 10 raw classification probability predictions")
        print(round(private$predictions[1:10], 2))
      }
    },
    
    calculateCoeffcients = function() {
      # Do semi-manual calc to rank cols by order of importance
      coeffTemp <- private$fitLogit$coefficients

      if (isTRUE(self$params$debug)) {
        print("Coefficients for the default logit (for ranking var import)")
        print(coeffTemp)
      }

      private$coefficients <- coeffTemp[2:length(coeffTemp)]  # drop intercept
    },

    calculateMultiplyRes = function() {
      # Apply multiplication of coeff across each row of test set Remove y (label) so
      # we do multiplication only on X (features)
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        print("Test set after removing predicted column")
        print(str(private$dfTest))
      }

      private$multiplyRes <- sweep(private$dfTestRaw, 2, private$coefficients, `*`)

      if (isTRUE(self$params$debug)) {
        print("Data frame after multiplying raw vals by coeffs")
        print(private$multiplyRes[1:10, ])
      }
    },

    calculateOrderedFactors = function() {
      # Calculate ordered factors of importance for each row's prediction
      private$orderedFactors <- t(sapply(1:nrow(private$multiplyRes), function(i) colnames(private$multiplyRes[order(private$multiplyRes[i,
                                                                                                                                         ], decreasing = TRUE)])))

      if (isTRUE(self$params$debug)) {
        print("Data frame after getting column importance ordered")
        print(private$orderedFactors[1:10, ])
      }
    },

    saveDataIntoDb = function() {
      dtStamp <- as.POSIXlt(Sys.time())

      # Combine grain.col, prediction, and time to be put back into SAM table
      private$outDf <- data.frame(
        0,
        # BindingID
        'R',
        # BindingNM
        dtStamp,
        # LastLoadDTS
        private$grainTest,
        # GrainID
        private$predictedVals,
        # PredictedProbab
        private$orderedFactors[, 1:3]
      )    # Top 3 Factors

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

      if (isTRUE(self$params$debug)) {
        print('Dataframe with predictions:')
        print(str(private$outDf))
      }

      if (isTRUE(self$params$writeToDB)) {
        # Save df to table in SAM database
        out <- RODBC::sqlSave(
          channel = self$params$sqlConn,
          dat = private$outDf,
          tablename = self$params$destSchemaTable,
          append = T,
          rownames = F,
          colnames = F,
          safer = T,
          nastring = NULL,
          verbose = self$params$debug
        )
  
        # Print success if insert was successful
        if (out == 1) {
          print('SQL Server insert was successful')
        }
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

    # buildFitObject = function() {
    #   # Get fit object by linear model
    #   # if linear, set to logit for logistic
    #   private$fit = private$fitLogit
    # },
    # 
    # #Override: Build Deploy Model
    # buildDeployModel = function() {
    #   if (isTRUE(self$params$debug)) {
    #     print('Training data set immediately before training')
    #     print(str(private$dfTrain))
    #   }
    # 
    #   # Start default logit (for var importance)
    #   private$fitGeneralizedLinearModel()
    # 
    #   # Build fit object
    #   self$buildFitObject()
    # 
    #   print('Details for proability model:')
    #   print(private$fit)
    # },

    #Override: deploy the model
    deploy = function() {
      # Connect to sql via odbc driver
      private$connectDataSource()
      
      if (isTRUE(self$params$useSavedModel)) {
        load("rmodel_var_import_lasso.rda")  # Produces fitLogit object
        private$fitLogit <- fitLogit
        
        load("rmodel_combined_lasso.rda") # Produces fit object (for probability)
          private$fit <- fitObj
          private$modMat <- fitObj$modMat
          private$modFmla <- fitObj$modFmla
          fitObj$modMat <- NULL
          fitObj$modFmla <- NULL
          
      } else {
        # private$registerClustersOnCores()

        # build deploy model
        # self$buildDeployModel()
      }

      # Save model
      # private$saveModel()

      # Predict
      private$performPrediction()

      # Calculate Coeffcients
      private$calculateCoeffcients()

      # Calculate MultiplyRes
      private$calculateMultiplyRes()

      # Calculate Ordered Factors
      private$calculateOrderedFactors()

      # Save data into db
      private$saveDataIntoDb()

      # Clean up.
      # if (isTRUE(!self$params$useSavedModel)) {
      #   private$stopClustersOnCores()
      # }
      private$closeDataSource()
    },

    #Get predicted values
    getPredictedValsForUnitTest = function() {
      return(private$predictedValsForUnitTest)
    },
    
    # Surface outDf as attribute for export to Oracle, MySQL, etc
    getOutDf = function() {
      return(private$outDf)
    }
  )
)
