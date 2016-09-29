# Import the common functions.
source('R/common.R')
source('R/DeploySupervisedModel.R')

#' Deploy a production-ready predictive RandomForest model
#'
#' @description This step allows one to
#' \itemize{
#' \item Create a final model on all of your training data
#' \item Automatically save the model
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server
#' }
#' @docType class
#' @import caret
#' @import doParallel
#' @importFrom R6 R6Class
#' @import ranger
#' @import RODBC
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param testWindowCol This column dictates the split between model training and
#' test sets. Those rows with zeros in this column indicate the training set
#' while those that have ones indicate the test set
#' @param predictedCol Column that you want to predict.
#' @param impute For training df, set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @seealso \code{\link{HCRTools}}
#' @examples
#' #### Classification example using data from csv ####
#' # This example requires
#' #    1) You set your working directory to source file location
#' #    2) To receive predictions from R back to SQL Server, you'll need to save
#' #       and run an entity in SAMD that has only the following columns
#' #
#' # GrainID decimal(38,0) not null, <--change col to match ID in summary table
#' # PredictedProbNBR decimal(38,2),
#' # Factor1TXT varchar(255),
#' # Factor2TXT varchar(255),
#' # Factor3TXT varchar(255),
#'
#' # If you prefer to not use SAMD, execute this in SSMS to create output table:
#' # CREATE TABLE dbo.HCRDeployClassificationBASE(
#' #   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#' #   GrainID int <--change to match inputID, PredictedProbNBR decimal(38, 2),
#' #   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
#' # )
#'
#' #setwd("C:/Yourscriptlocation/Useforwardslashes") # Encomment this command #
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' connection.string <- 'driver={SQL Server};
#'                       server=localhost;
#'                       database=SAM;
#'                       trusted_connection=true'
#'
#' # Can delete this line in your work:
#' csvfile <- system.file("extdata", "HREmployeeDeploy.csv",package = "HCRTools")
#'
#' df <- read.csv(file = csvfile, #<--replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' # Remove unnecessary columns
#' df$VacationHours <- NULL
#'
#' head(df)
#'
#' p <- DeploySupervisedModelParameters$new()
#' p$type = 'classification'
#' p$df = df
#' p$grainCol = 'GrainID'
#' p$testWindowCol = 'InTestWindow'
#' p$predictedCol = 'SalariedFlag'
#' p$impute = TRUE
#' p$debug = FALSE
#' p$useSavedModel = FALSE
#' p$cores = 1
#' p$sqlConn = connection.string
#' p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'
#' p$rfmtry = 2
#'
#' dRF <- DeployRandomForest$new(p)
#' dRF$deploy()
#'
#' print(proc.time() - ptm)
#' @export


DeployRandomForest <- R6Class("DeployRandomForest",

  #Inheritance
  inherit = DeploySupervisedModel,

  #Private members
  private = list(

    # variables
    coefficients = NULL,
    multiply_res = NULL,
    ordered.factors = NULL,
    predictedValsForUnitTest = NULL,

    # functions
    connectDataSource = function() {
      odbcCloseAll()
      # Convert the connection string into a real connection object.
      self$params$sqlConn <- odbcDriverConnect(self$params$sqlConn)
    },

    closeDataSource = function() {
      odbcCloseAll()
    },

    fitGeneralizedLinearModel = function() {
      if (isTRUE(self$params$debug)) {
        print('generating fit.logit...')
      }

      if (self$params$type == 'classification') {
        private$fit.logit = glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          family = binomial(link = "logit"),
          metric = "ROC",
          control = list(maxit = 10000),
          trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
        )

      } else if (self$params$type == 'regression') {
        private$fit.logit = glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          metric = "RMSE",
          control = list(maxit = 10000)
        )
      }

    },

    saveModel = function() {

      if (isTRUE(self$params$debug)) {
        print('Saving model...')
      }

      # Save models if specified
      if (isTRUE(!self$params$useSavedModel)) {

        #NOTE: save(private$fit.logit, ...) does not work!
        fitLogitObj = private$fit.logit
        fitObj = private$fit

        save(fitLogitObj, file = "rmodel_var_import.rda")
        save(fitObj, file = "rmodel_probability.rda")
      }

      # This isn't needed if formula interface is used in randomForest
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        print('Test set before being used in predict(), after removing y')
        print(str(private$dfTest))
      }

    },

    performPrediction = function() {
      if (self$params$type == 'classification') {
        #  these are probabilities
        predictedVALStemp = predict(private$fit, data = private$dfTest)
        private$predictedVals <- predictedVALStemp$predictions[, 2]
        private$predictedValsForUnitTest <- private$predictedVals[5] # for unit test

        print('Probability predictions are based on random forest')

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in prob prediction: ', nrow(private$predictedVals)))
          print('First 10 raw classification probability predictions')
          print(round(private$predictedVals[1:10], 2))
        }

      } else if (self$params$type == 'regression') {
        # this is in-kind prediction
        predictedVALStemp = predict(private$fit, data = self$dfTest)
        private$predictedVals <- predictedVALStemp$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0(
            'Rows in regression prediction: ',
            length(private$predictedVals)
          ))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictedVals[1:10], 2))
        }

      }

    },

    calculateCoeffcients = function() {
      # Do semi-manual calc to rank cols by order of importance
      coefftemp <- private$fit.logit$coefficients

      if (isTRUE(self$params$debug)) {
        print('Coefficients for the default logit (for ranking var import)')
        print(coefftemp)
      }

      private$coefficients <-
        coefftemp[2:length(coefftemp)] # drop intercept

    },

    calculateMultiplyRes = function() {
      # Apply multiplication of coeff across each row of test set
      # Remove y (label) so we do multiplication only on X (features)
      private$dfTest[[self$params$predictedCol]] <- NULL

      if (isTRUE(self$params$debug)) {
        print('Test set after removing predicted column')
        print(str(private$dfTest))
      }

      private$multiply_res <-
        sweep(private$dfTestRAW, 2, private$coefficients, `*`)

      if (isTRUE(self$params$debug)) {
        print('Data frame after multiplying raw vals by coeffs')
        print(private$multiply_res[1:10, ])
      }

    },

    calculateOrderedFactors = function() {
      # Calculate ordered factors of importance for each row's prediction
      private$ordered.factors = t(sapply
                                  (1:nrow(private$multiply_res),
                                  function(i)
                                    colnames(private$multiply_res[order(private$multiply_res[i, ],
                                                                        decreasing = TRUE)])))

      if (isTRUE(self$params$debug)) {
        print('Data frame after getting column importance ordered')
        print(private$ordered.factors[1:10, ])
      }

    },

    saveDataIntoDb = function() {
      dtstamp = as.POSIXlt(Sys.time(), "GMT")

      # Combine grain.col, prediction, and time to be put back into SAM table
      outdf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtstamp,                           # LastLoadDTS
        private$grainTest,                 # GrainID
        private$predictedVals,             # PredictedProbab
        private$ordered.factors[, 1:3])    # Top 3 Factors

      prediectedResultsName = ""
      if (self$params$type == 'classification') {
        prediectedResultsName = "PredictedProbNBR"
      } else if (self$params$type == 'regression') {
        prediectedResultsName = "PredictedValueNBR"
      }
      colnames(outdf) <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol,
        prediectedResultsName,
        "Factor1TXT",
        "Factor2TXT",
        "Factor3TXT"
      )

      if (isTRUE(self$params$debug)) {
        print('Dataframe going to SQL Server:')
        print(str(outdf))
      }

      # Save df to table in SAM database
      out = sqlSave(
        channel = self$params$sqlConn,
        dat = outdf,
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

  ),

  #Public members
  public = list(
    #Constructor
    #p: new DeploySupervisedModelParameters class object,
    #   i.e. p = DeploySupervisedModelParameters$new()
    initialize = function(p) {
      super$initialize(p)

      if (!is.null(p$rfmtry))
        self$params$rfmtry <- p$rfmtry

      if (!is.null(p$trees))
        self$params$trees <- p$trees
    },

    #This would be a user-defined method
    # which gets called by buildFitObject function
    fitRandomForest = function() {
      # Set proper mtry (either based on recc default or specified)
      if (nchar(self$params$rfmtry) == 0 &&
          self$params$type == 'classification') {
        rfmtry.temp <- floor(sqrt(ncol(private$dfTrain)))
      } else if (nchar(self$params$rfmtry) == 0 &&
                 self$params$type == 'regression') {
        rfmtry.temp <- max(floor(ncol(private$dfTrain) / 3), 1)
      } else {
        rfmtry.temp <- self$params$rfmtry
      }

      if (isTRUE(self$params$debug)) {
        print('generating fit for random forest...')
      }

      if (self$params$type == 'classification') {
        private$fit = ranger(
          as.formula(paste(
            self$params$predictedCol, '.', sep = " ~ "
          )),
          data = private$dfTrain,
          probability = TRUE,
          num.trees = self$params$trees,
          write.forest = TRUE,
          mtry = rfmtry.temp
        )

      } else if (self$params$type == 'regression') {
        private$fit = ranger(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = private$dfTrain,
          num.trees = self$params$trees,
          write.forest = TRUE,
          mtry = rfmtry.temp
        )
      }

    },

    buildFitObject = function() {

      # Get fit object by random forest
      self$fitRandomForest()

    },

    #Override: Build Deploy Model
    buildDeployModel = function() {
      if (isTRUE(self$params$debug)) {
        print('Training data set immediately before training')
        print(str(private$dfTrain))
      }

      # Start default logit (for var importance)
      private$fitGeneralizedLinearModel()

      # Build fit object
      self$buildFitObject()

      print('Details for proability model:')
      print(private$fit)

    },

    #Override: deploy the model
    deploy = function() {

      # Connect to sql via odbc driver
      private$connectDataSource()

      if (isTRUE(self$params$useSavedModel)) {
        load("rmodel_var_import.rda")  # Produces fit.logit object
        private$fit.logit <- fit.logit

        load("rmodel_probability.rda") # Produces fit object (for probability)
        private$fit <- fit
      } else {
        private$registerClustersOnCores()

        # build deploy model
        self$buildDeployModel()
      }

      # Save model
      private$saveModel()

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
      if (isTRUE(!self$params$useSavedModel)) {
        private$stopClustersOnCores()
      }
      private$closeDataSource()
    },

    #Get predicted values
    getPredictedValsForUnitTest = function() {
      return(private$predictedValsForUnitTest)
    }
  )

)
