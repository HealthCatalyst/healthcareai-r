# Import the common functions.
source('R/common.R')

#' Deploy a production-ready predictive model
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
#' @param grain.col The dataframe's column that has IDs pertaining to the grain
#' @param window.col This column dictates the split between model training and
#' test sets. Those rows with zeros in this column indicate the training set
#' while those that have ones indicate the test set
#' @param predicted.col Column that you want to predict.
#' @param impute For training df, set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @seealso \code{\link{DevelopSupervisedModel}}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' # The data will read in as-is and you can find the data itself here
#' # C:\Users\levi.thatcher\Documents\R\win-library\3.2\HCRTools\extdata OR
#' # C:\Program Files\R\R-3.2.3\library\HCRTools\extdata
#'
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
#' # CREATE TABLE dbo.HCRDeployTest1BASE(
#' #   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#' #   GrainID int <--change to match inputID, PredictedProbNBR decimal(38, 2),
#' #   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
#' # )
#'
#' #setwd("C:/Your/script/location") # Needed if using YOUR CSV file
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
#' df$VacationHours <- NULL
#'
#' head(df)
#'
#' o <- DeploySupervisedModel$new(type = 'classification',
#'                                df = df,
#'                                grain.col = 'GrainID',
#'                                window.col = 'InTestWindow',
#'                                predicted.col = 'SalariedFlag',
#'                                impute = TRUE,
#'                                debug = FALSE,  # <-- change to TRUE to debug
#'                                use.saved.model = FALSE)
#'
#'
#' o$deploy(model = 'rf',
#'          cores = 1,
#'          sqlcnxn = connection.string,
#'          # Note: Do not use [ or ] in output table
#'          dest.schema.table = 'dbo.HCRDeployTest1BASE',
#'          debug = FALSE,  # <-- change this to TRUE to debug
#'          rfmtry = 2)
#'
#' print(proc.time() - ptm)
#'
#'
#' #### Regression example using data from SQL Server ####
#' # This example requires
#' #     1) You set your working directory to source file location
#' #     2) To receive predictions from R back to SQL Server, you'll need to
#' #        save and run an entity in SAMD that has only the following columns
#'
#' # GrainID decimal(38,0) not null, <--change col to match ID in summary table
#' # PredictedValueNBR decimal(38,2),
#' # Factor1TXT varchar(255),
#' # Factor2TXT varchar(255),
#' # Factor3TXT varchar(255),
#'
#' # If you prefer to not use SAMD, execute this in SSMS to create output table:
#' # CREATE TABLE dbo.HCRDeployTest2BASE(
#' #   BindingID float, BindingNM varchar(255), LastLoadDTS datetime2,
#' #   GrainID int <--change to match inputID, PredictedValueNBR decimal(38, 2),
#' #   Factor1TXT varchar(255), Factor2TXT varchar(255), Factor3TXT varchar(255)
#' # )
#'
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' connection.string <- 'driver={SQL Server};
#'                       server=localhost;
#'                       database=SAM;
#'                       trusted_connection=true'
#'
#' # Use this for an example SQL source:
#' # query <- "SELECT * FROM [SAM].[YourCoolSAM].[SomeTrainingSetTable]"
#' # df <- SelectData(connection.string, query)
#'
#' # Can delete these four lines when you set up your SQL connection/query
#' csvfile <- system.file("extdata", "HREmployeeDeploy.csv",package = "HCRTools")
#' df <- read.csv(file = csvfile,
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' head(df)
#'
#' df <- subset(df, select = -c(SalariedFlag))
#'
#' o <- DeploySupervisedModel$new(type = 'regression',
#'                                df = df,
#'                                grain.col = 'GrainID',
#'                                window.col = 'InTestWindow',
#'                                predicted.col = 'VacationHours',
#'                                impute = TRUE,
#'                                debug = FALSE,  # <-- change to T for debug
#'                                use.saved.model = FALSE)
#'
#' o$deploy(model = 'logit',
#'          cores = 1,
#'          sqlcnxn = connection.string,
#'          debug = FALSE,  # <-- change this to TRUE to debug
#'          # Note: Do not use [ or ] in output table
#'          dest.schema.table = 'dbo.HCRDeployTest2BASE')
#'
#' print(proc.time() - ptm)
#'
#' @export

# TODO: Add documentation for methods' parameters

DeploySupervisedModel <- R6Class("DeploySupervisedModel",
  public = list(

    df = NA,
    grain.col = NA,
    window.col = NA,
    use.saved.model = NA,
    predicted.col = NA,
    impute = NA,

    dfTest = NA,
    dfTestRAW = NA,
    dfTrain = NA,
    graintest = NA,

    type = NA,

    initialize = function(type,
                          df,
                          grain.col,
                          window.col,
                          predicted.col,
                          impute,
                          use.saved.model,
                          debug = FALSE) {

      # TODO: if type is not specified at all - print a helpful message.

      if (length(ReturnColsWithMoreThanFiftyFactors(df))>0){
        message('The following columns in the data frame have more than fifty factors:')
        message(paste(shQuote(ReturnColsWithMoreThanFiftyFactors(df)), collapse=", "))
        message(paste('This drastically reduces performance.',
                      'Consider combining these factors into a new column with fewer factors.'))
      }

      if (type != 'regression' && type != 'classification') {
        stop('Your type must be regression or classification')
      }

      # Convert to data.frame (in case of data.table)
      # This also converts chr cols to (needed) factors
      df <- as.data.frame(unclass(df))

      # Remove columns that are only NA
      if (isTRUE(use.saved.model)) {
          # Put predicted.col back (since it's NULL when sql only pulls in test)
          temp <- df[[predicted.col]]
          df <- df[,colSums(is.na(df)) < nrow(df)]
          df[[predicted.col]] <- temp
      } else {
          df <- df[,colSums(is.na(df)) < nrow(df)]
      }

      # Remove date columns
      datelist = grep("DTS$", colnames(df))
      if (length(datelist) > 0) {
          df = df[, -datelist]
      }

      if (isTRUE(debug)) {
        print('Entire df after removing cols with DTS')
        print(str(df))
        print('Now going to remove zero-var cols...')
      }

      # Remove columns with zero variance
      df <- RemoveColsWithAllSameValue(df)

      if (isTRUE(debug)) {
        print('Entire df after removing feature cols w/zero var')
        print(str(df))
      }

      # Remove grain.col from df; below we split it into graintest
      if (nchar(grain.col) != 0) {
        full.grain <- df[[grain.col]]
        df[[grain.col]] <- NULL
      } else {
        stop('You must specify a GrainID column when initializing
             TuneSupervisedModel')
      }

      if (isTRUE(debug)) {
        print('Entire data set after separating out grain col')
        print(str(df))
        print('Now creating dummy variables')
      }

      # Make sure label column is numeric before creating dummy var
      df[[predicted.col]] = as.numeric(df[[predicted.col]])

      # Split factor columns into dummy columns (for use in deploypred method)
      data <- dummyVars( ~ ., data = df, fullRank = T)
      df <- data.frame(predict(data, newdata = df, na.action = na.pass))

      # Now that we have dummy vars, switch label to factor so this is classif.
      if (type == 'classification') {
        # Since caret can't handle 0/1 for classif, need to convert to N/Y
        # http://stackoverflow.com/questions/18402016/error-when
        # -i-try-to-predict-class-probabilities-in-r-caret
        df[[predicted.col]] <- ifelse(df[[predicted.col]] == 1, 'N', 'Y')
        df[[predicted.col]] <- as.factor(df[[predicted.col]])
      }

      if (isTRUE(debug)) {
        print('Entire data set after creating dummy vars')
        print(str(df))
      }

      # If creating new model, split train set from df
      if (isTRUE(!use.saved.model)) {
        # Note that the paste0 is needed here bc window.col is now dummy var
        # For now it's assumed that dummyVars always sets Y to 1
        dfTrain = df[df[[paste0(window.col,'.Y')]] == 0,]
        # Drop window col from train set, as it's no longer needed
        dfTrain <- dfTrain[, !(names(dfTrain) %in% c(window.col,
                                                     paste0(window.col,'.Y')))]

        if (isTRUE(debug)) {
          print('Training data set after splitting from main df')
          print(str(dfTrain))
        }

        # Remove rows where predicted.col is null in train
        dfTrain = RemoveRowsWithNAInSpecCol(dfTrain, predicted.col)

        if (isTRUE(debug)) {
          print('Training data set after removing rows where pred col is null')
          print(str(dfTrain))
          print('Splitting train and test from main df...')
        }
      }

      # Always create test set from df, and drop window.cols from test set
      dfTest = df[df[[paste0(window.col,'.Y')]] == 1,]
      dfTest <- dfTest[, !(names(dfTest) %in% c(window.col,
                                                 paste0(window.col,'.Y')))]

      if (isTRUE(debug)) {
        print('Test set after splitting from df (and then removing windowcol)')
        print(str(dfTest))
      }

      # Now that we have train/test, split grain col into test (for use at end)
      if (nchar(grain.col) != 0) {
          graintest <- full.grain[df[[paste0(window.col,'.Y')]] == 1]

          if (isTRUE(debug)) {
            print('Grain col vector with rows of test set (after created)')
            print(graintest[1:10])
          }

      } else {
          stop('You must specify a GrainID column when initializing
          TuneSupervisedModel')
      }

      # Use imputation on train set (if we're creating new model)
      if (isTRUE(impute) && isTRUE(!use.saved.model)) {
          if (isTRUE(debug)) {
            print('Doing imputation on training set...')
          }
          dfTrain[] <- lapply(dfTrain, ImputeColumn)

          if (isTRUE(debug)) {
            print('Training set after doing imputation')
            print(str(dfTrain))
          }

      # If user doesn't ask for imputation, remove rows with any NA's
      } else if (isTRUE(!impute) && isTRUE(!use.saved.model)) {
          dfTrain = na.omit(dfTrain)

          if (isTRUE(debug)) {
            print('Training set after removing rows with NULLs')
            print(str(dfTrain))
          }
      }

      # Pass raw (un-imputed) dfTest to object, so important factors aren't null
      self$dfTestRAW <- dfTest[, !(names(dfTest) %in% c(predicted.col))]

      if (isTRUE(debug)) {
        print('Raw test set (sans imputation) created for mult with coeffs')
        print(str(self$dfTestRAW))
      }

      # Always do imputation on all of test set (since now no NAs in pred.col)
      dfTest[] <- lapply(dfTest, ImputeColumn)

      if (isTRUE(debug)) {
        print('Test set after undergoing imputation')
        print(str(dfTest))
      }

      # If creating a new model, pass training set to object
      if (isTRUE(!use.saved.model)) self$dfTrain <- dfTrain

      self$dfTest <- dfTest
      self$graintest <- graintest
      self$grain.col <- grain.col
      self$predicted.col <- predicted.col
      self$use.saved.model <- use.saved.model
      self$type <- type

    }, # End intialize method (e.g., constructor)

    deploy = function(model,
                      cores = 4,
                      sqlcnxn,
                      dest.schema.table,
                      rfmtry = "",
                      trees = 201,
                      debug = FALSE) {

    # Convert the connection string into a real connection object.
    odbcCloseAll()
    sqlcnxn <- odbcDriverConnect(sqlcnxn)

      if (isTRUE(self$use.saved.model)) {
        load("rmodel_var_import.rda")  # Produces fit.logit object
        load("rmodel_probability.rda") # Produces fit object (for probability)
      } else {
          if (cores > 1) {
            suppressMessages(library(doParallel))
            cl <- makeCluster(cores)
            registerDoParallel(cl)
          }

          if (isTRUE(debug)) {
            print('Training data set immediately before training')
            print(str(self$dfTrain))
          }

          # Start default logit (for var importance)
          if (self$type == 'classification') {
            fit.logit = glm(
              as.formula(paste(self$predicted.col, '.', sep = " ~ ")),
              data = self$dfTrain,
              family = binomial(link = "logit"),
              metric = "ROC",
              control = list(maxit = 10000),
              trControl = trainControl(classProbs = TRUE,
                                       summaryFunction = twoClassSummary))

          } else if (self$type == 'regression') {
            fit.logit = glm(
              as.formula(paste(self$predicted.col, '.', sep = " ~ ")),
              data = self$dfTrain,
              metric = "RMSE",
              control = list(maxit = 10000))
          }
          if (cores > 1) {
            stopCluster(cl)
            registerDoSEQ()
          }

          # Check which model was chosen (for non-factor-ranking train work)
          if (model == 'rf') {

            # Set proper mtry (either based on recc default or specified)
            if (nchar(rfmtry) == 0 && self$type == 'classification') {
                rfmtry.temp <- floor(sqrt(ncol(self$dfTrain)))
            } else if (nchar(rfmtry) == 0 && self$type == 'regression') {
                rfmtry.temp <- max(floor(ncol(self$dfTrain)/3), 1)
            } else {
                rfmtry.temp <- rfmtry
            }

            if (cores > 1) {
                suppressMessages(library(doParallel))
                cl <- makeCluster(cores)
                registerDoParallel(cl)
            }
            if (self$type == 'classification') {

              fit = ranger(
                as.formula(paste(self$predicted.col, '.', sep = " ~ ")),
                data = self$dfTrain,
                probability = TRUE,
                num.trees = trees,
                write.forest = TRUE,
                mtry = rfmtry.temp)

            } else if (self$type == 'regression') {

              fit = ranger(
                as.formula(paste(self$predicted.col, '.', sep = " ~ ")),
                data = self$dfTrain,
                num.trees = trees,
                write.forest = TRUE,
                mtry = rfmtry.temp)
            }

              if (cores > 1) {
                stopCluster(cl)
                registerDoSEQ()
              }

          } else {
              fit = fit.logit # set to logit, if logit is chosen over rf
          }
      }

      print('Details for proability model:')
      print(fit)

      # Save models if specified
      if (isTRUE(!self$use.saved.model)) {
          save(fit.logit, file = "rmodel_var_import.rda")
          save(fit, file = "rmodel_probability.rda")
      }

      # This isn't needed if formula interface is used in randomForest
      self$dfTest[[self$predicted.col]] <- NULL

      if (isTRUE(debug)) {
        print('Test set before being used in predict(), after removing y')
        print(str(self$dfTest))
      }

      if (self$type == 'classification') {
        if (model == 'rf') {  #  these are probabilities
           predictedVALStemp = predict(fit, data = self$dfTest)
           predictedVALS <- predictedVALStemp$predictions[,2]
           print('Probability predictions are based on random forest')

        } else {              #  these are probabilities
           predictedVALS = predict(fit,
                                   newdata = self$dfTest, type = "response")
           print('Probability predictions are based on logistic')
        }

        if (isTRUE(debug)) {
            print(paste0('Rows in prob prediction: ', nrow(predictedVALS)))
            print('First 10 raw classification probability predictions')
            print(round(predictedVALS[1:10],2))
        }

      } else if (self$type == 'regression') {  # this is in-kind prediction
        if (model == 'rf') {
            predictedVALStemp = predict(fit, data = self$dfTest)
            predictedVALS <- predictedVALStemp$predictions
        } else {
          predictedVALS = predict(fit, newdata = self$dfTest)
        }

        if (isTRUE(debug)) {
            print(paste0('Rows in regression prediction: ',
                         length(predictedVALS)))
            print('First 10 raw regression predictions (with row # first)')
            print(round(predictedVALS[1:10],2))
        }

      }

      # Do semi-manual calc to rank cols by order of importance
      coefftemp <- fit.logit$coefficients

      if (isTRUE(debug)) {
          print('Coefficients for the default logit (for ranking var import)')
          print(coefftemp)
      }

      coefficients <- coefftemp[2:length(coefftemp)] # drop intercept

      # Apply multiplication of coeff across each row of test set
      # Remove y (label) so we do multiplication only on X (features)
      self$dfTest[[self$predicted.col]] <- NULL

      if (isTRUE(debug)) {
        print('Test set after removing predicted column')
        print(str(self$dfTest))
      }

      multiply_res <- sweep(self$dfTestRAW, 2, coefficients, `*`)

      if (isTRUE(debug)) {
        print('Data frame after multiplying raw vals by coeffs')
        print(multiply_res[1:10,])
      }

      # Calculate ordered factors of importance for each row's prediction
      ordered.factors = t(sapply
                          (1:nrow(multiply_res),
                          function(i)
                            colnames(multiply_res[order(multiply_res[i,],
                                                        decreasing = TRUE)])))

      if (isTRUE(debug)) {
        print('Data frame after getting column importance ordered')
        print(ordered.factors[1:10,])
      }

      dtstamp = as.POSIXlt(Sys.time(), "GMT")

      # Combine grain.col, prediction, and time to be put back into SAM table

      outdf <- data.frame(0,                           # BindingID
                          'R',                         # BindingNM
                          dtstamp,                     # LastLoadDTS
                          self$graintest,              # GrainID
                          predictedVALS,               # PredictedProbab
                          ordered.factors[,1:3])       # Top 3 Factors

      if (self$type == 'classification') {
        colnames(outdf) <- c("BindingID","BindingNM","LastLoadDTS",
                             self$grain.col, "PredictedProbNBR",
                             "Factor1TXT", "Factor2TXT", "Factor3TXT")

        } else if (self$type == 'regression') {
        colnames(outdf) <- c("BindingID","BindingNM","LastLoadDTS",
                             self$grain.col, "PredictedValueNBR",
                             "Factor1TXT", "Factor2TXT", "Factor3TXT")
      }

      if (isTRUE(debug)) {
        print('Dataframe going to SQL Server:')
        print(str(outdf))
      }

      # Save df to table in SAM database
      out = sqlSave(channel = sqlcnxn,
                    dat = outdf,
                    tablename = dest.schema.table,
                    append = T,
                    rownames = F,
                    colnames = F,
                    safer = T,
                    nastring = NULL,
                    verbose = debug)

      # Clean up.
      odbcCloseAll()

      # Print success if insert was successful
      if (out == 1) {
        print('SQL Server insert was successful')
      }

    }
  ) # End method list
) # End Deploy class
