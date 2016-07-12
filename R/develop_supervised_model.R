# Import the common functions.
source('R/common.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @import caret
#' @import doParallel
#' @import e1071
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @import RODBC
#' @import grpreg
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grain.col The dataframe's column that has IDs pertaining to the grain
#' @param predicted.col Column that you want to predict.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{DeploySupervisedModel}}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' # The examples will run as-is, but you can find the data used here
#' # C:\Users\levi.thatcher\Documents\R\win-library\3.2\HCRTools\extdata OR
#' # C:\Program Files\R\R-3.2.3\library\HCRTools\extdata
#'
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(HCRTools)
#' data(iris)
#'
#' head(iris)
#'
#' set.seed(43)
#' o <- DevelopSupervisedModel$new(type = 'regression',
#'                                 df = iris,
#'                                 #grain.col = 'GrainID',
#'                                 predicted.col = 'Sepal.Width',
#'                                 impute = TRUE)
#'
#' o$grlasso(var.imp = TRUE,
#'           cores = 1,
#'           debug = TRUE)
#'
#' o$randForest(cores = 1,
#'              debug = FALSE)
#
#' print(proc.time() - ptm)
#'
#'
#' #### Example using csv data ####
#' library(HCRTools)
#' #setwd("C:/Your/script/location") # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
#'
#' totaldf <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' head(totaldf)
#'
#' set.seed(43)
#' o <- DevelopSupervisedModel$new(type = 'classification',
#'                                 df = totaldf,
#'                                 #grain.col = OPTIONAL/ENCOURAGED
#'                                 predicted.col = 'SalariedFlag',
#'                                 impute = FALSE)
#'
#' o$grlasso(var.imp = TRUE,
#'           cores = 1,
#'           debug = TRUE)
#'
#'
#' o$randForest(cores = 1,
#'              debug = FALSE)
#'
#' o$plotROC()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' o$getCutOffs('rf', .6)
#' print(proc.time() - ptm)
#'
#'
#' #### Example using SQL Server data ####
#' # This example requires:
#' #    1) That your local SQL Server has AdventureWorks2012 installed
#'
#' ptm <- proc.time()
#' library(HCRTools)
#' library(RODBC)
#'
#' connection.string = '
#'   driver={SQL Server};
#'   server=localhost;
#'   database=AdventureWorks2012;
#'   trusted_connection=true
#' '
#' query = "
#'   SELECT
#'     [OrganizationLevel]
#'     ,[MaritalStatus]
#'     ,[Gender]
#'     ,[SalariedFlag]
#'     ,[VacationHours]
#'     ,[SickLeaveHours]
#'   FROM [AdventureWorks2012].[HumanResources].[Employee]
#' "
#'
#' df <- SelectData(connection.string, query)
#' head(df)
#'
#' set.seed(43)
#' o <- DevelopSupervisedModel$new(type = 'classification',
#'                                 df = df,
#'                                 #grain.col = OPTIONAL/ENCOURAGED,
#'                                 predicted.col = 'MaritalStatus',
#'                                 impute = TRUE)
#'
#' o$grlasso(cores = 1,
#'           debug = FALSE)
#'
#' o$randForest(cores = 1,
#'              debug = FALSE)
#'
#' o$plotROC()
#' print(proc.time() - ptm)
#'
#' @export

# TODO: Add documentation for methods' parameters

DevelopSupervisedModel <- R6Class("DevelopSupervisedModel",
  public = list(

    df = NA,
    dfTrain = NA,
    dfTest = NA,
    predicted.col = NA,

    perfrf = NA,
    perflasso = NA,

    type = NA,
    prevalence = NA,
    AUC_lasso = NA,
    AUC_rf = NA,
    rmse_lasso = NA,
    mae_lasso = NA,
    rmse_rf = NA,
    mae_rf = NA,

    # For performance report
    memsize_of_dataset = NA,
    initial_dataset_rows = NA,
    initial_dataset_cols = NA,
    lasso.imp = NA,
    rf.var.imp = NA,
    lasso.conf.matrix = NA,
    rf.conf.matrix = NA,
    ROC_lasso = NA,
    ROC_rf = NA,

    initialize = function(type,
                          df,
                          grain.col = "",
                          predicted.col,
                          impute,
                          debug = FALSE) {

      # Creating attributes for performance report
      self$memsize_of_dataset = format(object.size(df), units = "Mb")
      self$initial_dataset_rows = nrow(df)
      self$initial_dataset_cols = ncol(df)
      # For use in confusion matrices
      self$prevalence = table(df[[predicted.col]])[2]

      if (length(ReturnColsWithMoreThanFiftyFactors(df))>0){
        message('The following columns in the data frame have more than fifty factors:')
        message(paste(shQuote(ReturnColsWithMoreThanFiftyFactors(df)), collapse=", "))
        message('This drastically reduces performance. Consider combining these factors into a new column with fewer factors.')
      }

      if (isTRUE(debug)) {
        print('Entire data set at the top of the constructor')
        print(str(df))
      }

      if (type != 'regression' && type != 'classification') {
          stop('Your type must be regression or classification')
      }

      if (type =='classification' && IsBinary(df[,predicted.col]) == FALSE){
          stop('Dependent variable must be binary for classification')
      }

      if (type =='regression' && IsBinary(df[,predicted.col]) == TRUE){
        stop('Dependent variable cannot be binary for regression')
      }

      # Convert to data.frame (in case of data.table)
      # This also converts chr cols to (needed) factors
      df <- as.data.frame(unclass(df))

      if (isTRUE(debug)) {
        print('Entire data set after converting to df and chr to factor')
        print(str(df))
      }

      if (isTRUE(impute)) {
          df[] <- lapply(df, ImputeColumn)

          if (isTRUE(debug)) {
            print('Entire data set after imputation')
            print(str(df))
          }

      } else {
          if (isTRUE(debug)) {
            print(paste0("Rows in data set before removing rows with NA's: ",
                         nrow(df)))
          }

          # Remove rows with any NA's
          df = na.omit(df)

          if (isTRUE(debug)) {
            print(paste0("Rows in data set after removing rows with NA's: ",
                         nrow(df)))
            print("Entire data set after removing rows with NA's")
            print(str(df))
          }
      }

      # Remove columns that are only NA
      df <- df[,colSums(is.na(df)) < nrow(df)]

      # Remove date columns
      datelist = grep("DTS$", colnames(df))
      if (length(datelist) > 0) {
          df = df[, -datelist]
      }

      if (isTRUE(debug)) {
        print('Entire data set after removing cols with DTS (ie date cols)')
        print(str(df))
        print('Now going to remove zero-var cols...')

      }

      # If grain.col is specified, remove this col
      if (nchar(grain.col) != 0) {
          df[[grain.col]] <- NULL
      }

      if (isTRUE(debug) && nchar(grain.col) != 0) {
        print('Entire data set after separating out grain col')
        print(str(df))
        print('Now splitting training set from validation set')
      }

      #Declare that the predicted col is a factor, or category to be predicted.
      if (type == 'classification') {
          df[[predicted.col]] = as.factor(df[[predicted.col]])
      }

      trainIndex = createDataPartition(y = df[[predicted.col]],
                                       p = 0.8,
                                       list = FALSE, times = 1)

      self$df = df
      self$dfTrain = df[ trainIndex,]
      self$dfTest  = df[-trainIndex,]

      if (isTRUE(debug)) {
        print('Training data set after splitting from main df')
        print(str(self$dfTrain))
      }

      if (isTRUE(debug)) {
        print('Validation data set after splitting from main df')
        print(str(self$dfTest))
      }

      # Remove rows where predicted.col is null in train
      self$dfTrain = RemoveRowsWithNAInSpecCol(self$dfTrain, predicted.col)

      if (isTRUE(debug)) {
        print('Training data set after removing rows where pred col is null')
        print(str(self$dfTrain))
      }

      self$predicted.col = predicted.col
      self$type = type
    },

    grlasso = function(var.imp = TRUE,
                       print.results = TRUE, # For rmarkdown report
                       cores = 4,
                       debug = FALSE) {

      if (isTRUE(debug)) {
        print('Training data set immediately before training')
        print(str(self$dfTrain))
      }

      if (cores > 1) {
        suppressMessages(library(doParallel))
        cl <- makeCluster(cores)
        registerDoParallel(cl)
      }

      dfTrainTEMP = self$dfTrain
      dfTestTEMP = self$dfTest
      #Create a model formula, without the predicted variable, for use in
      #creating the model matrix.
      modfmla = as.formula(paste("~",paste(names(dfTrainTEMP[ ,!(colnames(dfTrainTEMP) == self$predicted.col)]),
                                           collapse="+")))
      #Create the model matrix, without the intercept column, to be used in the
      #grouped Lasso function.
      modMat = model.matrix(modfmla, data=dfTrainTEMP)[,-1]

      #Make sure the dependent variable is numeric in both the train and test set.
      #If it is a factor, the first level alphabetically will be set to 0 and the other level
      #will be set to 1.
      if(is.factor(dfTrainTEMP[[self$predicted.col]])){
        dfTrainTEMP[[self$predicted.col]] =
          ifelse(dfTrainTEMP[[self$predicted.col]] == levels(dfTrainTEMP[[self$predicted.col]])[1],0,1)
      }

      if(is.factor(dfTestTEMP[[self$predicted.col]])){
        dfTestTEMP[[self$predicted.col]] =
          ifelse(dfTestTEMP[[self$predicted.col]] == levels(dfTestTEMP[[self$predicted.col]])[1],0,1)
      }

      #Creating the groups for the grouped Lasso model.
      #Factor variables have a group that is one less than the number of levels.
      #Everything else has length one.
      #The length of this vector should be the same as the number of columns in modMat.
      group = rep(1:(ncol(dfTrainTEMP)-1),
                  times=sapply(dfTrainTEMP[ ,!(colnames(dfTrainTEMP) == self$predicted.col)],
                               function(x) ifelse(is.factor(x), length(levels(x))-1, 1)))

      if (length(group) != ncol(modMat)) {
        stop('There is a mismatch in group definition and model matrix definition')
        #This message should likely be refined, perhaps something different for greater than
        #or less than ...
      }

      if (self$type == 'classification') {

        fit.grlasso = cv.grpreg(X = modMat,
                                y = dfTrainTEMP[[self$predicted.col]],
                                group = group,
                                #lambda = can enter values here, but we will use default
                                family = "binomial",
                                penalty="grLasso",
                                nfolds=5)

        #Index of largest lambda within one cvse of the lambda with lowest cve:
        #These are sorted from largest to smallest lambda, hence pulling the minimum index.
        ind.lambda1se = min(which(fit.grlasso$cve <= (fit.grlasso$cve+fit.grlasso$cvse)[fit.grlasso$min]))

        #largest lambda within one cvse of the lambda with lowest cve (ie. lambda to use in final fit):
        lambda.1se = fit.grlasso$lambda[ind.lambda1se]

        predictprob = predict(object = fit.grlasso,
                              X = model.matrix(modfmla, data=dfTestTEMP)[,-1],
                              lambda = lambda.1se,
                              type ="response")

        ytest = dfTestTEMP[[self$predicted.col]]
        pred <- prediction(predictprob, ytest)
        self$perflasso <- performance(pred, "tpr", "fpr")

        #NOTE THAT THE CLASS WILL BE 0 OR 1.
        predictclass = predict(object = fit.grlasso,
                               X = model.matrix(modfmla, data=dfTestTEMP)[,-1],
                               lambda = lambda.1se,
                               type ="class")

        if (isTRUE(debug)) {
          print(paste0('Rows in probability prediction: ', length(predictprob)))
          print('First 10 raw classification probability predictions')
          print(round(predictprob[1:10],2))
        }

        if (isTRUE(print.results)) {
          self$lasso.conf.matrix = confusionMatrix(predictclass,
                                                   dfTestTEMP[[self$predicted.col]],
                                                   prevalence=self$prevalence)
          print(self$lasso.conf.matrix)
          }

      ####
        ROC_lasso = roc(ytest~predictprob)
        self$AUC_lasso = auc(ROC_lasso)
        print(paste0('AUC: ', round(self$AUC_lasso, 2)))
        print(paste0('95% CI AUC: (', round(ci(self$AUC_lasso)[1],2), ',', round(ci(self$AUC_lasso)[3],2), ')'))
        self$ROC_lasso = ROC_lasso

        print("Grouped Lasso coefficients:")
        print(fit.grlasso$fit$beta[,ind.lambda1se])


        if (isTRUE(var.imp)) {
          lasso.imp = names(dfTrainTEMP[ ,!(colnames(dfTrainTEMP) == self$predicted.col)])[predict(fit.grlasso, modMat,type = "groups",
                                                                                                   lambda = lambda.1se)]
        }

      } else if (self$type == 'regression') {

        fit.grlasso = cv.grpreg(X = modMat,
                                y = dfTrainTEMP[[self$predicted.col]],
                                group = group,
                                #lambda = can enter values here, but we will use default
                                family = "gaussian",
                                penalty="grLasso",
                                nfolds=5)

        #Index of largest lambda within one cvse of the lambda with lowest cve:
        #These are sorted from largest to smallest lambda, hence pulling the minimum index.
        ind.lambda1se = min(which(fit.grlasso$cve <= (fit.grlasso$cve+fit.grlasso$cvse)[fit.grlasso$min]))

        #largest lambda within one cvse of the lambda with lowest cve (ie. lambda to use in final fit):
        lambda.1se = fit.grlasso$lambda[ind.lambda1se]

        predictions = predict(object = fit.grlasso,
                              X = model.matrix(modfmla, data=dfTestTEMP)[,-1],
                              lambda = lambda.1se,
                              type ="response")


        if (isTRUE(debug)) {
          print(paste0('Rows in regression prediction: ',
                       nrow(predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(predictions[1:10],2))
        }

        #IF predicted.col IS NOT NUMERIC TO BEGIN WITH, THIS IS NOT GOING TO FIX IT.
        ytest = as.numeric(dfTestTEMP[[self$predicted.col]])

        if (isTRUE(print.results)) {
          self$rmse_lasso = sqrt(mean((ytest - predictions)^2))
          print(paste0('RMSE: ', round(self$rmse_lasso, 2)))

          self$mae_lasso = mean(abs(ytest - predictions))
          print(paste0('MAE: ', round(self$mae_lasso, 2)))

          print("Grouped Lasso coefficients:")
          print(fit.grlasso$fit$beta[,ind.lambda1se])
          }
      }

      if (cores > 1) {
        stopCluster(cl)
        registerDoSEQ()
      }

      if (isTRUE(var.imp)) {
        self$lasso.imp = names(dfTrainTEMP[ ,!(colnames(dfTrainTEMP) == self$predicted.col)])[predict(fit.grlasso, modMat,type = "groups",lambda = lambda.1se)]
        print(paste0("Variables with non-zero coefficients: ",paste0(self$lasso.imp, collapse=", ")))
      }

      return(invisible(fit.grlasso))
    },

randForest = function(var.imp = TRUE,
                      print.results = TRUE, # For rmarkdown report
                      cores = 4,
                      tune = FALSE,
                      debug = FALSE,
                      trees = 201) {

      if (isTRUE(debug)) {
        print('Training data set immediately before training')
        print(str(self$dfTrain))
      }

      if (cores > 1) {
          suppressMessages(library(doParallel))
          cl <- makeCluster(cores)
          registerDoParallel(cl)
      }

      if (self$type == 'classification') {
          if (isTRUE(tune)) {
            # Create reasonable gridsearch for mtry
            # This optimal value comes from randomForest documentation
            optimal = floor(sqrt(ncol(self$dfTrain)))

            mtry_list = c(optimal - 1, optimal, optimal + 1)
            # Make it such that lowest mtry is 2
            if (length(which(mtry_list < 0)) > 0) {
              mtry_list = mtry_list + 3
            } else if (length(which(mtry_list == 0)) > 0) {
              mtry_list = mtry_list + 2
            } else if (length(which(mtry_list == 1)) > 0) {
              mtry_list = mtry_list + 1
            }

            print(paste(c('Performing grid search across these mtry values: ',
                          mtry_list), collapse = " "))

            grid <-  data.frame(mtry = mtry_list) # Number of features/tree

            train.control <- trainControl(
              method = "CV",
              number = 5,
              verboseIter = if (isTRUE(debug)) TRUE else FALSE,
              classProbs = TRUE,
              summaryFunction = twoClassSummary
            )
          } else {

            grid <- data.frame(.mtry = floor(sqrt(ncol(self$dfTrain))))

            train.control <- trainControl(
              method = "none",
              number = 1,
              verboseIter = if (isTRUE(debug)) TRUE else FALSE,
              classProbs = TRUE,
              summaryFunction = twoClassSummary
            )
          }

          fit.rf = train(
            x = self$dfTrain[ ,!(colnames(self$dfTrain) == self$predicted.col)],
            y = factor(self$dfTrain[[self$predicted.col]]),
            method = "ranger",
            importance = 'impurity',
            metric = "ROC",
            num.trees = trees,
            tuneGrid = grid,
            trControl = train.control
          )

          predictprob = predict(object = fit.rf,
                                newdata = self$dfTest,
                                type = 'prob')

          if (isTRUE(debug)) {
            print(paste0('Rows in probability prediction: ', nrow(predictprob)))
            print('First 10 raw classification probability predictions')
            print(round(predictprob[1:10,2],2))
          }

          ytest = as.numeric(self$dfTest[[self$predicted.col]])
          pred <- prediction(predictprob[,2], ytest)
          self$perfrf <- performance(pred, "tpr", "fpr")

          predictclass = predict(fit.rf,
                                 newdata = self$dfTest)

          if (isTRUE(debug)) {
            print(paste0('Rows in discrete prediction: ', nrow(predictprob)))
            print('First 10 raw classification discrete predictions')
            print(predictclass[1:10])
          }

          if (isTRUE(print.results)) {
            self$rf.conf.matrix = confusionMatrix(predictclass,
                                             self$dfTest[[self$predicted.col]],
                                             prevalence=self$prevalence)

            print(self$rf.conf.matrix)
          }

          ROC_rf = roc(ytest~predictprob[,2])
          self$AUC_rf = auc(ROC_rf)
          self$ROC_rf = ROC_rf

          if (isTRUE(print.results)) {
            print(paste0('AUC: ', round(self$AUC_rf, 2)))
            print(paste0('95% CI AUC: (', round(ci(self$AUC_rf)[1],2), ',', round(ci(self$AUC_rf)[3],2), ')'))
          }

      } else if (self$type == 'regression') {

        if (isTRUE(tune)) {
          # Create reasonable gridsearch for mtry
          # This optimal value comes from randomForest documentation
          optimal = max(floor(ncol(self$dfTrain)/3), 1)

          mtry_list = c(optimal - 1, optimal, optimal + 1)
          # Make it such that lowest mtry is 2
          if (length(which(mtry_list < 0)) > 0) {
            mtry_list = mtry_list + 3
          } else if (length(which(mtry_list == 0)) > 0) {
            mtry_list = mtry_list + 2
          } else if (length(which(mtry_list == 1)) > 0) {
            mtry_list = mtry_list + 1
          }

          print(paste(c('Performing grid search across these mtry values: ',
                        mtry_list), collapse = " "))

          grid <-  data.frame(mtry = mtry_list) # Number of features/tree

          train.control <- trainControl(
            method = "CV",
            number = 5,
            verboseIter = if (isTRUE(debug)) TRUE else FALSE
          )
        } else {
          grid <- data.frame(.mtry = max(floor(ncol(self$dfTrain)/3), 1))

          train.control <- trainControl(
            method = "none",
            number = 1,
            verboseIter = if (isTRUE(debug)) TRUE else FALSE
          )
        }

        fit.rf = train(
          x = self$dfTrain[ ,!(colnames(self$dfTrain) == self$predicted.col)],
          y = self$dfTrain[[self$predicted.col]],
          method = "ranger",
          importance = 'impurity',
          metric = "RMSE",
          num.trees = trees,
          tuneGrid = grid,
          trControl =  train.control
        )

      predictions = predict(fit.rf, newdata = self$dfTest)

      if (isTRUE(debug)) {
          print(paste0('Rows in regression prediction: ', length(predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(predictions[1:10],2))
      }

      ytest = as.numeric(self$dfTest[[self$predicted.col]])

      if (isTRUE(print.results)) {
        self$rmse_rf = sqrt(mean((ytest - predictions)^2))
        print(paste0('RMSE: ', round(self$rmse_rf, 8)))
        self$mae_rf = mean(abs(ytest - predictions))
        print(paste0('MAE: ', round(self$mae_rf, 8)))
      }
      } # End regression ifelse

      if (cores > 1) {
        stopCluster(cl)
        registerDoSEQ()
      }

      if (isTRUE(var.imp)) {
        self$rf.var.imp <- varImp(fit.rf, top = 20)

        print(self$rf.var.imp)
        print(dotPlot(self$rf.var.imp))
      }

      return(invisible(fit.rf))
   },

plotROC = function() {
  if (IsBinary(self$df[[self$predicted.col]])) {
    plot(self$ROC_rf, col = "red", legacy.axes=TRUE,
         mar=c(4, 4, 3, 2)+.1)
    plot(self$ROC_lasso, add = TRUE, col = "blue", lty=2)
    title(main = "ROC")
    legend("bottomright",
           c("Random Forest", "Grouped Lasso"),
           cex = 0.8,
           col = c("red", "blue"),
           lty = 1:2,
           inset = .1)
     } else {
         print("ROC is not created because the column you're predicting
                is not binary")
    }
   },

   getCutOffs = function(method, tpr) {
     if (method != 'rf' && method != 'lasso') {
       print("method argument has to be 'rf' or 'lasso'")
     }

     if (method == 'rf') {
       # Get index of when true-positive rate is > tpr
       indy <- which(as.numeric(unlist(self$perfrf@y.values)) > tpr)

       # Correpsonding probability cutoff value (ie when category falls to 1)
       print('Corresponding cutoff for 0/1 fallover:')
       print(self$perfrf@alpha.values[[1]][indy[1]])

       # Corresponding false-positive rate
       print('Corresponding false-positive rate:')
       print(self$perfrf@x.values[[1]][indy[1]][[1]])

     } else if (method == 'lasso') {
       # Get index of when true-positive rate is > tpr
       indy <- which(as.numeric(unlist(self$perflasso@y.values)) > tpr)

       # Correpsonding probability cutoff value (ie when category falls to 1)
       print('Corresponding cutoff for 0/1 fallover:')
       print(self$perflasso@alpha.values[[1]][indy[1]])

       # Corresponding false-positive rate
       print('Corresponding false-positive rate:')
       print(self$perflasso@x.values[[1]][indy[1]][[1]])
     }
   }
  ) # End method list
) # End class

######
