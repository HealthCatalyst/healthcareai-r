# Import the common functions.
source('R/common.R')
source('R/SupervisedModel.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
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
#'
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' data(iris)
#' head(iris)
#'
#' set.seed(43)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = iris
#' p$type = 'regression'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'Sepal.Width'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' print(proc.time() - ptm)
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
#' p <- SupervisedModelParameters$new()
#' p$df = totaldf
#' p$type = 'classification'
#' p$impute = FALSE
#' p$grainCol = ''
#' p$predictedCol = 'SalariedFlag'
#' p$debug = TRUE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' lasso$getCutOffs(tpr=.8)
#' print(proc.time() - ptm)
#'
#' #### Example using SQL Server data ####
#' # This example requires:
#' #    1) That your local SQL Server has AdventureWorks2012 installed
#'
#' ptm <- proc.time()
#' library(HCRTools)
#' library(RODBC)
#'
#' connection.string = "
#' driver={SQL Server};
#' server=localhost;
#' database=AdventureWorks2012;
#' trusted_connection=true
#' "
#'
#' query = "
#' SELECT
#' [OrganizationLevel]
#' ,[MaritalStatus]
#' ,[Gender]
#' ,IIF([SalariedFlag]=0,'N','Y') AS SalariedFlag
#' ,[VacationHours]
#' ,[SickLeaveHours]
#' FROM [AdventureWorks2012].[HumanResources].[Employee]
#' "
#'
#' df <- SelectData(connection.string, query)
#' head(df)
#'
#' set.seed(43)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = df
#' p$type = 'classification'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'SalariedFlag'
#' p$debug = FALSE
#' p$cores = 1
#'
#' # Run Lasso
#' lasso <- Lasso$new(p)
#' lasso$run()
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' #Plot ROCs from both supervised model classes
#' plot(rf$getROC(), col = "red", legacy.axes=TRUE, mar=c(4, 4, 3, 2)+.1)
#' plot(lasso$getROC(), add = TRUE, col = "blue", lty=2)
#' title(main = "ROC")
#' legend("bottomright",
#'        c("RandomForest", "Lasso"),
#'        cex = 0.8,
#'        col = c("red", "blue"),
#'        lty = 1:2,
#'        inset = .1)
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' lasso$getCutOffs(tpr=.8)
#'
#' print(proc.time() - ptm)
#'
#' @export


Lasso <- R6Class("Lasso",

  #Inheritance
  inherit = SupervisedModel,

  #Private members
  private = list(

  	#data related
  	dfTrainTEMP = NA,
  	dfTestTEMP = NA,

  	#models for formula and matrix
  	modfmla = NA,
  	modMat = NA,

  	#groups for the grouped Lasso model
  	group = NA,

  	#fit model and lamda values
  	fit.grlasso = NA,
  	ind.lambda1se = NA,
  	lambda.1se = NA,

  	#predictions
  	predictions = NA,

  	#performance metrics
  	confMatrix = NA,
  	ROC = NA,
  	AUC = NA,
  	rmse = NA,
  	mae = NA,
  	perf = NA,
  	prevalence = NA

  ),

  #Public members
  public = list(

    #Constructor
    #p: new SuperviseModelParameters class object, i.e. p = SuperviseModelParameters$new()
    initialize = function (p) {
      super$initialize(p)
    },

    #Override: build Grouped Lasso model
    buildModel = function () {

      private$dfTrainTEMP = private$dfTrain
      private$dfTestTEMP = private$dfTest

      #Create a model formula, without the predicted variable, for use in
      #creating the model matrix.
      private$modfmla = as.formula(paste("~",paste(names(private$dfTrainTEMP[ ,!(colnames(private$dfTrainTEMP) == self$params$predictedCol)]),
                                           collapse="+")))

      #Create the model matrix, without the intercept column, to be used in the
      #grouped Lasso function.
      private$modMat = model.matrix(private$modfmla, data=private$dfTrainTEMP)[,-1]

      #Make sure the dependent variable is numeric in both the train and test set.
      #If it is a factor, the first level alphabetically will be set to 0 and the other level
      #will be set to 1.
      if(is.factor(private$dfTrainTEMP[[self$params$predictedCol]])){
        private$dfTrainTEMP[[self$params$predictedCol]] =
          ifelse(private$dfTrainTEMP[[self$params$predictedCol]] == levels(private$dfTrainTEMP[[self$params$predictedCol]])[1],0,1)
      }

      if(is.factor(private$dfTestTEMP[[self$params$predictedCol]])){
        private$dfTestTEMP[[self$params$predictedCol]] =
          ifelse(private$dfTestTEMP[[self$params$predictedCol]] == levels(private$dfTestTEMP[[self$params$predictedCol]])[1],0,1)
      }


      #Creating the groups for the grouped Lasso model.
      #Factor variables have a group that is one less than the number of levels.
      #Everything else has length one.
      #The length of this vector should be the same as the number of columns in modMat.
      private$group = rep(1:(ncol(private$dfTrainTEMP)-1),
                  times=sapply(private$dfTrainTEMP[ ,!(colnames(private$dfTrainTEMP) == self$params$predictedCol)],
                               function(x) ifelse(is.factor(x), length(levels(x))-1, 1)))

      if (length(private$group) != ncol(private$modMat)) {
        stop('There is a mismatch in group definition and model matrix definition')
        #This message should likely be refined, perhaps something different for greater than
        #or less than ...
      }

      #Generate fit grlasso object
      familyModuleName = ""
      if(self$params$type == 'classification')
        familyModuleName = "binomial"
      else if(self$params$type == 'regression')
        familyModuleName = "gaussian"

      private$fit.grlasso = cv.grpreg(X = private$modMat,
                                      y = private$dfTrainTEMP[[self$params$predictedCol]],
                                      group = private$group,
                                      #lambda = can enter values here,
                                      #  but we will use default
                                      family = familyModuleName,
                                      penalty = "grLasso",
                                      nfolds = 5)

    },

    #predict results
    performPrediction = function () {

      #Index of largest lambda within one cvse of the lambda with lowest cve:
      #These are sorted from largest to smallest lambda, hence pulling the minimum index.
      private$ind.lambda1se = min(which(private$fit.grlasso$cve <= (private$fit.grlasso$cve + private$fit.grlasso$cvse)[private$fit.grlasso$min]))

      #largest lambda within one cvse of the lambda with lowest cve (ie. lambda to use in final fit):
      private$lambda.1se = private$fit.grlasso$lambda[private$ind.lambda1se]

      #predictions
      private$predictions = predict(object = private$fit.grlasso,
                                    X = model.matrix(private$modfmla, data=private$dfTestTEMP)[,-1],
                                    lambda = private$lambda.1se,
                                    type = "response")

    },

    #generate performance metrics
    generatePerformanceMetrics = function () {

      # classification
      if(self$params$type == 'classification') {

        predictprob <- private$predictions

        #prediction
        ytest = private$dfTestTEMP[[self$params$predictedCol]]
        pred <- ROCR::prediction(private$predictions, ytest)

        #performance
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        #NOTE THAT THE CLASS WILL BE 0 OR 1.
        predictclass = predict(object = private$fit.grlasso,
                               X = model.matrix(private$modfmla, data=private$dfTestTEMP)[,-1],
                               lambda = private$lambda.1se,
                               type ="class")


        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', length(predictprob)))
          print('First 10 raw classification probability predictions')
          print(round(predictprob[1:10],2))
        }

        private$ROC = roc(ytest~predictprob)
        private$AUC = auc(private$ROC)

        #Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('AUC: ', round(private$AUC, 2)))
          print(paste0('95% CI AUC: (', round(ci(private$AUC)[1],2), ',', round(ci(private$AUC)[3],2), ')'))
        }

      }
      #regression
      else if (self$params$type == 'regression') {

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ', nrow(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }

        #IF predicted.col IS NOT NUMERIC TO BEGIN WITH, THIS IS NOT GOING TO FIX IT.
        ytest = as.numeric(private$dfTestTEMP[[self$params$predictedCol]])

        #Show error measures for Regression
        private$rmse = sqrt(mean((ytest - private$predictions)^2))
        private$mae= mean(abs(ytest - private$predictions))

        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 2)))
          print(paste0('MAE: ', round(private$mae, 2)))
        }
      }

      if (isTRUE(self$params$printResults)) {
        print("Grouped Lasso coefficients:")
        print(private$fit.grlasso$fit$beta[,private$ind.lambda1se])
      }

      private$stopClustersOnCores()

      if (isTRUE(self$params$varImp)) {
        imp = names(private$dfTrainTEMP[ ,!(colnames(private$dfTrainTEMP) == self$params$predictedCol)])[predict(private$fit.grlasso, private$modMat,type = "groups", lambda = private$lambda.1se)]
        print(paste0("Variables with non-zero coefficients: ",paste0(imp, collapse=", ")))
      }

      return(invisible(private$fit.grlasso))
    },

    #Override: run prediction
    run = function () {

      # Build Model
	    self$buildModel()

      # Perform prediction
      self$performPrediction()

      # Generate performance metrics
      self$generatePerformanceMetrics()
    },

    #get ROC
    getROC = function () {
      if (!IsBinary(self$params$df[[self$params$predictedCol]])) {
        print("ROC is not created because the column you're predicting is not binary")
        return (NULL)
      }

      return (private$ROC)
    },

    #getAUC
    getAUC = function () {
      return(private$AUC)
    },

    #getRMSE
    getRMSE = function () {
      return(private$rmse)
    },

    #getMAE
    getMAE = function () {
      return(private$mae)
    },

    #getPerformanceMetric
    getPerf = function () {
      return(private$perf)
    },

    #getCutOffs
    getCutOffs = function (tpr) {
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
