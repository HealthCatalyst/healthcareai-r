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
#' @import lme4
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @import RODBC
#' @export

LinearMixedModel <- R6Class("LinearMixedModel",


  # Inheritance
  inherit = SupervisedModel,

  # Private members
  private = list(

    # Mixed model-specific datasets
    train_test = NA,
    lmm_train = NA,
    lmm_test = NA,

    # Git random forest model
    fit.lmm = NA,

    predictions = NA,

    # Performance metrics
    confMatrix = NA,
    ROC = NA,
    AUC = NA,
    rmse = NA,
    mae = NA,
    perf = NA,
    prevalence = NA
  ),

  # Public members
  public = list(

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)
    },

    # Start of functions
    buildDataset = function(){
      # Prepare data according to InTestWindow column
      # 1) Combine test/train (which was randomly generated in base class)
      print('Create combined test/train')
      private$train_test <- rbind(private$dfTrain,private$dfTest)
      print(str(private$train_test))

      # 2) Order by PatientID, PatientEncounterID
      print('Before ordering')
      print('Heellllllooooo')
      print('PredictedCol')
      print(self$params$predictedCol)
      print('Heellllllooooo2')
      print('PersonalCol')
      print(self$params$personCol)
      print('GrainCol')
      print(self$params$grainCol)

      #dd[with(dd, order(-z, b)), ]
      private$train_test <- private$train_test[with(private$train_test,
                                                    order(private$train_test[[self$params$grainCol]])), ]
      print('Ordered by PatID, PatEncID')
      print(str(private$train_test))

      # 3) Split out test/train using logic from deploy (ie with )
      private$lmm_test <- setDT(private$train_test)[, .SD[.N], by = eval(self$params$personCol)]
      private$lmm_train <- setDT(private$train_test)[, .SD[1:.N-1], by = eval(self$params$personCol)]
      print('Splitting for mixed test/train')
      print(str(private$lmm_test))
      print(str(private$lmm_train))
    },

    # Override: build model
    # Linear Mixed model (random intercept with fixed mean)
    buildModel = function() {

      # Build formula
      col_list <- colnames(private$lmm_train)

      # Remove target col from list
      col_list <- col_list[col_list != self$params$predictedCol]

      # Remove grain col from list
      col_list <- col_list[col_list != self$params$grainCol]

      # Remove random-effects col from list
      fixed_cols_temp <- col_list[col_list != self$params$personCol]
      print(fixed_cols_temp)

      fixed_cols <- paste(fixed_cols_temp, "+ ", collapse = "")
      print(fixed_cols)

      formula <- paste0(self$params$predictedCol, " ~ ",
                        fixed_cols,
                        "(1|", self$params$personCol, ")")

      print(formula)

      print(str(train))

      # Train RandomForest
      if (self$params$type == 'classification') {
        private$fit.lmm = glmer(formula = formula,
                                data = private$lmm_train,
                                family = binomial(link='logit'))
      }
      else if (self$params$type == 'regression') {
        private$fit.lmm = glmer(formula = formula,
                                data = private$lmm_train)
      }
    },

    # Perform prediction
    performPrediction = function() {

      if (self$params$type == 'classification') {
        private$predictions <- predict(object = private$fit.lmm,
                                       newdata = private$lmm_test,
                                       allow.new.levels = TRUE,
                                       type = "response")
      }
      else if (self$params$type == 'regression') {
        private$predictions <- predict(object = private$fit.lmm,
                                       newdata = private$lmm_test,
                                       allow.new.levels = TRUE)
      }
    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {

      if (self$params$type == 'classification') {
        predictprob <- private$predictions

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in probability prediction: ', nrow(predictprob)))
          print('First 10 raw classification probability predictions')
          print(round(predictprob[1:10],2))
        }

        ytest = as.numeric(private$lmm_test[[self$params$predictedCol]])
        pred <- prediction(predictprob, ytest)
        private$perf <- ROCR::performance(pred, "tpr", "fpr")

        private$ROC = pROC::roc(ytest~predictprob)
        private$AUC = pROC::auc(private$ROC)

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('AUC: ', round(private$AUC, 2)))
          print(paste0('95% CI AUC: (', round(ci(private$AUC)[1],2),
                       ',',
                       round(ci(private$AUC)[3],2), ')'))
        }
      }

      # Regression
      else if (self$params$type == 'regression') {

        if (isTRUE(self$params$debug)) {
          print(paste0('Rows in regression prediction: ',
                       length(private$predictions)))
          print('First 10 raw regression predictions (with row # first)')
          print(round(private$predictions[1:10],2))
        }

        ytest = as.numeric(private$lmm_test[[self$params$predictedCol]])

        # Error measures
        private$rmse = sqrt(mean((ytest - private$predictions) ^ 2))
        private$mae = mean(abs(ytest - private$predictions))

        # Show results
        if (isTRUE(self$params$printResults)) {
          print(paste0('RMSE: ', round(private$rmse, 8)))
          print(paste0('MAE: ', round(private$mae, 8)))
        }
      }

      private$stopClustersOnCores()

      return(invisible(private$fit.lmm))
    },

    # Override: run RandomForest algorithm
    run = function() {

      self$buildDataset()

      # Build Model
      self$buildModel()

      # Perform prediction
      self$performPrediction()

      # Generate performance metrics
      self$generatePerformanceMetrics()
    },

    getROC = function() {
      if (!IsBinary(self$params$df[[self$params$predictedCol]])) {
        print("ROC is not created because the column you're predicting is not binary")
        return(NULL)
      }

      return(private$ROC)
    },

    getAUC = function() {
      return(private$AUC)
    },

    getRMSE = function() {
      return(private$rmse)
    },

    getMAE = function() {
      return(private$mae)
    },

    getPerf = function() {
      return(private$perf)
    },

    getCutOffs = function(tpr) {
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
