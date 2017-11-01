#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a linear mixed model on your data. LMM is 
#' best suited to longitudinal data that has multiple entries for a each patient or
#' physician. It will fit a slightly different linear model to each patient.
#' This algorithm works best with linearly separable data sets. As data sets
#' become longer than 100k rows or wider than 50 features, performance will suffer.
#' @docType class
#' @usage LinearMixedModelDevelopment(type, df, 
#' grainCol, personCol, predictedCol, impute, debug, cores, modelName)
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import lme4
#' @import pROC
#' @importFrom R6 R6Class
#' @import ROCR
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The data frame's ID column pertaining to the grain
#' @param personCol The data frame's ID column pertaining to the person/patient
#' @param predictedCol Column that you want to predict. If you're doing
#' classification then this should be Y/N.
#' @param impute Set all-column imputation to T or F.
#' If T, this uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' Values are saved for deployment.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @param cores Number of cores you'd like to use. Defaults to 2.
#' @param modelName Optional string. Can specify the model name. If used, you must load the same one in the deploy step.
#' @section Methods: 
#' The above describes params for initializing a new linearMixedModelDevelopment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new linear mixed model development class using the 
#' parameters saved in \code{p}, documented above. This method loads, cleans, and prepares data for
#' model training. \cr
#' \emph{Usage:} \code{$new(p)}
#' @section \code{$run()}:
#' Trains model, displays feature importance and performance. \cr
#' \emph{Usage:}\code{$new()} 
#' @section \code{$getPredictions()}:
#' Returns the predictions from test data. \cr
#' \emph{Usage:} \code{$getPredictions()} \cr
#' @section \code{$getROC()}:
#' Returns the ROC curve object for \code{\link{plotROCs}}. Classification models only. \cr
#' \emph{Usage:} \code{$getROC()} \cr
#' @section \code{$getPRCurve()}:
#' Returns the PR curve object for \code{\link{plotPRCurve}}. Classification models only. \cr
#' \emph{Usage:} \code{$getROC()} \cr
#' @section \code{$getAUROC()}:
#' Returns the area under the ROC curve from testing for classification models. \cr
#' \emph{Usage:} \code{$getAUROC()} \cr
#' @section \code{$getRMSE()}:
#' Returns the RMSE from test data for regression models. \cr
#' \emph{Usage:} \code{$getRMSE()} \cr
#' @section \code{$getMAE()}:
#' Returns the RMSE from test data for regression models. \cr
#' \emph{Usage:} \code{$getMAE()} \cr
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{selectData}}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'
#' ### Built-in example; Doing classification
#' library(healthcareai)
#' library(lme4)
#'
#' df <- sleepstudy
#'
#' str(df)
#'
#' # Create binary column for classification
#' df$ReactionFLG <- ifelse(df$Reaction > 300, "Y", "N")
#' df$Reaction <- NULL
#'
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "classification"
#' p$impute <- TRUE
#' p$personCol <- "Subject"  # Think of this as PatientID
#' p$predictedCol <- "ReactionFLG"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' ### Doing regression
#' library(healthcareai)
#'
#' # SQL query and connection goes here - see SelectData function.
#'
#' df <- sleepstudy
#'
#' # Add GrainID, which is equivalent to PatientEncounterID
#' df$GrainID <- seq.int(nrow(df))
#'
#' str(df)
#'
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "regression"
#' p$impute <- TRUE
#' p$grainCol <- "GrainID"  # Think of this as PatientEnounterID
#' p$personCol <- "Subject"  # Think of this as PatientID
#' p$predictedCol <- "Reaction"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' #### Example using csv data ####
#' library(healthcareai)
#' # setwd('C:/Your/script/location') # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "healthcareai")
#' #Replace csvfile with "path/to/yourfile"
#' df <- read.csv(file = csvfile, header = TRUE, na.strings = c("NULL", "NA", ""))
#'
#' head(df)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "classification"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$personCol <- "PatientID"
#' p$predictedCol <- "ThirtyDayReadmitFLG"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#'
#' set.seed(42) 
#' # Run Lasso
#' # Lasso <- LassoDevelopment$new(p)
#' # Lasso$run()
#' cat(proc.time() - ptm, '\n')
#' 
#' \dontrun{
#' #### This example is specific to Windows and is not tested. 
#' #### Example using SQL Server data ####
#' # This example requires that you alter your connection string / query
#' # to read in your own data
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
#' # This query should pull only rows for training. They must have a label.
#' query <- "
#' SELECT
#'  [PatientEncounterID]
#' ,[PatientID]
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' --no WHERE clause, because we want train AND test
#' "
#'
#' df <- selectData(connection.string, query)
#' head(df)
#'
#' set.seed(42)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "classification"
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$personCol <- "PatientID"
#' p$predictedCol <- "ThirtyDayReadmitFLG"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' # Create Mixed Model
#' lmm <- LinearMixedModelDevelopment$new(p)
#' lmm$run()
#' 
#' # Remove person col, since RF can't use it
#' df$personCol <- NULL
#' p$df <- df
#' p$personCol <- NULL
#' 
#' set.seed(42) 
#' # Run Random Forest
#' rf <- RandomForestDevelopment$new(p)
#' rf$run()
#' 
#' # Plot ROC
#' rocs <- list(lmm$getROC(), rf$getROC())
#' names <- c("Linear Mixed Model", "Random Forest")
#' legendLoc <- "bottomright"
#' plotROCs(rocs, names, legendLoc)
#' 
#' # Plot PR Curve
#' rocs <- list(lmm$getPRCurve(), rf$getPRCurve())
#' names <- c("Linear Mixed Model", "Random Forest")
#' legendLoc <- "bottomleft"
#' plotPRCurve(rocs, names, legendLoc)
#'
#' cat(proc.time() - ptm, '\n')
#' }
#' 
#' @export

LinearMixedModelDevelopment <- R6Class("LinearMixedModelDevelopment",


  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Mixed model-specific datasets
    trainTest = NA,
    lmmTrain = NA,
    lmmTest = NA,

    # Fit LMM
    fitLmm = NA,

    predictions = NA,
    
    algorithmShortName = "LMM",

    # Performance metrics
    ROCPlot = NA,
    PRCurvePlot = NA,
    AUROC = NA,
    AUPR = NA,
    RMSE = NA,
    MAE = NA
    
    # functions
  ),
  
  # Public members
  public = list(

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)
    },
    getPredictions = function(){
      return(private$predictions)
    },
    # Start of functions
    buildDataset = function(){
      # TODO Soon: Prepare data according to InTestWindow column, in case
      # user wants to predict for row that's not the last in the person group
      # Combine test/train (which was randomly generated in base class)

      private$trainTest <- rbind(private$dfTrain,private$dfTest)

      if (isTRUE(self$params$debug)) {
       cat('Re-combined train/test for MM specific use', '\n')
       cat(str(private$trainTest), '\n')
      }

      # TODO Later: figure out why ordering in sql query is better auc than internal
      # ordering. http://stackoverflow.com/a/1296745/5636012
      # If ordering using with, access PersonID col via df[[PersonID]]

      # Split out test/train by taking last row of each PersonID for test set
      # TODO Soon: do this split using the InTestWindowCol
      private$lmmTrain <- data.table::setDT(private$trainTest)[, .SD[1:.N - 1], by = eval(self$params$personCol)]
      private$lmmTest <- data.table::setDT(private$trainTest)[, .SD[.N], by = eval(self$params$personCol)]

      if (isTRUE(self$params$debug)) {
       cat('Mixed model-specific training set after creation', '\n')
       cat(str(private$lmmTrain), '\n')
       cat('Mixed model-specific test set after creation', '\n')
       cat(str(private$lmmTest), '\n')
      }
    },

    # Override: build model
    # Linear Mixed model (random intercept with fixed mean)
    buildModel = function() {

      # Start build formula by grabbing column names
      colList <- colnames(private$lmmTrain)

      # Remove target col from list
      colList <- colList[colList != self$params$predictedCol]

      # Remove grain col from list
      colList <- colList[colList != self$params$grainCol]

      # Remove random-effects col from list
      fixedColsTemp <- colList[colList != self$params$personCol]

      # Collapse columns in list into a large string of cols
      fixedCols <- paste(fixedColsTemp, "+ ", collapse = "")

      formula <- paste0(self$params$predictedCol, " ~ ",
                        fixedCols,
                        "(1|", self$params$personCol, ")")

      if (isTRUE(self$params$debug)) {
        cat('Formula to be used:', '\n')
        cat(formula, '\n')
        cat('Training the general linear mixed-model...', '\n')
        cat('Using random intercept with fixed mean...', '\n')
      }

      if (self$params$type == 'classification') {
        private$fitLmm <- glmer(formula = formula,
                                data = private$lmmTrain,
                                family = binomial(link = 'logit'))
      } else if (self$params$type == 'regression') {
        private$fitLmm <- lmer(formula = formula,
                                data = private$lmmTrain)
      }
    },

    # Perform prediction
    performPrediction = function() {

      if (self$params$type == 'classification') {
        private$predictions <- predict(object = private$fitLmm,
                                       newdata = private$lmmTest,
                                       allow.new.levels = TRUE,
                                       type = "response")
        
        if (isTRUE(self$params$debug)) {
          cat(paste0('Predictions generated: ', nrow(private$predictions)), '\n')
          cat('First 10 raw classification probability predictions', '\n')
          cat(round(private$predictions[1:10],2), '\n')
        }
      }
      else if (self$params$type == 'regression') {
        private$predictions <- predict(object = private$fitLmm,
                                       newdata = private$lmmTest,
                                       allow.new.levels = TRUE)
        
        if (isTRUE(self$params$debug)) {
          cat(paste0('Predictions generated: ', '\n',
                       length(private$predictions)))
          cat('First 10 raw regression predictions (with row # first)', '\n')
          cat(round(private$predictions[1:10],2), '\n')
        }
      }
    },

    # Generate performance metrics
    generatePerformanceMetrics = function() {

      ytest <- as.numeric(private$lmmTest[[self$params$predictedCol]])
      
      calcObjList <- calculatePerformance(private$predictions, 
                                           ytest, 
                                           self$params$type)
      
      # Make these objects available for plotting and unit tests
      private$ROCPlot <- calcObjList[[1]]
      private$PRCurvePlot <- calcObjList[[2]]
      private$AUROC <- calcObjList[[3]]
      private$AUPR <- calcObjList[[4]]
      private$RMSE <- calcObjList[[5]]
      private$MAE <- calcObjList[[6]]

      return(invisible(private$fitLmm))
    },

    # Override: run Linear Mixed algorithm
    run = function() {

      self$buildDataset()

      # Build Model
      self$buildModel()
      
      # fit GLM for row-wise variable importance
      super$fitGeneralizedLinearModel()
      
      # save model
      super$saveModel(fitModel = private$fitLmm)

      # Perform prediction
      self$performPrediction()

      # Generate performance metrics
      self$generatePerformanceMetrics()
    },

    getROC = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        cat("ROC is not created because the column you're predicting is not binary", '\n')
        return(NULL)
      }
      return(private$ROCPlot)
    },
    
    getPRCurve = function() {
      if (!isBinary(self$params$df[[self$params$predictedCol]])) {
        cat("PR Curve is not created because the column you're predicting is not binary", '\n')
        return(NULL)
      }
      return(private$PRCurvePlot)
    },
    
    getAUROC = function() {
      return(private$AUROC)
    },

    getRMSE = function() {
      return(private$RMSE)
    },

    getMAE = function() {
      return(private$MAE)
    },
    
    getCutOffs = function() {
      warning("`getCutOffs` is deprecated. Please use `generateAUC` instead. See 
              ?generateAUC", call. = FALSE)
    }
  )
)
