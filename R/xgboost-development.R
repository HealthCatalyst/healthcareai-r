#' Compare predictive models, created on your data
#'
#' @description This step allows you to create an XGBoost classification model, based on
#' your data. Use model type 'multiclass' with 2 or more classes. XGBoost is an ensemble model,
#' well suited to non-linear data and very fast. Can be parameter-dependent. 
#' @docType class
#' @usage XGBoostDevelopment(type, df, grainCol, predictedCol, 
#' impute, debug, cores, modelName, xgb_params, xgb_nrounds)
#' @import caret
#' @import doParallel
#' @import e1071
#' @import xgboost
#' @importFrom R6 R6Class
#' @param type The type of model. Currently requires 'multiclass'.
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
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
#' @param xgb_params A list, containing optional xgboost parameters. The full list of params can be found at
#' \url{http://xgboost.readthedocs.io/en/latest/parameter.html}. 
#' @param xgb_nrounds Number of rounds to use for boosting.
#' @section Methods: 
#' The above describes params for initializing a new XGBoostDevelopment class with 
#' \code{$new()}. Individual methods are documented below.
#' @section \code{$new()}:
#' Initializes a new XGBoost development class using the 
#' parameters saved in \code{p}, documented above. This method loads, cleans, and prepares data for
#' model training. \cr
#' \emph{Usage:} \code{$new(p)}
#' @section \code{$run()}:
#' Trains model, displays predictions and class-wise performance. \cr
#' \emph{Usage:} \code{$new()} 
#' @section \code{$getPredictions()}:
#' Returns the predictions from test data. \cr
#' \emph{Usage:} \code{$getPredictions()} \cr
#' @section \code{$generateConfusionMatrix()}:
#' Returns the confusion matrix and statistics generated during model development. \cr
#' \emph{Usage:} \code{$getConfusionMatrix()} \cr
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso Information on the example dataset can be found at: 
#' \url{http://archive.ics.uci.edu/ml/datasets/dermatology/}
#' @seealso Information on the xgboost parameters can be found at:
#' \url{https://github.com/dmlc/xgboost/blob/master/doc/parameter.md}
#' @seealso \code{\link{selectData}}
#' 
#' @examples
#'
#' #### Example using csv dataset ####
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # 1. Load data. Categorical columns should be characters.
#' csvfile <- system.file("extdata", 
#'                       "dermatology_multiclass_data.csv", 
#'                       package = "healthcareai")
#' 
#'  # Replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'               header = TRUE, 
#'               stringsAsFactors = FALSE,
#'               na.strings = c("NULL", "NA", "", "?"))
#' 
#' str(df) # check the types of columns
#' 
#' # 2. Develop and save model
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$type <- "multiclass"
#' p$impute <- TRUE
#' p$grainCol <- "PatientID"
#' p$predictedCol <- "target"
#' p$debug <- FALSE
#' p$cores <- 1
#' # xgb_params must be a list with all of these things in it. 
#' # if you would like to tweak parameters, go for it! 
#' # Leave objective and eval_metric as they are.
#' p$xgb_params <- list("objective" = "multi:softprob",
#'                   "eval_metric" = "mlogloss",
#'                   "max_depth" = 6, # max depth of each learner
#'                   "eta" = 0.1, # learning rate
#'                   "silent" = 0, # verbose output when set to 1
#'                   "nthread" = 2) # number of processors to use
#' 
#' # Run model
#' boost <- XGBoostDevelopment$new(p)
#' boost$run()
#' 
#' # Get output data 
#' outputDF <- boost$getPredictions()
#' head(outputDF)
#'
#' print(proc.time() - ptm)
#'
#' @export

XGBoostDevelopment <- R6Class("XGBoostDevelopment",

  # Inheritance
  inherit = SupervisedModelDevelopment,

  # Private members
  private = list(

    # Grid object for grid search (TODO: add random parameter search)
    grid = NA,
    predictions = NA,
    test_label = NA,
    
    algorithmShortName = "XGB",

    # Performance metrics
    ROCPlot = NA,
    PRCurvePlot = NA,
    AUROC = NA,
    AUPR = NA,
    RMSE = NA,
    MAE = NA,

    # Start of functions
    # Prepare data for XGBoost
    xgbPrepareData = function() {
      cat('Preparing data...', '\n')
      # XGB requires data.matrix format, not data.frame.
      # R factors are 1 indexed, XGB is 0 indexed, so we must subtract 1 from the labels. They must be numeric.
      temp_train_data <- private$dfTrain[ ,!(colnames(private$dfTrain) == self$params$predictedCol)]
      temp_train_data[] <- lapply(temp_train_data, as.numeric)
      temp_train_label <- as.numeric(private$dfTrain[[self$params$predictedCol]]) - 1 
      self$xgb_trainMatrix <- xgb.DMatrix(data = data.matrix(temp_train_data), 
                                          label = data.matrix(temp_train_label))
      rm(temp_train_data, temp_train_label) # clean temp variables

      temp_test_data <- private$dfTest[ ,!(colnames(private$dfTest) == self$params$predictedCol)]
      temp_test_data[] <- lapply(temp_test_data, as.numeric)
      temp_test_label <- as.numeric(private$dfTest[[self$params$predictedCol]]) - 1 
      self$xgb_testMatrix <- xgb.DMatrix(data = data.matrix(temp_test_data), 
                                         label = data.matrix(temp_test_label)) 
      private$test_label <- temp_test_label # save for confusion matrix and output
      rm(temp_test_data, temp_test_label) # clean temp variables
    },

    buildModel = function() {
      cat('Building model...', '\n')
      self$fitXGB <- xgb.train(params = self$params$xgb_params,
                             data = self$xgb_trainMatrix,
                             nrounds = self$params$xgb_nrounds)
      # Save target list into the fit object for use in deploy
      # coerce the target names to characters to avoid factor subsetting issues
      self$fitXGB$xgb_targetNames <- as.character(self$params$xgb_targetNames)
    },

    # Perform prediction
    performPrediction = function() {
      cat('Generating predictions...', '\n')
      temp_predictions <- predict(self$fitXGB, newdata = self$xgb_testMatrix, reshape = TRUE)

      # Build prediction output
      private$predictions <- as.data.frame(temp_predictions)
      # Pull the maximum probability for a given row.
      private$predictions$predicted_label = max.col(private$predictions)
      # XGBoost internally uses 0-indexed factors. Add 1 to match R's 1-indexed factors.
      private$predictions$true_label = private$test_label + 1

      # Set column names to match input targets
      colnames(private$predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames

      # Set up a mapping for the values themselves
      from <- 1:self$params$xgb_numberOfClasses
      to <- self$params$xgb_targetNames
      map = setNames(to,from)
      private$predictions$predicted_label <- map[private$predictions$predicted_label] # note square brackets
      private$predictions$true_label <- map[private$predictions$true_label] 

      # Prepare output 
      private$predictions <- cbind(private$grainTest, private$predictions)
      colnames(private$predictions)[1] <- self$params$grainCol
    }

    # TODO: Cross validation and random search
  ),

  # Public members
  public = list(
    # xgboost specific placeholders
    xgb_trainMatrix = NA,
    xgb_testMatrix = NA,
    
    # Get xgb model
    fitXGB = NA,

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      # check that it's a multiclass type
      if (p$type != 'multiclass') {
        cat('XGBoost currently only supports "multiclass" type.', '\n')
      }

      set.seed(43)
      super$initialize(p)

      # TODO set up tuning to actually work.
      if (!is.null(p$tune)) {
        self$params$tune = p$tune
      }

      # print xgb params (for sanity)
      cat('xgb_params for model training are:', '\n')
      cat(str(self$params$xgb_params), '\n')
    
    },

    getPredictions = function(){
      cat('Retrieving raw predictions...', '\n')
      return(private$predictions)
    },

    # Generate performance metrics
    generateConfusionMatrix = function() {
      cat('Generating confusion matrix...', '\n')
      u = union(private$predictions$true_label, 
                private$predictions$predicted_label)
      true_labels <- factor(private$predictions$true_label, u)
      predicted_labels <- factor(private$predictions$predicted_label, u)
      print(caret::confusionMatrix(predicted_labels, true_labels, 
                            dnn = c("Predicted","True")))
    },

   # Run XGBoost Multiclass
   run = function() {
      # Prepare data for xgboost
      private$xgbPrepareData()

      # Build Model
      private$buildModel()
      
      # save model
      super$saveModel(fitModel = self$fitXGB)

      # Perform prediction
      private$performPrediction()

      # Generate confusion matrix
      self$generateConfusionMatrix()
    }
  )
)
