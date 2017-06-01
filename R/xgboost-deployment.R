#' Deploy a production-ready predictive XGBoost model
#'
#' @description This step allows one to
#' \itemize{
#' \item Automatically load a saved model from \code{\link{XGBoostDevelopment}}
#' \item Run the model against test data to generate predictions
#' \item Push these predictions to SQL Server or CSV
#' }
#' @docType class
#' @usage XGBoostDeployment(type, df, grainCol, testWindowCol, 
#' predictedCol, impute, debug)
#' @import caret
#' @import doParallel
#' @import xgboost
#' @importFrom dplyr mutate
#' @import magrittr
#' @importFrom R6 R6Class
#' @param type The type of model (must be multiclass)
#' @param df Dataframe whose columns are used for new predictions
#' @param grainCol The dataframe's column that has IDs pertaining to the grain
#' @param testWindowCol (Depreciated) Predictions will be made for all rows.
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
#' #### Example using csv dataset ####
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' # 1. Load data. Categorical columns should be characters.
#' csvfile <- system.file("extdata", 
#'                        "dermatology_multiclass_data.csv", 
#'                        package = "healthcareai")
#' 
#' # Replace csvfile with 'path/file'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                stringsAsFactors = FALSE,
#'                na.strings = c("NULL", "NA", "", "?"))
#' 
#' str(df) # check the types of columns
#' dfDevelop <- df[1:346,] # use most of data to train and evalute the model.
#' dfDeploy <- df[347:366,] # reserve 20 rows for deploy step.
#' 
#' # 2. Develop and save model (saving is automatic)
#' set.seed(42)
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- dfDevelop
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
#'                      "eval_metric" = "mlogloss",
#'                      "max_depth" = 6, # max depth of each learner
#'                      "eta" = 0.1, # learning rate
#'                      "silent" = 0, # verbose output when set to 1
#'                      "nthread" = 2) # number of processors to use
#' 
#' # Run model
#' boost <- XGBoostDevelopment$new(p)
#' boost$run()
#' 
#' ## 3. Load saved model (automatic) and use DEPLOY to generate predictions. 
#' p2 <- SupervisedModelDeploymentParams$new()
#' p2$type <- "multiclass"
#' p2$df <- dfDeploy
#' p2$grainCol <- "PatientID"
#' p2$predictedCol <- "target"
#' p2$impute <- TRUE
#' p2$debug <- FALSE
#' 
#' # Deploy model to make new predictions
#' boostD <- XGBoostDeployment$new(p2)
#' boostD$deploy()
#' 
#' # Get output dataframe for csv or SQL
#' outDf <- boostD$getOutDf()
#' head(outDf)
#' 
#' # Write to CSV (or JSON, MySQL, etc) using plain R syntax
#' # write.csv(df,'path/predictionsfile.csv')
#' 
#' # Get raw predictions if you want
#' # rawPredictions <- boostD$getPredictions()
#' 
#' print(proc.time() - ptm)
#' 

 XGBoostDeployment <- R6Class("XGBoostDeployment",
  #Inheritance
  inherit = SupervisedModelDeployment,

  #Private members
  private = list(

    # variables
    outDf = NA,
    test_label = NA,
    
    fitXGB = NA,
    predictions = NA,
    temp_predictions = NA,
    orderedProbs = NA,

    # functions
    # Prepare data for XGBoost
    xgbPrepareData = function() {
      cat('Preparing data...', '\n')
      # XGB requires data.matrix format, not data.frame.
      # R factors are 1 indexed, XGB is 0 indexed, so we must subtract 1 from the labels. They must be numeric.
      temp_test_data <- data.matrix(private$dfTestTemp[ ,!(colnames(private$dfTestTemp) == self$params$predictedCol)])
      temp_test_label <- data.matrix(as.numeric(private$dfTestTemp[[self$params$predictedCol]])) - 1 
      self$xgb_testMatrix <- xgb.DMatrix(data = temp_test_data, label = temp_test_label) 
      private$test_label <- temp_test_label # save for confusion matrix and output
      rm(temp_test_data, temp_test_label) # clean temp variables
    },

    # Perform prediction
    performPrediction = function() {
      cat('Generating Predictions...','\n')
      private$temp_predictions <- predict(object = private$fitXGB,
                                  newdata = self$xgb_testMatrix,
                                  reshape = TRUE)
      
      # Build prediction output
      private$predictions <- private$temp_predictions %>% 
        data.frame() %>%
        mutate(predicted_label = max.col(.),
               true_label = private$test_label + 1)

      # Set column names to match input targets
      colnames(private$predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames
      colnames(private$temp_predictions)[1:self$params$xgb_numberOfClasses] <- self$params$xgb_targetNames

      # Set up a mapping for the values themselves
      from <- 1:self$params$xgb_numberOfClasses
      to <- self$params$xgb_targetNames
      map = setNames(to,from)
      private$predictions$predicted_label <- map[private$predictions$predicted_label] # note square brackets
      private$predictions$true_label <- map[private$predictions$true_label] 

      # Prepare output 
      private$predictions <- cbind(private$grainTest, private$predictions)
      colnames(private$predictions)[1] <- self$params$grainCol
      
      if (isTRUE(self$params$debug)) {
        cat('Rows in multiclass prediction: ', length(private$predictions), '\n')
        cat('First 10 raw probability predictions (with row # first)', '\n')
        print(private$predictions[1:10,])
      }
    },

    calculateOrderedFactors = function() {
      cat('Ordering top probabilities...', '\n')
      # Calculate ordered factors of importance for each row's prediction
      # Get column indices of max values in each row
      nRows = dim(private$temp_predictions)[1]
      maxColInds <- t(sapply(1:nRows, function(i) 
        order(private$temp_predictions[i,], decreasing=TRUE)))
      
      # sort column values in each row
      maxColVals <- t(sapply(1:nRows, function(i) 
        private$temp_predictions[i,maxColInds[i,]]))

      # get names of maximum values for each row
      maxColNames <- t(sapply(1:nRows, 
        function(i) colnames(private$temp_predictions)[maxColInds[i,]]))

      # combine top 3 maxColVals and maxColNames to make maxProbDF
      private$orderedProbs <- cbind(maxColVals[,1], maxColNames[,1],
         maxColVals[,2], maxColNames[,2],
         maxColVals[,3], maxColNames[,3])

      # update column names
      colnames(private$orderedProbs) <- c('PredictedProb1','PredictedClass1',
        'PredictedProb2','PredictedClass2',
        'PredictedProb3','PredictedClass3')

      # update row names
      row.names(private$orderedProbs) <- 1:nRows

      if (isTRUE(self$params$debug)) {
        cat('Data frame after getting column importance ordered', '\n')
        print(private$orderedProbs[1:10, ])
      }
    },

    createDf = function() {
      cat('Creating ouput dataframe', '\n')
      dtStamp <- as.POSIXlt(Sys.time())

      # Combine grain.col, prediction, and time to be put back into SAM table
      private$outDf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtStamp,                           # LastLoadDTS
        private$grainTest,                 # GrainID
        private$orderedProbs)              # Top 3 Factors

      predictedResultsName = ""
      if (self$params$type == 'classification') {
        predictedResultsName = "PredictedProbNBR"
      } else if (self$params$type == 'regression') {
        predictedResultsName = "PredictedValueNBR"
      }
      colnames(private$outDf)[1:4] <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol
      )

      if (isTRUE(self$params$debug)) {
        cat('Dataframe with predictions:', '\n')
        cat(str(private$outDf), '\n')
      }
    }
  ),

  #Public members
  public = list(
    xgb_testMatrix = NA,
    #Constructor
    #p: new SupervisedModelDeploymentParams class object,
    #   i.e. p = SupervisedModelDeploymentParams$new()
    initialize = function(p) {
      cat('Initializing XGBoost Deploy...','\n')
      super$initialize(p)
    },

    #Override: deploy the model
    deploy = function() {
      cat('Loading XGB Model...','\n')

      # Try to load the model
      tryCatch({
        load("rmodel_probability_XGB.rda") # Produces fit object (for probability)
        private$fitXGB <- fitObj
       }, error = function(e) {
        # temporary fix until all models are working.
        stop('You must use a saved model. Run XGBoost development to train 
              and save the model, then XGBoost deployment to make predictions.
              See ?XGBoostDevelopment')
      })
      
      # Prepare data for xgboost
      private$xgbPrepareData()

      # Predict
      private$performPrediction()

      # Calculate Ordered Factors
      private$calculateOrderedFactors()

      # create dataframe for output
      private$createDf()
    },
    
    # Surface outDf as attribute for export to Oracle, MySQL, etc
    getOutDf = function() {
      return(private$outDf)
    },
    # surface raw predictions for inspection
    getPredictions = function() {
      return(private$predictions)
    }
  )
)
