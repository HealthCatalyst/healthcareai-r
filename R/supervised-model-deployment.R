
#' Deploy predictive models, created on your data
#'
#' @description This step allows one to create deploy models on your data
#' and helps determine which performs best.
#' @docType class
#' @usage SupervisedModelDeployment(object)
#' @import caret
#' @importFrom R6 R6Class
#' @param object of SupervisedModelDeploymentParams class for $new() constructor
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#'
#' @export

SupervisedModelDeployment <- R6Class("SupervisedModelDeployment",

 #Private members
 private = list(
  
  ###########
  # Variables

  dfTest = NULL,
  dfTestRaw = NULL,
  dfTrain = NULL,
  dfTemp = NULL,
  dfTestTemp = NULL,

  grainTest = NULL,
  dfGrain = NULL,

  fit = NA,
  predictedVals = NA,

  clustersOnCores = NA,

  ###########
  # Functions
  registerClustersOnCores = function() {
  if (self$params$cores > 1) {
    suppressMessages(library(doParallel))
    private$clustersOnCores <- makeCluster(self$params$cores)
    registerDoParallel(private$clustersOnCores)
    }
  },

  stopClustersOnCores = function() {
  if (self$params$cores > 1) {
    stopCluster(private$clustersOnCores)
    registerDoSEQ()
    }
  },

  setConfigs = function(p) {
  self$params <- SupervisedModelDeploymentParams$new()

  if (!is.null(p$df))
  self$params$df <- p$df

  if (!is.null(p$grainCol))
  self$params$grainCol <- p$grainCol

  if (!is.null(p$predictedCol))
  self$params$predictedCol <- p$predictedCol

  if (!is.null(p$personCol))
  self$params$personCol <- p$personCol

  if (!is.null(p$groupCol))
  self$params$groupCol <- p$groupCol

  if (!is.null(p$type) && p$type != '') {
  self$params$type <- p$type

    # validation on type string values
    if (self$params$type != 'regression' &&
       self$params$type != 'classification' &&
       self$params$type != 'multiclass') {
     stop('Your type must be regression, classification, or multiclass')
    }
  }

  if (!is.null(p$impute))
  self$params$impute <- p$impute

  if (!is.null(p$debug))
  self$params$debug <- p$debug

  # for deploy method
  if (!is.null(p$cores))
  self$params$cores <- p$cores
  },

  loadData = function() {
    cat('Loading Data...','\n')

    if (isTRUE(self$params$debug)) {
      print('Entire data set at the top of the constructor')
      print(str(self$params$df))
      print('Now going to convert chr cols to factor cols...')
    }

    # This also converts chr cols to (needed) factors
    self$params$df <- as.data.frame(unclass(self$params$df))

    # Remove columns that are only NA
    self$params$df <- self$params$df[, colSums(is.na(self$params$df)) < nrow(self$params$df)]

    # self$params$df[[self$params$predictedCol]] <- temp

    # Remove date columns
    dateList <- grep("DTS$", colnames(self$params$df))
    if (length(dateList) > 0) {
      self$params$df <- self$params$df[, -dateList]
    }

    if (isTRUE(self$params$debug)) {
      print('Entire df after removing cols with DTS')
      print(str(self$params$df))
      print('Now going to check for cols with fifty+ categories...')
    }

    if (length(returnColsWithMoreThanFiftyCategories(self$params$df)) > 0) {
      warning('These columns in the df have more than fifty categories: \n',
      paste(
      shQuote(returnColsWithMoreThanFiftyCategories(self$params$df)), 
      collapse = ", "),
      '\n This drastically reduces performance. \n',
      'Consider combining into new col with fewer categories.')
    }

    if (isTRUE(self$params$debug)) {
      print('Entire df after removing feature cols with all same value')
      print(str(self$params$df))
      print('Now separating grain column')
    }

    # Remove grain.col from df; below we split it into graintest
    if (nchar(self$params$grainCol) != 0) {
      fullGrain <- self$params$df[[self$params$grainCol]]
      self$params$df[[self$params$grainCol]] <- NULL
      } else {
      stop('You must specify a GrainID column when using DeploySupervisedModel')
    }

    if (isTRUE(self$params$debug)) {
      print('Entire data set after separating out grain col')
      print(str(self$params$df))
      print('Now starting imputation, or removing rows with NULLs')
    }

    # Remove predicted column if it exists
    if ((nchar(self$params$predictedCol) != 0) & (self$params$type != 'multiclass')) {
      self$params$df[[self$params$predictedCol]] <- NULL
    }

    if (isTRUE(self$params$debug)) {
      print('Entire data set after separating removing predicted column')
      print(str(self$params$df))
      print('Now starting imputation, or removing rows with NULLs')
    }

    # Impute columns
    # TODO: impute using training data (currently uses deploy data)
    self$params$df[] <- lapply(self$params$df, imputeColumn)

    if (isTRUE(self$params$debug)) {
      print('Entire data set after imputation')
      print(str(self$params$df))
    }

    # Now that we have train/test, split grain col into test (for use at end)
    if (nchar(self$params$grainCol) != 0) {
      private$grainTest <- fullGrain

      if (isTRUE(self$params$debug)) {
        print('Final prepared test set. Grain column shown for debugging.')
        temp <- cbind(private$grainTest[1:10], self$params$df[1:10,])
        colnames(temp)[1] <- self$params$grainCol
        print(temp)
        rm(temp)
      }
    } else {
      stop('You must specify a GrainID column when using DeploySupervisedModel')
    }

    # For LMM, remove ID col so it doesn't interfere with row-based varimp calc
    if (nchar(self$params$personCol) != 0) {
      private$dfTestRaw[[self$params$personCol]] <- NULL
    }
  },
  
  formatFactorColumns = function(){
    # Manually Assign factor levels based on which ones were present in training.
    private$dfTestRaw <- self$params$df
    factorLevels <- self$modelInfo$factorLevels
    
    # Check to see if there are new levels in test data vs. training data.
    # Save new levels and set values to NA.
    newLevels <- list()
    for (col in names(self$modelInfo$factorLevels)) {
      # find new levels not seen in training data
      testLevels <- levels(private$dfTestRaw[[col]])
      newLevelValues <- testLevels[!testLevels %in% factorLevels[[col]]]
      if (length(newLevelValues) > 0) {
        newLevels[[col]] <- newLevelValues
      }
      # Set new levels to NA
      private$dfTestRaw[[col]][!(private$dfTestRaw[[col]] %in% factorLevels[[col]])] <- NA
    }
    
    # Display warning if new categorical variable levels are found
    if (length(newLevels) > 0) {
      warning('New categorical variable levels were found:\n',
              paste(' - ', names(newLevels), ":", newLevels, collapse = "\n"),
              '\nThese values have been set to NA.', sep = "")
    }
    
    # Impute missing values introduced through new factor levels
    # TODO: impute using training data (currently uses deploy data)
    private$dfTestRaw[, names(newLevels)] <- sapply(private$dfTestRaw[, names(newLevels)], imputeColumn)
    
    # Assign new factor levels using training data factor levels
    for (col in names(self$modelInfo$factorLevels)) {
      private$dfTestRaw[[col]] <- factor(private$dfTestRaw[[col]],
                                         levels = factorLevels[[col]],
                                         ordered = FALSE)
    }
    
    if (isTRUE(self$params$debug)) {
      print('Raw data set after setting factors:')
      print(str(private$dfTestRaw))
    }
  },

  makeFactorDummies = function(){
    # Split factor columns into dummy columns (for use in deploy top factors method)
    data <- dummyVars(~., data = private$dfTestRaw, fullRank = T)
    private$dfTestRaw <- data.frame(predict(data, newdata = private$dfTestRaw, na.action = na.pass))

    if (isTRUE(self$params$debug)) {
      print('Raw data set after creating dummy vars (for top 3 factors only)')
      print(str(private$dfTestRaw))
    }
  }
),

  #Public members
  public = list(
    ###########
    # Variables

    #parameters
    params = NA,
    modelInfo = NA,

    ###########
    # Functions

    #Constructor
    #p: new SupervisedModelDeploymentParams class object,
    #   i.e. p = SupervisedModelDeploymentParams$new()
    initialize = function(p) {
      #Set config parameters
      private$setConfigs(p)

      #Load data
      private$loadData()
    },

    #Deploy the Model
    deploy = function() {
    }
  )
)
