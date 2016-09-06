# Import the common functions.
source('R/common.R')
source('R/SupervisedModelParameters.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @import caret
#' @importFrom R6 R6Class
#' @param object of SuperviseModelParameters class for $new() constructor
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{HCRTools}}
#'
#' @export

SupervisedModel <- R6Class("SupervisedModel",

  #Private members
  private = list(

    ###########
    # Variables

    dfTrain = NA,
    dfTest = NA,
    prevalence = NA,

    clustersOnCores = NA,

    # Creating attributes for performance report
    memsize_of_dataset = NA,
    initial_dataset_rows = NA,
    initial_dataset_cols = NA,

    ###########
    # Functions

    registerClustersOnCores = function () {
      if (self$params$cores > 1) {
        suppressMessages(library(doParallel))
        private$clustersOnCores <- makeCluster(self$params$cores)
        registerDoParallel(private$clustersOnCores)
      }
    },

    stopClustersOnCores = function () {
      if (self$params$cores > 1) {
        stopCluster(private$clustersOnCores)
        registerDoSEQ()
      }
    },

    #Set config parameters for the algorithm
    setConfigs = function (p) {

      self$params <- SupervisedModelParameters$new()

      if(!is.null(p$df))
        self$params$df <- p$df

      if(!is.null(p$groupCol))
        self$params$groupCol <- p$groupCol

      if(!is.null(p$grainCol))
        self$params$grainCol <- p$grainCol

      if(!is.null(p$predictedCol))
        self$params$predictedCol <- p$predictedCol

      if(!is.null(p$type) && p$type != '') {
        self$params$type <- p$type

        # validation on type string values
        if (self$params$type != 'REGRESSION' && self$params$type != 'CLASSIFICATION') {
          stop('Your type must be regression or classification')
        }
        if (self$params$type =='CLASSIFICATION' && IsBinary(self$params$df[[self$params$predictedCol]]) == FALSE){
          stop('Dependent variable must be binary for classification')
        }
        if (self$params$type =='REGRESSION' && IsBinary(self$params$df[[self$params$predictedCol]]) == TRUE){
          stop('Dependent variable cannot be binary for regression')
        }
      }

      if(!is.null(p$impute))
        self$params$impute <- p$impute

      if(!is.null(p$debug))
        self$params$debug <- p$debug

      if(!is.null(p$varImp))
        self$params$varImp <- p$varImp

      if(!is.null(p$printResults))
        self$params$printResults <- p$printResults

      if(!is.null(p$cores))
        self$params$cores <- p$cores

      #Set additional settings
      if (isTRUE(self$params$debug)) {
        print('Training data set immediately before training')
        print(str(private$dfTrain))
      }

      #Set up clusters on cores
      private$registerClustersOnCores();
    },

    #Load data
    loadData = function () {

      # init dataset variables
      private$memsize_of_dataset = format(object.size(self$params$df), units = "Mb")
      private$initial_dataset_rows = nrow(self$params$df)
      private$initial_dataset_cols = ncol(self$params$df)

      # For use in confusion matrices
      private$prevalence = table(self$params$df[[self$params$predictedCol]])[2]

      if (length(ReturnColsWithMoreThanFiftyCategories(self$params$df))>0){
        message('The following columns in the data frame have more than fifty factors:')
        message(paste(shQuote(ReturnColsWithMoreThanFiftyCategories(self$params$df)), collapse=", "))
        message(paste('This drastically reduces performance.',
                      'Consider combining these factors into a new column with fewer factors.'))
      }

      if (isTRUE(self$params$debug)) {
        print('Entire data set at the top of the constructor')
        print(str(self$params$df))
      }

      if (isTRUE(self$params$debug)) {
        print('Entire df after removing cols with DTS')
        print(str(self$params$df))
        print('Now going to remove zero-var cols...')
      }

      # Remove columns with zero variance
      self$params$df <- RemoveColsWithAllSameValue(self$params$df)

      if (isTRUE(self$params$debug)) {
        print('Entire df after removing feature cols w/zero var')
        print(str(self$params$df))
      }

      # Convert to data.frame (in case of data.table)
      # This also converts chr cols to (needed) factors
      self$params$df <- as.data.frame(unclass(self$params$df))

      if (isTRUE(self$params$debug)) {
        print('Entire data set after converting to df and chr to factor')
        print(str(self$params$df))
      }

      if (isTRUE(self$params$impute)) {
        self$params$df[] <- lapply(self$params$df, ImputeColumn)

        if (isTRUE(self$params$debug)) {
          print('Entire data set after imputation')
          print(str(self$params$df))
        }

      } else {
        if (isTRUE(self$params$debug)) {
          print(paste0("Rows in data set before removing rows with NA's: ", nrow(self$params$df)))
        }

        # Remove rows with any NA's
        self$params$df = na.omit(self$params$df)

        if (isTRUE(self$params$debug)) {
          print(paste0("Rows in data set after removing rows with NA's: ", nrow(self$params$df)))
          print("Entire data set after removing rows with NA's")
          print(str(self$params$df))
        }
      }

      # Remove columns that are only NA
      self$params$df <- self$params$df[,colSums(is.na(self$params$df)) < nrow(self$params$df)]

      # Remove date columns
      datelist = grep("DTS$", colnames(self$params$df))
      if (length(datelist) > 0) {
        self$params$df = self$params$df[, -datelist]
      }

      if (isTRUE(self$params$debug)) {
        print('Entire data set after removing cols with DTS (ie date cols)')
        print(str(self$params$df))
        print('Now going to remove zero-var cols...')
      }

      # If grain.col is specified, remove this col
      if (nchar(self$params$grainCol) != 0) {
        df[[self$params$grainCol]] <- NULL
      }

      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after separating out grain col')
        print(str(self$params$df))
        print('Now splitting training set from validation set')
      }

      #Declare that the predicted col is a factor, or category to be predicted.
      if (self$params$type == 'CLASSIFICATION') {
        self$params$df[[self$params$predictedCol]] = as.factor(self$params$df[[self$params$predictedCol]])
      }

      trainIndex = createDataPartition(y = self$params$df[[self$params$predictedCol]],
                                       p = 0.8,
                                       list = FALSE, times = 1)

      private$dfTrain = self$params$df[ trainIndex,]
      private$dfTest  = self$params$df[-trainIndex,]

      if (isTRUE(self$params$debug)) {
        print('Training data set after splitting from main df')
        print(str(private$dfTrain))
      }

      if (isTRUE(self$params$debug)) {
        print('Validation data set after splitting from main df')
        print(str(private$dfTest))
      }

      # Remove rows where predicted.col is null in train
      private$dfTrain = RemoveRowsWithNAInSpecCol(private$dfTrain, self$params$predictedCol)

      if (isTRUE(self$params$debug)) {
        print('Training data set after removing rows where pred col is null')
        print(str(private$dfTrain))
      }

    }

  ),

  #Public members
  public = list(

    ###########
    # Variables

    #parameters
    params = NA,

    ###########
    # Functions

    #Constructor
    #p: new SuperviseModelParameters class object, i.e. p = SuperviseModelParameters$new()
    initialize = function (p) {

      #Set config parameters
      private$setConfigs(p)

      #Load data
      private$loadData()

    },

    #Build the Model
    buildModel = function () {
    },

    #Run the Model for Prediction
    run = function () {
    }

  )

)
