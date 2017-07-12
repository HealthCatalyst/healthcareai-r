#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @usage UnsupervisedModel(object)
#' @import caret
#' @importFrom R6 R6Class
#' @param object of UnsupervisedModelParams class for $new() constructor
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#'
#' @export

UnsupervisedModel <- R6Class("UnsupervisedModel",
                             
  #Private members
  private = list(
    
    ###########
    # Variables
    
    clustersOnCores = NA,
    grainColValues = NA,
    labelColValues = NA,
    # Creating attributes for performance report
    memsizeOfDataset = NA,
    initialDatasetRows = NA,
    initialDatasetCols = NA,
    
    ###########
    # Functions
    
    registerClustersOnCores = function() {
      if (self$params$cores > 1) {
        suppressMessages(library(doParallel))
        private$clustersOnCores <-
          makeCluster(self$params$cores)
        registerDoParallel(private$clustersOnCores)
      }
    },
    
    stopClustersOnCores = function() {
      if (self$params$cores > 1) {
        stopCluster(private$clustersOnCores)
        registerDoSEQ()
      }
    },
    
    #Set config parameters for the algorithm
    setConfigs = function(p) {
      self$params <- UnsupervisedModelParams$new()
      
      if (!is.null(p$df))
        self$params$df <- p$df
      
      if (!is.null(p$type)) {
        self$params$type <- p$type
        
        #validation on type string value
        if (self$params$type != 'numeric' && self$params$type != 'categorical' && self$params$type != 'mixed') {
          stop('Your type must be numeric, categorical, or mixed')
        }
        
        # If the type is numeric, then all the variables should be numeric.
        if (self$params$type == 'numeric' && isNumeric(self$params$df) == FALSE) {
          stop("Variables must be numeric")
        }
        
        # If the type is Kmodes, then all the variables should be categorical.
        if (self$params$type == 'categorical' && isCategorical(self$params$df,15) == FALSE) {
          stop("Variables must be categorical")
        }
        
      }
      
      if (!is.null(p$labelCol)) 
        self$params$labelCol <- p$labelCol
      
      if (!is.null(p$numOfClusters))
        self$params$numOfClusters <- p$numOfClusters
      
      
      if (!is.null(p$debug))
        self$params$debug <- p$debug
      
      if (!is.null(p$featureReduction))
        self$params$featureReduction <- p$featureReduction
      
      if (!is.null(p$printResults))
        self$params$printResults <- p$printResults
      
      if (!is.null(p$cores))
        self$params$cores <- p$cores
      
      #Set additional settings
      if (isTRUE(self$params$debug)) {
      }
      
      #Set up clusters on cores
      private$registerClustersOnCores()
    },
    
    #Load data
    loadData = function() {
      
      # init dataset variables
      private$memsizeOfDataset <- format(object.size(self$params$df), 
                                         units = "Mb")
      private$initialDatasetRows <- nrow(self$params$df)
      private$initialDatasetCols <- ncol(self$params$df)
      
      # For use in confusion matrices
      # private$prevalence <- table(self$params$df[[self$params$predictedCol]])[2]
      
      if (length(returnColsWithMoreThanFiftyCategories(self$params$df)) > 0) {
        warning('These columns in the df have more than fifty categories: \n',
                paste(
                  shQuote(returnColsWithMoreThanFiftyCategories(self$params$df)), 
                  collapse = ", "),
                '\n This drastically reduces performance. \n',
                'Consider combining into new col with fewer categories.')
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
      self$params$df <- removeColsWithAllSameValue(self$params$df)
      
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
        self$params$df[] <- lapply(self$params$df, imputeColumn)
        
        if (isTRUE(self$params$debug)) {
          print('Entire data set after imputation')
          print(str(self$params$df))
        }
        
      } else {
        if (isTRUE(self$params$debug)) {
          print(paste0("Rows in data set before removing rows with NA's: ", nrow(self$params$df)))
        }
        
        # Remove rows with any NA's
        self$params$df <- na.omit(self$params$df)
        
        if (isTRUE(self$params$debug)) {
          print(paste0("Rows in data set after removing rows with NA's: ", nrow(self$params$df)))
          print("Entire data set after removing rows with NA's")
          print(str(self$params$df))
        }
      }
      
      # Remove columns that are only NA
      self$params$df <- self$params$df[,colSums(is.na(self$params$df)) < nrow(self$params$df)]
      
      # Remove date columns
      dateList <- grep("DTS$", colnames(self$params$df))
      if (length(dateList) > 0) {
        self$params$df <- self$params$df[, -dateList]
      }
      
      if (isTRUE(self$params$debug)) {
        print('Entire data set after removing cols with DTS (ie date cols)')
        print(str(self$params$df))
        print('Now going to remove grainCol...')
      }
      
      # Remove grain col
      if (nchar(self$params$grainCol) != 0) {
        private$grainColValues <- self$params$df[[self$params$grainCol]]
        self$params$df[[self$params$grainCol]] <- NULL
      }
      
      # Remove label col
      if (nchar(self$params$labelCol) != 0) {
        private$labelColValues <- self$params$df[[self$params$labelCol]]
        self$params$df[[self$params$labelCol]] <- NULL
      }
      
      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after separating out grain col')
        print(str(self$params$df))
        #print('Now splitting training set from validation set')
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
    #p: new UnsuperviseModelParameters class object, i.e. p = UnsuperviseModelParameters$new()
    initialize = function(p) {
      
      #Set config parameters
      private$setConfigs(p)
      
      #Load data
      private$loadData()
    },
    
    #Build the Model
    buildClusters = function() {
    },
    
    #Run the Model
    run = function() {
    }
  )
)

