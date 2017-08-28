#' Build clusters based on your data.
#' @description This step allows one to build clusters on your data
#' @docType class
#' @usage UnsupervisedModel(object)
#' @importFrom R6 R6Class
#' @param object of UnsupervisedModelParams class for $new() constructor
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @seealso \code{\link{KmeansClustering}}
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
    imputeVals = NA,
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
      
      if (!is.null(p$grainCol)) 
        self$params$grainCol <- p$grainCol
      
      if (nchar(p$labelCol) != 0) { 
        self$params$labelCol <- p$labelCol
        message('Kmeans is ideal for unlabeled data. The label column here is meant 
for comparison after clustering. If you are trying to do classification,
we recommend that you use supervised multiclass.\n')
      }
      
      if (!is.null(p$impute))
        self$params$impute <- p$impute
      
      if (!is.null(p$numOfClusters))
        self$params$numOfClusters <- p$numOfClusters
      
      if (!is.null(p$debug))
        self$params$debug <- p$debug
      
      if (!is.null(p$usePCA))
        self$params$usePCA <- p$usePCA
      
      if (!is.null(p$numOfPCA))
        self$params$numOfPCA <- p$numOfPCA

      
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
      
      # Impute all columns except grain, person, and predicted.
      colsToImpute <- !(names(self$params$df) %in% 
                          c(self$params$grainCol, self$params$personCol, self$params$predictedCol))
      # Impute is TRUE
      if (isTRUE(self$params$impute)) {
        temp <- imputeDF(self$params$df[names(self$params$df[colsToImpute])], self$modelInfo$imputeVals)
        self$params$df[,colsToImpute] <- temp$df
        private$imputeVals <- temp$imputeVals
        temp <- NULL
        
        if (isTRUE(self$params$debug)) {
          print('Entire data set after imputation')
          print(str(self$params$df))
        }
        # Impute is FALSE
      } else {
        if (isTRUE(self$params$debug)) {
          print(paste0("Rows in data set before removing rows with NA's: ", nrow(self$params$df)))
        }
        # Calculate impute values for deploy
        temp <- imputeDF(self$params$df[,colsToImpute]) 
        private$imputeVals <- temp$imputeVals
        temp <- NULL
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
      } else {
        private$grainColValues <- 1:nrow(self$params$df)
      }
      
      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after separating out grain col')
        print(str(self$params$df))
        print('Now going to remove labelCol...')
      }
      
      # Remove label col
      if (nchar(self$params$labelCol) != 0) {
        private$labelColValues <- self$params$df[[self$params$labelCol]]
        self$params$df[[self$params$labelCol]] <- NULL
      }
      
      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after separating out label col')
        print(str(self$params$df))
        print('Now creating dummy variables for binary columns...')
      }
      
      # Create dummy variables for binary columns
      # Find binary columns
      nFacs <- sapply(self$params$df[sapply(self$params$df, is.factor)], nlevels)
      binaryCatCols <- names(nFacs[nFacs==2])
      if (length(binaryCatCols>0)) {
        # Create dummies
        data <- caret::dummyVars(~., data = self$params$df[binaryCatCols], fullRank = T)
        temp <- data.frame(predict(data, newdata = self$params$df[binaryCatCols], na.action = na.pass))
        # Remove originals, replace with binary
        self$params$df[binaryCatCols] <- NULL
        self$params$df <- cbind(self$params$df, temp)
      }


      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after creating dummy variables.')
        print(str(self$params$df))
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
    performClustering = function() {
    },
    
    #Run the Model
    run = function() {
    }
  )
)

