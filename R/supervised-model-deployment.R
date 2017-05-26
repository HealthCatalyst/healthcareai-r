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
   dfTrainTemp = NULL,
   dfTestTemp = NULL,

   grainTest = NULL,
   fit = NA,
   fitLogit = NA,
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

     if (!is.null(p$testWindowCol))
       self$params$testWindowCol <- p$testWindowCol

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

     if (!is.null(p$writeToDB))
       self$params$writeToDB <- p$writeToDB
     
     if (!is.null(p$sqlConn))
       self$params$sqlConn <- p$sqlConn

     if (!is.null(p$destSchemaTable))
       self$params$destSchemaTable <- p$destSchemaTable

     },

   loadData = function() {
     if (isTRUE(self$params$debug)) {
       print('Entire data set at the top of the constructor')
       print(str(self$params$df))
       print('Now going to convert chr cols to factor cols...')
     }

     # Save this to put in later.
     tempTestWindow <- self$params$df[[self$params$testWindowCol]]

     # Convert to data.frame (in case of data.table)
     # This also converts chr cols to (needed) factors
     self$params$df <- as.data.frame(unclass(self$params$df))

     # Remove columns that are only NA
     # Put predicted.col back (since it's NULL when sql only pulls in test)
     temp <- self$params$df[[self$params$predictedCol]]
     self$params$df <-
       self$params$df[, colSums(is.na(self$params$df)) < nrow(self$params$df)]
     self$params$df[[self$params$predictedCol]] <- temp

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

     # Remove columns with zero variance
     self$params$df <- removeColsWithAllSameValue(self$params$df)
     self$params$df[[self$params$testWindowCol]] <- tempTestWindow
     # Prevents removing the test window column when they are all the same.

     if (isTRUE(self$params$debug)) {
       print('Entire df after removing feature cols w/zero var')
       print(str(self$params$df))
     }

     # Remove grain.col from df; below we split it into graintest
     if (nchar(self$params$grainCol) != 0) {
       fullGrain <- self$params$df[[self$params$grainCol]]
       self$params$df[[self$params$grainCol]] <- NULL
     } else {
       stop('You must specify a GrainID column when initializing TuneSupervisedModel')
     }

     if (isTRUE(self$params$debug)) {
       print('Entire data set after separating out grain col')
       print(str(self$params$df))
       print('Now splitting into Train and Test')
     }

     # Split df into temp train and test (then rejoin for dummy creation)
     # Sadly, this even has to be done nightly (with a saved model)
     private$dfTrainTemp <-
       self$params$df[self$params$df[[self$params$testWindowCol]] == 'N', ]
     private$dfTestTemp <-
       self$params$df[self$params$df[[self$params$testWindowCol]] == 'Y', ]

     if (isTRUE(self$params$debug)) {
       print('Temp training set after splitting')
       print(str(private$dfTrainTemp))
       print('Temp testing set after splitting')
       print(str(private$dfTestTemp))

       print('Now starting imputation, or removing rows with NULLs')
     }

     # Always do imputation on all of test set (since each row needs pred)
     private$dfTestTemp[] <- lapply(private$dfTestTemp, imputeColumn)

     # Join temp train and test back together, so dummy creation is consistent
     self$params$df <-
       rbind(private$dfTrainTemp, private$dfTestTemp)

     if (isTRUE(self$params$debug)) {
       print('Entire data set after imputation')
       print(str(self$params$df))
       print('Now creating dummy variables...')
     }

     # Make sure label column is numeric before creating dummy var
     self$params$df[[self$params$predictedCol]] =
       as.numeric(self$params$df[[self$params$predictedCol]])

     # If all Y, remove test window col before imputation. Add back in after.
     if (isTRUE(all(tempTestWindow == 'Y'))) {
       self$params$df[[self$params$testWindowCol]] <- NULL
       private$dfTestTemp[[self$params$testWindowCol]] <- NULL
     }

     # Split factor columns into dummy columns (for use in deploypred method)
     data <- dummyVars(~., data = self$params$df, fullRank = T)
     self$params$df <-
       data.frame(predict(data, newdata = self$params$df, na.action = na.pass))

     # Add test window column back in with correct name and as ones.
     if (isTRUE(all(tempTestWindow == 'Y'))) {
       self$params$df[[paste0(self$params$testWindowCol, '.Y')]] <- 1
     }  

     # Now that we have dummy vars, switch label to factor so this is classif.
     if (self$params$type == 'classification') {
       # Since caret can't handle 0/1 for classif, need to convert to N/Y
       # http://stackoverflow.com/questions/18402016/error-when
       # -i-try-to-predict-class-probabilities-in-r-caret
       self$params$df[[self$params$predictedCol]] <-
         ifelse(self$params$df[[self$params$predictedCol]] == 1, 'N', 'Y')
       self$params$df[[self$params$predictedCol]] <-
         as.factor(self$params$df[[self$params$predictedCol]])
     }

     if (isTRUE(self$params$debug)) {
       print('Entire data set after creating dummy vars')
       print(str(self$params$df))
     }

     # Always create test set from df, and drop test.window.cols from test set
     private$dfTest <- self$params$df[self$params$df[[paste0(self$params$testWindowCol, '.Y')]] == 1, ]
     private$dfTest <-
       private$dfTest[, !(names(private$dfTest) %in% c(
         self$params$testWindowCol,
         paste0(self$params$testWindowCol, '.Y')
       ))]

     if (isTRUE(self$params$debug)) {
       print('Test set after splitting from df (and then removing windowcol)')
       print(str(private$dfTest))
     }

     # Now that we have train/test, split grain col into test (for use at end)
     if (nchar(self$params$grainCol) != 0) {
        tempMask <-
          rownames(self$params$df[self$params$df[[paste0(self$params$testWindowCol, '.Y')]] == 1,])
       private$grainTest <- fullGrain[as.integer(tempMask)]

       if (isTRUE(self$params$debug)) {
         print('Grain col vector with rows of test set (after created)')
         print(private$grainTest[1:10])
       }

     } else {
       stop('You must specify a GrainID column when initializing
         TuneSupervisedModel')
    }

    # Pass raw (un-imputed) dfTest to object, so important factors aren't null
    private$dfTestRaw <-
      private$dfTest[, !(names(private$dfTest) %in% c(self$params$predictedCol))]

    # For LMM, remove ID col so it doesn't interfere with row-based varimp calc
    if (nchar(self$params$personCol) != 0) {
       private$dfTestRaw[[self$params$personCol]] <- NULL
    }

    if (isTRUE(self$params$debug)) {
      print('Raw test set (sans imputation) created for mult with coeffs')
      print(str(private$dfTestRaw))
    }

    if (isTRUE(self$params$debug)) {
      print('Test set after undergoing imputation')
      print(str(private$dfTest))
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
