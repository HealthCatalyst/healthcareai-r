#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @usage SupervisedModelDevelopment(object)
#' @import caret
#' @importFrom R6 R6Class
#' @param object of SuperviseModelParameters class for $new() constructor
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#'
#' @export

SupervisedModelDevelopment <- R6Class("SupervisedModelDevelopment",

  #Private members
  private = list(

    ###########
    # Variables
    dfTrain = NA,
    dfTrainRaw = NA,
    dfTest = NA,
    dfGrain = NA,
    grainTest = NA,
    prevalence = NA,
    factorLevels = NA,
    imputeVals = NA,
    
    algorithmShortName = NA,

    clustersOnCores = NA,
    grainColValues = NA,

    # Creating attributes for performance report
    memsizeOfDataset = NA,
    initialDatasetRows = NA,
    initialDatasetCols = NA,

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

    #Set config parameters for the algorithm
    setConfigs = function(p) {

      self$params <- SupervisedModelDevelopmentParams$new()

      if (!is.null(p$df))
        self$params$df <- p$df

      if (p$grainCol != "") {
        if (p$grainCol %in% colnames(p$df)) {
        self$params$grainCol <- p$grainCol
        } else {
          stop("Your specified grainCol is not in your development data.\n",
               "Please check your specifications.")
        }
      }
    
      if (!is.null(p$predictedCol)) {
        if (p$predictedCol %in% colnames(p$df)) {
        self$params$predictedCol <- p$predictedCol
        } else { 
          stop("Your specified predictedCol is not in your development data.\n",
               "Please check your specifications.")
        }
      }
    
      if (!is.null(p$personCol))
        self$params$personCol <- p$personCol

      if (!is.null(p$groupCol))
        self$params$groupCol <- p$groupCol

      if (!is.null(p$type) && p$type != '') {
        self$params$type <- p$type

        # validation on type string values
        if (self$params$type != 'regression' 
            && self$params$type != 'classification' 
            && self$params$type != 'multiclass') {
          stop('Your type must be regression, classification, or multiclass')
        }
        if (self$params$type == 'classification' 
            && !isBinary(self$params$df[[self$params$predictedCol]])) {
          stop('Dependent variable must be binary for classification')
        }
        if (self$params$type == 'regression' 
            && isBinary(self$params$df[[self$params$predictedCol]])) {
          stop('Dependent variable cannot be binary for regression')
        }
        if (self$params$type == 'classification' 
            && isBinary(self$params$df[[self$params$predictedCol]])
            && !isTargetYN(self$params$df[[self$params$predictedCol]])) {
          stop('predictedCol must be Y/N. IIF function in sql may help')
        }

        # Make sure N/Y are properly ordered (deals with edge case)
        if (self$params$type == "classification" 
            && is.factor(self$params$df[[self$params$predictedCol]]) 
            && levels(self$params$df[[self$params$predictedCol]])[1] == "Y") {
          self$params$df[[self$params$predictedCol]] <- 
            factor(self$params$df[[self$params$predictedCol]], 
                   levels = c("N", "Y"))
        }
      }

      if (!is.null(p$impute))
        self$params$impute <- p$impute

      if (!is.null(p$debug))
        self$params$debug <- p$debug

      if (!is.null(p$varImp))
        self$params$varImp <- p$varImp

      if (!is.null(p$printResults))
        self$params$printResults <- p$printResults

      if (!is.null(p$cores))
        self$params$cores <- p$cores

      if (!is.null(p$xgb_params))
        self$params$xgb_params <- p$xgb_params
      
      if (!is.null(p$modelName))
        self$params$modelName <- p$modelName

      #Set additional settings
      if (isTRUE(self$params$debug)) {
        print('Training data set immediately before training')
        print(str(private$dfTrain))
      }

      #Set up clusters on cores
      private$registerClustersOnCores();
    },

    #Load data
    loadData = function() {

      # init dataset variables
      private$memsizeOfDataset <- format(object.size(self$params$df), 
                                         units = "Mb")
      private$initialDatasetRows <- nrow(self$params$df)
      private$initialDatasetCols <- ncol(self$params$df)

      # For use in confusion matrices
      private$prevalence <- table(self$params$df[[self$params$predictedCol]])[2]
      
      # Identify factor columns with more than 50 levels, other than grainCol 
      # and predictedCol
      skipCols <- c(self$params$predictedCol, 
                    self$params$grainCol, 
                    self$params$personCol)
      fiftyPlus <- returnColsWithMoreThanFiftyCategories(self$params$df)
      fiftyPlus <- fiftyPlus[!(fiftyPlus %in% skipCols)]
      if (length(fiftyPlus) > 0) {
        warning('These columns in the df have more than fifty categories: \n',
                paste(
                 shQuote(fiftyPlus), 
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

      # For multiclass xgboost initialization:
      # 1. Save the class names before they are converted.
      # 2. Get the number of classes.
      # 3. Save the grain column for output.
      if (self$params$type == 'multiclass' ) {
        # Forget target classes that don't occur in the developset
        self$params$df[[self$params$predictedCol]] <-
          as.factor(as.character(self$params$df[[self$params$predictedCol]]))
        # Names
        ind <- grep(self$params$predictedCol, colnames(self$params$df))
        tempCol <- self$params$df[,ind]
        self$params$xgb_targetNames <- sort(unique(tempCol))
        # Number
        self$params$xgb_numberOfClasses <- length(self$params$xgb_targetNames)
        self$params$xgb_params$num_class <- self$params$xgb_numberOfClasses
        # Grain
        private$dfGrain <- self$params$df[[self$params$grainCol]]
        if (is.null((private$dfGrain))) {
          private$dfGrain <- 1:nrow(self$params$df)
          self$params$grainCol <- "dfRowNumber"
        }
        rm(ind, tempCol)
        # prints
        if (isTRUE(self$params$debug)) {
          print('Unique classes found:')
          print(self$params$xgb_targetNames)
          print('Number of classes:')
          print(self$params$xgb_numberOfClasses)
        }
      }


      # Convert to data.frame (in case of data.table)
      # This also converts chr cols to (needed) factors
      self$params$df <- as.data.frame(unclass(self$params$df))
      
      # Remove factors levels which don't actually occur in the training data
      # Different case for single column vs. multiple columns
      # Identify factor columns
      factors <- sapply(self$params$df, is.factor)
      # Don't touch predictedCol, grainCol, or personCol
      for (col in skipCols) factors[[col]] <- FALSE

      if (is.data.frame(self$params$df[, factors])) { # multiple columns
        self$params$df[, factors] <- lapply(self$params$df[, factors], as.character)
        self$params$df[, factors] <- lapply(self$params$df[, factors], as.factor)
      } else {# single column
        self$params$df[, factors] <- sapply(self$params$df[, factors], as.character)
        self$params$df[, factors] <- sapply(self$params$df[, factors], as.factor)
      }

      # Check for factor levels which occur infrequently
      lowLevels = list()
      tempDf = self$params$df
      for (col in names(tempDf)) {
        if (is.factor(tempDf[, col]) & !(col %in% skipCols)) { 
          tab <- table(tempDf[, col])
          if (any(tab <= 3)) {
            lowLevels[[col]] <- names(tab)[tab <= 3]  
          }
        }
      }
      
      # Print warning about factors with levels that occur infrequently
      if (length(lowLevels) > 0) {
        # Detailed warning
        warningMessage <- paste0('Each of the following categorical variable ',
                                 'levels occurs 3 times or fewer:\n',
                                 paste('- ', names(lowLevels), ":", lowLevels, 
                                       collapse = "\n"),
                                 '\nConsider grouping these together with ',
                                 'other levels.')
        # Print shorter warning if there are many factor levels which occur
        # infrequently
        if (max(unlist(lapply(lowLevels, length))) > 5) {
          warningMessage <- paste0('Each of the following categorical ',
                                   'variables has levels that occur 3 times or',
                                   ' fewer:\n',
                                   paste('- ', names(lowLevels), ":",
                                         lapply(lowLevels, length), "levels",
                                         collapse = "\n"),
                                   '\nConsider grouping these together with ',
                                   'other levels.\n',
                                   'You can view the levels of a column using',
                                   ' the "table" command.')
        }
        warning(warningMessage)
      }
      
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
      # Save imputation values into modelInfo
      self$modelInfo$imputeVals <- private$imputeVals

      # Remove columns that are only NA
      self$params$df <- self$params$df[,colSums(is.na(self$params$df)) < nrow(self$params$df)]

      # Remove date columns
      self$params$df <- removeColsWithDTSSuffix(self$params$df)

      if (isTRUE(self$params$debug)) {
        print('Entire data set after removing cols with DTS (ie date cols)')
        print(str(self$params$df))
        print('Now going to remove grainCol...')
      }

      # For rf/lasso, remove grain col (if specified)
      # For LMM, don't remove grain col even if specified--note personCol
      if ((nchar(self$params$grainCol) != 0) & (nchar(self$params$personCol) == 0)) {
        private$grainColValues <- self$params$df[[self$params$grainCol]]
        self$params$df[[self$params$grainCol]] <- NULL
      }

      if (isTRUE(self$params$debug) && nchar(self$params$grainCol) != 0) {
        print('Entire data set after separating out grain col')
        print(str(self$params$df))
        print('Now splitting training set from validation set')
      }

      #Declare that the predicted col is a factor, or category to be predicted.
      if (self$params$type == 'classification' || self$params$type == 'multiclass' ) {
        self$params$df[[self$params$predictedCol]] = as.factor(self$params$df[[self$params$predictedCol]])
      }
      
      # Save column names for deploy
      self$modelInfo$columnNames <- names(self$params$df)
      
      # Save model parameters for deploy
      self$modelInfo$type <- self$params$type
      self$modelInfo$predictedCol <- self$params$predictedCol
      self$modelInfo$grainCol <- self$params$grainCol
      
      # Perform train/test split

      # Remove rows where predicted.col is null in train.
      # Also in test, because it breaks performance metric calculation.
      self$params$df <- removeRowsWithNAInSpecCol(self$params$df,
                                                  self$params$predictedCol)

      if (isTRUE(self$params$debug)) {
        print('Training data set after removing rows where pred col is null')
        print(str(private$dfTrain))
      }

      trainIndex <- createDataPartition(y = self$params$df[[self$params$predictedCol]],
                                       p = 0.8,
                                       list = FALSE, times = 1)

      private$dfTrain <- self$params$df[ trainIndex,]
      private$dfTest  <- self$params$df[-trainIndex,]
      private$grainTest  <- private$dfGrain[-trainIndex] # Save grain for xgboost output
      self$trainIndex <- trainIndex

      if (isTRUE(self$params$debug)) {
        print('Training data set after splitting from main df')
        print(str(private$dfTrain))
      }

      if (isTRUE(self$params$debug)) {
        print('Validation data set after splitting from main df')
        print(str(private$dfTest))
      }

      # Get factor levels for dummy creation in deploy. 
      # TODO replace this part with LIME.
      private$dfTrainRaw <- private$dfTrain
      private$dfTrainRaw[[self$params$predictedCol]] <- NULL

      # Get factor levels as a private attribute and find levels which don't
      # make it into the training data
      private$factorLevels <- list()
      missingLevels <- list()
      for (col in names(private$dfTrainRaw)) {
        # only keep factor variables other than response variable
        if ((is.factor(private$dfTrainRaw[, col])) 
            & (col != self$params$predictedCol)
            & (col != self$params$personCol)) {
          # add levels to list
          devLevels <- levels(private$dfTrainRaw[, col])
          private$factorLevels[col] <- list(devLevels)
          # check for levels that are not in the train set
          trainLevels <- levels(as.factor(as.character(private$dfTrainRaw[, col])))
          if (length(devLevels[!devLevels %in% trainLevels]) > 0) {
            missingLevels[[col]] <- devLevels[!devLevels %in% trainLevels]
          }
        }
      }
      
      # Add factor levels (calculated in SMD) to fitLogit object
      self$modelInfo$factorLevels <- private$factorLevels 
      
      # Print warning about factors with levels that occur infrequently
      if (length(missingLevels) > 0) {
        warning('The following categorical variable levels were not used in ',
                'training the model:\n',
                paste('- ', names(missingLevels), ":", missingLevels, 
                      collapse = "\n")
                )
      }

      if (isTRUE(self$params$debug)) {
        print('Factor levels for top factor calculation')
        print(str(private$factorLevels))
      }
    }, 
    
    # This function must be in here for the row-wise predictions and
    # can be replaced when LIME-like functionality is complete.
    fitGeneralizedLinearModel = function() {
      if (isTRUE(self$params$debug)) {
        cat('generating fitLogit for row-wise guidance...',"\n")
      }
      
      if (self$params$type == 'classification') {
        self$modelInfo$fitLogit <- glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = self$params$df,
          family = binomial(link = "logit"),
          metric = "ROC",
          control = list(maxit = 10000),
          trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
        )
        
      } else if (self$params$type == 'regression') {
        self$modelInfo$fitLogit <- glm(
          as.formula(paste(self$params$predictedCol, '.', sep = " ~ ")),
          data = self$params$df,
          metric = "RMSE",
          control = list(maxit = 10000)
        )
      }
    },
    
    saveModel = function(fitModel) {
      if (isTRUE(self$params$debug)) {
        cat("Saving model...","\n")
      }
      
      # Get model and associated information
      fitObj <- fitModel
      modelInfo <- self$modelInfo
      
      # Set file names for model and associated information
      modelName <- ifelse(is.null(self$params$modelName),
                          "",
                          paste0(self$params$modelName, "_"))
      
      fitObjFile <- paste("rmodel_probability_", modelName, 
                          private$algorithmShortName, ".rda", 
                          sep = "")
      modelInfoFile <- paste("rmodel_info_", modelName, 
                             private$algorithmShortName, ".rda", 
                             sep = "")

      # Save model and associated information
      save(fitObj, file = fitObjFile)
      save(modelInfo, file = modelInfoFile)
    }
  ),

  #Public members
  public = list(

    ###########
    # Variables

    #parameters
    params = NA,
    trainIndex = NA,
    modelInfo = list(),

    ###########
    # Functions

    #Constructor
    #p: new SupervisedModelDevelopmentParams class object,
    #   i.e. p = SuperviseModelParams$new()
    initialize = function(p) {

      #Set config parameters
      private$setConfigs(p)

      #Load data
      private$loadData()
    },

    #Build the Model
    buildModel = function() {
    },

    #Run the Model for Prediction
    run = function() {
    }
  )
)
