
#' Deploy predictive models, created on your data
#'
#' @description This step allows one to create deploy models on your data
#' and helps determine which performs best.
#' @docType class
#' @usage SupervisedModelDeployment(object)
#' @import caret
#' @importFrom R6 R6Class
#' @param object of SupervisedModelDeploymentParams class for $new() constructor
#' @references \url{http://healthcareai-r.readthedocs.io}
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
  fitObj = NA,
  predictedVals = NA,
  modelName = NA,
  clustersOnCores = NA,
  fitObjFile = NA,
  modelInfoFile = NA,

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
  
  if (!is.null(p$modelName))
  self$params$modelName <- p$modelName
  
  # for deploy method
  if (!is.null(p$cores))
  self$params$cores <- p$cores
  },

  loadData = function() {
    # Load model info
    cat('Loading Model Info...','\n')
    private$loadModelAndInfo()

    cat('Loading Data...','\n')
    if (isTRUE(self$params$debug)) {
      print('Entire data set at the top of the constructor')
      print(str(self$params$df))
      print('Now going to convert chr cols to factor cols...')
    }

    # This also converts chr cols to (needed) factors
    self$params$df <- as.data.frame(unclass(self$params$df))

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
    # Check that a grain column has been specified
    if (nchar(self$params$grainCol) != 0) {
      # Check that grain column exists in dataframe
      if (self$params$grainCol %in% names(self$params$df)) {
        fullGrain <- self$params$df[[self$params$grainCol]]
        self$params$df[[self$params$grainCol]] <- NULL
      } else {
        # grain column not present in df -> ERROR
        stop(paste0('The grain columm you provided does not match any column ',
                    'in the the dataframe. Double check the spelling of your ',
                    'grain column name, keeping in mind that R is case ',
                    'sensitive.\nProvided grain column name: ', 
                    self$params$grainCol))
      }
    } else {
      # No grain column specified -> ERROR
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
    
    # Check that all columns used in develop are present (except predictedCol
    # and grainCOl) and drop extra columns which weren't used in develop (e.g.,
    # columns with no variation in develop, brand new columns in deploy, etc.)
    columnsToKeep <- self$modelInfo$columnNames
    toDrop <- c(self$modelInfo$predictedCol, self$modelInfo$grainCol)
    columnsToKeep <- columnsToKeep[!(columnsToKeep %in% toDrop)]
    # check that all needed columns from develop are present
    if (all(columnsToKeep %in% names(self$params$df))) {
      # drop extra columns in deploy
      self$params$df <- self$params$df[, columnsToKeep]
    } else {# missing columns from develop
      missingCols <- columnsToKeep[!(columnsToKeep %in% names(self$params$df))]
      stop(paste0("Some columns used to develop the model are missing\n",
                  "Missing columns: ", paste(missingCols, collapse = " ")))
    }

    if (isTRUE(self$params$debug)) {
      print('Entire data set after separating removing predicted column')
      print(str(self$params$df))
      print('Now starting imputation, or removing rows with NULLs')
    }

    # Impute columns
    # Impute all columns except grain, person, and predicted.
      colsToImpute <- !(names(self$params$df) %in% 
        c(self$params$grainCol, self$params$personCol, self$params$predictedCol))
    # Impute is TRUE
    if (isTRUE(self$params$impute)) { 
      temp <- imputeDF(self$params$df[names(self$params$df[colsToImpute])], self$modelInfo$imputeVals)
      self$params$df[,colsToImpute] <- temp$df
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
      # Remove rows with any NA's
      self$params$df <- na.omit(self$params$df)

      if (isTRUE(self$params$debug)) {
        print(paste0("Rows in data set after removing rows with NA's: ", nrow(self$params$df)))
        print("Entire data set after removing rows with NA's")
        print(str(self$params$df))
      }
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
    
    # Impute missing values introduced through new factor levels (if any)
    imputeVals <- self$modelInfo$imputeVals
    if (length(newLevels) > 0) {
      out <- imputeDF(as.data.frame(private$dfTestRaw[names(newLevels)]), imputeVals[names(newLevels)])
      private$dfTestRaw[, names(newLevels)] <- as.data.frame(out[1])
    }

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
  }, 
  
  createDf = function() {
    dtStamp <- as.POSIXlt(Sys.time())
    
    # Combine grain.col, prediction, and time to be put back into SAM table
    # TODO: use a common function to reduce lasso-specific code here
    private$outDf <- data.frame(
      0,    # BindingID
      'R',  # BindingNM
      dtStamp,                    # LastLoadDTS
      private$grainTest,          # GrainID
      private$predictions         # Predicted probabilty or predicted values
    )    
    
    predictedResultsName <- ""
    if (self$params$type == "classification") {
      predictedResultsName <- "PredictedProbNBR"
    } else if (self$params$type == "regression") {
      predictedResultsName <- "PredictedValueNBR"
    }
    colnames(private$outDf) <- c(
      "BindingID",
      "BindingNM",
      "LastLoadDTS",
      self$params$grainCol,
      predictedResultsName
    )
    
    # Add top factor columns to outDf (without including the grainCol twice)
    topFactorsDf <- self$getTopFactors(numberOfFactors = 3, includeWeights = F)
    private$outDf <- cbind(private$outDf, topFactorsDf[, 2:ncol(topFactorsDf)])
    
    # Remove row names so df can be written to DB
    # TODO: in writeData function, find how to ignore row names
    rownames(private$outDf) <- NULL
    
    if (isTRUE(self$params$debug)) {
      cat('Dataframe with predictions:', '\n')
      cat(str(private$outDf), '\n')
    }
  },
  
  loadModelAndInfo = function() {
    # Set file names for model and associated information
    modelName <- ifelse(is.null(self$params$modelName), 
                        "",
                        paste0(self$params$modelName, "_"))
    private$fitObjFile <- paste("rmodel_probability_", modelName, 
                        private$algorithmShortName, ".rda", 
                        sep = "")
    private$modelInfoFile <- paste("rmodel_info_", modelName, 
                           private$algorithmShortName, ".rda", 
                           sep = "")
    # Try to load the model
    tryCatch({
      load(private$modelInfoFile)  # Get model info
      self$modelInfo <- modelInfo
      load(private$fitObjFile) # Produces fit object (for probability)
      private$fitObj <- fitObj
      
      # Explicitly print when default model name is being used.
      if (is.null(self$params$modelName)) {
        cat("The modelName parameter was not specified. Using defaults:\n",
            "- ", private$fitObjFile, "\n",
            "- ", private$modelInfoFile, "\n"
            , sep = "")
      }
    }, error = function(e) {
      # Detailed error message
      message <- paste0('You must use a saved model. If you did not already ',
                       'develop a model, first run ',
                       private$algorithmName,
                       'Development to train and save the model. See ?',
                       private$algorithmName,
                       'Development for details.')
      missingFileFlag <- FALSE
      # Provide name of fitObjFile in error message if file doesn't exist 
      if (!file.exists(private$fitObjFile)) {
        message <- paste0(message,
                          "\n- Could not find saved model file: ", private$fitObjFile)
        missingFileFlag <- TRUE
      }
      # Provide name of modelInfoFile in error message if file doesn't exist 
      if (!file.exists(private$modelInfoFile)) {
        message <- paste0(message,
                          "\n- Could not find associated saved model info ",
                          "file: ", 
                          private$modelInfoFile)
        missingFileFlag <- TRUE
      }
      # Provide working directory if either of the two files is missing
      if (missingFileFlag) {
        message <- paste0(message, 
                          "\n- Current working directory: ", getwd())
        if (is.null(self$params$modelName)) {
          # Remind user of modelName parameter if it was not set in deploy and 
          # the default model files are missing
          message <- paste0(message,
                            "\nIf you set a custom model name in development, ",
                            "make sure to also set the modelName ",
                            "parameter in deployment.")
        }
      }
      stop(message)
    })
    
    # Trigger a warning if the predicted variable is different from that of
    # the saved model (since the predicted column is not used, this does not
    # necessarily imply a problem so should not trigger an error)
    if (self$modelInfo$predictedCol != self$params$predictedCol) {
      warningMessage <- paste0("The name of the predicted column in the saved ",
                               "model differs from the name of the predicted ",
                               "column that you have set.", 
                               "\n- Old predicted column: ", 
                               self$modelInfo$predictedCol,
                               "\n- New predicted column: ", 
                               self$params$predictedCol)
      warning(warningMessage)
    }
    
    # Check that the model type (classification, etc.) of the saved model
    # matches the type used in deployment parameters. If not, trigger an error.
    if (self$modelInfo$type != self$params$type) {
      errorMessage <- paste0("The saved model you have loaded is a ",
                             self$modelInfo$type, " model, but you are trying ",
                             "to deploy a ", self$params$type, " model.")
      stop(errorMessage)
    }
  }, 
  
  buildProcessVariableDfList = function(modifiableVariableLevels,
                                        grainColumnValues = NULL, 
                                        smallerBetter = TRUE) {

    # If no grain column values are specified, use the whole dataframe
    if (length(grainColumnValues) == 0) {
      grainColumnValues <- private$grainTest
    } else {
      # Check for misspecified grain column values. If any are found, trigger a 
      # warning
      nonGrain <- grainColumnValues[!(grainColumnValues %in% private$grainTest)]
      if (length(nonGrain) > 0) {
        wrn_mes <- paste0("The following grain column IDs could not be found",
               " and will be omitted: ", paste(nonGrain, collapse = " "))
        warning(wrn_mes)
        grainColumnValues <- intersect(grainColumnValues, private$grainTest)
      }
      # Rearrange grain ids to match the rows
      # TODO: figure out how to avoid re-ordering the grain column values but
      # still match them up correctly with the dataframe rows
      grainColumnValues <- private$grainTest[private$grainTest 
                                             %in% grainColumnValues]
    }

    # Get rows corresponding to the grain ids
    dataframe <- self$params$df[private$grainTest %in% grainColumnValues, ]
    
    # Build the process variables df list
    build_process_variable_df_list(dataframe = dataframe,
                                   modifiable_variable_levels = modifiableVariableLevels,
                                   grain_column_values = grainColumnValues,
                                   predict_function = self$performNewPredictions,
                                   smaller_better = smallerBetter)
  }, 
  
  checkModifiableVariableInput = function(modifiableVariables, 
                                          modifiableVariableLevels) {
    # Set modifiableProcessVariables and smallerPredictionsDesired params
    # Check that either both are present or both are absent.
    # Also check that the modifiable variables actually exist in the data
    
    # If variables are provided in `modifiableVariableLevels` but not 
    # `modifiableVariables` add them to the latter with a warning
    if (missing(modifiableVariables)) {
      modifiableVariables <- names(modifiableVariableLevels)
      warning("No modifiableVariables provided. Using names of ",
              "modifiableVariableLevels: ",
              paste(modifiableVariables, collapse = ", "))
    } else {
      omitted <- 
        names(modifiableVariableLevels)[
          which(!names(modifiableVariableLevels) %in% modifiableVariables)]
      if (length(omitted)) {
        warning(paste(omitted, collapse = ", "), " included in ",
                "modifiableVariableLevels but not modifiableVariables.",
                " Added to modifiableVariables.")
        modifiableVariables <- c(modifiableVariables, omitted)
      }
    }
    
    # Check that mofiable process variable actually exist in the data
    extraColumns <- setdiff(modifiableVariables,
                            names(self$params$df))
    # Issue a warning if some variables are not found in the data
    if (length(extraColumns) > 0) {
      warning("Some of the modifiable process variables specified are not ",
              "present in the data. Mystery variables: \n",
              paste(" - ", extraColumns, collapse = "\n"),
              "\nThese modifiable variables will not be used.")
      modifiableVariables <- intersect(modifiableVariables, 
                                       names(self$params$df))
    }
    
    # Check that modifiable process variables make sense for lasso
    if (private$algorithmName == "Lasso" & length(modifiableVariables) > 0) {
      # Find modifiable variables with 0 coefficient
      notModifiable <- setdiff(modifiableVariables, 
                               self$modelInfo$usedVariables)
      # If such variables exist, remove them from the list of modifiable
      # variables and print a warning.
      if (length(notModifiable) > 0) {
        warning("The following variables have coefficients of 0 in the lasso ",
                "model and will not be used as modifiable variables:\n",
                paste(" - ", notModifiable, "\n"))
        # Remove modifiable variables that are not used by lasso
        modifiableVariables <- intersect(modifiableVariables,
                                         self$modelInfo$usedVariables)
      }
    }
    
    # Check that the modifiable variables are factors or that levels have been
    # specified explicitly
    if (length(modifiableVariables) > 0) {
      nonFactors <- private$findNonFactors(modifiableVariables)
      nonFactors <- setdiff(nonFactors, names(modifiableVariableLevels))
      if (length(nonFactors) > 0) {
        warning("Modifiable process variables must either be categorical ",
                "variables or you must explicitly specify the levels. The ", 
                "following variables are not categorical and will not be ",
                "used:\n",
                paste(" - ", nonFactors, collapse = "\n"))
        modifiableVariables <- setdiff(modifiableVariables, nonFactors)
      }
    }
    
    # Add factor levels which weren't specified explicitly
    for (variable in modifiableVariables) {
      if (!(variable %in% names(modifiableVariableLevels))) {
        modifiableVariableLevels[[variable]] <- self$modelInfo$factorLevels[[variable]]
      }
    }
    
    # Subset to only include valid variables
    modifiableVariableLevels <- modifiableVariableLevels[modifiableVariables]
    
    return(modifiableVariableLevels)
  },
  
  # This function takes a vector of column names and returns the names of 
  # columns which are not factors
  findNonFactors = function(columns) {
    if (!is.null(columns)) {
      return(columns[!unlist(lapply(self$params$df[columns], is.factor))])
    } else {
      return(NULL)
    }
  }
),

  #Public members
  public = list(
    ###########
    # Variables
    modelInfo = NA,

    #parameters
    params = NA,
    
    processVariableDfList = NA,

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
    },
    
    # Get and write model metadata
    getMetadata = function() {

      # Initialize list of metadata items
      metadata <- list()
      
      load(private$modelInfoFile)
      sesInfo <- sessionInfo()
      
      # Model name (custom if provided, default otherwise)
      metadata$model_name <- if (is.null(self$params$modelName)) "default" else self$params$modelName
       
      # Model algorithm
      metadata$algorithm <- private$algorithmShortName
      
      # Datetime model last trained
      metadata$model_last_developed <- file.mtime(private$fitObjFile)
      
      # Datetime predictions made
      metadata$prediction_datetime <- format(Sys.time(), 
                                             paste("%Y-%m-%d %H:%M:%OS", 3, sep = ""))
      
      # R version
      metadata$r_version <- paste0(sesInfo$R.version$major, ".", sesInfo$R.version$minor)
      
      # healthcare.ai version
      metadata$healthcareai_version <- sesInfo$otherPkgs$healthcareai$Version
      
      # Other packages with versions
      packages <- c(sesInfo$basePkgs, names(sesInfo$otherPkgs))
      packages <- sapply(packages[packages != "healthcareai"], 
                         function(pkg) paste(pkg, as.character(packageVersion(pkg))),
                         USE.NAMES = FALSE)
      metadata$other_packages_loaded <- packages
      
      # Features
      metadata$feature_columns <- 
        modelInfo$columnNames[!modelInfo$columnNames %in% 
                                c(modelInfo$predictedCol, modelInfo$grainCol)]
      
      # Assign metadata to an attribute of outDf
      attr(private$outDf, "metadata") <- metadata
      
      # Write metadata to file
      ## Copy captured session info to desired file name
      file_name <- paste0("predictionMetadata_",
                          metadata$model_name, 
                          "_",
                          format(Sys.time(), paste("%Y-%m-%d_%H.%M.%OS", 3, sep = "")), 
                          ".txt")
      
      ## Convert metadata list into formatted character vector to write to file
      to_write <- sapply(names(metadata), function(x) 
        paste0(x, ":\n\t", paste(metadata[[x]], collapse = ", ")))
      
      ## Add PHI warning to top of what will be written to log file
      to_write <- c("WARNING: This file may contain Protected Health Information. Treat it with care.\n", to_write)
      
      ## Write to file
      write(to_write, file = file_name, append = FALSE)
      
      ## Print to screen that file was written and may contain PHI
      message("Model metadata written to ", file_name,  
              "\nWARNING: **This file may contain PHI.**")
      
      ## Append console output to other metadata
      write("console_output_during_prediction:\n\t", file_name, append = TRUE)
      file.append(file_name, "tmp_prediction_console_output.txt")
      
      ## Remove sink file
      invisible(file.remove("tmp_prediction_console_output.txt"))
    },
    
    # A function to get the ordered list of top factors with parameters to 
    # choose how many factors to include and whether or not to include weights
    getTopFactors = function(numberOfFactors = NA, includeWeights = FALSE) {
      # Include all factors by default
      if (is.na(numberOfFactors)) {
        numberOfFactors <- ncol(private$orderedFactors)
      }
      # Don't include more factors than exist
      numberOfFactors <- min(numberOfFactors, ncol(private$orderedFactors))
      # Include grain column
      topFactorsDf <- data.frame(id = private$grainTest)
      if (includeWeights) {
      # Get factor weights
        factorWeights <- t(sapply(1:nrow(private$multiplyRes),
                                  function(i)
                                        private$multiplyRes[i, ][order(private$multiplyRes[i, ],
                                                                  decreasing = TRUE)]))
      }
      # Add each of the top factors
      for (i in 1:numberOfFactors) {
        ithTopFactor <- paste0("Factor", i, "TXT")
        topFactorsDf[[ithTopFactor]] <- private$orderedFactors[, i]
        if (includeWeights) {
          ithWeight <- paste0("Factor", i, "Weight")
          topFactorsDf[[ithWeight]] <- as.numeric(factorWeights[, i])
        }
      }
      return(topFactorsDf)
    },
    
    # Build and return a dataframe with recommendations for the modifiable
    # process varaibles
    getProcessVariablesDf = function(modifiableVariables,
                                     variableLevels = NULL,
                                     grainColumnIDs = NULL,
                                     smallerBetter = TRUE,
                                     repeatedFactors = FALSE,
                                     numTopFactors = 3) {
      # Keep track of time for debugging.
      t0 <- proc.time()
      # Remove modifiable variables which are not valid.
      variableLevels <- private$checkModifiableVariableInput(modifiableVariables,
                                                             variableLevels)
      if (length(variableLevels) == 0) {
        stop("No valid modifiable variables used.")
      }
      
      # Build the list of dataframes.
      processVariableDfList <- private$buildProcessVariableDfList(modifiableVariableLevels = variableLevels,
                                                                  grainColumnValues = grainColumnIDs,
                                                                  smallerBetter = smallerBetter)
      
      # Get name of prediction column in outDf
      predCol <- ifelse(self$params$type == "classification",
                        "PredictedProbNBR",
                        "PredictedValueNBR")
      
      # Get grain column and original predictions
      originalPredictions <- private$outDf[c(self$params$grainCol, predCol)]
      # Rename grainCol to allow dplyr join (else trouble with "by" argument)
      names(originalPredictions)[1] <- "df_grain_column"
            
      # Join grain column and original prediction to recommendations
      processDf <- dplyr::inner_join(originalPredictions,
                                     build_process_variables_df(processVariableDfList,
                                                                repeatedFactors,
                                                                numTopFactors),
                                      by = c("df_grain_column"))
      
      # Rename grain column
      names(processDf)[names(processDf) == "df_grain_column"] <- self$params$grainCol
      if (self$params$debug) {
        ellapsedTime <- (proc.time() - t0)[3]
        message("Modifiable variables computed for ", 
                nrow(processDf),
                " rows in ", 
                ellapsedTime, 
                " seconds.")
      }
      
      # Return the dataframe
      processDf
    }
  )
)
