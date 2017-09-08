# Note: This is very incomplete and somewhere between R and pseudocode

develop <- function(x, ... ) UseMethod("develop", x)

develop.formula <- function(formula, data, ...) {
  
  if (!inherits(formula, "formula")) 
    stop("develop.formula needs a y ~ x formula as its first argument.  
             You passed it a ", class(formula))
  
  # Parse the formula to get the data and then use standard generic:
  develop(data, predictColumn, ... )
}

# develop is already way too long and therefore non-modular.
# Needs to be chunked out into smaller functions.
develop <- function(data
                    , predictColumn = NULL
                    , type = NULL
                    # Default to using all algorithms defined for the type:
                    , algorithms = NULL
                    # It's a little weird to put this much info the default, but
                    # it would make it easy for users to copy this list and modify 
                    # it; otherwise nested lists can be tricky.
                    , tuningParams = list(randomForest = list(mtry = c(10, 100, 1000),
                                                              ntree = c(100, 200, 300)),
                                          lasso = list(alpha = c(0, 1),
                                                       lambdaMinRatio = c(1e-2, 1e-3, 1e-4)),
                                          xgb = list())
                    , performanceMetric = NULL  # Default to our favorite for the type
) {
  
  # If type of model isn't specified, figure it out:
  if (is.null(type)) {
    
    # If no predictColumn, unsupervised
    if (is.null(predictColumn)) {
      type <- "unsupervised"
      # Otherwise distinguish binary classify, multiclass, and regression:
    } else {
      # Let 0/1 outcomes be binary classification: 
      if (length(unique(data[[predictColumn]])) == 2) {  
        type <- "classification"
      } else if (!is.numeric(data[[predictColumn]])) {
        type <- "multiClass"
      } else {
        type <- "regression"
      }
    }
  }
  # Initialize modelList object that will eventually be returned
  models <- modelList(type)
  
  
  # Decide which algorithms to train
  
  # Currently supported algorithm dictionary
  algorithmsByType <- list(unsupervised = "kmeans",
                           classification = c("randomForest", "lasso"),
                           multiClass = "xgb",
                           regression = c("randomForest", "lasso"))
  availableAlgorithms <- algorithmsByType[[type]]
  
  # If algorithms to use not specified, use all available ...
  if (is.null(algorithms)) {
    algorithms <- availableAlgorithms
    # ... otherwise make sure all specified algorithms are available
  } else {
    unsupportedAlgorithms <- algorithms[!algorithms %in% availableAlgorithms]
    if (length(unsupportedAlgorithms)) {
      stop(paste(unsupportedAlgorithms, sep = " "), "not currently available for ", type)
    }
  }
  
  
  # Perhaps call appropriate training function(s) here for type x algorithm
  # e.g. regression-lasso, classification-lasso, multiClass-xgb, etc.
  
  # For each supported algorithm, check if it's to be trained
  # If so train over hyperparameter grid and keep the best performing model.
  if ("randomForest" %in% algorithms) {
    RFs <- train.rf(x = select(data, -predictColumn), 
                    y = data[[predictColumn]],
                    hyperParams = tuningParams$randomForest)
    chooseModel(RFs, performanceMetric)    
    models <- c(models, RF)
    
  }

}


modelList <- function(type, ...) {
  # Constructor function for class modelList
  checkModelType(type)
  initialList <- list()
  class(initialList) <- c(paste0(type, "List"), "modelList", "list")
  return(initialList)
}

as.modelList <- function(models, type) {
  # Take a list of models and check their class
  # Might be able to determine type from models, but with all the different algorithms might be tough
  if (!inherits(models, "list"))
    stop("models should be a list of models")
  checkModelType(type)
  
  class(models) <- c(paste0(type, "List"), "modelList", "list")
}

checkModelType <- function(type) {
  modelTypes <- c("unsupervised", "classification", "multiClass", "regression")
  if (!type %in% modelTypes)
    stop("type must be one of:", paste(modelTypes, " "))
}

train <- function(x, ...) UseMethod(x)

train.rf <- function(x, y, hyperParams) {
  
    parameterGrid <- expand.grid(hyperParams)
    
    # Train at each combo of hyperparameters and return the whole model list
    # Could use caret for this, but caret has always seemed like thin
    # wrappers on the underlying functions anyway.
    lapply(seq_len(nrow(parameterGrid)), function(i) {
      randomForest(data, etc., 
                   mtry = parameterGrid$mtry[i],
                   ntree = parameterGrid$ntree[i])
    }) %>%
      as.modelList() %>%
      return()
}

chooseModel <- function(modelList, performanceMetric) {
    # Call assessModels, return best performing model
}


assessModels <- function(modelList, metric = NULL) {
  
  if (is.null(metric)) {
    # Use class(modelList) to pick our favorite
  }
    
  # metric could be a function name. Need to figure out how to make that call work
  scores <- sapply(models, metric) %>%
    # Add attribute for metric. Use elsewhere to determine whether min or max value is best model
    structure("metric" = metric)
  
  return(scores)
}