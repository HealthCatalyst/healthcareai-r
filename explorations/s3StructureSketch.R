# Note: This is incomplete, but sketches some general structure ideas for 
# classes, methods, defaults, and workflow.

# Initialize the generic function `develop`
develop <- function(x, ... ) UseMethod("develop", x)

# Create a develop method for formulas. This is standard with plot, randomForest, etc.
# and lets the user provide formula syntax, which we then parse to separate
# the response and features and provide to develop.default, or they can provide
# a y column and x data matrix directly to develop.default
develop.formula <- function(formula, data, ...) {
  
  if (!inherits(formula, "formula")) 
    stop("develop.formula needs a y ~ x formula as its first argument.  
             You passed it a ", class(formula))
  
  # Parse the formula to get the data and then use standard generic:
  develop.default(data, predictColumn, ... )
}

# develop should probably be chunked into smaller functions.
develop.default <- function(data
                    , predictColumn = NULL
                    , type = NULL
                    # Default to using all algorithms defined for the type:
                    , algorithms = NULL
                    , baseCategory = NULL
                    # It's a little weird to put this much info in the default, but
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
  
  # For each supported algorithm, check if it's to be trained.
  # If so, train over hyperparameter grid, choose the model with best-performing
  # hyperparameters and add that model to the model list. 
  # NB: In general you don't want to grow lists like this in R, but for such a
  # short list it's fine.
  if ("randomForest" %in% algorithms) {
    # data prep steps
    RFs <- train.rf(x = select(data, -predictColumn), 
                    y = data[[predictColumn]],
                    hyperParams = tuningParams$randomForest)
    chooseModel(RFs, performanceMetric)    
    models <- c(models, RF)
    
  }

}

# For a given prediction task, an analyst will likely train multiple models, 
# but they will always be of the same type (regression, classificaiton, etc.),
# so I propose that our fundamental unit is a list of models -- class modelList
# with child classes for regressionList, classificationList, etc. and parent
# class list.
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
    stop("models should be a list of models. If you have just a single model,
         pass it to as.modelList wrapped in list(): as.modelList(models = list(myModel)")
  checkModelType(type)
  
  class(models) <- c(paste0(type, "List"), "modelList", "list")
}

checkModelType <- function(type) {
  modelTypes <- c("unsupervised", "classification", "multiClass", "regression")
  if (!type %in% modelTypes)
    stop("type must be one of:\t", paste(modelTypes, " "))
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
    
  # metric could be a function name. 
  scores <- sapply(models, metric) %>%
    # Add attribute for metric. Use elsewhere to determine whether min or max value is best model
    structure("metric" = metric)
  
  return(scores)
}