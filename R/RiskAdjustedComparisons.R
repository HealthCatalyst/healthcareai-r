# Import the common functions.
source('R/common.R')

#' Make risk adjusted comparisons between groups/units or years/months
#'
#' @description This class allows you to create a model based on the
#' performance of many groups in a cohort (besides group A, for example) and
#' see how well group A does against what the model would predict. Ranking each
#' of the groups this way provides a sense of which group's doing best in
#' terms of a particular measure.
#' @docType class
#' @import caret
#' @import ranger
#' @importFrom R6 R6Class
#' @param df Dataframe whose columns are used for calc.
#' @param predicted.col Column that you want to predict.
#' @param group.col Column that we'll use to differentiate
#' @param impute Set all-column imputation to F or T.
#'
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' # The examples will run as-is, but you can find the data used here
#' # C:\Users\levi.thatcher\Documents\R\win-library\3.2\HCRTools\extdata OR
#' # C:\Program Files\R\R-3.2.3\library\HCRTools\extdata
#'
#' #### Example using csv data ####
#' library(HCRTools)
#' #setwd("C:/Your/script/location") # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
#'
#' totaldf <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' head(totaldf)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = totaldf
#' p$groupCol = 'Gender'
#' p$impute = TRUE
#' p$predictedCol = 'SalariedFlag'
#' p$debug = TRUE
#' p$varImp = TRUE
#' p$numberOfTrees = 200
#'
#' riskAdjComp <- RiskAdjustedComparisons$new(p)
#' riskAdjComp$run()
#'
#' @export

RiskAdjustedComparisons <- R6Class("RiskAdjustedComparisons",

  #Inheritance
  inherit = SupervisedModel,

  private = list(

    dfTemp = NA,
    grid = NA,
    fit.rf = NA,
    predicted = NA,
    dfReturn = NA

  ),

  public = list(

    #Constructor
    #p: new SuperviseModelParameters class object, i.e. p = SuperviseModelParameters$new()
    initialize = function (p) {
      super$initialize(p)
    },

    buildModel = function (group.by.list, j) {

      # Just grab rows corresponding to a particular category in the factor col
      private$dfTemp <- self$params$df[self$params$df[[self$params$groupCol]] == j,]

      trainIndex = createDataPartition(y = private$dfTemp[[self$params$predictedCol]],
                                       p = 0.8,
                                       list = FALSE, times = 1)

      private$dfTrain = private$dfTemp[ trainIndex,]
      private$dfTest  = private$dfTemp[-trainIndex,]

      private$grid <- data.frame(.mtry = floor(sqrt(ncol(private$dfTrain))))

      train.control <- trainControl(
        method = "none",
        number = 1,
        verboseIter = isTRUE(self$params$debug),
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )

      private$fit.rf = train(
        x = private$dfTrain[ ,!(colnames(private$dfTrain) == self$params$predictedCol)],
        y = factor(private$dfTrain[[self$params$predictedCol]]),
        method = "ranger",
        importance = 'impurity',
        metric = "ROC",
        num.trees = self$params$numberOfTrees,
        tuneGrid = private$grid,
        trControl = train.control
      )

    },

    performPrediction = function () {

      private$predicted = predict(object = private$fit.rf,
                          newdata = private$dfTest,
                          type = 'raw')

    },

    generatePerformanceMetrics = function (group.by.list, comparative.performance) {

      # Performed above or below predicted (neg is bad performance)
      diff <- (ifelse(private$dfTest[[self$params$predictedCol]] == 'Y', 1, 0)
               - ifelse(private$predicted == 'Y', 1, 0))

      comparative.performance <- c(comparative.performance, sum(diff))

      # Calculate mean-centered prediction error
      comparative.performance <- (as.numeric(comparative.performance) -
                                    (mean(as.numeric(comparative.performance))))

      private$dfReturn <- data.frame(group.by.list, comparative.performance)

      print("Finished calculating your risk-adjusted comparison")
      print('Note that positive values denote performance above expected:')
      print(private$dfReturn)

    },

    #Override: run prediction
    run = function () {

      # Pre-create empty vectors
      group.by.list <- vector('character')
      comparative.performance <- vector('character')

      for (j in unique(self$params$df[[self$params$groupCol]])) {
        group.by.list <- c(group.by.list, j)

        # Build Model
        self$buildModel(group.by.list, j)

        # Perform prediction
        self$performPrediction()

        # Generate performance metrics
        self$generatePerformanceMetrics(group.by.list, comparative.performance)
      }

      return(invisible(private$dfReturn))
    }


  )

)
