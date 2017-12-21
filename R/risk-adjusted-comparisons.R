#' Make risk adjusted comparisons between groups/units or years/months
#'
#' @description This class allows you to create a model based on the
#' performance of many groups in a cohort (besides group A, for example) and
#' see how well group A does against what the model would predict. Ranking each
#' of the groups this way provides a sense of which group's doing best in
#' terms of a particular measure.
#' @docType class
#' @usage RiskAdjustedComparisons(df, predictedCol, groupCol, impute)
#' @import caret
#' @import ranger
#' @importFrom R6 R6Class
#' @param df Dataframe whose columns are used for calc.
#' @param predictedCol Column that you want to predict.
#' @param groupCol Column that we'll use to differentiate
#' @param impute Set all-column imputation to F or T.
#'
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'
#' #### Example using SQL data ####
#' 
#' \dontrun{
#' library(healthcareai)
#'
#' connection.string <- "
#' driver={SQL Server};
#' server=localhost;
#' database=SAM;
#' trusted_connection=true
#' "
#'
#' query <- "
#' SELECT
#' [PatientEncounterID]
#' ,[PatientID]
#' ,[SystolicBPNBR]
#' ,[LDLNBR]
#' ,[A1CNBR]
#' ,[GenderFLG]
#' ,[ThirtyDayReadmitFLG]
#' FROM [SAM].[dbo].[HCRDiabetesClinical]
#' "
#'
#' df <- selectData(connection.string, query)
#'
#' p <- SupervisedModelDevelopmentParams$new()
#' p$df <- df
#' p$groupCol <- "GenderFLG"
#' p$impute <- TRUE
#' p$predictedCol <- "ThirtyDayReadmitFLG"
#' p$debug <- FALSE
#' p$cores <- 1
#'
#' riskAdjComp <- RiskAdjustedComparisons$new(p)
#' riskAdjComp$run()
#' }
#'
#' @export

RiskAdjustedComparisons <- R6Class("RiskAdjustedComparisons",

  #Inheritance
  inherit = SupervisedModelDevelopment,

  private = list(
    dfTemp = NA,
    grid = NA,
    fitRf = NA,
    predicted = NA
  ),

  public = list(
    dfReturn = NA,

    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      super$initialize(p)
    },

    buildModel = function(groupbyList, j) {
      # Just grab rows corresponding to a particular category in the factor col
      private$dfTest <- self$params$df[self$params$df[[self$params$groupCol]] == j,]
      private$dfTrain <- self$params$df[self$params$df[[self$params$groupCol]] != j,]

      # Only works for classification
      private$grid <- data.frame(mtry = floor(sqrt(ncol(private$dfTrain))), 
                                 splitrule = 'gini',
                                 min.node.size = 1)
      
      trainCtrl <- trainControl(
        method = "none",
        number = 1,
        verboseIter = isTRUE(self$params$debug),
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )
      
      private$fitRf = train(
        x = private$dfTrain[ ,!(colnames(private$dfTrain) == self$params$predictedCol)],
        y = factor(private$dfTrain[[self$params$predictedCol]]),
        method = "ranger",
        importance = 'impurity',
        metric = "ROC",
        num.trees = self$params$trees,
        tuneGrid = private$grid,
        trControl = trainCtrl
      )
    },

    performPrediction = function() {
      private$predicted = predict(object = private$fitRf,
                          newdata = private$dfTest,
                          type = 'raw')
    },

    #Override: run prediction
    run = function() {
      # Pre-create empty vectors
      groupbyList <- vector('character')
      comparativePerformance <- vector('character')

      for (j in unique(self$params$df[[self$params$groupCol]])) {
        groupbyList <- c(groupbyList, j)

        # Build Model
        self$buildModel(groupbyList, j)

        # Perform prediction
        self$performPrediction()

        # Performed above or below predicted (neg is bad performance)
        diff <- (ifelse(private$dfTest[[self$params$predictedCol]] == 'Y', 1, 0)
                 - ifelse(private$predicted == 'Y', 1, 0))

        comparativePerformance <- c(comparativePerformance, sum(diff))
      }

      # Calculate mean-centered prediction error
      comparativePerformance <- (as.numeric(comparativePerformance) -
                                    (mean(as.numeric(comparativePerformance))))

      self$dfReturn <- data.frame(groupbyList, comparativePerformance)

      print("Finished calculating your risk-adjusted comparison")
      print('Note that positive values denote performance above expected:')
      print(self$dfReturn)

      return(invisible(self$dfReturn))
    }
  )
)
