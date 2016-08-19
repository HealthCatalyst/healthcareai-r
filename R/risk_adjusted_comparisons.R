# Import the common functions.
source('R/common.R')

#' Make risk adjusted comparisons between groups/units or years/months
#'
#' @description This class allows you to create a model based on the
#' performance of many groups in a cohort (besides group A, for example) and
#' see how well group A does against what the model would predict. Ranking each
#' of the groups this way provides a sense of which group's doing best in
#' terms of a particular measure.
#' @import caret
#' @import ranger
#' @param df Dataframe whose columns are used for calc.
#' @param predicted.col Column that you want to predict.
#' @param group.col Column that we'll use to differentiate
#' @param impute Set all-column imputation to F or T.
#'
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
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
#' set.seed(43)
#' o <- RiskAdjustedComparisons$new(df = totaldf,
#'                                  group.col = 'Gender',
#'                                  #grain.col = OPTIONAL/ENCOURAGED
#'                                  predicted.col = 'SalariedFlag',
#'                                  impute = TRUE)
#'
#' o$compare(debug=TRUE)
#'
#' @export

RiskAdjustedComparisons <- R6Class("RiskAdjustedComparisons",
  public = list(

    # Add attributes here
    df = NA,
    predicted.col = NA,
    group.col = NA,
    grain.col = NA,
    dfTrain = NA,
    dfTest = NA,


  initialize = function(df,
                        predicted.col,
                        group.col,
                        impute,
                        grain.col = "",
                        debug=FALSE) {
    # Clean and impute data

    if (length(ReturnColsWithMoreThanFiftyFactors(df))>0){
      message('The following columns in the data frame have more than fifty factors:')
      message(paste(shQuote(ReturnColsWithMoreThanFiftyFactors(df)), collapse=", "))
      message('This drastically reduces performance. Consider combining these factors into a new column with fewer factors.')
    }

    if (isTRUE(debug)) {
      print('Entire data set at the top of the constructor')
      print(str(df))
    }

    # Convert to data.frame (in case of data.table)
    # This also converts chr cols to (needed) factors
    df <- as.data.frame(unclass(df))

    if (isTRUE(debug)) {
      print('Entire data set after converting to df and chr to factor')
      print(str(df))
    }

    if (isTRUE(impute)) {
      df[] <- lapply(df, ImputeColumn)

      if (isTRUE(debug)) {
        print('Entire data set after imputation')
        print(str(df))
      }

    } else {
      if (isTRUE(debug)) {
        print(paste0("Rows in data set before removing rows with NA's: ",
                     nrow(df)))
      }

      # Remove rows with any NA's
      df = na.omit(df)

      if (isTRUE(debug)) {
        print(paste0("Rows in data set after removing rows with NA's: ",
                     nrow(df)))
        print("Entire data set after removing rows with NA's")
        print(str(df))
      }
    }

    # Remove columns that are only NA
    df <- df[,colSums(is.na(df)) < nrow(df)]

    # Remove date columns
    datelist = grep("DTS$", colnames(df))
    if (length(datelist) > 0) {
      df = df[, -datelist]
    }

    if (isTRUE(debug)) {
      print('Entire data set after removing cols with DTS (ie date cols)')
      print(str(df))
      print('Now going to remove zero-var cols...')

    }

    # If grain.col is specified, remove this col
    if (nchar(grain.col) != 0) {
      df[[grain.col]] <- NULL
    }

    if (isTRUE(debug) && nchar(grain.col) != 0) {
      print('Entire data set after separating out grain col')
      print(str(df))
      print('Now splitting training set from validation set')
    }

    self$df <- df
    self$predicted.col <- predicted.col
    self$group.col <- group.col
    self$grain.col <- grain.col
  },

  compare = function(debug=FALSE) {

    # Pre-create empty vectors
    group.by.list <- vector('character')
    comparative.performance <- vector('character')

    for (j in unique(self$df[[self$group.col]])) {
      group.by.list <- c(group.by.list, j)

      # Just grab rows corresponding to a particular category in the factor col
      dftemp <- self$df[self$df[[self$group.col]] == j,]

      trainIndex = createDataPartition(y = dftemp[[self$predicted.col]],
                                       p = 0.8,
                                       list = FALSE, times = 1)


      self$dfTrain = dftemp[ trainIndex,]
      self$dfTest  = dftemp[-trainIndex,]

      grid <- data.frame(.mtry = floor(sqrt(ncol(self$dfTrain))))

      train.control <- trainControl(
        method = "none",
        number = 1,
        verboseIter = if (isTRUE(debug)) TRUE else FALSE,
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )

      fit.rf = train(
        x = self$dfTrain[ ,!(colnames(self$dfTrain) == self$predicted.col)],
        y = factor(self$dfTrain[[self$predicted.col]]),
        method = "ranger",
        importance = 'impurity',
        metric = "ROC",
        num.trees = 200,
        tuneGrid = grid,
        trControl = train.control
      )

      predicted = predict(object = fit.rf,
                            newdata = self$dfTest,
                            type = 'raw')

      # Performed above or below predicted (neg is bad performance)
      diff <- (ifelse(self$dfTest[[self$predicted.col]] == 'Y', 1, 0)
              - ifelse(predicted == 'Y', 1, 0))

      comparative.performance <- c(comparative.performance, sum(diff))

       # Calculate mean-centered prediction error
      comparative.performance <- (as.numeric(comparative.performance) -
                                 (mean(as.numeric(comparative.performance))))

      dfreturn <- data.frame(group.by.list, comparative.performance)
    }

    print("Finished calculating your risk-adjusted comparison")
    print('Note that positive values denote performance above expected:')
    print(dfreturn)

    return(invisible(dfreturn))

  }
 )
)
