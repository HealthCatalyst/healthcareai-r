#' @title
#' Recalculate predicted value based on alternate scenarios
#'
#' @description After getting alternate features via calculateSDChanges
#' recalculate predicted values for each row in df.
#' @param df Data frame from which we'll calculate alternate predictions
#' @param modelObj Object representing the model that is used for predictions
#' @param type String representing which type of model is used
#' @param outVectorAppend Optional list of values that we'll append predictions
#' to. If not used, then a new vector is created.
#' @param removeCols Optional list of column names to remove before calculating
#' alternate predictions.
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' library(caret)
#' df <- data.frame(a=c(1,2,3,1),
#'                  b=c('m','f','m','m'),
#'                  c=c(0.7,1.4,2.4,2.0),
#'                  d=c(100,250,200,150))
#'
#' # Get alternate feature scenarios
#' dfResult <- calculateSDChanges(df=df,
#'                                rowNum=2,
#'                                sizeOfSDPerturb = 0.5,
#'                                numColLeaveOut='d')
#'
#' y <- c('y','n','y','n')
#'
#' # Train model on original data frame
#' glmOb <- train(x = df,y = y,method = 'glm',family = 'binomial')
#'
#' outList <- calulcateAlternatePredictions(df=dfResult,
#'                                          modelObj=glmOb,
#'                                          type='lasso')
#' outList
calulcateAlternatePredictions <- function(df,
                                          modelObj,
                                          type,
                                          outVectorAppend=NULL,
                                          removeCols=NULL){
  
  if (type != 'rf' && type != 'lasso' && type != 'lmm') {
    stop('Your type of model has to be rf, lasso, or lmm')
  }
  
  if (missing(outVectorAppend)) {
    outVectorAppend <- list()
  }
  
  if (!missing(removeCols)) {
    df <- df[, !(colnames(df) %in% removeCols)]
  }
  
  # For each row in df, calculate new prediction and append to vector
  for (i in 1:nrow(df)) {
    
    if (type == 'rf') {
      outTemp <- stats::predict(object = modelObj,newdata = df[i,],type = 'prob')
      outVectorAppend <- c(outVectorAppend,outTemp[2])
      
    } else if (type == 'lasso') {
      outTemp <- stats::predict(object = modelObj,newdata = df[i,],type = 'prob')
      outVectorAppend <- c(outVectorAppend,outTemp[2])
      
    } else if (type == 'lmm') {
      outTemp <- stats::predict(object = modelObj,newdata = df[i,],type = 'prob')
      outVectorAppend <- c(outVectorAppend,outTemp[2])
    }
  }
  outVectorAppend
}

#' @title
#' Calculate std deviation up/down for each numeric field in row
#'
#' @description Add/subtract each numeric col (for each row) by std dev, such
#' that we have a new alternate data frame
#' @param dfOriginal Data frame from Error in as.double(y) : 
#' cannot coerce type 'S4' to vector of type 'double' which we'll draw a row for alt-scenarios
#' @param rowNum Row in dfOriginal that we'll create alt-scenarios for
#' @param numColLeaveOut Numeric columns to leave out of alterlative scenarios
#' @param sizeOfSDPerturb Default is 0.5. Shrink or expand SD drop/addition
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,2,3),
#'                  b=c('m','f','m'),
#'                  c=c(0.7,1.4,2.4),
#'                  d=c(100,250,200),
#'                  e=c(400,500,505))
#'
#'
#' dfResult <- calculateSDChanges(dfOriginal = df,
#'                                rowNum = 2,
#'                                numColLeaveOut = c('d','e'),
#'                                sizeOfSDPerturb = 0.5)
#' dfResult
calculateSDChanges <- function(dfOriginal,
                               rowNum,
                               numColLeaveOut,
                               sizeOfSDPerturb=0.5) {
  
  df1 <- dfOriginal[rowNum,]
  
  # Find list of numeric cols in rowNum
  colIterList <- names(df1)
  numericList <- character()
  
  for (v in colIterList) {
    if (is.numeric(df1[[v]])) {
      numericList <- c(numericList,v)
    }
  }
  
  # Remove any undesired num columns from being considered (ie, PatientID, etc)
  if (!missing(numColLeaveOut)) {
    numericList <- numericList[!numericList %in% numColLeaveOut]
  }
  
  # For length of this numeric col list, create that many new rows * 2
  # This way, we can replace the up and down value into the pre-existing data
  # If we hit the pop max/min, we'll remove extra rows below the for loop
  dfAlternative <- df1[rep(seq_len(nrow(df1)), each = length(numericList)*2),]
  dfAlternative
  
  # Add column denoting which column is being perturbed
  dfAlternative$AlteredCol <- NULL
  
  # For each numeric col, calculate up/down and push into data frame
  # There's a new row for plus and then a new row for minus
  j <- 1
  for (i in numericList) {
    tempAdd <- dfAlternative[j,i] +
      (stats::sd(dfOriginal[,i]) * sizeOfSDPerturb)
    
    # Check if adding half SD puts person over max of entire pop
    if (tempAdd <= max(dfOriginal[,i])) {
      # If this keeps them within pop, let's consider this scenario
      dfAlternative[j,i] <- tempAdd
      # Name which column was altered for this row
      dfAlternative[j,'AlteredCol'] <- i
      j <- j + 1
    }
    
    tempSubtract <- dfAlternative[j,i] -
      (stats::sd(dfOriginal[,i]) * sizeOfSDPerturb)
    
    # Check if subtracting half SD puts person under min of entire pop
    if (tempSubtract >= min(dfOriginal[,i])) {
      # If this keeps them within pop, let's consider this scenario
      dfAlternative[j,i] <- tempSubtract
      # Name which column was altered for this row
      dfAlternative[j,'AlteredCol'] <- i
      j <- j + 1
    }
  }
  
  outOfBoundsCount <- nrow(dfAlternative) - (j - 1)
  
  # Trim extra rows, if max or min were reached immediately above
  if (outOfBoundsCount > 0) {
    # Grab all but last outOfBoundsCount # of rows
    dfAlternative <- utils::head(dfAlternative, -outOfBoundsCount)
  }
  
  dfAlternative
}

#' @title
#' Find most biggest drop in predictive probability across alternate features
#'
#' @description Compare each alternate probability prediction and determine
#' which ones are lowest compared to the original; return the top three column
#' names that lead to the biggest drop and their target value.
#' @param dfAlternateFeat Data frame of alternate feature values
#' @param originalRow Row from original data frame upon which alternates are
#' based
#' @param predictionVector List of alternate predictions
#' @param predictionOriginal Scalar representing original prediction for
#' row, without alternative scenario
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' library(caret)
#' df <- data.frame(a = c(1,2,3,1),
#'                  b = c('m','f','m','m'),
#'                  c = c(0.7,1.4,2.4,2.0),
#'                  d = c(100,250,200,150))
#'
#' y <- c('y','n','y','n')
#'
#' dfAlt <- calculateSDChanges(df = df,
#'                             rowNum = 2,
#'                             sizeOfSDPerturb = 0.5,
#'                             numColLeaveOut = 'd')
#'
#' glmOb <- train(x = df,y = y,method = 'glm',family = 'binomial')
#'
#' originalPred <- predict(object = glmOb,
#'                         newdata = df[4,],
#'                         type = 'prob')
#'
#' alternatePred <- calulcateAlternatePredictions(df = dfAlt,
#'                                                modelObj = glmOb,
#'                                                type = 'lasso',
#'                                                removeCols = 'AlteredCol')
#'
#' dfResult <- findBestAlternateScenarios(dfAlternateFeat = dfAlt,
#'                                        originalRow = df[4,],
#'                                        predictionVector = as.numeric(alternatePred),
#'                                        predictionOriginal = originalPred[[2]])
#'
#' dfResult
findBestAlternateScenarios <- function(dfAlternateFeat,
                                       originalRow,
                                       predictionVector,
                                       predictionOriginal) {
  
  # Initialize vectors
  predDiff <- numeric()
  colChanged <- character()
  alternateValue <- numeric()
  
  # For each Row in alternate scenario df, calculate distance
  # between original prediction and alternate prediction
  for (i in 1:nrow(dfAlternateFeat)) {
    # Finding drop from original probability (so drop is a positive number)
    tempDiff <- predictionOriginal - predictionVector[i]
    predDiff <- c(predDiff,tempDiff)
    
    # Finding col that had been alternated
    colChanged <- c(colChanged,dfAlternateFeat[i,'AlteredCol'])
    
    # Finding alternate value for altered col
    # Double brackets make resulting value a scalar instead of list
    tempColChanged <- colChanged[i]
    
    alternateValue <- c(alternateValue, dfAlternateFeat[[i,tempColChanged]])
  }
  
  # Find index of greatest drop in predicted probability
  orderedPredDiffIndex <- order(-predDiff)
  
  # Order by greatest drop in predicted probability
  orderedProbDrop <- predDiff[orderedPredDiffIndex]
  cat('orderedProbDrop', '\n')
  cat(orderedProbDrop, '\n')
  
  # Find associated features that were changed
  orderedColChanged <- colChanged[orderedPredDiffIndex]
  cat('orderedColChanged', '\n')
  cat(orderedColChanged, '\n')
  
  # Find associated alternate values
  orderedAlternateValue <- alternateValue[orderedPredDiffIndex]
  cat('orderedAlternateValue', '\n')
  cat(orderedAlternateValue, '\n')
  
  dfOptResult <- data.frame(orderedProbDrop,
                            orderedColChanged,
                            orderedAlternateValue)
  
  dfOptResult
}