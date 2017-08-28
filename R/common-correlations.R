#' @title
#' Correlation analysis on an input table over all numeric columns
#'
#' @description Calculate correlations between every numeric column in a table
#' @param df A data frame
#' @return A data frame with column names and corresponding correlations
#' with the target column
#'
#' @importFrom stats cor cor.test
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,2,3,4,5,6),
#' b=c(6,5,4,3,2,1),
#' c=c(3,4,2,1,3,5),
#' d=c('M','F','F','F','M','F')) #<- is ignored
#'
#' dfResult <- calculateAllCorrelations(df)
#' dfResult
calculateAllCorrelations <- function(df) {
  dfResult <- cor(df[sapply(df, is.numeric)])
  dfResult
}

#' @title
#' Correlation analysis on an input table, focusing on one target variable
#'
#' @description Calculates correlations between each numeric column in a table
#' and and a target column
#' @param df A data frame
#' @param targetCol Name of target column against which correlations will be
#' calculated
#' @return A data frame with column names and corresponding correlations and
#' p-values with the target column
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,2,3,4,5,6),
#' b=c(6,5,4,3,2,1),
#' c=c(3,4,2,1,3,5),
#' d=c('M','F','F','F','M','F')) #<- is ignored
#'
#' dfResult <- calculateTargetedCorrelations(df=df,targetCol='c')
#' dfResult
calculateTargetedCorrelations <- function(df, targetCol) {
  if (!is.numeric(df[[targetCol]])) {
    stop("Your target column must be numeric")
  }
  
  # Initialize variable, since we will iterate
  pValue <- vector("numeric")
  
  # Pull only numeric columns
  nums <- sapply(df, is.numeric)
  df <- df[, nums]
  
  colList <- names(df)
  # Trim list of col names, so target doesn't check against itself
  colList <- colList[colList != targetCol]
  
  # Make list of correlations
  cor <- cor(as.matrix(df[[targetCol]]), as.matrix(df[, !(colnames(df) == targetCol)]))
  
  # Make list of corr-related p-values
  for (i in colList) {
    pValue <- c(pValue, cor.test(df[[targetCol]], df[, i])$p.value)
  }
  
  dfResult <- data.frame(t(cor), pValue)
  # Change name of corr col
  names(dfResult)[names(dfResult) == "t.cor."] <- "correlation"
  
  # Change row name to actual col
  dfResult <- cbind(column = rownames(dfResult), dfResult)
  rownames(dfResult) <- NULL
  
  colnames(dfResult) <- c("Column", "Correlation", "PValue")
  
  dfResult
}