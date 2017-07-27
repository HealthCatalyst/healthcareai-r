#' @title
#' Last observation carried forward
#'
#' @description
#' Carries the last observed value forward for all columns in a data.table
#' grouped by an id.
#'
#' @param df data frame sorted by an ID column and a time or sequence number
#' column.
#' @param id A column name (in ticks) in df to group rows by.
#' @return A data frame where the last non-NA values are carried forward
#' (overwriting NAs) until the group ID changes.
#'
#' @import data.table
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(personID=c(1,1,2,2,3,3,3),
#'                 wt=c(.5,NA,NA,NA,.3,.7,NA),
#'                 ht=c(NA,1,3,NA,4,NA,NA),
#'                 date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
#'                        '01/01/2015','01/15/2015','01/30/2015'))
#'
#' head(df,n=7)
#'
#' dfResult <- groupedLOCF(df, 'personID')
#'
#' head(dfResult, n = 7)
groupedLOCF <- function(df, id) {
  # Note that the object that results acts as both a data frame and datatable
  df <- data.table::setDT(df)
  
  # Create a vector of booleans where each element is mapped to a row in the
  # data.table.  Each value is FALSE unless the corresponding row is the first row
  # of a person.  In other words, each TRUE represents a change of PersonID in the
  # data.table.
  changeFlags <- c(TRUE, get(id, df)[-1] != get(id, df)[-nrow(df)])
  
  # A helper that finds the last non-NA value for a given column x in df.
  locf <- function(x) x[cummax(((!is.na(x)) | changeFlags) * seq_len(nrow(df)))]
  
  # By avoiding using the 'by' operator of data.table, we're reducing the number of
  # calls from (N rows / P people) * C columns to just C columns; this is just once
  # for each column in the data.table.
  dfResult <- df[, lapply(.SD, locf)]
  dfResult
}

#' @title
#' Perform imputation on a dataframe
#'
#' @description This class performs imputation on a vector. For numeric vectors
#' the vector-mean is used; for factor columns, the most frequent value is used.
#' @param df A vector, or column of values with NAs.
#' @return A vector, or column of values now with no NAs
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' # Impute a single column
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' df <- df[,1]
#' out <- imputeDF(df)
#' dfOut <- out$df # imputed data frame
#' imputeVals <- out$imputeVals # imputed values
#' print(dfOut)

#' # Impute an entire data frame
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' out <- imputeDF(df)
#' dfOut <- out$df # imputed data frame
#' imputeVals <- out$imputeVals # imputed values
#' print(dfOut)
#' 
#' # To impute using your own values (one per column)
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' myValues <- list(2, 'Y', 26.5, 'N') 
#' out <- imputeDF(df, myValues)
#' dfOut <- out$df # imputed data frame
#' print(dfOut)

imputeDF <- function(df, imputeVals = list()) {
  # Use separate functions to make applying easier in deploy
  # This function finds the value to impute for a column
  getImputeValuesColumn <- function(col) {
    if (is.numeric(col)) {
      value <- mean(col, na.rm = TRUE) # this is where the method could change
    } else {
      value <- names(which.max(table(col)))
    }
    return(value)
  }
  
  # This function applies the value to a column
  applyImputeValuesColumn <- function(col, val) {
    col[is.na(col)] <- val
    return(col)
  }

  # Check to make sure that df is a dataframe
  if (!(is.data.frame(df))) {
    stop('df must be a dataframe.')
  }

  # Calculate imputation values if needed
  if (length(imputeVals) == 0) {
    # Calculate values
    imputeVals <- lapply(df, getImputeValuesColumn)
  } else {
    if (!(is.list(imputeVals))){
      stop("imputeValues must be a list.")
    }
    cat('Using supplied values for imputation.', '\n')
    if (length(imputeVals) != ncol(df)) {
      stop('Your dataframe must have the same number of columns as your provided list!')
    }
  }

  # Apply imputation values
  df[] <- lapply(1:ncol(df), function(x,y) applyImputeValuesColumn(df[,x], imputeVals[[x]]))
  return(list(df=df, imputeVals=imputeVals))
}
