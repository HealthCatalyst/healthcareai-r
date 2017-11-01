

#' @title
#' Check if a vector has only two unique values.
#'
#' @description Check if a vector is binary (not counting NA's)
#' @param v A vector, or column of values
#' @return A boolean
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' isBinary(c(1,2,NA))
#' isBinary(c(1,2,3))
isBinary <- function(v) {
  x <- unique(v)
  boolResult <- length(x) - sum(is.na(x)) == 2L
  boolResult
}

#' Remove columns from a data frame when those columns have the same values in
#' each row
#'
#' @description Remove columns from a data frame when all of their rows are the
#' same value (after removing NA's)
#' @param df A data frame
#' @return A data frame with zero-variance columns removed
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,1,1),
#'                 b=c('a','b','b'),
#'                 c=c('a','a','a'),
#'                 d=c(NA,'1',NA))
#' dfResult <- removeColsWithAllSameValue(df)
#' head(dfResult)
removeColsWithAllSameValue <- function(df) {
  dfResult <- df[sapply(df, function(x) length(unique(x[!is.na(x)])) > 1)]
  if (ncol(dfResult) == 0) {
    cat("All columns were removed.")
  }
  dfResult
}

#' @title
#' Remove rows where specified col is NA
#' @description Remove rows from a data frame where a particular col is NA
#' @param df A data frame to be altered
#' @param desiredCol A column name in the df (in ticks)
#' @return dfResult The input data frame with rows removed
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,2,3),b=c('Y','N',NA),c=c(NA,'Y','N'))
#' dfResult <- removeRowsWithNAInSpecCol(df,'b')
#' head(dfResult)
removeRowsWithNAInSpecCol <- function(df, desiredCol) {
  completeVec <- stats::complete.cases(df[[desiredCol]])
  dfResult <- df[completeVec, ]
  dfResult
}

#' @title
#' Return vector of columns in a data frame with greater than 50 categories
#'
#' @description Returns a vector of the names of the columns that have more than
#' 50 categories
#' @param df A data frame
#' @return A vector that contains the names of the columns with greater
#' than 50 categories
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#'                     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#'                  b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                      'o','p','q','r','s','t','u','v','w','x','y','z','aa',
#'                      'bb','cc','dd','ee','ff','gg','hh','ii','jj','kk','ll',
#'                      'mm','nn','oo','pp','qq','rr','ss','tt','uu','vv','ww',
#'                      'xx','yy'))
#' colList <- returnColsWithMoreThanFiftyCategories(df)
#' colList
returnColsWithMoreThanFiftyCategories <- function(df) {
  colList <- vector("character")
  for (columnName in names(df)) {
    if (nlevels(df[[columnName]]) > 50) {
      colList <- c(colList, columnName)
    }
  }
  colList
}

#' @title
#' Remove columns with DTS suffix
#' @description Remove columns with DTS in the suffix of the column name
#' @param df A data frame to be altered
#' @return The input data frame with DTS suffix columns removed
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(testDTS=c(1,2,3),b=c('Y','N',NA),c=c(NA,'Y','N'))
#' dfResult <- removeColsWithDTSSuffix(df)
#' head(dfResult)
removeColsWithDTSSuffix <- function(df){
  dateList <- grep("DTS$", colnames(df))
  if (length(dateList) > 0 &&
      (length(names(df)) - length(dateList)) != 1) {
      df[, -dateList]
  } else if (length(dateList) > 0 &&
             (length(names(df)) - length(dateList)) == 1)
  {
    df[-dateList]
  } else {df}
}

#' Check if a data frame only has numeric columns.
#' @description Check if a dataframe only has numeric columns
#' @param df A dataframe
#' @return A boolean
#' 
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples 
#' df <- data.frame(a=c(1,2,3),
#'                  b=c(NA,3,2))
#' isNumeric(df)
isNumeric <- function(df) {
  a <- sapply(df, is.numeric)
  at <- table(a)
  if (is.na(at["TRUE"])) return(FALSE)
  else if (at["TRUE"] == ncol(df)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Remove columns from a data frame that are only NA
#' 
#' @description Remove columns from a data frame when all of their rows are NA's
#' @param df A data frame
#' @return A data frame with columns of only NA's removed
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a=c(1,1,1),
#'                 b=c('a','b','b'),
#'                 c=c('a','NA','a'),
#'                 d=c(NA,NA,NA))
#' dfResult <- removeColsWithOnlyNA(df)
#' head(dfResult)
#' 
removeColsWithOnlyNA <- function(df) {
  name <- names(df)[colSums(is.na(df)) < nrow(df)]
  dfres <- df[,colSums(is.na(df)) < nrow(df)]
  dfres <- as.data.frame(dfres)
  if (ncol(dfres) == 0) {
    cat("All columns were removed.")
  }
  names(dfres) <- name
  dfres
}

#' @title
#' Tests whether predictedCol is Y/N. Allows for NAs to be present.
#'
#' @description Returns a logical, TRUE or FALSE, depending on what is
#' contained in the vector.  If any NAs are present in the vector, they will be
#' removed later on in development with removeRowsWithNAInSpecCol.
#' @param x A data frame column, matrix column, or vector
#' @return A boolean
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
#' dat2 <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
#' dat3 <- data.frame(a = c('Y', 'N', 'Y', 'N'))
#' dat4 <- data.frame(a = c('Y', 'N', 'Y', 'N', NA))
#' isTargetYN(dat[, 1])
#' isTargetYN(dat2[, 1])
#' isTargetYN(dat3[, 1])
#' isTargetYN(dat4[, 1])
#'
isTargetYN <- function(x) {
  NAremoved <- subset(x, !is.na(x))
  boolResult <- all(NAremoved %in% c('Y', 'N'))
  boolResult
}
