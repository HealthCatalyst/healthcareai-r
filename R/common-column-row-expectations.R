

#' @title
#' Check if a vector has only two unique values.
#'
#' @description Check if a vector is binary (not counting NA's)
#' @param v A vector, or column of values
#' @return A boolean
#'
#' @export
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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