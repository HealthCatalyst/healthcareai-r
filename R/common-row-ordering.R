#' @title
#' Order the rows in a data frame by date
#'
#' @description Returns a data frame that's ordered by its date column
#' @param df A data frame
#' @param dateCol Name of column in data frame that contains dates
#' @param descending Boolean for whether the output should be in descending order
#' @return A data frame ordered by date column
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(date=c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
#'                 a=c(1,2,3,4))
#' dfResult <- orderByDate(df,'date', descending=FALSE)
#' head(dfResult)
orderByDate <- function(df, dateCol, descending = FALSE) {
  df[[dateCol]] <- as.POSIXct(df[[dateCol]], truncated = 5)
  # Drop equals FALSE so that one column data frames are not converted to arrays
  if (descending == FALSE) {
    dfResult <- df[order(df[[dateCol]]), , drop = FALSE]
  } else {
    dfResult <- df[rev(order(df[[dateCol]])), , drop = FALSE]
  }
  dfResult
}