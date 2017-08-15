#' @title
#' Find any columns that have a trend above a particular threshold
#' @description
#' Search within subgroups and find trends that are six months of longer.
#' @param df A data frame
#' @param dateCol A string denoting the date column
#' @param groupbyCol A string denoting the column by which to group
#' @return A data frame containing the dimensional attribute (ie gender), the
#' subset the data was grouped by (ie M/F), the measures that had trends
#' (ie, mortality or readmission), and the ending month.
#'
#' @importFrom stats aggregate formula
#' @importFrom utils tail
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'dates <- c(as.Date('2012-01-01'),as.Date('2012-01-02'),as.Date('2012-02-01'),
#'           as.Date('2012-03-01'),as.Date('2012-04-01'),as.Date('2012-05-01'),
#'           as.Date('2012-06-01'),as.Date('2012-06-02'))
#'y1 <- c(0,1,2,6,8,13,14,16)               # large positive
#'y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1.5)     # small positive
#'y3 <- c(1,0,-2,-2,-4,-5,-7,-8)            # big negative
#'y4 <- c(.5,0,-.5,-.5,-.5,-.5,-.6,0)       # small negative
#'gender <- c('M','F','F','F','F','F','F','F')
#'df <- data.frame(dates,y1,y2,y3,y4,gender)
#'
#'dfResult <- findTrends(df = df,
#'                       dateCol = 'dates',
#'                       groupbyCol = 'gender')
findTrends <- function(df, dateCol, groupbyCol) {
  df$year <- as.POSIXlt(df[[dateCol]])$year + 1900
  df$month <- as.POSIXlt(df[[dateCol]])$mo + 1
  
  df[[dateCol]] <- NULL
  
  df <- aggregate(formula(paste0(".~", groupbyCol, "+year+month")), data = df,
                  FUN = sum)
  
  df <- df[with(df, order(year, month)), ]
  
  # TODO: alter this last month dynamically when we search over all time
  finalYrMonth <- paste0(month.abb[df$month[length(df$month)]], "-", df$year[length(df$year)])
  
  # Pre-create empty vectors
  metricTrendList <- vector("character")
  aggregatedColList <- vector("character")
  
  # Create list that doesn't have cols we aggregated by
  colIterList <- names(df)
  remove <- c(groupbyCol, "year", "month")
  colIterList <- colIterList[!colIterList %in% remove]
  
  # If the last six values are monotonically increasing, add col name to list
  for (j in unique(df[[groupbyCol]])) {
    # Just grab rows corresponding to a particular category in the factor col
    dfTemp <- df[df[[groupbyCol]] == j, ]
    
    cat("df after grouping and focusing on one category in group col:", '\n')
    print(tail(dfTemp, n = 6))
    
    # Iterate over all columns except for cols that we aggregated by
    for (i in colIterList) {
      if (is.numeric(dfTemp[[i]])) {
        # Check if last six values are monotonically increasing
        n <- nrow(dfTemp)
        if (n > 5) {
          # TODO: make this check into a function
          checkIncr <- all(dfTemp[[i]][(n - 5):n] == cummax(dfTemp[[i]][(n -
                                                                           5):n]))
          checkDecr <- all(dfTemp[[i]][(n - 5):n] == cummin(dfTemp[[i]][(n -
                                                                           5):n]))
          if (isTRUE(checkIncr) || isTRUE(checkDecr)) {
            # If true, append col names to list to output
            aggregatedColList <- c(aggregatedColList, j)
            metricTrendList <- c(metricTrendList, i)
          }
        }
      }
    }
  }
  
  if (length(metricTrendList) == 0) {
    cat("No trends of sufficient length found")
    return()
  } else {
    dfResult <- data.frame(groupbyCol, aggregatedColList, metricTrendList, finalYrMonth)
    colnames(dfResult) <- c("DimAttribute", "GroupBy", "MeasuresTrending", "FinalDate")
    cat("Trends were found:", '\n')
    print(dfResult)
  }
}