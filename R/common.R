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
#' Convert datetime column into dummy columns
#'
#' @description
#' Convert datetime column into dummy columns of day, hour, etc, such that one
#' can use daily and seasonal patterns in their model building.
#'
#' @param df A data frame. Indicates the datetime column.
#' @param dateTimeCol A string. Column name in df that will be converted
#' into several columns.
#' @param depth A string. Specifies the depth with which to expand extra columns
#' (starting with a year column). 'd' expands to day, 'h' expands to hour
#' (default), m' expands to minute, and 's' expands to second.
#' @param returnDtCol A boolean. Return the original dateTimeCol with
#' the modified data frame?
#' @return A data frame which now includes several columns based on time
#' rather than just one datetime column
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' dtCol <- c('2001-06-09 12:45:05','2002-01-29 09:30:05','2002-02-02 07:36:50',
#'           '2002-03-04 16:45:01','2002-11-13 20:00:10','2003-01-29 07:31:43',
#'           '2003-07-07 17:30:02','2003-09-28 01:03:20')
#' y1 <- c(.5,1,3,6,8,13,14,1)
#' y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1)
#' df <- data.frame(dtCol,y1,y2)
#'
#' df <- convertDateTimeColToDummies(df, 'dtCol')
#' head(df)

convertDateTimeColToDummies <- function(df, dateTimeCol, depth = "h", returnDtCol = FALSE) {
  if (depth == "d") {
    df[[dateTimeCol]] <- as.POSIXct(df[[dateTimeCol]])
    df$year <- as.POSIXlt(df[[dateTimeCol]])$year + 1900
    df$month <- as.POSIXlt(df[[dateTimeCol]])$mo + 1
    df$weekOfYear <- strftime(df[[dateTimeCol]], format = "%W")
    df$dayOfMonth <- as.POSIXlt(df[[dateTimeCol]])$mday
    df$dayOfWeek <- as.POSIXlt(df[[dateTimeCol]])$wday + 1

  } else if (depth == "h") {
    df[[dateTimeCol]] <- as.POSIXct(df[[dateTimeCol]])
    df$year <- as.POSIXlt(df[[dateTimeCol]])$year + 1900
    df$month <- as.POSIXlt(df[[dateTimeCol]])$mo + 1
    df$weekOfYear <- strftime(df[[dateTimeCol]], format = "%W")
    df$dayOfMonth <- as.POSIXlt(df[[dateTimeCol]])$mday
    df$dayOfWeek <- as.POSIXlt(df[[dateTimeCol]])$wday + 1
    df$hour <- as.POSIXlt(df[[dateTimeCol]])$hour

  } else if (depth == "m") {
    df[[dateTimeCol]] <- as.POSIXct(df[[dateTimeCol]])
    df$year <- as.POSIXlt(df[[dateTimeCol]])$year + 1900
    df$month <- as.POSIXlt(df[[dateTimeCol]])$mo + 1
    df$weekOfYear <- strftime(df[[dateTimeCol]], format = "%W")
    df$dayOfMonth <- as.POSIXlt(df[[dateTimeCol]])$mday
    df$dayOfWeek <- as.POSIXlt(df[[dateTimeCol]])$wday + 1
    df$hour <- as.POSIXlt(df[[dateTimeCol]])$hour
    df$min <- as.POSIXlt(df[[dateTimeCol]])$min

  } else if (depth == "s") {
    df[[dateTimeCol]] <- as.POSIXct(df[[dateTimeCol]])
    df$year <- as.POSIXlt(df[[dateTimeCol]])$year + 1900
    df$month <- as.POSIXlt(df[[dateTimeCol]])$mo + 1
    df$weekOfYear <- strftime(df[[dateTimeCol]], format = "%W")
    df$dayOfMonth <- as.POSIXlt(df[[dateTimeCol]])$mday
    df$dayOfWeek <- as.POSIXlt(df[[dateTimeCol]])$wday + 1
    df$hour <- as.POSIXlt(df[[dateTimeCol]])$hour
    df$min <- as.POSIXlt(df[[dateTimeCol]])$min
    df$Sec <- as.POSIXlt(df[[dateTimeCol]])$sec
  }

  if (isTRUE(!returnDtCol)) {
    df[[dateTimeCol]] <- NULL
  }
  df
}

#' @title
#' Perform imputation on a vector
#'
#' @description This class performs imputation on a vector. For numeric vectors
#' the vector-mean is used; for factor columns, the most frequent value is used.
#' @param v A vector, or column of values with NAs.
#' @return A vector, or column of values now with no NAs
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' # For a numeric vector
#' vResult <- imputeColumn(c(1,2,3,NA))
#'
#' # For a factor vector
#' vResult <- imputeColumn(c('Y','N','Y',NA))
#'
#' # To use this function on an entire data frame:
#' df <- data.frame(a=c(1,2,3,NA),
#'                 b=c('Y','N','Y',NA))
#' df[] <- lapply(df, imputeColumn)
#' head(df)

imputeColumn <- function(v) {
  if (is.numeric(v)) {
    v[is.na(v)] <- mean(v, na.rm = TRUE)
  } else {
    v[is.na(v)] <- names(which.max(table(v)))
  }
  v
}

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
#' Pull data into R via an ODBC connection
#' @description Select data from an ODBC database and return the results as
#' a data frame.
#' @param connectionString A string specifying the driver, server, database,
#' and whether Windows Authentication will be used.
#' @param query The SQL query (in ticks or quotes)
#' @param randomize Boolean that dictates whether returned rows are randomized
#' @return df A data frame containing the selected rows
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' \donttest{
#' #### This example is specific to Windows and is not tested on CRAN 
#' connectionString <- '
#'   driver={SQL Server};
#'   server=localhost;
#'   database=SAM;
#'   trustedConnection=true
#'   '
#'
#' query <- '
#'   SELECT
#'     A1CNBR
#'   FROM SAM.dbo.HCRDiabetesClinical
#'   '
#'
#' df <- selectData(connectionString, query)
#' head(df)
#' }

selectData <- function(connectionString, query, randomize = FALSE) {
  if (isTRUE(randomize)) {
    orderPres <- grep("order", tolower(query))

    if (length(orderPres == 0)) {
      stop("You cannot randomize while using the SQL order keyword.")
    }

    query <- paste0(query, " ORDER BY NEWID()")
  }

  # TODO: if debug: cat(connectionString)
  cnxn <- RODBC::odbcDriverConnect(connectionString)

  # TODO: if debug: cat(query) 
  # TODO: if debug: time this operation and print time spent to pull data.
  df <- RODBC::sqlQuery(channel = cnxn, 
                        na.strings = c("NULL", "NA", ""), 
                        query = query)

  RODBC::odbcCloseAll()

  # Make sure there are enough rows to actually do something useful.
  if (is.null(nrow(df))) {
    cat(df)  # Print the SQL error, which is contained in df.
    stop("Your SQL contains an error.")
  }
  if (nrow(df) == 0) {
    cat("Too few rows returned from SQL: ")
    cat(nrow(df))
    cat(" rows returned.")
    cat("Adjust your query to return more data!")
  }
  # TODO: if debug: print the number of rows selected.
  df  # Return the selected data.
}


#' @title
#' Write data to database
#' @description Write data frame to database via ODBC connection
#' @param df A data frame being written to a database
#' @param server A string.
#' @param database A string.
#' @param schemaDotTable A string representing the destination schema and
#' table. Note that brackets aren't expected.
#' @return Nothing
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' \donttest{
#' #### This example is specific to Windows and is not tested. 
#' df <- data.frame(a=c(1,2,3),
#'                  b=c(2,4,6),
#'                  c=c('one','two','three'))
#'
#' writeData(df,'localhost','SAM','dbo.HCRWriteData')
#' }

writeData <- function(df, server, database, schemaDotTable) {
  # TODO: use sub function to remove brackets from schemaDotTable TODO: add
  # try/catch around sqlSave
  connectionString <- paste0("driver={SQL Server};
                             server=",
                             server, ";
                             database=", database, ";
                             trustedConnection=true")

  sqlCnxn <- RODBC::odbcDriverConnect(connectionString)

  # Save df to table in specified database
  out <- RODBC::sqlSave(channel = sqlCnxn, dat = df, tablename = schemaDotTable, append = T,
                 rownames = F, colnames = F, safer = T, nastring = NULL)

  # Clean up.
  RODBC::odbcCloseAll()

  if (out == 1) {
    print("SQL Server insert was successful")
  } else {
    print("SQL Server insert failed")
  }
}

#' @title
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
    message("All columns were removed.")
  }
  dfResult
}

#' @title
#' Find any columns that have a trend above a particular threshold
#' @description
#' Find numeric columns in data frame that have an absolute slope greater than
#' that specified via threshold argument.
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
#' @references \url{http://healthcare.ai}
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
#'dfResult

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

    print("df after grouping and focusing on one category in group col:")
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
    message("No trends of sufficient length found")
    return()
  } else {
    dfResult <- data.frame(groupbyCol, aggregatedColList, metricTrendList, finalYrMonth)
    colnames(dfResult) <- c("DimAttribute", "GroupBy", "MeasuresTrending", "FinalDate")
    print("Trends were found:")
    print(dfResult)
  }
}

#' @title
#' Order the rows in a data frame by date
#'
#' @description Returns a data frame that's ordered by its date column
#' @param df A data frame
#' @param dateCol Name of column in data frame that contains dates
#' @param descending Boolean for whether the output should be in descending order
#' @return A data frame ordered by date column
#'
#' @importFrom lubridate ymd_hms
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(date=c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
#'                 a=c(1,2,3,4))
#' dfResult <- orderByDate(df,'date', descending=FALSE)
#' head(dfResult)

orderByDate <- function(df, dateCol, descending = FALSE) {
  df[[dateCol]] <- lubridate::ymd_hms(df[[dateCol]], truncated = 5)
  #drop equals false so that one column data frames are not converted to arrays
  if (descending == FALSE) {
    dfResult <- df[order(df[[dateCol]]), , drop = FALSE]
  } else {
    dfResult <- df[rev(order(df[[dateCol]])), , drop = FALSE]
  }
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
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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
#'                 b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                     'o','p','q','r','s','t','u','v','w','x','y','z','aa','bb',
#'                     'cc','dd','ee','ff','gg','hh','ii','jj','kk','ll','mm','nn',
#'                     'oo','pp','qq','rr','ss','tt','uu','vv','ww','xx','yy'))
#' colList <- returnColsWithMoreThanFiftyCategories(df)

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
#' Calculates percentage of each column in df that is NULL (NA)
#'
#' @description Returns a vector with percentage of each column that is NULL
#' in the original data frame
#' @param df A data frame
#' @return A vector that contains the percentage of NULL in each column
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df = data.frame(a=c(1,2,NA,NA,3),
#'                 b=c(NA,NA,NA,NA,NA),
#'                 c=c(NA,NA,'F','M',NA))
#' colList = countPercentEmpty(df)
#' colList

countPercentEmpty <- function(df) {
  colList <- colMeans(is.na(df))
  colList
}

#' @title
#' Creates column based on days since first date
#'
#' @description Adds a new column to the data frame, which shows days since
#' first day in input column
#' @param df A data frame
#' @param dtCol A string denoting the date-time column of interest
#' @param returnDtCol A boolean. Return the original dtCol with the modified
#' data frame?
#' @return A data frame that now has a new column
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' dtCol = c('2001-06-09 12:45:05','2002-01-29 09:30:05','2002-02-02 07:36:50',
#' '2002-03-04 16:45:01','2002-11-13 20:00:10','2003-01-29 07:31:43',
#' '2003-07-07 17:30:02','2003-09-28 01:03:20')
#' y1 <- c(.5,1,3,6,8,13,14,1) # Not being used at all
#' df <- data.frame(dtCol, y1)
#' head(df)
#' dfResult <- countDaysSinceFirstDate(df, 'dtCol')
#' head(dfResult)

countDaysSinceFirstDate <- function(df, dtCol, returnDtCol = FALSE) {
  # Find first date in date list
  earliest <- df[[dtCol]][order(format(as.Date(df[[dtCol]]), "%y%m%d"))[1]]
  # Find diff between each date and first date
  dayDiff <- as.numeric(difftime(df[[dtCol]], earliest, units = "days"))
  # Make output col name include input name (in case of multiple uses)
  combinedName <- paste0(dtCol, "DaysSinceFirstDate")
  df[[combinedName]] <- dayDiff

  if (isTRUE(!returnDtCol)) {
    df[[dtCol]] <- NULL
  }
  df
}

#' @title
#' Plot ROCs from SupervisedModel classes
#'
#' @description Plot ROCs calculated by children classes of SupervisedModel
#' @param rocs A vector/array/list of ROC values
#' @param names A vector of algorithm/class names
#' @param legendLoc Location of the legend string to display
#'
#' @importFrom graphics legend title par
#' @importFrom utils head
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

plotROCs <- function(rocs, names, legendLoc) {
  # generate color vector TODO: auto generate these colors dynamically as rgb
  # values based on the length of rocs list
  colvec <- c("red", "blue", "green", "orange", "brown", "magenta")

  # plot ROCs
  rocIndex <- 1
  for (roc in rocs) {
    if (rocIndex == 1) {
      par(pty = "s")
      plot(x = roc@x.values[[1]],
           y = roc@y.values[[1]],
           col = colvec[rocIndex],
           mar = c(4, 4, 3, 2) + 0.1,
           type = 'l',
           main = "ROC",
           xlab = "False Positive Rate", ylab = "True Positive Rate")
      
    } else {
      par(pty = "s")
      par(new = TRUE) # lay second line over first
      plot(x = roc@x.values[[1]],
           y = roc@y.values[[1]], 
           col = colvec[rocIndex], 
           lty = 2,
           type = 'l',
           main = "ROC",
           xlab = "False Positive Rate", ylab = "True Positive Rate",
           yaxt = "n") # turn off extra y axis
    }
    rocIndex <- rocIndex + 1
  }
  # legend
  legend(x=legendLoc, names, cex = 0.8, col = colvec, lty = 1:2, inset = 0.1)
  return()
}

#' @title
#' Plot PR Curves from SupervisedModel classes
#'
#' @description Plot PRCurves calculated by children classes of SupervisedModel
#' @param PRCurves A vector/array/list of PR curves
#' @param names A vector of algorithm/class names
#' @param legendLoc Location of the legend string to display
#'
#' @importFrom graphics legend title
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

plotPRCurve <- function(PRCurves, names, legendLoc) {
  # generate color vector TODO: auto generate these colors dynamically as rgb
  # values based on the length of PRCurves list
  colvec <- c("red", "blue", "green", "orange", "brown", "magenta")
  
  # plot PRCurves
  prIndex <- 1
  for (pr in PRCurves) {
    if (prIndex == 1) {
      par(pty = "s")
      plot(x = pr@x.values[[1]],
           y = pr@y.values[[1]],
           col = colvec[prIndex],
           mar = c(4, 4, 3, 2) + 0.1,
           type = 'l',
           main = "PR Curve",
           xlab = "Recall", ylab = "Precision")
    } else {
      par(pty = "s")
      par(new = TRUE) # lay second line over first
      plot(x = pr@x.values[[1]],
           y = pr@y.values[[1]],
           col = colvec[prIndex], 
           lty = 2,
           type = 'l',
           main = "PR Curve",
           xlab = "Recall", ylab = "Precision",
           yaxt = "n") # turn off extra y axis
    }
    prIndex <- prIndex + 1
  }
  # legend
  legend(x = legendLoc, names, cex = 0.8, col = colvec, lty = 1:2, inset = 0.1)
  return()
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
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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
#' @references \url{http://healthcare.ai}
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
  print('orderedProbDrop')
  print(orderedProbDrop)

  # Find associated features that were changed
  orderedColChanged <- colChanged[orderedPredDiffIndex]
  print('orderedColChanged')
  print(orderedColChanged)

  # Find associated alternate values
  orderedAlternateValue <- alternateValue[orderedPredDiffIndex]
  print('orderedAlternateValue')
  print(orderedAlternateValue)

  dfOptResult <- data.frame(orderedProbDrop,
                            orderedColChanged,
                            orderedAlternateValue)

  dfOptResult
}

#' @title
#' Generate ROC or PR curve for a dataset.
#' @description Generates ROC curve and AUC for Sensitivity/Specificity or 
#' Precision/Recall.
#' @param predictions A vector of predictions from a machine learning model.
#' @param labels A vector of the true labels. Must be the same length as 
#' predictions.
#' @param aucType A string. Indicates AUC_ROC or AU_PR and can be "SS" or "PR". 
#' Defaults to SS.
#' @param plotFlg Binary value controlling plots. Defaults to FALSE (no).
#' @param allCutoffsFlg Binary value controlling list of all thresholds. 
#' Defaults to FALSE (no).
#' @return AUC: A number between 0 and 1. Integral AUC of chosen plot type.
#' @return IdealCutoffs: Array of cutoff and associated TPR/FPR or pre/rec.
#' @return Performance: ROCR performance class containing all ROC information.
#'
#' @import ROCR
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' # generate data
#' # example probablities
#' df <- data.frame(a = rep( seq(0,1,by=0.1), times=9))
#' # example ground truth values
#' df[,'b'] <- (runif(99,0,1)*df[,'a']) > 0.5
#' 
#' # prepare vectors
#' pred <- df[,'a']
#' labels <- df[,'b']
#' 
#' # generate the AUC
#' auc = generateAUC(predictions = pred, 
#'                   labels = labels,
#'                   aucType = 'SS',
#'                   plotFlg = TRUE,
#'                   allCutoffsFlg = TRUE)
#' 
generateAUC <- function(predictions, 
                        labels, 
                        aucType='SS', 
                        plotFlg=FALSE, 
                        allCutoffsFlg=FALSE) {
  
  # Error check for uneven length predictions and labels
  if (length(predictions) != length(labels)) {
    stop('Data vectors are not equal length!')
  }
  
  aucType <- toupper(aucType)
  
  # default to SS if something else is entered
  if (aucType != 'SS' && aucType != 'PR') {
    print('Drawing ROC curve with Sensitivity/Specificity')
    aucType <- 'SS'
  }
  
  # generate ROC data
  pred = ROCR::prediction(predictions, labels)
  
  # get performance and AUC from either curve type
  if (aucType == 'PR') {
    perf <- ROCR::performance(pred, "prec", "rec")
    x <- as.numeric(unlist(perf@x.values))
    y <- as.numeric(unlist(perf@y.values))
    
    # Convert NaNs to zero
    y[ is.nan(y) ] <- 0
    # From: http://stackoverflow.com/a/30280873/5636012
    area <- sum(diff(x) * (head(y,-1) + tail(y,-1)))/2
    
    # print threshholds and AUC
    cat(sprintf("Area under the PR curve is: %0.2f \n", area))

  } else if (aucType == 'SS') {
    perf <- ROCR::performance(pred, "tpr","fpr")
    perf.auc <- ROCR::performance(pred, measure = "auc")
    area <- perf.auc@y.values[[1]]
    
    # print AUC
    cat(sprintf("Area under the ROC curve is: %0.2f \n", area)) 
  }
  
  if (aucType == 'SS') {
    titleTemp <- 'ROC'
    xtitle <- 'False Positive Rate'
    ytitle <- 'True Positive Rate'
  } else {
    titleTemp <- 'PR Curve'
    xtitle <- 'Recall'
    ytitle <- 'Precision'
  }
  
  # plot AUC 
  if (isTRUE(plotFlg)) {
    plot(x = perf@x.values[[1]],
         y = perf@y.values[[1]],
         type = 'l',
         col = "blue", 
         lwd = 2, 
         main = titleTemp,
         xlab = xtitle,
         ylab = ytitle)
  }
  
  # get ideal cutoff values.
  IdealCuts <- getCutOffs(perf = perf, 
                          aucType = aucType, 
                          allCutoffsFlg = allCutoffsFlg)
  
  return(list('AUC' = area, 'IdealCutoffs' = IdealCuts, 'Performance' = perf))
}

#' @title
#' Generate performance metrics after model has been trained
#' @description Generates AU_ROC and AU_PR (including 95% confidence)
#' @param predictions A vector of predictions from a machine learning model.
#' @param ytest A vector of the true labels. Must be the same length as 
#' predictions.
#' @param type A string. Indicates model type and can be "regression" or 
#' "classification". 
#' Defaults to SS.
#' @return Curves (if classification); otherwise nothing. Prints results.
#'
#' @import ROCR
#' @import pROC
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

calculatePerformance <- function(predictions, ytest, type) {
  
  # These are returned for plotting
  ROCPlot <- NULL
  PRCurvePlot <- NULL
  
  # These are returned for unit tests
  AUROC <- NULL
  AUPR <- NULL
  RMSE <- NULL
  MAE <- NULL
  
  if (type == 'classification') {

    # Performance curves for return and plotting
    myOutput <- generateAUC(predictions, ytest, 'SS')
    AUROC = myOutput[[1]]
    ROCPlot = myOutput[[3]]
    ROCConf <- pROC::roc(ytest~predictions) # need pROC for 95% confidence
    conf <- pROC::auc(ROCConf) 
    cat(sprintf('95%% CI AU_ROC: (%0.2f , %0.2f) \n', ci(conf)[1], ci(conf)[3]))
    cat(sprintf('\n'))
    
    # Performance AUC calcs (AUPR is ROCR-based)
    myOutput <- generateAUC(predictions, ytest, 'PR')
    AUPR = myOutput[[1]]
    PRCurvePlot = myOutput[[3]]
    ROCConf <- pROC::roc(ytest~predictions) # need pROC for 95% confidence
    AUROC <- pROC::auc(ROCConf)   
    cat(sprintf('\n')) 
    
  } else if (type == 'regression') {
    
    RMSE <- sqrt(mean((ytest - predictions) ^ 2))
    MAE <- mean(abs(ytest - predictions))
    
    print(paste0('RMSE: ', round(RMSE, 8)))
    print(paste0('MAE: ', round(MAE, 8)))
  }
  
  return(list(ROCPlot,PRCurvePlot,AUROC,AUPR,RMSE,MAE))
}

#' @title
#' Function to initialize and populate the SupervisedModelDevelopmentParams each time a unit test is run.
#'
#' @description Initialize and populate SupervisedModelDevelopmentParams
#' @param df A data frame to use with the new supervised model.
#' @return Supervised Model Development Params class
#' 
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

initializeParamsForTesting <- function(df) {
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$impute = TRUE
  p$debug = FALSE
  p$cores = 1
  p$tune = FALSE
  p$numberOfTrees = 201
  return(p)
}


#' @title
#' Function to return ideal cutoff and TPR/FPR or precision/recall.
#'
#' @description Calculates ideal cutoff by proximity to corner of the ROC curve.
#' @param perf An ROCR performance class. (Usually made by generateAUC)
#' @param aucType A string. Indicates AUC_ROC or AU_PR and can be "SS" or "PR". 
#' Defaults to SS.
#' @param allCutoffsFlg Binary value controlling list of all thresholds. 
#' @return Array of ideal cutoff and associated TPR/FPR or pre/rec.
#' 
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' 
getCutOffs = function(perf, aucType = 'SS', allCutoffsFlg = FALSE) {
  ## TODO: Give user the ability to give higher weight to recall or FPR
  x <- unlist(perf@x.values)
  y <- unlist(perf@y.values)
  p <- unlist(perf@alpha.values)
  # for ROC curves
  if (aucType == 'SS') {
    d = (x - 0) ^ 2 + (y - 1) ^ 2
    ind = which(d == min(d))
    tpr = y[[ind]]
    fpr = x[[ind]]
    cutoff = p[[ind]]
    cat(sprintf("Ideal cutoff is %0.2f, yielding TPR of %0.2f and FPR of %0.2f \n", 
                cutoff, tpr, fpr))  
    if (isTRUE(allCutoffsFlg)) {
      cat(sprintf("%-7s %-6s %-5s \n", 'Thresh', 'TPR', 'FPR'))
      cat(sprintf("%-7.2f %-6.2f %-6.2f \n", 
                  unlist(perf@alpha.values), unlist(perf@y.values), unlist(perf@x.values)))  
    }
    return(c(cutoff, tpr, fpr)) # list of integers
    # for PR curves
  } else if (aucType == 'PR') { 
    d = (x - 1) ^ 2 + (y - 1) ^ 2
    # Convert NaNs to one
    d[ is.nan(d) ] <- 1
    ind = which(d == min(d))
    pre = y[[ind]]
    rec = x[[ind]]
    cutoff = p[[ind]]
    cat(sprintf("Ideal cutoff is %0.2f, yielding Precision of %0.2f and Recall of %0.2f \n", 
                cutoff, pre, rec))  
    if (isTRUE(allCutoffsFlg)) {
      cat(sprintf("%-7s %-10s %-10s \n", 'Thresh', 'Precision', 'Recall'))
      cat(sprintf("%-7.2f %-10.2f %-10.2f \n", 
                  unlist(perf@alpha.values), unlist(perf@y.values), unlist(perf@x.values)))
    }
    return(c(cutoff, pre, rec)) # list of integers
  }
}