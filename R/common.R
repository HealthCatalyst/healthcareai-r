#' @title
#' Last observation carried forward
#'
#' @description
#' Carries the last observed value forward for all columns in a data.table
#' grouped by an id.
#'
#' @param df data.frame sorted by an ID column and a time or sequence number
#' column.
#' @param id A column name (in ticks) in df to group rows by.
#' @return A data.table where the last non-NA values are carried forward
#' (overwriting NAs) until the group ID changes.
#'
#' @import data.table
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' df = data.frame(PersonID=c(1,1,2,2,3,3,3),
#'                 wt=c(.5,NA,NA,NA,.3,.7,NA),
#'                 ht=c(NA,1,3,NA,4,NA,NA),
#'                 date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
#'                        '01/01/2015','01/15/2015','01/30/2015'))
#'
#' head(df,n=7)
#'
#' df.result = GroupedLOCF(df, 'PersonID')
#'
#' head(df.result,n=7)
GroupedLOCF <- function(df, id) {
  # Carries the last observed value forward for all columns in a data.table
  # grouped by an id.
  #
  # Args:
  #   df: data.frame sorted by an ID column and a time or sequence number column.
  #
  #   id: a column name in df to group rows by.
  #
  # Returns:
  #   A data.table where the last non-NA values are carried forward (overwriting
  #   NAs) until the group ID changes.
  #

  # Note that the object that results acts as both a dataframe and datatable
  df <- setDT(df)

  # Create a vector of booleans where each element is mapped to a row
  # in the data.table.  Each value is FALSE unless the corresponding
  # row is the first row of a person.  In other words, each TRUE
  # represents a change of PersonID in the data.table.
  change.flags <- c(TRUE, get(id, df)[-1] != get(id, df)[-nrow(df)])

  # A helper that finds the last non-NA value for a given column x in df.
  locf <- function(x) x[cummax(((!is.na(x)) | change.flags) * seq_len(nrow(df)))]

  # By avoiding using the 'by' operator of data.table, we're reducing
  # the number of calls from (N rows / P people) * C columns to just C columns;
  # this is just once for each column in the data.table.
  df[, lapply(.SD, locf)]
}

#' @title
#' Convert datetime column into dummy columns
#'
#' @description
#' Convert datetime column into dummy columns of day, hour, etc, such that one
#' can use daily and seasonal patterns in their model building.
#'
#' @param df A dataframe. Indicates the datetime column.
#' @param date.time.col A string. Column name in df that will be converted
#' into several columns.
#' @param depth A string. Specifies the depth with which to expand extra columns
#' (starting with a year column). 'd' expands to day, 'h' expands to hour
#' (default), m' expands to minute, and 's' expands to second.
#' @param return.dt.col A boolean. Return the original date.time.col with
#' the modified dataframe?
#' @return A dataframe which now includes several columns based on time
#' rather than just one datetime column
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' DTCol = c("2001-06-09 12:45:05","2002-01-29 09:30:05","2002-02-02 07:36:50",
#'           "2002-03-04 16:45:01","2002-11-13 20:00:10","2003-01-29 07:31:43",
#'           "2003-07-07 17:30:02","2003-09-28 01:03:20")
#' y1 <- c(.5,1,3,6,8,13,14,1)
#' y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1)
#' df <- data.frame(DTCol,y1,y2)
#'
#' df <- ConvertDateTimeColToDummies(df, 'DTCol')

ConvertDateTimeColToDummies <- function(df,
                                        date.time.col,
                                        depth='h',
                                        return.dt.col=FALSE) {
  if (depth == 'd') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1

  } else if (depth == 'h') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour

  } else if (depth == 'm') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour
    df$Min <- as.POSIXlt(df[[date.time.col]])$min

  } else if (depth == 's') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour
    df$Min <- as.POSIXlt(df[[date.time.col]])$min
    df$Sec <- as.POSIXlt(df[[date.time.col]])$sec
  }

  if (isTRUE(!return.dt.col)) {
    df[[date.time.col]] <- NULL
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' # For a numeric vector
#' v_result = ImputeColumn(c(1,2,3,NA))
#'
#' # For a factor vector
#' v_result = ImputeColumn(c('Y','N','Y',NA))
#'
#' # To use this function on an entire dataframe:
#' df = data.frame(a=c(1,2,3,NA),
#'                 b=c('Y','N','Y',NA))
#' df[] <- lapply(df, ImputeColumn)
ImputeColumn <- function(v) {
  # Does imputation for NA values in a column
  #
  # Args:
  #   v: Vector, a column of values.
  #
  # Returns:
  #   A vector (or column) whose values have been imputed
  #
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' IsBinary(c(1,2,NA))
#' IsBinary(c(1,2,3))
IsBinary <- function(v) {
  # Checks if a vector (or column) has only two unique values
  #
  # Args:
  #   v: Vector, a column of values.
  #
  # Returns:
  #   Boolean of whether column has only two unique values (not counting NA)
  #
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}

#' @title
#' Remove rows where specified col is NA
#' @description Remove rows from a dataframe where a particular col is NA
#' @param df A dataframe to be altered
#' @param desiredCol A column name in the df (in ticks)
#' @return The input dataframe with rows removed
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,2,3),b=c('Y','N',NA),c=c(NA,'Y','N'))
#' resdf = RemoveRowsWithNAInSpecCol(df,'b')
RemoveRowsWithNAInSpecCol <- function(df, desiredCol) {
  # Removes rows from df where specified col is NA
  #
  # Args:
  #   df: Dataframe that is to be altered
  #
  #   desiredCol: Column that has NA's of concern
  #
  # Returns:
  #   A dataframe that has had rows removed
  #
  completeVec <- stats::complete.cases(df[[desiredCol]])

  return(df[completeVec, ])
}

#' @title
#' Pull data into R via an ODBC connection
#' @description Select data from an ODBC database and return the results as
#' a data frame.
#' @param connection.string A string specifying the driver, server, database,
#' and whether Windows Authentication will be used.
#' @param query The SQL query (in ticks)
#' @return dataframe
#'
#' @import RODBC
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' connection.string = '
#'   driver={SQL Server};
#'   server=localhost;
#'   database=AdventureWorks2012;
#'   trusted_connection=true'
#'
#' query = '
#'   SELECT
#'     [OrganizationLevel]
#'   FROM [AdventureWorks2012].[HumanResources].[Employee]'
#'
#' dataframe <- SelectData(connection.string, query)


SelectData <- function(connection.string, query) {
  # Select data from an ODBC database and return the results as a data frame.
  #
  # If the query contains errors or returns too few rows, an error message is
  # printed and this function invokes 'stop', halting execution.
  #
  # Args:
  #   connection.string: A string representation of an ODBC connection string.
  #
  #  query: A string SQL query to select data from the database.
  #
  # Returns:
  #   A dataframe containing the selected rows.
  #

  # TODO: if debug: cat(connection.string)
  cnxn <- odbcDriverConnect(connection.string)

  # TODO: if debug: cat(query)
  # TODO: if debug: time this operation and print the time it takes to select everything.
  df <- sqlQuery(
    channel = cnxn,
    na.strings = 'NULL',
    query = query
  )

  odbcCloseAll()

  # Make sure there are enough rows to actually do something useful.
  if (is.null(nrow(df))) {
    cat(df)  # Print the SQL error, which is contained in df.
    stop("Your SQL contains an error.")
  }
  if (nrow(df) < 200) {
    cat("Too few rows returned from SQL: ")
    cat(nrow(df))
    cat(" rows returned.")
    cat("Adjust your query to return more data!")
  }
  # TODO: if debug: print the number of rows selected.
  df  # Return the selected data.
}

#' @title
#' Remove columns from a dataframe when those columns have the same values in
#' each row
#'
#' @description Remove columns from a dataframe when all of their rows are the
#' same value
#' @param df A dataframe
#' @return A dataframe with those columns removed
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1),b=c('a','b','b'),c=c('a','a','a'),d = c(NA,'1',NA))
#' resdf = RemoveColsWithAllSameValue(df)


RemoveColsWithAllSameValue <- function(df) {
  df <- df[sapply(df, function(x) length(unique(x))>1)]
  if (ncol(df)==0){
    message('All columns were removed.')
  }
  df
}

#' @title
#' Return vector of columns in a dataframe with greater than 50 categories
#'
#' @description Returns a vector of the names of the columns that have more than
#' 50 categories
#' @param df A dataframe
#' @return colList A vector that contains the names of the columns with greater
#' than 50 categories
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#'                     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#'                 b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                     'o','p','q','r','s','t','u','v','w','x','y','z','aa','bb',
#'                     'cc','dd','ee','ff','gg','hh','ii','jj','kk','ll','mm','nn',
#'                     'oo','pp','qq','rr','ss','tt','uu','vv','ww','xx','yy'))
#' colList = ReturnColsWithMoreThanFiftyCategories(df)

ReturnColsWithMoreThanFiftyCategories <- function(df) {
  colList=vector('character')
  for (columnName in names(df)){
    if (nlevels(df[[columnName]])>50){
      colList<-c(colList,columnName)
    }
  }
  colList
}

#' @title
#' Find any columns that have a trend above a particular threshold
#' @description
#' Find numeric columns in dataframe that have an absolute slope greater than
#' that specified via threshold argument.
#' @param df A dataframe
#' @return A vector of column names
#'
#' @importFrom stats aggregate formula
#' @importFrom utils tail
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#'x <- c(as.Date("2012-01-01"),as.Date("2012-01-02"),as.Date("2012-02-01"),
#'       as.Date("2012-03-01"),as.Date("2012-04-01"),as.Date("2012-05-01"),
#'       as.Date("2012-06-01"),as.Date("2012-06-02"))
#'y1 <- c(0,1,2,6,8,13,14,16)         # large positive
#'y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1.5)  # small positive
#'y3 <- c(1,0,-2,-2,-4,-5,-7,-8)       # big negative
#'y4 <- c(.5,0,-.5,-.5,-.5,-.5,-.6,0)  # small negative
#'y5 <- c('M','F','F','F','F','F','F','F')            # factor col
#'df <- data.frame(x,y1,y2,y3,y4,y5)
#'
#'res = FindTrends(df = df,
#'                 datecol = 'x',
#'                 coltoaggregate = 'y5')
#'
FindTrends <- function(df,
                       datecol,
                       coltoaggregate) {

  df$year <- as.POSIXlt(df[[datecol]])$year + 1900
  df$month <- as.POSIXlt(df[[datecol]])$mo + 1

  df[[datecol]] <- NULL

  df <- aggregate(formula(paste0(".~", coltoaggregate, "+year+month")),
                      data=df,
                      FUN=sum)

  df <- df[with(df, order(year, month)), ]

  # TODO: alter this last month dynamically when we search over all time
  final_yr_month = paste0(month.abb[df$month[length(df$month)]],
                         '-',
                         df$year[length(df$year)])

  # Pre-create empty vectors
  metric.trend.list <- vector('character')
  aggregated.col.list <- vector('character')

  # Create list that doesn't have cols we aggregated by
  coliterlist = names(df)
  remove <- c(coltoaggregate,"year","month")
  coliterlist <- coliterlist[!coliterlist %in% remove]

  # If the last six values are monotonically increasing, add col name to list
  for (j in unique(df[[coltoaggregate]])) {
    # Just grab rows corresponding to a particular category in the factor col
    dftemp <- df[df[[coltoaggregate]] == j,]

    print('Dataframe after grouping and focusing on one category in group col:')
    print(tail(dftemp,n=6))

    # Iterate over all columns except for cols that we aggregated by
    for (i in coliterlist) {
      if (is.numeric(dftemp[[i]])) {
        # Check if last six values are monotonically increasing
        n <- nrow(dftemp)
        if (n>5) {
          check.incr = all(dftemp[[i]][(n-5):n] == cummax(dftemp[[i]][(n-5):n]))
          check.decr = all(dftemp[[i]][(n-5):n] == cummin(dftemp[[i]][(n-5):n]))
          if (isTRUE(check.incr) || isTRUE(check.decr)) {
            # If true, append col names to list to output
            aggregated.col.list <- c(aggregated.col.list, j)
            metric.trend.list <- c(metric.trend.list, i)
          }
        }
      }
    }
  }

  if (length(metric.trend.list) == 0) {
    message('No trends of sufficient length found')
    return()
  } else {
    dfreturn = data.frame(coltoaggregate,
                          aggregated.col.list,
                          metric.trend.list,
                          final_yr_month)
    colnames(dfreturn) <- c("DimAttribute",
                            "GroupBy",
                            "MeasuresTrending",
                            "FinalDate")
    print('Trends were found:')
    print(dfreturn)
    return(dfreturn)
  }
}

#' @title
#' Order the rows in a dataframe by date
#'
#' @description Returns a dataframe that's ordered by its date column
#' @param df A dataframe
#' @param dateCol Name of column in dataframe that contains dates
#' @param descending Boolean for whether the output should be in descending order
#' @return df A dataframe ordered by date column
#'
#' @importFrom lubridate ymd_hms
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' df = data.frame(date=c('2009-01-01','2010-01-01','2009-03-08','2009-01-19',
#'                        '2010-11-11'),
#'                 a=c(1,2,3,4,5))
#' resdf = OrderByDate(df,'date',descending=FALSE)

OrderByDate <- function(df,datecol,descending=FALSE) {
  df[[datecol]] <- lubridate::ymd_hms(df[[datecol]],truncated = 5)

  if (descending == FALSE) {
    df <- df[order(df[[datecol]]),]
  } else {
    df <- df[rev(order(df[[datecol]])),]
  }
  df
}


#' @title
#' Correlation analysis on an input table, focusing on one target variable
#'
#' @description Calculates correlations between each numeric column in a table
#' and and a target column
#' @param df A dataframe
#' @param target.col Name of target column against which correlations will be
#' calculated
#' @return df A dataframe with column names and corresponding correlations and
#' p-values with the target column
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#'
#' df <- data.frame(a=c(1,2,3,4,5,6),
#' b=c(6,5,4,3,2,1),
#' c=c(3,4,2,1,3,5),
#' d=c('M','F','F','F','M','F')) <- is ignored
#'
#' res <- CalculateTargetedCorrelations(df,'c')
#' res

CalculateTargetedCorrelations <- function(df,targetcol) {
  if (!is.numeric(df[[targetcol]])) {
    stop("Your target column must be numeric")
  }

  # Initialize variable, since we will iterate
  pvalue <- vector('numeric')

  # Pull only numeric columns
  # Pull only numeric columns
  nums <- sapply(df, is.numeric)
  df <- df[ , nums]


  collist <- names(df)
  # Trim list of col names, so target doesn't check against itself
  collist <- collist[collist != targetcol]

  # Make list of correlations
  cor <- cor(as.matrix(df[[targetcol]]), as.matrix(df[ ,!(colnames(df) == targetcol)]))

  # Make list of corr-related p-values
  for (i in collist) {
    pvalue <- c(pvalue,cor.test(df[[targetcol]], df[,i])$p.value)
  }

  dfout <- data.frame(t(cor),pvalue)
  # Change name of corr col
  names(dfout)[names(dfout) == 't.cor.'] <- 'correlation'

  # Change row name to actual col
  dfout <- cbind(column = rownames(dfout), dfout)
  rownames(dfout) <- NULL

  dfout
}

#' @title
#' Correlation analysis on an input table over all numeric columns
#'
#' @description Calculate correlations between every numeric column in a table
#' @param df A dataframe
#' @return df A dataframe with column names and corresponding correlations
#' with the target column
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#'
#' df <- data.frame(a=c(1,2,3,4,5,6),
#' b=c(6,5,4,3,2,1),
#' c=c(3,4,2,1,3,5),
#' d=c('M','F','F','F','M','F')) <- is ignored
#'
#' res <- CalculateAllCorrelations(df,'c')
#' res

CalculateAllCorrelations <- function(df) {
  dfout <- cor(df[sapply(df, is.numeric)])

  dfout
}

