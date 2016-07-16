#' @title
#' Last observation carried forward
#'
#' @description
#' Carries the last observed value forward for all columns in a data.table
#' grouped by an id.
#'
#' @param dt data.table sorted by an ID column and a time or sequence number
#' column.
#' @param id A column name (in ticks) in dt to group rows by.
#' @return A data.table where the last non-NA values are carried forward (overwriting
#' NAs) until the group ID changes.
#'
#' @import data.table
#' @export
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(data.table)
#' dt = data.table(PersonID=c(1,1,2,2,3,3,3),
#'                 wt=c(.5,NA,NA,NA,.3,.7,NA),
#'                 ht=c(NA,1,3,NA,4,NA,NA),
#'                 date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
#'                        '01/01/2015','01/15/2015','01/30/2015'))
#'
#' head(dt,n=7)
#'
#' df.result = GroupedLOCF(dt, 'PersonID')
#'
#' head(df.result,n=7)
GroupedLOCF <- function(dt, id) {
  # Carries the last observed value forward for all columns in a data.table
  # grouped by an id.
  #
  # Args:
  #   dt: data.table sorted by an ID column and a time or sequence number column.
  #
  #   id: a column name in dt to group rows by.
  #
  # Returns:
  #   A data.table where the last non-NA values are carried forward (overwriting
  #   NAs) until the group ID changes.
  #

  # Create a vector of booleans where each element is mapped to a row
  # in the data.table.  Each value is FALSE unless the corresponding
  # row is the first row of a person.  In other words, each TRUE
  # represents a change of PersonID in the data.table.
  change.flags <- c(TRUE, get(id, dt)[-1] != get(id, dt)[-nrow(dt)])

  # A helper that finds the last non-NA value for a given column x in dt.
  locf <- function(x) x[cummax(((!is.na(x)) | change.flags) * seq_len(nrow(dt)))]

  # By avoiding using the 'by' operator of data.table, we're reducing
  # the number of calls from (N rows / P people) * C columns to just C columns;
  # this is just once for each column in the data.table.
  dt[, lapply(.SD, locf)]
}

#' @title
#' Convert datetime column into dummy columns
#'
#' @description
#' Convert datetime column into dummy columns of day, hour, etc, such that one
#' can use daily and seasonal patterns in their model building.
#'
#' @param df A dataframe, which includes the datetime column.
#' @param date.time.col A column name (in ticks) in df that will be converted
#' into several columns.
#' @param depth The specificity with which to expand extra columns. 'h' expands
#' to hour, 'm' expands to minute, and 's' expands to second.
#' @return A dataframe which now includes several columns based on time
#' rather than just one datetime column
#'
#' @export
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
    df$WeekofYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1

  } else if (depth == 'h') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekofYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour

  } else if (depth == 'm') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekofYear <- strftime(df[[date.time.col]],format="%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour
    df$Min <- as.POSIXlt(df[[date.time.col]])$min

  } else if (depth == 's') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekofYear <- strftime(df[[date.time.col]],format="%W")
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
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(RODBC)
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
#' @description Returns a vector of the names of the columns that have more than 50 factors
#' @param df A dataframe
#' @return colList A vector that contains the names of the columns with greater than 50 categories
#'
#' @export
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#'                     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#'                 b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                     'o','p','q','r','s','t','u','v','w','x','y','z','aa','bb',
#'                     'cc','dd','ee','ff','gg','hh','ii','jj','kk','ll','mm','nn',
#'                     'oo','pp','qq','rr','ss','tt','uu','vv','ww','xx','yy'))
#' colList = ReturnColsWithMoreThanFiftyFactors(df)


ReturnColsWithMoreThanFiftyFactors <- function(df) {
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
#' @importFrom stats lm
#' @export
#' @seealso \code{\link{HCRTools}}
#' @examples
#' x <- seq(as.Date("2012-01-01"), as.Date("2012-01-06"), by = "days")
#' y1 <- c(1,3,6,8,13,14)          # big positive
#' y2 <- c(1,1.2,1.2,1.2,1.3,1.3)  # small positive
#' y3 <- c(0,-2,-2,-4,-5,-7)       # big negative
#' y4 <- c(0,-.5,-.5,-.5,-.5,-.6)  # small negative
#' y5 <- c(0,1,1,2,5,5)            # factor col
#'
#' data <- data.frame(x,y1,y2,y3,y4,y5)
#' data$y5 <- as.factor(data$y5)
#'
#' # Using nelson rule 3 (ie six data points mono decr or incr)
#' col_list <- FindTrendsAboveThreshold(df = data,
#'                                      datecol = 'x',
#'                                      nelson = TRUE)
#'
#' # Using linear slope thresholds (overall entire series)
#' col_list <- FindTrendsAboveThreshold(df = data,
#'                                      datecol = 'x',
#'                                      threshold = 0.5,
#'                                      nelson = FALSE)
#'

FindTrendsAboveThreshold <- function(df, datecol, threshold=0.5, nelson=TRUE) {
  #TODO: Create function to order rows by date DESC and ASC
  #df <- df[order(as.Date(df[[datecol]],,format="%Y-%m-%d")),drop=FALSE]

  # Pre-create empty trend vector
  col_list <- vector('character')
  count <- 1

  # If the last six values are monotonically increasing, add col name to list
  if (isTRUE(nelson)) { # Using nelson rule three
    for (i in names(df)) {
      if (is.numeric(df[,i])) {

        # Check if last six values are monotonically increasing
        check.incr = all(tail(df[[i]],6) == cummax(tail(df[[i]],6)))
        check.decr = all(tail(df[[i]],6) == cummin(tail(df[[i]],6)))
        if (isTRUE(check.incr) || isTRUE(check.decr)) {
          col_list <- c(col_list, i)
          count <- count + 1
        }
      }
    }
  } else {

    # If time-trend of particular col is above thresh, add to list
    for (i in names(df)) {
      if (is.numeric(df[,i])) {
        # Scale vector by dividing by its std deviation
        df[[i]] = scale(df[[i]])

        # Check if absolute slope is greater than threshold
        model = lm(df[[i]] ~ df[[datecol]])
        if (abs(model$coefficients[2]) > threshold) {
          col_list <- c(col_list, i)
          count <- count + 1
        }
      }
    }
  }
  if (length(col_list) == 0) {
    message('No trends of sufficient slope found')
    return()
  } else {
    return(col_list)
  }
}
