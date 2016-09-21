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
#' df_result = GroupedLOCF(df, 'PersonID')
#'
#' head(df_result, n = 7)

GroupedLOCF <- function(df, id) {
  # Note that the object that results acts as both a data frame and datatable
  df <- data.table::setDT(df)

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
  df_result <- df[, lapply(.SD, locf)]
  df_result
}

#' @title
#' Convert datetime column into dummy columns
#'
#' @description
#' Convert datetime column into dummy columns of day, hour, etc, such that one
#' can use daily and seasonal patterns in their model building.
#'
#' @param df A data frame. Indicates the datetime column.
#' @param date.time.col A string. Column name in df that will be converted
#' into several columns.
#' @param depth A string. Specifies the depth with which to expand extra columns
#' (starting with a year column). 'd' expands to day, 'h' expands to hour
#' (default), m' expands to minute, and 's' expands to second.
#' @param return.dt.col A boolean. Return the original date.time.col with
#' the modified data frame?
#' @return A data frame which now includes several columns based on time
#' rather than just one datetime column
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' dt_col = c("2001-06-09 12:45:05","2002-01-29 09:30:05","2002-02-02 07:36:50",
#'           "2002-03-04 16:45:01","2002-11-13 20:00:10","2003-01-29 07:31:43",
#'           "2003-07-07 17:30:02","2003-09-28 01:03:20")
#' y1 <- c(.5,1,3,6,8,13,14,1)
#' y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1)
#' df <- data.frame(dt_col,y1,y2)
#'
#' df <- ConvertDateTimeColToDummies(df, 'dt_col')
#' head(df)

ConvertDateTimeColToDummies <- function(df,
                                        date.time.col,
                                        depth='h',
                                        return.dt.col=FALSE) {
  if (depth == 'd') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format = "%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1

  } else if (depth == 'h') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format = "%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour

  } else if (depth == 'm') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format = "%W")
    df$DayOfMonth <- as.POSIXlt(df[[date.time.col]])$mday
    df$DayOfWeek <- as.POSIXlt(df[[date.time.col]])$wday + 1
    df$Hour <- as.POSIXlt(df[[date.time.col]])$hour
    df$Min <- as.POSIXlt(df[[date.time.col]])$min

  } else if (depth == 's') {

    df[[date.time.col]] <- as.POSIXct(df[[date.time.col]])
    df$Year <- as.POSIXlt(df[[date.time.col]])$year + 1900
    df$Month <- as.POSIXlt(df[[date.time.col]])$mo + 1
    df$WeekOfYear <- strftime(df[[date.time.col]],format = "%W")
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
#' # To use this function on an entire data frame:
#' df = data.frame(a=c(1,2,3,NA),
#'                 b=c('Y','N','Y',NA))
#' df[] <- lapply(df, ImputeColumn)
#' head(df)

ImputeColumn <- function(v) {
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
  x <- unique(v)
  bool_result <- length(x) - sum(is.na(x)) == 2L
  bool_result
}

#' @title
#' Remove rows where specified col is NA
#' @description Remove rows from a data frame where a particular col is NA
#' @param df A data frame to be altered
#' @param desired_col A column name in the df (in ticks)
#' @return df_result The input data frame with rows removed
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,2,3),b=c('Y','N',NA),c=c(NA,'Y','N'))
#' df_result = RemoveRowsWithNAInSpecCol(df,'b')
#' head(df_result)

RemoveRowsWithNAInSpecCol <- function(df, desired_col) {
  completeVec <- stats::complete.cases(df[[desired_col]])
  df_result <- df[completeVec,]
  df_result
}

#' @title
#' Pull data into R via an ODBC connection
#' @description Select data from an ODBC database and return the results as
#' a data frame.
#' @param connection.string A string specifying the driver, server, database,
#' and whether Windows Authentication will be used.
#' @param query The SQL query (in ticks or quotes)
#' @return df A data frame containing the selected rows
#'
#' @import RODBC
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' connection.string = "
#'   driver={SQL Server};
#'   server=localhost;
#'   database=AdventureWorks2012;
#'   trusted_connection=true
#'   "
#'
#' query = "
#'   SELECT
#'     [OrganizationLevel]
#'   FROM [AdventureWorks2012].[HumanResources].[Employee]
#'   "
#'
#' df <- SelectData(connection.string, query)
#' head(df)

SelectData <- function(connection.string, query) {
  # TODO: if debug: cat(connection.string)
  cnxn <- odbcDriverConnect(connection.string)

  # TODO: if debug: cat(query)
  # TODO: if debug: time this operation and print the time spent to pull data.
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
#' Write data to database
#' @description Write data frame to database via ODBC connection
#' @param df A data frame being written to a database
#' @param server A string.
#' @param database A string.
#' @param schema_dot_table A string representing the destination schema and
#' table. Note that brackets aren't expected.
#' @return Nothing
#'
#' @import RODBC
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' df <- data.frame(a=c(1,2,3),
#'                  b=c(2,4,6),
#'                  c=c('one','two','three'))
#'
#' #WriteData(df,'localhost','SAM','dbo.HCRWriteData')

WriteData <- function(df, server, database, schema_dot_table) {

  # TODO: use sub function to remove brackets from schema_dot_table
  # TODO: add try/catch around sqlSave
  connection.string <-
    paste0("driver={SQL Server};
           server=",server,";
           database=",database,";
           trusted_connection=true")

  sqlcnxn <- odbcDriverConnect(connection.string)

  # Save df to table in specified database
  out <- sqlSave(channel = sqlcnxn,
                dat = df,
                tablename = schema_dot_table,
                append = T,
                rownames = F,
                colnames = F,
                safer = T,
                nastring = NULL)

  # Clean up.
  odbcCloseAll()

  if (out == 1) {
    print('SQL Server insert was successful')
  } else {
    print('SQL Server insert failed')
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1),
#'                 b=c('a','b','b'),
#'                 c=c('a','a','a'),
#'                 d=c(NA,'1',NA))
#' df_result = RemoveColsWithAllSameValue(df)
#' head(df_result)

RemoveColsWithAllSameValue <- function(df) {
  df_result <- df[sapply(df, function(x) length(unique(x[!is.na(x)])) > 1)]
  if (ncol(df_result) == 0) {
    message('All columns were removed.')
  }
  df_result
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#'                     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#'                 b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                     'o','p','q','r','s','t','u','v','w','x','y','z','aa','bb',
#'                     'cc','dd','ee','ff','gg','hh','ii','jj','kk','ll','mm','nn',
#'                     'oo','pp','qq','rr','ss','tt','uu','vv','ww','xx','yy'))
#' col_list = ReturnColsWithMoreThanFiftyCategories(df)

ReturnColsWithMoreThanFiftyCategories <- function(df) {
  col_list <- vector('character')
  for (columnName in names(df)) {
    if (nlevels(df[[columnName]]) > 50) {
      col_list <- c(col_list,columnName)
    }
  }
  col_list
}

#' @title
#' Find any columns that have a trend above a particular threshold
#' @description
#' Find numeric columns in data frame that have an absolute slope greater than
#' that specified via threshold argument.
#' @param df A data frame
#' @return A data frame containing the dimensional attribute (ie gender), the
#' subset the data was grouped by (ie M/F), the measures that had trends
#' (ie, mortality or readmission), and the ending month.
#'
#' @importFrom stats aggregate formula
#' @importFrom utils tail
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#'dates <- c(as.Date("2012-01-01"),as.Date("2012-01-02"),as.Date("2012-02-01"),
#'           as.Date("2012-03-01"),as.Date("2012-04-01"),as.Date("2012-05-01"),
#'           as.Date("2012-06-01"),as.Date("2012-06-02"))
#'y1 <- c(0,1,2,6,8,13,14,16)               # large positive
#'y2 <- c(.8,1,1.2,1.2,1.2,1.3,1.3,1.5)     # small positive
#'y3 <- c(1,0,-2,-2,-4,-5,-7,-8)            # big negative
#'y4 <- c(.5,0,-.5,-.5,-.5,-.5,-.6,0)       # small negative
#'gender <- c('M','F','F','F','F','F','F','F')
#'df <- data.frame(dates,y1,y2,y3,y4,gender)
#'
#'df_result = FindTrends(df = df,
#'                       datecol = 'dates',
#'                       coltoaggregate = 'gender')

FindTrends <- function(df,
                       datecol,
                       coltoaggregate) {

  df$year <- as.POSIXlt(df[[datecol]])$year + 1900
  df$month <- as.POSIXlt(df[[datecol]])$mo + 1

  df[[datecol]] <- NULL

  df <- aggregate(formula(paste0(".~", coltoaggregate, "+year+month")),
                      data = df,
                      FUN = sum)

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

    print('df after grouping and focusing on one category in group col:')
    print(tail(dftemp, n = 6))

    # Iterate over all columns except for cols that we aggregated by
    for (i in coliterlist) {
      if (is.numeric(dftemp[[i]])) {
        # Check if last six values are monotonically increasing
        n <- nrow(dftemp)
        if (n > 5) {
          # TODO: make this check into a function
          check.incr = all(dftemp[[i]][(n - 5):n] == cummax(dftemp[[i]][(n - 5):n]))
          check.decr = all(dftemp[[i]][(n - 5):n] == cummin(dftemp[[i]][(n - 5):n]))
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
    df_result = data.frame(coltoaggregate,
                          aggregated.col.list,
                          metric.trend.list,
                          final_yr_month)
    colnames(df_result) <- c("DimAttribute",
                            "GroupBy",
                            "MeasuresTrending",
                            "FinalDate")
    print('Trends were found:')
    print(df_result)
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#' df = data.frame(date=c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
#'                 a=c(1,2,3,4))
#' df_result = OrderByDate(df,'date', descending=FALSE)
#' head(df_result)

OrderByDate <- function(df,datecol,descending=FALSE) {
  df[[datecol]] <- lubridate::ymd_hms(df[[datecol]],truncated = 5)

  if (descending == FALSE) {
    df_result <- df[order(df[[datecol]]),]
  } else {
    df_result <- df[rev(order(df[[datecol]])),]
  }
  df_result
}

#' @title
#' Correlation analysis on an input table, focusing on one target variable
#'
#' @description Calculates correlations between each numeric column in a table
#' and and a target column
#' @param df A data frame
#' @param target.col Name of target column against which correlations will be
#' calculated
#' @return A data frame with column names and corresponding correlations and
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
#' d=c('M','F','F','F','M','F')) #<- is ignored
#'
#' df_result <- CalculateTargetedCorrelations(df=df,target.col='c')
#' df_result

CalculateTargetedCorrelations <- function(df,target.col) {
  if (!is.numeric(df[[target.col]])) {
    stop("Your target column must be numeric")
  }

  # Initialize variable, since we will iterate
  pvalue <- vector('numeric')

  # Pull only numeric columns
  nums <- sapply(df, is.numeric)
  df <- df[ , nums]

  collist <- names(df)
  # Trim list of col names, so target doesn't check against itself
  collist <- collist[collist != target.col]

  # Make list of correlations
  cor <- cor(as.matrix(df[[target.col]]),
             as.matrix(df[ ,!(colnames(df) == target.col)]))

  # Make list of corr-related p-values
  for (i in collist) {
    pvalue <- c(pvalue,cor.test(df[[target.col]], df[,i])$p.value)
  }

  df_result <- data.frame(t(cor),pvalue)
  # Change name of corr col
  names(df_result)[names(df_result) == 't.cor.'] <- 'correlation'

  # Change row name to actual col
  df_result <- cbind(column = rownames(df_result), df_result)
  rownames(df_result) <- NULL

  colnames(df_result) <- c('Column','Correlation','PValue')

  df_result
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' library(HCRTools)
#'
#' df <- data.frame(a=c(1,2,3,4,5,6),
#' b=c(6,5,4,3,2,1),
#' c=c(3,4,2,1,3,5),
#' d=c('M','F','F','F','M','F')) #<- is ignored
#'
#' df_result <- CalculateAllCorrelations(df)
#' df_result

CalculateAllCorrelations <- function(df) {
  df_result <- cor(df[sapply(df, is.numeric)])
  df_result
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
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#'                     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#'                 b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n',
#'                     'o','p','q','r','s','t','u','v','w','x','y','z','aa','bb',
#'                     'cc','dd','ee','ff','gg','hh','ii','jj','kk','ll','mm','nn',
#'                     'oo','pp','qq','rr','ss','tt','uu','vv','ww','xx','yy'))
#' col_list = ReturnColsWithMoreThanFiftyCategories(df)

ReturnColsWithMoreThanFiftyCategories <- function(df) {
  col_list <- vector('character')
  for (columnName in names(df)) {
    if (nlevels(df[[columnName]]) > 50) {
      col_list <- c(col_list,columnName)
    }
  }
  col_list
}

#' @title
#' Calculates percentage of each column in df that is NULL (NA)
#'
#' @description Returns a vector with percentage of each column that is NULL
#' in the original data frame
#' @param df A data frame
#' @return A vector that contains the names of the columns with greater
#' than 50 categories
#'
#' @export
#' @references \url{https://community.healthcatalyst.com/community/data-science}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,2,NA,NA,3),
#'                 b=c(NA,NA,NA,NA,NA),
#'                 c=c(NA,NA,'F','M',NA))
#' col_list = CountPercentEmpty(df)
#' col_list

CountPercentEmpty <- function(df) {
  col_list <- colMeans(is.na(df))
  col_list
}

