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
#' Remove columns from a dataframe when those columns have the same values in each row
#'
#' @description Remove columns from a dataframe when all of their rows are the same value
#' @param df A dataframe
#' @return A dataframe with those columns removed
#'
#' @export
#' @seealso \code{\link{HCRTools}}
#' @examples
#' df = data.frame(a=c(1,1,1),b=c('a','b','b'),c=c('a','a','a'),d = c(NA,'1',NA))
#' resdf = RemoveColsWithAllSameValue(df)


RemoveColsWithAllSameValue <- function(df) {
  df[sapply(df, function(x) length(unique(x))>1)]
}
