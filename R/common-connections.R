#' @title 
#' Add SAM utility columns to table
#' @description When working in a Health Catalyst Source Area Mart (SAM), 
#' utility columns are added automatically when running a non-R binding
#' @param df A dataframe
#' @return A dataframe with three additional columns
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a = c(1,2,NA,NA),
#'                  b = c(100,300,200,150))
#' head(df)
#' df <- addSAMUtilityCols(df)
#' head(df)
addSAMUtilityCols <- function(df) {
  df <- cbind(BindingID = 0,
              BindingNM = 'R',
              LastLoadDTS = Sys.time(),
              df)
  df
}

#' @title
#' Pull data into R via an ODBC connection
#' @description Select data from an ODBC database and return the results as
#' a data frame.
#' @param MSSQLConnectionString A string specifying the driver, server, 
#' database, and whether Windows Authentication will be used.
#' @param query The SQL query (in ticks or quotes)
#' @param SQLiteFileName A string. If dbtype is SQLite, here one specifies the 
#' database file to query from
#' @param randomize Boolean that dictates whether returned rows are randomized
#' @return df A data frame containing the selected rows
#' @import methods
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @seealso \code{\link{writeData}}
#' @examples
#' 
#' \donttest{
#' # This example is specific to SQL Server
#' 
#' # To instead pull data from Oracle see here 
#' # https://cran.r-project.org/web/packages/ROracle/ROracle.pdf
#' # To pull data from MySQL see here 
#' # https://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf
#' # To pull data from Postgres see here 
#' # https://cran.r-project.org/web/packages/RPostgreSQL/RPostgreSQL.pdf
#' 
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
selectData <- function(MSSQLConnectionString = NULL, 
                       query, 
                       SQLiteFileName = NULL,
                       randomize = FALSE) {
  
  if ((is.null(MSSQLConnectionString)) && (is.null(SQLiteFileName))) {
    stop('You must specify a either a MSSQLConnectionString for SQL Server or a SQLiteFileName for SQLite')
  }
  
  if (isTRUE(randomize)) {
    orderPres <- grep("order", tolower(query))
    
    if (length(orderPres == 0)) {
      stop("You cannot randomize while using the SQL order keyword.")
    }
    query <- paste0(query, " ORDER BY NEWID()")
  }
  
  # TODO: if debug: time this operation and print time spent to pull data.
  if (!is.null(MSSQLConnectionString)) {
    con <- DBI::dbConnect(odbc::odbc(),
                          .connection_string = MSSQLConnectionString)
  } else if (!is.null(SQLiteFileName)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = SQLiteFileName)
  }
  
  tryCatch(df <- DBI::dbGetQuery(con, query),
           error = function(e) {
             e$message <- "Your SQL likely contains an error."
             stop(e)
           })
  
  # Close connection
  DBI::dbDisconnect(con)
  
  # For SQLite, convert DTS cols from char to POSIX - SQLite as no date type
  if (!is.null(SQLiteFileName)) {
    colNumsDTS <- grep("DTS", names(df))
    df[colNumsDTS] <- lapply(df[colNumsDTS], as.POSIXct)
  }
  
  # Make sure there are enough rows to actually do something useful.
  if (is.null(nrow(df))) {
    cat(df)  # Print the SQL error, which is contained in df.
    stop("Your SQL contains an error.")
  }
  if (nrow(df) == 0) {
    warning("Zero rows returned from SQL. ",
            "Adjust your query to return more data!")
  }
  df  # Return the selected data.
}

#' @title
#' Write data to database
#' @description Write data frame to database table via ODBC connection
#' @param MSSQLConnectionString A string specifying the driver, server, 
#' database, and whether Windows Authentication will be used.
#' @param df Dataframe that hold the tabular data
#' @param SQLiteFileName A string. If dbtype is SQLite, here one specifies the 
#' database file to query from
#' @param tableName String. Name of the table that receives the new rows
#' @param addSAMUtilityColumns Boolean. Whether to add Health Catalyst-related
#' date-time stamp, BindingID, and BindingNM to df before saving to db.
#' @param connectionString Deprecated. A string specifying the driver, server, 
#' database, and whether Windows Authentication will be used. See 
#' ?MSSQLConnectionString instead.
#' @return Nothing
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @seealso \code{\link{selectData}}
#' @examples
#' 
#' \donttest{
#' # This example is specific to SQL Server.
#' 
#' # To instead pull data from Oracle see here 
#' # https://cran.r-project.org/web/packages/ROracle/ROracle.pdf
#' # To pull data from MySQL see here 
#' # https://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf
#' # To pull data from Postgres see here 
#' # https://cran.r-project.org/web/packages/RPostgreSQL/RPostgreSQL.pdf 
#'
#' # Before running this example, create the table in SQL Server via
#' # CREATE TABLE [dbo].[HCRWriteData](
#' # [a] [float] NULL,
#' # [b] [float] NULL,
#' # [c] [varchar](255) NULL)
#' 
#' connectionString <- '
#'   driver={SQL Server};
#'   server=localhost;
#'   database=SAM;
#'   trustedConnection=true
#'   '
#'
#' df <- data.frame(a=c(1,2,3),
#'                  b=c(2,4,6),
#'                  c=c('one','two','three'))
#'
#' writeData(MSSQLConnectionString = connectionString, 
#'           df = df, 
#'           tableName = 'HCRWriteData')
#' }
#' 
#' \dontrun{
#' #This example shows the RODBC way of writing to a non-default schema while 
#' #ODBC is being fixed. Here is a link to the non-default issue in ODBC: 
#' #https://github.com/rstats-db/odbc/issues/91
#' 
#' #First, create this table in SQL Server using a non-default schema. The 
#' #example creates this table in the SAM database on localhost. You will also 
#' #need to create a new schema(Cardiovascular) in SSMS for this specific 
#' #example to work.
#' #CREATE TABLE [Cardiovascular].[TestTable](
#' #[a] [float] NULL,
#' #[b] [float] NULL,
#' #[c] [varchar](255) NULL)
#' 
#' # Install the RODBC pacakge onto your machine. You only need to do this one 
#' # time.
#' #install.packages("RODBC")
#' 
#' # Load the package
#' library(RODBC)
#' 
#' # Create a connection to work with
#' con <- RODBC::odbcDriverConnect('driver={SQL Server};
#'                                 server=localhost;
#'                                 database=SAM;
#'                                 trusted_connection=true')
#' 
#' # Df write to SQL Server. df columns names must match the SQL table in SSMS.
#' df <- data.frame(a = c(10, 20, 30),
#'                  b = c(20, 40, 60),
#'                  c = c("oneT", "twoT", "threeT"))
#'
#' # Write the df to the SQL table                 
#' RODBC::sqlSave(con, df, "Cardiovascular.TestTable", append = TRUE, 
#'                rownames = FALSE)
#' 
#' # Verify that the table was written to 
#' confirmDf <- RODBC::sqlQuery(con, 'select * from Cardiovascular.TestTable') 
#' head(confirmDf)
#' }
writeData <- function(MSSQLConnectionString = NULL, 
                      df, 
                      SQLiteFileName = NULL,
                      tableName,
                      addSAMUtilityColumns = FALSE,
                      connectionString = NULL) {
  
  if (!is.null(connectionString)) {
    stop(paste0('The connectionString parameter has been deprecated. ',
                'Use MSSQLConnectionString instead'))
  }
  
  # TODO: if debug: time this operation and print time spent to pull data.
  if ((is.null(MSSQLConnectionString)) && (is.null(SQLiteFileName))) {
    stop(paste0('You must specify a either a MSSQLConnectionString for ',
                'SQL Server or a SQLiteFileName for SQLite'))
  }
  
  if (!is.null(MSSQLConnectionString)) {
    con <- DBI::dbConnect(odbc::odbc(),
                          .connection_string = MSSQLConnectionString)
  } else if (!is.null(SQLiteFileName)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = SQLiteFileName)
  }

  if (isTRUE(addSAMUtilityColumns)) {
    df <- healthcareai::addSAMUtilityCols(df)
  }
  
  # For SQLite, convert DTS cols from POSIX to char - SQLite as no date type
  if (!is.null(SQLiteFileName)) {
    colNumsDTS <- grep("DTS", names(df))
    df[colNumsDTS] <- lapply(df[colNumsDTS], as.character)
  }
  
  DBI::dbWriteTable(conn = con, 
                    name = tableName, 
                    value = df,
                    append = TRUE)

  # Close connection
  DBI::dbDisconnect(con)

  # TODO: get success and # of inserted rows from dbWriteTable function  
  if (is.null(MSSQLConnectionString)) {
    cat(nrow(df), "rows were inserted into the SQLite table", tableName)  
  } else {
    cat(nrow(df), "rows were inserted into the SQL Server table", tableName)
    
  }
}