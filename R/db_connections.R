#' @title
#' Build a connection string for use with MSSQL and dbConnect
#' @description Handy utility to build a connection string to pass into
#' \code{DBI::dbConnect}. Accepts trusted connections or username/password.
#' @param server A string, quoted, required. The name of the server you are
#' trying to connect to.
#' @param driver A string, quoted, optional. Defaults to "SQL Server", but use
#' any driver you like.
#' @param database A string, quoted, optional. If provided, connection string
#' will include a specific database. If NA (default), it will connect to master
#' and you'll have to specify the database when running a query.
#' @param trusted Logical, optional, defaults to TRUE. If FALSE, you must use a
#' user_id and password.
#' @param user_id A string, quoted, optional. Don't include if using trusted.
#' @param password A string, quoted, optional. Don't include if using trusted.
#' @return A connection string
#' @seealso \code{\link{db_read}}
#' @export
#' @examples
#' \dontrun{
#' my_con <- build_connection_string(server = "localhost")
#' con <- DBI::dbConnect(odbc::odbc(), .connection_string = my_con)
#'
#' # with username and password
#' my_con <- build_connection_string(server = "localhost",
#'                                   user_id = "jules.winnfield",
#'                                   password = "pathoftherighteous")
#' con <- DBI::dbConnect(odbc::odbc(), .connection_string = my_con)
#' }
build_connection_string <- function(server,
                                    driver = "SQL Server",
                                    database,
                                    trusted=TRUE,
                                    user_id,
                                    password) {
  # Error checks
  if (!is.character(server))
    stop("You must provide a quoted server name")

  if (missing(database))
    database <- NA

  # Change trusted to false if user/pass are provided
  if (!missing(user_id) & !missing(password))
    trusted <- FALSE

  # Build connection string
  con_str <- paste0("driver={", driver, "};",
                    "server={", server, "};")

  if (is.character(database)) {
    con_str <- paste0(con_str,
                      "database=", database, ";")
  }

  if (rlang::is_true(trusted)) {
    con_str <- paste0(con_str,
                      "trusted_connection=true;")
  } else if (!missing(user_id) & !missing(password)) {
    con_str <- paste0(con_str,
                      "uid=", user_id, ";",
                      "pwd=", password, ";")
  } else {
    stop("You must use trusted=TRUE or provide a user_id and password.")
  }
  return(con_str)
}

#' @title
#' Read from a SQL Server database table
#' @description Use a database connection to read from an existing SQL Server
#' table with a SQL query.
#' @param con An odbc database connection. Can be made using
#' \code{build_connection_string}. Required.
#' @param query A string, quoted, required. This sql query will be executed
#' against the database you are connected to.
#' @param pull_into_memory Logical, optional, defaults to TRUE. If FALSE,
#' \code{db_read} will create a reference to the queried data rather than
#' pulling into memory. Set to FALSE for very large tables.
#' @details Use \code{pull_into_memory} when working with large tables.
#' Rather than returning the data into memory, this
#' function will return a reference to the specified query. It will be executed
#' only when needed, in a "lazy" style. Or, you can execute using the
#' \code{collect()} function.
#' @return A tibble of data or reference to the table.
#' @seealso \code{\link{build_connection_string}}
#' importFrom dbplyr as.sql
#  ^ This is a placeholder. We need dbplyr as a backend for dplyr's db functionality
#' @export
#' @examples
#' \dontrun{
#' my_con <- build_connection_string(server = "HPHI-EDWDEV")
#' con <- DBI::dbConnect(odbc::odbc(), .connection_string = my_con)
#' d <- db_read(con,
#'              "SELECT TOP 10 * FROM [Shared].[Cost].[FacilityAccountCost]")
#'
#' # Get a reference and collect later
#' ref <- db_read(con,
#'                "SELECT TOP 10 * FROM [Shared].[Cost].[FacilityAccountCost]",
#'                pull_into_memory = FALSE)
#' d <- collect(ref)
#' }
#'
db_read <- function(con,
                    query,
                    pull_into_memory = TRUE) {

  if (class(con)[1] != "Microsoft SQL Server") {
    stop("con needs to be a Microsoft SQL Server database connection.")
  }
  # Pull pointer to data using dplyr
  d <- dplyr::tbl(con, dplyr::sql(query))

  if (pull_into_memory) {
    d <- d %>% collect()
  }

  return(d)
}

#' @title
#' Add SAM utility columns to table
#' @description When working in a Health Catalyst Source Area Mart (SAM),
#' utility columns are added automatically when running a non-R binding
#' @param d A dataframe
#' @return A dataframe with three additional columns
#' @import dplyr
#' @export
#' @examples
#' d <- data.frame(a = c(1,2,NA,NA),
#'                  b = c(100,300,200,150))
#' d <- add_SAM_utility_cols(d)
#'
add_SAM_utility_cols <- function(d) {
  d <- d %>%
    mutate(BindingID = 0,
           BindingNM = "R",
           LastLoadDTS = Sys.time())
  return(d)
}




# Old functions ------------------

#' @title
#' Defunct. See \code{\link{db_read}}
#' @description Removed in v2.0.0
#' @param ... Garbage collector
#' @export
#'
selectData <- function(...) {
  .Defunct("db_read")
}

#' @title Defunct. See
#' \href{https://docs.healthcare.ai/articles/site_only/db_connections.html}{this
#' vignette} for help writing to databases.
#' @description Removed in v2.0.0
#' @param ... Garbage collector
#' @export
#'
writeData <- function(...) {
  .Defunct("db_write")
}
