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
#' pulling into memory.
#' @details Use \code{pull_into_memory} when working with large tables.
#' Rather than returning the data into memory, this
#' function will return a reference to the specified query. It will be executed
#' only when needed, in a "lazy" style. Or, you can execute using the
#' \code{collect()} function.
#' @return A tibble of data or reference to the table.
#' @export
#' @examples
#' \dontrun{
#' my_con <- build_connection_string(server = "localhost",
#'                                   database = "myDB")
#' con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
#' d <- db_read(con, "SELECT * FROM my_table)
#'
#' # Get a reference and collect later # nolint
#' ref <- db_read(con, "SELECT * FROM my_table, pull_into_memory = FALSE)
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
  d <- tbl(con, sql(query))

  if (pull_into_memory) {
    d <- d %>% collect()
  }

  return(d)
}
