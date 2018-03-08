#' @title
#' Write to an MSSQL database table
#' @description Use a database connection to write to a table in a specified
#' schema.
#' @param d A dataframe to write to the database. Required.
#' @param con An RODBC database connection. Can be made using
#' \code{RODBC::odbcDriverConnect}. Required.
#' @param table_name Character, required.
#' @param schema Character, optional. Defaults to "dbo".
#' @param overwrite Logical, optional, defaults to FALSE.
#' @details This function currently requires an RODBC connection to a specific
#' database. See examples for how to generate one. Column names and types in d
#' must match those in destination table.
#' @return Silently returns number of rows written to new table.
#' @export
#' @examples
#' \dontrun{
#' my_con <- build_connection_string(server = "localhost",
#'                                   database = "myDB")
#' rcon <- RODBC::odbcDriverConnect(my_con)
#'
#' db_write(sdfs)
#'
#' DBI::dbDisconnect(rcon)
#' }
db_write <- function(d,
                     con,
                     table_name,
                     schema = "dbo",
                     overwrite = FALSE) {
  # TODO: Do we we want to create the table if it doesn't exist?
  # TODO: currently fails with ugly error if column names don't match
  # TODO: fails if data types dont match and can't be coerced

  # Error checks
  if (!is.data.frame(d)) {
    stop("\"d\" must be a data frame.")
  }
  if (class(con) != "RODBC") {
    stop("You must use an RODBC connection for this function")
  }

  if (!stringr::str_detect(attr(con, "connection.string"), "DATABASE")) {
    stop("You must specify a database in the connection string.")
  }

  tablename <- paste0(schema, ".", table_name)
  if (!length(odbcTableExists(con, tablename, abort = FALSE))) {
    stop(paste("The table", tablename,
               "doesn't exist in the specified database."))
  }

  safer <- TRUE
  append <- TRUE
  if (isTRUE(overwrite)) {
    safer <- FALSE
    append <- FALSE
  }

  out <- RODBC::sqlSave(channel = con,
                        dat = d,
                        tablename = tablename,
                        append = append,
                        safer = safer,
                        rownames = FALSE)


  if (isTRUE(overwrite)) {
    res <- paste(nrow(d), "rows successfully written.")
  } else {
    res <- paste(nrow(d), "rows successfully appended.")
  }
  return(invisible(res))
}
