#' @title
#' Write to an MSSQL database table
#' @description Use a database connection to write to a table in a specified
#' schema.
#' @param d A dataframe or tibble to write to the database. Required.
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
#' db_write(conn)
#' }
db_write <- function(d,
                     con,
                     table_name,
                     schema = "dbo",
                     overwrite = FALSE) {

  # Error checks
  if (!is.data.frame(d)) {
    stop("\"d\" must be a data frame.")
  }
  if (class(con) != "RODBC") {
    stop("You must use an RODBC connection for this function")
  }

  safe <- TRUE
  append <- TRUE
  if (isTRUE(overwrite)) {
    safer <- FALSE
    append <- FALSE
  }

  out <- RODBC::sqlSave(channel = con,
                        dat = d,
                        tablename = paste0(schema, ".", table_name),
                        append = append,
                        safer = safer)


  if (isTRUE(overwrite)) {
    res <- paste(nrow(d), "rows successfully written.")
  } else {
    res <- paste(nrow(d), "rows successfully appended.")
  }
  return(invisible(res))
}
