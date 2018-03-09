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
#' @export
#' @examples
#' my_con <- build_connection_string(server = "localhost")
#' # con <- DBI::dbConnect(odbc::odbc(), .connection_string = my_con) # nolint
#'
#' # with username and password
#' my_con <- build_connection_string(server = "localhost",
#'                                   user_id = "jules.winnfield",
#'                                   password = "pathoftherighteous")
#' # con <- DBI::dbConnect(odbc::odbc(), .connection_string = my_con) #nolint
#'
build_connection_string <- function(server,
                                    driver = "SQL Server",
                                    database = NA,
                                    trusted=TRUE,
                                    user_id = NA,
                                    password = NA) {
  # Error checks
  if (!is.character(server)) {
    stop("You must provide a quoted server name")
  }

  # Change trusted to false if user/pass are provided
  if (!is.na(user_id) & !is.na(password)) {
    trusted <- FALSE
  }

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
  } else if (!is.na(user_id) & !is.na(password)) {
    con_str <- paste0(con_str,
                      "uid=", user_id, ";",
                      "pwd=", password, ";")
  } else {
    stop("You must use trusted=TRUE or provide a user_id and password.")
  }
  return(con_str)
}
