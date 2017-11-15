#' Check column in data frame for any missingness
#'
#' @param d data frame
#' @param col_name a column in d; either as a quosure'd variable, as captured by
#' rlang::enquo in the parent function, or as a string.
#'
#' @return TRUE if no missingness. Otherwise stops with an informative message.
#' @noRd
missing_check <- function(d, col_name) {
  if (!any(is.na(dplyr::pull(d, !!col_name)))) {
    return(TRUE)
  } else {
    called_from <- sys.call(1)[[1]]
    stop("Fill in missingness in ", rlang::get_expr(col_name),
         " before calling ", called_from)
  }
}
