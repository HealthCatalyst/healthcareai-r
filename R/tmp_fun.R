#' Finds numeric columns
#'
#' @param d data frame
#'
#' @return column names that are numeric
#' @export
find_numeric_columns <- function(d) {
  which_are_numeric <- sapply(d, is.numeric)
  num_cols <- names(which_are_numeric)[which_are_numeric]
  return(num_cols)
}
