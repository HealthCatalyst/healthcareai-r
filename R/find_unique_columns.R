#' @title Find categorical columns with all unique values
#'
#' @description \code{find_unique_columns} will find categorical columns in your
#'   data that have all uniuqe values and then returns a character vector with
#'   length-zero if there are none or the names of all-unique, categorical
#'   columns. The usecase is that these columns should be ignored in model
#'   training.
#'
#' @param data A dataframe or tibble containing data to find unique columns.
#'
#' @return A character vector containing the names of the categorical columns
#'   with all unique values
#'
#' @noRd
#'
#' @examples
#' d <- data.frame(id_field = c("A","B","C","D"),
#' test1_field = c(10,20,30,40),
#' test2_field = c(100,200,300,300),
#' test3_field = c("A1","B1","B1","D1"),
#' test4_field = c("AA","BB","CC","DD"))
#' find_unique_columns(d)
find_unique_columns <- function(data) {
  unique_columns <-
    data %>%
    dplyr::select_if(function(col)
      is.numeric(col) == FALSE &&
        length(unique(col)) == nrow(data)) %>%
    names()
  return(unique_columns)
  }
