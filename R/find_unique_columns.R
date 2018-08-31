#' Given character vector of already ignored (or outcome) columns, find other
#' all-unique character columns, warn about and return them. Returns empty
#' character vector if there are none.
#' @noRd
find_columns_to_ignore <- function(d, already_ignored = character()) {
  all_unique_chars <- setdiff(find_unique_columns(d), already_ignored)
  if (length(all_unique_chars)) {
    warning("The following column(s) have a unique value for every row so will be ignored: ",
            paste0(all_unique_chars, collapse = ", "))
  }
  return(all_unique_chars)
}

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
      (is.character(col) || is.factor(col)) &&
        length(unique(col)) == nrow(data)) %>%
    names()
  date_columns <- map_lgl(unique_columns, ~{
    grepl("DTS$", .x)
  }
  )
  unique_columns <- unique_columns[!date_columns]
  return(unique_columns)
}
