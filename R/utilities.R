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
    stop("Fill in missingness in ", rlang::get_expr(col_name),
         " before calling ", match.call())
  }
}

#' Function to skip specific tests if they are not being run on Appveyor.
#'
#' @description This function will skip a test if it's not being run on
#'   Appveyor. Used for SQL Server related tests, since we don't know if the
#'   environment will be present. These tests are are run on Appveyor.
#'
#' @noRd
#' @references \url{http://healthcareai-r.readthedocs.io}
skip_on_not_appveyor <- function() {
    if (identical(Sys.getenv("APPVEYOR"), "True")) {
        return()
    }
    testthat::skip("Not on Appveyor")
}

#' find_new_levels
#'
#' @return A list, of length zero if all observed character values/factor levels
#'   in new_df were observed in ref_df, otherwise with names of variable names
#'   and character vectors of previously unobserved values.
#' @noRd
find_new_levels <- function(new_df, ref_df) {
  levs <-
    dplyr::select_if(new_df, ~ !is.numeric(.x)) %>%
    purrr::map(~ unique(.x))
  purrr::map(names(levs), ~
    as.character(levs[[.x]][!levs[[.x]] %in% unique(ref_df[[.x]])])) %>%
    setNames(names(levs))
}

#' Take list of character vectors as from find_new_levels and format for
#' printing
#'
#' @return character vector
#' @noRd
format_new_levels <- function(new_levels) {
  purrr::map(names(new_levels), ~ {
    if (!length(new_levels[[.x]])) return(NULL) else
      paste0("\n\t", .x, ": ", paste(new_levels[[.x]], collapse = ", "))
  }) %>%
    purrr::flatten()
}
