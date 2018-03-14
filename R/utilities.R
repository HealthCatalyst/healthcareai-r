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

#' Modify model object or predicted DF if PR. Otherwise, return as is.
#' @param object model_list
#' @return model_list
#' @noRd
change_pr_metric <- function(object) {
  if (is.model_list(object)) {
    if (object[[1]]$metric == "AUC") {  # PR was used
      object <-
        purrr::map(object, function(x) {
          x$metric <- "PR"
          names(x$results)[names(x$results) == "AUC"] <- "PR"
          return(x)
        })
    }
  } else if (is.hcai_predicted_df(object)) {
    if (attr(object, "model_info")$metric == "AUC")
      attr(object, "model_info")$metric <- "PR"
  }
  return(object)
}
