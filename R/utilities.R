#' Check column in data frame for any missingness and error if found
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

#' find_new_levels
#'
#' @return A list, of length zero if all observed character values/factor levels
#'   in new_df were observed in ref_df, otherwise with names of variable names
#'   and character vectors of previously unobserved values.
#'   Variables in new_df not present in ref_df are ignored.
#' @noRd
find_new_levels <- function(new_df, ref_df) {
  new_df <- new_df[, names(new_df) %in% names(ref_df), drop = FALSE]
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

#' compare_dfs
#' @noRd
#' @return list of variables in both, only in training, and only in predicting.
#'   Variables of the same name but different type (is/is-not numeric) will
#'   appear in both.
compare_dfs <- function(training, predicting) {
  t_vars <- get_classes_sorted(training)
  p_vars <- get_classes_sorted(predicting)
  list(
    both = dplyr::inner_join(t_vars, p_vars, by = c("variable", "is_numeric"))[["variable"]],
    training_only = dplyr::anti_join(t_vars, p_vars, by = c("variable", "is_numeric"))[["variable"]],
    predicting_only = dplyr::anti_join(p_vars, t_vars, by = c("variable", "is_numeric"))[["variable"]]
  )
}

#' get_classes_sorted - Utility for compare_dfs
#' @noRd
get_classes_sorted <- function(d) {
  classes <- purrr::map_lgl(d, is.numeric)
  broom::tidy(classes) %>%
    setNames(c("variable", "is_numeric")) %>%
    dplyr::arrange(variable)
}


#' Modify model object or predicted DF if PR. Otherwise, return as is.
#' @param object model_list
#' @return model_list
#' @noRd
change_pr_metric <- function(object) {
  if (is.model_list(object)) {
    # AUC is caret's code for PR
    if (object[[1]]$metric == "AUC") {
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
