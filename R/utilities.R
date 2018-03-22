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
#' @details new and ref can either be data frames of lists as returned by
#'   get_factor_levels.
#' @return A list, of length zero if all observed character
#'   values/factor levels in new were observed in ref, otherwise with
#'   names of variable names and character vectors of previously unobserved
#'   values. Variables in new not present in ref are ignored.
#' @noRd
find_new_levels <- function(new, ref) {
  if (is.data.frame(new))
    new <- get_factor_levels(new)
  if (is.data.frame(ref))
    ref <- get_factor_levels(ref)
  lapply(names(ref), function(v) dplyr::setdiff(new[[v]], ref[[v]])) %>%
    setNames(names(ref))
}

#' ID factors as not-numeric and not-date. Find unique values in them.
#' @noRd
get_factor_levels <- function(d) {
  not_factors <- dplyr::union(names(d)[purrr::map_lgl(d, ~ is.numeric(.x))],
                              find_date_cols(d))
  d <- d[, !names(d) %in% not_factors, drop = FALSE]
  lapply(d, function(x) as.character(unique(x)))
}

#' Take list of character vectors as from find_new_levels and format for
#' printing. Removes length-0 items from the list, and optionally removes NAs
#' from character vectors.
#'
#' @return character vector
#' @noRd
format_new_levels <- function(new_levels, remove_nas = FALSE) {
  purrr::map(names(new_levels), ~ {
    if (!length(new_levels[[.x]])) return(NULL) else {
      levs <- new_levels[[.x]]
      if (remove_nas)
        levs <- levs[!is.na(levs)]
      paste0("\n\t", .x, ": ", paste(levs, collapse = ", "))
    }
  }) %>%
    purrr::flatten()
}

#' Test whether all variables in a training dataframe are present and of the
#' same type in a prediction data frame
#' @noRd
#' @return Logical
dfs_compatible <- function(training, predicting) {
  t_vars <- get_classes_sorted(training)
  p_vars <- get_classes_sorted(predicting)
  joined <- dplyr::left_join(t_vars, p_vars, by = "variable")
  look_same <- isTRUE(all.equal(joined$is_numeric.x, joined$is_numeric.y))
  return(look_same)
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
