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
    stop("There is missingness in ", rlang::get_expr(col_name),
         " that must be filled in. Consider using `impute()`.")
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
  lapply(names(ref), function(v)
    dplyr::setdiff(names(new[[v]]), names(ref[[v]]))) %>%
    setNames(names(ref))
}

#' ID factors as not-numeric and not-date. Find unique values in them.
#' @noRd
get_factor_levels <- function(d) {
  not_factors <- dplyr::union(names(d)[purrr::map_lgl(d, ~ is.numeric(.x))],
                              find_date_cols(d))
  d <- d[, !names(d) %in% not_factors, drop = FALSE]
  lapply(d, table, useNA = "ifany")
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
  tibble::tibble(variable = names(classes), is_numeric = classes) %>%
    dplyr::arrange(variable)
}

#' Modify model object or predicted DF if PR. Otherwise, return as is.
#' @param object model_list
#' @return model_list
#' @noRd
change_metric_names <- function(object) {
  metrics <- get_metric_names()
  old_metric <- if (is.model_list(object)) object[[1]]$metric else attr(object, "model_info")$metric
  # Only switch metrics if they're not already ours
  if (!old_metric %in% metrics$ours) {
    if (is.model_list(object)) {
      for (i in seq_along(object)) {
        switch_row <- which(metrics$caret == object[[i]]$metric)
        object[[i]]$metric <- metrics$ours[switch_row]
        names(object[[i]]$results)[names(object[[i]]$results) == metrics$caret[switch_row]] <-
          metrics$ours[switch_row]
      }
    } else if (is.predicted_df(object)) {
      attr(object, "model_info")$metric <-
        metrics$ours[metrics$caret == old_metric]
    }
  }
  return(object)
}

#' Returns the order of performance of models in m, ie the number in the first
#' position is the index of the best model
#' @noRd
order_models <- function(m) {
  mi <- extract_model_info(m)
  metric <-
    get_metric_names() %>%
    dplyr::filter(caret == mi$metric)
  perf <- do.call(rbind, purrr::map(names(m), ~ evaluate(m[.x])))
  out <- order(perf[, metric$ours], decreasing = m[[1]]$maximize)
  setNames(out, names(m)[out])
}

#' Function to skip specific tests if they are not being run on Appveyor.
#'
#' @description This function will skip a test if it's not being run on
#'   Appveyor. Used for SQL Server related tests, since we don't know if the
#'   environment will be present. These tests are are run on Appveyor.
#'
#' @noRd
skip_on_not_appveyor <- function() {
  if (identical(Sys.getenv("APPVEYOR"), "True")) {
    return()
  }
  testthat::skip("Not on Appveyor")
}

#' Whether var is quo or character returns d without it
#' @noRd
select_not <- function(d, var) {
  if (rlang::is_quosure(var))
    var <- rlang::quo_name(var)
  d[, -which(names(d) %in% var), drop = FALSE]
}

#' @export
dplyr::`%>%`

# Cut the middle out of string and replace with ...
trunc_char <- function(x, max_char) {
  if (max_char <= 5) {
    warning(max_char, " characters isn't enough to bookend, so I'll give you ",
            "the first ", max_char)
    x <- stringr::str_sub(x, end = max_char)
  }
  to_trunc <- stringr::str_length(x) > max_char
  if (any(to_trunc)) {
    end_char <- ceiling(max_char / 2) - 2
    x[to_trunc] <- paste0(stringr::str_sub(x, end = end_char), "...",
                          stringr::str_sub(x, start = -end_char))[to_trunc]
  }
  return(x)
}

#' Mode
#'
#' @param x Either a vector or a frequency table from \code{table}
#'
#' @return The modal value of x
#' @export
#'
#' @examples
#' x <- c(3, 1:5)
#' Mode(x)
#' Mode(table(x))
Mode <- function(x) {
  if (is.table(x)) {
    x <- names(sort(x, decreasing = TRUE))[1]
    suppressWarnings(if (!is.na(as.numeric(x))) x <- as.numeric(x))
    x
  } else {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
}

#' Finds a step object
#' @param x an object that contains a recipe
#' @param step_name the name of a step
#' @return the step object
#' @noRd
get_recipe_step <- function(x, step_name) {
  steps <- (x %>% attr("recipe"))$step
  loc <- purrr::map_lgl(steps, ~class(.x) %>% first() == step_name)
  step_object <-
    if (any(loc))
      steps[loc][[1]]
    else
      NULL
  return(step_object)
}
