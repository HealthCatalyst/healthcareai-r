#' Get variable importance
#'
#' @param x model_list
#'
#' @return Data frame of variables and importance scores
#' @export
#'
#' @examples
#' m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes)
#' get_variable_importance(m)
get_variable_importance <- function(x) {
  importances <- lapply(x, safe_imp)
  have_imp <- purrr::map_lgl(importances, ~ is.null(.x$error))
  if (!any(have_imp))
    stop("Can't get variable importance for any of these models.")
  model_ranks <- rank_models(x)
  # Use the best model (min rank) where we have variable importance
  use <- which.min(model_ranks[have_imp])
  # When we do multiclass, we'll want to average across cols in imp object
  importances[[use]]$result[[1]][, 1, drop = FALSE] %>%
    tibble::rownames_to_column() %>%
    setNames(c("variable", "importance")) %>%
    dplyr::arrange(desc(importance)) %>%
    dplyr::mutate(variable = factor(variable),
                  variable = reorder(variable, -importance)) %>%
    as_tibble()
}

safe_imp <- purrr::safely(~ caret::varImp(.x, useModel = FALSE))

