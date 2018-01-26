train_models <- function(d, outcome, ..., models = c("rf", "knn"), impute = TRUE) {
  outcome <- rlang::enquo(outcome)
  y <- dplyr::pull(d, !!outcome)
  d %>%
    dplyr::select(-!!outcome) %>%
    prep_data(..., impute = impute) %>%
    dplyr::mutate(!!rlang::quo_name(outcome) := y) %>%
    tune_models(outcome = rlang::UQE(outcome), models = models)
}
