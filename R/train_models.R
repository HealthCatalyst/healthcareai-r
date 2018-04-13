#' Fit models
#'
#' @param ... parsed and used as needed. E.g. hyperparameters from flash_models
#'   or tune_depth from tune_models
#'
#' @return list of models to be passed with details to as.model_list
#' @noRd
train_models <- function(d, outcome, models, metric, train_control, tune_method,
                         hyperparameters, ...) {
  dots <- list(...)
  mes <- if (tune_method == "none") "Training at fixed values: " else "Training with cross validation: "
  y <- dplyr::pull(d, !!outcome)
  d <- dplyr::select(d, -!!outcome)
  dim_x <- dim(d)
  tune_grids <-
    if (!is.null(hyperparameters)) {
      hyperparameters
    } else {
      make_tune_grids(models, tune_method, n = dim_x[1], k = dim_x[2],
                      tune_depth = if (tune_method == "none") 1L else dots$tune_depth,
                      model_class = if (is.factor(y)) "classification" else "regression")
    }

  train_list <-
    lapply(models, function(model) {

      tune_grid <- as.data.frame(unname(tune_grids[model]))

      # We use kknn and ranger, but user input is "knn" and "rf"
      model <- translate_model_names(model)
      message(mes, caret::getModelInfo(model)[[1]]$label)

      # Reduce kmax for kknn. train either takes a string or a list like what's returned here
      # If not tuning, the piece that adjust_knn edits isn't called
      # Instead of this: train_args$method <- get_caret_method(model)
      train_args <- list(x = d,
                         y = y,
                         method = model,
                         metric = metric,
                         trControl = train_control,
                         tuneGrid = tune_grid)

      # Add arguments specific to models
      if (model == "ranger")
        train_args$importance <- "impurity"

      suppressPackageStartupMessages(
        do.call(caret::train, train_args))
    })
  structure(train_list, positive_class = levels(y)[1])
}
