#' Fit models
#'
#' @param ... parsed and used as needed. E.g. hyperparameters from flash_models
#'   or tune_depth from tune_models
#'
#' @return list of models to be passed with details to as.model_list
#' @noRd
train_models <- function(d, outcome, models, metric, train_control, tune, ...) {
  dots <- list(...)
  mes <- if (tune) "Training with cross validation: " else "Training at fixed values: "
  y <- dplyr::pull(d, !!outcome)
  d <- dplyr::select(d, -!!outcome)
  train_list <-
    lapply(models, function(model) {
      message(mes, caret::getModelInfo(model)[[1]]$label)
      train_args <- list(x = d,
                         y = y,
                         method = model,
                         metric = metric,
                         trControl = train_control)
      if (tune) {
        train_args$tuneLength <- dots$tune_depth
        # Reduce kmax for kknn
        if (model == "kknn")
          model <- adjust_knn()
      } else {
        tune_grid <- as.data.frame(dots$hyperparameters[[translate_model_names(model)]])
        train_args$tuneGrid  <- tune_grid
      }
      suppressPackageStartupMessages({
        do.call(caret::train, train_args)
      })
    })
  structure(train_list, positive_class = levels(y)[1])
}
