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
      # Start list of arguments to train
      train_args <- list(x = d,
                         y = y,
                         method = model,
                         metric = metric,
                         trControl = train_control)

      # Add arguments specific to models
      if (model == "ranger")
        train_args$importance <- "impurity"
      # Reduce kmax for kknn. train either takes a string or a list like what's returned here
      # If not tuning, the piece that adjust_knn edits isn't called
      if (model == "kknn")
        train_args$method <- adjust_knn()

      # Add arguments specific to whether tuning or not
      if (tune) {
        train_args$tuneLength <- dots$tune_depth
      } else {
        tune_grid <- as.data.frame(dots$hyperparameters[[translate_model_names(model)]])
        train_args$tuneGrid  <- tune_grid
      }

      suppressPackageStartupMessages(
        do.call(caret::train, train_args))
    })
  structure(train_list, positive_class = levels(y)[1])
}
