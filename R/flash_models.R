#' Train models without tuning for performance
#'
#' @param d A data frame
#' @param outcome Name of the column to predict
#' @param model_class "regression" or "classification". If not provided, this
#'   will be determined by the class of `outcome` with the determination
#'   displayed in a message.
#' @param models Names of models to try, by default "rf" for random forest and
#'   "knn" for k-nearest neighbors. See \code{\link{supported_models}} for
#'   available models.
#' @param hyperparameters Optional list of hyperparameters to use. If missing,
#'   default values will be used. If provided, must be a named list of named
#'   lists where the outer list contains models and the inner lists contain
#'   hyperparameter values. See
#'   \code{healthcareai:::get_hyperparameter_defaults()} for a template.
#' @param metric What metric to use to assess model performance? Options for
#'   regression: "RMSE" (root-mean-squared error, default), "MAE" (mean-absolute
#'   error), or "Rsquared." For classification: "ROC" (area under the receiver
#'   operating characteristic curve), or "PR" (area under the precision-recall
#'   curve).
#'
#' @export
#' @seealso \code{\link{tune_models}}, \code{\link{prep_data}},
#'   \code{\link{predict.model_list}}, \code{\link{supported_models}}
#'
#' @return A model_list object
#' @details This function has two major differences from
#'   \code{\link{tune_models}}: \enumerate{\item{It uses default hyperparameter
#'   values to train models instead of using cross-validation to optimize
#'   hyperparameter values for predictive performance.} \item{It is much
#'   faster.}}
#'
#' @examples
flash_models <- function(d,
                         outcome,
                         model_class,
                         models = c("rf", "knn"),
                         hyperparameters,
                         metric) {

  if (missing(hyperparameters))
    hyperparameters <- get_hyperparameter_defaults(models)
  if (!dplyr::setequal(names(hyperparameters), models))
      stop("`models` and names of the list passed to `hyperparameters` must match. ",
           "You provided:\nmodels: ", paste(models, collapse = ", "),
           "\nhyperparameter names:", paste(names(hyperparameters), collapse = ", "))

  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models, metric)
  # Pull each item out of "model_args" list and assign in this environment
  for (arg in names(model_args))
    assign(arg, model_args[[arg]])

  train_control <- setup_train_control(tune_method = "none", model_class, metric)
  if (metric == "PR")
    metric <- "AUC"

  train_list <-
    lapply(models, function(model) {
      suppressPackageStartupMessages({
        tune_grid <- as.data.frame(hyperparameters[[translate_model_names(model)]])
        caret::train(x = dplyr::select(d, -!!outcome),
                     y = dplyr::pull(d, !!outcome),
                     method = model,
                     metric = metric,
                     trControl = train_control,
                     tuneGrid = tune_grid)
      })
    })

  # Add class
  train_list <- as.model_list(listed_models = train_list,
                              tuned = FALSE,
                              target = rlang::quo_name(outcome))

  # Add recipe object if one came in on d
  attr(train_list, "recipe") <- attr(d, "recipe")

  return(train_list)
}
