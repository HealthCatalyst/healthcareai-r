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
#' @param n_folds How many folds to train the model on. Default = 5, minimum =
#'   2. Whie flash_models doesn't use cross validation to tune hyperparameters,
#'   it trains \code{n_folds} models to evaluate performance out of fold.
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
#' # Prepare data
#' prepped_data <- prep_data(pima_diabetes, patient_id, outcome = diabetes)
#'
#' # Simplest use. Get models quickly at default hyperparameter values
#' flash_models(prepped_data, diabetes)
#'
#' # Set non-default hyperparameter values by passing a list of lists to \code{hyperparameters}
#' models <-
#'   flash_models(d = prepped_data,
#'                outcome = diabetes,
#'                hyperparameters = list(
#'                  rf = list(
#'                    mtry = 3,
#'                    splitrule = "gini",
#'                    min.node.size = 1
#'                  ),
#'                  knn = list(
#'                    kmax = 3,
#'                    distance = 2,
#'                    kernel = "gaussian"
#'                  )
#'                )
#'   )
#' summary(models)
#'
#' # Speed comparison of no tuning with flash_models vs. tuning with tune_models:
#' \dontrun{
#'   # ~40 seconds:
#'   system.time(
#'     tune_models(prepped_data, diabetes)
#'   )
#'   # ~6 seconds:
#'   system.time(
#'     flash_models(prepped_data, diabetes)
#'   )
#' }
flash_models <- function(d,
                         outcome,
                         model_class,
                         models = c("rf", "knn"),
                         n_folds = 5,
                         hyperparameters,
                         metric) {

  if (missing(hyperparameters))
    hyperparameters <- get_hyperparameter_defaults(models)
  models <- tolower(models)
  names(hyperparameters) <- tolower(names(hyperparameters))
  if (!dplyr::setequal(names(hyperparameters), models))
      stop("`models` and names of the list passed to `hyperparameters` must match (case-insensitive).",
           "You provided:\nmodels: ", paste(models, collapse = ", "),
           "\nhyperparameter names:", paste(names(hyperparameters), collapse = ", "))

  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models, metric)
  # Pull each item out of "model_args" list and assign in this environment
  for (arg in names(model_args))
    assign(arg, model_args[[arg]])

  train_control <- setup_train_control(tune_method = "none", model_class, metric, n_folds)
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

  train_list <- add_model_attrs(models = train_list,
                                recipe = attr(d, "recipe"),
                                tuned = FALSE,
                                target = rlang::quo_name(outcome))
  return(train_list)
}
