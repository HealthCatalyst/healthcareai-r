#' Train models without tuning for performance
#'
#' @param d A data frame
#' @param outcome Name of the column to predict
#' @param models Names of models to try, by default "rf" for random forest and
#'   "knn" for k-nearest neighbors. See \code{\link{supported_models}} for
#'   available models.
#' @param metric What metric to use to assess model performance? Options for
#'   regression: "RMSE" (root-mean-squared error, default), "MAE" (mean-absolute
#'   error), or "Rsquared." For classification: "ROC" (area under the receiver
#'   operating characteristic curve), or "PR" (area under the precision-recall
#'   curve).
#' @param positive_class For classification only, which outcome level is the
#'   "yes" case, i.e. should be associated with high probabilities? Defaults to
#'   "Y" or "yes" if present, otherwise is the first level of the outcome
#'   variable (first alphabetically if the training data outcome was not already
#'   a factor).
#' @param n_folds How many folds to train the model on. Default = 5, minimum =
#'   2. Whie flash_models doesn't use cross validation to tune hyperparameters,
#'   it trains \code{n_folds} models to evaluate performance out of fold.
#' @param hyperparameters Optional list of hyperparameters to use. If missing,
#'   default values will be used. If provided, must be a named list of named
#'   lists where the outer list contains models and the inner lists contain
#'   hyperparameter values. See
#'   \code{healthcareai:::get_hyperparameter_defaults()} for a template.
#' @param model_class "regression" or "classification". If not provided, this
#'   will be determined by the class of `outcome` with the determination
#'   displayed in a message.
#'
#' @export
#' @seealso \code{\link{tune_models}}, \code{\link{prep_data}},
#'   \code{\link{predict.model_list}}, \code{\link{supported_models}}
#'
#' @return A model_list object
#' @details This function has two major differences from
#'   \code{\link{tune_models}}: 1. It uses fixed hyperparameter values to train
#'   models instead of using cross-validation to optimize hyperparameter values
#'   for predictive performance, and, as a result, 2. It is much faster.
#'
#' @examples
#' \dontrun{
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
#' # ~40 seconds:
#' system.time(
#'   tune_models(prepped_data, diabetes)
#' )
#' # ~6 seconds:
#' system.time(
#'   flash_models(prepped_data, diabetes)
#' )
#' }
flash_models <- function(d,
                         outcome,
                         models = c("rf", "knn"),
                         metric,
                         positive_class,
                         n_folds = 5,
                         hyperparameters = NULL,
                         model_class) {

  models <- tolower(models)
  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models, metric)
  # Pull each item out of "model_args" list and assign in this environment
  for (arg in names(model_args))
    assign(arg, model_args[[arg]])

  train_control <- setup_train_control(model_class, metric, n_folds)
  if (metric == "PR")
    metric <- "AUC"

  # Don't train models where hyperparameters weren't provided, if any were provided
  if (!is.null(hyperparameters))
      models <- align_models_hyperparameters(models, hyperparameters)

  train_list <- train_models(d, outcome, models, metric, train_control,
                             tune_method = "none", hyperparameters = hyperparameters)
  train_list <- as.model_list(listed_models = train_list,
                              tuned = FALSE,
                              target = rlang::quo_name(outcome),
                              recipe = recipe,
                              positive_class = attr(train_list, "positive_class")) %>%
    structure(timestamp = Sys.time())
  return(train_list)
}

align_models_hyperparameters <- function(models, hyperparameters) {
  check_models(names(hyperparameters))
}
