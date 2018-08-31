#' Tune multiple machine learning models using cross validation to optimize
#' performance
#'
#' @param d A data frame from \code{\link{prep_data}}. If you want to prepare
#' your data on your own, use \code{prep_data(..., no_prep = TRUE)}.
#' @param outcome Optional. Name of the column to predict. When omitted the
#'   outcome from \code{\link{prep_data}} is used; otherwise it must match the
#'   outcome provided to \code{\link{prep_data}}.
#' @param models Names of models to try. See \code{\link{get_supported_models}}
#'   for available models. Default is all available models.
#' @param metric Which metric should be used to assess model performance?
#'   Options for classification: "ROC" (default) (area under the receiver
#'   operating characteristic curve) or "PR" (area under the precision-recall
#'   curve). Options for regression: "RMSE" (default) (root-mean-squared error,
#'   default), "MAE" (mean-absolute error), or "Rsquared." Options for
#'   multiclass: "Accuracy" (default) or "Kappa" (accuracy, adjusted for class
#'   imbalance).
#' @param positive_class For classification only, which outcome level is the
#'   "yes" case, i.e. should be associated with high probabilities? Defaults to
#'   "Y" or "yes" if present, otherwise is the first level of the outcome
#'   variable (first alphabetically if the training data outcome was not already
#'   a factor).
#' @param n_folds How many folds to use in cross-validation? Default = 5.
#' @param tune_depth How many hyperparameter combinations to try? Default = 10.
#'   Value is multiplied by 5 for regularized regression. Increasing this value
#'   when tuning XGBoost models may be particularly useful for performance.
#' @param hyperparameters Optional, a list of data frames containing
#'   hyperparameter values to tune over. If NULL (default) a random,
#'   \code{tune_depth}-deep search of the hyperparameter space will be
#'   performed. If provided, this overrides tune_depth. Should be a named list
#'   of data frames where the names of the list correspond to models (e.g. "rf")
#'   and each column in the data frame contains hyperparameter values. See
#'   \code{\link{hyperparameters}} for a template. If only one model is
#'   specified to the \code{models} argument, the data frame can be provided
#'   bare to this argument.
#' @param model_class "regression" or "classification". If not provided, this
#'   will be determined by the class of `outcome` with the determination
#'   displayed in a message.
#' @param model_name Quoted, name of the model. Defaults to the name of the
#' outcome variable.
#' @param allow_parallel Logical, defaults to FALSE. If TRUE and a parallel
#'   backend is set up (e.g. with \code{doMC}) models with support for parallel
#'   training will be trained across cores.
#'
#' @export
#' @importFrom rlang quo_name
#'
#' @seealso For setting up model training: \code{\link{prep_data}},
#'   \code{\link{supported_models}}, \code{\link{hyperparameters}}
#'
#'   For evaluating models: \code{\link{plot.model_list}},
#'   \code{\link{evaluate.model_list}}
#'
#'   For making predictions: \code{\link{predict.model_list}}
#'
#'   For faster, but not-optimized model training: \code{\link{flash_models}}
#'
#'   To prepare data and tune models in a single step:
#'   \code{\link{machine_learn}}
#'
#' @return A model_list object. You can call \code{plot}, \code{summary},
#'   \code{evaluate}, or \code{predict} on a model_list.
#' @details Note that this function is training a lot of models (100 by default)
#'   and so can take a while to execute. In general a model is trained for each
#'   hyperparameter combination in each fold for each model, so run time is a
#'   function of length(models) x n_folds x tune_depth. At the default settings,
#'   a 1000 row, 10 column data frame should complete in about 30 seconds on a
#'   good laptop.
#'
#' @examples
#' \dontrun{
#' ### Examples take about 30 seconds to run
#' # Prepare data for tuning
#' d <- prep_data(pima_diabetes, patient_id, outcome = diabetes)
#'
#' # Tune random forest, xgboost, and regularized regression classification models
#' m <- tune_models(d)
#'
#' # Get some info about the tuned models
#' m
#'
#' # Get more detailed info
#' summary(m)
#'
#' # Plot performance over hyperparameter values for each algorithm
#' plot(m)
#'
#' # To specify hyperparameter values to tune over, pass a data frame
#' # of hyperparameter values to the hyperparameters argument:
#' rf_hyperparameters <-
#'   expand.grid(
#'     mtry = 1:5,
#'     splitrule = c("gini", "extratrees"),
#'     min.node.size = 1
#'   )
#' grid_search_models <-
#'   tune_models(d = d,
#'               outcome = diabetes,
#'               models = "rf",
#'               hyperparameters = list(rf = rf_hyperparameters)
#'   )
#' plot(grid_search_models)
#' }
tune_models <- function(d,
                        outcome,
                        models,
                        metric,
                        positive_class,
                        n_folds = 5,
                        tune_depth = 10,
                        hyperparameters = NULL,
                        model_class,
                        model_name = NULL,
                        allow_parallel = FALSE) {

  if (n_folds <= 1)
    stop("n_folds must be greater than 1.")
  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models,
                               metric = metric, positive_class, n_folds)
  metric <- model_args$metric

  # Pull each item out of "model_args" list and assign in this environment
  for (arg in names(model_args))
    assign(arg, model_args[[arg]])

  # Set up cross validation details
  train_control <- setup_train_control(model_class, metric, n_folds)
  hyperparameters <-
    if (!is.null(hyperparameters)) {
      # If only tuning one model and hyperparameters aren't in a list, put them in one
      if (is.data.frame(hyperparameters)) {
        if (length(models) == 1) {
          hyperparameters <- structure(list(hyperparameters), names = models)
        } else {
          stop("You passed a data frame to hyperparameters. Either put it in a list ",
               "with names matching models or specify the one model you want to tune via the `models` argument.")
        }
      }
      tuned <- max(purrr::map_int(hyperparameters, nrow)) > 1
      hyperparameters
    } else {
      tuned <- tune_depth > 1
      get_random_hyperparameters(models = models, n = nrow(d), k = ncol(d) - 1,
                                 tune_depth = tune_depth, model_class = model_class)
    }

  if (metric == "PR")
    metric <- "AUC" # For caret internal function

  # Rough check for training that will take a while, message if so
  check_training_time(ddim = dim(d), n_folds = n_folds,
                      hpdim = purrr::map_int(hyperparameters, nrow)) %>%
    message()

  train_list <- train_models(d, outcome, models, metric, train_control,
                             hyperparameters, tuned, allow_parallel)

  train_list <- as.model_list(listed_models = train_list,
                              tuned = tuned,
                              target = rlang::quo_name(outcome),
                              model_class = model_class,
                              recipe = recipe,
                              positive_class = attr(train_list, "positive_class"),
                              model_name = model_name,
                              best_levels = best_levels,
                              original_data_str = original_data_str,
                              versions = attr(train_list, "versions")) %>%
    structure(timestamp = Sys.time())
  return(train_list)
}
