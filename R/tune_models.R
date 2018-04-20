#' Tune multiple machine learning models using cross validation to optimize
#' performance
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
#' @param n_folds How many folds to use in cross-validation? Default = 5.
#' @param tune_depth How many hyperparameter combinations to try? Defualt = 10.
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
#'
#' @export
#' @importFrom kknn kknn
#' @importFrom ranger ranger
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
#'   To prepare data and tune models in a single step: \code{\link{machine_learn}}
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
#' # Tune random forest and k-nearest neighbors classification models
#' m <- tune_models(d, outcome = diabetes)
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
                        model_class) {

  if (n_folds <= 1)
    stop("n_folds must be greater than 1.")

  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models, metric, positive_class)
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
  n_mod <- n_folds * tune_depth * length(models)
  obs <- nrow(d)
  if ( (obs > 1000 && n_mod > 10) || (obs > 100 && n_mod > 100) )
    message("You've chosen to tune ", n_mod, " models (n_folds = ", n_folds,
            " x tune_depth = ", tune_depth, " x ", "length(models) = ",
            length(models), ") on a ", format(obs, big.mark = ","), " row dataset. ",
            "This may take a while...")

  train_list <- train_models(d, outcome, models, metric, train_control, hyperparameters, tuned)
  train_list <- as.model_list(listed_models = train_list,
                              tuned = tuned,
                              target = rlang::quo_name(outcome),
                              recipe = recipe,
                              positive_class = attr(train_list, "positive_class")) %>%
    structure(timestamp = Sys.time())
  return(train_list)
}
