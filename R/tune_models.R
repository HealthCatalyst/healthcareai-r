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
#' @param tune_method How to search hyperparameter space? Default = "random".
#' @param hyperparameters Currently not supported.
#' @param model_class "regression" or "classification". If not provided, this
#'   will be determined by the class of `outcome` with the determination
#'   displayed in a message.
#'
#' @export
#' @importFrom kknn kknn
#' @importFrom ranger ranger
#' @importFrom dplyr mutate
#' @importFrom rlang quo_name
#'
#' @seealso \code{\link{prep_data}}, \code{\link{predict.model_list}},
#'   \code{\link{supported_models}}
#'
#' @return A model_list object
#' @details Note that this function is training a lot of models (100 by default)
#'   and so can take a while to execute. In general a model is trained for each
#'   hyperparameter combination in each fold for each model, so run time is a
#'   function of length(models) x n_folds x tune_depth. At the default settings,
#'   a 1000 row, 10 column data frame should complete in about 30 seconds on a
#'   good laptop.
#'
#' @examples
#' \dontrun{
#' ### Takes ~20 seconds
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
#' # Extract confusion matrix for random forest (the model with best-performing
#' # hyperparameter values is used)
#' caret::confusionMatrix(m$`Random Forest`, norm = "none")
#'
#' # Compare performance of algorithms at best hyperparameter values
#' rs <- resamples(m)
#' dotplot(rs)
#' }
tune_models <- function(d,
                        outcome,
                        models,
                        metric,
                        positive_class,
                        n_folds = 5,
                        tune_depth = 10,
                        tune_method = "random",
                        hyperparameters,
                        model_class) {

  if (n_folds <= 1)
    stop("n_folds must be greater than 1.")

  model_args <- setup_training(d, rlang::enquo(outcome), model_class, models, metric, positive_class)
  # Pull each item out of "model_args" list and assign in this environment
  for (arg in names(model_args))
    assign(arg, model_args[[arg]])

  # Set up cross validation details
  train_control <- setup_train_control(tune_method, model_class, metric, n_folds)
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

  y <- dplyr::pull(d, !!outcome)
  d <- dplyr::select(d, -!!outcome)
  # Loop over models, tuning each
  train_list <-
    lapply(models, function(model) {
      message("Running cross validation for ",
              caret::getModelInfo(model)[[1]]$label)
      # Reduce kmax for kknn
      if (model == "kknn")
        model <- adjust_knn()
      # Train models
      suppressPackageStartupMessages(
        caret::train(x = d,
                     y = y,
                     method = model,
                     metric = metric,
                     trControl = train_control,
                     tuneLength = tune_depth
        )
      )
    })

  train_list <- as.model_list(listed_models = train_list,
                              tuned = TRUE,
                              target = rlang::quo_name(outcome),
                              recipe = recipe,
                              positive_class = levels(y)[1]) %>%
    structure(timestamp = Sys.time())
  return(train_list)
}
