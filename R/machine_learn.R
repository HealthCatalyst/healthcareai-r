#' Prepare data and train machine learning models
#'
#' @param d data frame
#' @param outcome target column, unquoted
#' @param ... identifier columns not to be used in model training, unquoted
#' @param models models to be trained. See \code{\link{supported_models}}
#' @param impute Logical, if TRUE (default) missing values will be filled by
#'   \code{\link{hcai_impute}}
#'
#' @return model_list object ready to make predictions via
#'   \code{\link{predict.model_list}}
#' @export
#'
#' @details This is a high-level wrapper function. For finer control of
#'   data cleaning and preparation use \code{\link{prep_data}} or the functions
#'   it wraps. For finer control of model tuning use \code{\link{tune_models}}.
#'
#' @examples
#' # Split data into training and test sets using a subset of the data for speed
#' test_data <- pima_diabetes[1:10, ]
#' training_data <- pima_diabetes[11:100, ]
#'
#' # Clean and prep the data, tune algorithms over hyperparameter values to predict diabetes
#' models <- machine_learn(training_data, diabetes)
#'
#' # Make predictions (predicted probability of diabetes) on test data
#' predict(models, test_data)
#'
#' # Predict numeric outcomes simply by specifying the name of the outcome variable
#' age_model <- machine_learn(training_data, age)
#'
#' # If new data isn't specifed, get predictions on training data
#' predict(age_model)
machine_learn <- function(d, outcome, ..., models = c("rf", "knn"), impute = TRUE) {
  outcome <- rlang::enquo(outcome)
  d %>%
    prep_data(..., outcome = !!outcome, impute = impute) %>%
    tune_models(outcome = !!outcome, models = models)
}
