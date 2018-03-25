#' Machine Learn
#'
#' @description Prepare data and train machine learning models.
#'
#' @param d A data frame
#' @param ... Columns to be ignored in model training, e.g. ID columns,
#'   unquoted.
#' @param outcome Name of the target column, i.e. what you want to predict.
#'   Unquoted. Must be named, i.e. you must specify \code{outcome = }
#' @param models Models to be trained, k-nearest neighbors and random forest by
#'   default. See \code{\link{supported_models}} for details.
#' @param impute Logical, if TRUE (default) missing values will be filled by
#'   \code{\link{hcai_impute}}
#'
#' @return model_list object ready to make predictions via
#'   \code{\link{predict.model_list}}
#' @export
#'
#' @details This is a high-level wrapper function. For finer control of data
#'   cleaning and preparation use \code{\link{prep_data}} or the functions it
#'   wraps. For finer control of model tuning use \code{\link{tune_models}}.
#'
#' @examples
#' # Split data into training and test sets using a subset of the data for speed
#' test_data <- pima_diabetes[1:10, ]
#' training_data <- pima_diabetes[11:100, ]
#'
#' ### Classification ###
#'
#' # Clean and prep the data, tune algorithms over hyperparameter values to predict diabetes
#' models <- machine_learn(training_data, outcome = diabetes)
#'
#' # Make predictions (predicted probability of diabetes) on test data
#' predict(models, test_data)
#'
#' ### Regression ###
#'
#' # Predict numeric outcomes simply by specifying the name of the outcome variable
#' age_model <- machine_learn(training_data, outcome = age)
#'
#' # If new data isn't specifed, get predictions on training data
#' predict(age_model)
machine_learn <- function(d, ..., outcome, models, impute = TRUE) {

  if (!is.data.frame(d))
    stop("\"d\" must be a data frame.")

  dots <- rlang::quos(...)
  ignored <- map_chr(dots, rlang::quo_name)
  if (length(ignored)) {
    not_there <- setdiff(ignored, names(d))
    if (length(not_there))
      stop("The following variable(s) were passed to the ... argument of machine_learn",
           " but are not the names of columns in the data frame: ",
           paste(not_there, collapse = ", "))
  }

  outcome <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(outcome)) {
    mes <- "You must provide an outcome variable to machine_learn."
    if (length(ignored))
      mes <- paste(mes, "Did you forget to specify `outcome = `?")
    stop(mes)
  }
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% names(d))
    stop("You passed ", outcome_chr, " to the outcome argument of machine_learn,",
         "but that isn't a column in d.")

  if (missing(models))
    models <- get_supported_models()

  d %>%
    prep_data(!!!dots, outcome = !!outcome, impute = impute) %>%
    tune_models(outcome = !!outcome, models = models)
}
