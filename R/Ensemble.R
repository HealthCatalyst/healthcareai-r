#' Ensembling
#'
#' @param d is a dataset used to train the models
#' @param ... are columns need to be ignored; comma separated
#' @param outcome Response or Predicted or dependent variable
#' @param newdata Dataset used to predict the outcome,
#' #'if not provided prepared data from dataset d will be used as new data
#' @param Ensembling logical. True(Default)
#'
#' @return Quality_Matrix or Ensembled_Quality_Matrix based on the ensembling input if TRUE or False provided by user.
#' @export
#'
#' @examples#' # Split the data into training and test sets
#' d <- pima_diabetes
#'
#' ## Classification ##
#'
#' # Clean and prepare the dataset, specifying that patient_id is an ID column,
#' diabetes_models <- Ensemble(d, patient_id, outcome = diabetes)
#' it will return confusion matrix.
Ensemble <- function(d, ..., outcome, newdata, Ensembling = TRUE) {
  if (!is.data.frame(d))
    stop("\"d\" must be a data frame.")

  dots <- rlang::quos(...)
  ignored <- map_chr(dots, rlang::quo_name)
  if (length(ignored)) {
    not_there <- setdiff(ignored, names(d))
    if (length(not_there))
      stop(
        "The following variable(s) were passed to the ... argument of Ensemble",
        " but are not the names of columns in the data frame: ",
        list_variables(not_there)
      )
  }

  outcome <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(outcome)) {
    mes <- "You must provide an outcome variable to Ensemble"
    if (length(ignored))
      mes <- paste(mes, "Did you forget to specify `outcome = `?")
    stop(mes)
  }
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% names(d))
    stop(
      "You passed ",
      outcome_chr,
      " to the outcome argument of Ensemble,",
      "but that isn't a column in d."
    )

  if (!is.logical(Ensembling)) {
    Ensembling <- TRUE
    stop("Ensembling must be logical.")
  }

  pd <- prep_data(d, outcome = !!outcome, impute = impute)

  if (missing(newdata)) {
    newdata <- pd
  }
  if (!inherits(newdata, "data.frame"))
    stop("newdata must be a data frame")

  if (Ensembling) {
    pd_Not_Ignored <-
      prep_data(d, !!!dots, outcome = !!outcome, impute = impute)

    pd <- prep_data(d, outcome = !!outcome, impute = impute)

    pd_ignored <- dplyr::select(pd, !!!dots, !!outcome)

    m1 <-
      multinom(outcome = !!outcome ~ .,
               data = pd_Not_Ignored)

    m2 <-
      multinom(outcome = !!outcome ~ .,
               data = pd_ignored)

    predicted_1 <- predict(object = m1, newdata, type = "prob")

    predicted_2 <- predict(object = m1, newdata, type = "prob")

    predicted$avg <- (predicted_1 + predicted_2) / 2

    predicted$avg_classes <-
      as.factor(ifelse(testSet$pred_avg > 0.5, "Y", "N"))

    confusionMatrix(prepared_data$outcome, predicted_2)

    Ensembled_Quality_Matrix <-
      confusionMatrix(prepared_data$outcome, predicted$avg_classes)

    return(Ensembled_Quality_Matrix)

  }

  else{
    pd_Not_Ignored <-
      prep_data(d, !!!dots, outcome = !!outcome, impute = impute)

    m1 <-
      multinom(outcome = !!outcome ~ .,
               data = pd_Not_Ignored)

    predicted_1 <- predict(m1, newdata)

    Quality_Matrix <-
      confusionMatrix(prepared_data$outcome, predicted_1)

    return(Quality_Matrix)

  }
}
