#########################################
### S3 generics for predicted_df ###
#########################################

#' Class check
#' @param x object
#' @return logical
#' @export
is.predicted_df <- function(x) "predicted_df" %in% class(x)

# print method for predicted data frame
#' @export
print.predicted_df <- function(x, ...) {
  x <- change_metric_names(x)
  mi <- attr(x, "model_info")
  mes <- paste0("\"predicted_", mi$target, "\" predicted by ",
                mi$algorithm, " last trained: ", mi$timestamp,
                "\nPerformance in training: ", mi$metric, " = ",
                round(mi$performance, 2), "\n")
  message(mes)
  # Avoid dispatching print.prepped_df:
  y <- structure(x, class = class(x)[!stringr::str_detect(class(x), "^(predicted)|(prepped)")])
  print(y)
  return(invisible(x))
}

################################################
### Utility functions for predict.model_list ###
################################################

#' Determine whether to prep_data before making predictions
#' @noRd
determine_prep <- function(object, newdata, mi = extract_model_info(object)) {
  if ("recipe" %in% names(attributes(object)) && !inherits(newdata, "prepped_df")) {
    # There is a recipe and newdata doesn't have prepped class.
    # Check to see if it looks like newdata may have prepped, warn if so, then prep
    if (dfs_compatible(dplyr::select(object[[1]]$trainingData, -.outcome),
                       dplyr::select(newdata, -which(names(newdata) == mi$target))))
      warning("The data used in model training was prepped using `prep_data`. ",
              "Therefore, the data you want to make predictions on must also be prepped. ",
              "It looks like you might have done that by passing `newdata` ",
              "through `prep_data` before passing it to `predict`, but I can't be sure, ",
              "so it will be prepped now before making predictions. ",
              "If you passed the prediction data through `prep_data` before ",
              "`predict`, set `predict(prepdata = FALSE)`.")
    return(TRUE)
  } else {
    # There's either no recipe on model_list or newdata has been prepped, so don't prep
    return(FALSE)
  }
}

#' When not prepping in predict, check that prediction will work
#' @noRd
ready_no_prep <- function(training_data, newdata) {
  # Select variables to be used in prediction:
  training_data <- dplyr::select(training_data, -.outcome)
  to_pred <- newdata[, names(newdata) %in% names(training_data), drop = FALSE]
  # Check for no missingness
  has_missing <- missingness(to_pred, FALSE) > 0
  if (any(has_missing))
    stop("The following variables have missingness that needs to be ",
         "addressed before making predictions. ",
         "Consider using prep_data to address this.\n\t",
         list_variables(names(has_missing)[has_missing]))
  # Check for no new levels in factors
  missing_levels <-
    find_new_levels(to_pred, training_data) %>%
    format_new_levels()
  if (length(missing_levels))
    stop("The following variable(s) had the following value(s) ",
         "in predict that were not observed in training. ",
         "Consider using prep_data to address this.", missing_levels)
  return(to_pred)
}

#' check for new factor levels and send new data to prep_data before predicting
#' @noRd
ready_with_prep <- function(object, newdata, mi = extract_model_info(object)) {
  recipe <- attr(object, "recipe")
  if (is.null(recipe))
    stop("Can't prep data in prediction without a recipe from training data.")

  # Check for new levels in factors not present in training and warn if present
  new_levels <- find_new_levels(newdata, attr(recipe, "factor_levels"))
  # Don't check ignored columns. NAs are checked in prep_data
  new_levels <- new_levels[!names(new_levels) %in% attr(recipe, "ignored_columns")] %>%
    format_new_levels(remove_nas = TRUE)
  if (length(new_levels))
    warning("The following variables(s) had the following value(s) in ",
            "predict that were not observed in training. ",
            new_levels, "\n")
  prep_data(newdata, recipe = recipe) %>%
    return()
}
