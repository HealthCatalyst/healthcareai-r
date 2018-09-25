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
  if (!"model_info" %in% names(attributes(x))) {
    x <- structure(x, class = class(x)[!stringr::str_detect(class(x), "^(predicted)|(prepped)")])
    NextMethod(x)
  } else {
    x <- change_metric_names(x)
    mi <- attr(x, "model_info")
    mes <- paste0("\"predicted_", mi$target, "\" predicted by ",
                  mi$algorithm, " last trained: ", mi$timestamp,
                  "\nPerformance in training: ", mi$metric, " = ",
                  round(mi$performance, 2), "\n")
    if (!mi$has_training_data)
      mes <- paste0(mes, "Your model was sanitized of PHI when stored and no ",
                    "new data was provided. If this sanitation was in ",
                    "error, use `save_models(model, sanitize_phi = FALSE)` to ",
                    "keep it. Otherwise, to include your data in this ",
                    "dataframe, please use `cbind` or ",
                    "`predict(model, newdata)`.\n")
    message(mes)
    # Avoid dispatching print.prepped_df:
    y <- structure(x, class = class(x)[!stringr::str_detect(class(x), "^(predicted)|(prepped)")])
    print(y)
    return(invisible(x))
  }
}

################################################
### Utility functions for predict.model_list ###
################################################

#' check for new factor levels and send new data to prep_data before predicting
#' @noRd
ready_with_prep <- function(object, newdata, mi = extract_model_info(object)) {
  recipe <- attr(object, "recipe")
  if (is.null(recipe))
    stop("Can't prep data in prediction without a recipe from training data.")

  # Make newdata column order the same as training data for XGBoost
  ord <- match(names(recipe$template), names(newdata))
  # The ord part gets columns that are in training_data; the which part retains any other columns
  newdata <- newdata[, c(ord[!is.na(ord)],
                         which(!names(newdata) %in% names(recipe$template)))]

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
