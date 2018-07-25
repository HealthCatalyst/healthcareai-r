#' Patient Impact Predictor
#'
#' @param model A model_list object
#' @param d A data frame on which \code{model} can make predictions
#' @param new_values A list of alternative values of variables of interest. The
#'   names of the list must be variables in \code{d} and the entries are the
#'   alternative value(s) to try.
#' @param n Integer, default = 3. The number of alternatives to return for each
#'   patient.
#' @param repeated_factors Logical, default = FALSE. Do you want multiple
#'   modifications of the same variable for the same patient? E.g. Reducing BMI
#'   to 15 and to 20.
#' @param smaller_better Logical, default = TRUE. Are lesser values of the
#'   outcome variable in \code{model} preferable?
#' @param expected_signs TODO
#' @param excluded_transitions TODO
#' @param id An identifying column in \code{d} to return in the output data
#'   frame. If this isn't provided, an ID column from \code{model}'s data prep
#'   will be used if available.
#'
#' @return A tibble with \code{number_of the following columns: id, "variable":
#'   the name of the variable being altered, "original_value": the patient's
#'   observed value of "variable", "modified_value" the altered value of
#'   "variable", "original_prediction": the patient's original prediction,
#'   "modified_prediction": the patient's prediction given the that "variable"
#'   changes to "modified_value", and "improvement": the difference between the
#'   original and modified prediction with positive being improvement, based on
#'   the value of \code{smaller_better}.
#' @export
#'
#' @examples
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
#'                    tune = FALSE, models = "xgb")
#' pip(model = m, d = pima_diabetes[1:3, ],
#'     new_values = list(weight_class = c("underweight", "normal", "overweight"),
#'                       plasma_glucose = c(75, 100)))
pip <- function(model, d, new_values, n = 3, repeated_factors = FALSE,
                smaller_better = TRUE, expected_signs, excluded_transitions,
                id) {

  id <- rlang::enquo(id)
  # Try to find an ID column in the model's recipe
  if (rlang::quo_is_missing(id))
    id <- rlang::as_quosure(attr(attr(model, "recipe"), "ignored_columns"))

  # Add row_id column used to join permutations; deleted at end
  d <- dplyr::mutate(d, row_id = dplyr::row_number())

  # Warn if any variables to be tested don't do any work
  if (extract_model_info(model)$best_model_name == "glmnet") {
    int <- interpret(model)
    offered_unused <- names(new_values)[!names(new_values) %in% int$variable]
      if (length(offered_unused))
        warning("The following variables will never produce impact because the ",
                "model used to make predictions is glm and these variables ",
                "don't have any impact on glm predictions: ",
                list_variables(offered_unused))
  }

  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(d, new_values)

  # Make Predictions on the original and permuted data
  suppressWarnings({
    suppressMessages({
      d <- predict(model, newdata = d)
      names(d)[stringr::str_which(names(d), "^predicted_")[1]] <- "base_prediction"
      permuted_df <- predict(model, newdata = permuted_df)
      names(permuted_df)[stringr::str_which(names(permuted_df), "^predicted_")[1]] <- "new_prediction"
    })
  })

  # Scaling constant to change the ordering depending on smaller_better
  ordering_direction <- if (smaller_better) -1 else 1

  # Join the dataframes containing the old and new predictions
  d <-
    d %>%
    select(!!id, row_id, base_prediction) %>%
    # Join on row number
    dplyr::inner_join(
      select(permuted_df, row_id, new_prediction, current_value,
             alt_value, process_variable_name)
      , by = "row_id") %>%
    # Add delta column
    dplyr::mutate(improvement = new_prediction - base_prediction)

  # Check for predictions that are same or worse as the original and
  # replace the recomendation and modified prediction with the originals
  # and the delta with 0
  if (smaller_better) {
    to_fix <- d$improvement >= 0L
    d$improvement <- d$improvement * -1
  } else {
    to_fix <- d$improvement <= 0L
  }

  if (length(to_fix)) {
    # These rows will only be exposed if best delta is 0, so replace with current
    d$alt_value[to_fix] <- d$current_value[to_fix]
    d$new_prediction[to_fix] <- d$base_prediction[to_fix]
    d$improvement[to_fix] <- 0
  }

  d <- d %>%
    select(!!id, row_id,
           variable = process_variable_name,
           original_value = current_value,
           modified_value = alt_value,
           original_prediction = base_prediction,
           modified_prediction = new_prediction,
           improvement) %>%
    group_by(row_id) %>%
    arrange(row_id, desc(improvement)) %>%
    mutate(value_rank = dplyr::row_number())
  # Remove recs w/i patient on same variable
  if (!repeated_factors)
    d <- dplyr::distinct(d, variable, .keep_all = TRUE)
  d %>%
    filter(value_rank <= n) %>%
    ungroup() %>%
    select(-row_id, -value_rank)
}

#' @title Take a dataframe and build a larger dataframe by permuting the
#' values in certain columns.
#' @description Take a dataframe and build a larger dataframe by permuting the
#' values in the columns corresponding to \code{variable_levels}.
#' @param dataframe A dataframe.
#' @param variable_levels A list indexed by the modifiable
#' process variables and containing the factor levels of each such variable.
#' @return A large dataframe where the values in the modifiable variable
#' columns have been permuted.
#' @keywords internal
#' @importFrom dplyr %>%
permute_process_variables <- function(dataframe,
                                      variable_levels) {
  # Get the names of the modifiable variables
  modifiable_names <- names(variable_levels)

  # Build a dataframe for each modifiable variable and glue these together
  lapply(X = seq_along(variable_levels), FUN = function(i) {
    # Get variable name and levels
    variable <- modifiable_names[i]
    if (is.factor(dataframe[[variable]])) {
      levels <- factor(variable_levels[[i]],
                       levels = levels(dataframe[[variable]]))
    } else {
      levels <- variable_levels[[i]]
    }

    # For one variable, cycle through all levels and build a dataframe for
    # each one by perturbing the levels, then combine these.
    one_variable_df <- lapply(X = levels,
                              FUN = build_one_level_df,
                              dataframe = dataframe,
                              variable = variable) %>%
      dplyr::bind_rows()
    # Add the modifiable variable to the dataframe
    one_variable_df["process_variable_name"] <- as.character(variable)
    # Output each one_variable_df so that they may be combined
    one_variable_df
  }) %>%
    dplyr::bind_rows()
}

#' @title
#' Replace all value in the column of a dataframe with a given value.
#' @description Takes a dataframe and replaces all the values in the
#' \code{variable} column with the value \code{level}. Columns
#' tracking which variable and level were used are also added to the dataframe.
#' @param dataframe a dataframe
#' @param variable The name of the column whose values we wish to
#' change
#' @param level The value to use in the \code{variable} column
#' @return A dataframe
#' @keywords internal
build_one_level_df <- function(dataframe, variable, level) {
  # Add reference columns
  dataframe$current_value <- as.character(dataframe[[variable]])
  dataframe$alt_value <- as.character(level)
  # Fill the modifiable variable column with the specified level
  dataframe[[variable]] <- level
  return(dataframe)
}
