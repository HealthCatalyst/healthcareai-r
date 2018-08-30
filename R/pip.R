#' Patient Impact Predictor
#'
#' @param model A model_list object, as from \code{\link{machine_learn}} or
#'   \code{\link{tune_models}}
#' @param d A data frame on which \code{model} can make predictions
#' @param new_values A list of alternative values for variables of interest. The
#'   names of the list must be variables in \code{d} and the entries are the
#'   alternative values to try.
#' @param n Integer, default = 3. The maximum number of alternatives to return
#'   for each patient. Note that the actual number returned may be less than
#'   \code{n}, for example if \code{length(new_values) < n} or if
#'   \code{allow_same} is FALSE.
#' @param allow_same Logical, default = FALSE. If TRUE, \code{pip} may return
#'   rows with \code{modified_value = original_value} and \code{improvement =
#'   0}. This happens when there are fewer than \code{n} modifications for a
#'   patient that result in improvement. If \code{allow_same} is TRUE and
#'   \code{length(new_values) >= n} you are likely to get \code{n} results for
#'   each patient; however, contraints from \code{variable_direction} or
#'   \code{prohibited_transitions} could make recommendations for some variables
#'   impossible, resulting in fewer than \code{n} recommendations.
#' @param repeated_factors Logical, default = FALSE. Do you want multiple
#'   modifications of the same variable for the same patient?
#' @param smaller_better Logical, default = TRUE. Are lesser values of the
#'   outcome variable in \code{model} preferable?
#' @param variable_direction Named numeric vector or list with entries of -1 or
#'   1. This specifies the direction numeric variables are permitted to move to
#'   produce improvements. Names of the vector are names of variables in
#'   \code{d}; entries are 1 to indicate only increases can yield improvements
#'   or -1 to indicate only decreases can yield improvements. Numeric variables
#'   not appearing in this list may increase or decrease to surface
#'   improvements.
#' @param prohibited_transitions A list of data frames that contain variable
#'   modifications that won't be considered by \code{pip}. Names of the list are
#'   names of variables in \code{d}, and data frames have two columns, "from"
#'   and "to", indicating the original value and modified value, respectively,
#'   of the prohibited transition. If column names are not "from" and "to", the
#'   first column will be assumed to be the "from" column. This is intended for
#'   categorical variables, but could be used for integers as well.
#' @param id Optional. A unquoted variable name in \code{d} representing an
#'   identifier column; it will be included in the returned data frame. If not
#'   provided, an ID column from \code{model}'s data prep will be used if
#'   available.
#'
#' @description Identify opportunities to improve patient outcomes by exploring
#'   changes in predicted outcomes over changes to input variables. \strong{Note
#'   that causality cannot be established by this function.} Omitted variable
#'   bias and other statistical phenomena may mean that the impacts predicted
#'   here are not realizable. Clinical guidance is essential in choosing
#'   \code{new_values} and acting on impact predictions. Extensive options are
#'   provided to control what impact predictions are surfaced, including
#'   \code{variable_direction} and \code{prohibited_transitions}.
#'
#' @return A tibble with any id columns and "variable": the name of the variable
#'   being altered, "original_value": the patient's observed value of
#'   "variable", "modified_value": the altered value of "variable",
#'   "original_prediction": the patient's original prediction,
#'   "modified_prediction": the patient's prediction given the that "variable"
#'   changes to "modified_value", "improvement": the difference between the
#'   original and modified prediction with positive values reflecting
#'   improvement based on the value of \code{smaller_better}, and "impact_rank":
#'   the rank of the modification for that patient.
#' @export
#'
#' @examples
#' # First, we need a model to make recommendations
#' set.seed(52760)
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
#'                    tune = FALSE, models = "xgb")
#' # Let's look at changes in predicted outcomes for three patients changing their
#' # weight class, blood glucose, and blood pressure
#' modifications <- list(weight_class = c("underweight", "normal", "overweight"),
#'                       plasma_glucose = c(75, 100),
#'                       diastolic_bp = 70)
#' pip(model = m, d = pima_diabetes[1:3, ], new_values = modifications)
#'
#' # In the above example, only the first patient has a positive predicted impact
#' # from changing their diastolic_bp, so for the other patients fewer than the
#' # default n=3 predictions are provided. We can get n=3 predictions for each
#' # patient by specifying allow_same, which will recommend the other two patients
#' # maintain their current diastolic_bp.
#' pip(model = m, d = pima_diabetes[1:3, ], new_values = modifications, allow_same = TRUE)
#'
#' # Sometimes clinical knowledge trumps machine learning. In particular, machine
#' # learning models don't establish causality, they only leverage correlation.
#' # Patient impact predictor suggests causality, so clinicians should always be
#' # consulted to ensure that the causal impacts are medically sound.
#' #
#' # If there is clinical knowledge to suggest what impact a variable should have,
#' # that knowledge can be provided to pip. The way it is provided depends on
#' # whether the variable is categorical (prohibited_transitions) or numeric
#' # (variable_direction).
#'
#' ### Constraining categorical variables ###
#' # Suppose a clinician says that recommending a patient change their weight class
#' # to underweight from any value except normal is a bad idea. We can disallow
#' # those suggestions using prohibited_transitions. Note the change in patient
#' # 1's second recommendation goes from underweight to normal.
#' prohibit <- data.frame(from = setdiff(unique(pima_diabetes$weight_class), "normal"),
#'                        to = "underweight")
#' pip(model = m, d = pima_diabetes[1:3, ], new_values = modifications,
#'     prohibited_transitions = list(weight_class = prohibit))
#'
#' ### Constraining numeric variables ###
#' # Suppose a clinician says that increasing diastolic_bp should never be
#' # recommended to improve diabetes outcomes, and likewise for reducing
#' # plasma_glucose (which is clinically silly, but provides an illustration). The
#' # following code ensures that diastolic_bp is only recommended to decrease and
#' # plasma_glucose is only recommended to increase. Note that the plasma_glucose
#' # recommendations disappear, because no patient would see their outcomes
#' # improve by increasing their plasma_glucose.
#' directional_changes <- c(diastolic_bp = -1, plasma_glucose = 1)
#' pip(model = m, d = pima_diabetes[1:3, ], new_values = modifications,
#'     variable_direction = directional_changes)
pip <- function(model, d, new_values, n = 3, allow_same = FALSE,
                repeated_factors = FALSE, smaller_better = TRUE,
                variable_direction = NULL, prohibited_transitions = NULL, id) {

  id <- rlang::enquo(id)
  # Try to find an ID column in the model's recipe
  if (rlang::quo_is_missing(id))
    id <- rlang::as_quosure(attr(attr(model, "recipe"), "ignored_columns"))

  # Add row_id column used to join permutations; deleted at end
  d <- dplyr::mutate(d, row_id = dplyr::row_number())

  mi <- extract_model_info(model)
  if (mi$m_class == "Multiclass")
    stop("pip doesn't support multiclass models. You could try a regular ",
         "classification model in a one-vs-all fashion.")
  # Warn if any variables to be tested don't do any work
  if (mi$best_model_name == "glmnet") {
    int <- interpret(model)
    offered_unused <- names(new_values)[!names(new_values) %in% int$variable]
    if (length(offered_unused))
      warning("The following variables will never produce impact because the ",
              "model used to make predictions is glm and these variables ",
              "don't have any impact on glm predictions: ",
              list_variables(offered_unused))
  }

  # Check format of prohibited_transitions
  if (!is.null(prohibited_transitions)) {
    if (!is.list(prohibited_transitions) ||
        is.data.frame(prohibited_transitions) ||
        is.null(names(prohibited_transitions)) ||
        any(purrr::map_lgl(prohibited_transitions, ~ !is.data.frame(.x))))
      stop("prohibited_transitions must be a named list of data frames")
    not_vars <- names(prohibited_transitions)[!names(prohibited_transitions) %in% names(d)]
    if (length(not_vars))
      stop("The following variables were given as names in prohibited_transitions ",
           "but are not variables in d: ", list_variables(not_vars))
    # Set names and order of columns
    prohibited_transitions <-
      purrr::map(prohibited_transitions, function(pt) {
        if (!dplyr::setequal(names(pt), c("from", "to")))
          names(pt) <- c("from", "to")
        dplyr::select(pt, from, to)
      })
  }

  # Ensure variable_direction is valid
  if (!is.null(variable_direction))
    variable_direction <- check_variable_direction(d, variable_direction)

  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(d, new_values, variable_direction)

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
    dplyr::mutate(improvement = new_prediction - base_prediction) %>%
    # Remove rows that where current and alternative values are the same
    dplyr::filter(current_value != alt_value)

  # Check for predictions that are same or worse as the original and
  # replace the recomendation and modified prediction with the originals
  # and the delta with 0
  if (smaller_better) {
    to_fix <- d$improvement >= 0L
    d$improvement <- d$improvement * -1
  } else {
    to_fix <- d$improvement <= 0L
  }

  d <- if (allow_same) {
    # Replace modifications that hurt outcome with x to x and improvement of 0
    d %>%
      dplyr::mutate(
        alt_value = dplyr::case_when(to_fix ~ current_value, !to_fix ~ alt_value),
        new_prediction = dplyr::case_when(to_fix ~ base_prediction, !to_fix ~ new_prediction),
        improvement = dplyr::case_when(to_fix ~ 0, !to_fix ~ improvement)
      )
  } else {
    # Remove modifications that hurt outcomes
    dplyr::filter(d, !to_fix)
  }

  # Remove prohibited transitions
  if (!is.null(prohibited_transitions) && nrow(d)) {
    split_vars <- split(d, d$process_variable_name)
    d <- purrr::map_df(names(split_vars), function(v)
      remove_prohibited(split_vars[[v]], prohibited_transitions[[v]]))
  }

  d <-
    d %>%
    select(!!id, row_id,
           variable = process_variable_name,
           original_value = current_value,
           modified_value = alt_value,
           original_prediction = base_prediction,
           modified_prediction = new_prediction,
           improvement) %>%
    group_by(row_id)
  # Remove recs w/i patient on same variable
  if (!repeated_factors)
    d <- dplyr::distinct(d, variable, .keep_all = TRUE)
  d <-
    d %>%
    arrange(row_id, desc(improvement)) %>%
    mutate(impact_rank = dplyr::row_number()) %>%
    filter(impact_rank <= n) %>%
    ungroup() %>%
    select(-row_id)

  class(d) <- c("pip_df", class(d))
  return(d)
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
permute_process_variables <- function(dataframe, variable_levels, variable_direction) {

  # Get the names of the modifiable variables
  modifiable_names <- names(variable_levels)

  # Build a dataframe for each modifiable variable and glue these together
  purrr::map_df(seq_along(variable_levels), function(i) {
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
    purrr::map_df(levels, build_one_level_df,
                  dataframe = dataframe,
                  variable = variable,
                  one_variable_direction = variable_direction[variable])
  })
}

#' @title
#' Replace all value in the column of a dataframe with a given value.
#' @description Takes a dataframe and replaces all the values in the
#' \code{variable} column with the value \code{level}. Columns
#' tracking which variable and level were used are also added to the dataframe.
#' @param dataframe a dataframe
#' @param variable The name of the column whose values we wish to
#' change
#' @param level The modified value to use in the \code{variable} column
#' @return A dataframe
#' @keywords internal
build_one_level_df <- function(dataframe, variable, level, one_variable_direction) {
  # Add reference columns
  dataframe$current_value <- dataframe[[variable]]
  dataframe$alt_value <- as.character(level)
  dataframe$process_variable_name <- variable
  # Fill the modifiable variable column with the specified level
  dataframe[[variable]] <- level
  # If variable was provided in variable_direction, filter any transitions in
  # the wrong direction
  if (!is.null(one_variable_direction) && !is.na(one_variable_direction)) {
    keepers <- one_variable_direction * level >= one_variable_direction * dataframe$current_value
    dataframe <- dplyr::slice(dataframe, which(keepers))
  }
  dataframe$current_value <- as.character(dataframe$current_value)
  return(dataframe)
}

# Remove prohibited. Called mainly from build_one_level_df,
# but also from pip to remove x to x transitions if !allow_same
remove_prohibited <- function(d, prohibited) {
  if (is.null(prohibited)) return(d)
  if (any(stats::na.omit(suppressWarnings(as.numeric(d$current_value))) %% 1 != 0))
    warning("You're prohibiting transitions in ", unique(d$process_variable_name),
            " but at least some values in ", unique(d$process_variable_name),
            " are non-integer numbers, which makes them hard to specify precisely ",
            "for prohibited_transitions. Are you sure you don't want to use ",
            "variable_direction instead?")
  d %>%
    dplyr::mutate(current_value = as.character(current_value),
                  alt_value = as.character(alt_value)) %>%
    dplyr::anti_join(dplyr::mutate_all(prohibited, as.character),
                     by = c(current_value = "from", alt_value = "to"))
}

check_variable_direction <- function(d, variable_direction) {
  variable_direction <- unlist(variable_direction)
  # Check format of variable_direction
  not_ones <- names(variable_direction)[!variable_direction %in% c(-1, 1)]
  if (length(not_ones))
    stop("Entries in variable_direction can only be -1 or 1. Problem variables: ",
         list_variables(not_ones))
  not_present <- names(variable_direction)[!names(variable_direction) %in% names(d)]
  if (length(not_present))
    stop("The following variables were provided as names of `variable_direction` ",
         "but are not present in d: ", list_variables(not_present))
  non_num <-
    d[, names(d) %in% names(variable_direction), drop = FALSE] %>%
    purrr::map_lgl(~ !is.numeric(.x))
  if (any(non_num))
    stop("Only numeric variables can be provided to `variable_direction` ",
         "to control what's allowed for non-numeric variables, use ",
         "`prohibited_transitions`. Problem variables: ",
         list_variables(names(non_num)[non_num]))
  return(variable_direction)
}
