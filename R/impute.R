#' @title Impute data and return a reusable recipe
#'
#' @description \code{impute} will impute your data using a variety of methods
#' for both nominal and numeric data. Currently supports mean (numeric only),
#' new_category (categorical only), bagged trees, or knn.
#'
#' @param d A dataframe or tibble containing data to impute.
#' @param ... Optional. Unquoted variable names to not be imputed. These will be
#'   returned unaltered.
#' @param recipe Optional, a recipe object or an imputed data frame (containing
#'   a recipe object as an attribute). If provided, this recipe will be applied
#'   to impute new data contained in d with values saved in the recipe. Use this
#'   param if you'd like to apply the same values used for imputation on a
#'   training dataset in production.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#'   \code{"bagimpute"} or \code{"knnimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#'   \code{"bagimpute"} or \code{"knnimpute"}.
#' @param numeric_params A named list with parmeters to use with chosen
#'   imputation method on numeric data. Options are
#'  \code{bag_model} (bagimpute only), \code{bag_trees} (bagimpute
#'  only), \code{bag_options} (bagimpute only), \code{bag_trees}
#'  (bagimpute only), \code{knn_K} (knnimpute only), \code{impute_with}
#'  (knnimpute only), (bag or knn) or \code{seed_val} (bag or knn).
#'  See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#'   imputation method on nominal data. Options are
#'  \code{bag_model} (bagimpute only), \code{bag_trees} (bagimpute
#'  only), \code{bag_options} (bagimpute only), \code{bag_trees}
#'  (bagimpute only), \code{knn_K} (knnimpute only), \code{impute_with}
#'  (knnimpute only), (bag or knn) or \code{seed_val} (bag or knn).
#'  See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param verbose Gives a print out of what will be imputed and which method
#'   will be used.
#' @return Imputed data frame with reusable recipe object for future imputation
#'   in attribute "recipe".
#'
#' @export
#' @import recipes
#' @importFrom purrr map_chr
#'
#' @examples
#' d <- pima_diabetes
#' d_train <- d[1:700, ]
#' d_test <- d[701:768, ]
#' # Train imputer
#' train_imputed <- impute(d = d_train, patient_id, diabetes)
#' # Apply to new data
#' impute(d = d_test, patient_id, diabetes, recipe = train_imputed)
#' # Specify methods:
#' impute(d = d_train, patient_id, diabetes, numeric_method = "bagimpute",
#' nominal_method = "new_category")
#' # Specify method and param:
#' impute(d = d_train, patient_id, diabetes, nominal_method = "knnimpute",
#' nominal_params = list(knn_K = 4))
impute <- function(d = NULL,
                   ...,
                   recipe = NULL,
                   numeric_method = "mean",
                   nominal_method = "new_category",
                   numeric_params = NULL,
                   nominal_params = NULL,
                   verbose = FALSE) {
  # Check to make sure that df is a dframe
  if (!is.data.frame(d)) {
    stop("\"d\" must be a tibble or dataframe.")
  }

  ignore_columns <- rlang::quos(...)
  # Save column order
  col_order <- names(d)

  # If recipe is NULL, stays that way; else looked for in attr and validated
  recipe <- check_rec_obj(recipe)

  # Display missingness and which variables will and won't be imputed
  has_missingness <-
    d %>%
    missingness() %>%
    dplyr::filter(percent_missing > 0)
  ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
  missingness_ignored <- split(has_missingness,
                               has_missingness$variable %in% ignored)
  if (!length(missingness_ignored))
    imp_summary <- "No imputation needed -- all variables fully present!"
  if ("FALSE" %in% names(missingness_ignored)) {
    imp_summary <- missingness_ignored[["FALSE"]] %>%
      dplyr::mutate(imputation_method_used =
                      purrr::map_chr(variable, ~ ifelse(is.numeric(d[[.x]]),
                                                        numeric_method,
                                                        nominal_method))
      )
  }
  if ("TRUE" %in% names(missingness_ignored)) {
    warning("These ignored variables still have missingness: ",
            list_variables(missingness_ignored[["TRUE"]]$variable))
    imp_summary <- dplyr::bind_rows(imp_summary,
                             missingness_ignored[["TRUE"]] %>%
                               dplyr::mutate(imputation_method = "ignored"))

  }

  if (verbose) {
    message("The following missingness will be imputed: ")
    print(imp_summary)
  }

  if (!length(ignore_columns)) {
    d_ignore <- NULL
  } else {
    # Make sure all columns are present
    present <- ignored %in% names(d)
    if (any(!present))
      stop(list_variables(ignored[!present]), " not found in d.")

    d_ignore <- dplyr::select(d, !!!ignore_columns)
    d <- dplyr::select(d, -dplyr::one_of(ignored))
  }

  # If recipe object is not provided, train it and predict.
  if (is.null(recipe)) {
    # Train
    recipe <-
      d %>%
      recipe(formula = "~.") %>%  # nolint
      hcai_impute(numeric_method = numeric_method,
                  nominal_method = nominal_method,
                  numeric_params = numeric_params,
                  nominal_params = nominal_params) %>%
      prep(training = d)
  }
  # Predict
  d_imputed <- bake(recipe, new_data = d)

  # Add ignore columns back in.
  d_imputed <- dplyr::bind_cols(d_imputed, d_ignore)
  d_imputed <- d_imputed[, col_order]

  # Add recipe object as an attribute of the data frame
  attr(d_imputed, "recipe") <- recipe
  attr(d_imputed, "imp_summary") <- imp_summary

  # Add class signature to data frame so we can ID for imputation in deployment
  class(d_imputed) <- c("hcai_imputed_df", class(d_imputed))

  return(d_imputed)
}

# print method for impute
#' @export
print.hcai_imputed_df <- function(x, ...) {
  s <- attr(x, "imp_summary")
  cat("Original missingness and methods used in imputation:\n\n")
  print(s)
  cat("\nCurrent data:\n\n")
  NextMethod(print, x)
  return(invisible(x))
}
