#' @title Impute data and return a reusable recipe.
#'
#' @description \code{impute} will impute your data using a variety of methods
#' for both nominal and numeric data. Currently supports mean (numeric only),
#' new_category (categorical only), bagged trees, or knn.
#'
#' @param d A dataframe or tibble containing data to impute.
#' @param ... Optional. Unquoted variable names to not be imputed. These will be
#'   returned unaltered.
#' @param rec_obj Optional, a recipe object. If provided, this recipe will be
#'   applied to impute new data contained in d with values saved in the recipe.
#'   Use this param if you'd like to apply the same values used for imputation
#'   on a training dataset in production.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#'   \code{"bagimpute"} or \code{"knnimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#'   \code{"bagimpute"} or \code{"knnimpute"}.
#' @param numeric_params A named list with parmeters to use with chosen
#'   imputation method on numeric data. Options are \code{bag_model} (bagimpute
#'   only), \code{bag_options} (bagimpute only), \code{knn_K}, (knnimpute only),
#'   \code{impute_with}, (bag or knn) or \code{seed_val} (bag or knn). See
#'   \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#'   imputation method on nominal data. Options are \code{bag_model} (bagimpute
#'   only), \code{bag_options} (bagimpute only), \code{knn_K}, (knnimpute only),
#'   \code{impute_with}, (bag or knn) or \code{seed_val} (bag or knn). See
#'   \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param verbose Currently unusued. Logical; if TRUE information about
#'   processing is printed.
#' @return Imputed data frame with reusable recipe object for future imputation
#'   in attribute "rec_obj".
#'
#' @export
#' @import recipes
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#'
#' @examples
#' library(recipes)
#' d <- pima_diabetes
#'
#' d_train <- d[1:700, ]
#' d_test <- d[701:768, ]
#' # Train imputer
#' train_imputed <- impute(d = d_train, patient_id, diabetes)
#' # Apply to new data
#' impute(d = d_test, patient_id, diabetes, rec_obj = attr(d, "rec_obj"))
#' # Specify methods:
#' impute(d = d_train, patient_id, diabetes, numeric_method = "bagimpute",
#' nominal_method = "new_category")
#' # Specify method and param:
#' impute(d = d_train, patient_id, diabetes, nominal_method = "knnimpute",
#' nominal_params = list(knn_K = 4))
data_prep <- function(d = NULL,
                      ...,
                      rec_obj = NULL,
                      convert_0_1_to_factor = TRUE,
                      convert_dates = TRUE,
                      collapse_rare_factors = TRUE,
                      remove_near_zero_variance = TRUE,
                      impute = TRUE,
                      center = FALSE,
                      scale = FALSE,
                      dummies = FALSE,
                      verbose = FALSE) {
  # Check to make sure that d is a dframe
  if (!is.data.frame(d)) {
    stop("\"d\" must be a tibble or dataframe.")
  }
  ignore_columns <- rlang::quos(...)
  ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
  present <- ignored %in% names(d)
  if (any(!present))
    stop(paste(ignored[!present], collapse = ", "), " not found in d.")

  # Separate data into ignored and not
  d_ignore <- dplyr::select(d, !!!ignored)
  d <- dplyr::select(d, -dplyr::one_of(ignored))

  ##### TO DO
  # if (!is.null(rec_obj)) -- pull check and grab from attr from impute.R
  #   Apply recipe and return
  #####

  # Initialize recipe
  rec <- d %>%
    recipe(formula = "~.")

  # Find largely missing columns and convert to factors
  # rec <- rec %>% step_hcai_mostly_missing_to_factor()

  # Convert 0/1 columns to factors (step_bin2factor)
  if (convert_0_1_to_factor) {
    cols <- find_0_1_cols(d)
    rec <- rec %>%
      step_bin2factor(!!cols, levels = c("Y", "N"))
  }

  # Convert date columns to useful features and remove original.
  if (convert_dates) {
    cols <- find_date_cols(d)
    rec <- rec %>%
      step_date(!!cols,
                features = c("dow", "month", "year")) %>%
      step_rm(!!cols)
  }

  # Impute
  if (impute) {
    rec <- rec %>%
      hcai_impute()
  } else if (is.list(impute)) {
    # Set defaults
    ip <- list(numeric_method = "mean",
               nominal_method = "new_category",
               numeric_params = NULL,
               nominal_params = NULL)
    ip[names(ip) %in% names(impute)] <- impute[names(impute) %in% names(ip)]
    extras  <- names(impute)[!(names(impute) %in% names(ip))]
    if (length(extras > 0)) {
      warning("You have extra imputation parameters that won't be used: ",
              paste(extras, collapse = ", "),
              ". Available params are: ", paste(names(ip), collapse = ", "))
    }
    # Impute takes defaults or user specified inputs. Error handling inside.
    rec <- rec %>%
      hcai_impute(numeric_method = ip$numeric_method,
                  nominal_method = ip$nominal_method,
                  numeric_params = ip$numeric_params,
                  nominal_params = ip$nominal_params)
  } else if (impute != FALSE) {
    stop("impute must be boolean or list.")
  }

  # Collapse rare factors into "other"
  if (collapse_rare_factors) {
    rec <- rec %>%
      step_other(all_nominal(), threshold = .02)
  }

  # Log transform
  # Saving until columns can be specified

  # Center
  if (center) {
    rec <- rec %>%
      step_center(all_numeric())
  }

  # Scale
  if (scale) {
    rec <- rec %>%
      step_scale(all_numeric())
  }

  # Remove columns with near zero variance
  if (remove_near_zero_variance) {
    rec <- rec %>%
      step_nzv(everything())
  }

  # Dummies
  if (isTRUE(dummies)) {
    rec <- rec %>%
      step_dummy(all_nominal())
  }

  # Apply recipe
  d_clean <- rec %>%
    prep(training = d) %>%
    bake(newdata = d)

  # Add ignore columns back in.
  d_clean <- dplyr::bind_cols(d_ignore, d_clean)

  attr(d_clean, "rec_obj") <- rec
  return(d_clean)
}













# # Check to make sure rec_obj is a valid recipe
# if (!inherits(rec_obj, "recipe") && !is.null(rec_obj)) {
#   stop("\"rec_obj\" must be a valid recipe object.")
# }
#
# d_ignore <- dplyr::select(d, !!!ignore_columns)
# # Save column order
# col_order <- names(d)
#
# # Display missingness and which variables will and won't be imputed
# has_missingness <-
#   d %>%
#   missingness() %>%
#   dplyr::filter(percent_missing > 0)
# ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
# missingness_ignored <- split(has_missingness,
#                              has_missingness$variable %in% ignored)
#
# if ("FALSE" %in% names(missingness_ignored)) {
#   imp_summary <- missingness_ignored[["FALSE"]] %>%
#     dplyr::mutate(imputation_method_used =
#                     purrr::map_chr(variable, ~ ifelse(is.numeric(d[[.x]]),
#                                                       numeric_method,
#                                                       nominal_method))
#     )
# }
# if ("TRUE" %in% names(missingness_ignored)) {
#   warning("These ignored variables still have missingness: ",
#           paste(missingness_ignored[["TRUE"]]$variable, collapse = ", "))
#   imp_summary <- dplyr::bind_rows(imp_summary,
#                            missingness_ignored[["TRUE"]] %>%
#                              dplyr::mutate(imputation_method = "ignored"))
#
# }
#
# if (verbose) {
#   message("The following missingness will be imputed: ")
#   print(imp_summary)
# }
#
# if (!length(ignore_columns)) {
#   d_ignore <- NULL
# } else {
#   # Make sure all columns are present
#   present <- ignored %in% names(d)
#   if (any(!present))
#     stop(paste(ignored[!present], collapse = ", "), " not found in d.")
#
#   d_ignore <- dplyr::select(d, !!!ignore_columns)
#   d <- dplyr::select(d, -dplyr::one_of(ignored))
# }
#
# # If recipe object is not provided, train it and predict.
# if (is.null(rec_obj)) {
#   # Train
#   rec_obj <-
#     d %>%
#     recipe(formula = "~.") %>%  # nolint
#     hcai_impute(numeric_method = numeric_method,
#                 nominal_method = nominal_method,
#                 numeric_params = numeric_params,
#                 nominal_params = nominal_params) %>%
#     prep(training = d)
# }
# # Predict
# d_imputed <- bake(rec_obj, newdata = d)
#
# # Add ignore columns back in.
# d_imputed <- dplyr::bind_cols(d_imputed, d_ignore)
# d_imputed <- d_imputed[, col_order]
#
# # Add recipe object as an attribute of the data frame
# attr(d_imputed, "rec_obj") <- rec_obj
# attr(d_imputed, "imp_summary") <- imp_summary
#
# # Add class signature to data frame so we can ID for imputation in deployment
# class(d_imputed) <- c("hcai_imputed_df", class(d_imputed))
#
# return(d_imputed)

# print method for impute
#' @export
print.hcai_imputed_df <- function(x, ...) {
  s <- attr(x, "imp_summary")
  r <- attr(x, "rec_obj")

  message("Missingness and imputation method to be used:")
  print(s)
  message("\nRecipe object used for imputation:")
  print(r)
}
