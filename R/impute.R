#' @title
#' Impute data and return a reusable recipe.
#'
#' @description `impute` will impute your data using a variety of methods for
#' both nominal and numeric data. Currently supports mean (numeric only),
#' new_category (categorical only), bagged trees, or knn. Grain and target will
#' not be imputed.
#' @param data A dataframe or tibble containing data to impute.
#' @param target A string, the name of the target column. Required if training
#' a new recipe.
#' @param grain A string, the name of the grain column. Required if training
#' a new recipe.
#' @param rec_obj Optional, a recipe object. If provided, grain and target are
#' not required and this recipe will be used to bake data contained in data.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param numeric_params A named list with parmeters to use with chosen
#' imputation method on numeric data. Options are \code{bag_model},
#' \code{bag_options}, \code{knn_K}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#' imputation method on nominal data. Options are \code{bag_model},
#' \code{bag_options}, \code{knn_K}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @return Imputed data and a reusable recipe object for future imputation.
#'
#' @export
#' @import recipes

#' @examples
#' library(recipes)
#'
#' n = 100
#' set.seed(9)
#' d <- tibble::tibble(patient_id = 1:n,
#'                     age = sample(c(30:80, NA), size = n, replace = TRUE),
#'                     hemoglobin_count = rnorm(n, mean = 15, sd = 1),
#'                     hemoglobin_category = sample(c("Low", "Normal", "High", NA),
#'                                                  size = n, replace = TRUE),
#'                     disease = ifelse(hemoglobin_count < 15, "Yes", "No")
#' )
#' d_train <- d[1:80, ]
#' d_test <- d[81:100, ]
#'
#' # Train imputer
#' data_and_recipe <- impute(data = d_train,
#'                           grain = "patient_id",
#'                           target = "disease")
#' # Apply to new data
#' res <- impute(data = d_test,
#'               rec_obj = data_and_recipe$rec_obj)
#'
#' # Specify methods:
#' data_and_recipe <- impute(data = d_train,
#'                           grain = "patient_id",
#'                           target = "disease",
#'                           numeric_method = "bagimpute",
#'                           nominal_method = "new_category")
#'
#' # Specify method and param:
#' data_and_recipe <- impute(data = d_train,
#'                           grain = "patient_id",
#'                           target = "disease",
#'                           nominal_method = "knnimpute",
#'                           nominal_params = list(knn_K = 4))
#'
#' impute <- function(data = NULL,
#'                    target = NULL,
#'                    grain = NULL,
#'                    rec_obj = NULL,
#'                    numeric_method = "mean",
#'                    nominal_method = "new_category",
#'                    numeric_params = NULL,
#'                    nominal_params = NULL) {
#'
impute <- function(data = NULL,
                   target = NULL,
                   grain = NULL,
                   rec_obj = NULL,
                   numeric_method = "mean",
                   nominal_method = "new_category",
                   numeric_params = NULL,
                   nominal_params = NULL) {
  # Check to make sure that df is a dataframe
  if (!(is.data.frame(data))) {
    stop("\"data\" must be a tibble or dataframe.")
  }

  # Check to make sure rec_obj is a valid recipe
  if (!inherits(rec_obj, "recipe") && !is.null(rec_obj)) {
    stop("\"rec_obj\" must be a valid recipe object.")
  }

  message("Data contains the following levels of missingness:")
  print(missingness(data))
  message("Grain and target columns will not be imputed.")

  # If recipe object is not provided, train it and predict.
  if (is.null(rec_obj)) {
    if (is.null(target) || !(target %in% names(data))) {
      stop("\"target\" must be a column name in data")
    }
    if (is.null(grain) || !(grain %in% names(data))) {
      stop("\"grain\" must be a column name in data.")
    }

    form = paste0(target, " ~ . - ", grain)

    # Train
    rec_obj <- recipe(x = data, formula = form) %>%
      hcai_impute(numeric_method = numeric_method,
                  nominal_method = nominal_method,
                  numeric_params = numeric_params,
                  nominal_params = nominal_params) %>%
        prep(training = data)
  }
    # Predict
  data_imputed <-
    bake(rec_obj, newdata = data)

  return(list(data_imputed = data_imputed,
            rec_obj = rec_obj))
}


