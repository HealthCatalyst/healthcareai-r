#' @title
#' Specify imputation methods for an existing recipe
#'
#' @description `hcai-impute` adds various imputation methods to an existing
#' recipe. Currently supports mean (numeric only), new_category (categorical
#' only), bagged trees, or knn.
#' @param rec_obj A recipe object. imputation will be added to the sequence of
#'  operations for this recipe.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param numeric_params A named list with parmeters to use with chosen
#' imputation method on numeric data. Options are \code{bag_model},
#' \code{bag_options}, \code{knn_k}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#' imputation method on nominal data. Options are \code{bag_model},
#' \code{bag_options}, \code{knn_k}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps.
#'
#' @export
#' @import recipes

#' @examples
#' library(healthcareai)
#' library(recipes)
#'
#' n = 100
#' d <- tibble::tibble(encounter_id = 1:n,
#'             patient_id = sample(1:20, size = n, replace = TRUE),
#'             hemoglobin_count = rnorm(n, mean = 15, sd = 1),
#'             hemoglobin_category = sample(c("Low", "Normal", "High", NA),
#'                                          size = n, replace = TRUE),
#'             disease = ifelse(hemoglobin_count < 15, "Yes", "No")
#' )
#'
#' # Initialize
#' my_recipe <- recipe(disease ~ ., data = d)
#'
#' # Create recipe
#' my_recipe <- my_recipe %>%
#'   hcai_impute(numeric_method = "mean",
#'     nominal_method = "new_category")
#' my_recipe
#'
#' # Train recipe
#' trained_recipe <- prep(my_recipe, training = d)
#'
#' # Apply recipe
#' data_modified <- bake(trained_recipe, newdata = d)
#' missingness(data_modified)
hcai_impute <- function(rec_obj,
                        numeric_method = "mean",
                        nominal_method = "new_category",
                        numeric_params = NULL,
                        nominal_params = NULL) {
  # Check to make sure rec_obj is the right type
  if (class(rec_obj) != "recipe") {
    stop("rec_obj must be recipe object"
    )
  }

  # If methods dont exist, use defaults
  if (!(numeric_method %in% c("mean", "bagimpute", "knnimpute"))) {
    stop("non-supported numeric method. Use \"mean\", \"bagimpute\",
         or \"knnimpute\"")
  }
  if (!(nominal_method %in% c("new_category", "bagimpute", "knnimpute"))) {
    stop("non-supported nominal method. Use \"new_category\", \"bagimpute\",
         or \"knnimpute\"")
  }

  # Assign defaults for params
  defaults <- list(
    bag_model = NULL,
    bag_options = list(nbagg = 25, keepX = FALSE),
    knn_K = 5,
    impute_with = imp_vars(all_predictors()),
    seed_val = sample.int(1000, 1)
  )

  # Fill in user-specified params
  num_p <- defaults
  num_p[names(num_p) %in% names(numeric_params)] <- numeric_params
  nom_p <- defaults
  nom_p[names(nom_p) %in% names(nominal_params)] <- nominal_params

  # Numerics
  if (numeric_method == "mean") {
    rec_obj <- step_meanimpute(rec_obj, all_numeric())
  } else if (numeric_method == "bagimpute") {
    rec_obj <- step_bagimpute(
      rec_obj,
      all_numeric(),
      models = num_p$bag_model,
      options = num_p$bag_options,
      impute_with = num_p$impute_with,
      seed_val = num_p$seed_val)
  } else if (numeric_method == "knnimpute") {
    rec_obj <- step_knnimpute(
      rec_obj,
      all_numeric(),
      K = num_p$knn_K,
      impute_with = num_p$impute_with)
  } else {
    stop("non-supported numeric method")
  }

  # Nominals
  if (nominal_method == "new_category") {
    rec_obj <- step_hcai_missing(rec_obj, all_nominal())
  } else if (nominal_method == "bagimpute") {
    rec_obj <- step_bagimpute(
      rec_obj,
      all_nominal(),
      models = nom_p$bag_model,
      options = nom_p$bag_options,
      impute_with = nom_p$impute_with,
      seed_val = nom_p$seed_val)
  }  else if (nominal_method == "knnimpute") {
    rec_obj <- step_knnimpute(
      rec_obj,
      all_nominal(),
      K = nom_p$knn_K,
      impute_with = nom_p$impute_with)
  } else {
    stop("non-supported nominal method")
  }
}
