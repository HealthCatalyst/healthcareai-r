#' @title
#' Clean NA values from categorical/nominal variables
#'
#' @description `step_hcai_missing` creates a specification of a recipe that 
#'  will replace NA values `hcai_missing`.
#' @param recipe A recipe object. The step will be added to the sequence of 
#'  operations for this recipe.
#' @param ... One or more selector functions to choose which variables are 
#'  affected by the step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the number of NA values have been
#'  counted in preprocessing.
#' @param na_percentage A named numeric vector of NA percentages. This
#'  is `NULL` until computed by [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  NA counts).
#'
#' @export
#' @import recipes
#' @importFrom rlang quos
#' @details NA values must g `step_scale` estimates
#'  the variable standard deviations from the data used in the
#'  `training` argument of `prep.recipe`.
#'  `bake.recipe` then applies the scaling to new data sets
#'  using these standard deviations.
#' @examples
#' library(healthcareai)
#' library(tibble)
#' library(recipes)
#' n = 100
#' d <- tibble(encounter_id = 1:n,
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
#'   step_hcai_missing(all_nominal())
#' my_recipe
#' 
#' # Train recipe
#' trained_recipe <- prep(my_recipe, training = d)
#' 
#' # Apply recipe
#' data_modified <- bake(trained_recipe, newdata = d)
#' 
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
      impute_with = num_p$impute_with,
      seed_val = num_p$seed_val)
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
      impute_with = nom_p$impute_with,
      seed_val = nom_p$seed_val)
  } else {
    stop("non-supported nominal method")
  }
}
