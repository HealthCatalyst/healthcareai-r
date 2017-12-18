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
#' \code{bag_options}, \code{knn_K}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#' imputation method on nominal data. Options are \code{bag_model},
#' \code{bag_options}, \code{knn_K}, \code{impute_with}, or \code{seed_val}.
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps.
#'
#' @export
# Initialize

impute <- function(data = NULL,
                        rec_obj = NULL,
                        numeric_method = "mean",
                        nominal_method = "new_category",
                        numeric_params = NULL,
                        nominal_params = NULL) {


  # Check to make sure that df is a dataframe
  if (!(is.data.frame(data))) {
    stop('data must be a tibble or dataframe.')
  }

  # Check to make sure imputeVals is a list or NULL
  if (!(inherits(rec_obj, "recipe") || is.null(rec_obj))) {
    stop("rec_obj must be a valid recipe object.")
  }

  # If recipe object is not provided, train it.
  if (!(is.null(rec_obj))) {
    rec_obj <- recipe(disease ~ ., data = fit_data)
  }

  # Create recipe
  my_recipe <- my_recipe %>%
    hcai_impute()
  my_recipe

  # Train recipe
  trained_recipe <- prep(my_recipe, training = d)

  # Apply recipe
  data_modified <- bake(trained_recipe, newdata = d)
}
