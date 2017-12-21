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
                   target = NULL,
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

  # If recipe object is not provided, train it and predict.
  if (is.null(rec_obj)) {
    if (is.null(target) || !(target %in% names(data))) {
      stop("\"target\" must be a column name in data")
    }

    form = paste0(target, " ~ .")

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


