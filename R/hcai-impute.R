#' @title
#' Specify imputation methods for an existing recipe
#'
#' @description `hcai-impute` adds various imputation methods to an existing
#' recipe. Currently supports mean (numeric only), new_category (categorical
#' only), bagged trees, or knn.
#' @param recipe A recipe object. imputation will be added to the sequence of
#'  operations for this recipe.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#' \code{"bagimpute"} or \code{"knnimpute"}.
#' @param numeric_params A named list with parmeters to use with chosen
#' imputation method on numeric data. Options are \code{bag_model} (bagimpute
#' only), \code{bag_options} (bagimpute only), \code{knn_K}, (knnimpute only),
#' \code{impute_with}, (bag or knn) or \code{seed_val} (bag or knn).
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with chosen
#' imputation method on nominal data. Options are \code{bag_model} (bagimpute
#' only), \code{bag_options} (bagimpute only), \code{knn_K}, (knnimpute only),
#' \code{impute_with}, (bag or knn) or \code{seed_val} (bag or knn).
#' See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps.
#'
#' @export
#' @import recipes

#' @examples
#' library(recipes)
#'
#' n = 100
#' set.seed(9)
#' d <- tibble::tibble(patient_id = 1:n,
#'             age = sample(c(30:80, NA), size = n, replace = TRUE),
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
#'   hcai_impute()
#' my_recipe
#'
#' # Train recipe
#' trained_recipe <- prep(my_recipe, training = d)
#'
#' # Apply recipe
#' data_modified <- bake(trained_recipe, newdata = d)
#' missingness(data_modified)
#'
#'
#' # Specify methods:
#' my_recipe <- my_recipe %>%
#'   hcai_impute(numeric_method = "bagimpute",
#'     nominal_method = "new_category")
#' my_recipe
#'
#' # Specify methods and params:
#' my_recipe <- my_recipe %>%
#'   hcai_impute(numeric_method = "knnimpute",
#'     numeric_params = list(knn_K = 4))
#' my_recipe
hcai_impute <- function(recipe,
                        nominal_method = "new_category",
                        numeric_method = "mean",
                        numeric_params = NULL,
                        nominal_params = NULL) {
  # Check to make sure recipe is the right type
  if (!inherits(recipe, "recipe")) {
    stop("recipe must be recipe object"
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
  num_p <- nom_p <- defaults
  num_p[names(num_p) %in% names(numeric_params)] <- numeric_params
  nom_p[names(nom_p) %in% names(nominal_params)] <- nominal_params

  # Warn if extra bag names
  available_param_names <- c("bag_model", "bag_options", "knn_K", "impute_with",
                             "seed_val")
  all_user_params <- names(c(numeric_params, nominal_params))
  extras  <- all_user_params[!(all_user_params %in% available_param_names)]
  if (length(extras > 0)) {
    warning("You have extra imputation parameters that won't be used: ",
            list_variables(extras),
            ". Available params are: ", list_variables(available_param_names))
  }

  # Catch datasets where all predictors are of one type
  vi <- recipe$var_info
  all_nominal <- !any(vi$type[vi$role == "predictor"] == "numeric")
  all_numeric <- !any(vi$type[vi$role == "predictor"] == "nominal")

  # Numerics
  if (!all_nominal) {
    if (numeric_method == "mean") {
      recipe <- step_meanimpute(recipe, all_numeric(), - all_outcomes())
    } else if (numeric_method == "bagimpute") {
      recipe <- step_bagimpute(
        recipe,
        all_numeric(), - all_outcomes(),
        models = num_p$bag_model,
        options = num_p$bag_options,
        impute_with = num_p$impute_with,
        seed_val = num_p$seed_val)
    } else if (numeric_method == "knnimpute") {
      recipe <- step_knnimpute(
        recipe,
        all_numeric(), - all_outcomes(),
        K = num_p$knn_K,
        impute_with = num_p$impute_with)
    } else {
      stop("non-supported numeric method")
    }
  }

  # Nominals
  if (!all_numeric) {
    if (nominal_method == "new_category") {
      recipe <- step_missing(recipe, all_nominal(), - all_outcomes())
    } else if (nominal_method == "bagimpute") {
      recipe <- step_bagimpute(
        recipe,
        all_nominal(),
        models = nom_p$bag_model, - all_outcomes(),
        options = nom_p$bag_options,
        impute_with = nom_p$impute_with,
        seed_val = nom_p$seed_val)
    }  else if (nominal_method == "knnimpute") {
      recipe <- step_knnimpute(
        recipe,
        all_nominal(), - all_outcomes(),
        K = nom_p$knn_K,
        impute_with = nom_p$impute_with)
    } else {
      stop("non-supported nominal method")
    }
  }
  return(recipe)
}
