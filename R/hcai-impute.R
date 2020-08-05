#' @title
#' Specify imputation methods for an existing recipe
#'
#' @description `hcai-impute` adds various imputation methods to an existing
#' recipe. Currently supports mean (numeric only), new_category (categorical
#' only), bagged trees, or knn.
#' @param recipe A recipe object. imputation will be added to the sequence of
#'  operations for this recipe.
#' @param numeric_method Defaults to \code{"mean"}. Other choices are
#' \code{"bagimpute"}, \code{"knnimpute"} or \code{"locfimpute"}.
#' @param nominal_method Defaults to \code{"new_category"}. Other choices are
#' \code{"bagimpute"}, \code{"knnimpute"} or \code{"locfimpute"}.
#' @param numeric_params A named list with parmeters to use with
#'  chosen imputation method on numeric data. Options are
#'  \code{bag_model} (bagimpute only), \code{bag_trees} (bagimpute
#'  only), \code{bag_options} (bagimpute only), \code{bag_trees}
#'  (bagimpute only), \code{knn_K} (knnimpute only), \code{impute_with}
#'  (knnimpute only), (bag or knn) or \code{seed_val} (bag or knn).
#'  See \link{step_bagimpute} or \link{step_knnimpute} for details.
#' @param nominal_params A named list with parmeters to use with
#'  chosen imputation method on nominal data. Options are
#'  \code{bag_model} (bagimpute only), \code{bag_trees} (bagimpute
#'  only), \code{bag_options} (bagimpute only), \code{bag_trees}
#'  (bagimpute only), \code{knn_K} (knnimpute only), \code{impute_with}
#'  (knnimpute only), (bag or knn) or \code{seed_val} (bag or knn).
#'  See \link{step_bagimpute} or \link{step_knnimpute} for details.
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
#' data_modified <- bake(trained_recipe, new_data = d)
#' missingness(data_modified)
#'
#'
#' # Specify methods:
#' my_recipe <- my_recipe %>%
#'   hcai_impute(numeric_method = "bagimpute",
#'     nominal_method = "locfimpute")
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
  possible_numeric_methods <- c("mean", "bagimpute", "knnimpute", "locfimpute")
  if (!(numeric_method %in% possible_numeric_methods)) {
    stop("non-supported numeric method. Use \"mean\", \"bagimpute\",
         \"locfimpute\", or \"knnimpute\"")
  }
  possible_nominal_methods <- c("new_category", "bagimpute", "knnimpute",
                                "locfimpute")
  if (!(nominal_method %in% possible_nominal_methods)) {
    stop("non-supported nominal method. Use \"new_category\", \"bagimpute\",
         \"locfimpute\", or \"knnimpute\"")
  }

  # Assign defaults for params
  defaults <- list(
    bag_model = NULL,
    bag_options = list(keepX = FALSE),
    knn_K = 5,
    bag_trees = 25,
    impute_with = imp_vars(all_predictors()),
    seed_val = sample.int(1000, 1)
  )

  # Fill in user-specified params
  num_p <- nom_p <- defaults
  # Silence confusing warning when params don't match
  suppressWarnings(
    num_p[names(num_p) %in% names(numeric_params)] <- numeric_params
  )
  suppressWarnings(
    nom_p[names(nom_p) %in% names(nominal_params)] <- nominal_params
  )

  # Warn if params don't match chosen imputation method
  check_params(possible_numeric_methods, numeric_method, numeric_params)
  check_params(possible_nominal_methods, nominal_method, nominal_params)

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
        trees = num_p$bag_trees,
        options = num_p$bag_options,
        impute_with = num_p$impute_with,
        seed_val = num_p$seed_val)
    } else if (numeric_method == "knnimpute") {
      if ("character" %in% map_chr(recipe$template, ~{
        class(.x) %>% first()
      }))
        message("`knnimpute` depends on another library that does not support ",
                "character columns yet. If `knnimpute` fails please convert ",
                "all character columns to factors for knn imputation.")
      recipe <- step_knnimpute(
        recipe,
        all_numeric(), - all_outcomes(),
        neighbors = num_p$knn_K,
        impute_with = num_p$impute_with)
    } else if (numeric_method == "locfimpute") {
      recipe <- step_locfimpute(
        recipe,
        all_numeric(), - all_outcomes())
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
        trees = nom_p$bag_trees,
        options = nom_p$bag_options,
        impute_with = nom_p$impute_with,
        seed_val = nom_p$seed_val)
    } else if (nominal_method == "knnimpute") {
      recipe <- step_knnimpute(
        recipe,
        all_nominal(), - all_outcomes(),
        neighbors = nom_p$knn_K,
        impute_with = nom_p$impute_with)
    } else if (nominal_method == "locfimpute") {
      recipe <- step_locfimpute(
        recipe,
        all_nominal(), - all_outcomes())
    } else {
      stop("non-supported nominal method")
    }
  }
  return(recipe)
}


#' Throws a warning if the parameters given don't match the supported parameters
#' @noRd
check_params <- function(possible_methods, cur_method, cur_params) {
  available_params <- list(
    knnimpute = c("knn_K", "impute_with", "seed_val"),
    bagimpute = c("bag_model", "bag_trees", "bag_options", "impute_with",
                  "seed_val"),
    locfimpute = NULL,
    mean = NULL,
    new_category = NULL
  )

  purrr::map(possible_methods, ~{
    if (cur_method == .x) {
      matched_params <- names(cur_params) %in% available_params[[.x]]
      new_params <- names(cur_params)[!matched_params]
      if (length(new_params)) {
        available_params_mes <-
          if (is.null(available_params[[.x]]))
            paste0("There are not available parameters for ", .x, ".")
          else
            paste0("Available ", .x, " params are: ",
                   list_variables(available_params[[.x]]), ".")
        warning("The following extra parameters won't be used for ", .x, ": ",
                list_variables(new_params), ". ", available_params_mes)
      }
    }
  })
}
