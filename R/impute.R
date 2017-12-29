#' @title
#' Impute data and return a reusable recipe.
#'
#' @description `impute` will impute your data using a variety of methods for
#' both nominal and numeric data. Currently supports mean (numeric only),
#' new_category (categorical only), bagged trees, or knn.
#' @param d A dataframe or tibble containing data to impute.
#' @param ... Optional. Unquoted variable names to not be imputed. These will
#' be returned unaltered.
#' @param rec_obj Optional, a recipe object. If provided, this recipe will be
#' applied to impute new data contained in d with values saved in the recipe.
#' Use this param if you'd like to save imputation values from a large data set
#' and then apply them to a small dataset (rather than recalculating from a much
#' smaller number of rows).
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
#'                     age = sample(c(30:80), size = n, replace = TRUE),
#'                     hemoglobin_count = rnorm(n, mean = 15, sd = 1),
#'                     hemoglobin_category = sample(c("Low", "Normal", "High", NA),
#'                                                  size = n, replace = TRUE),
#'                     disease = ifelse(hemoglobin_count < 15, "Yes", "No")
#' )
#' d$age[sample(1:n, size = 20)] <- NA
#' d$hemoglobin_count[sample(1:n, size = 15)] <- NA
#'
#'
#' d_train <- d[1:80, ]
#' d_test <- d[81:100, ]
#' # Train imputer and apply
#' data_and_recipe <- impute(d = d_train,
#'                           grain = "patient_id",
#'                           target = "disease")
#'
#' # Apply to new data
#' res <- impute(d = d_test,
#'               grain = "patient_id",
#'               target = "disease",
#'               rec_obj = data_and_recipe$rec_obj)
#'
#' # Specify methods:
#' d_and_recipe <- impute(d = d_train,
#'                           grain = "patient_id",
#'                           target = "disease",
#'                           numeric_method = "bagimpute",
#'                           nominal_method = "new_category")
#'
#' # Specify method and param:
#' d_and_recipe <- impute(d = d_train,
#'                           grain = "patient_id",
#'                           target = "disease",
#'                           nominal_method = "knnimpute",
#'                           nominal_params = list(knn_K = 4))
#'
#'
#'
#'
impute <- function(d = NULL,
                   ...,
                   rec_obj = NULL,
                   numeric_method = "mean",
                   nominal_method = "new_category",
                   numeric_params = NULL,
                   nominal_params = NULL) {
  # Check to make sure that df is a dframe
  if (!(is.data.frame(d))) {
    stop("\"d\" must be a tibble or dataframe.")
  }

  # Check to make sure rec_obj is a valid recipe
  if (!inherits(rec_obj, "recipe") && !is.null(rec_obj)) {
    stop("\"rec_obj\" must be a valid recipe object.")
  }

  ignore_columns <- rlang::quos(...)

  # Save column order
  col_order <- names(d)

  # Display missingness
  message("d contains the following levels of missingness:")
  print(missingness(d))
  message(paste0("Ignore columns will not be imputed."))

  if (length(ignore_columns) == 0) {
    d_ignore <- NULL
  } else {
    # Make sure all columns are present
    cols <- sapply(ignore_columns, rlang::quo_name)
    present <- cols %in% names(d)
    if (any(!present))
      stop(paste(cols[!present], collapse = ", "), " not found in d.")

    d_ignore <- dplyr::select(d, !!!ignore_columns)
    d <- dplyr::select(d, -one_of(cols)) # -!!!ignore_columns failed here.
  }


  # If recipe object is not provided, train it and predict.
  if (is.null(rec_obj)) {
    # Train
    rec_obj <- recipe(x = d, formula = "~.") %>%
      hcai_impute(numeric_method = numeric_method,
                  nominal_method = nominal_method,
                  numeric_params = numeric_params,
                  nominal_params = nominal_params) %>%
        prep(training = d)
  }
    # Predict
  d_imputed <-
    bake(rec_obj, newdata = d)

  # Add ignore columns back in.
  d_imputed <- bind_cols(d_imputed, d_ignore)
  d_imputed <- d_imputed[, col_order]

  return(list(d_imputed = d_imputed,
            rec_obj = rec_obj))
}


