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
step_hcai_missing <- function(recipe, 
                              ..., 
                              role = NA, 
                              trained = FALSE,
                              na_percentage = NULL) {
  terms <- quos(...) 
  if (length(terms) == 0)
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(
    recipe, 
    step_hcai_missing_new(
      terms = terms,
      role = role,
      trained = trained,
      na_percentage = na_percentage
    )
  )
}

# Initialze a new object
step_hcai_missing_new <- function(terms = NULL, 
                                  role = NA, 
                                  trained = FALSE,
                                  na_percentage = NULL) {
  step(
    subclass = "hcai_missing", 
    terms = terms,
    role = role,
    trained = trained,
    na_percentage = na_percentage
  )
}

#' @export
prep.step_hcai_missing <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  na_percentage <- sapply(training[, col_names], function(x) {
    100 * round(sum(is.na(x)) / length(x), 3)
  }
  )
  
  step_hcai_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    na_percentage = na_percentage
  )
}

#' @importFrom tidyr replace_na
#' @importFrom stats setNames
#' @export
bake.step_hcai_missing <- function(object, newdata, ...) {
  vars <- names(object$na_percentage)
  
  # Add new level to all factors
  newdata[vars] <- lapply(newdata[vars], function(x){
    levels(x) <- c(levels(x), "hcai_missing")
    x
  })
  
  # Replace NAs
  replacement_list <-
    rep("hcai_missing", length(vars)) %>%
    as.list %>%
    setNames(vars)
  newdata <- newdata %>%
    replace_na(replacement_list)
}

#' @importFrom utils getFromNamespace
#' @export
print.step_hcai_missing <-
  function(x, width = max(20, options()$width - 30), ...) {
    printer = getFromNamespace("printer", "recipes")
    cat("Filling NA with hcai_missing for ", sep = "")
    printer(names(x$na_percentage), x$terms, x$trained, width = width)
    invisible(x)
  }
