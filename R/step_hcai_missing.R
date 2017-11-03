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
#' @details NA values must g `step_scale` estimates
#'  the variable standard deviations from the data used in the
#'  `training` argument of `prep.recipe`.
#'  `bake.recipe` then applies the scaling to new data sets
#'  using these standard deviations.
#' @examples
#' df <- data.frame(date=c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
#'                 a=c(1,2,3,4))
#' dfResult <- orderByDate(df,'date', descending=FALSE)
#' head(dfResult)
step_hcai_missing <- function(recipe, 
                              ..., 
                              role = NA, 
                              trained = FALSE,
                              na_percentage = NULL) {
  terms <- rlang::quos(...) 
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

#' @export
bake.step_hcai_missing <- function(object, newdata, ...) {
  require(tibble)
  vars <- names(object$na_percentage)
  
  for (i in vars) {
    # Add missing level
    levs <- levels(newdata[[i]])
    levels(newdata[[i]]) <- c(levs, "hcai_missing")
    
    # Set NA to the new level
    newdata[is.na(newdata[, i]), i] <- "hcai_missing"
  }
  ## Always convert to tibbles on the way out
  as_tibble(newdata)
}
