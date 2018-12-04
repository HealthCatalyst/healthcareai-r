#' Last Observation Carried Forward Imputation
#'
#' @description \code{step_locfimpute} creates a *specification* of a recipe
#'   step that will substitute missing values with the most recent variable
#'   value. If the first variable value is missing, it is imputed with the first
#'   present value.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables will be
#'   imputed. See [selections()] for more details. For the `tidy` method, these
#'   are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the number of NA values have been
#'   counted in preprocessing.
#' @param id a unique step id that will be used to unprep
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @return For \code{step_locfimpute}, an updated version of recipe with the new
#'   step added to the sequence of existing steps (if any). For the \code{tidy}
#'   method, a tibble with columns \code{terms} (the selectors or variables
#'   selected) and \code{trained} (a logical that states whether the recipe has
#'   been prepped).
#' @export
#' @examples
#' library(recipes)
#'
#' prepped <-
#'   recipe(formula = "~.", pima_diabetes) %>%
#'   step_locfimpute(weight_class, insulin, skinfold, diastolic_bp) %>%
#'   prep()
#'
#' bake(prepped, new_data = pima_diabetes)
step_locfimpute <- function(recipe, ..., role = NA, trained = FALSE,
                           skip = FALSE,
                           id = rand_id("bagimpute")) {
  add_step(
    recipe,
    step_locfimpute_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_locfimpute_new <- function(terms = NULL, role = NA,
                                trained = FALSE, cols = NULL, skip = FALSE,
                                id) {
  step(subclass = "locfimpute", terms = terms, role = role, trained = trained,
       cols = cols, skip = skip, id = id)
}

#' @importFrom stats as.formula model.frame
#' @export
prep.step_locfimpute <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_locfimpute_new(terms = x$terms, role = x$role, trained = TRUE,
                      cols = col_names, skip = x$skip, id = x$id)
}

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_locfimpute <- function(object, new_data, ...) {
  new_data <- tidyr::fill(new_data, object$cols) %>%
    tidyr::fill(object$cols, .direction = "up")
  return(new_data)
}

#' @export
print.step_locfimpute <- function(x, width = max(20, options()$width - 29),
                                 ...) {
  cat("LOCF Imputation for ")
  printer(x$cols, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_locfimpute
#' @param x A `step_locfimpute` object.
#' @export
#' @export tidy.step_locfimpute
tidy.step_locfimpute <- function(x, ...) {
  if (x$trained == TRUE) {
    res <- expand.grid(
      terms = x$cols,
      trained = TRUE,
      id = x$id
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- expand.grid(
      terms = term_names,
      trained = FALSE,
      id = x$id
    )
  }
  as_tibble(res)
}
