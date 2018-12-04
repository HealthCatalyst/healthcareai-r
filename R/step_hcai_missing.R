#' @title
#' Clean NA values from categorical/nominal variables
#'
#' @description \code{step_missing} creates a specification of a recipe that
#'  will replace NA values with a new factor level, \code{missing}.
#' @param recipe A recipe object. The step will be added to the sequence of
#'  operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'  affected by the step. See \code{?recipes::selections()} for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the number of NA values have been
#'  counted in preprocessing.
#' @param na_percentage A named numeric vector of NA percentages. This
#'  is \code{NULL} until computed by \code{prep.recipe()}.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked?
#' @param id a unique step id that will be used to unprep
#' @return An updated version of \code{recipe} with the new step
#'  added to the sequence of existing steps (if any). For the
#'  \code{tidy} method, a tibble with columns \code{terms} (the
#'  selectors or variables selected) and \code{value} (the
#'  NA counts).
#'
#' @export
#' @import recipes
#' @importFrom rlang quos
#' @details NA values are counted when the recipe is trained using
#' \code{prep.recipe}. \code{bake.recipe} then fills in the missing values for
#' the new data.
#' @examples
#' library(recipes)
#' n = 100
#' d <- tibble::tibble(encounter_id = 1:n,
#'                     patient_id = sample(1:20, size = n, replace = TRUE),
#'                     hemoglobin_count = rnorm(n, mean = 15, sd = 1),
#'                     hemoglobin_category = sample(c("Low", "Normal", "High", NA),
#'                                                  size = n, replace = TRUE),
#'                     disease = ifelse(hemoglobin_count < 15, "Yes", "No")
#' )
#'
#' # Initialize
#' my_recipe <- recipe(disease ~ ., data = d)
#'
#' # Create recipe
#' my_recipe <- my_recipe %>%
#'   step_missing(all_nominal())
#' my_recipe
#'
#' # Train recipe
#' trained_recipe <- prep(my_recipe, training = d)
#'
#' # Apply recipe
#' data_modified <- bake(trained_recipe, new_data = d)
step_missing <- function(recipe,
                              ...,
                              role = NA,
                              trained = FALSE,
                              na_percentage = NULL,
                              skip = FALSE,
                         id = rand_id("bagimpute")) {
  terms <- quos(...)
  if (length(terms) == 0)
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(
    recipe,
    step_missing_new(
      terms = terms,
      role = role,
      trained = trained,
      na_percentage = na_percentage,
      skip = skip,
      id = id
    )
  )
}

# Initialze a new object
step_missing_new <- function(terms = NULL,
                             role = NA,
                             trained = FALSE,
                             na_percentage = NULL,
                             skip = FALSE,
                             id) {
  step(
    subclass = "missing",
    terms = terms,
    role = role,
    trained = trained,
    na_percentage = na_percentage,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_missing <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  na_percentage <- sapply(training[, col_names], function(x) {
    100 * sum(is.na(x)) / length(x)
  }
  )

  # Give warnings about greater than 50% null
  if (any(na_percentage > 50)) {
    warn_deets <-
      paste0(names(na_percentage[na_percentage > 50]), ": ",
            round(na_percentage[na_percentage > 50], 0), "%", collapse = "\n")
    warning("The following categorical columns have greater than 50% missing ",
            "values and will be filled with the category 'missing':\n",
            warn_deets)
  }

  step_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    na_percentage = na_percentage,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tidyr replace_na
#' @importFrom stats setNames
#' @export
bake.step_missing <- function(object, new_data, ...) {

  # If no columns to be imputed, return the input data
  if (is.null(object$na_percentage))
    return(new_data)

  vars <- names(object$na_percentage)
  # Add new level to all factors
  new_data[vars] <- lapply(new_data[vars], function(x){
    levels(x) <- c(levels(x), "missing")
    x
  })

  # Replace NAs
  replacement_list <-
    rep("missing", length(vars)) %>%
    as.list %>%
    setNames(vars)
  new_data %>%
    replace_na(replacement_list)
}

#' @export
print.step_missing <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Filling NA with missing for ", sep = "")
    printer(names(x$na_percentage), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom tibble tibble
#' @rdname step_missing
#' @param x A `step_missing` object.
#' @export
#' @export tidy.step_missing
tidy.step_missing <- function(x, ...) {
  if (x$trained == TRUE) {
    res <- tibble(terms = names(x$na_percentage),
                  value = round(x$na_percentage, 2),
                  id = x$id)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = NA_real_, id = x$id)
  }
  res
}
