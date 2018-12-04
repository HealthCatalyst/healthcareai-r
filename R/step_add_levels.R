#' Add levels to nominal variables
#'
#' @param recipe recipe object. This step will be added
#' @param ... One or more selector functions
#' @param role Ought to be nominal
#' @param trained Has the recipe been prepped?
#' @param cols columns to be prepped
#' @param levels Factor levels to add to variables. Default = c("other",
#'   "missing")
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked?
#' @param id a unique step id that will be used to unprep
#'
#' @return Recipe with the new step
#' @export
#'
#' @examples
#' library(recipes)
#' d <- data.frame(num = 1:30,
#'                 has_missing = c(rep(NA, 10), rep('b', 20)),
#'                 has_rare = c("rare", rep("common", 29)),
#'                 has_both = c("rare", NA, rep("common", 28)),
#'                 has_neither = c(rep("cat1", 15), rep("cat2", 15)))
#' rec <- recipe( ~ ., d) %>%
#'   step_add_levels(all_nominal()) %>%
#'   prep(training = d)
#' baked <- bake(rec, d)
#' lapply(d[, sapply(d, is.factor)], levels)
#' lapply(baked[, sapply(baked, is.factor)], levels)
step_add_levels <- function(recipe, ..., role = NA, trained = FALSE,
                            cols = NULL, levels = c("other", "missing"),
                            skip = FALSE,
                            id = rand_id("bagimpute")) {
  terms <- rlang::quos(...)
  if (length(terms) == 0)
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(recipe,
           step_add_levels_new(terms = terms, trained = trained, role = role,
                               levels = levels, skip = skip, id = id))
}

step_add_levels_new <- function(terms = NULL, role = NA, trained = FALSE,
                                cols = NULL, levels = NULL, skip = FALSE,
                                id) {
  step(subclass = "add_levels", terms = terms, role = role, trained = trained,
       cols = cols, levels = levels, skip = skip, id = id)
}

#' @export
prep.step_add_levels <- function(x, training, info) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  if (any(info$type[info$variable %in% col_names] != "nominal"))
    stop("step_add_levels is only appropriate for nominal variables")
  step_add_levels_new(terms = x$terms, role = x$role, trained = TRUE,
                      cols = col_names, levels = x$levels, skip = x$skip,
                      id = x$id)
}

#' @export
bake.step_add_levels <- function(object, new_data, ...) {
  for (column in object$cols) {
    levels(new_data[[column]]) <- union(levels(new_data[[column]]), object$levels)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_add_levels <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Adding levels to: ", sep = "")
  printer(x$levels, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_add_levels
#' @param x A `step_add_levels` object.
#' @export
#' @export tidy.step_add_levels
tidy.step_add_levels <- function(x, ...) {
  res <- if (x$trained) {
    tibble::tibble(terms = x$cols, value = paste(x$levels, collapse = ", "),
                   id = x$id)
  } else {
    tibble::tibble(terms = sel2char(x$terms), value = NA_character_,
                   id = x$id)
  }
  return(res)
}
