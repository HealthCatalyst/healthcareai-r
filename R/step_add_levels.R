#' Add levels to nominal variables
#'
#' @description Adds levels to factor variables. If baking data contains factor
#'   levels unobserved in prep, they will be converted to the first value in the
#'   "levels" arguement ("other" by default). This provides protection against
#'   the common problem of factor levels in prediction that were unobserved in
#'   training.
#'
#' @param recipe recipe object. This step will be added
#' @param ... One or more selector functions
#' @param role Ought to be nominal
#' @param trained Has the recipe been prepped?
#' @param cols columns to be prepped
#' @param levels Factor levels to add to variables. Default = c("other",
#'   "hcai_missing"). If new values are observed in baking vs. prep, they will
#'   be replaced with the first value of this argument (i.e. "other" by
#'   default).
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
                            cols = NULL, levels = c("other", "hcai_missing"),
                            observed_levels = NULL) {
  terms <- rlang::quos(...)
  if (length(terms) == 0)
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(recipe,
           step_add_levels_new(terms = terms, trained = trained, role = role,
                               levels = levels,
                               observed_levels = observed_levels))
}

step_add_levels_new <- function(terms = NULL, role = NA, trained = FALSE,
                                cols = NULL, levels = NULL,
                                observed_levels = NULL) {
  step(subclass = "add_levels", terms = terms, role = role, trained = trained,
       cols = cols, levels = levels, observed_levels = observed_levels)
}

#' @export
prep.step_add_levels <- function(x, training, info) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  if (any(info$type[info$variable %in% col_names] != "nominal"))
    stop("step_add_levels is only appropriate for nominal variables")
  observed_levels <- purrr::map(training[, col_names, drop = FALSE], ~ levels(.x))
  step_add_levels_new(terms = x$terms, role = x$role, trained = TRUE,
                      cols = col_names, levels = x$levels,
                      observed_levels = observed_levels)
}

#' @export
bake.step_add_levels <- function(object, newdata, ...) {
  for (column in object$cols) {
    levels(newdata[[column]]) <- union(levels(newdata[[column]]), object$levels)
    # NA can't be a level, and %in% says NA %in% x is FALSE, so record NAs here
    # and replace them after substituting "other"
    nas <- is.na(newdata[[column]])
    new_values <- !newdata[[column]] %in% object$observed_levels[[column]]
    if (any(new_values & !nas)) {
      newdata[[column]][new_values] <- object$levels[1]
      newdata[[column]][nas] <- NA
    }
  }
  tibble::as_tibble(newdata)
}

#' @export
print.step_add_levels <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Adding levels to: ", sep = "")
  recipes:::printer(x$levels, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_add_levels
#' @param x A `step_add_levels` object.
#' @export
tidy.step_add_levels <- function(x, ...) {
  res <- if (x$trained) {
    tibble::tibble(terms = x$cols, value = paste(x$levels, collapse = ", "))
  } else {
    tibble::tibble(terms = recipes:::sel2char(x$terms), value = NA_character_)
  }
  return(res)
}
