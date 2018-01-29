#' Add levels to nominal variables
#'
#' @param recipe
#' @param ...
#' @param role
#' @param trained
#' @param levels
#'
#' @return
#' @export
#'
#' @examples
step_add_levels <- function(recipe, ..., role = NA, trained = FALSE,
                            cols = NULL, levels = c("other", "hcai_missing")) {
  terms <- rlang::quos(...)
  if(length(terms) == 0)
    stop("Please supply at least one variable specification. See ?selections.")
  add_step(recipe,
           step_add_levels_new(terms = terms, trained = trained, role = role,
                               levels = levels))
}

step_add_levels_new <- function(terms = NULL, role = NA, trained = FALSE,
                                cols = NULL, levels = NULL) {
  step(subclass = "add_levels", terms = terms, role = role, trained = trained,
       cols = cols, levels = levels)
}

#' @export
prep.step_add_levels <- function(x, training, info) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  if (any(info$type[info$variable %in% col_names] != "nominal"))
    stop("step_add_levels is only appropriate for nominal variables")
  step_add_levels_new(terms = x$terms, role = x$role, trained = TRUE,
                      cols = col_names, levels = x$levels)
}

#' @export
bake.step_add_levels <- function(object, newdata, ...) {
 for (column in object$cols) {
   levels(newdata[[column]]) <- union(levels(newdata[[column]]), object$levels)
 }
  as_tibble(newdata)
}
