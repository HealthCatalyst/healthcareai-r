#' Constructor function for class model_list
#'
#' @param type Every model_list object has a child class that specifies the type
#'   of model. Currently classification and regression are supported.
#' @param ... Models to become a model_list, but you should use as.model_list
#'
#' @return An empty list with classes list, model_list, and type_list
#' @export
#' @importFrom purrr map_lgl
#'
#' @examples
#' model_list("regression")
model_list <- function(type, ...) {
  check_model_type(type = type)
  empty_list <- as.model_list(listed_models = list(...), type = type)
  return(empty_list)
}

#' Make models into model_list object
#'
#' @param ... \code{caret}-trained models to put into a model list
#' @param listed_models Use this if your models are already in a list
#' @param type "classification" or "regression"
#'
#' @return A model_list with child class type_list
#' @export
as.model_list <- function(..., listed_models = NULL, type) {
  listed_models <-
    c(structure(list(...),
                names = sapply(match.call(expand.dots = FALSE)$..., deparse)),
      listed_models)
  if (any(!purrr::map_lgl(listed_models, inherits, "train")))
    warning("Those don't look like caret-trained models.")
  check_model_type(type)
  class(listed_models) <- c(paste0(type, "_list"),
                            "model_list",
                            class(listed_models))
  return(listed_models)
}

#' Check that the model type is supported
#'
#' @param type
#'
#' @return Nothing. Errors if type is not supported.
#' @noRd
check_model_type <- function(type) {
  model_types <- c("classification", "regression")
  if (!type %in% model_types)
    stop("type must be one of: ", paste(model_types, collapse = ", "))
  return(invisible(NULL))
}
