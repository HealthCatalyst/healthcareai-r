#' Constructor function for class model_list
#'
#' @param model_class Every model_list object has a child class that specifies the model_class. Currently classification and regression are supported.
#' @param ... Models to become a model_list, but you should use as.model_list
#'
#' @return An empty list with classes list, model_list, and type_list
#' @export
#' @importFrom purrr map_lgl
#'
#' @examples
#' model_list("regression")
model_list <- function(model_class, ...) {
  check_model_class(model_class = model_class)
  empty_list <- as.model_list(listed_models = list(...), model_class = model_class)
  return(empty_list)
}

#' Make models into model_list object
#'
#' @param ... \code{caret}-trained models to put into a model list
#' @param listed_models Use this if your models are already in a list
#' @param model_class "classification" or "regression"
#' @param target Quoted name of response variable
#'
#' @importFrom purrr map_chr
#' @return A model_list with child class type_list
#' @export
as.model_list <- function(..., listed_models = NULL, target = ".outcome",
                          model_class) {
  listed_models <-
    c(structure(list(...),
                names = sapply(match.call(expand.dots = FALSE)$..., deparse)),
      listed_models)
  if (length(listed_models)) {
    if (any(!purrr::map_lgl(listed_models, inherits, "train")))
      stop("Those don't look like caret-trained models.")
    if (length(unique(lapply(listed_models, function(mm) mm$trainingData))) > 1)  # nolint
      stop("Those models don't appear to have been trained on the same data.")
    types <- unique(tolower(purrr::map_chr(listed_models, ~ .x$modelType)))
    if (length(types) > 1L)
      stop("All model_class elements need to be the same. Yours: ",
           paste(types, collapse = ", "))
    if (!missing(model_class) && model_class != types)
      stop("You provided a model_class argument, but it doesn't match the model(s).")
    model_class <- types
    names(listed_models) <- purrr::map_chr(listed_models, ~ .x$modelInfo$label)
  }
  check_model_class(model_class)
  class(listed_models) <- c(paste0(model_class, "_list"),
                            "model_list",
                            class(listed_models))
  attr(listed_models, "target") <- target
  return(listed_models)
}

#' Check that the model class is supported
#'
#' @param model_class
#'
#' @return Nothing. Errors if model_class is not supported.
#' @noRd
check_model_class <- function(model_class) {
  if (missing(model_class))
    stop("You have to provide a model_class")
  supported_model_classes <- c("classification", "regression")
  if (!model_class %in% supported_model_classes)
    stop("model_class must be one of: ",
         paste(supported_model_classes, collapse = ", "))
  return(invisible(NULL))
}
