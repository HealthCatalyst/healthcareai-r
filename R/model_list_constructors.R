#' Make models into model_list object
#'
#' @param ... \code{caret}-trained models to put into a model list
#' @param listed_models Use this if your models are already in a list
#' @param model_class "classification" or "regression"
#' @param target Quoted name of response variable
#' @param tuned Logical; if FALSE, will have super-class untuned_models
#'
#' @importFrom purrr map_chr
#' @return A model_list with child class type_list
#' @export
as.model_list <- function(..., listed_models = NULL, target = ".outcome",
                          model_class, tuned = TRUE) {
  listed_models <- c(
    structure(list(...),
              names = purrr::map_chr(as.list(match.call(expand.dots = FALSE)$...), deparse)),
    listed_models
  )
  if (length(listed_models)) {
    if (any(!purrr::map_lgl(listed_models, inherits, "train")))
      stop("Those don't look like caret-trained models.")
    if (length(unique(lapply(listed_models, function(mm) mm$trainingData))) > 1)
      stop("Those models don't appear to have been trained on the same data.")
    types <- unique(tolower(purrr::map_chr(listed_models, ~ .x$modelType)))
    if (length(types) > 1L)
      stop("All model_class elements need to be the same. Yours: ",
           paste(types, collapse = ", "))
    if (!missing(model_class) && model_class != types)
      stop("model_class doesn't match the model(s).")
    model_class <- types
    names(listed_models) <- purrr::map_chr(listed_models, ~ .x$modelInfo$label)
    # Remove training data from all but the first model
    for (i in setdiff(seq_along(listed_models), 1)) {
      listed_models[[i]]$trainingData <- NULL
    }
  }
  check_model_class(model_class)
  structure(listed_models,
            class = c(paste0(model_class, "_list"), "model_list", class(listed_models)),
            tuned = tuned,
            target = target,
            timestamp = Sys.time())
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
