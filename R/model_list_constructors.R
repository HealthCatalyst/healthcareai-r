#' Make models into model_list object
#'
#' @param ... \code{caret}-trained models to put into a model list
#' @param listed_models Use this if your models are already in a list
#' @param model_class "classification" or "regression". Will be determined if
#'   not provided
#' @param target Quoted name of response variable
#' @param tuned Logical; if FALSE, will have super-class untuned_models
#' @param recipe recipe object from prep+_data, or NULL if the data didn't go
#'   through prep_data
#' @param positive_class If classification, the positive outcome class,
#'   otherwise NULL
#' @param model_name Quoted, name of the model. Defaults to the name of the
#' outcome variable.
#' @param best_levels best_levels list as attached to data frames from
#'   add_best_levels
#' @param original_data_str zero-row data frame with names and classes of all
#'   columns except the outcome as they came into either the model training
#'   function such as tune_models or prep_data
#' @param versions A list containing the following environmental variables from
#'   model training: r_version, hcai_version, and other_packages (a tibble). If
#'   not provided, will be extracted from the current session. See
#'   healthcareai:::attach_session_info for details
#'
#' @importFrom purrr map_chr
#' @importFrom purrr map_lgl
#' @return A model_list with child class type_list
#' @export
as.model_list <- function(...,
                          listed_models = NULL,
                          target = ".outcome",
                          model_class,
                          tuned = TRUE,
                          recipe = NULL,
                          positive_class = NULL,
                          model_name = NULL,
                          best_levels = NULL,
                          original_data_str,
                          versions) {
  listed_models <- c(
    structure(list(...),
              names = purrr::map_chr(as.list(match.call(expand.dots = FALSE)$...), deparse)),
    listed_models
  )

  types <- unique(tolower(purrr::map_chr(listed_models, ~ .x$modelType)))
  if (length(types) > 1L)
    stop("All model_class elements need to be the same. Yours: ",
         list_variables(types))
  if (!missing(model_class) && model_class != types) {
    if (model_class == "multiclass") {
      types <- "multiclass" # caret doesn't distinguish between 2 classes and >2
    } else {
      stop("model_class doesn't match the model(s).")
    }
  }
  model_class <- types
  names(listed_models) <- purrr::map_chr(listed_models, ~ .x$modelInfo$label)
  # Remove training data from models. Kept in recipe instead
  # and remove "call" object containing training data from all models
  for (i in seq_along(listed_models)) {
    # First, get dimensions of data at training (after dummies, etc.)
    ddim <- dim(listed_models[[i]]$trainingData)
    listed_models[[i]]$trainingData <- NULL
    listed_models[[i]]$call <- NULL
  }
  if (is.null(model_name))
    model_name <- target
  if (missing(versions))
    versions <- attr(attach_session_info(1), "versions")
  check_model_class(model_class)
  structure(listed_models,
            model_name = model_name,
            class = c(paste0(model_class, "_list"), "model_list", class(listed_models)),
            tuned = tuned,
            target = target,
            recipe = recipe,
            positive_class = positive_class,
            best_levels = best_levels,
            original_data_str = tibble::as_tibble(original_data_str),
            ddim = ddim,
            versions = versions) %>%
    structure(., performance = evaluate(.))
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
  supported_model_classes <- c("classification", "regression", "multiclass")
  if (!model_class %in% supported_model_classes)
    stop("model_class must be one of: ",
         list_variables(supported_model_classes))
  return(invisible(NULL))
}
